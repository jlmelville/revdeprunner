# This file composes private helpers defined in other package source files.
# nolint start: object_usage_linter.

reuse_inventory_binaries <- function(
  requests,
  inventory_bindings,
  lane,
  path_plan,
  transfer_policy = "copy"
) {
  requests <- normalize_inventory_reuse_requests(requests)
  validate_compatibility_lane(lane)
  validate_runtime_root_plan(path_plan)
  transfer_policy <- validate_warehouse_transfer_policy(transfer_policy)
  bindings <- normalize_inventory_selection_bindings(inventory_bindings)

  inventory_before <- observe_inventory_inputs(bindings$inventory_path)
  selected <- lapply(seq_len(nrow(requests)), function(row) {
    selection <- select_inventory_binary(
      bindings,
      requests$package[[row]],
      requests$version[[row]],
      lane,
      path_plan
    )
    source_snapshot <- if (identical(selection$status, "selected")) {
      warehouse_file_snapshot(selection$source_path)
    } else {
      NULL
    }
    list(selection = selection, source_snapshot = source_snapshot)
  })
  inventory_after <- observe_inventory_inputs(bindings$inventory_path)
  if (!identical(inventory_after, inventory_before)) {
    stop(
      "An inventory changed during batch artifact selection.",
      call. = FALSE
    )
  }
  selections <- lapply(selected, `[[`, "selection")
  selected_source_snapshots <- lapply(selected, `[[`, "source_snapshot")
  names(selections) <- requests$package

  promotions <- lapply(seq_along(selections), function(index) {
    selection <- selections[[index]]
    if (identical(selection$status, "missing")) {
      return(NULL)
    }
    validate_warehouse_source_unchanged(
      selection$source_path,
      selected_source_snapshots[[index]]
    )
    promote_warehouse_artifact(
      selection$source_path,
      selection$artifact,
      path_plan,
      transfer_policy
    )
  })
  names(promotions) <- requests$package

  new_inventory_binary_reuse(
    requests,
    selections,
    promotions,
    lane,
    path_plan,
    transfer_policy
  )
}

normalize_inventory_reuse_requests <- function(requests) {
  fields <- c("package", "version")
  if (
    !is.data.frame(requests) ||
      !identical(names(requests), fields) ||
      !all(vapply(requests, is.character, logical(1L))) ||
      nrow(requests) == 0L ||
      anyNA(requests)
  ) {
    stop("`requests` has an invalid structure.", call. = FALSE)
  }

  normalized <- data.frame(
    package = vapply(requests$package, validate_package_name, character(1L)),
    version = vapply(
      requests$version,
      validate_package_version,
      character(1L)
    ),
    stringsAsFactors = FALSE
  )
  if (anyDuplicated(normalized$package)) {
    stop(
      "Inventory reuse requests must contain unique packages.",
      call. = FALSE
    )
  }
  normalized <- normalized[
    order(normalized$package, normalized$version, method = "radix"),
    ,
    drop = FALSE
  ]
  rownames(normalized) <- NULL
  normalized
}

new_inventory_binary_reuse <- function(
  requests,
  selections,
  promotions,
  lane,
  path_plan,
  transfer_policy
) {
  reuse <- structure(
    list(
      lane_id = lane$lane_id,
      path_plan_id = path_plan$path_plan_id,
      warehouse_root = runtime_role_path(path_plan, "warehouse"),
      transfer_policy = transfer_policy,
      requests = requests,
      selections = selections,
      promotions = promotions
    ),
    class = "revdeprunner_inventory_binary_reuse"
  )
  validate_inventory_binary_reuse(reuse, lane, path_plan)
  reuse
}

validate_inventory_binary_reuse <- function(reuse, lane, path_plan) {
  fields <- c(
    "lane_id",
    "path_plan_id",
    "warehouse_root",
    "transfer_policy",
    "requests",
    "selections",
    "promotions"
  )
  if (
    !inherits(reuse, "revdeprunner_inventory_binary_reuse") ||
      !is.list(reuse) ||
      !identical(names(reuse), fields)
  ) {
    stop("Inventory binary reuse has an invalid structure.", call. = FALSE)
  }
  validate_sha256_identity(reuse$lane_id, "lane_id")
  validate_sha256_identity(reuse$path_plan_id, "path_plan_id")
  validate_compatibility_lane(lane)
  validate_runtime_root_plan(path_plan)
  validate_inventory_reuse_warehouse_root(reuse$warehouse_root)
  validate_warehouse_transfer_policy(reuse$transfer_policy)
  if (
    !identical(reuse$lane_id, lane$lane_id) ||
      !identical(reuse$path_plan_id, path_plan$path_plan_id) ||
      !identical(
        reuse$warehouse_root,
        runtime_role_path(path_plan, "warehouse")
      )
  ) {
    stop(
      "Inventory reuse does not match its lane or runtime-root plan.",
      call. = FALSE
    )
  }

  requests <- normalize_inventory_reuse_requests(reuse$requests)
  if (!identical(reuse$requests, requests)) {
    stop("Inventory reuse requests are not normalized.", call. = FALSE)
  }
  expected_names <- requests$package
  if (
    !is.list(reuse$selections) ||
      !is.list(reuse$promotions) ||
      !identical(names(reuse$selections), expected_names) ||
      !identical(names(reuse$promotions), expected_names)
  ) {
    stop("Inventory reuse entries do not match their requests.", call. = FALSE)
  }

  for (row in seq_len(nrow(requests))) {
    selection <- reuse$selections[[row]]
    promotion <- reuse$promotions[row]
    validate_inventory_artifact_selection(selection)
    if (
      !identical(selection$package, requests$package[[row]]) ||
        !identical(selection$version, requests$version[[row]]) ||
        !identical(selection$lane_id, reuse$lane_id)
    ) {
      stop(
        "Inventory reuse selection does not match its request.",
        call. = FALSE
      )
    }

    if (identical(selection$status, "missing")) {
      if (!is.null(promotion[[1L]])) {
        stop(
          "Missing inventory reuse selection must not have a promotion.",
          call. = FALSE
        )
      }
      next
    }
    validate_inventory_reuse_promotion(
      promotion[[1L]],
      selection,
      reuse$warehouse_root,
      reuse$transfer_policy
    )
  }

  invisible(reuse)
}

validate_inventory_reuse_warehouse_root <- function(warehouse_root) {
  if (
    !is.character(warehouse_root) ||
      length(warehouse_root) != 1L ||
      is.na(warehouse_root) ||
      !nzchar(warehouse_root) ||
      !runtime_path_is_absolute(warehouse_root) ||
      grepl("\\\\", warehouse_root)
  ) {
    stop("Inventory reuse warehouse root is invalid.", call. = FALSE)
  }

  warehouse_root
}

validate_inventory_reuse_promotion <- function(
  promotion,
  selection,
  warehouse_root,
  transfer_policy
) {
  fields <- c(
    "artifact_id",
    "source_path",
    "warehouse_path",
    "transfer_policy",
    "reused"
  )
  if (
    !inherits(promotion, "revdeprunner_warehouse_promotion") ||
      !is.list(promotion) ||
      !identical(names(promotion), fields)
  ) {
    stop("Inventory reuse promotion has an invalid structure.", call. = FALSE)
  }
  artifact_id <- selection$artifact$artifact_id
  digest <- sub("^sha256:", "", artifact_id)
  expected_path <- file.path(
    warehouse_root,
    "artifacts",
    "sha256",
    substr(digest, 1L, 2L),
    digest
  )
  if (
    !identical(promotion$artifact_id, artifact_id) ||
      !identical(promotion$source_path, selection$source_path) ||
      !identical(promotion$warehouse_path, expected_path) ||
      !identical(promotion$transfer_policy, transfer_policy) ||
      !is.logical(promotion$reused) ||
      length(promotion$reused) != 1L ||
      is.na(promotion$reused)
  ) {
    stop(
      "Inventory reuse promotion does not match its selection.",
      call. = FALSE
    )
  }

  invisible(promotion)
}

# nolint end
