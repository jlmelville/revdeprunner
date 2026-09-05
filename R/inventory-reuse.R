reuse_inventory_binaries <- function(
  requests,
  inventory_bindings,
  lane,
  path_plan
) {
  requests <- normalize_inventory_reuse_requests(requests)
  selections <- select_inventory_binaries(
    requests,
    inventory_bindings,
    lane,
    path_plan
  )

  cache_paths <- vapply(
    seq_along(selections),
    function(index) {
      selection <- selections[[index]]
      if (identical(selection$status, "missing")) {
        return(NA_character_)
      }
      publish_binary_cache_artifact(
        selection$source_path,
        selection$artifact,
        path_plan
      )
    },
    character(1L)
  )
  names(cache_paths) <- requests$package

  new_inventory_binary_reuse(
    requests,
    selections,
    cache_paths,
    lane,
    path_plan
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
  cache_paths,
  lane,
  path_plan
) {
  reuse <- structure(
    list(
      lane_id = lane$lane_id,
      path_plan_id = path_plan$path_plan_id,
      requests = requests,
      selections = selections,
      cache_paths = cache_paths
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
    "requests",
    "selections",
    "cache_paths"
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
  if (
    !identical(reuse$lane_id, lane$lane_id) ||
      !identical(reuse$path_plan_id, path_plan$path_plan_id)
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
      !is.character(reuse$cache_paths) ||
      !identical(names(reuse$selections), expected_names) ||
      !identical(names(reuse$cache_paths), expected_names)
  ) {
    stop("Inventory reuse entries do not match their requests.", call. = FALSE)
  }

  for (row in seq_len(nrow(requests))) {
    selection <- reuse$selections[[row]]
    cache_path <- reuse$cache_paths[[row]]
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
    if (
      identical(selection$status, "selected") &&
        !selection$cache_root %in% path_plan$source_cache_roots
    ) {
      stop(
        "Inventory reuse selection has an undeclared source-cache root.",
        call. = FALSE
      )
    }

    if (identical(selection$status, "missing")) {
      if (!is.na(cache_path)) {
        stop(
          "Missing inventory reuse selection must not have a cache path.",
          call. = FALSE
        )
      }
      next
    }
    validate_binary_cache_artifact(
      cache_path,
      selection$artifact,
      path_plan
    )
  }

  invisible(reuse)
}
