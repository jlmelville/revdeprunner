# This file composes private helpers defined in other package source files.
# nolint start: object_usage_linter.

select_inventory_binary <- function(
  inventory_bindings,
  package,
  version,
  lane,
  path_plan
) {
  package <- validate_package_name(package)
  version <- validate_package_version(version)
  validate_compatibility_lane(lane)
  validate_runtime_root_plan(path_plan)
  bindings <- normalize_inventory_selection_bindings(inventory_bindings)

  inventory_before <- observe_inventory_inputs(bindings$inventory_path)
  inventories <- lapply(bindings$inventory_path, read_cache_inventory)
  inventory_after <- observe_inventory_inputs(bindings$inventory_path)
  if (!identical(inventory_after, inventory_before)) {
    stop("An inventory changed during artifact selection.", call. = FALSE)
  }

  inventory_ids <- vapply(
    inventories,
    function(inventory) inventory$inventory_sha256,
    character(1L)
  )
  cache_roots <- vapply(
    inventories,
    function(inventory) inventory$observation$cache_root,
    character(1L)
  )
  if (anyDuplicated(inventory_ids)) {
    stop(
      "Inventory bindings must identify distinct inventories.",
      call. = FALSE
    )
  }
  if (anyDuplicated(cache_roots)) {
    stop(
      "Inventory bindings must identify distinct cache roots.",
      call. = FALSE
    )
  }
  if (!all(cache_roots %in% path_plan$source_cache_roots)) {
    stop(
      "Every inventory cache root must be a declared source-cache root.",
      call. = FALSE
    )
  }

  candidates <- collect_inventory_binary_candidates(
    inventories,
    bindings,
    inventory_ids,
    package,
    version,
    lane
  )
  if (nrow(candidates) == 0L) {
    return(new_inventory_artifact_selection(
      status = "missing",
      package = package,
      version = version,
      lane = lane
    ))
  }

  selected_priority <- min(candidates$priority)
  candidates <- candidates[
    candidates$priority == selected_priority,
    ,
    drop = FALSE
  ]
  if (length(unique(candidates$sha256)) != 1L) {
    stop(
      "The selected inventory priority contains conflicting artifact hashes.",
      call. = FALSE
    )
  }
  candidates <- candidates[
    order(candidates$relative_path, method = "radix"),
    ,
    drop = FALSE
  ]
  selected <- candidates[1L, , drop = FALSE]
  source_path <- validate_selected_inventory_source(selected, path_plan)
  artifact <- new_artifact_identity(
    package = package,
    version = version,
    sha256 = selected$sha256[[1L]],
    archive_type = "binary",
    lane = lane
  )

  new_inventory_artifact_selection(
    status = "selected",
    package = package,
    version = version,
    lane = lane,
    artifact = artifact,
    inventory_path = selected$inventory_path[[1L]],
    inventory_sha256 = selected$inventory_sha256[[1L]],
    cache_root = selected$cache_root[[1L]],
    relative_path = selected$relative_path[[1L]],
    source_path = source_path,
    priority = selected$priority[[1L]]
  )
}

normalize_inventory_selection_bindings <- function(inventory_bindings) {
  fields <- c("inventory_path", "lane_id", "priority")
  if (
    !is.data.frame(inventory_bindings) ||
      !identical(names(inventory_bindings), fields) ||
      !is.character(inventory_bindings$inventory_path) ||
      !is.character(inventory_bindings$lane_id) ||
      !is.integer(inventory_bindings$priority) ||
      nrow(inventory_bindings) == 0L ||
      anyNA(inventory_bindings) ||
      any(!nzchar(inventory_bindings$inventory_path)) ||
      anyDuplicated(inventory_bindings$inventory_path) ||
      any(inventory_bindings$priority < 1L) ||
      anyDuplicated(inventory_bindings$priority)
  ) {
    stop("`inventory_bindings` has an invalid structure.", call. = FALSE)
  }
  invisible(vapply(
    inventory_bindings$lane_id,
    validate_sha256_identity,
    character(1L),
    argument = "lane_id"
  ))

  paths <- path.expand(inventory_bindings$inventory_path)
  if (
    any(!vapply(paths, runtime_path_is_absolute, logical(1L))) ||
      any(grepl("\\\\", paths)) ||
      any(vapply(paths, warehouse_path_is_link, logical(1L))) ||
      any(!file.exists(paths))
  ) {
    stop(
      "Inventory bindings must use existing resolved non-link files.",
      call. = FALSE
    )
  }
  info <- file.info(paths, extra_cols = FALSE)
  if (
    any(is.na(info$isdir)) ||
      any(info$isdir) ||
      any(
        !vapply(
          paths,
          function(path) utils::file_test("-f", path),
          logical(1L)
        )
      )
  ) {
    stop(
      "Inventory bindings must use existing resolved non-link files.",
      call. = FALSE
    )
  }
  resolved <- normalizePath(paths, winslash = "/", mustWork = TRUE)
  if (!identical(paths, resolved)) {
    stop(
      "Inventory bindings must use existing resolved non-link files.",
      call. = FALSE
    )
  }

  bindings <- inventory_bindings
  bindings$inventory_path <- resolved
  bindings <- bindings[
    order(bindings$priority, bindings$inventory_path, method = "radix"),
    ,
    drop = FALSE
  ]
  rownames(bindings) <- NULL
  bindings
}

collect_inventory_binary_candidates <- function(
  inventories,
  bindings,
  inventory_ids,
  package,
  version,
  lane
) {
  rows <- lapply(seq_along(inventories), function(index) {
    if (!identical(bindings$lane_id[[index]], lane$lane_id)) {
      return(NULL)
    }
    artifacts <- inventories[[index]]$observation$artifacts
    if (nrow(artifacts) == 0L) {
      return(NULL)
    }
    r_major_minor <- built_r_major_minor(artifacts$built)
    selected <- artifacts$status == "ok" &
      artifacts$archive_type == "binary" &
      !is.na(artifacts$package) &
      artifacts$package == package &
      !is.na(artifacts$version) &
      artifacts$version == version &
      !is.na(r_major_minor) &
      r_major_minor == lane$r_major_minor &
      !is.na(artifacts$platform) &
      artifacts$platform == lane$r_platform
    artifacts <- artifacts[selected, , drop = FALSE]
    if (nrow(artifacts) == 0L) {
      return(NULL)
    }
    artifacts$inventory_path <- bindings$inventory_path[[index]]
    artifacts$inventory_sha256 <- inventory_ids[[index]]
    artifacts$priority <- bindings$priority[[index]]
    artifacts
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) {
    empty <- empty_artifact_observations()
    empty$inventory_path <- character()
    empty$inventory_sha256 <- character()
    empty$priority <- integer()
    return(empty)
  }

  candidates <- do.call(rbind, rows)
  rownames(candidates) <- NULL
  candidates
}

validate_selected_inventory_source <- function(selected, path_plan) {
  relative_path <- selected$relative_path[[1L]]
  validate_inventory_selection_relative_path(relative_path)
  source_path <- file.path(selected$cache_root[[1L]], relative_path)
  source_path <- normalize_warehouse_source(source_path, path_plan)
  if (!path_is_within(selected$cache_root[[1L]], source_path)) {
    stop("Selected artifact escapes its inventory cache root.", call. = FALSE)
  }

  observed <- observe_artifact(
    source_path,
    relative_path,
    selected$cache_root[[1L]]
  )
  fields <- names(empty_artifact_observations())
  expected <- selected[, fields, drop = FALSE]
  rownames(expected) <- NULL
  if (!identical(observed, expected)) {
    stop(
      "Selected artifact changed since its inventory was written.",
      call. = FALSE
    )
  }

  source_path
}

validate_inventory_selection_relative_path <- function(relative_path) {
  if (
    !is.character(relative_path) ||
      length(relative_path) != 1L ||
      is.na(relative_path) ||
      !nzchar(relative_path) ||
      runtime_path_is_absolute(relative_path) ||
      grepl("\\\\", relative_path)
  ) {
    stop("Inventory artifact path must be a safe relative path.", call. = FALSE)
  }
  components <- strsplit(relative_path, "/", fixed = TRUE)[[1L]]
  if (any(!nzchar(components)) || any(components %in% c(".", ".."))) {
    stop("Inventory artifact path must be a safe relative path.", call. = FALSE)
  }

  relative_path
}

new_inventory_artifact_selection <- function(
  status,
  package,
  version,
  lane,
  artifact = NULL,
  inventory_path = NA_character_,
  inventory_sha256 = NA_character_,
  cache_root = NA_character_,
  relative_path = NA_character_,
  source_path = NA_character_,
  priority = NA_integer_
) {
  selection <- structure(
    list(
      status = status,
      package = package,
      version = version,
      lane_id = lane$lane_id,
      artifact = artifact,
      inventory_path = inventory_path,
      inventory_sha256 = inventory_sha256,
      cache_root = cache_root,
      relative_path = relative_path,
      source_path = source_path,
      priority = priority
    ),
    class = "revdeprunner_inventory_artifact_selection"
  )
  validate_inventory_artifact_selection(selection)
  selection
}

validate_inventory_artifact_selection <- function(selection) {
  fields <- c(
    "status",
    "package",
    "version",
    "lane_id",
    "artifact",
    "inventory_path",
    "inventory_sha256",
    "cache_root",
    "relative_path",
    "source_path",
    "priority"
  )
  if (
    !inherits(selection, "revdeprunner_inventory_artifact_selection") ||
      !is.list(selection) ||
      !identical(names(selection), fields)
  ) {
    stop(
      "Inventory artifact selection has an invalid structure.",
      call. = FALSE
    )
  }
  validate_package_name(selection$package)
  validate_package_version(selection$version)
  validate_sha256_identity(selection$lane_id, "lane_id")
  if (
    !is.character(selection$status) ||
      length(selection$status) != 1L ||
      is.na(selection$status) ||
      !selection$status %in% c("selected", "missing")
  ) {
    stop("Inventory artifact selection status is unsupported.", call. = FALSE)
  }

  provenance_fields <- c(
    "inventory_path",
    "inventory_sha256",
    "cache_root",
    "relative_path",
    "source_path"
  )
  if (identical(selection$status, "missing")) {
    if (
      !is.null(selection$artifact) ||
        !all(vapply(
          selection[provenance_fields],
          function(value)
            is.character(value) && length(value) == 1L && is.na(value),
          logical(1L)
        )) ||
        !is.integer(selection$priority) ||
        length(selection$priority) != 1L ||
        !is.na(selection$priority)
    ) {
      stop(
        "Missing inventory selection fields are inconsistent.",
        call. = FALSE
      )
    }
    return(invisible(selection))
  }

  validate_artifact_identity(selection$artifact)
  if (
    !identical(selection$artifact$package, selection$package) ||
      !identical(selection$artifact$version, selection$version) ||
      !identical(selection$artifact$archive_type, "binary") ||
      !identical(selection$artifact$lane_id, selection$lane_id)
  ) {
    stop("Selected artifact identity is inconsistent.", call. = FALSE)
  }
  scalar_text <- vapply(
    selection[provenance_fields],
    function(value) {
      is.character(value) &&
        length(value) == 1L &&
        !is.na(value) &&
        nzchar(value)
    },
    logical(1L)
  )
  if (
    !all(scalar_text) ||
      !is.integer(selection$priority) ||
      length(selection$priority) != 1L ||
      is.na(selection$priority) ||
      selection$priority < 1L
  ) {
    stop("Selected inventory provenance is inconsistent.", call. = FALSE)
  }
  validate_sha256(selection$inventory_sha256, "inventory_sha256")
  validate_inventory_selection_relative_path(selection$relative_path)
  if (
    !runtime_path_is_absolute(selection$inventory_path) ||
      grepl("\\\\", selection$inventory_path) ||
      !identical(
        basename(selection$inventory_path),
        paste0(selection$inventory_sha256, ".rds")
      ) ||
      !runtime_path_is_absolute(selection$cache_root) ||
      grepl("\\\\", selection$cache_root) ||
      !identical(
        selection$source_path,
        file.path(selection$cache_root, selection$relative_path)
      )
  ) {
    stop("Selected inventory provenance is inconsistent.", call. = FALSE)
  }

  invisible(selection)
}

# nolint end
