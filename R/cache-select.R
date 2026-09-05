select_cached_binaries <- function(requests, observations, lane, path_plan) {
  requests <- normalize_binary_reuse_requests(requests)
  validate_compatibility_lane(lane)
  validate_runtime_root_plan(path_plan)
  observations <- normalize_cache_observations(observations, path_plan)

  selections <- lapply(seq_len(nrow(requests)), function(row) {
    select_cached_binary(
      observations,
      requests$package[[row]],
      requests$version[[row]],
      lane,
      path_plan
    )
  })
  names(selections) <- requests$package
  selections
}

select_cached_binary <- function(
  observations,
  package,
  version,
  lane,
  path_plan
) {
  package <- validate_package_name(package)
  version <- validate_package_version(version)
  candidates <- collect_cached_binary_candidates(
    observations,
    package,
    version,
    lane
  )
  if (nrow(candidates) == 0L) {
    return(new_cached_binary_selection(
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
      "The selected cache priority contains conflicting artifact hashes.",
      call. = FALSE
    )
  }
  candidates <- candidates[
    order(candidates$relative_path, method = "radix"),
    ,
    drop = FALSE
  ]
  selected <- candidates[1L, , drop = FALSE]
  source_path <- cached_selection_source_path(selected, path_plan)
  artifact <- new_artifact_identity(
    package = package,
    version = version,
    sha256 = selected$sha256[[1L]],
    archive_type = "binary",
    lane = lane
  )

  new_cached_binary_selection(
    status = "selected",
    package = package,
    version = version,
    lane = lane,
    artifact = artifact,
    cache_root = selected$cache_root[[1L]],
    relative_path = selected$relative_path[[1L]],
    source_path = source_path,
    priority = selected$priority[[1L]]
  )
}

normalize_cache_observations <- function(observations, path_plan) {
  template <- empty_cache_observations()
  fields <- names(template)
  if (
    !is.data.frame(observations) ||
      !identical(names(observations), fields) ||
      !all(vapply(
        fields,
        function(field) {
          identical(typeof(observations[[field]]), typeof(template[[field]]))
        },
        logical(1L)
      ))
  ) {
    stop("Cache observations have an invalid structure.", call. = FALSE)
  }
  if (nrow(observations) == 0L) {
    return(observations)
  }

  valid_statuses <- c(
    "ok",
    "incomplete_metadata",
    "unreadable_archive",
    "unreadable_file"
  )
  valid_archive_types <- c("source", "binary", "unknown")
  if (
    anyNA(observations$cache_root) ||
      anyNA(observations$relative_path) ||
      anyNA(observations$status) ||
      any(!observations$status %in% valid_statuses) ||
      anyNA(observations$archive_type) ||
      any(!observations$archive_type %in% valid_archive_types) ||
      anyNA(observations$priority) ||
      any(observations$priority < 1L)
  ) {
    stop("Cache observations contain invalid values.", call. = FALSE)
  }
  present_hashes <- !is.na(observations$sha256)
  if (
    any(
      present_hashes &
        !grepl("^[a-f0-9]{64}$", observations$sha256)
    )
  ) {
    stop(
      "Cache observations contain an invalid artifact SHA-256.",
      call. = FALSE
    )
  }
  invisible(vapply(
    observations$relative_path,
    validate_cache_relative_path,
    character(1L)
  ))
  if (
    any(!observations$cache_root %in% path_plan$source_cache_roots) ||
      anyDuplicated(paste(
        observations$cache_root,
        observations$relative_path,
        sep = "\034"
      ))
  ) {
    stop(
      "Cache observations do not match the runtime cache roots.",
      call. = FALSE
    )
  }
  root_priorities <- unique(observations[c("cache_root", "priority")])
  if (
    anyDuplicated(root_priorities$cache_root) ||
      anyDuplicated(root_priorities$priority)
  ) {
    stop("Cache observation priorities are inconsistent.", call. = FALSE)
  }

  observations <- observations[
    order(
      observations$priority,
      observations$relative_path,
      method = "radix"
    ),
    ,
    drop = FALSE
  ]
  rownames(observations) <- NULL
  observations
}

collect_cached_binary_candidates <- function(
  observations,
  package,
  version,
  lane
) {
  if (nrow(observations) == 0L) {
    return(observations)
  }
  r_major_minor <- built_r_major_minor(observations$built)
  selected <- observations$status == "ok" &
    observations$archive_type == "binary" &
    !is.na(observations$package) &
    observations$package == package &
    !is.na(observations$version) &
    observations$version == version &
    !is.na(r_major_minor) &
    r_major_minor == lane$r_major_minor &
    !is.na(observations$platform) &
    observations$platform == lane$r_platform
  observations[selected, , drop = FALSE]
}

cached_selection_source_path <- function(selected, path_plan) {
  relative_path <- selected$relative_path[[1L]]
  validate_cache_relative_path(relative_path)
  source_path <- file.path(selected$cache_root[[1L]], relative_path)
  source_path <- normalize_artifact_path(source_path, path_plan)
  if (!path_is_within(selected$cache_root[[1L]], source_path)) {
    stop("Selected artifact escapes its cache root.", call. = FALSE)
  }

  source_path
}

validate_cache_relative_path <- function(relative_path) {
  if (
    !is.character(relative_path) ||
      length(relative_path) != 1L ||
      is.na(relative_path) ||
      !nzchar(relative_path) ||
      runtime_path_is_absolute(relative_path) ||
      grepl("\\\\", relative_path)
  ) {
    stop("Cache artifact path must be a safe relative path.", call. = FALSE)
  }
  components <- strsplit(relative_path, "/", fixed = TRUE)[[1L]]
  if (any(!nzchar(components)) || any(components %in% c(".", ".."))) {
    stop("Cache artifact path must be a safe relative path.", call. = FALSE)
  }

  relative_path
}

new_cached_binary_selection <- function(
  status,
  package,
  version,
  lane,
  artifact = NULL,
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
      cache_root = cache_root,
      relative_path = relative_path,
      source_path = source_path,
      priority = priority
    ),
    class = "revdeprunner_cached_binary_selection"
  )
  validate_cached_binary_selection(selection)
  selection
}

validate_cached_binary_selection <- function(selection) {
  fields <- c(
    "status",
    "package",
    "version",
    "lane_id",
    "artifact",
    "cache_root",
    "relative_path",
    "source_path",
    "priority"
  )
  if (
    !inherits(selection, "revdeprunner_cached_binary_selection") ||
      !is.list(selection) ||
      !identical(names(selection), fields)
  ) {
    stop("Cached binary selection has an invalid structure.", call. = FALSE)
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
    stop("Cached binary selection status is unsupported.", call. = FALSE)
  }

  provenance_fields <- c("cache_root", "relative_path", "source_path")
  if (identical(selection$status, "missing")) {
    if (
      !is.null(selection$artifact) ||
        !all(vapply(
          selection[provenance_fields],
          function(value) {
            is.character(value) && length(value) == 1L && is.na(value)
          },
          logical(1L)
        )) ||
        !is.integer(selection$priority) ||
        length(selection$priority) != 1L ||
        !is.na(selection$priority)
    ) {
      stop("Missing cached binary fields are inconsistent.", call. = FALSE)
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
    stop("Cached binary artifact identity is inconsistent.", call. = FALSE)
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
    stop("Cached binary provenance is inconsistent.", call. = FALSE)
  }
  validate_cache_relative_path(selection$relative_path)
  if (
    !runtime_path_is_absolute(selection$cache_root) ||
      grepl("\\\\", selection$cache_root) ||
      !identical(
        selection$source_path,
        file.path(selection$cache_root, selection$relative_path)
      )
  ) {
    stop("Cached binary provenance is inconsistent.", call. = FALSE)
  }

  invisible(selection)
}
