report_cache_inventories <- function(inventory_paths) {
  inventory_paths <- normalize_inventory_paths(inventory_paths)
  inputs_before <- observe_inventory_inputs(inventory_paths)
  inventories <- lapply(inventory_paths, read_cache_inventory)
  inputs_after <- observe_inventory_inputs(inventory_paths)

  if (!identical(inputs_after, inputs_before)) {
    stop("An inventory changed while reports were generated.", call. = FALSE)
  }

  cache_roots <- vapply(
    inventories,
    function(inventory) inventory$observation$cache_root,
    character(1L)
  )
  if (anyDuplicated(cache_roots)) {
    stop("Each inventory must describe a different cache root.", call. = FALSE)
  }

  ordering <- order(cache_roots, method = "radix")
  inventories <- inventories[ordering]
  inventory_table <- data.frame(
    cache_root = cache_roots[ordering],
    inventory_sha256 = vapply(
      inventories,
      function(inventory) inventory$inventory_sha256,
      character(1L)
    ),
    stringsAsFactors = FALSE
  )
  artifacts <- combine_inventory_artifacts(inventories, inventory_table)

  structure(
    list(
      inventories = inventory_table,
      duplicate_hashes = report_duplicate_hashes(artifacts),
      hash_collisions = report_hash_collisions(artifacts),
      artifact_issues = report_artifact_issues(artifacts),
      compatibility_conflicts = report_compatibility_conflicts(artifacts)
    ),
    class = "revdeprunner_inventory_report"
  )
}

normalize_inventory_paths <- function(inventory_paths) {
  if (
    !is.character(inventory_paths) ||
      length(inventory_paths) < 2L ||
      anyNA(inventory_paths) ||
      any(!nzchar(inventory_paths))
  ) {
    stop("`inventory_paths` must contain at least two paths.", call. = FALSE)
  }

  inventory_paths <- path.expand(inventory_paths)
  linked <- nzchar(Sys.readlink(inventory_paths))
  if (any(linked)) {
    stop("Inventory inputs must not be symbolic links.", call. = FALSE)
  }
  if (any(!file.exists(inventory_paths))) {
    stop("Every inventory input must identify an existing file.", call. = FALSE)
  }

  inventory_paths <- normalizePath(
    inventory_paths,
    winslash = "/",
    mustWork = TRUE
  )
  info <- file.info(inventory_paths, extra_cols = FALSE)
  if (any(is.na(info$isdir)) || any(info$isdir)) {
    stop("Every inventory input must identify a file.", call. = FALSE)
  }
  if (anyDuplicated(inventory_paths)) {
    stop("Inventory input paths must be unique.", call. = FALSE)
  }

  sort(inventory_paths, method = "radix")
}

observe_inventory_inputs <- function(inventory_paths) {
  info <- file.info(inventory_paths, extra_cols = FALSE)
  hashes <- vapply(
    inventory_paths,
    digest::digest,
    character(1L),
    algo = "sha256",
    file = TRUE,
    serialize = FALSE
  )

  data.frame(
    path = inventory_paths,
    size_bytes = unname(info$size),
    modified_at = format(
      info$mtime,
      format = "%Y-%m-%dT%H:%M:%OS6Z",
      tz = "UTC"
    ),
    sha256 = hashes,
    stringsAsFactors = FALSE
  )
}

read_cache_inventory <- function(path) {
  filename <- basename(path)
  if (!grepl("^[a-f0-9]{64}\\.rds$", filename)) {
    stop(
      "Inventory filename must contain its lowercase SHA-256 identity.",
      call. = FALSE
    )
  }

  payload <- read_inventory_payload(path) # nolint: object_usage_linter.
  inventory_sha256 <- digest::digest(
    payload,
    algo = "sha256",
    serialize = FALSE
  )
  if (!identical(filename, paste0(inventory_sha256, ".rds"))) {
    stop(
      "Inventory content does not match its filename identity.",
      call. = FALSE
    )
  }

  observation <- tryCatch(
    unserialize(payload),
    error = function(error) {
      stop("Unable to deserialize the cache inventory.", call. = FALSE)
    }
  )
  validate_cache_inventory_observation(observation)

  list(
    inventory_sha256 = inventory_sha256,
    observation = observation
  )
}

validate_cache_inventory_observation <- function(observation) {
  if (
    !inherits(observation, "revdeprunner_cache_observation") ||
      !is.list(observation) ||
      !all(
        c("cache_root", "artifacts", "repository_metadata") %in%
          names(observation)
      )
  ) {
    stop("Inventory does not contain a cache observation.", call. = FALSE)
  }

  cache_root <- observation$cache_root
  if (
    !is.character(cache_root) ||
      length(cache_root) != 1L ||
      is.na(cache_root) ||
      !nzchar(cache_root)
  ) {
    stop("Inventory cache root is invalid.", call. = FALSE)
  }

  artifact_template <- empty_artifact_observations() # nolint: object_usage_linter.
  metadata_template <- empty_repository_metadata_observations() # nolint: object_usage_linter.
  validate_inventory_rows(
    observation$artifacts,
    artifact_template,
    cache_root,
    "artifact"
  )
  validate_inventory_rows(
    observation$repository_metadata,
    metadata_template,
    cache_root,
    "repository metadata"
  )

  artifacts <- observation$artifacts
  if (nrow(artifacts) > 0L) {
    valid_statuses <- c(
      "ok",
      "incomplete_metadata",
      "unreadable_archive",
      "unreadable_file"
    )
    if (anyNA(artifacts$status) || any(!artifacts$status %in% valid_statuses)) {
      stop("Inventory contains an invalid artifact status.", call. = FALSE)
    }
    valid_archive_types <- c("source", "binary", "unknown")
    if (
      anyNA(artifacts$archive_type) ||
        any(!artifacts$archive_type %in% valid_archive_types)
    ) {
      stop("Inventory contains an invalid archive type.", call. = FALSE)
    }
    present_hashes <- !is.na(artifacts$sha256)
    if (
      any(
        present_hashes &
          !grepl("^[a-f0-9]{64}$", artifacts$sha256)
      )
    ) {
      stop("Inventory contains an invalid artifact SHA-256.", call. = FALSE)
    }
  }

  repository_metadata <- observation$repository_metadata
  if (nrow(repository_metadata) > 0L) {
    if (
      anyNA(repository_metadata$status) ||
        any(!repository_metadata$status %in% c("ok", "unreadable_file"))
    ) {
      stop(
        "Inventory contains an invalid repository metadata status.",
        call. = FALSE
      )
    }
    present_hashes <- !is.na(repository_metadata$sha256)
    if (
      any(
        present_hashes &
          !grepl("^[a-f0-9]{64}$", repository_metadata$sha256)
      )
    ) {
      stop(
        "Inventory contains an invalid repository metadata SHA-256.",
        call. = FALSE
      )
    }
  }

  invisible(NULL)
}

validate_inventory_rows <- function(rows, template, cache_root, label) {
  fields <- names(template)
  if (!is.data.frame(rows) || !all(fields %in% names(rows))) {
    stop(
      sprintf("Inventory %s rows have an invalid structure.", label),
      call. = FALSE
    )
  }
  matching_types <- vapply(
    fields,
    function(field) identical(typeof(rows[[field]]), typeof(template[[field]])),
    logical(1L)
  )
  if (!all(matching_types)) {
    stop(
      sprintf("Inventory %s rows have incompatible field types.", label),
      call. = FALSE
    )
  }
  if (nrow(rows) == 0L) {
    return(invisible(NULL))
  }
  if (
    anyNA(rows$cache_root) ||
      any(rows$cache_root != cache_root) ||
      anyNA(rows$relative_path) ||
      any(!nzchar(rows$relative_path)) ||
      anyDuplicated(rows$relative_path)
  ) {
    stop(sprintf("Inventory %s rows are inconsistent.", label), call. = FALSE)
  }

  invisible(NULL)
}

combine_inventory_artifacts <- function(inventories, inventory_table) {
  artifacts <- lapply(
    inventories,
    function(inventory) inventory$observation$artifacts
  )
  artifacts <- do.call(rbind, artifacts)
  rownames(artifacts) <- NULL
  artifacts$inventory_sha256 <- inventory_table$inventory_sha256[
    match(artifacts$cache_root, inventory_table$cache_root)
  ]
  artifacts <- artifacts[,
    c(
      "cache_root",
      "inventory_sha256",
      names(empty_artifact_observations())[-1L] # nolint: object_usage_linter.
    )
  ]

  order_artifact_rows(artifacts, c("cache_root", "relative_path"))
}

report_duplicate_hashes <- function(artifacts) {
  candidates <- artifacts[!is.na(artifacts$sha256), , drop = FALSE]
  groups <- split(seq_len(nrow(candidates)), candidates$sha256)
  members <- group_members(
    groups,
    function(rows) length(unique(candidates$cache_root[rows])) > 1L
  )
  report <- candidates[members, , drop = FALSE]
  order_artifact_rows(
    report,
    c("sha256", "package", "version", "cache_root", "relative_path")
  )
}

report_hash_collisions <- function(artifacts) {
  complete_identity <- !is.na(artifacts$package) &
    nzchar(artifacts$package) &
    !is.na(artifacts$version) &
    nzchar(artifacts$version) &
    !is.na(artifacts$sha256)
  candidates <- artifacts[complete_identity, , drop = FALSE]
  groups <- split(
    seq_len(nrow(candidates)),
    artifact_identity(candidates$package, candidates$version)
  )
  members <- group_members(
    groups,
    function(rows) {
      has_cross_root_disagreement(
        candidates$cache_root[rows],
        candidates$sha256[rows]
      )
    }
  )
  report <- candidates[members, , drop = FALSE]
  order_artifact_rows(
    report,
    c("package", "version", "sha256", "cache_root", "relative_path")
  )
}

report_artifact_issues <- function(artifacts) {
  issue_statuses <- c(
    "incomplete_metadata",
    "unreadable_archive",
    "unreadable_file"
  )
  report <- artifacts[artifacts$status %in% issue_statuses, , drop = FALSE]
  order_artifact_rows(
    report,
    c("status", "package", "version", "cache_root", "relative_path")
  )
}

report_compatibility_conflicts <- function(artifacts) {
  binary_identity <- artifacts$archive_type == "binary" &
    !is.na(artifacts$package) &
    nzchar(artifacts$package) &
    !is.na(artifacts$version) &
    nzchar(artifacts$version)
  candidates <- artifacts[binary_identity, , drop = FALSE]
  candidates$r_major_minor <- built_r_major_minor(candidates$built)
  candidates$conflict_dimensions <- character(nrow(candidates))
  groups <- split(
    seq_len(nrow(candidates)),
    artifact_identity(candidates$package, candidates$version)
  )

  reports <- lapply(groups, function(rows) {
    dimensions <- character()
    if (
      has_cross_root_disagreement(
        candidates$cache_root[rows],
        candidates$r_major_minor[rows]
      )
    ) {
      dimensions <- c(dimensions, "r_major_minor")
    }
    if (
      has_cross_root_disagreement(
        candidates$cache_root[rows],
        candidates$platform[rows]
      )
    ) {
      dimensions <- c(dimensions, "platform")
    }
    if (length(dimensions) == 0L) {
      return(NULL)
    }

    report <- candidates[rows, , drop = FALSE]
    report$conflict_dimensions <- paste(dimensions, collapse = ",")
    report
  })
  reports <- Filter(Negate(is.null), reports)
  if (length(reports) == 0L) {
    return(candidates[FALSE, , drop = FALSE])
  }

  report <- do.call(rbind, reports)
  rownames(report) <- NULL
  order_artifact_rows(
    report,
    c(
      "package",
      "version",
      "conflict_dimensions",
      "r_major_minor",
      "platform",
      "cache_root",
      "relative_path"
    )
  )
}

group_members <- function(groups, qualifies) {
  selected <- vapply(groups, qualifies, logical(1L))
  if (!any(selected)) {
    return(integer())
  }

  unname(unlist(groups[selected], use.names = FALSE))
}

artifact_identity <- function(package, version) {
  paste(package, version, sep = "\034")
}

built_r_major_minor <- function(built) {
  result <- rep(NA_character_, length(built))
  present <- !is.na(built)
  matches <- regexec(
    "^R[[:space:]]+([0-9]+\\.[0-9]+)(?:\\.|;|$)",
    built[present]
  )
  fields <- regmatches(built[present], matches)
  result[present] <- vapply(
    fields,
    function(field) if (length(field) == 2L) field[[2L]] else NA_character_,
    character(1L)
  )
  result
}

has_cross_root_disagreement <- function(cache_root, value) {
  present <- !is.na(value) & nzchar(value)
  cache_root <- cache_root[present]
  value <- value[present]
  if (length(unique(cache_root)) < 2L || length(unique(value)) < 2L) {
    return(FALSE)
  }

  pairs <- utils::combn(seq_along(value), 2L)
  any(
    cache_root[pairs[1L, ]] != cache_root[pairs[2L, ]] &
      value[pairs[1L, ]] != value[pairs[2L, ]]
  )
}

order_artifact_rows <- function(rows, fields) {
  if (nrow(rows) == 0L) {
    rownames(rows) <- NULL
    return(rows)
  }

  keys <- lapply(rows[fields], function(value) {
    ifelse(is.na(value), "", as.character(value))
  })
  ordering <- do.call(order, c(keys, list(method = "radix", na.last = TRUE)))
  rows <- rows[ordering, , drop = FALSE]
  rownames(rows) <- NULL
  rows
}
