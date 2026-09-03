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

package_version_key <- function(package, version) {
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
