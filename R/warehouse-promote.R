# This file composes private helpers defined in other package source files.
# nolint start: object_usage_linter.

promote_warehouse_artifact <- function(
  source_path,
  artifact,
  path_plan,
  transfer_policy = "copy",
  archive_name = basename(source_path)
) {
  validate_artifact_identity(artifact)
  validate_runtime_root_plan(path_plan)
  transfer_policy <- validate_warehouse_transfer_policy(transfer_policy)
  source_path <- normalize_warehouse_source(source_path, path_plan)
  archive_name <- validate_warehouse_archive_name(archive_name)

  warehouse_root <- runtime_role_path(path_plan, "warehouse")
  artifact_path <- warehouse_artifact_path(warehouse_root, artifact)

  if (warehouse_path_exists(artifact_path)) {
    validate_existing_warehouse_artifact(
      artifact_path,
      artifact,
      warehouse_root
    )
    return(new_warehouse_promotion(
      artifact,
      source_path,
      artifact_path,
      transfer_policy,
      reused = TRUE
    ))
  }

  validate_warehouse_archive(source_path, artifact, archive_name)
  warehouse_paths <- materialize_warehouse_paths(warehouse_root, artifact)
  validate_runtime_root_plan(path_plan)
  if (!identical(warehouse_paths$artifact, artifact_path)) {
    stop("Warehouse artifact path changed during promotion.", call. = FALSE)
  }

  suffix <- warehouse_archive_suffix(source_path)
  staged_path <- tempfile(
    pattern = ".artifact-",
    tmpdir = warehouse_paths$staging,
    fileext = suffix
  )
  on.exit(unlink(staged_path, force = TRUE), add = TRUE)

  copied <- warehouse_copy_file(
    source_path,
    staged_path,
    overwrite = FALSE,
    copy.mode = FALSE,
    copy.date = FALSE
  )
  if (!isTRUE(copied)) {
    stop("Unable to copy the artifact into warehouse staging.", call. = FALSE)
  }
  validate_warehouse_archive(staged_path, artifact, archive_name)

  refreshed_paths <- materialize_warehouse_paths(warehouse_root, artifact)
  validate_runtime_root_plan(path_plan)
  if (!identical(refreshed_paths, warehouse_paths)) {
    stop("Warehouse paths changed during promotion.", call. = FALSE)
  }

  published <- suppressWarnings(
    warehouse_publish_link(staged_path, warehouse_paths$artifact)
  )
  if (!isTRUE(published)) {
    if (warehouse_path_exists(warehouse_paths$artifact)) {
      validate_existing_warehouse_artifact(
        warehouse_paths$artifact,
        artifact,
        warehouse_root
      )
      return(new_warehouse_promotion(
        artifact,
        source_path,
        warehouse_paths$artifact,
        transfer_policy,
        reused = TRUE
      ))
    }
    stop("Unable to publish the staged artifact atomically.", call. = FALSE)
  }

  remove_published <- TRUE
  on.exit(
    if (remove_published) unlink(warehouse_paths$artifact, force = TRUE),
    add = TRUE
  )
  remove_published <- FALSE

  new_warehouse_promotion(
    artifact,
    source_path,
    warehouse_paths$artifact,
    transfer_policy,
    reused = FALSE
  )
}

validate_warehouse_archive_name <- function(archive_name) {
  if (
    !is.character(archive_name) ||
      length(archive_name) != 1L ||
      is.na(archive_name) ||
      !nzchar(archive_name) ||
      !identical(archive_name, basename(archive_name))
  ) {
    stop("`archive_name` must be one archive basename.", call. = FALSE)
  }
  warehouse_archive_suffix(archive_name)
  archive_name
}

validate_warehouse_transfer_policy <- function(transfer_policy) {
  if (
    !is.character(transfer_policy) ||
      length(transfer_policy) != 1L ||
      is.na(transfer_policy) ||
      !identical(transfer_policy, "copy")
  ) {
    stop("`transfer_policy` must be exactly `copy`.", call. = FALSE)
  }

  transfer_policy
}

normalize_warehouse_source <- function(source_path, path_plan) {
  if (
    !is.character(source_path) ||
      length(source_path) != 1L ||
      is.na(source_path) ||
      !nzchar(source_path)
  ) {
    stop("`source_path` must be one non-empty path.", call. = FALSE)
  }

  source_path <- path.expand(source_path)
  if (!runtime_path_is_absolute(source_path) || grepl("\\\\", source_path)) {
    stop("`source_path` must be an absolute forward-slash path.", call. = FALSE)
  }
  if (warehouse_path_is_link(source_path)) {
    stop("The warehouse source must not be a symbolic link.", call. = FALSE)
  }
  if (!file.exists(source_path)) {
    stop("The warehouse source must identify an existing file.", call. = FALSE)
  }

  resolved <- normalizePath(source_path, winslash = "/", mustWork = TRUE)
  if (!identical(source_path, resolved)) {
    stop("`source_path` must be a resolved physical path.", call. = FALSE)
  }
  if (!utils::file_test("-f", resolved) || dir.exists(resolved)) {
    stop("The warehouse source must identify a regular file.", call. = FALSE)
  }
  if (file.access(resolved, mode = 4L) != 0L) {
    stop("The warehouse source must be readable.", call. = FALSE)
  }

  run_root <- runtime_role_path(path_plan, "run")
  approved_roots <- c(path_plan$source_cache_roots, run_root)
  contained <- vapply(
    approved_roots,
    path_is_within,
    logical(1L),
    path = resolved
  )
  if (!any(contained)) {
    stop(
      "The warehouse source must remain within a source-cache or run root.",
      call. = FALSE
    )
  }

  resolved
}

runtime_role_path <- function(path_plan, role) {
  selected <- path_plan$paths$path[path_plan$paths$role == role]
  if (length(selected) != 1L) {
    stop(sprintf("Runtime plan has no unique `%s` path.", role), call. = FALSE)
  }

  selected
}

warehouse_file_snapshot <- function(path) {
  info <- file.info(path, extra_cols = FALSE)
  if (
    nrow(info) != 1L ||
      is.na(info$isdir) ||
      info$isdir ||
      is.na(info$size) ||
      is.na(info$mtime) ||
      is.na(info$mode)
  ) {
    stop("Unable to observe the complete warehouse source.", call. = FALSE)
  }

  list(
    size_bytes = unname(info$size),
    modified_at = unname(as.numeric(info$mtime)),
    mode = unname(as.character(info$mode)),
    sha256 = digest::digest(
      path,
      algo = "sha256",
      file = TRUE,
      serialize = FALSE
    )
  )
}

validate_warehouse_source_unchanged <- function(source_path, before) {
  after <- warehouse_file_snapshot(source_path)
  if (!identical(after, before)) {
    stop("The warehouse source changed during promotion.", call. = FALSE)
  }

  invisible(after)
}

validate_warehouse_archive <- function(
  path,
  artifact,
  archive_name = basename(path)
) {
  observed_sha256 <- digest::digest(
    path,
    algo = "sha256",
    file = TRUE,
    serialize = FALSE
  )
  if (!identical(observed_sha256, artifact$sha256)) {
    stop("Artifact payload does not match its SHA-256 identity.", call. = FALSE)
  }

  metadata <- read_archive_metadata(
    path,
    archive_filename_fields(archive_name),
    archive_name
  )
  if (!identical(metadata$status, "ok")) {
    stop(
      sprintf("Artifact archive validation failed: %s", metadata$error),
      call. = FALSE
    )
  }
  observed <- c(
    package = metadata$package,
    version = metadata$version,
    archive_type = metadata$archive_type
  )
  expected <- unlist(
    artifact[c("package", "version", "archive_type")],
    use.names = TRUE
  )
  if (!identical(observed, expected)) {
    stop(
      "Artifact archive metadata does not match its identity.",
      call. = FALSE
    )
  }

  invisible(metadata)
}

warehouse_archive_suffix <- function(path) {
  matched <- regexpr(
    "\\.(?:tar\\.(?:gz|bz2|xz)|tgz|zip)$",
    basename(path),
    ignore.case = TRUE,
    perl = TRUE
  )
  if (matched[[1L]] == -1L) {
    stop("Warehouse source has an unsupported archive suffix.", call. = FALSE)
  }

  substring(basename(path), matched)
}

materialize_warehouse_paths <- function(warehouse_root, artifact) {
  data_root <- dirname(warehouse_root)
  staging_root <- ensure_warehouse_directory(
    warehouse_root,
    data_root,
    "warehouse root"
  )
  staging_root <- ensure_warehouse_directory(
    file.path(staging_root, ".staging"),
    warehouse_root,
    "warehouse staging directory"
  )
  artifacts_root <- ensure_warehouse_directory(
    file.path(warehouse_root, "artifacts"),
    warehouse_root,
    "warehouse artifacts directory"
  )
  algorithm_root <- ensure_warehouse_directory(
    file.path(artifacts_root, "sha256"),
    warehouse_root,
    "warehouse SHA-256 directory"
  )

  digest <- sub("^sha256:", "", artifact$artifact_id)
  prefix_root <- ensure_warehouse_directory(
    file.path(algorithm_root, substr(digest, 1L, 2L)),
    warehouse_root,
    "warehouse hash-prefix directory"
  )

  list(
    staging = staging_root,
    artifact = warehouse_artifact_path(warehouse_root, artifact)
  )
}

warehouse_artifact_path <- function(warehouse_root, artifact) {
  digest <- sub("^sha256:", "", artifact$artifact_id)
  file.path(
    warehouse_root,
    "artifacts",
    "sha256",
    substr(digest, 1L, 2L),
    digest
  )
}

ensure_warehouse_directory <- function(path, anchor, label) {
  if (warehouse_path_is_link(path)) {
    stop(sprintf("The %s must not be a symbolic link.", label), call. = FALSE)
  }
  if (file.exists(path) && !dir.exists(path)) {
    stop(sprintf("The %s must identify a directory.", label), call. = FALSE)
  }
  if (!dir.exists(path) && !dir.create(path, recursive = FALSE)) {
    stop(sprintf("Unable to create the %s.", label), call. = FALSE)
  }

  resolved <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (!identical(path, resolved) || !path_is_within(anchor, resolved)) {
    stop(sprintf("The %s escapes its managed root.", label), call. = FALSE)
  }

  resolved
}

warehouse_path_exists <- function(path) {
  file.exists(path) || warehouse_path_is_link(path)
}

validate_existing_warehouse_artifact <- function(
  path,
  artifact,
  warehouse_root
) {
  if (warehouse_path_is_link(path)) {
    stop(
      "Existing warehouse artifact must not be a symbolic link.",
      call. = FALSE
    )
  }
  if (!file.exists(path) || !utils::file_test("-f", path) || dir.exists(path)) {
    stop(
      "Existing warehouse artifact must identify a regular file.",
      call. = FALSE
    )
  }

  resolved <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (!identical(path, resolved) || !path_is_within(warehouse_root, resolved)) {
    stop(
      "Existing warehouse artifact escapes the warehouse root.",
      call. = FALSE
    )
  }
  observed_sha256 <- digest::digest(
    resolved,
    algo = "sha256",
    file = TRUE,
    serialize = FALSE
  )
  if (!identical(observed_sha256, artifact$sha256)) {
    stop(
      "Existing warehouse artifact does not match its identity.",
      call. = FALSE
    )
  }

  invisible(resolved)
}

warehouse_path_is_link <- function(path) {
  target <- Sys.readlink(path)
  length(target) == 1L && !is.na(target) && nzchar(target)
}

warehouse_copy_file <- function(from, to, ...) {
  file.copy(from, to, ...)
}

warehouse_publish_link <- function(from, to) {
  file.link(from, to)
}

new_warehouse_promotion <- function(
  artifact,
  source_path,
  warehouse_path,
  transfer_policy,
  reused
) {
  structure(
    list(
      artifact_id = artifact$artifact_id,
      source_path = source_path,
      warehouse_path = normalizePath(
        warehouse_path,
        winslash = "/",
        mustWork = TRUE
      ),
      transfer_policy = transfer_policy,
      reused = reused
    ),
    class = "revdeprunner_warehouse_promotion"
  )
}

# nolint end
