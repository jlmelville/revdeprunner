validate_package_archive_name <- function(archive_name) {
  if (
    !is.character(archive_name) ||
      length(archive_name) != 1L ||
      is.na(archive_name) ||
      !nzchar(archive_name) ||
      !identical(archive_name, basename(archive_name))
  ) {
    stop("`archive_name` must be one archive basename.", call. = FALSE)
  }
  package_archive_suffix(archive_name)
  archive_name
}

normalize_artifact_path <- function(path, path_plan) {
  if (
    !is.character(path) ||
      length(path) != 1L ||
      is.na(path) ||
      !nzchar(path)
  ) {
    stop("`path` must be one non-empty path.", call. = FALSE)
  }

  path <- path.expand(path)
  if (!runtime_path_is_absolute(path) || grepl("\\\\", path)) {
    stop("`path` must be an absolute forward-slash path.", call. = FALSE)
  }
  if (path_is_link(path)) {
    stop("The artifact path must not be a symbolic link.", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop("The artifact path must identify an existing file.", call. = FALSE)
  }

  resolved <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (!identical(path, resolved)) {
    stop("`path` must be a resolved physical path.", call. = FALSE)
  }
  if (!utils::file_test("-f", resolved) || dir.exists(resolved)) {
    stop("The artifact path must identify a regular file.", call. = FALSE)
  }
  if (file.access(resolved, mode = 4L) != 0L) {
    stop("The artifact path must be readable.", call. = FALSE)
  }

  approved_roots <- c(
    path_plan$source_cache_roots,
    runtime_role_path(path_plan, "run"),
    file.path(runtime_role_path(path_plan, "binary-cache"), "src", "contrib"),
    file.path(runtime_role_path(path_plan, "source-cache"), "src", "contrib")
  )
  contained <- vapply(
    approved_roots,
    path_is_within,
    logical(1L),
    path = resolved
  )
  if (!any(contained)) {
    stop("The artifact must remain within a cache or run root.", call. = FALSE)
  }

  resolved
}

artifact_file_snapshot <- function(path) {
  info <- file.info(path, extra_cols = FALSE)
  if (
    nrow(info) != 1L ||
      is.na(info$isdir) ||
      info$isdir ||
      is.na(info$size) ||
      is.na(info$mtime) ||
      is.na(info$mode)
  ) {
    stop("Unable to observe the complete artifact file.", call. = FALSE)
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

validate_artifact_file_unchanged <- function(path, before) {
  after <- artifact_file_snapshot(path)
  if (!identical(after, before)) {
    stop("The artifact file changed while it was in use.", call. = FALSE)
  }

  invisible(after)
}

validate_package_archive <- function(
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

package_archive_suffix <- function(path) {
  matched <- regexpr(
    "\\.(?:tar\\.(?:gz|bz2|xz)|tgz|zip)$",
    basename(path),
    ignore.case = TRUE,
    perl = TRUE
  )
  if (matched[[1L]] == -1L) {
    stop("Package archive has an unsupported suffix.", call. = FALSE)
  }

  substring(basename(path), matched)
}

path_is_link <- function(path) {
  target <- Sys.readlink(path)
  length(target) == 1L && !is.na(target) && nzchar(target)
}
