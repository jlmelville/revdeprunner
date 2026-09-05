runner_binary_cache_contrib_path <- function(data_root) {
  file.path(data_root, "binary-cache", "src", "contrib")
}

runner_binary_cache_contrib <- function(path_plan) {
  validate_runtime_root_plan(path_plan)
  file.path(runtime_role_path(path_plan, "binary-cache"), "src", "contrib")
}

publish_binary_cache_artifact <- function(
  source_path,
  artifact,
  path_plan,
  archive_name = basename(source_path)
) {
  validate_artifact_identity(artifact)
  if (!identical(artifact$archive_type, "binary")) {
    stop(
      "Only binary artifacts can be published to the runner cache.",
      call. = FALSE
    )
  }
  source_path <- normalize_artifact_path(source_path, path_plan)
  archive_name <- validate_package_archive_name(archive_name)
  validate_package_archive(source_path, artifact, archive_name)

  cache_root <- ensure_revdep_directory(
    runner_binary_cache_contrib(path_plan),
    "runner binary cache"
  )
  cache_path <- file.path(cache_root, archive_name)
  if (file.exists(cache_path)) {
    observed <- digest::digest(
      cache_path,
      algo = "sha256",
      file = TRUE,
      serialize = FALSE
    )
    if (identical(observed, artifact$sha256)) {
      cranlike::add_PACKAGES(archive_name, dir = cache_root)
      return(normalizePath(cache_path, winslash = "/", mustWork = TRUE))
    }
  }

  staged <- tempfile(
    pattern = ".binary-",
    tmpdir = cache_root,
    fileext = package_archive_suffix(archive_name)
  )
  on.exit(unlink(staged, force = TRUE), add = TRUE)
  copied <- file.copy(
    source_path,
    staged,
    overwrite = FALSE,
    copy.mode = FALSE,
    copy.date = FALSE
  )
  if (!isTRUE(copied)) {
    stop("Unable to copy the binary into runner cache staging.", call. = FALSE)
  }
  if (!file.rename(staged, cache_path)) {
    stop("Unable to publish the binary to the runner cache.", call. = FALSE)
  }
  cranlike::add_PACKAGES(archive_name, dir = cache_root)
  normalizePath(cache_path, winslash = "/", mustWork = TRUE)
}

validate_binary_cache_artifact <- function(cache_path, artifact, path_plan) {
  cache_root <- runner_binary_cache_contrib(path_plan)
  cache_path <- normalize_artifact_path(cache_path, path_plan)
  if (!identical(dirname(cache_path), cache_root)) {
    stop("Binary cache publication path is invalid.", call. = FALSE)
  }
  validate_package_archive(cache_path, artifact, basename(cache_path))
  invisible(cache_path)
}
