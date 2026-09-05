runner_source_cache_contrib_path <- function(data_root) {
  file.path(data_root, "source-cache", "src", "contrib")
}

runner_source_cache_contrib <- function(path_plan) {
  validate_runtime_root_plan(path_plan)
  file.path(runtime_role_path(path_plan, "source-cache"), "src", "contrib")
}

source_acquisition_archive_name <- function(source_url) {
  path <- sub("[?].*$", "", source_url)
  utils::URLdecode(basename(path))
}

source_cache_archive_name <- function(artifact, source_url) {
  validate_artifact_identity(artifact)
  if (!identical(artifact$archive_type, "source")) {
    stop("Only source artifacts belong in the source cache.", call. = FALSE)
  }
  paste0(
    artifact$package,
    "_",
    artifact$version,
    package_archive_suffix(source_acquisition_archive_name(source_url))
  )
}

publish_source_cache_artifact <- function(
  source_path,
  artifact,
  source_url,
  path_plan
) {
  archive_name <- source_cache_archive_name(artifact, source_url)
  source_path <- normalize_artifact_path(source_path, path_plan)
  validate_package_archive(
    source_path,
    artifact,
    source_acquisition_archive_name(source_url)
  )

  cache_root <- ensure_revdep_directory(
    runner_source_cache_contrib(path_plan),
    "runner source cache"
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
    pattern = ".source-",
    tmpdir = cache_root,
    fileext = package_archive_suffix(archive_name)
  )
  on.exit(unlink(staged, force = TRUE), add = TRUE)
  if (
    !file.copy(
      source_path,
      staged,
      overwrite = FALSE,
      copy.mode = FALSE,
      copy.date = FALSE
    )
  ) {
    stop("Unable to copy the source into runner cache staging.", call. = FALSE)
  }
  validate_package_archive(staged, artifact, archive_name)
  if (!file.rename(staged, cache_path)) {
    stop("Unable to publish the source to the runner cache.", call. = FALSE)
  }
  cranlike::add_PACKAGES(archive_name, dir = cache_root)
  cache_path <- normalizePath(cache_path, winslash = "/", mustWork = TRUE)
  validate_package_archive(cache_path, artifact, archive_name)
  cache_path
}
