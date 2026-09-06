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
  immutable_path <- pin_binary_cache_artifact(
    source_path,
    artifact,
    path_plan,
    archive_name
  )

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
      return(immutable_path)
    }
  }

  staged <- tempfile(
    pattern = ".binary-",
    tmpdir = cache_root,
    fileext = package_archive_suffix(archive_name)
  )
  on.exit(unlink(staged, force = TRUE), add = TRUE)
  copied <- file.copy(
    immutable_path,
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
  immutable_path
}

binary_cache_artifact_path <- function(artifact, path_plan, archive_name) {
  file.path(
    runtime_role_path(path_plan, "binary-cache"),
    "artifacts",
    artifact$sha256,
    archive_name
  )
}

pin_binary_cache_artifact <- function(
  source_path,
  artifact,
  path_plan,
  archive_name
) {
  path <- binary_cache_artifact_path(artifact, path_plan, archive_name)
  ensure_revdep_directory(dirname(path), "immutable binary storage")
  if (!file.exists(path)) {
    staged <- tempfile(
      ".binary-",
      tmpdir = dirname(path),
      fileext = package_archive_suffix(archive_name)
    )
    on.exit(unlink(staged, force = TRUE), add = TRUE)
    if (!file.copy(source_path, staged, copy.mode = FALSE, copy.date = FALSE)) {
      stop("Unable to stage an immutable binary.", call. = FALSE)
    }
    validate_package_archive(staged, artifact, archive_name)
    # Link publication is atomic and cannot overwrite a concurrently published file.
    if (!file.link(staged, path) && !file.exists(path)) {
      stop("Unable to publish an immutable binary.", call. = FALSE)
    }
  }
  validate_package_archive(path, artifact, archive_name)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

validate_binary_cache_artifact <- function(cache_path, artifact, path_plan) {
  cache_path <- normalize_artifact_path(cache_path, path_plan)
  expected <- binary_cache_artifact_path(
    artifact,
    path_plan,
    basename(cache_path)
  )
  if (!identical(cache_path, expected)) {
    stop("Binary cache publication path is invalid.", call. = FALSE)
  }
  validate_package_archive(cache_path, artifact, basename(cache_path))
  invisible(cache_path)
}
