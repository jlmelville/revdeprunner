runner_source_cache_contrib_path <- function(data_root) {
  file.path(data_root, "source-cache", "src", "contrib")
}

runner_source_cache_contrib <- function(path_plan) {
  validate_runtime_root_plan(path_plan)
  contrib <- runner_source_cache_contrib_path(path_plan$data_root)
  validate_runtime_derived_path(contrib, path_plan$data_root, "source-cache")
  contrib
}

source_acquisition_archive_name <- function(source_url) {
  path <- sub("[?].*$", "", source_url)
  utils::URLdecode(basename(path))
}

source_cache_archive_name <- function(package, version, source_url) {
  paste0(
    package,
    "_",
    version,
    package_archive_suffix(source_acquisition_archive_name(source_url))
  )
}

publish_source_cache_artifact <- function(
  source_path,
  artifact,
  source_url,
  path_plan
) {
  validate_artifact_identity(artifact)
  if (!identical(artifact$archive_type, "source")) {
    stop("Only source artifacts belong in the source cache.", call. = FALSE)
  }
  archive_name <- source_cache_archive_name(
    artifact$package,
    artifact$version,
    source_url
  )
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
  if (!path_is_link(cache_path) && utils::file_test("-f", cache_path)) {
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

find_cached_source <- function(source, observations, path_plan) {
  if (is.na(source$expected_md5[[1L]])) {
    return(NULL)
  }
  candidates <- observations[
    observations$status == "ok" &
      observations$archive_type == "source" &
      observations$package %in% source$package &
      observations$version %in% source$version &
      !is.na(observations$sha256),
    ,
    drop = FALSE
  ]
  paths <- file.path(candidates$cache_root, candidates$relative_path)
  matches <- vapply(
    seq_along(paths),
    function(i) {
      path <- tryCatch(
        normalize_artifact_path(paths[[i]], path_plan),
        error = function(error) NULL
      )
      if (is.null(path) || !path_is_within(candidates$cache_root[[i]], path)) {
        return(FALSE)
      }
      before <- artifact_file_snapshot(path)
      md5 <- digest::digest(path, algo = "md5", file = TRUE, serialize = FALSE)
      validate_artifact_file_unchanged(path, before)
      identical(before$sha256, candidates$sha256[[i]]) &&
        identical(md5, source$expected_md5[[1L]])
    },
    logical(1L)
  )
  if (!any(matches)) {
    return(NULL)
  }
  candidates <- candidates[matches, , drop = FALSE]
  if (length(unique(candidates$sha256)) != 1L) {
    stop(
      sprintf(
        "Cached source identity is ambiguous for %s %s.",
        source$package[[1L]],
        source$version[[1L]]
      ),
      call. = FALSE
    )
  }
  first <- order(
    candidates$priority,
    candidates$relative_path,
    method = "radix"
  )[[1L]]
  list(
    path = paths[matches][[first]],
    package = source$package[[1L]],
    version = source$version[[1L]],
    sha256 = candidates$sha256[[first]]
  )
}
