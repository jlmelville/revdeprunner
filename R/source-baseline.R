acquire_revdep_baseline <- function(cohort, snapshot, data_root, path_plan) {
  package_row <- revdep_plan_baseline(cohort$package, snapshot)
  if (nrow(package_row) != 1L) {
    stop("The frozen package baseline is unavailable.", call. = FALSE)
  }
  source_url <- source_acquisition_url(package_row)
  archive_name <- source_acquisition_archive_name(source_url)
  directory <- ensure_revdep_directory(
    file.path(
      data_root,
      "baselines",
      cohort$package,
      package_row$Version[[1L]]
    ),
    "baseline directory"
  )
  path <- file.path(directory, archive_name)
  if (file.exists(path)) {
    return(validate_baseline_source(path, cohort, snapshot))
  }

  staging <- tempfile(".baseline-", tmpdir = directory)
  if (!dir.create(staging)) {
    stop("Unable to create baseline staging state.", call. = FALSE)
  }
  on.exit(unlink(staging, recursive = TRUE, force = TRUE), add = TRUE)
  temporary <- file.path(staging, archive_name)
  download_preparation_source(
    source_url,
    temporary,
    cohort$package,
    package_row$Version[[1L]],
    path_plan
  )
  validate_baseline_source(temporary, cohort, snapshot)
  if (!file.rename(temporary, path)) {
    if (file.exists(path)) {
      return(validate_baseline_source(path, cohort, snapshot))
    }
    stop("Unable to publish the frozen package baseline.", call. = FALSE)
  }
  validate_baseline_source(path, cohort, snapshot)
}

validate_baseline_source <- function(path, cohort, snapshot) {
  validate_reverse_dependency_cohort(cohort, snapshot)
  path <- normalize_regular_artifact_file(path, "baseline source archive")
  filename <- archive_filename_fields(basename(path))
  metadata <- read_archive_metadata(path, filename)
  package_rows <- revdep_plan_baseline(cohort$package, snapshot)
  if (
    nrow(package_rows) != 1L ||
      !identical(metadata$status, "ok") ||
      !identical(metadata$archive_type, "source") ||
      !identical(metadata$package, cohort$package) ||
      !identical(metadata$version, package_rows$Version[[1L]])
  ) {
    stop(
      "Baseline source archive does not match the frozen package version.",
      call. = FALSE
    )
  }
  expected_md5 <- source_acquisition_md5(package_rows)
  if (is.na(expected_md5)) {
    stop(
      "The frozen baseline source checksum is unavailable.",
      call. = FALSE
    )
  }
  observed_md5 <- digest::digest(
    path,
    algo = "md5",
    file = TRUE,
    serialize = FALSE
  )
  if (!identical(observed_md5, expected_md5)) {
    stop(
      "Baseline source archive differs from its frozen checksum.",
      call. = FALSE
    )
  }
  list(
    path = path,
    package = metadata$package,
    version = metadata$version,
    md5 = observed_md5,
    sha256 = digest::digest(
      path,
      algo = "sha256",
      file = TRUE,
      serialize = FALSE
    )
  )
}

validate_baseline_binding <- function(baseline, cohort, snapshot) {
  fields <- c("path", "package", "version", "md5", "sha256")
  package_rows <- revdep_plan_baseline(cohort$package, snapshot)
  if (
    !is.list(baseline) ||
      !identical(names(baseline), fields) ||
      nrow(package_rows) != 1L ||
      !identical(baseline$package, cohort$package) ||
      !identical(baseline$version, package_rows$Version[[1L]])
  ) {
    stop("Stock baseline source evidence changed.", call. = FALSE)
  }
  path <- normalize_regular_artifact_file(
    baseline$path,
    "baseline source archive"
  )
  expected_md5 <- source_acquisition_md5(package_rows)
  observed <- c(
    md5 = digest::digest(
      path,
      algo = "md5",
      file = TRUE,
      serialize = FALSE
    ),
    sha256 = digest::digest(
      path,
      algo = "sha256",
      file = TRUE,
      serialize = FALSE
    )
  )
  if (
    !identical(baseline$path, path) ||
      !identical(baseline$md5, expected_md5) ||
      !identical(unlist(baseline[c("md5", "sha256")]), observed)
  ) {
    stop("Stock baseline source evidence changed.", call. = FALSE)
  }
  invisible(baseline)
}
