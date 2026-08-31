# This file composes private helpers defined in other package source files.
# nolint start: object_usage_linter.

source_acquisition_schema_version <- function() {
  "revdeprunner-source-acquisition/v1"
}

acquire_source_artifact <- function(
  package,
  source_plan,
  universe,
  cohort,
  snapshot,
  binary_reuse,
  lane,
  path_plan,
  previous = NULL
) {
  validate_source_acquisition_plan(
    source_plan,
    universe,
    cohort,
    snapshot,
    binary_reuse,
    lane,
    path_plan
  )
  acquire_source_artifact_in_context(
    package,
    source_plan,
    path_plan,
    previous
  )
}

acquire_source_artifact_in_context <- function(
  package,
  source_plan,
  path_plan,
  previous = NULL
) {
  package <- validate_package_name(package)
  source <- source_acquisition_planned_row(source_plan, package)

  if (!is.null(previous)) {
    validate_source_acquisition_record(
      previous,
      source_plan,
      path_plan
    )
    if (!identical(previous$package, package)) {
      stop(
        "The previous source acquisition belongs to another package.",
        call. = FALSE
      )
    }
    return(previous)
  }

  archive_name <- source_acquisition_archive_name(source$source_url[[1L]])
  suffix <- warehouse_archive_suffix(archive_name)
  staging <- source_acquisition_staging_directory(path_plan)
  destination <- tempfile(
    pattern = paste0(".", package, "-"),
    tmpdir = staging,
    fileext = suffix
  )
  on.exit(unlink(destination, force = TRUE), add = TRUE)

  status <- source_download_file(source$source_url[[1L]], destination)
  if (
    !is.numeric(status) ||
      length(status) != 1L ||
      is.na(status) ||
      status != 0
  ) {
    stop("Source download returned a nonzero status.", call. = FALSE)
  }

  destination <- validate_source_download_payload(destination, staging)
  validate_source_acquisition_md5(
    destination,
    source$expected_md5[[1L]]
  )

  sha256 <- digest::digest(
    destination,
    algo = "sha256",
    file = TRUE,
    serialize = FALSE
  )
  artifact <- new_artifact_identity(
    source$package[[1L]],
    source$version[[1L]],
    sha256,
    "source"
  )
  validate_warehouse_archive(destination, artifact, archive_name)
  warehouse_path <- source_acquisition_warehouse_path(path_plan, artifact)
  acquisition <- new_source_acquisition(
    source_plan,
    source,
    artifact,
    warehouse_path,
    path_plan
  )

  promotion <- promote_warehouse_artifact(
    destination,
    artifact,
    path_plan
  )
  if (!identical(promotion$warehouse_path, acquisition$warehouse_path)) {
    stop(
      "Source promotion returned an unexpected warehouse path.",
      call. = FALSE
    )
  }

  validate_source_acquisition_record(
    acquisition,
    source_plan,
    path_plan
  )
  acquisition
}

validate_source_acquisition <- function(
  acquisition,
  source_plan,
  universe,
  cohort,
  snapshot,
  binary_reuse,
  lane,
  path_plan
) {
  validate_source_acquisition_plan(
    source_plan,
    universe,
    cohort,
    snapshot,
    binary_reuse,
    lane,
    path_plan
  )
  validate_source_acquisition_record(acquisition, source_plan, path_plan)
}

validate_source_acquisition_record <- function(
  acquisition,
  source_plan,
  path_plan
) {
  validate_composite_contract_record(
    acquisition,
    c(
      "schema_version",
      "acquisition_id",
      "source_plan_id",
      "path_plan_id",
      "package",
      "version",
      "source_url",
      "expected_md5",
      "artifact",
      "warehouse_path"
    ),
    "revdeprunner_source_acquisition",
    "source acquisition"
  )
  if (
    !identical(acquisition$schema_version, source_acquisition_schema_version())
  ) {
    stop("Source acquisition schema version is unsupported.", call. = FALSE)
  }
  validate_sha256_identity(acquisition$acquisition_id, "acquisition_id")
  validate_sha256_identity(acquisition$source_plan_id, "source_plan_id")
  validate_sha256_identity(acquisition$path_plan_id, "path_plan_id")
  validate_package_name(acquisition$package)
  validate_package_version(acquisition$version)
  validate_preparation_source_url(acquisition$source_url)
  if (
    !is.character(acquisition$expected_md5) ||
      length(acquisition$expected_md5) != 1L ||
      (!is.na(acquisition$expected_md5) &&
        !grepl("^[a-f0-9]{32}$", acquisition$expected_md5))
  ) {
    stop(
      "Source acquisition expected MD5 is invalid.",
      call. = FALSE
    )
  }
  validate_artifact_identity(acquisition$artifact)
  validate_contract_text(acquisition$warehouse_path, "warehouse_path")

  if (
    !identical(acquisition$source_plan_id, source_plan$source_plan_id) ||
      !identical(acquisition$path_plan_id, path_plan$path_plan_id)
  ) {
    stop("Source acquisition context bindings do not match.", call. = FALSE)
  }

  source <- source_acquisition_planned_row(
    source_plan,
    acquisition$package
  )
  planned <- c(
    package = source$package[[1L]],
    version = source$version[[1L]],
    source_url = source$source_url[[1L]],
    expected_md5 = source$expected_md5[[1L]]
  )
  observed <- unlist(
    acquisition[c("package", "version", "source_url", "expected_md5")],
    use.names = TRUE
  )
  if (!identical(observed, planned)) {
    stop(
      "Source acquisition does not match its planned source.",
      call. = FALSE
    )
  }

  artifact <- acquisition$artifact
  if (
    !identical(artifact$package, acquisition$package) ||
      !identical(artifact$version, acquisition$version) ||
      !identical(artifact$archive_type, "source") ||
      !is.na(artifact$lane_id)
  ) {
    stop(
      "Source acquisition artifact does not match its planned source.",
      call. = FALSE
    )
  }
  expected_path <- source_acquisition_warehouse_path(path_plan, artifact)
  if (!identical(acquisition$warehouse_path, expected_path)) {
    stop(
      "Source acquisition warehouse path is not deterministic.",
      call. = FALSE
    )
  }
  warehouse_root <- runtime_role_path(path_plan, "warehouse")
  validate_existing_warehouse_artifact(
    acquisition$warehouse_path,
    artifact,
    warehouse_root
  )
  archive_name <- source_acquisition_archive_name(acquisition$source_url)
  validate_warehouse_archive(
    acquisition$warehouse_path,
    artifact,
    archive_name
  )
  validate_source_acquisition_md5(
    acquisition$warehouse_path,
    acquisition$expected_md5
  )

  fields <- source_acquisition_identity_fields(
    acquisition$source_plan_id,
    acquisition$path_plan_id,
    acquisition$package,
    acquisition$version,
    acquisition$source_url,
    acquisition$expected_md5,
    acquisition$artifact$artifact_id,
    acquisition$warehouse_path
  )
  expected_id <- record_identity(acquisition$schema_version, fields)
  if (!identical(acquisition$acquisition_id, expected_id)) {
    stop(
      "Source acquisition identity does not match its fields.",
      call. = FALSE
    )
  }

  invisible(acquisition)
}

new_source_acquisition <- function(
  source_plan,
  source,
  artifact,
  warehouse_path,
  path_plan
) {
  schema_version <- source_acquisition_schema_version()
  fields <- source_acquisition_identity_fields(
    source_plan$source_plan_id,
    path_plan$path_plan_id,
    source$package[[1L]],
    source$version[[1L]],
    source$source_url[[1L]],
    source$expected_md5[[1L]],
    artifact$artifact_id,
    warehouse_path
  )
  structure(
    list(
      schema_version = schema_version,
      acquisition_id = record_identity(schema_version, fields),
      source_plan_id = source_plan$source_plan_id,
      path_plan_id = path_plan$path_plan_id,
      package = source$package[[1L]],
      version = source$version[[1L]],
      source_url = source$source_url[[1L]],
      expected_md5 = source$expected_md5[[1L]],
      artifact = artifact,
      warehouse_path = warehouse_path
    ),
    class = "revdeprunner_source_acquisition"
  )
}

source_acquisition_planned_row <- function(source_plan, package) {
  package <- validate_package_name(package)
  source <- source_plan$sources[
    source_plan$sources$package == package,
    ,
    drop = FALSE
  ]
  if (nrow(source) != 1L) {
    stop(
      "`package` must identify one available planned source.",
      call. = FALSE
    )
  }

  source
}

source_acquisition_staging_directory <- function(path_plan) {
  validate_runtime_root_plan(path_plan)
  run_root <- runtime_role_path(path_plan, "run")
  run_root <- ensure_source_acquisition_directory(
    run_root,
    path_plan$runs_root,
    "source acquisition run root"
  )
  staging <- ensure_source_acquisition_directory(
    file.path(run_root, "source-downloads"),
    run_root,
    "source download staging directory"
  )
  validate_runtime_root_plan(path_plan)
  staging
}

ensure_source_acquisition_directory <- function(path, anchor, label) {
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

validate_source_download_payload <- function(path, staging) {
  if (warehouse_path_is_link(path)) {
    stop(
      "Downloaded source payload must not be a symbolic link.",
      call. = FALSE
    )
  }
  if (!file.exists(path) || !utils::file_test("-f", path) || dir.exists(path)) {
    stop("Source download did not create one regular file.", call. = FALSE)
  }
  resolved <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (!identical(path, resolved) || !path_is_within(staging, resolved)) {
    stop("Downloaded source payload escapes its staging root.", call. = FALSE)
  }
  if (file.access(resolved, mode = 4L) != 0L) {
    stop("Downloaded source payload is not readable.", call. = FALSE)
  }

  resolved
}

validate_source_acquisition_md5 <- function(path, expected_md5) {
  if (is.na(expected_md5)) {
    return(invisible(NA_character_))
  }
  observed <- digest::digest(
    path,
    algo = "md5",
    file = TRUE,
    serialize = FALSE
  )
  if (!identical(observed, expected_md5)) {
    stop("Downloaded source MD5 does not match its plan.", call. = FALSE)
  }

  invisible(observed)
}

source_acquisition_archive_name <- function(source_url) {
  path <- sub("[?].*$", "", source_url)
  utils::URLdecode(basename(path))
}

source_acquisition_warehouse_path <- function(path_plan, artifact) {
  validate_artifact_identity(artifact)
  warehouse_root <- runtime_role_path(path_plan, "warehouse")
  digest <- sub("^sha256:", "", artifact$artifact_id)
  file.path(
    warehouse_root,
    "artifacts",
    "sha256",
    substr(digest, 1L, 2L),
    digest
  )
}

source_acquisition_identity_fields <- function(
  source_plan_id,
  path_plan_id,
  package,
  version,
  source_url,
  expected_md5,
  artifact_id,
  warehouse_path
) {
  c(
    source_plan_id = source_plan_id,
    path_plan_id = path_plan_id,
    package = package,
    version = version,
    source_url = source_url,
    expected_md5 = expected_md5,
    artifact_id = artifact_id,
    warehouse_path = warehouse_path
  )
}

source_download_file <- function(url, destination) {
  utils::download.file(
    url = url,
    destfile = destination,
    mode = "wb",
    quiet = TRUE
  )
}

# nolint end
