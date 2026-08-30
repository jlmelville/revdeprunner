# These private fixtures exercise one real Linux source-to-binary preparation
# path while keeping repositories, libraries, logs, and the warehouse local.

# nolint start: object_usage_linter.
source_preparation_runner_lane <- function() {
  architecture <- R.version$arch
  if (is.null(architecture) || !nzchar(architecture)) {
    architecture <- sub("-.*$", "", R.version$platform)
  }
  os_abi <- R.version$os
  if (is.null(os_abi) || !nzchar(os_abi)) {
    os_abi <- tolower(Sys.info()[["sysname"]])
  }
  revdeprunner:::new_compatibility_lane(
    sub("^([0-9]+[.][0-9]+).*$", "\\1", as.character(getRversion())),
    R.version$platform,
    architecture,
    os_abi,
    "fixture-toolchain"
  )
}

make_installable_source_archive <- function(repository_root) {
  staging_root <- tempfile("source-preparation-package-")
  dir.create(staging_root)
  on.exit(unlink(staging_root, recursive = TRUE), add = TRUE)
  package_root <- file.path(staging_root, "BuildPkg")
  dir.create(package_root)
  dir.create(file.path(package_root, "R"))
  dir.create(file.path(package_root, "src"))
  writeLines(
    c(
      "Package: BuildPkg",
      "Type: Package",
      "Title: Source Preparation Fixture",
      "Version: 2.0",
      paste0(
        "Authors@R: person('Fixture', 'Author', role = c('aut', 'cre'), ",
        "email = 'fixture@example.test')"
      ),
      "Description: A compiled fixture for source preparation tests.",
      "License: MIT",
      "Encoding: UTF-8",
      "NeedsCompilation: yes"
    ),
    file.path(package_root, "DESCRIPTION")
  )
  writeLines(
    c("useDynLib(BuildPkg)", "export(build_value)"),
    file.path(package_root, "NAMESPACE")
  )
  writeLines(
    "build_value <- function() 42L",
    file.path(package_root, "R", "build.R")
  )
  writeLines(
    "void buildpkg_noop(void) {}",
    file.path(package_root, "src", "build.c")
  )

  archive_root <- file.path(repository_root, "src", "contrib")
  dir.create(archive_root, recursive = TRUE)
  archive <- file.path(archive_root, "BuildPkg_2.0.tar.gz")
  old_working_directory <- setwd(staging_root)
  on.exit(setwd(old_working_directory), add = TRUE)
  utils::tar(
    archive,
    files = "BuildPkg",
    compression = "gzip",
    tar = "internal"
  )
  normalizePath(archive, winslash = "/", mustWork = TRUE)
}

make_source_preparation_fixture <- function(
  missing_binary_packages = character()
) {
  fixture <- make_source_acquisition_fixture(
    run_id = "run-20260829-wp3f",
    missing_binary_packages = missing_binary_packages,
    lane = source_preparation_runner_lane()
  )
  repository_root <- file.path(fixture$root, "repository")
  secondary_root <- file.path(fixture$root, "secondary-repository")
  dir.create(repository_root)
  dir.create(secondary_root)
  source_archive <- make_installable_source_archive(repository_root)

  primary <- paste0(
    "file://",
    normalizePath(
      file.path(repository_root, "src", "contrib"),
      winslash = "/",
      mustWork = TRUE
    )
  )
  secondary <- paste0(
    "file://",
    normalizePath(secondary_root, winslash = "/", mustWork = TRUE)
  )
  repositories <- c(CRAN = primary, Secondary = secondary)
  database <- source_acquisition_fixture_database()
  original <- source_acquisition_fixture_repositories()
  database$Repository[database$Repository == original[["CRAN"]]] <- primary
  database$Repository[database$Repository == original[["Secondary"]]] <-
    secondary
  database$MD5sum[
    database$Package == "BuildPkg" & database$Version == "2.0"
  ] <- digest::digest(
    source_archive,
    algo = "md5",
    file = TRUE,
    serialize = FALSE
  )
  contracts <- source_acquisition_fixture_contracts(database, repositories)
  source_plan <- build_source_acquisition_plan(fixture, contracts)
  command_plan <- revdeprunner:::new_command_plan(
    "prepare",
    fixture$path_plan,
    file.path(R.home("bin"), "R"),
    FALSE,
    contracts$snapshot,
    contracts$cohort,
    contracts$universe,
    fixture$lane
  )

  fixture$download_contracts <- contracts
  fixture$source_plan <- source_plan
  fixture$command_plan <- command_plan
  fixture$repository_root <- repository_root
  fixture$source_archive <- source_archive
  fixture
}

source_preparation_context <- function(fixture) {
  contracts <- fixture$download_contracts
  list(
    source_plan = fixture$source_plan,
    universe = contracts$universe,
    cohort = contracts$cohort,
    snapshot = contracts$snapshot,
    binary_reuse = fixture$binary_reuse,
    lane = fixture$lane,
    path_plan = fixture$path_plan,
    command_plan = fixture$command_plan
  )
}

prepare_fixture_source_binary <- function(
  fixture,
  source_acquisition = NULL,
  previous = NULL,
  timeout_seconds = 60L
) {
  context <- source_preparation_context(fixture)
  if (is.null(source_acquisition)) {
    source_acquisition <- if (is.null(previous)) {
      acquire_fixture_build_source(fixture)
    } else {
      previous$source_acquisition
    }
  }
  revdeprunner:::prepare_source_binary(
    "BuildPkg",
    context$source_plan,
    context$universe,
    context$cohort,
    context$snapshot,
    context$binary_reuse,
    context$lane,
    context$path_plan,
    context$command_plan,
    source_acquisition,
    previous,
    timeout_seconds
  )
}

acquire_fixture_build_source <- function(fixture) {
  contracts <- fixture$download_contracts
  revdeprunner:::acquire_source_artifact(
    "BuildPkg",
    fixture$source_plan,
    contracts$universe,
    contracts$cohort,
    contracts$snapshot,
    fixture$binary_reuse,
    fixture$lane,
    fixture$path_plan
  )
}

source_preparation_run_root <- function(fixture) {
  file.path(fixture$paths[[3L]], fixture$path_plan$run_id)
}

source_preparation_warehouse_snapshot <- function(fixture) {
  warehouse <- file.path(fixture$paths[[2L]], "warehouse")
  if (!dir.exists(warehouse)) {
    return(data.frame())
  }
  snapshot_test_cache(warehouse)
}

mock_source_preparation_process <- function(
  message,
  status,
  timed_out = FALSE,
  create_binary = NULL
) {
  force(message)
  force(status)
  force(timed_out)
  force(create_binary)
  function(
    r_executable,
    arguments,
    working_directory,
    stdout_path,
    stderr_path,
    timeout_seconds
  ) {
    writeLines("fixture stdout", stdout_path)
    writeLines(message, stderr_path)
    if (!is.null(create_binary)) {
      create_binary(working_directory)
    }
    list(
      command = revdeprunner:::render_source_preparation_command(
        r_executable,
        arguments
      ),
      started_at = "2026-08-29T12:00:00.000000Z",
      duration_ms = 10,
      status = as.integer(status),
      timed_out = timed_out,
      warnings = if (timed_out) "command timed out" else character()
    )
  }
}
# nolint end
