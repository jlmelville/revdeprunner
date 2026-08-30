# These private tests protect the first network-to-warehouse boundary. They use
# tiny local source repositories and never contact an external service.

# nolint start: object_usage_linter.
make_source_download_fixture <- function(
  build_archive_package = "BuildPkg",
  corrupt_build = FALSE,
  build_md5 = NULL,
  build_filename = "BuildPkg_2.0.tar.gz",
  planned_build_file = NA_character_
) {
  fixture <- make_source_acquisition_fixture()
  repository_root <- file.path(fixture$root, "repository")
  secondary_root <- file.path(fixture$root, "secondary-repository")
  dir.create(repository_root)
  dir.create(secondary_root)

  build_archive <- make_test_archive(
    repository_root,
    "src/contrib",
    build_archive_package,
    "2.0",
    "yes",
    filename = build_filename
  )
  if (corrupt_build) {
    writeBin(charToRaw("not a source archive"), build_archive)
  }
  file_archive <- make_test_archive(
    repository_root,
    "src/contrib/custom",
    "FilePkg",
    "3.0",
    "no",
    filename = "FilePkg_3.0.tar.gz"
  )

  primary <- paste0(
    "file://",
    normalizePath(
      file.path(repository_root, "src/contrib"),
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
  observed_build_md5 <- digest::digest(
    build_archive,
    algo = "md5",
    file = TRUE,
    serialize = FALSE
  )
  database$MD5sum[
    database$Package == "BuildPkg" & database$Version == "2.0"
  ] <- if (is.null(build_md5)) observed_build_md5 else build_md5
  database$File[
    database$Package == "BuildPkg" & database$Version == "2.0"
  ] <- planned_build_file
  database$MD5sum[database$Package == "FilePkg"] <- NA_character_

  contracts <- source_acquisition_fixture_contracts(database, repositories)
  plan <- build_source_acquisition_plan(fixture, contracts)
  fixture$download_contracts <- contracts
  fixture$source_plan <- plan
  fixture$repository_root <- repository_root
  fixture$build_archive <- build_archive
  fixture$file_archive <- file_archive
  fixture
}

acquire_fixture_source <- function(fixture, package, previous = NULL) {
  contracts <- fixture$download_contracts
  revdeprunner:::acquire_source_artifact(
    package,
    fixture$source_plan,
    contracts$universe,
    contracts$cohort,
    contracts$snapshot,
    fixture$binary_reuse,
    fixture$lane,
    fixture$path_plan,
    previous
  )
}

validate_fixture_acquisition <- function(fixture, acquisition) {
  contracts <- fixture$download_contracts
  revdeprunner:::validate_source_acquisition(
    acquisition,
    fixture$source_plan,
    contracts$universe,
    contracts$cohort,
    contracts$snapshot,
    fixture$binary_reuse,
    fixture$lane,
    fixture$path_plan
  )
}

source_download_staging_files <- function(fixture) {
  staging <- file.path(
    fixture$paths[[3L]],
    fixture$path_plan$run_id,
    "source-downloads"
  )
  if (!dir.exists(staging)) {
    return(character())
  }
  list.files(staging, all.files = TRUE, no.. = TRUE, full.names = TRUE)
}

source_warehouse_snapshot <- function(fixture) {
  warehouse <- file.path(fixture$paths[[2L]], "warehouse")
  if (!dir.exists(warehouse)) {
    return(data.frame())
  }
  snapshot_test_cache(warehouse)
}
# nolint end

test_that("source acquisition downloads, validates, promotes, and reuses", {
  fixture <- make_source_download_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  repository_before <- snapshot_test_cache(fixture$repository_root)
  cache_before <- snapshot_test_cache(fixture$paths[[5L]])

  acquisition <- acquire_fixture_source(fixture, "BuildPkg")

  expect_s3_class(acquisition, "revdeprunner_source_acquisition")
  expect_identical(
    names(acquisition),
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
    )
  )
  expect_identical(
    acquisition$schema_version,
    "revdeprunner-source-acquisition/v1"
  )
  expect_match(acquisition$acquisition_id, "^sha256:[a-f0-9]{64}$")
  expect_identical(acquisition$package, "BuildPkg")
  expect_identical(acquisition$version, "2.0")
  expect_identical(acquisition$artifact$archive_type, "source")
  expect_true(is.na(acquisition$artifact$lane_id))
  expect_identical(
    acquisition$artifact$sha256,
    digest::digest(
      fixture$build_archive,
      algo = "sha256",
      file = TRUE,
      serialize = FALSE
    )
  )
  expect_true(file.exists(acquisition$warehouse_path))
  expect_invisible(validate_fixture_acquisition(fixture, acquisition))
  expect_length(source_download_staging_files(fixture), 0L)
  expect_identical(
    snapshot_test_cache(fixture$repository_root),
    repository_before
  )
  expect_identical(snapshot_test_cache(fixture$paths[[5L]]), cache_before)

  testthat::local_mocked_bindings(
    source_download_file = function(url, destination) {
      stop("the downloader must not run", call. = FALSE)
    },
    .package = "revdeprunner"
  )
  reused <- acquire_fixture_source(fixture, "BuildPkg", acquisition)
  expect_identical(reused, acquisition)
  expect_length(source_download_staging_files(fixture), 0L)
})

test_that("source acquisition supports an absent repository MD5", {
  fixture <- make_source_download_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)

  acquisition <- acquire_fixture_source(fixture, "FilePkg")

  expect_true(is.na(acquisition$expected_md5))
  expect_identical(
    acquisition$source_url,
    paste0(
      "file://",
      normalizePath(
        file.path(fixture$repository_root, "src/contrib"),
        winslash = "/",
        mustWork = TRUE
      ),
      "/custom/FilePkg_3.0.tar.gz"
    )
  )
  expect_identical(acquisition$artifact$package, "FilePkg")
  expect_invisible(validate_fixture_acquisition(fixture, acquisition))
  expect_length(source_download_staging_files(fixture), 0L)
})

test_that("source acquisition cleans failed and partial downloads", {
  fixture <- make_source_download_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  warehouse_before <- source_warehouse_snapshot(fixture)
  testthat::local_mocked_bindings(
    source_download_file = function(url, destination) {
      writeBin(charToRaw("partial download"), destination)
      7L
    },
    .package = "revdeprunner"
  )

  expect_error(
    acquire_fixture_source(fixture, "BuildPkg"),
    "nonzero status",
    fixed = TRUE
  )
  expect_length(source_download_staging_files(fixture), 0L)
  expect_identical(source_warehouse_snapshot(fixture), warehouse_before)
})

test_that("source acquisition rejects checksum and archive mismatches", {
  wrong_md5 <- make_source_download_fixture(build_md5 = strrep("0", 32L))
  on.exit(unlink(wrong_md5$root, recursive = TRUE), add = TRUE)
  wrong_md5_before <- source_warehouse_snapshot(wrong_md5)
  expect_error(
    acquire_fixture_source(wrong_md5, "BuildPkg"),
    "MD5 does not match",
    fixed = TRUE
  )
  expect_length(source_download_staging_files(wrong_md5), 0L)
  expect_identical(source_warehouse_snapshot(wrong_md5), wrong_md5_before)

  corrupt <- make_source_download_fixture(corrupt_build = TRUE)
  on.exit(unlink(corrupt$root, recursive = TRUE), add = TRUE)
  corrupt_before <- source_warehouse_snapshot(corrupt)
  expect_error(
    acquire_fixture_source(corrupt, "BuildPkg"),
    "archive validation failed",
    fixed = TRUE
  )
  expect_length(source_download_staging_files(corrupt), 0L)
  expect_identical(source_warehouse_snapshot(corrupt), corrupt_before)

  wrong_package <- make_source_download_fixture(
    build_archive_package = "OtherPkg"
  )
  on.exit(unlink(wrong_package$root, recursive = TRUE), add = TRUE)
  wrong_package_before <- source_warehouse_snapshot(wrong_package)
  expect_error(
    acquire_fixture_source(wrong_package, "BuildPkg"),
    "archive validation failed",
    fixed = TRUE
  )
  expect_length(source_download_staging_files(wrong_package), 0L)
  expect_identical(
    source_warehouse_snapshot(wrong_package),
    wrong_package_before
  )

  mismatched_name <- make_source_download_fixture(
    build_filename = "MirrorBlob_9.9.tar.gz",
    planned_build_file = "MirrorBlob_9.9.tar.gz"
  )
  on.exit(unlink(mismatched_name$root, recursive = TRUE), add = TRUE)
  mismatched_name_before <- source_warehouse_snapshot(mismatched_name)
  expect_error(
    acquire_fixture_source(mismatched_name, "BuildPkg"),
    "archive validation failed",
    fixed = TRUE
  )
  expect_length(source_download_staging_files(mismatched_name), 0L)
  expect_identical(
    source_warehouse_snapshot(mismatched_name),
    mismatched_name_before
  )
})

test_that("source acquisition refuses absent packages and linked staging", {
  fixture <- make_source_download_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  warehouse_before <- source_warehouse_snapshot(fixture)
  expect_error(
    acquire_fixture_source(fixture, "MissingPkg"),
    "one available planned source",
    fixed = TRUE
  )
  expect_length(source_download_staging_files(fixture), 0L)
  expect_identical(source_warehouse_snapshot(fixture), warehouse_before)

  run_root <- file.path(fixture$paths[[3L]], fixture$path_plan$run_id)
  outside <- file.path(fixture$root, "outside-downloads")
  dir.create(run_root)
  dir.create(outside)
  linked <- suppressWarnings(
    file.symlink(outside, file.path(run_root, "source-downloads"))
  )
  if (isTRUE(linked)) {
    expect_error(
      acquire_fixture_source(fixture, "BuildPkg"),
      "must not be a symbolic link",
      fixed = TRUE
    )
  } else {
    succeed()
  }
  expect_length(list.files(outside, all.files = TRUE, no.. = TRUE), 0L)
  expect_identical(source_warehouse_snapshot(fixture), warehouse_before)
})

test_that("source acquisition validation rejects mutation and stale payloads", {
  fixture <- make_source_download_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  acquisition <- acquire_fixture_source(fixture, "BuildPkg")

  changed <- acquisition
  changed$source_url <- NULL
  expect_error(
    validate_fixture_acquisition(fixture, changed),
    "invalid structure",
    fixed = TRUE
  )

  changed <- acquisition
  changed$source_url <- sub("BuildPkg", "OtherPkg", changed$source_url)
  expect_error(
    validate_fixture_acquisition(fixture, changed),
    "does not match its planned source",
    fixed = TRUE
  )

  changed <- acquisition
  changed$acquisition_id <- paste0("sha256:", strrep("0", 64L))
  expect_error(
    validate_fixture_acquisition(fixture, changed),
    "identity does not match",
    fixed = TRUE
  )

  writeBin(charToRaw("changed"), acquisition$warehouse_path)
  testthat::local_mocked_bindings(
    source_download_file = function(url, destination) {
      stop("the downloader must not run", call. = FALSE)
    },
    .package = "revdeprunner"
  )
  expect_error(
    acquire_fixture_source(fixture, "BuildPkg", acquisition),
    "does not match its identity",
    fixed = TRUE
  )
  expect_error(
    validate_fixture_acquisition(fixture, acquisition),
    "does not match its identity",
    fixed = TRUE
  )
})
