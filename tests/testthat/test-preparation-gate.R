# These private tests protect dependency-ordered preparation and resumable
# report assembly without exposing an unfinished public API.

# nolint start: object_usage_linter.
preparation_gate_context <- function(fixture) {
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

run_preparation_gate_fixture <- function(
  fixture,
  previous = NULL,
  timeout_seconds = 60L
) {
  do.call(
    revdeprunner:::prepare_dependency_universe,
    c(
      preparation_gate_context(fixture),
      list(previous = previous, timeout_seconds = timeout_seconds)
    )
  )
}

preparation_gate_outcome <- function(gate, package) {
  gate$report$results$outcome[gate$report$results$package == package]
}

test_that("the preparation gate returns complete dependency-ordered evidence", {
  fixture <- make_source_preparation_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  real_download <- revdeprunner:::source_download_file
  download_urls <- character()
  testthat::local_mocked_bindings(
    source_download_file = function(url, destination) {
      download_urls <<- c(download_urls, url)
      real_download(url, destination)
    },
    .package = "revdeprunner"
  )

  gate <- run_preparation_gate_fixture(fixture)
  context <- preparation_gate_context(fixture)

  expect_identical(
    gate$execution_order,
    c("BuildPkg", "FilePkg", "MissingPkg", "HitPkg")
  )
  expect_identical(preparation_gate_outcome(gate, "BuildPkg"), "prepared")
  expect_identical(preparation_gate_outcome(gate, "FilePkg"), "prepared")
  expect_identical(preparation_gate_outcome(gate, "MissingPkg"), "unavailable")
  expect_identical(preparation_gate_outcome(gate, "HitPkg"), "blocked")
  expect_identical(
    gate$report$results$blocking_dependency[
      gate$report$results$package == "HitPkg"
    ],
    "MissingPkg"
  )
  expect_identical(
    names(gate$source_acquisitions),
    "BuildPkg"
  )
  expect_length(download_urls, 1L)
  expect_match(download_urls, "BuildPkg_2.0.tar.gz", fixed = TRUE)
  expect_identical(names(gate$source_preparations), "BuildPkg")
  expect_identical(nrow(gate$report$results), 4L)
  expect_identical(nrow(gate$report$sources), 1L)
  expect_invisible(
    revdeprunner:::validate_preparation_report(
      gate$report,
      context$universe,
      context$cohort,
      context$snapshot,
      context$lane
    )
  )
  expect_invisible(
    revdeprunner:::validate_preparation_gate(gate, context)
  )

  invalid <- gate
  invalid$execution_order <- rev(invalid$execution_order)
  testthat::local_mocked_bindings(
    source_download_file = function(...) {
      stop("the downloader must not run", call. = FALSE)
    },
    run_source_preparation_process = function(...) {
      stop("the R command must not run", call. = FALSE)
    },
    .package = "revdeprunner"
  )
  expect_error(
    run_preparation_gate_fixture(fixture, previous = invalid),
    "execution order is inconsistent",
    fixed = TRUE
  )

  changed_context <- context
  changed_context$snapshot$packages$Version[[1L]] <- "999.0"
  expect_error(
    revdeprunner:::validate_preparation_gate(gate, changed_context),
    "snapshot",
    fixed = TRUE
  )
})

test_that("the gate validates its complete source plan once", {
  fixture <- make_source_preparation_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  real_validator <- revdeprunner:::validate_source_acquisition_plan
  validation_calls <- 0L
  testthat::local_mocked_bindings(
    validate_source_acquisition_plan = function(...) {
      validation_calls <<- validation_calls + 1L
      real_validator(...)
    },
    .package = "revdeprunner"
  )

  gate <- run_preparation_gate_fixture(fixture)

  expect_s3_class(gate$report, "revdeprunner_preparation_report")
  expect_identical(validation_calls, 1L)
})

test_that("source evidence can be empty when no source archive is acquired", {
  fixture <- make_source_preparation_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)

  sources <- revdeprunner:::preparation_gate_source_rows(
    list(),
    fixture$source_plan
  )

  expect_identical(names(sources), revdeprunner:::preparation_source_fields())
  expect_identical(nrow(sources), 0L)
})

test_that("timeouts continue independent work and block transitive dependents", {
  database <- source_acquisition_fixture_database()
  database$Imports[database$Package == "FilePkg"] <- "HitPkg"
  fixture <- make_source_preparation_fixture(database = database)
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  testthat::local_mocked_bindings(
    run_source_preparation_process = mock_source_preparation_process(
      "build exceeded its preparation deadline",
      124L,
      timed_out = TRUE
    ),
    .package = "revdeprunner"
  )

  gate <- run_preparation_gate_fixture(fixture)

  expect_identical(preparation_gate_outcome(gate, "BuildPkg"), "timeout")
  expect_identical(preparation_gate_outcome(gate, "MissingPkg"), "unavailable")
  expect_identical(preparation_gate_outcome(gate, "HitPkg"), "blocked")
  expect_identical(preparation_gate_outcome(gate, "FilePkg"), "blocked")
  expect_identical(
    gate$report$results$blocking_dependency[
      gate$report$results$package == "FilePkg"
    ],
    "HitPkg"
  )
  expect_match(
    gate$report$results$diagnostic_excerpt[
      gate$report$results$package == "BuildPkg"
    ],
    "preparation deadline",
    fixed = TRUE
  )
})

test_that("a gate with only hits and blockers needs no source build", {
  database <- source_acquisition_fixture_database()
  database$Imports[database$Package == "BuildPkg"] <-
    "SubjectPkg, MissingPkg"
  fixture <- make_source_preparation_fixture(database = database)
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  testthat::local_mocked_bindings(
    run_source_preparation_process = function(...) {
      stop("the R command must not run", call. = FALSE)
    },
    .package = "revdeprunner"
  )

  gate <- run_preparation_gate_fixture(fixture)

  expect_length(gate$source_preparations, 0L)
  expect_identical(preparation_gate_outcome(gate, "FilePkg"), "prepared")
  expect_identical(preparation_gate_outcome(gate, "MissingPkg"), "unavailable")
  expect_identical(preparation_gate_outcome(gate, "BuildPkg"), "blocked")
  expect_identical(preparation_gate_outcome(gate, "HitPkg"), "blocked")
  expect_identical(nrow(gate$report$attempts), 0L)
})

test_that("retries replace failures while preserving successes and history", {
  fixture <- make_source_preparation_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  failed <- testthat::with_mocked_bindings(
    run_preparation_gate_fixture(fixture),
    run_source_preparation_process = mock_source_preparation_process(
      "configure: error: install libfixture-dev",
      1L
    ),
    .package = "revdeprunner"
  )
  expect_identical(
    preparation_gate_outcome(failed, "BuildPkg"),
    "compilation-failure"
  )
  failed_attempt_id <- failed$report$results$evidence_attempt_id[
    failed$report$results$package == "BuildPkg"
  ]

  real_runner <- revdeprunner:::run_source_preparation_process
  commands <- character()
  retried <- testthat::with_mocked_bindings(
    run_preparation_gate_fixture(fixture, previous = failed),
    source_download_file = function(...) {
      stop("the downloader must not run", call. = FALSE)
    },
    run_source_preparation_process = function(
      r_executable,
      arguments,
      working_directory,
      stdout_path,
      stderr_path,
      timeout_seconds
    ) {
      commands <<- c(commands, paste(arguments, collapse = " "))
      real_runner(
        r_executable,
        arguments,
        working_directory,
        stdout_path,
        stderr_path,
        timeout_seconds
      )
    },
    .package = "revdeprunner"
  )

  expect_identical(preparation_gate_outcome(retried, "BuildPkg"), "prepared")
  expect_length(commands, 2L)
  expect_true(all(grepl("BuildPkg", commands, fixed = TRUE)))
  expect_true(failed_attempt_id %in% retried$report$attempts$attempt_id)
  expect_gt(nrow(retried$report$attempts), nrow(failed$report$attempts))

  reused <- testthat::with_mocked_bindings(
    run_preparation_gate_fixture(fixture, previous = retried),
    source_download_file = function(...) {
      stop("the downloader must not run", call. = FALSE)
    },
    run_source_preparation_process = function(...) {
      stop("the R command must not run", call. = FALSE)
    },
    .package = "revdeprunner"
  )
  expect_identical(reused, retried)
})

test_that("dependency cycles fail before acquisition or preparation", {
  database <- source_acquisition_fixture_database()
  database$Imports[database$Package == "BuildPkg"] <-
    "SubjectPkg, FilePkg"
  database$Depends[database$Package == "FilePkg"] <-
    "SubjectPkg, BuildPkg"
  universe <- source_acquisition_fixture_contracts(database)$universe

  expect_error(
    revdeprunner:::preparation_dependency_order(universe),
    "cycle",
    fixed = TRUE
  )
})
# nolint end
