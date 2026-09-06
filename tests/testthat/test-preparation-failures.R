test_that("download and released-subject setup failures are actionable and resumable", {
  local <- make_revdep_run_fixture()
  on.exit(unlink(local$fixture$root, recursive = TRUE), add = TRUE)
  runtime <- tempfile("preparation-failures-")
  dir.create(runtime)
  on.exit(unlink(runtime, recursive = TRUE), add = TRUE)
  withr::local_envvar(c(
    REVDEP_RUNNER_DATA = file.path(runtime, "data"),
    REVDEP_RUNNER_RUNS = file.path(runtime, "runs"),
    CRANCACHE_DIR = file.path(runtime, "missing-crancache")
  ))
  queries <- 0L
  failure <- "SubjectPkg"
  fail_install <- FALSE
  download <- revdeprunner:::source_download_file
  run <- revdeprunner:::run_source_preparation_process
  failed_process <- mock_source_preparation_process(
    "missing baseline library",
    1L
  )
  local_mocked_bindings(
    revdep_plan_package_database = function(repos) {
      queries <<- queries + 1L
      local$database
    },
    revdep_plan_cran_database = function() NULL,
    source_download_file = function(url, destination) {
      if (!is.null(failure) && grepl(paste0(failure, "_"), url, fixed = TRUE)) {
        stop("repository temporarily unavailable")
      }
      download(url, destination)
    },
    run_source_preparation_process = function(...) {
      args <- list(...)
      if (fail_install && any(grepl("SubjectPkg_", args[[2L]], fixed = TRUE))) {
        failed_process(...)
      } else run(...)
    },
    .package = "revdeprunner"
  )
  candidate <- local$fixture$paths[[1L]]
  baseline_failure <- revdep_prepare(candidate, repos = local$bases)
  expect_identical(baseline_failure$summary$state, "preparation-incomplete")
  expect_identical(baseline_failure$problems$package, "SubjectPkg")
  expect_identical(baseline_failure$problems$outcome, "download-failure")
  expect_true(all(file.exists(unlist(baseline_failure$problems[c(
    "stdout_path",
    "stderr_path"
  )]))))
  expect_match(
    baseline_failure$problems$diagnostic_excerpt,
    "repository temporarily unavailable"
  )
  expect_error(revdep_check(baseline_failure), "Preparation is incomplete")

  failure <- "BuildPkg"
  source_failure <- revdep_prepare(candidate, repos = local$bases)
  expect_identical(source_failure$problems$package, "BuildPkg")
  expect_identical(source_failure$problems$stage, "download")
  expect_identical(
    source_failure$evidence$checkpoint,
    baseline_failure$evidence$checkpoint
  )

  failure <- NULL
  ready <- revdep_prepare(candidate, repos = local$bases)
  expect_identical(ready$summary$state, "ready")
  expect_identical(queries, 1L)
  successes <- readRDS(ready$evidence$checkpoint)$gate$source_preparations
  unlink(file.path(runtime, "runs"), recursive = TRUE)
  fail_install <- TRUE
  installation_failure <- revdep_prepare(candidate, repos = local$bases)
  expect_identical(installation_failure$summary$state, "preparation-incomplete")
  expect_identical(installation_failure$problems$package, "SubjectPkg")
  expect_identical(
    installation_failure$problems$outcome,
    "installation-failure"
  )
  expect_match(
    installation_failure$problems$diagnostic_excerpt,
    "missing baseline library"
  )
  expect_true(file.exists(installation_failure$problems$stderr_path))
  expect_identical(
    readRDS(ready$evidence$checkpoint)$gate$source_preparations,
    successes
  )
  expect_error(revdep_check(installation_failure), "Preparation is incomplete")
  fail_install <- FALSE
  repaired <- revdep_prepare(candidate, repos = local$bases)
  expect_identical(repaired$summary$state, "ready")
  expect_identical(
    readRDS(ready$evidence$checkpoint)$gate$source_preparations,
    successes
  )
})
