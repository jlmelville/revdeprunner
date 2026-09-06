test_that("comparison admission rejects corrupt saved preparation metadata", {
  skip_if_stock_tools_unavailable()
  local <- make_revdep_run_fixture()
  on.exit(unlink(local$fixture$root, recursive = TRUE), add = TRUE)
  runtime <- file.path(local$fixture$root, "comparison-admission")
  withr::local_envvar(c(
    REVDEP_RUNNER_DATA = file.path(runtime, "data"),
    REVDEP_RUNNER_RUNS = file.path(runtime, "runs"),
    CRANCACHE_DIR = file.path(runtime, "missing-crancache")
  ))
  local_mocked_bindings(
    revdep_plan_package_database = function(repos) local$database,
    revdep_plan_cran_database = function() NULL,
    initialize_stock_revdepcheck = function(...)
      stop("valid preparation reached comparison initialization"),
    .package = "revdeprunner"
  )
  prepared <- revdep_prepare(local$fixture$paths[[1L]], repos = local$bases)
  expect_identical(prepared$summary$state, "ready")
  expect_error(
    revdep_check(prepared),
    "valid preparation reached comparison initialization"
  )
  checkpoint <- prepared$evidence$checkpoint
  saved <- readRDS(checkpoint)

  corrupt <- saved
  corrupt$context$snapshot$packages$MD5sum[[1L]] <- strrep("0", 32L)
  saveRDS(corrupt, checkpoint)
  expect_error(revdep_check(prepared), "snapshot identity does not match")

  corrupt <- saved
  corrupt$gate$report$report_id <- paste0("sha256:", strrep("0", 64L))
  saveRDS(corrupt, checkpoint)
  expect_error(revdep_check(prepared), "report identity does not match")
})
