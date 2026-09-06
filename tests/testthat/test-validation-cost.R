# These tests guard measured validation costs. Spies delegate to real validators;
# corruption rejection and workflow outcomes are covered by the behavior tests.

test_that("preparation bounds full-plan and context validation across resume", {
  skip_if_not(identical(unname(Sys.info()[["sysname"]]), "Linux"))
  local <- make_revdep_run_fixture()
  on.exit(unlink(local$fixture$root, recursive = TRUE), add = TRUE)
  runtime <- file.path(local$fixture$root, "validation-cost-runtime")
  withr::local_envvar(c(
    REVDEP_RUNNER_DATA = file.path(runtime, "data"),
    REVDEP_RUNNER_RUNS = file.path(runtime, "runs"),
    CRANCACHE_DIR = file.path(runtime, "missing-crancache")
  ))
  plan_validations <- 0L
  context_admissions <- 0L
  validate_plan <- revdeprunner:::validate_public_revdep_plan
  validate_context <- revdeprunner:::validate_preparation_gate_context
  local_mocked_bindings(
    revdep_plan_package_database = function(repos) local$database,
    revdep_plan_cran_database = function() NULL,
    validate_public_revdep_plan = function(plan) {
      plan_validations <<- plan_validations + 1L
      validate_plan(plan)
    },
    validate_preparation_gate_context = function(context) {
      context_admissions <<- context_admissions + 1L
      validate_context(context)
    },
    .package = "revdeprunner"
  )

  prepared <- revdep_prepare(local$fixture$paths[[1L]], repos = local$bases)
  expect_identical(prepared$summary$state, "ready")
  expect_lte(plan_validations, 1L)
  expect_lte(context_admissions, 1L)

  plan_validations <- 0L
  context_admissions <- 0L
  resumed <- revdep_prepare(local$fixture$paths[[1L]], repos = local$bases)
  expect_identical(resumed$evidence$checkpoint, prepared$evidence$checkpoint)
  expect_identical(plan_validations, 0L)
  expect_lte(context_admissions, 1L)

  plan_validations <- 0L
  context_admissions <- 0L
  supplied <- revdep_prepare(prepared$plan)
  expect_identical(supplied$summary$state, "ready")
  expect_lte(plan_validations, 1L)
  expect_lte(context_admissions, 1L)
})

test_that("the gate bounds complete source-plan validation", {
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

  gate <- do.call(
    revdeprunner:::prepare_dependency_universe,
    c(
      source_preparation_context(fixture),
      list(baseline_source = fixture$baseline_source)
    )
  )

  expect_s3_class(gate$report, "revdeprunner_preparation_report")
  expect_lte(validation_calls, 1L)
})

test_that("stock admission avoids repeating deep validation and runtime probes", {
  skip_if_not(identical(unname(Sys.info()[["sysname"]]), "Linux"))
  skip_if_stock_tools_unavailable()
  fixture <- make_stock_repository_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  context <- source_preparation_context(fixture)
  validate_initialization <- revdeprunner:::validate_stock_revdepcheck_initialization
  validation_modes <- logical()
  local_mocked_bindings(
    validate_stock_revdepcheck_initialization = function(
      initialization,
      context,
      require_pre_worker = TRUE
    ) {
      validation_modes <<- c(validation_modes, require_pre_worker)
      validate_initialization(initialization, context, require_pre_worker)
    },
    .package = "revdeprunner"
  )
  initialization <- revdeprunner:::initialize_stock_revdepcheck(
    fixture$gate,
    context,
    fixture$baseline,
    exclude_targets = "FilePkg"
  )
  expect_length(validation_modes, 0L)

  expect_invisible(testthat::with_mocked_bindings(
    revdeprunner:::validate_stock_revdepcheck_initialization(
      initialization,
      context
    ),
    validate_preparation_gate = function(...) {
      stop("deep gate validation was repeated", call. = FALSE)
    },
    validate_preparation_report = function(...) {
      stop("deep report validation was repeated", call. = FALSE)
    },
    validate_baseline_source = function(...) {
      stop("deep baseline validation was repeated", call. = FALSE)
    },
    observe_stock_runtime = function(...) {
      stop("stock runtime probe was repeated", call. = FALSE)
    },
    .package = "revdeprunner"
  ))

  validation_modes <- logical()
  result <- revdeprunner:::run_stock_revdepcheck(
    initialization,
    context,
    worker_timeout_seconds = 60L,
    process_timeout_seconds = 300L
  )
  expect_identical(result$state, "success")
  expect_lte(length(validation_modes), 2L)
  expect_false(any(validation_modes))
})
