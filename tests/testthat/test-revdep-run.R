test_that("public preparation checkpoints completed packages before later interruption", {
  local <- make_revdep_run_fixture()
  on.exit(unlink(local$fixture$root, recursive = TRUE), add = TRUE)
  runtime <- tempfile("revdep-preparation-recovery-")
  dir.create(runtime)
  on.exit(unlink(runtime, recursive = TRUE), add = TRUE)
  withr::local_envvar(c(
    REVDEP_RUNNER_DATA = file.path(runtime, "data"),
    REVDEP_RUNNER_RUNS = file.path(runtime, "runs"),
    CRANCACHE_DIR = file.path(runtime, "missing-crancache")
  ))
  calls <- 0L
  prepare <- revdeprunner:::prepare_source_binary_in_context
  local_mocked_bindings(
    revdep_plan_package_database = function(repos) local$database,
    revdep_plan_cran_database = function() NULL,
    prepare_source_binary_in_context = function(...) {
      calls <<- calls + 1L
      if (calls == 2L) stop("interrupted before second package")
      prepare(...)
    },
    .package = "revdeprunner"
  )
  expect_error(
    revdep_prepare(local$fixture$paths[[1L]], repos = local$bases),
    "interrupted before second package"
  )
  checkpoints <- list.files(
    file.path(runtime, "data"),
    pattern = "^prepare-.*rds$",
    recursive = TRUE,
    full.names = TRUE
  )
  expect_length(checkpoints, 1L)
  saved <- readRDS(checkpoints[[1L]])
  expect_false(is.null(saved$gate))
  successes <- saved$gate$source_preparations
  expect_length(successes, 1L)
  unlink(file.path(runtime, "runs"), recursive = TRUE)
  resumed <- revdep_prepare(local$fixture$paths[[1L]], repos = local$bases)
  expect_identical(resumed$summary$state, "ready")
  after <- readRDS(checkpoints[[1L]])$gate$source_preparations
  expect_identical(after[names(successes)], successes)
  current_lane <- revdeprunner:::revdep_compatibility_lane()
  expect_error(
    with_mocked_bindings(
      revdep_check(resumed),
      revdep_compatibility_lane = function() {
        current_lane$r_major_minor <- "0.0"
        current_lane
      },
      .package = "revdeprunner"
    ),
    "incompatible with the current R environment",
    fixed = TRUE
  )
})

test_that("public preparation and checks compose the local proven engine", {
  skip_if_not(identical(unname(Sys.info()[["sysname"]]), "Linux"))
  skip_if_not(revdep_run_stock_tools_supported())
  local <- make_revdep_run_fixture()
  on.exit(unlink(local$fixture$root, recursive = TRUE), add = TRUE)
  runtime <- tempfile("revdep-public-runtime-")
  dir.create(runtime)
  on.exit(unlink(runtime, recursive = TRUE), add = TRUE)
  withr::local_envvar(c(
    REVDEP_RUNNER_DATA = file.path(runtime, "data"),
    REVDEP_RUNNER_RUNS = file.path(runtime, "runs"),
    CRANCACHE_DIR = file.path(runtime, "missing-crancache")
  ))

  queries <- 0L
  plan_validations <- 0L
  context_admissions <- 0L
  query_database <- local$database
  validate_plan <- revdeprunner:::validate_public_revdep_plan
  validate_context <- revdeprunner:::validate_preparation_gate_context
  local_mocked_bindings(
    revdep_plan_package_database = function(repos) {
      queries <<- queries + 1L
      query_database
    },
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

  expect_message(
    prepared <- revdep_prepare(
      local$fixture$paths[[1L]],
      repos = local$bases
    ),
    NA
  )

  expect_s3_class(prepared, "revdep_prepared")
  expect_identical(prepared$summary$state, "ready")
  expect_identical(prepared$summary$selected_targets, 3L)
  expect_identical(nrow(prepared$problems), 0L)
  expect_identical(prepared$plan$unavailable$dependency, "OptionalPkg")
  expect_identical(prepared$plan$unavailable$relationship, "Suggests")
  expect_identical(plan_validations, 1L)
  expect_identical(context_admissions, 1L)
  expect_false("OptionalPkg" %in% prepared$evidence$report$requirements$package)
  expect_false("OptionalPkg" %in% prepared$evidence$report$results$package)
  expect_true(file.exists(prepared$evidence$baseline$path))
  expect_match(basename(prepared$evidence$checkpoint), "^prepare-v5-")
  preparation_state <- readRDS(prepared$evidence$checkpoint)
  expect_identical(
    preparation_state$version,
    "revdeprunner-prepare-state/v5"
  )
  expect_identical(
    preparation_state$context$r_executable,
    normalizePath(file.path(R.home("bin"), "R"), winslash = "/")
  )
  expect_true(is.data.frame(
    preparation_state$context$binary_reuse$observations
  ))
  expect_false(dir.exists(file.path(runtime, "data", "manifests")))
  expect_false(dir.exists(file.path(runtime, "data", "warehouse")))
  runner_source_cache <- file.path(
    runtime,
    "data",
    "source-cache",
    "src",
    "contrib"
  )
  expect_true(file.exists(file.path(runner_source_cache, "PACKAGES")))
  expect_match(
    capture.output(print(prepared))[[1L]],
    "Reverse-dependency preparation for SubjectPkg"
  )

  progress <- capture.output(
    resumed <- revdep_prepare(
      local$fixture$paths[[1L]],
      repos = local$bases,
      verbose = TRUE
    ),
    type = "message"
  )
  expect_identical(progress[[1L]], "Planning preparation.")
  expect_true(any(grepl(
    "Reusing prepared binary: BuildPkg",
    progress,
    fixed = TRUE
  )))
  expect_identical(
    tail(progress, 1L),
    "Preparation complete: 3/3 packages ready."
  )
  expect_identical(queries, 1L)
  expect_identical(plan_validations, 1L)
  expect_identical(context_admissions, 2L)
  expect_identical(
    resumed$evidence$checkpoint,
    prepared$evidence$checkpoint
  )
  expect_identical(resumed$plan, prepared$plan)
  expect_identical(
    resumed$summary$snapshot_id,
    prepared$summary$snapshot_id
  )
  expect_identical(resumed$summary$state, "ready")

  source <- file.path(local$fixture$paths[[1L]], "R", "subject.R")
  writeLines(c(readLines(source), "# Ordinary source edit."), source)
  edited <- revdep_prepare(local$fixture$paths[[1L]], repos = local$bases)
  expect_identical(edited$evidence$checkpoint, resumed$evidence$checkpoint)
  expect_identical(
    readRDS(edited$evidence$checkpoint)$gate$source_preparations,
    preparation_state$gate$source_preparations
  )

  later_plan <- revdep_plan(local$fixture$paths[[1L]], repos = local$bases)
  runner_cache <- file.path(runtime, "data", "binary-cache", "src", "contrib")
  expect_true(all(
    c(runner_source_cache, runner_cache) %in%
      attr(later_plan, "cache_roots", exact = TRUE)
  ))
  expect_true(all(later_plan$requirements$action == "reuse"))
  expect_true(all(later_plan$requirements$cache_source == runner_cache))
  expect_identical(later_plan$summary$source_builds, 0L)
  later_prepared <- revdep_prepare(later_plan)
  later_state <- readRDS(later_prepared$evidence$checkpoint)
  source_observations <- later_state$context$binary_reuse$observations
  source_observations <- source_observations[
    source_observations$status == "ok" &
      source_observations$archive_type == "source",
    ,
    drop = FALSE
  ]
  expect_setequal(
    source_observations$package,
    later_state$context$source_plan$sources$package
  )
  expect_true(all(vapply(
    later_state$gate$source_acquisitions,
    is.null,
    logical(1L)
  )))

  progress <- capture.output(
    result <- revdep_check(resumed, verbose = TRUE),
    type = "message"
  )
  expect_true(any(grepl("Comparison run:", progress, fixed = TRUE)))
  expect_true(any(grepl("3/3 targets complete", progress, fixed = TRUE)))
  expect_identical(tail(progress, 1L), "Comparison complete: success.")
  expect_s3_class(result, "revdep_result")
  expect_identical(result$summary$state, "success")
  expect_true(all(result$results$outcome == "unchanged"))
  expect_identical(nrow(result$diagnostics), 0L)
  expect_match(basename(result$evidence$checkpoint), "^check-v4-")
  comparison_state <- readRDS(result$evidence$checkpoint)
  expect_identical(comparison_state$version, "revdeprunner-check-state/v4")
  expect_identical(
    comparison_state$initialization$r_executable,
    preparation_state$context$r_executable
  )
  expect_false("repository" %in% names(comparison_state$initialization))
  expect_match(
    capture.output(print(result))[[1L]],
    "Reverse-dependency result for SubjectPkg"
  )

  comparison_state$result$compiler <- list(legacy = TRUE)
  comparison_state$result$private_libraries <- data.frame(legacy = TRUE)
  saveRDS(comparison_state, result$evidence$checkpoint)
  expect_message(repeated <- revdep_check(resumed), NA)
  expect_identical(repeated, result)

  # This simulates admission metadata, not an actual R upgrade. Patch releases
  # retain binary eligibility but must not inherit a completed comparison.
  lane <- revdeprunner:::revdep_compatibility_lane()
  patched_lane <- revdeprunner:::new_compatibility_lane(
    lane$r_major_minor,
    lane$r_platform,
    lane$architecture,
    lane$os_abi,
    paste0(lane$toolchain_tag, "-patch-witness")
  )
  with_mocked_bindings(
    {
      admitted <- revdeprunner:::revdep_prepared_state(resumed)
      expect_identical(
        admitted$gate$report$artifacts,
        preparation_state$gate$report$artifacts
      )
      expect_error(revdep_check(resumed), "fresh comparison initialization")
    },
    revdep_compatibility_lane = function() patched_lane,
    initialize_stock_revdepcheck = function(...)
      stop("fresh comparison initialization"),
    .package = "revdeprunner"
  )

  query_database <- revdep_run_distinct_snapshot_database(local$database)
  refreshed_plan <- revdep_plan(
    local$fixture$paths[[1L]],
    cache = character(),
    repos = local$bases
  )
  expect_identical(queries, 3L)
  expect_false(identical(
    refreshed_plan$summary$snapshot_id,
    resumed$plan$summary$snapshot_id
  ))

  refreshed <- revdep_prepare(refreshed_plan)
  expect_identical(plan_validations, 3L)
  refreshed_result <- revdep_check(refreshed)
  expect_false(identical(
    refreshed_result$evidence$checkpoint,
    result$evidence$checkpoint
  ))
  expect_identical(
    refreshed_result$summary$snapshot_id,
    refreshed_plan$summary$snapshot_id
  )
})

test_that("preparation failures return actionable problems before checks", {
  skip_if_not(identical(unname(Sys.info()[["sysname"]]), "Linux"))
  local <- make_revdep_run_fixture()
  on.exit(unlink(local$fixture$root, recursive = TRUE), add = TRUE)
  runtime <- tempfile("revdep-public-problems-")
  dir.create(runtime)
  on.exit(unlink(runtime, recursive = TRUE), add = TRUE)
  withr::local_envvar(c(
    REVDEP_RUNNER_DATA = file.path(runtime, "data"),
    REVDEP_RUNNER_RUNS = file.path(runtime, "runs")
  ))
  real_process <- revdeprunner:::run_source_preparation_process
  failed_process <- mock_source_preparation_process(
    "configure: install libfixture-dev before retrying",
    status = 1L
  )
  local_mocked_bindings(
    revdep_plan_package_database = function(repos) local$database,
    revdep_plan_cran_database = function() NULL,
    run_source_preparation_process = function(
      r_executable,
      arguments,
      working_directory,
      stdout_path,
      stderr_path,
      timeout_seconds
    ) {
      runner <- if (
        any(grepl(
          "BuildPkg_2.0.tar.gz",
          arguments,
          fixed = TRUE
        ))
      ) {
        failed_process
      } else {
        real_process
      }
      runner(
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

  prepared <- revdep_prepare(
    local$fixture$paths[[1L]],
    cache = character(),
    repos = local$bases
  )

  expect_identical(prepared$summary$state, "preparation-incomplete")
  expect_true("BuildPkg" %in% prepared$problems$package)
  build <- prepared$problems[prepared$problems$package == "BuildPkg", ]
  expect_identical(build$outcome, "compilation-failure")
  expect_match(build$diagnostic_excerpt, "libfixture-dev", fixed = TRUE)
  expect_true(file.exists(build$stdout_path))
  expect_true(file.exists(build$stderr_path))
  expect_error(
    revdep_check(prepared),
    "Preparation is incomplete",
    fixed = TRUE
  )
})

test_that("candidate identity ignores Git state and changes with package code", {
  root <- tempfile("revdep-candidate-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  write_revdep_run_candidate(root)
  context <- list(
    path_plan = list(package_root = root),
    cohort = list(package = "SubjectPkg")
  )
  baseline <- revdeprunner:::revdep_source_candidate_identity(context)

  dir.create(file.path(root, ".git"))
  writeLines("ignored", file.path(root, ".git", "HEAD"))
  git_only <- revdeprunner:::revdep_source_candidate_identity(context)
  expect_identical(git_only, baseline)

  writeLines(
    "subject_value <- function() 43L",
    file.path(root, "R", "subject.R")
  )
  changed <- revdeprunner:::revdep_source_candidate_identity(context)
  expect_false(identical(changed, baseline))
})

test_that("execution controls reject invalid values before reading preparation", {
  for (value in list(
    0,
    -1,
    1.5,
    Inf,
    NA_real_,
    1 + 1i,
    .Machine$integer.max + 1
  )) {
    expect_warning(
      expect_error(
        revdep_prepare("absent-package", timeout_seconds = value),
        "`timeout_seconds` must be a positive whole number",
        fixed = TRUE
      ),
      NA
    )
    for (argument in c("worker_timeout_seconds", "process_timeout_seconds")) {
      args <- c(list(prepared = NULL), stats::setNames(list(value), argument))
      expect_warning(
        expect_error(
          do.call(revdep_check, args),
          paste0("`", argument, "` must be a positive whole number"),
          fixed = TRUE
        ),
        NA
      )
    }
  }
  expect_error(
    revdep_check(NULL, repeat_checks = NA),
    "`repeat_checks` must be TRUE or FALSE",
    fixed = TRUE
  )
  expect_error(
    revdep_check(NULL, verbose = 1),
    "`verbose` must be TRUE or FALSE",
    fixed = TRUE
  )
  expect_error(
    revdep_prepare(NULL, verbose = NA),
    "`verbose` must be TRUE or FALSE",
    fixed = TRUE
  )
})

test_that("legacy private checkpoints request a fresh preparation", {
  expect_error(
    revdeprunner:::validate_revdep_prepare_state(
      list(version = "revdeprunner-prepare-state/v2"),
      "request"
    ),
    "Create a fresh preparation",
    fixed = TRUE
  )
  expect_error(
    revdeprunner:::validate_revdep_check_state(
      list(version = "revdeprunner-check-state/v2"),
      "request",
      list()
    ),
    "Create a fresh preparation",
    fixed = TRUE
  )

  root <- tempfile("legacy-parent-prepare-v5-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  checkpoint <- file.path(root, "prepare-v5-request.rds")
  legacy <- file.path(root, "prepare-v4-request.rds")
  saveRDS(list(version = "revdeprunner-prepare-state/v2"), legacy)
  plan <- structure(list(), class = "revdep_plan")
  testthat::local_mocked_bindings(
    require_linux_revdep_runner = function() invisible(NULL),
    validate_public_revdep_plan = function(value) value,
    revdep_runtime_storage = function() list(data = root, runs = root),
    revdep_prepare_plan_request = function(plan, storage) {
      list(id = "request", checkpoint = checkpoint)
    },
    .package = "revdeprunner"
  )

  expect_error(
    revdep_prepare(plan),
    "Remove it and create a fresh preparation",
    fixed = TRUE
  )
  expect_false(file.exists(checkpoint))
})
