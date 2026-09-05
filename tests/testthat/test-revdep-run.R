# These exported-API fixtures compose the accepted private preparation helpers.

revdep_run_fixture_database <- function() {
  database <- source_acquisition_fixture_database()
  primary <- source_acquisition_fixture_repositories()[["CRAN"]]
  database$Imports[database$Package == "HitPkg"] <- "SubjectPkg"
  database$Suggests[
    database$Package == "BuildPkg" & database$Repository == primary
  ] <- "HitPkg, OptionalPkg"
  database$NeedsCompilation[
    database$Package == "BuildPkg" & database$Repository == primary
  ] <- "no"
  database
}

write_revdep_run_candidate <- function(path) {
  dir.create(file.path(path, "R"), showWarnings = FALSE)
  writeLines(
    c(
      "Package: SubjectPkg",
      "Type: Package",
      "Title: Public Runner Subject Fixture",
      "Version: 0.2",
      paste0(
        "Authors@R: person('Fixture', 'Author', role = c('aut', 'cre'), ",
        "email = 'fixture@example.test')"
      ),
      "Description: A pure-R package-under-test fixture.",
      "License: MIT",
      "Encoding: UTF-8",
      "NeedsCompilation: no"
    ),
    file.path(path, "DESCRIPTION")
  )
  writeLines("export(subject_value)", file.path(path, "NAMESPACE"))
  writeLines(
    "subject_value <- function() 42L",
    file.path(path, "R", "subject.R")
  )
}

make_revdep_run_fixture <- function() {
  fixture <- make_source_preparation_fixture(
    missing_binary_packages = c("BuildPkg", "FilePkg", "HitPkg"),
    database = revdep_run_fixture_database(),
    build_imports = "SubjectPkg"
  )
  write_revdep_run_candidate(fixture$paths[[1L]])
  repositories <- fixture$download_contracts$snapshot$repositories
  database <- fixture$download_contracts$snapshot$packages
  database <- database[
    database$Repository == repositories[["CRAN"]],
    ,
    drop = FALSE
  ]
  bases <- c(CRAN = sub("/src/contrib$", "", repositories[["CRAN"]]))
  list(
    fixture = fixture,
    database = database,
    bases = bases
  )
}

revdep_run_stock_tools_supported <- function() {
  required <- c("revdepcheck", "crancache", "cranlike")
  if (!all(vapply(required, requireNamespace, logical(1L), quietly = TRUE))) {
    return(FALSE)
  }
  tryCatch(
    {
      revdeprunner:::require_stock_adapter_tools()
      TRUE
    },
    error = function(error) FALSE
  )
}

revdep_run_distinct_snapshot_database <- function(database) {
  unrelated <- database[database$Package == "SubjectPkg", , drop = FALSE]
  unrelated$Package <- "SnapshotOnlyPkg"
  unrelated$Version <- "1.0"
  unrelated$Depends <- NA_character_
  unrelated$Imports <- NA_character_
  unrelated$LinkingTo <- NA_character_
  unrelated$Suggests <- NA_character_
  unrelated$File <- NA_character_
  unrelated$MD5sum <- "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"
  rbind(database, unrelated)
}

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

  prepared <- revdep_prepare(
    local$fixture$paths[[1L]],
    repos = local$bases
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
  expect_match(basename(prepared$evidence$checkpoint), "^prepare-v3-")
  preparation_state <- readRDS(prepared$evidence$checkpoint)
  expect_identical(
    preparation_state$version,
    "revdeprunner-prepare-state/v3"
  )
  expect_identical(
    preparation_state$context$r_executable,
    normalizePath(file.path(R.home("bin"), "R"), winslash = "/")
  )
  expect_match(
    capture.output(print(prepared))[[1L]],
    "Reverse-dependency preparation for SubjectPkg"
  )

  resumed <- revdep_prepare(
    local$fixture$paths[[1L]],
    repos = local$bases
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

  later_plan <- revdep_plan(local$fixture$paths[[1L]], repos = local$bases)
  runner_cache <- file.path(runtime, "data", "binary-cache", "src", "contrib")
  expect_true(all(later_plan$requirements$action == "reuse"))
  expect_true(all(later_plan$requirements$cache_source == runner_cache))
  expect_identical(later_plan$summary$source_builds, 0L)

  result <- revdep_check(resumed)
  expect_s3_class(result, "revdep_result")
  expect_identical(result$summary$state, "success")
  expect_true(all(result$results$outcome == "unchanged"))
  expect_identical(nrow(result$diagnostics), 0L)
  expect_match(basename(result$evidence$checkpoint), "^check-v3-")
  comparison_state <- readRDS(result$evidence$checkpoint)
  expect_identical(comparison_state$version, "revdeprunner-check-state/v3")
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
  repeated <- revdep_check(resumed)
  expect_identical(repeated, result)

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
  expect_identical(plan_validations, 2L)
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

  root <- tempfile("legacy-parent-prepare-v3-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  checkpoint <- file.path(root, "prepare-v3-request.rds")
  legacy <- file.path(root, "prepare-v2-request.rds")
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
