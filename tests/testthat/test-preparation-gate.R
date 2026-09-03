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
    r_executable = fixture$r_executable
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
      list(
        baseline_source = fixture$baseline_source,
        previous = previous,
        timeout_seconds = timeout_seconds
      )
    )
  )
}

preparation_gate_outcome <- function(gate, package) {
  gate$report$results$outcome[gate$report$results$package == package]
}

install_fixture_baseline <- function(fixture) {
  context <- preparation_gate_context(fixture)
  revdeprunner:::install_runner_supplied_baseline(
    fixture$baseline_source,
    context,
    revdeprunner:::source_preparation_build_library(context$path_plan),
    60L
  )
}

source_dependency_database <- function() {
  database <- source_acquisition_fixture_database()
  primary <- source_acquisition_fixture_repositories()[["CRAN"]]
  build <- database$Package == "BuildPkg" &
    database$Repository == primary
  database$Imports[build] <- "SubjectPkg, FilePkg (>= 3.0)"
  database
}

set_preparation_ambient_library <- function(path) {
  variables <- c("R_LIBS", "R_LIBS_SITE", "R_LIBS_USER")
  prior <- Sys.getenv(variables, unset = NA_character_)
  restore <- function() {
    do.call(Sys.unsetenv, list(variables))
    present <- !is.na(prior)
    if (any(present)) {
      do.call(Sys.setenv, as.list(prior[present]))
    }
  }
  do.call(
    Sys.setenv,
    as.list(stats::setNames(rep(path, length(variables)), variables))
  )
  restore
}

test_that("runner-supplied baseline prepares targets that import it", {
  fixture <- make_source_preparation_fixture(
    missing_binary_packages = "BuildPkg",
    database = source_dependency_database(),
    build_imports = "SubjectPkg"
  )
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)

  gate <- run_preparation_gate_fixture(fixture)
  library <- file.path(source_preparation_run_root(fixture), "build-library")

  expect_identical(preparation_gate_outcome(gate, "BuildPkg"), "prepared")
  expect_identical(
    as.character(utils::packageVersion("SubjectPkg", lib.loc = library)),
    "0.1"
  )
  expect_false("SubjectPkg" %in% gate$report$results$package)
  expect_false("SubjectPkg" %in% gate$report$requirements$package)
  expect_false("SubjectPkg" %in% gate$report$artifacts$package)
  expect_false("SubjectPkg" %in% gate$report$sources$package)
  expect_false("SubjectPkg" %in% gate$report$attempts$package)
})

test_that("source builds reuse exact dependency-ordered preparations", {
  fixture <- make_source_preparation_fixture(
    missing_binary_packages = "FilePkg",
    database = source_dependency_database(),
    build_imports = "FilePkg (>= 3.0)"
  )
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)

  gate <- run_preparation_gate_fixture(fixture)
  library <- file.path(source_preparation_run_root(fixture), "build-library")

  expect_identical(preparation_gate_outcome(gate, "FilePkg"), "prepared")
  expect_identical(preparation_gate_outcome(gate, "BuildPkg"), "prepared")
  expect_identical(
    as.character(utils::packageVersion("FilePkg", lib.loc = library)),
    "3.0"
  )

  unlink(library, recursive = TRUE)
  real_runner <- revdeprunner:::run_source_preparation_process
  commands <- character()
  gate <- testthat::with_mocked_bindings(
    run_preparation_gate_fixture(fixture, gate),
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
  expect_length(commands, 3L)
  expect_identical(
    sum(grepl("SubjectPkg_0.1.tar.gz", commands, fixed = TRUE)),
    1L
  )
  expect_false(any(grepl("--build", commands, fixed = TRUE)))
  expect_identical(preparation_gate_outcome(gate, "BuildPkg"), "prepared")

  testthat::local_mocked_bindings(
    run_source_preparation_process = function(...) {
      stop("the R command must not run", call. = FALSE)
    },
    .package = "revdeprunner"
  )
  expect_identical(run_preparation_gate_fixture(fixture, gate), gate)
})

test_that("source builds exclude an older ambient dependency", {
  fixture <- make_source_preparation_fixture(
    missing_binary_packages = "FilePkg",
    database = source_dependency_database(),
    build_imports = "FilePkg (>= 3.0)"
  )
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  ambient_library <- file.path(fixture$root, "ambient-library")
  dir.create(ambient_library)
  old_source <- make_installable_source_archive(
    file.path(fixture$root, "ambient-repository"),
    package = "FilePkg",
    version = "1.0",
    needs_compilation = "no"
  )
  status <- system2(
    file.path(R.home("bin"), "R"),
    c("CMD", "INSTALL", paste0("--library=", ambient_library), old_source),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(status, "status"))
  restore_environment <- set_preparation_ambient_library(ambient_library)
  on.exit(restore_environment(), add = TRUE)

  gate <- run_preparation_gate_fixture(fixture)
  library <- file.path(source_preparation_run_root(fixture), "build-library")

  expect_identical(preparation_gate_outcome(gate, "BuildPkg"), "prepared")
  expect_identical(
    as.character(utils::packageVersion("FilePkg", lib.loc = library)),
    "3.0"
  )
  expect_identical(
    as.character(utils::packageVersion("FilePkg", lib.loc = ambient_library)),
    "1.0"
  )
})

test_that("binary-hit installation failures are typed and block dependents", {
  cases <- list(
    failure = list(
      runner = mock_source_preparation_process(
        "binary installation failed",
        1L
      ),
      result = "installation-failure",
      attempt = "failure",
      diagnostic = "binary installation failed"
    ),
    timeout = list(
      runner = mock_source_preparation_process(
        "binary installation timed out",
        124L,
        timed_out = TRUE
      ),
      result = "timeout",
      attempt = "timeout",
      diagnostic = "timed out"
    )
  )
  for (case in cases) {
    fixture <- make_source_preparation_fixture(
      database = source_dependency_database(),
      build_imports = "FilePkg (>= 3.0)"
    )
    on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
    install_fixture_baseline(fixture)
    gate <- testthat::with_mocked_bindings(
      run_preparation_gate_fixture(fixture),
      run_source_preparation_process = case$runner,
      .package = "revdeprunner"
    )

    expect_identical(
      preparation_gate_outcome(gate, "FilePkg"),
      case$result
    )
    expect_identical(preparation_gate_outcome(gate, "BuildPkg"), "blocked")
    expect_identical(
      gate$report$results$blocking_dependency[
        gate$report$results$package == "BuildPkg"
      ],
      "FilePkg"
    )
    attempt <- gate$report$attempts[
      gate$report$attempts$package == "FilePkg",
      ,
      drop = FALSE
    ]
    expect_identical(attempt$stage, "install")
    expect_identical(attempt$outcome, case$attempt)
    expect_match(attempt$diagnostic_excerpt, case$diagnostic)
  }
})

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

test_that("binary-hit reuse requires retained successful install proof", {
  fixture <- make_source_preparation_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  gate <- run_preparation_gate_fixture(fixture)
  context <- preparation_gate_context(fixture)
  hit_attempt <- gate$report$attempts$package == "FilePkg" &
    gate$report$attempts$stage == "install"
  expect_identical(sum(hit_attempt), 1L)

  invalid <- gate
  retained_attempts <- revdeprunner:::preparation_gate_attempt_records(
    gate$report$attempts[!hit_attempt, , drop = FALSE]
  )
  invalid$report <- revdeprunner:::new_preparation_report(
    context$universe,
    context$cohort,
    context$snapshot,
    context$lane,
    lapply(seq_len(nrow(gate$report$artifacts)), function(row) {
      structure(
        as.list(gate$report$artifacts[row, , drop = FALSE]),
        class = "revdeprunner_artifact_identity"
      )
    }),
    gate$report$sources,
    retained_attempts,
    gate$report$results
  )
  expect_invisible(
    revdeprunner:::validate_preparation_report(
      invalid$report,
      context$universe,
      context$cohort,
      context$snapshot,
      context$lane
    )
  )
  expect_error(
    revdeprunner:::validate_preparation_gate(invalid, context),
    "successful binary-hit install attempt",
    fixed = TRUE
  )

  real_runner <- revdeprunner:::run_source_preparation_process
  commands <- character()
  repaired <- testthat::with_mocked_bindings(
    run_preparation_gate_fixture(fixture, invalid),
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
  expect_length(commands, 1L)
  expect_match(commands, "FilePkg", fixed = TRUE)
  expect_false(grepl("--build", commands, fixed = TRUE))
  expect_invisible(
    revdeprunner:::validate_preparation_gate(repaired, context)
  )

  testthat::local_mocked_bindings(
    run_source_preparation_process = function(...) {
      stop("the R command must not run", call. = FALSE)
    },
    .package = "revdeprunner"
  )
  expect_identical(run_preparation_gate_fixture(fixture, repaired), repaired)
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
  install_fixture_baseline(fixture)
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

test_that("a gate with only hits and blockers runs no source build", {
  database <- source_acquisition_fixture_database()
  database$Imports[database$Package == "BuildPkg"] <-
    "SubjectPkg, MissingPkg"
  fixture <- make_source_preparation_fixture(database = database)
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  install_fixture_baseline(fixture)
  real_runner <- revdeprunner:::run_source_preparation_process
  commands <- character()
  testthat::local_mocked_bindings(
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

  gate <- run_preparation_gate_fixture(fixture)

  expect_length(gate$source_preparations, 0L)
  expect_identical(preparation_gate_outcome(gate, "FilePkg"), "prepared")
  expect_identical(preparation_gate_outcome(gate, "MissingPkg"), "unavailable")
  expect_identical(preparation_gate_outcome(gate, "BuildPkg"), "blocked")
  expect_identical(preparation_gate_outcome(gate, "HitPkg"), "blocked")
  expect_length(commands, 1L)
  expect_match(commands, "FilePkg", fixed = TRUE)
  expect_false(grepl("--build", commands, fixed = TRUE))
  expect_identical(nrow(gate$report$attempts), 1L)
})

test_that("retries replace failures while preserving successes and history", {
  fixture <- make_source_preparation_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  install_fixture_baseline(fixture)
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
  expect_length(commands, 3L)
  expect_identical(sum(grepl("BuildPkg", commands, fixed = TRUE)), 2L)
  expect_identical(sum(grepl("FilePkg", commands, fixed = TRUE)), 1L)
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

test_that("runner dependencies precede its virtual preparation step", {
  database <- source_acquisition_fixture_database()
  runner_dependency <- database[
    database$Package == "SubjectPkg",
    ,
    drop = FALSE
  ]
  runner_dependency$Package <- "RunnerDep"
  runner_dependency$Version <- "1.0"
  runner_dependency$Depends <- NA_character_
  runner_dependency$Imports <- NA_character_
  runner_dependency$MD5sum <- "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"
  database$Imports[database$Package == "SubjectPkg"] <- "RunnerDep"
  database <- rbind(database, runner_dependency)
  universe <- source_acquisition_fixture_contracts(database)$universe

  steps <- revdeprunner:::preparation_dependency_steps(universe)
  runner <- match("SubjectPkg", steps)

  expect_lt(match("RunnerDep", steps), runner)
  expect_true(all(
    match(c("BuildPkg", "FilePkg", "HitPkg"), steps) > runner
  ))
  expect_identical(
    setdiff(steps, "SubjectPkg"),
    revdeprunner:::preparation_dependency_order(universe)
  )
})
# nolint end
