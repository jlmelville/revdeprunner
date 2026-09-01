# These private tests protect the exact repository and clean installability
# boundary without exposing the unfinished runner as a public API.

# nolint start: object_usage_linter.
repository_fixture_database <- function(
  dependent = FALSE,
  stock_dependency = FALSE,
  pure_r_build = FALSE,
  hyphenated_build_version = FALSE
) {
  database <- source_acquisition_fixture_database()
  database$Imports[database$Package == "HitPkg"] <- "SubjectPkg"
  if (dependent) {
    database$Depends[database$Package == "FilePkg"] <-
      "BuildPkg, SubjectPkg"
  }
  if (stock_dependency) {
    database$Suggests[
      database$Package == "BuildPkg" &
        database$Repository ==
          source_acquisition_fixture_repositories()[["CRAN"]]
    ] <- "HitPkg"
  }
  if (pure_r_build) {
    database$NeedsCompilation[
      database$Package == "BuildPkg" &
        database$Repository ==
          source_acquisition_fixture_repositories()[["CRAN"]]
    ] <- "no"
  }
  if (hyphenated_build_version) {
    database$Version[
      database$Package == "BuildPkg" &
        database$Repository ==
          source_acquisition_fixture_repositories()[["CRAN"]]
    ] <- "2.0-1"
  }
  database
}

make_repository_preparation_fixture <- function(
  dependent = FALSE,
  stock_dependency = FALSE,
  pure_r_build = FALSE,
  hyphenated_build_version = FALSE
) {
  fixture <- make_source_preparation_fixture(
    missing_binary_packages = c("BuildPkg", "FilePkg", "HitPkg"),
    database = repository_fixture_database(
      dependent,
      stock_dependency,
      pure_r_build,
      hyphenated_build_version
    ),
    build_imports = "SubjectPkg"
  )
  fixture$gate <- do.call(
    revdeprunner:::prepare_dependency_universe,
    c(
      source_preparation_context(fixture),
      list(
        baseline_source = fixture$baseline_source,
        timeout_seconds = 60L
      )
    )
  )
  fixture
}

repository_fixture_process_package <- function(arguments) {
  marker <- match("--args", arguments)
  stopifnot(!is.na(marker), length(arguments) >= marker + 1L)
  arguments[[marker + 1L]]
}

repository_fixture_process_stage <- function(stdout_path) {
  sub("[.]stdout[.]log$", "", basename(stdout_path))
}

if (!identical(unname(Sys.info()[["sysname"]]), "Linux")) {
  test_that("repository projection reports its Linux boundary", {
    expect_error(
      revdeprunner:::require_linux_repository_projection(),
      "supported only on Linux",
      fixed = TRUE
    )
  })
} else {
  test_that("an exact repository installs and loads every prepared package", {
    fixture <- make_repository_preparation_fixture()
    on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
    context <- source_preparation_context(fixture)
    warehouse_before <- source_preparation_warehouse_snapshot(fixture)

    first <- revdeprunner:::project_preparation_repository(
      fixture$gate,
      context
    )
    second <- revdeprunner:::project_preparation_repository(
      fixture$gate,
      context
    )
    calls <- list()
    real_runner <- revdeprunner:::run_repository_preparation_process
    ready <- testthat::with_mocked_bindings(
      revdeprunner:::prepare_repository_universe(
        fixture$gate,
        context,
        fixture$baseline_source,
        60L
      ),
      run_repository_preparation_process = function(
        r_executable,
        arguments,
        working_directory,
        stdout_path,
        stderr_path,
        timeout_seconds
      ) {
        calls[[length(calls) + 1L]] <<- c(
          package = repository_fixture_process_package(arguments),
          stage = repository_fixture_process_stage(stdout_path)
        )
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

    expect_false(first$reused)
    expect_true(second$reused)
    expect_true(ready$projection$reused)
    expect_identical(first$manifest, second$manifest)
    expect_identical(
      first$manifest$package,
      c("BuildPkg", "FilePkg", "HitPkg")
    )
    expect_true(all(grepl("_R_", first$manifest$archive_name, fixed = TRUE)))
    expect_true(dir.exists(first$repository_path))
    expect_true(file.exists(file.path(
      first$repository_path,
      "src",
      "contrib",
      "PACKAGES"
    )))
    expect_false(any(file.exists(file.path(
      first$repository_path,
      "src",
      "contrib",
      c("PACKAGES.gz", "PACKAGES.rds")
    ))))
    expect_identical(
      ready$report$results$outcome,
      rep("ready", 3L)
    )
    expect_identical(
      vapply(calls, `[[`, character(1L), "package"),
      c("BuildPkg", "FilePkg", "HitPkg", "BuildPkg", "FilePkg", "HitPkg")
    )
    expect_identical(
      vapply(calls, `[[`, character(1L), "stage"),
      c(rep("install", 3L), rep("namespace-load", 3L))
    )
    expect_true(all(
      fixture$gate$report$attempts$attempt_id %in%
        ready$report$attempts$attempt_id
    ))
    expect_identical(
      sort(list.files(ready$library_path), method = "radix"),
      c("BuildPkg", "FilePkg", "HitPkg", "SubjectPkg")
    )
    expect_identical(
      as.character(
        utils::packageVersion("SubjectPkg", lib.loc = ready$library_path)
      ),
      "0.1"
    )
    expect_false("SubjectPkg" %in% ready$report$attempts$package)
    expect_identical(
      source_preparation_warehouse_snapshot(fixture),
      warehouse_before
    )
    expect_invisible(
      revdeprunner:::validate_repository_preparation(ready, context)
    )

    wrong_binding <- ready
    wrong_binding$report$universe_id <- paste0(
      "sha256:",
      strrep("0", 64L)
    )
    expect_error(
      revdeprunner:::validate_repository_preparation(
        wrong_binding,
        context
      ),
      "does not match its preparation context",
      fixed = TRUE
    )

    incomplete_results <- ready
    incomplete_results$report$results$outcome[] <- "prepared"
    expect_error(
      revdeprunner:::validate_repository_preparation(
        incomplete_results,
        context
      ),
      "completed result outcomes",
      fixed = TRUE
    )

    missing_result <- ready
    missing_result$report$results <-
      missing_result$report$results[-1L, , drop = FALSE]
    expect_error(
      revdeprunner:::validate_repository_preparation(
        missing_result,
        context
      ),
      "cover its requirements",
      fixed = TRUE
    )

    escaped_projection <- ready
    escaped_projection$projection$repository_path <- fixture$root
    expect_error(
      revdeprunner:::validate_repository_preparation(
        escaped_projection,
        context
      ),
      "projection path is inconsistent",
      fixed = TRUE
    )
  })

  test_that("repository verification preserves literal hyphenated versions", {
    fixture <- make_repository_preparation_fixture(
      hyphenated_build_version = TRUE
    )
    on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
    context <- source_preparation_context(fixture)

    ready <- revdeprunner:::prepare_repository_universe(
      fixture$gate,
      context,
      fixture$baseline_source,
      60L
    )

    build <- ready$report$results[
      ready$report$results$package == "BuildPkg",
      ,
      drop = FALSE
    ]
    description <- read.dcf(
      file.path(ready$library_path, "BuildPkg", "DESCRIPTION"),
      fields = c("Package", "Version")
    )
    expect_identical(build$version, "2.0-1")
    expect_identical(build$outcome, "ready")
    expect_identical(unname(description[1L, ]), c("BuildPkg", "2.0-1"))
    expect_false(revdeprunner:::source_preparation_library_has_package(
      ready$library_path,
      "BuildPkg",
      "2.0.1"
    ))
    expect_invisible(
      revdeprunner:::validate_repository_preparation(ready, context)
    )
  })

  test_that("incomplete preparation stops before repository projection", {
    fixture <- make_source_preparation_fixture()
    on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
    gate <- do.call(
      revdeprunner:::prepare_dependency_universe,
      c(
        source_preparation_context(fixture),
        list(
          baseline_source = fixture$baseline_source,
          timeout_seconds = 60L
        )
      )
    )
    context <- source_preparation_context(fixture)

    expect_error(
      revdeprunner:::prepare_repository_universe(
        gate,
        context,
        fixture$baseline_source,
        60L
      ),
      "every preparation result to be prepared",
      fixed = TRUE
    )
    expect_false(dir.exists(file.path(fixture$paths[[2L]], "repositories")))
  })

  test_that("invalid or stale repository metadata is never overwritten", {
    fixture <- make_repository_preparation_fixture()
    on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
    context <- source_preparation_context(fixture)
    expected_path <- file.path(
      fixture$paths[[2L]],
      "repositories",
      sub("^sha256:", "", fixture$gate$report$report_id)
    )

    expect_error(
      testthat::with_mocked_bindings(
        revdeprunner:::project_preparation_repository(fixture$gate, context),
        repository_write_packages = function(contrib_path) {
          writeLines(
            c("Package: WrongPkg", "Version: 9.9", "File: wrong.tar.gz"),
            file.path(contrib_path, "PACKAGES")
          )
        },
        .package = "revdeprunner"
      ),
      "PACKAGES metadata",
      fixed = TRUE
    )
    expect_false(file.exists(expected_path))
    staging <- file.path(fixture$paths[[2L]], "repositories", ".staging")
    expect_length(list.files(staging, all.files = TRUE, no.. = TRUE), 0L)
    projection <- revdeprunner:::project_preparation_repository(
      fixture$gate,
      context
    )
    alternate_index <- file.path(
      projection$repository_path,
      "src",
      "contrib",
      "PACKAGES.rds"
    )
    saveRDS(matrix(c("WrongPkg", "9.9"), nrow = 1L), alternate_index)
    expect_error(
      revdeprunner:::project_preparation_repository(fixture$gate, context),
      "unexpected entries",
      fixed = TRUE
    )
    unlink(alternate_index)
    writeLines(
      c("Package: WrongPkg", "Version: 9.9", "File: wrong.tar.gz"),
      file.path(projection$repository_path, "src", "contrib", "PACKAGES")
    )
    expect_error(
      revdeprunner:::project_preparation_repository(fixture$gate, context),
      "PACKAGES metadata",
      fixed = TRUE
    )
  })

  test_that("install failures block dependents and preserve independent work", {
    fixture <- make_repository_preparation_fixture(dependent = TRUE)
    on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
    context <- source_preparation_context(fixture)
    real_runner <- revdeprunner:::run_repository_preparation_process
    failed_runner <- mock_source_preparation_process(
      "binary installation failed",
      1L
    )
    calls <- character()

    result <- testthat::with_mocked_bindings(
      revdeprunner:::prepare_repository_universe(
        fixture$gate,
        context,
        fixture$baseline_source,
        60L
      ),
      run_repository_preparation_process = function(
        r_executable,
        arguments,
        working_directory,
        stdout_path,
        stderr_path,
        timeout_seconds
      ) {
        package <- repository_fixture_process_package(arguments)
        stage <- repository_fixture_process_stage(stdout_path)
        calls <<- c(calls, paste(package, stage, sep = ":"))
        if (identical(package, "BuildPkg") && identical(stage, "install")) {
          return(failed_runner(
            r_executable,
            arguments,
            working_directory,
            stdout_path,
            stderr_path,
            timeout_seconds
          ))
        }
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

    expect_identical(
      result$report$results$outcome,
      c("installation-failure", "blocked", "ready")
    )
    expect_identical(
      result$report$results$blocking_dependency,
      c(NA_character_, "BuildPkg", NA_character_)
    )
    expect_identical(
      calls,
      c("BuildPkg:install", "HitPkg:install", "HitPkg:namespace-load")
    )
    expect_match(
      result$report$results$diagnostic_excerpt[[1L]],
      "binary installation failed",
      fixed = TRUE
    )
  })

  test_that("namespace timeouts block dependents after clean installation", {
    fixture <- make_repository_preparation_fixture(dependent = TRUE)
    on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
    context <- source_preparation_context(fixture)
    real_runner <- revdeprunner:::run_repository_preparation_process
    timeout_runner <- mock_source_preparation_process(
      "namespace load timed out",
      124L,
      timed_out = TRUE
    )
    calls <- character()

    result <- testthat::with_mocked_bindings(
      revdeprunner:::prepare_repository_universe(
        fixture$gate,
        context,
        fixture$baseline_source,
        60L
      ),
      run_repository_preparation_process = function(
        r_executable,
        arguments,
        working_directory,
        stdout_path,
        stderr_path,
        timeout_seconds
      ) {
        package <- repository_fixture_process_package(arguments)
        stage <- repository_fixture_process_stage(stdout_path)
        calls <<- c(calls, paste(package, stage, sep = ":"))
        if (
          identical(package, "BuildPkg") &&
            identical(stage, "namespace-load")
        ) {
          return(timeout_runner(
            r_executable,
            arguments,
            working_directory,
            stdout_path,
            stderr_path,
            timeout_seconds
          ))
        }
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

    expect_identical(
      result$report$results$outcome,
      c("timeout", "blocked", "ready")
    )
    expect_identical(
      result$report$results$blocking_dependency,
      c(NA_character_, "BuildPkg", NA_character_)
    )
    expect_identical(
      calls,
      c(
        "BuildPkg:install",
        "HitPkg:install",
        "FilePkg:install",
        "BuildPkg:namespace-load",
        "HitPkg:namespace-load"
      )
    )
    expect_match(
      result$report$results$diagnostic_excerpt[[1L]],
      "namespace load timed out",
      fixed = TRUE
    )
  })
}
# nolint end
