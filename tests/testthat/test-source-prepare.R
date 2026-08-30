# These private tests protect the first source-to-binary preparation boundary.
# One compiled fixture exercises real R commands; injected process results keep
# failure, timeout, and malformed-output checks deterministic.

# nolint start: object_usage_linter.
test_that("one source package builds, verifies, promotes, and reuses", {
  fixture <- make_source_preparation_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  repository_before <- snapshot_test_cache(fixture$repository_root)
  cache_before <- snapshot_test_cache(fixture$paths[[5L]])

  preparation <- prepare_fixture_source_binary(fixture)
  context <- source_preparation_context(fixture)

  expect_invisible(
    revdeprunner:::validate_source_preparation(preparation, context)
  )
  expect_identical(preparation$package, "BuildPkg")
  expect_identical(preparation$version, "2.0")
  expect_identical(preparation$result$outcome, "prepared")
  expect_identical(
    preparation$result$artifact_id,
    preparation$binary_artifact$artifact_id
  )
  expect_identical(preparation$binary_artifact$archive_type, "binary")
  expect_identical(
    preparation$binary_artifact$lane_id,
    fixture$lane$lane_id
  )
  expect_identical(
    vapply(preparation$attempts, `[[`, character(1L), "stage"),
    c("build", "install")
  )
  expect_true(all(vapply(
    preparation$attempts,
    function(attempt) identical(attempt$outcome, "success"),
    logical(1L)
  )))
  expect_true(file.exists(preparation$binary_path))
  expect_true(file.exists(preparation$promotion$warehouse_path))
  expect_false(preparation$promotion$reused)

  run_root <- source_preparation_run_root(fixture)
  for (attempt in preparation$attempts) {
    for (stream in c("stdout", "stderr")) {
      path <- file.path(run_root, attempt[[paste0(stream, "_path")]])
      expect_true(file.exists(path))
      expect_identical(
        digest::digest(
          path,
          algo = "sha256",
          file = TRUE,
          serialize = FALSE
        ),
        attempt[[paste0(stream, "_sha256")]]
      )
    }
  }
  attempt_root <- dirname(
    file.path(run_root, preparation$attempts[[2L]]$stdout_path)
  )
  installed <- read.dcf(
    file.path(attempt_root, "verification-library", "BuildPkg", "DESCRIPTION"),
    fields = c("Package", "Version")
  )
  expect_identical(unname(installed[1L, "Package"]), "BuildPkg")
  expect_identical(unname(installed[1L, "Version"]), "2.0")
  expect_identical(
    snapshot_test_cache(fixture$repository_root),
    repository_before
  )
  expect_identical(snapshot_test_cache(fixture$paths[[5L]]), cache_before)

  testthat::local_mocked_bindings(
    source_download_file = function(url, destination) {
      stop("the downloader must not run", call. = FALSE)
    },
    run_source_preparation_process = function(...) {
      stop("the R command must not run", call. = FALSE)
    },
    .package = "revdeprunner"
  )
  reused <- prepare_fixture_source_binary(fixture, previous = preparation)
  expect_identical(reused, preparation)
})

test_that("build failures and timeouts retain typed log evidence", {
  cases <- list(
    failure = list(
      runner = mock_source_preparation_process("compiler failed", 1L),
      result = "compilation-failure",
      attempt = "failure",
      excerpt = "compiler failed"
    ),
    timeout = list(
      runner = mock_source_preparation_process(
        "build interrupted after timeout",
        124L,
        timed_out = TRUE
      ),
      result = "timeout",
      attempt = "timeout",
      excerpt = "timed out"
    )
  )

  for (case in cases) {
    fixture <- make_source_preparation_fixture()
    on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
    acquisition <- acquire_fixture_build_source(fixture)
    warehouse_before <- source_preparation_warehouse_snapshot(fixture)
    testthat::local_mocked_bindings(
      run_source_preparation_process = case$runner,
      .package = "revdeprunner"
    )

    preparation <- prepare_fixture_source_binary(
      fixture,
      source_acquisition = acquisition
    )

    expect_identical(preparation$result$outcome, case$result)
    expect_identical(preparation$attempts[[1L]]$stage, "build")
    expect_identical(preparation$attempts[[1L]]$outcome, case$attempt)
    expect_match(
      preparation$result$diagnostic_excerpt,
      case$excerpt,
      fixed = TRUE
    )
    expect_null(preparation$binary_artifact)
    expect_null(preparation$promotion)
    expect_identical(
      source_preparation_warehouse_snapshot(fixture),
      warehouse_before
    )
    expect_invisible(
      revdeprunner:::validate_source_preparation(
        preparation,
        source_preparation_context(fixture)
      )
    )
  }
})

test_that("binary installation failure does not publish its artifact", {
  fixture <- make_source_preparation_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  acquisition <- acquire_fixture_build_source(fixture)
  warehouse_before <- source_preparation_warehouse_snapshot(fixture)
  real_runner <- revdeprunner:::run_source_preparation_process
  failed_install <- mock_source_preparation_process(
    "binary installation failed",
    2L
  )
  calls <- 0L
  testthat::local_mocked_bindings(
    run_source_preparation_process = function(...) {
      calls <<- calls + 1L
      if (calls == 1L) {
        real_runner(...)
      } else {
        failed_install(...)
      }
    },
    .package = "revdeprunner"
  )

  preparation <- prepare_fixture_source_binary(
    fixture,
    source_acquisition = acquisition
  )

  expect_identical(preparation$result$outcome, "installation-failure")
  expect_identical(
    vapply(preparation$attempts, `[[`, character(1L), "outcome"),
    c("success", "failure")
  )
  expect_match(
    preparation$result$diagnostic_excerpt,
    "binary installation failed",
    fixed = TRUE
  )
  expect_true(file.exists(preparation$binary_path))
  expect_null(preparation$promotion)
  expect_identical(
    source_preparation_warehouse_snapshot(fixture),
    warehouse_before
  )
  expect_invisible(
    revdeprunner:::validate_source_preparation(
      preparation,
      source_preparation_context(fixture)
    )
  )
})

test_that("the base-R process boundary detects a real timeout", {
  root <- tempfile("source-preparation-timeout-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  stdout_path <- file.path(root, "stdout.log")
  stderr_path <- file.path(root, "stderr.log")

  process <- revdeprunner:::run_source_preparation_process(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", "-e", "Sys.sleep(5)"),
    root,
    stdout_path,
    stderr_path,
    timeout_seconds = 1L
  )

  expect_identical(process$status, 124L)
  expect_true(process$timed_out)
  expect_true(process$duration_ms >= 900)
  expect_true(file.exists(stdout_path))
  expect_true(file.exists(stderr_path))
})

test_that("malformed and mismatched binary output fails before publication", {
  creators <- list(
    malformed = function(working_directory) {
      path <- file.path(
        working_directory,
        "BuildPkg_2.0_R_x86_64-pc-linux-gnu.tar.gz"
      )
      writeBin(charToRaw("not an archive"), path)
    },
    mismatched = function(working_directory) {
      make_test_archive(
        working_directory,
        repository = "",
        package = "OtherPkg",
        version = "2.0",
        needs_compilation = "yes",
        built = paste(
          "R 4.5.2",
          "x86_64-pc-linux-gnu",
          "2026-08-29",
          "unix",
          sep = "; "
        ),
        filename = "BuildPkg_2.0_R_x86_64-pc-linux-gnu.tar.gz"
      )
    }
  )

  for (creator in creators) {
    fixture <- make_source_preparation_fixture()
    on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
    acquisition <- acquire_fixture_build_source(fixture)
    warehouse_before <- source_preparation_warehouse_snapshot(fixture)
    testthat::local_mocked_bindings(
      run_source_preparation_process = mock_source_preparation_process(
        "build reported success",
        0L,
        create_binary = creator
      ),
      .package = "revdeprunner"
    )

    expect_error(
      prepare_fixture_source_binary(
        fixture,
        source_acquisition = acquisition
      ),
      "archive validation failed",
      fixed = TRUE
    )
    expect_identical(
      source_preparation_warehouse_snapshot(fixture),
      warehouse_before
    )
    logs <- list.files(
      file.path(source_preparation_run_root(fixture), "preparation"),
      pattern = "build[.](?:stdout|stderr)[.]log$",
      full.names = TRUE,
      recursive = TRUE
    )
    expect_length(logs, 2L)
  }
})

test_that("source preparation detects changed log evidence", {
  fixture <- make_source_preparation_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  preparation <- prepare_fixture_source_binary(fixture)
  run_root <- source_preparation_run_root(fixture)
  stdout_path <- file.path(
    run_root,
    preparation$attempts[[1L]]$stdout_path
  )
  connection <- file(stdout_path, open = "ab")
  writeBin(charToRaw("changed"), connection)
  close(connection)

  expect_error(
    revdeprunner:::validate_source_preparation(
      preparation,
      source_preparation_context(fixture)
    ),
    "does not match its SHA-256",
    fixed = TRUE
  )
})

test_that("source preparation rejects hits and dry-run plans", {
  fixture <- make_source_preparation_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  context <- source_preparation_context(fixture)

  expect_error(
    revdeprunner:::prepare_source_binary(
      "HitPkg",
      context$source_plan,
      context$universe,
      context$cohort,
      context$snapshot,
      context$binary_reuse,
      context$lane,
      context$path_plan,
      context$command_plan
    ),
    "planned binary miss",
    fixed = TRUE
  )
  pure_r_fixture <- make_source_preparation_fixture(
    missing_binary_packages = "FilePkg"
  )
  on.exit(unlink(pure_r_fixture$root, recursive = TRUE), add = TRUE)
  pure_r_context <- source_preparation_context(pure_r_fixture)
  expect_error(
    revdeprunner:::prepare_source_binary(
      "FilePkg",
      pure_r_context$source_plan,
      pure_r_context$universe,
      pure_r_context$cohort,
      pure_r_context$snapshot,
      pure_r_context$binary_reuse,
      pure_r_context$lane,
      pure_r_context$path_plan,
      pure_r_context$command_plan
    ),
    "limited to a planned compiled package",
    fixed = TRUE
  )

  dry_run <- revdeprunner:::new_command_plan(
    "prepare",
    context$path_plan,
    file.path(R.home("bin"), "R"),
    TRUE,
    context$snapshot,
    context$cohort,
    context$universe,
    context$lane
  )
  context$command_plan <- dry_run
  expect_error(
    do.call(
      revdeprunner:::prepare_source_binary,
      c(list(package = "BuildPkg"), context)
    ),
    "executable prepare command plan",
    fixed = TRUE
  )
})
# nolint end
