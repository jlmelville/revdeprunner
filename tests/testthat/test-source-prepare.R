# These private tests protect the first source-to-binary preparation boundary.
# One compiled fixture exercises real R commands; injected process results keep
# failure, timeout, and malformed-output checks deterministic.

test_that("one source package builds, verifies, publishes, and reuses", {
  fixture <- make_source_preparation_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  repository_before <- snapshot_test_cache(fixture$repository_root)
  cache_before <- snapshot_test_cache(fixture$paths[[4L]])
  acquisition <- acquire_fixture_build_source(fixture)
  source_before <- revdeprunner:::artifact_file_snapshot(
    acquisition$cache_path
  )

  preparation <- prepare_fixture_source_binary(
    fixture,
    source_acquisition = acquisition
  )
  context <- source_preparation_context(fixture)
  expect_identical(
    context$r_executable,
    normalizePath(file.path(R.home("bin"), "R"), winslash = "/")
  )

  expect_invisible(
    revdeprunner:::validate_source_preparation_record(preparation, context)
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
  expect_identical(
    basename(dirname(preparation$binary_path)),
    preparation$binary_artifact$sha256
  )
  expect_false(dir.exists(file.path(fixture$paths[[2L]], "warehouse")))

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
  source_copy <- list.files(
    file.path(attempt_root, "source"),
    full.names = TRUE
  )
  expect_length(source_copy, 1L)
  expect_false(grepl(
    acquisition$cache_path,
    preparation$attempts[[1L]]$command,
    fixed = TRUE
  ))
  expect_match(
    preparation$attempts[[1L]]$command,
    basename(source_copy),
    fixed = TRUE
  )
  expect_identical(
    digest::digest(
      source_copy,
      algo = "sha256",
      file = TRUE,
      serialize = FALSE
    ),
    acquisition$artifact$sha256
  )
  expect_identical(
    revdeprunner:::artifact_file_snapshot(acquisition$cache_path),
    source_before
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
  expect_identical(snapshot_test_cache(fixture$paths[[4L]]), cache_before)

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
    cache_before <- source_preparation_source_cache_snapshot(fixture)
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
    expect_identical(
      source_preparation_source_cache_snapshot(fixture),
      cache_before
    )
    expect_invisible(
      revdeprunner:::validate_source_preparation_record(
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
  cache_before <- source_preparation_source_cache_snapshot(fixture)
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
  expect_identical(
    source_preparation_source_cache_snapshot(fixture),
    cache_before
  )
  expect_invisible(
    revdeprunner:::validate_source_preparation_record(
      preparation,
      source_preparation_context(fixture)
    )
  )
})

test_that("the process boundary detects a real timeout", {
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

test_that("the process boundary resolves and revalidates its R executable", {
  root <- tempfile("source-preparation-r-executable-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  stdout_path <- file.path(root, "stdout.log")
  stderr_path <- file.path(root, "stderr.log")
  run <- function(r_executable) {
    revdeprunner:::run_source_preparation_process(
      r_executable,
      c("--vanilla", "-e", "quit(status = 0L)"),
      root,
      stdout_path,
      stderr_path,
      timeout_seconds = 30L
    )
  }

  expect_error(run(file.path(root, "missing-R")), "existing file", fixed = TRUE)
  expect_error(run(root), "existing file", fixed = TRUE)

  ephemeral <- file.path(root, "ephemeral-R")
  writeLines("temporary executable", ephemeral)
  resolved <- revdeprunner:::normalize_r_executable(ephemeral)
  unlink(ephemeral)
  expect_error(run(resolved), "existing file", fixed = TRUE)

  target <- file.path(R.home("bin"), "Rscript")
  alias <- file.path(root, "R-alias")
  if (isTRUE(file.symlink(target, alias))) {
    process <- run(alias)
    arguments <- c("--vanilla", "-e", "quit(status = 0L)")
    expect_identical(process$status, 0L)
    expect_identical(
      process$command,
      revdeprunner:::render_source_preparation_command(
        normalizePath(target, winslash = "/"),
        arguments
      )
    )
  } else {
    succeed()
  }
})

test_that("malformed and mismatched binary output fails before publication", {
  creators <- list(
    malformed = function(working_directory) {
      path <- file.path(
        working_directory,
        paste0(
          "BuildPkg_2.0_R_",
          source_preparation_runner_lane()$r_platform,
          ".tar.gz"
        )
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
          paste("R", as.character(getRversion())),
          source_preparation_runner_lane()$r_platform,
          "2026-08-29",
          "unix",
          sep = "; "
        ),
        filename = paste0(
          "BuildPkg_2.0_R_",
          source_preparation_runner_lane()$r_platform,
          ".tar.gz"
        )
      )
    }
  )

  for (creator in creators) {
    fixture <- make_source_preparation_fixture()
    on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
    acquisition <- acquire_fixture_build_source(fixture)
    cache_before <- source_preparation_source_cache_snapshot(fixture)
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
      source_preparation_source_cache_snapshot(fixture),
      cache_before
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
    revdeprunner:::validate_source_preparation_record(
      preparation,
      source_preparation_context(fixture)
    ),
    "does not match its SHA-256",
    fixed = TRUE
  )
})

test_that("source preparation supports pure-R misses and rejects hits", {
  fixture <- make_source_preparation_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  context <- source_preparation_context(fixture)

  expect_error(
    revdeprunner:::prepare_source_binary_in_context(
      "HitPkg",
      context,
      NULL
    ),
    "planned binary miss",
    fixed = TRUE
  )
  pure_r_fixture <- make_source_preparation_fixture(
    missing_binary_packages = "FilePkg"
  )
  on.exit(unlink(pure_r_fixture$root, recursive = TRUE), add = TRUE)
  pure_r_context <- source_preparation_context(pure_r_fixture)
  pure_r_acquisition <- revdeprunner:::acquire_source_artifact_in_context(
    "FilePkg",
    pure_r_context$source_plan,
    pure_r_context$path_plan
  )
  pure_r_preparation <- revdeprunner:::prepare_source_binary_in_context(
    "FilePkg",
    pure_r_context,
    pure_r_acquisition,
    timeout_seconds = 60L
  )
  expect_identical(pure_r_preparation$result$outcome, "prepared")
  expect_identical(
    pure_r_preparation$binary_artifact$package,
    "FilePkg"
  )
  expect_invisible(
    revdeprunner:::validate_source_preparation_record(
      pure_r_preparation,
      pure_r_context
    )
  )
})

test_that("source preparation accepts an explicit filename platform", {
  root <- tempfile("source-preparation-platform-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  lane <- source_preparation_runner_lane()
  package <- "DeclaredCompiled"
  version <- "1.0"
  binary_path <- make_test_archive(
    root,
    repository = "",
    package = package,
    version = version,
    needs_compilation = "yes",
    built = paste(
      paste("R", as.character(getRversion())),
      "",
      "2026-08-30",
      "unix",
      sep = "; "
    ),
    filename = paste0(
      package,
      "_",
      version,
      "_R_",
      lane$r_platform,
      ".tar.gz"
    )
  )
  artifact <- revdeprunner:::new_artifact_identity(
    package,
    version,
    digest::digest(
      binary_path,
      algo = "sha256",
      file = TRUE,
      serialize = FALSE
    ),
    "binary",
    lane
  )

  expect_invisible(
    revdeprunner:::validate_source_preparation_binary_payload(
      binary_path,
      artifact,
      lane
    )
  )
})
