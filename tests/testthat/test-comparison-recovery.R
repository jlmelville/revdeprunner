test_that("real timeouts and interruptions resume complete changed and unchanged pairs", {
  skip_if_not(identical(unname(Sys.info()[["sysname"]]), "Linux"))
  skip_if_stock_tools_unavailable()
  skip_if_not_installed("callr")
  root <- tempfile("comparison-recovery-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  subject_install_block <- file.path(root, "subject-install-block")
  local <- recovery_fixture(root, subject_install_block)
  on.exit(unlink(local$fixture$root, recursive = TRUE), add = TRUE)
  withr::local_envvar(c(
    REVDEP_RUNNER_DATA = file.path(root, "data"),
    REVDEP_RUNNER_RUNS = file.path(root, "runs"),
    CRANCACHE_DIR = file.path(root, "missing-crancache")
  ))
  local_mocked_bindings(
    revdep_plan_package_database = function(repos) local$database,
    revdep_plan_cran_database = function() NULL,
    .package = "revdeprunner"
  )
  prepared <- revdep_prepare(local$fixture$paths[[1L]], repos = local$bases)
  expect_identical(prepared$summary$state, "ready")
  preparation <- readRDS(prepared$evidence$checkpoint)
  prepared_path <- file.path(root, "prepared.rds")
  saveRDS(prepared, prepared_path)
  library <- recovery_runner_library(root)
  process <- NULL
  on.exit(
    if (!is.null(process) && process$is_alive()) process$kill_tree(),
    add = TRUE
  )

  # Kill after workspace creation, before initialization can return or unwind.
  initializing <- file.path(root, "initializing")
  process <- recovery_check_process(
    library,
    prepared_path,
    list(),
    root,
    initializing
  )
  recovery_wait(process, function() file.exists(initializing))
  checkpoint <- list.files(
    dirname(prepared$evidence$checkpoint),
    "^check-v4-",
    full.names = TRUE
  )
  intent <- readRDS(checkpoint[[1L]])
  expect_null(intent$initialization)
  unfinished <- file.path(
    preparation$context$path_plan$runs_root,
    preparation$context$path_plan$run_id,
    intent$workspace,
    "unfinished-witness"
  )
  expect_true(file.exists(unfinished))
  process$kill_tree()
  process$wait(timeout = 10000)
  unlink(initializing)

  # Stop inside stock's subject installation, retaining an actual install lock.
  file.create(subject_install_block)
  process <- recovery_check_process(library, prepared_path, list(), root)
  recovery_wait(
    process,
    function() file.exists(paste0(subject_install_block, "-waiting"))
  )
  installing <- readRDS(checkpoint[[1L]])
  old_library <- file.path(
    installing$initialization$paths$checkout,
    "revdep",
    "library",
    "SubjectPkg",
    "old"
  )
  locks <- list.files(old_library, "^00LOCK", full.names = TRUE)
  expect_gt(length(locks), 0L)
  process$kill_tree()
  process$wait(timeout = 10000)
  unlink(subject_install_block)

  # The next call changes only the overall process budget, with identical sources.
  process <- recovery_check_process(
    library,
    prepared_path,
    list(process_timeout_seconds = 1L),
    root
  )
  recovery_wait(process)
  timed_out <- process$get_result()
  expect_false(file.exists(unfinished))
  expect_identical(timed_out$summary$state, "comparison-incomplete")
  expect_true(readRDS(timed_out$evidence$checkpoint)$result$process$timed_out)
  process <- recovery_check_process(
    library,
    prepared_path,
    list(process_timeout_seconds = 300L),
    root
  )
  recovery_wait(process)
  completed <- process$get_result()
  expect_true(all(dir.exists(locks)))
  expect_false(identical(
    readRDS(completed$evidence$checkpoint)$initialization$paths$root,
    installing$initialization$paths$root
  ))
  if (
    !identical(
      completed$results$outcome,
      c("changed", "unchanged", "unchanged")
    )
  ) {
    print(completed$results)
    print(completed$diagnostics)
    saved <- readRDS(completed$evidence$checkpoint)
    for (path in c(
      saved$initialization$paths$stdout,
      saved$initialization$paths$stderr
    )) {
      cat("\nComparison process log:", path, "\n")
      if (file.exists(path))
        cat(tail(readLines(path, warn = FALSE), 80L), sep = "\n")
    }
    raw <- revdeprunner:::stock_namespace_function("db_get_results")(
      saved$initialization$paths$checkout,
      "BuildPkg"
    )
    for (side in names(raw)) {
      cat("\nBuildPkg", side, "saved check evidence:\n")
      cat(raw[[side]]$result, sep = "\n")
    }
    revdeprunner:::stock_namespace_function("db_disconnect")(
      saved$initialization$paths$checkout
    )
  }
  expect_identical(
    completed$results$outcome,
    c("changed", "unchanged", "unchanged")
  )
  expect_true(any(
    completed$changes$package == "BuildPkg" &
      completed$changes$severity == "error" &
      completed$changes$change == "added"
  ))
  expect_true(all(completed$changes$package == "BuildPkg"))
  expect_true(any(grepl("version ==", completed$changes$message, fixed = TRUE)))
  expect_identical(
    recovery_markers(local$markers),
    stats::setNames(rep(1L, 6L), names(recovery_markers(local$markers)))
  )

  # Repeat both sides; stop only after the third target enters its test script.
  file.create(file.path(local$markers, "block"))
  process <- recovery_check_process(
    library,
    prepared_path,
    list(repeat_checks = TRUE, process_timeout_seconds = 300L),
    root
  )
  recovery_wait(
    process,
    function() file.exists(file.path(local$markers, "waiting"))
  )
  saved <- readRDS(completed$evidence$checkpoint)
  database <- revdeprunner:::observe_stock_database(
    saved$initialization$paths$checkout
  )
  pairs <- revdeprunner:::stock_adapter_results(
    saved$initialization,
    list(status = 1L, timed_out = FALSE),
    database
  )
  expect_identical(pairs$outcome, c("changed", "unchanged", "incomplete"))
  retained <- recovery_markers(local$markers, c("BuildPkg", "FilePkg"))
  expect_true(all(retained == 2L))
  process$kill_tree()
  process$wait(timeout = 10000)
  expect_false(process$is_alive())
  unlink(file.path(local$markers, "block"))

  process <- recovery_check_process(
    library,
    prepared_path,
    list(process_timeout_seconds = 300L),
    root
  )
  recovery_wait(process)
  resumed <- process$get_result()
  expect_identical(resumed$results$outcome, completed$results$outcome)
  expect_identical(
    recovery_markers(local$markers, c("BuildPkg", "FilePkg")),
    retained
  )
  expect_true(all(recovery_markers(local$markers, "HitPkg") >= 2L))
  expect_identical(readRDS(prepared$evidence$checkpoint), preparation)

  markers <- recovery_markers(local$markers)
  process <- recovery_check_process(library, prepared_path, list(), root)
  recovery_wait(process)
  expect_identical(process$get_result(), resumed)
  expect_identical(recovery_markers(local$markers), markers)

  unlink(file.path(root, "runs"), recursive = TRUE)
  process <- recovery_check_process(library, prepared_path, list(), root)
  recovery_wait(process)
  expect_identical(process$get_result(), resumed)
  expect_identical(recovery_markers(local$markers), markers)
})
