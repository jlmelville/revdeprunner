test_that("preparation retries real interrupted installs with dirty working trees retained", {
  skip_if_not(identical(unname(Sys.info()[["sysname"]]), "Linux"))
  skip_if_not_installed("callr")
  local <- make_revdep_run_fixture()
  on.exit(unlink(local$fixture$root, recursive = TRUE), add = TRUE)
  root <- tempfile("installation-recovery-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  blocker <- file.path(root, "block")
  waiting <- file.path(root, "waiting")
  file.create(blocker)
  archive <- make_installable_source_archive(
    local$fixture$repository_root,
    package = "FilePkg",
    version = local$database$Version[local$database$Package == "FilePkg"],
    needs_compilation = "no",
    relative_directory = "custom",
    on_load = c(
      ".onLoad <- function(libname, pkgname) {",
      paste0("  if (file.exists(", deparse(blocker), ")) {"),
      paste0("    file.create(", deparse(waiting), ")"),
      paste0("    while (file.exists(", deparse(blocker), ")) Sys.sleep(0.05)"),
      "  }",
      "}"
    )
  )
  local$database$MD5sum[local$database$Package == "FilePkg"] <-
    unname(tools::md5sum(archive))
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
  plan <- revdep_plan(local$fixture$paths[[1L]], repos = local$bases)
  library <- recovery_runner_library(root)
  log <- file.path(root, "prepare.log")
  process <- callr::r_bg(
    function(plan)
      revdeprunner::revdep_prepare(plan, timeout_seconds = 60L, verbose = TRUE),
    args = list(plan = plan),
    libpath = c(library, .libPaths()),
    stdout = log,
    stderr = log,
    supervise = TRUE
  )
  on.exit(if (process$is_alive()) process$kill_tree(), add = TRUE)
  recovery_wait(process, function() file.exists(waiting))
  locks <- list.dirs(file.path(root, "runs"), recursive = TRUE)
  locks <- locks[grepl("^00LOCK", basename(locks))]
  expect_gt(length(locks), 0L)
  checkpoint <- list.files(
    file.path(root, "data"),
    "^prepare-.*rds$",
    recursive = TRUE,
    full.names = TRUE
  )
  saved <- readRDS(checkpoint[[1L]])$gate$source_preparations
  expect_true("BuildPkg" %in% names(saved))
  # These diagnostics must already be visible while the install is blocked.
  progress <- readLines(log, warn = FALSE)
  expect_true(any(grepl("budget: 60 s", progress, fixed = TRUE)))
  expect_true(any(grepl("build.stderr.log", progress, fixed = TRUE)))
  active <- tail(progress[startsWith(progress, "Process budget:")], 1L)
  if (length(active)) {
    stdout <- sub(".*stdout: (.*); stderr: .*", "\\1", active)
    stderr <- sub(".*; stderr: (.*)", "\\1", active)
    expect_true(file.exists(stdout))
    expect_true(file.exists(stderr))
    expect_true(any(grepl(
      "FilePkg",
      readLines(stderr, warn = FALSE),
      fixed = TRUE
    )))
  }
  process$kill_tree()
  process$wait(timeout = 10000)
  expect_false(process$is_alive())
  expect_true(all(dir.exists(locks)))

  # A real deadline leaves another dirty attempt; it must reach the hook again.
  unlink(waiting)
  timed_out <- revdep_prepare(plan, timeout_seconds = 10L)
  expect_true(file.exists(waiting))
  expect_true(any(timed_out$problems$outcome == "timeout"))
  unlink(blocker)
  progress <- capture.output(
    repaired <- revdep_prepare(plan, timeout_seconds = 60L, verbose = TRUE),
    type = "message"
  )
  expect_identical(repaired$summary$state, "ready")
  expect_true(any(grepl("finished in", progress, fixed = TRUE)))
  expect_identical(
    readRDS(checkpoint[[1L]])$gate$source_preparations[names(saved)],
    saved
  )
  expect_true(all(dir.exists(locks)))
})

test_that("supplied preparation plans outlive external binary cache directories", {
  local <- make_revdep_run_fixture()
  on.exit(unlink(local$fixture$root, recursive = TRUE), add = TRUE)
  root <- tempfile("cache-lifetime-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  withr::local_envvar(c(
    REVDEP_RUNNER_DATA = file.path(root, "provider-data"),
    REVDEP_RUNNER_RUNS = file.path(root, "provider-runs"),
    CRANCACHE_DIR = file.path(root, "missing-crancache")
  ))
  local_mocked_bindings(
    revdep_plan_package_database = function(repos) local$database,
    revdep_plan_cran_database = function() NULL,
    .package = "revdeprunner"
  )
  built <- revdep_prepare(local$fixture$paths[[1L]], repos = local$bases)
  binaries <- readRDS(built$evidence$checkpoint)$gate$source_preparations
  external <- file.path(root, "external-cache")
  dir.create(external)
  for (binary in binaries) expect_true(file.copy(binary$binary_path, external))
  withr::local_envvar(c(
    REVDEP_RUNNER_DATA = file.path(root, "owned-data"),
    REVDEP_RUNNER_RUNS = file.path(root, "owned-runs")
  ))
  plan <- revdep_plan(
    local$fixture$paths[[1L]],
    repos = local$bases,
    cache = external
  )
  expect_true(all(plan$requirements$action == "reuse"))
  prepared <- revdep_prepare(plan)
  saved <- readRDS(prepared$evidence$checkpoint)
  pinned <- saved$context$binary_reuse$cache_paths
  expect_true(all(file.exists(pinned)))
  expect_true(all(startsWith(pinned, file.path(root, "owned-data"))))
  unlink(external, recursive = TRUE)
  local_mocked_bindings(
    prepare_source_binary_in_context = function(...)
      stop("unexpected source rebuild"),
    .package = "revdeprunner"
  )
  resumed <- revdep_prepare(plan)
  expect_identical(resumed$summary$state, "ready")
  expect_identical(resumed$evidence$checkpoint, prepared$evidence$checkpoint)
  unlink(file.path(root, "owned-runs"), recursive = TRUE)
  restored <- revdep_prepare(plan)
  expect_identical(restored$summary$state, "ready")
  expect_false(dir.exists(external))
  expect_identical(
    readRDS(restored$evidence$checkpoint)$context$binary_reuse$cache_paths,
    pinned
  )
  writeLines("corrupted owned artifact", pinned[[1L]])
  expect_error(revdep_prepare(plan), "SHA-256")
})
