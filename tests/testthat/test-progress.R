test_that("public preparation reports validation and processed cache work", {
  local <- make_revdep_run_fixture()
  on.exit(unlink(local$fixture$root, recursive = TRUE), add = TRUE)
  withr::local_envvar(c(
    REVDEP_RUNNER_DATA = file.path(local$fixture$root, "progress-data"),
    REVDEP_RUNNER_RUNS = file.path(local$fixture$root, "progress-runs"),
    CRANCACHE_DIR = file.path(local$fixture$root, "missing-crancache")
  ))
  local_mocked_bindings(
    revdep_plan_package_database = function(repos) local$database,
    revdep_plan_cran_database = function() NULL,
    .package = "revdeprunner"
  )
  plan <- revdep_plan(local$fixture$paths[[1L]], repos = local$bases)
  expect_message(prepared <- revdep_prepare(plan), NA)
  messages <- capture.output(
    resumed <- revdep_prepare(plan, verbose = TRUE),
    type = "message"
  )
  expect_identical(resumed$summary$state, "ready")
  expect_identical(
    resumed$evidence$report$artifacts,
    prepared$evidence$report$artifacts
  )
  phases <- messages[grepl(
    "^(Validating supplied plan|Reading saved preparation|Validating preparation context|Validating saved preparation|Assembling preparation report|Validating completed preparation)",
    messages
  )]
  phases <- sub(" finished in [0-9.]+ s[.]$", " finished.", phases)
  expect_identical(
    phases,
    c(
      "Validating supplied plan.",
      "Validating supplied plan finished.",
      "Reading saved preparation.",
      "Reading saved preparation finished.",
      "Validating preparation context.",
      "Validating preparation context finished.",
      "Validating saved preparation.",
      "Validating saved preparation finished.",
      "Assembling preparation report.",
      "Assembling preparation report finished.",
      "Validating completed preparation.",
      "Validating completed preparation finished."
    )
  )
  totals <- messages[startsWith(messages, "Preparation:")]
  expect_identical(
    totals,
    sprintf("Preparation: %d/3 packages processed; 3/3 ready.", 1:3)
  )
  cached <- messages[grepl("cached package", messages, fixed = TRUE)]
  expect_identical(sub(".*[(]cached package ", "", cached), paste0(1:3, "/3)."))
  expect_message(revdep_prepare(plan), NA)
})

# A small live-log fixture distinguishes both sides without expensive real checks.
test_that("comparison progress reports side and stage and tolerates missing logs", {
  skip_if_stock_tools_unavailable()
  root <- tempfile("comparison-progress-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  write_revdep_run_candidate(root)
  todo <- data.frame(package = "TargetPkg", status = "todo")
  local_mocked_bindings(
    stock_namespace_function = function(name) {
      switch(
        name,
        db_disconnect = function(...) invisible(NULL),
        db_metadata_get = function(...) "run",
        dir_find = function(checkout, what) file.path(checkout, "checks")
      )
    },
    .package = "revdeprunner"
  )
  local_mocked_bindings(
    revdep_todo = function(...) todo,
    .package = "revdepcheck"
  )
  old <- file.path(
    root,
    "checks",
    "TargetPkg",
    "old",
    "TargetPkg.Rcheck",
    "00check.log"
  )
  new <- file.path(
    root,
    "checks",
    "TargetPkg",
    "new",
    "TargetPkg.Rcheck",
    "00check.log"
  )
  dir.create(dirname(old), recursive = TRUE)
  dir.create(dirname(new), recursive = TRUE)
  writeLines("* checking stale previous run ...", old)
  Sys.setFileTime(old, Sys.time() - 60)
  clock <- 0
  tick <- revdeprunner:::stock_comparison_progress(root, clock = function() {
    clock
  })
  initial <- capture.output(tick(), type = "message")
  expect_length(initial, 1L)
  expect_match(initial, "0/1 targets complete")
  expect_message(tick(), NA)
  writeLines("* checking whether package 'TargetPkg' can be installed ...", old)
  Sys.setFileTime(old, Sys.time() + 1)
  clock <- 10
  expect_message(
    tick(),
    "TargetPkg [(]old[)]; checking whether.*elapsed 10.0 s; log:"
  )
  expect_message(tick(), NA)
  clock <- 70
  expect_message(tick(), "TargetPkg [(]old[)].*elapsed 70.0 s")
  # Observe a partially written UTF-8 log without a final newline.
  writeBin(
    c(
      charToRaw("* checking examples ... OK\n* checking tests ..."),
      as.raw(c(0xe2, 0x82))
    ),
    new
  )
  Sys.setFileTime(new, Sys.time() + 2)
  clock <- 80
  expect_message(tick(), "TargetPkg [(]new[)]; checking tests.*elapsed 80.0 s")
  unlink(new)
  expect_message(tick(), "TargetPkg [(]old[)]")
  todo$status <- "done"
  expect_message(tick(), "1/1 targets complete")
})

test_that("unavailable progress database observations cannot fail comparison", {
  root <- tempfile("comparison-progress-unavailable-")
  disconnected <- 0L
  local_mocked_bindings(
    stock_namespace_function = function(name) {
      switch(
        name,
        db_metadata_get = function(...) stop("database busy"),
        db_disconnect = function(...) {
          disconnected <<- disconnected + 1L
          stop("database closed")
        }
      )
    },
    .package = "revdeprunner"
  )
  tick <- revdeprunner:::stock_comparison_progress(root)
  expect_message(tick(), NA)
  expect_identical(disconnected, 1L)
})
