# R-devel CI produced valid check errors but no parsed R-version string. This
# boundary test protects comparison and visible changes without needing R-devel.
test_that("missing display metadata does not erase comparison problems", {
  old <- structure(
    list(
      package = "BuildPkg",
      version = "2.0",
      platform = "linux",
      rversion = character(),
      timeout = FALSE,
      errors = character(),
      warnings = character(),
      notes = character()
    ),
    class = "rcmdcheck"
  )
  new <- old
  new$errors <- "checking tests ... ERROR\nThe candidate test failed."
  comparison <- rcmdcheck::compare_checks(old, new)
  normalized <- revdeprunner:::normalize_stock_comparison(comparison)
  expect_identical(normalized$status, "-")
  expect_identical(normalized$cmp$output, new$errors)
  expect_identical(normalized$cmp$change, 1)

  changes <- revdeprunner:::stock_adapter_changes(
    list(BuildPkg = normalized),
    data.frame(package = "BuildPkg", outcome = "changed")
  )
  expect_identical(changes$severity, "error")
  expect_identical(changes$change, "added")
  expect_identical(changes$message, new$errors)

  old$rversion <- new$rversion <- "R version 4.5.2"
  ordinary <- rcmdcheck::compare_checks(old, new)
  expect_identical(
    revdeprunner:::normalize_stock_comparison(ordinary),
    ordinary
  )
})

test_that("shared test errors report removed locations and keep both log paths", {
  old <- structure(
    list(
      test_fail = list(
        testthat = paste(
          "-- Failure ('test-one.R:12:5'): common",
          "-- Error ('test-two.R:77'): removed",
          sep = "\n"
        )
      )
    ),
    class = "rcmdcheck"
  )
  new <- old
  new$test_fail$testthat <- "-- Failure ('test-one.R:12:5'): common"
  details <- list(Pkg = list(old = list(old), new = new))
  results <- data.frame(
    package = "Pkg",
    old_status = "ERROR",
    new_status = "ERROR"
  )
  changes <- revdeprunner:::stock_test_failure_changes(
    details,
    results,
    "/checks"
  )
  expect_identical(changes$change, "removed")
  expect_identical(changes$location, "test-two.R:77")
  expect_identical(
    changes$old_log,
    "/checks/Pkg/old/Pkg.Rcheck/tests/testthat.Rout.fail"
  )
  expect_identical(
    changes$new_log,
    "/checks/Pkg/new/Pkg.Rcheck/tests/testthat.Rout.fail"
  )
  details$Pkg$new$test_fail <- list()
  expect_equal(
    nrow(revdeprunner:::stock_test_failure_changes(
      details,
      results,
      "/checks"
    )),
    0
  )
})

test_that("budget guidance counts only version-matched pending sides", {
  targets <- data.frame(package = c("Slow", "Done", "Other"), version = "1.0")
  timings <- data.frame(
    package = c("Slow", "Slow", "Done"),
    version = c("1.0", "0.9", "1.0"),
    side = c("old", "new", "new"),
    seconds = c(3600, 7200, 7200)
  )
  budget <- function(seconds, verbose = TRUE) {
    revdeprunner:::stock_check_budget(
      timings,
      targets,
      c("Slow", "Other"),
      seconds,
      verbose
    )
  }
  expect_message(
    budget(7200),
    "1/4 pending sides: 1.00 cumulative check-hours; longest Slow old, 60.0 min"
  )
  expect_warning(budget(3600, FALSE), "one-worker process budget")
  expect_silent(budget(7200, FALSE))
  expect_error(
    revdep_check(NULL, prior_timings = rbind(timings, timings)),
    "prior_timings"
  )
})
