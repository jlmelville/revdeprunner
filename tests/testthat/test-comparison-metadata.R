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

  # revdep_details replaces the comparison's outer class; the public changes
  # projection must apply the same repair as summary classification.
  class(comparison) <- "revdepcheck_details"
  local_mocked_bindings(
    revdep_details = function(...) comparison,
    .package = "revdepcheck"
  )
  changes <- revdeprunner:::stock_adapter_changes(
    "unused",
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
