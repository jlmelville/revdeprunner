test_that("named targets survive a snapshot change and reach both comparison sides", {
  skip_if_stock_tools_unavailable()
  root <- withr::local_tempdir()
  candidate <- file.path(root, "candidate")
  dir.create(candidate)
  write_revdep_run_candidate(candidate)
  repository <- file.path(root, "repository")
  contrib <- file.path(repository, "src", "contrib")
  repos <- c(CRAN = paste0("file://", repository))
  withr::local_envvar(c(
    REVDEP_RUNNER_DATA = file.path(root, "data"),
    REVDEP_RUNNER_RUNS = file.path(root, "runs")
  ))
  local_mocked_bindings(
    revdep_plan_cran_database = function() NULL,
    .package = "revdeprunner"
  )
  add_package <- function(package, imports = NA_character_) {
    make_installable_source_archive(
      repository,
      package = package,
      version = "0.1",
      needs_compilation = "no",
      imports = imports
    )
  }
  add_package("SubjectPkg")
  add_package("DirectPkg", "SubjectPkg")
  add_package("HDStIM", "DirectPkg")
  tools::write_PACKAGES(contrib, type = "source")
  plan_named <- function() {
    revdep_plan(
      candidate,
      recursive = TRUE,
      recursive_packages = "HDStIM",
      repos = repos,
      cache = character()
    )
  }
  first <- plan_named()
  expect_setequal(
    first$targets$package[first$targets$selected],
    c("DirectPkg", "HDStIM")
  )

  add_package("AnotherDirect", "SubjectPkg")
  add_package("OtherIndirect", "DirectPkg")
  tools::write_PACKAGES(contrib, type = "source")
  plan <- plan_named()
  expected <- c("AnotherDirect", "DirectPkg", "HDStIM")
  expect_false(identical(first$summary$snapshot_id, plan$summary$snapshot_id))
  expect_setequal(plan$targets$package[plan$targets$selected], expected)
  expect_setequal(plan$workload$target, expected)
  prepared <- revdep_prepare(plan)
  expect_identical(prepared$summary$state, "ready")
  expect_setequal(prepared$evidence$report$results$package, expected)
  result <- revdep_check(prepared)
  expect_stock_comparison_success(result)
  expect_setequal(result$results$package, expected)
  expect_true(all(result$results$outcome == "unchanged"))
  expect_false(anyNA(result$results[c("old_status", "new_status")]))

  # Checkout preparation reuses a checkpoint only for the same set of names.
  prepare_named <- function(packages) {
    revdep_prepare(
      candidate,
      recursive = TRUE,
      recursive_packages = packages,
      repos = repos,
      cache = character()
    )
  }
  one <- prepare_named("OtherIndirect")
  expect_setequal(
    one$evidence$report$results$package,
    c("AnotherDirect", "DirectPkg", "OtherIndirect")
  )
  both <- prepare_named(c("HDStIM", "OtherIndirect"))
  expect_false(identical(one$evidence$checkpoint, both$evidence$checkpoint))
  expect_identical(
    prepare_named(c("OtherIndirect", "HDStIM"))$evidence$checkpoint,
    both$evidence$checkpoint
  )
})
