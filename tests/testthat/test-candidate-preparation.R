test_that("candidate-only requirements prepare and reach both comparison environments", {
  skip_if_not(revdep_run_stock_tools_supported())
  local <- make_revdep_run_fixture()
  on.exit(unlink(local$fixture$root, recursive = TRUE), add = TRUE)
  root <- tempfile("candidate-preparation-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  withr::local_envvar(c(
    REVDEP_RUNNER_DATA = file.path(root, "data"),
    REVDEP_RUNNER_RUNS = file.path(root, "runs"),
    CRANCACHE_DIR = file.path(root, "missing-crancache")
  ))
  candidate <- local$fixture$paths[[1L]]
  description <- readLines(file.path(candidate, "DESCRIPTION"))
  writeLines(
    c(description, "Imports: CandidateDep (>= 1.0)"),
    file.path(candidate, "DESCRIPTION")
  )
  archive <- make_installable_source_archive(
    local$fixture$repository_root,
    package = "CandidateDep",
    version = "1.0",
    needs_compilation = "no",
    imports = "SubjectPkg"
  )
  row <- local$database[local$database$Package == "SubjectPkg", , drop = FALSE]
  row$Package <- "CandidateDep"
  row$Version <- "1.0"
  row$Imports <- "SubjectPkg"
  row$MD5sum <- unname(tools::md5sum(archive))
  local$database <- rbind(local$database, row)
  local_mocked_bindings(
    revdep_plan_package_database = function(repos) local$database,
    revdep_plan_cran_database = function() NULL,
    .package = "revdeprunner"
  )
  prepared <- revdep_prepare(candidate, repos = local$bases)
  expect_identical(prepared$summary$state, "ready")
  expect_true("CandidateDep" %in% prepared$evidence$report$results$package)
  result <- revdep_check(prepared)
  expect_identical(result$summary$state, "success")
  initialization <- readRDS(result$evidence$checkpoint)$initialization
  for (library in revdeprunner:::stock_subject_library_paths(
    initialization$paths,
    "SubjectPkg"
  )) {
    expect_identical(
      as.character(utils::packageVersion("CandidateDep", lib.loc = library)),
      "1.0"
    )
  }
  writeLines(
    c(description, "Imports: CandidateDep (>= 2.0)"),
    file.path(candidate, "DESCRIPTION")
  )
  expect_error(
    revdep_check(prepared),
    "dependency requirements have changed",
    fixed = TRUE
  )
  expect_error(
    revdep_prepare(candidate, repos = local$bases),
    "CandidateDep requires >= 2.0",
    fixed = TRUE
  )
})
