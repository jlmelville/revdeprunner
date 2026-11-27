test_that("a named Bioconductor retry preserves the original pair and its logs", {
  skip_if_stock_tools_unavailable()
  root <- withr::local_tempdir()
  withr::local_envvar(c(
    R_MAKEVARS_USER = NA_character_,
    REVDEP_RUNNER_DATA = file.path(root, "data"),
    REVDEP_RUNNER_RUNS = file.path(root, "runs")
  ))
  local_mocked_bindings(revdep_plan_cran_database = function() NULL)
  candidate <- file.path(root, "candidate")
  dir.create(candidate)
  write_revdep_run_candidate(candidate)
  cran <- file.path(root, "cran")
  bioc <- file.path(root, "bioc")
  repos <- c(CRAN = paste0("file://", cran), BioCsoft = paste0("file://", bioc))
  add <- function(repo, package, imports = NA_character_, tests = NULL) {
    make_installable_source_archive(
      repo,
      package,
      "0.1",
      "no",
      imports = imports,
      tests = tests,
      test_file = "testthat.R"
    )
  }
  add(cran, "SubjectPkg")
  add(cran, "CranTarget", "SubjectPkg, OnlyDep")
  add(bioc, "OnlyDep")
  add(
    bioc,
    "BioTarget",
    "SubjectPkg, OnlyDep",
    tests = c(
      "makevars <- Sys.getenv('R_MAKEVARS_USER')",
      "repaired <- file.exists(makevars) && '# paired retry' %in% readLines(makevars)",
      "if (!repaired) {",
      "  cat(\"-- Failure ('test-pca.R:10:2'): PCA\\n\")",
      "  if (utils::packageVersion('SubjectPkg') >= '0.2') {",
      "    cat(\"-- Failure ('test-mds.R:19:3'): MDS\\n\")",
      "  }",
      "  cat(rep('common failure context', 20), sep = '\\n')",
      "  stop('test failures')",
      "}"
    )
  )
  for (repo in c(cran, bioc)) {
    tools::write_PACKAGES(file.path(repo, "src", "contrib"), type = "source")
  }
  default <- revdep_plan(candidate, repos = repos, cache = character())
  expect_identical(default$targets$package, "CranTarget")
  expect_true("OnlyDep" %in% default$requirements$package)
  prepared <- revdep_prepare(
    candidate,
    repos = repos,
    cache = character(),
    target_repos = names(repos)
  )
  expect_identical(prepared$summary$state, "ready")
  expect_identical(prepared$plan$targets$package, c("BioTarget", "CranTarget"))
  first <- revdep_check(prepared)
  bio <- first$results[first$results$package == "BioTarget", ]
  expect_identical(c(bio$old_status, bio$new_status), c("ERROR", "ERROR"))
  expect_identical(bio$stock_status, "+")
  expect_equal(nrow(first$changes), 0L)
  expect_identical(first$test_failures$change, "added")
  expect_identical(first$test_failures$location, "test-mds.R:19:3")
  expect_true(all(file.exists(unlist(first$test_failures[c(
    "old_log",
    "new_log"
  )]))))
  expect_match(
    paste(capture.output(print(first)), collapse = " "),
    "test-mds.R:19:3"
  )
  expect_equal(nrow(first$timings), 4L)
  expect_true(all(first$timings$seconds > 0))
  expect_true(all(file.exists(first$timings$library_log)))
  inventories <- lapply(first$timings$library_log, readLines)
  expect_true(all(vapply(
    inventories,
    function(lines) any(grepl("OnlyDep +[(]0.1[)]", lines)),
    logical(1L)
  )))
  original <- readRDS(first$evidence$checkpoint)

  makevars <- file.path(root, "miloR.Makevars")
  writeLines(c("# paired retry", "CXXFLAGS += -std=gnu++17"), makevars)
  withr::local_envvar(c(R_MAKEVARS_USER = makevars))
  retry <- revdep_check(prepared, retry_targets = "BioTarget")
  expect_identical(retry$results$outcome, c("unchanged", "not_checked"))
  expect_false(any(retry$results$new_status == "ERROR", na.rm = TRUE))
  expect_equal(nrow(retry$test_failures), 0L)
  expect_identical(retry$timings$package, rep("BioTarget", 2L))
  expect_identical(retry$timings$side, c("old", "new"))
  expect_identical(retry$evidence$makevars$contents, readLines(makevars))
  expect_identical(readLines(retry$evidence$makevars$copy), readLines(makevars))
  expect_false(identical(retry$evidence$checkpoint, first$evidence$checkpoint))
  expect_identical(readRDS(retry$evidence$original_checkpoint), original)
  expect_identical(revdep_check(prepared), first)
  expect_identical(revdep_check(prepared, retry_targets = "BioTarget"), retry)
  expect_error(
    revdep_check(prepared, retry_targets = "OnlyDep"),
    "selected target"
  )
})
