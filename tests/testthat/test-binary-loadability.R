test_that("a binary losing an external shared library cannot pass public readiness", {
  skip_if_not(identical(unname(Sys.info()[["sysname"]]), "Linux"))
  skip_if_not(nzchar(Sys.which("cc")))
  local <- make_revdep_run_fixture()
  on.exit(unlink(local$fixture$root, recursive = TRUE), add = TRUE)
  root <- tempfile("binary-loadability-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  withr::local_envvar(c(
    REVDEP_RUNNER_DATA = file.path(root, "data"),
    REVDEP_RUNNER_RUNS = file.path(root, "runs"),
    CRANCACHE_DIR = file.path(root, "missing-crancache")
  ))
  external <- file.path(root, "librevdepfixture.so")
  code <- file.path(root, "external.c")
  writeLines("int external_fixture_value(void) { return 42; }", code)
  log <- file.path(root, "compile.log")
  expect_identical(
    system2(
      Sys.which("cc"),
      c("-shared", "-fPIC", "-o", shQuote(external), shQuote(code)),
      stdout = log,
      stderr = log
    ),
    0L
  )
  source <- file.path(root, "source")
  dir.create(source)
  archive <- local$fixture$source_archives$BuildPkg
  utils::untar(archive, exdir = source)
  package <- file.path(source, "BuildPkg")
  description <- readLines(file.path(package, "DESCRIPTION"))
  writeLines(
    sub("^NeedsCompilation:.*", "NeedsCompilation: yes", description),
    file.path(package, "DESCRIPTION")
  )
  dir.create(file.path(package, "src"))
  writeLines(
    c(
      "#include <R.h>",
      "#include <R_ext/Rdynload.h>",
      "extern int external_fixture_value(void);",
      "void R_init_BuildPkg(DllInfo *dll) {",
      "  (void)dll;",
      "  if (external_fixture_value() != 42) Rf_error(\"external fixture failed\");",
      "}"
    ),
    file.path(package, "src", "build.c")
  )
  writeLines(
    paste("PKG_LIBS =", external),
    file.path(package, "src", "Makevars")
  )
  writeLines(
    c("useDynLib(BuildPkg)", "export(build_value)"),
    file.path(package, "NAMESPACE")
  )
  withr::with_dir(
    source,
    utils::tar(archive, "BuildPkg", compression = "gzip", tar = "internal")
  )
  local$database$MD5sum[
    local$database$Package == "BuildPkg"
  ] <- unname(tools::md5sum(archive))
  local$database$NeedsCompilation[local$database$Package == "BuildPkg"] <- "yes"
  local_mocked_bindings(
    revdep_plan_package_database = function(repos) local$database,
    revdep_plan_cran_database = function() NULL,
    .package = "revdeprunner"
  )
  prepared <- revdep_prepare(local$fixture$paths[[1L]], repos = local$bases)
  expect_identical(prepared$summary$state, "ready")
  state <- readRDS(prepared$evidence$checkpoint)
  binary <- state$gate$source_preparations$BuildPkg
  library <- file.path(
    revdeprunner:::runtime_role_path(state$context$path_plan, "run"),
    "build-library"
  )
  loadNamespace("BuildPkg", lib.loc = library)
  on.exit(unloadNamespace("BuildPkg"), add = TRUE)
  expect_true(file.rename(external, paste0(external, ".disabled")))
  # A namespace already loaded in this process must not hide the loader failure.
  expect_error(
    revdep_check(prepared),
    "not loadable in the current environment",
    fixed = TRUE
  )
  unlink(
    revdeprunner:::runtime_role_path(state$context$path_plan, "run"),
    recursive = TRUE
  )
  failed <- revdep_prepare(local$fixture$paths[[1L]], repos = local$bases)
  problem <- failed$problems[
    failed$problems$package == "BuildPkg",
    ,
    drop = FALSE
  ]
  expect_identical(problem$outcome, "load-failure")
  expect_identical(problem$stage, "load")
  expect_match(problem$diagnostic_excerpt, "librevdepfixture.so", fixed = TRUE)
  expect_true(file.exists(problem$stderr_path))
  expect_identical(
    readRDS(failed$evidence$checkpoint)$gate$source_preparations$BuildPkg,
    binary
  )
  expect_true(file.rename(paste0(external, ".disabled"), external))
  runner <- revdeprunner:::run_source_preparation_process
  local_mocked_bindings(
    run_source_preparation_process = function(r_executable, arguments, ...) {
      if ("--build" %in% arguments)
        stop("Repair must reuse the verified binary")
      runner(r_executable, arguments, ...)
    },
    .package = "revdeprunner"
  )
  repaired <- revdep_prepare(local$fixture$paths[[1L]], repos = local$bases)
  expect_identical(repaired$summary$state, "ready")
})
