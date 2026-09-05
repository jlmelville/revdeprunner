revdep_plan_fixture_database <- function() {
  packages <- c(
    "mize",
    "RcppHNSW",
    "uwot",
    "rnndescent",
    "pureDep",
    "nativeDep",
    "deepOnly",
    "mizeDirect",
    "mizeDeep",
    "hnswDirect",
    "blocking",
    "bbknnR",
    "blockingDeep",
    paste0("uwotConsumer", 1:5)
  )
  database <- new.env(parent = emptyenv())
  database$data <- data.frame(
    Package = packages,
    Version = rep("1.0.0", length(packages)),
    Depends = NA_character_,
    Imports = NA_character_,
    LinkingTo = NA_character_,
    Suggests = NA_character_,
    Repository = rep(revdep_plan_fixture_contrib(), length(packages)),
    NeedsCompilation = rep("no", length(packages)),
    MD5sum = rep(strrep("a", 32L), length(packages)),
    stringsAsFactors = FALSE
  )
  set_field <- function(package, field, value) {
    database$data[database$data$Package == package, field] <- value
  }
  set_field("uwot", "Imports", "RcppHNSW, pureDep")
  set_field("uwot", "Suggests", "rnndescent")
  set_field("uwot", "NeedsCompilation", "yes")
  set_field("mizeDirect", "Imports", "mize, pureDep")
  set_field("mizeDeep", "Imports", "mizeDirect")
  set_field("hnswDirect", "Imports", "RcppHNSW, nativeDep")
  set_field("hnswDirect", "NeedsCompilation", "yes")
  set_field("blocking", "Imports", "rnndescent, RcppHNSW, nativeDep")
  set_field("blocking", "Suggests", "missingOS")
  set_field("blocking", "NeedsCompilation", "yes")
  set_field("bbknnR", "Imports", "rnndescent, pureDep")
  set_field("blockingDeep", "Imports", "blocking")
  for (index in 1:5) {
    package <- paste0("uwotConsumer", index)
    set_field(package, "Imports", "uwot")
  }
  set_field("uwotConsumer1", "Suggests", "deepOnly")
  set_field("nativeDep", "NeedsCompilation", "yes")
  database$data
}

revdep_plan_fixture_metadata <- function(database) {
  data.frame(
    Package = database$Package,
    Version = database$Version,
    SystemRequirements = ifelse(
      database$Package == "nativeDep",
      "libnative-dev",
      NA_character_
    ),
    stringsAsFactors = FALSE
  )
}

revdep_plan_fixture_contrib <- function() {
  "https://example.test/cran/src/contrib"
}

revdep_plan_fixture_repos <- function() {
  c(CRAN = "https://example.test/cran")
}

revdep_plan_fixture_bioc_repos <- function() {
  c(
    BioCsoft = "https://example.test/bioc",
    revdep_plan_fixture_repos()
  )
}

revdep_plan_fixture_bioc_database <- function() {
  database <- revdep_plan_fixture_database()
  database[database$Package == "hnswDirect", "Imports"] <-
    "RcppHNSW, nativeDep, bioDependency"
  deep <- database[database$Package == "hnswDirect", , drop = FALSE]
  deep$Package <- "hnswDeep"
  deep$Imports <- "hnswDirect"
  deep$NeedsCompilation <- "no"
  database <- rbind(database, deep)
  additions <- database[rep(1L, 5L), , drop = FALSE]
  additions$Package <- c(
    "bioDependency",
    "bioConsumer",
    "hnswDirect",
    "hnswDeep",
    "RcppHNSW"
  )
  additions$Version <- c(
    "1.0.0",
    "1.0.0",
    "9.9.9",
    "9.9.9",
    "9.9.9"
  )
  additions$Depends <- NA_character_
  additions$Imports <- c(
    NA_character_,
    "RcppHNSW",
    NA_character_,
    NA_character_,
    NA_character_
  )
  additions$LinkingTo <- NA_character_
  additions$Suggests <- NA_character_
  additions$Repository <- utils::contrib.url(
    revdep_plan_fixture_bioc_repos()[["BioCsoft"]],
    type = "source"
  )
  additions$NeedsCompilation <- "no"
  rbind(database, additions)
}

revdep_plan_fixture_checkout <- function(package) {
  root <- tempfile(paste0("revdep-plan-", tolower(package), "-"))
  dir.create(root)
  writeLines(
    c(
      paste("Package:", package),
      "Version: 1.0.0.9000"
    ),
    file.path(root, "DESCRIPTION")
  )
  root
}

local_revdep_plan_queries <- function(
  database = revdep_plan_fixture_database(),
  metadata = revdep_plan_fixture_metadata(database),
  .env = parent.frame()
) {
  testthat::local_mocked_bindings(
    revdep_plan_package_database = function(repos) database,
    revdep_plan_cran_database = function() metadata,
    .package = "revdeprunner",
    .env = .env
  )
}

test_that("one public planning call substitutes all four package checkouts", {
  database <- revdep_plan_fixture_database()
  local_revdep_plan_queries(database)
  packages <- c("mize", "RcppHNSW", "uwot", "rnndescent")
  roots <- vapply(packages, revdep_plan_fixture_checkout, character(1L))
  on.exit(unlink(roots, recursive = TRUE), add = TRUE)

  plans <- lapply(roots, function(root) {
    revdep_plan(
      root,
      cache = character(),
      repos = revdep_plan_fixture_repos()
    )
  })

  expect_true(all(vapply(plans, inherits, logical(1L), "revdep_plan")))
  expect_identical(
    unname(vapply(plans, function(plan) plan$summary$package, character(1L))),
    packages
  )
  expect_identical(
    unname(vapply(
      plans,
      function(plan) plan$summary$baseline_version,
      character(1L)
    )),
    rep("1.0.0", 4L)
  )
  expect_identical(
    unname(vapply(
      plans,
      function(plan) plan$summary$direct_targets,
      integer(1L)
    )),
    c(1L, 3L, 5L, 3L)
  )
  expect_true(all(vapply(
    plans,
    function(plan) {
      all(plan$targets$selected == (plan$targets$role == "direct"))
    },
    logical(1L)
  )))
  expect_identical(
    names(plans[[1L]]),
    c(
      "summary",
      "targets",
      "requirements",
      "unavailable",
      "repository_alternates"
    )
  )
  expect_identical(
    capture.output(print(plans[[1L]])),
    c(
      paste0(
        "Reverse-dependency plan for mize 1.0.0.9000 ",
        "(repository baseline 1.0.0)"
      ),
      "Targets: 1 selected (1 direct; 1 recursive-only candidates)",
      "Preparation: 1 requirements; 0 reusable; 1 source builds (0 native)"
    )
  )
})

test_that("Bioconductor resolves dependencies without widening CRAN targets", {
  database <- revdep_plan_fixture_bioc_database()
  local_revdep_plan_queries(database, revdep_plan_fixture_metadata(database))
  root <- revdep_plan_fixture_checkout("RcppHNSW")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  plan <- revdep_plan(
    root,
    cache = character(),
    repos = revdep_plan_fixture_bioc_repos()
  )

  expect_false("bioConsumer" %in% plan$targets$package)
  expect_true("bioDependency" %in% plan$requirements$package)
  expect_identical(plan$summary$direct_targets, 3L)
  expect_identical(plan$summary$baseline_version, "1.0.0")
  hnsw <- plan$targets[plan$targets$package == "hnswDirect", ]
  expect_identical(hnsw$version, "1.0.0")
  expect_identical(hnsw$relationship, "Imports")
  expect_identical(hnsw$needs_compilation, "yes")
  hnsw_deep <- plan$targets[plan$targets$package == "hnswDeep", ]
  expect_identical(hnsw_deep$role, "recursive-strong-only")
  expect_identical(hnsw_deep$direct_roots, "hnswDirect")
})

test_that("default planning adds standard dependency repositories", {
  database <- revdep_plan_fixture_database()
  observed <- NULL
  local_mocked_bindings(
    revdep_plan_package_database = function(repos) {
      observed <<- repos
      database
    },
    revdep_plan_cran_database = function() {
      revdep_plan_fixture_metadata(database)
    },
    .package = "revdeprunner"
  )
  withr::local_options(repos = revdep_plan_fixture_repos())
  root <- revdep_plan_fixture_checkout("RcppHNSW")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  plan <- revdep_plan(root, cache = character())

  expect_identical(observed[["CRAN"]], revdep_plan_fixture_repos()[["CRAN"]])
  expect_true("BioCsoft" %in% names(observed))
  expect_identical(plan$summary$direct_targets, 3L)
})

test_that("unnamed CRAN policy retains all explicit repository targets", {
  database <- revdep_plan_fixture_bioc_database()
  duplicate <-
    database$Repository ==
      utils::contrib.url(
        revdep_plan_fixture_bioc_repos()[["BioCsoft"]],
        type = "source"
      ) &
    database$Package %in% c("hnswDirect", "hnswDeep", "RcppHNSW")
  database <- database[!duplicate, , drop = FALSE]
  repos <- revdep_plan_fixture_bioc_repos()
  names(repos) <- c("Primary", "Secondary")
  local_revdep_plan_queries(database, revdep_plan_fixture_metadata(database))
  root <- revdep_plan_fixture_checkout("RcppHNSW")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  plan <- revdep_plan(root, cache = character(), repos = repos)

  expect_true("bioConsumer" %in% plan$targets$package)
  expect_true("bioDependency" %in% plan$requirements$package)
  expect_identical(plan$summary$direct_targets, 4L)
})

test_that("the package checkout is the only required argument", {
  database <- revdep_plan_fixture_database()
  database$Repository <- "https://cloud.r-project.org/src/contrib"
  metadata <- revdep_plan_fixture_metadata(database)
  local_revdep_plan_queries(database, metadata)
  old_options <- options(repos = c(CRAN = "@CRAN@"))
  old_cache <- Sys.getenv("CRANCACHE_DIR", unset = NA_character_)
  Sys.setenv(CRANCACHE_DIR = tempfile("missing-crancache-"))
  runtime <- tempfile("revdep-plan-runtime-")
  dir.create(runtime)
  withr::local_envvar(REVDEP_RUNNER_DATA = file.path(runtime, "data"))
  on.exit(options(old_options), add = TRUE)
  on.exit(
    {
      if (is.na(old_cache)) {
        Sys.unsetenv("CRANCACHE_DIR")
      } else {
        Sys.setenv(CRANCACHE_DIR = old_cache)
      }
    },
    add = TRUE
  )
  root <- revdep_plan_fixture_checkout("mize")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  plan <- revdep_plan(root)

  expect_s3_class(plan, "revdep_plan")
  expect_identical(plan$summary$package, "mize")
  expect_identical(plan$summary$cache_roots, 0L)
})

test_that("default planning reuses the runner binary cache", {
  database <- revdep_plan_fixture_database()
  local_revdep_plan_queries(database)
  root <- revdep_plan_fixture_checkout("rnndescent")
  runtime <- tempfile("revdep-plan-runner-cache-")
  contribution <- file.path(
    runtime,
    "data",
    "binary-cache",
    "src",
    "contrib"
  )
  dir.create(contribution, recursive = TRUE)
  on.exit(unlink(c(root, runtime), recursive = TRUE), add = TRUE)
  r_major_minor <- sub(
    "^([0-9]+[.][0-9]+).*$",
    "\\1",
    as.character(getRversion())
  )
  built <- paste(
    paste0("R ", r_major_minor, ".0"),
    R.version$platform,
    "2026-09-02",
    "unix",
    sep = "; "
  )
  make_test_archive(
    contribution,
    repository = ".",
    package = "pureDep",
    version = "1.0.0",
    needs_compilation = "no",
    built = built,
    filename = paste0(
      "pureDep_1.0.0_R_",
      R.version$platform,
      ".tar.gz"
    )
  )
  writeLines("Package: pureDep", file.path(contribution, "PACKAGES"))
  withr::local_envvar(c(
    REVDEP_RUNNER_DATA = file.path(runtime, "data"),
    CRANCACHE_DIR = tempfile("missing-crancache-")
  ))

  plan <- revdep_plan(root, repos = revdep_plan_fixture_repos())
  disabled <- revdep_plan(
    root,
    cache = character(),
    repos = revdep_plan_fixture_repos()
  )

  pure <- plan$requirements$package == "pureDep"
  expect_identical(plan$requirements$action[pure], "reuse")
  expect_identical(plan$requirements$cache_source[pure], contribution)
  expect_identical(plan$summary$cache_roots, 1L)
  expect_identical(disabled$summary$cache_roots, 0L)
  expect_true(all(disabled$requirements$action == "download-build"))
})

test_that("bounded recursive selection is stable and explains direct roots", {
  local_revdep_plan_queries()
  root <- revdep_plan_fixture_checkout("rnndescent")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  set.seed(20260901)
  random_state <- .Random.seed

  plan <- revdep_plan(
    root,
    recursive = TRUE,
    max_recursive = 2,
    sample_seed = 7,
    cache = character(),
    repos = revdep_plan_fixture_repos()
  )
  repeated <- revdep_plan(
    root,
    recursive = TRUE,
    max_recursive = 2,
    sample_seed = 7,
    cache = character(),
    repos = revdep_plan_fixture_repos()
  )
  alternate <- revdep_plan(
    root,
    recursive = TRUE,
    max_recursive = 2,
    sample_seed = 8,
    cache = character(),
    repos = revdep_plan_fixture_repos()
  )

  expect_identical(.Random.seed, random_state)
  expect_identical(plan, repeated)
  expect_false(identical(plan$summary$sample_key, alternate$summary$sample_key))
  expect_identical(plan$summary$selected_targets, 5L)
  expect_true(all(plan$targets$selected[plan$targets$role == "direct"]))
  expect_identical(
    sort(plan$targets$sample_rank[plan$targets$role != "direct"]),
    seq_len(plan$summary$recursive_only_targets)
  )

  direct <- plan$targets[plan$targets$role == "direct", ]
  expect_identical(
    direct$relationship[match(c("bbknnR", "blocking", "uwot"), direct$package)],
    c("Imports", "Imports", "Suggests")
  )
  recursive <- plan$targets[plan$targets$role != "direct", ]
  expect_identical(
    recursive$direct_roots[recursive$package == "blockingDeep"],
    "blocking"
  )
  expect_true(all(
    recursive$direct_roots[startsWith(recursive$package, "uwotConsumer")] ==
      "uwot"
  ))
  expect_identical(plan$summary$largest_recursive_root, "uwot")
  expect_identical(plan$summary$largest_recursive_root_targets, 5L)
})

test_that("the recursive bound is applied before requirement discovery", {
  local_revdep_plan_queries()
  root <- revdep_plan_fixture_checkout("rnndescent")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  plan <- revdep_plan(
    root,
    recursive = TRUE,
    max_recursive = 0,
    cache = character(),
    repos = revdep_plan_fixture_repos()
  )

  expect_identical(plan$summary$selected_targets, 3L)
  expect_false("deepOnly" %in% plan$requirements$package)
  expect_true(all(is.na(
    plan$targets$preparation_requirements[!plan$targets$selected]
  )))
})

test_that("plans report cache reuse and unavailable dependency evidence", {
  database <- revdep_plan_fixture_database()
  local_revdep_plan_queries(database)
  root <- revdep_plan_fixture_checkout("rnndescent")
  cache <- tempfile("revdep-plan-cache-")
  dir.create(cache)
  on.exit(unlink(c(root, cache), recursive = TRUE), add = TRUE)
  r_major_minor <- sub(
    "^([0-9]+[.][0-9]+).*$",
    "\\1",
    as.character(getRversion())
  )
  built <- paste(
    paste0("R ", r_major_minor, ".0"),
    R.version$platform,
    "2026-09-01",
    "unix",
    sep = "; "
  )
  make_test_archive(
    cache,
    repository = "cran-bin/src/contrib",
    package = "pureDep",
    version = "1.0.0",
    needs_compilation = "no",
    built = built,
    filename = paste0(
      "pureDep_1.0.0_R_",
      R.version$platform,
      ".tar.gz"
    )
  )
  before <- snapshot_test_cache(cache)

  plan <- revdep_plan(
    root,
    cache = cache,
    repos = revdep_plan_fixture_repos()
  )

  expect_identical(snapshot_test_cache(cache), before)
  expect_identical(
    plan$requirements$action[plan$requirements$package == "pureDep"],
    "reuse"
  )
  expect_identical(
    plan$requirements$action[plan$requirements$package == "nativeDep"],
    "download-build"
  )
  expect_identical(
    plan$requirements$system_requirements[
      plan$requirements$package == "nativeDep"
    ],
    "libnative-dev"
  )
  expect_identical(
    plan$unavailable$dependency,
    "missingOS"
  )
  expect_identical(plan$unavailable$relationship, "Suggests")
  expect_identical(plan$unavailable$affected_targets, "blocking")
  expect_identical(plan$summary$reusable_binaries, 1L)
  expect_gte(plan$summary$native_source_builds, 1L)
  expect_identical(plan$summary$declared_system_requirements, 1L)
})

test_that("canonical contribution rows win over Recommended alternates", {
  database <- revdep_plan_fixture_database()
  alternate <- database[database$Package == "mize", , drop = FALSE]
  alternate$Version <- "0.9.0"
  alternate$Repository <- paste0(
    revdep_plan_fixture_contrib(),
    "/4.5.0/Recommended"
  )
  local_revdep_plan_queries(rbind(alternate, database))
  root <- revdep_plan_fixture_checkout("mize")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  plan <- revdep_plan(
    root,
    cache = character(),
    repos = revdep_plan_fixture_repos()
  )

  expect_identical(plan$summary$baseline_version, "1.0.0")
  expect_identical(plan$summary$repository_alternates, 1L)
  expect_identical(plan$repository_alternates$package, "mize")
  expect_identical(plan$repository_alternates$selected_version, "1.0.0")
  expect_identical(plan$repository_alternates$discarded_version, "0.9.0")
})

test_that("genuinely ambiguous canonical rows remain errors", {
  database <- revdep_plan_fixture_database()
  duplicate <- database[database$Package == "mize", , drop = FALSE]
  duplicate$Version <- "0.9.0"
  local_revdep_plan_queries(rbind(duplicate, database))
  root <- revdep_plan_fixture_checkout("mize")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  expect_error(
    revdep_plan(
      root,
      cache = character(),
      repos = revdep_plan_fixture_repos()
    ),
    "duplicate package rows within a repository",
    fixed = TRUE
  )
})

test_that("non-Recommended child repository rows remain errors", {
  database <- revdep_plan_fixture_database()
  alternate <- database[database$Package == "mize", , drop = FALSE]
  alternate$Version <- "0.9.0"
  alternate$Repository <- paste0(
    revdep_plan_fixture_contrib(),
    "/staging"
  )
  local_revdep_plan_queries(rbind(alternate, database))
  root <- revdep_plan_fixture_checkout("mize")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  expect_error(
    revdep_plan(
      root,
      cache = character(),
      repos = revdep_plan_fixture_repos()
    ),
    "duplicate package rows within a repository",
    fixed = TRUE
  )
})

test_that("missing CRAN enrichment remains explicit unknown metadata", {
  database <- revdep_plan_fixture_database()
  local_revdep_plan_queries(database, metadata = NULL)
  root <- revdep_plan_fixture_checkout("mize")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  plan <- revdep_plan(
    root,
    cache = character(),
    repos = revdep_plan_fixture_repos()
  )

  expect_identical(plan$summary$cran_metadata, "unavailable")
  expect_gt(plan$summary$unknown_system_requirements, 0L)
  expect_true(all(
    plan$requirements$system_requirements_status == "unknown"
  ))
})

test_that("public planning controls reject inactive or lossy values", {
  local_revdep_plan_queries()
  root <- revdep_plan_fixture_checkout("mize")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  call <- function(...) {
    revdep_plan(
      root,
      ...,
      cache = character(),
      repos = revdep_plan_fixture_repos()
    )
  }

  expect_error(call(recursive = NA), "must be `TRUE` or `FALSE`", fixed = TRUE)
  expect_error(
    call(max_recursive = 1),
    "requires `recursive = TRUE`",
    fixed = TRUE
  )
  expect_error(
    call(recursive = TRUE, sample_seed = 1),
    "requires `max_recursive`",
    fixed = TRUE
  )
  invalid <- c(-1, 1.5, Inf, .Machine$integer.max + 1)
  for (value in invalid) {
    expect_error(
      call(recursive = TRUE, max_recursive = value),
      "non-negative integer",
      fixed = TRUE
    )
  }
})

# This internal seam is exercised directly because it proves that the public
# bounded plan reaches the preparation engine before any downloads or builds.
test_that("bounded public plans create only their selected universe", {
  local_revdep_plan_queries()
  root <- revdep_plan_fixture_checkout("rnndescent")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  plan <- revdep_plan(
    root,
    recursive = TRUE,
    max_recursive = 1L,
    cache = character(),
    repos = revdep_plan_fixture_repos()
  )
  runtime <- tempfile("bounded-plan-runtime-")
  dir.create(runtime)
  on.exit(unlink(runtime, recursive = TRUE), add = TRUE)
  storage <- list(
    data = revdeprunner:::ensure_revdep_directory(
      file.path(runtime, "data"),
      "test data"
    ),
    runs = revdeprunner:::ensure_revdep_directory(
      file.path(runtime, "runs"),
      "test runs"
    )
  )
  request <- revdeprunner:::revdep_prepare_plan_request(plan, storage)
  context <- revdeprunner:::revdep_prepare_context(plan, request, storage)
  selected <- plan$targets$package[plan$targets$selected]

  expect_identical(context$universe$cohort_policy, "selected")
  expect_identical(context$universe$targets$package, selected)
  expect_true(all(
    plan$targets$package[plan$targets$role == "direct"] %in% selected
  ))
  expect_false(any(
    context$universe$dependencies$target %in%
      plan$targets$package[!plan$targets$selected]
  ))
  expect_error(
    revdep_prepare(plan, recursive = FALSE),
    "cannot be combined with planning arguments",
    fixed = TRUE
  )
})
