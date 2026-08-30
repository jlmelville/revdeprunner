# These private tests protect the pure handoff from accepted binary reuse to
# later source acquisition without exposing a public API or performing I/O.

# nolint start: object_usage_linter.
source_acquisition_fixture_repositories <- function() {
  c(
    CRAN = "https://primary.example.test/src/contrib",
    Secondary = "https://secondary.example.test/src/contrib"
  )
}

source_acquisition_fixture_database <- function() {
  primary <- source_acquisition_fixture_repositories()[["CRAN"]]
  secondary <- source_acquisition_fixture_repositories()[["Secondary"]]
  data.frame(
    Package = c(
      "FilePkg",
      "SubjectPkg",
      "BuildPkg",
      "HitPkg",
      "BuildPkg"
    ),
    Version = c("3.0", "0.1", "2.0", "1.0", "9.0"),
    Depends = c("SubjectPkg", NA, NA, "SubjectPkg", NA),
    Imports = c(NA, NA, "SubjectPkg", "MissingPkg", "SubjectPkg"),
    LinkingTo = NA_character_,
    Suggests = NA_character_,
    Repository = c(primary, primary, primary, primary, secondary),
    File = c(
      "custom/FilePkg_3.0.tar.gz",
      NA,
      NA,
      NA,
      NA
    ),
    MD5sum = c(
      "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
      "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB",
      "CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC",
      "DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD",
      "EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE"
    ),
    NeedsCompilation = c("no", "no", "yes", "no", "yes"),
    SystemRequirements = c(
      NA,
      NA,
      "libxml2 (>= 2.9)",
      NA,
      "secondary-only-library"
    ),
    stringsAsFactors = FALSE
  )
}

source_acquisition_fixture_contracts <- function(
  database = source_acquisition_fixture_database(),
  repositories = source_acquisition_fixture_repositories()
) {
  snapshot <- revdeprunner:::new_repository_snapshot(repositories, database)
  cohort <- revdeprunner:::new_reverse_dependency_cohort(
    "SubjectPkg",
    snapshot
  )
  universe <- revdeprunner:::new_dependency_universe(
    cohort,
    snapshot,
    "direct",
    c("base", "methods", "stats", "utils")
  )
  list(snapshot = snapshot, cohort = cohort, universe = universe)
}

make_source_acquisition_fixture <- function(request_packages = NULL) {
  root <- tempfile("source-acquisition-")
  paths <- file.path(
    root,
    c("package", "data", "runs", "inventories", "cache")
  )
  dir.create(root)
  invisible(lapply(paths, dir.create))
  writeLines(
    c("Package: fixture", "Version: 1.0.0"),
    file.path(paths[[1L]], "DESCRIPTION")
  )

  built <- "R 4.5.2; x86_64-pc-linux-gnu; 2026-08-29; unix"
  make_test_archive(
    paths[[5L]],
    "cran-bin/src/contrib",
    "HitPkg",
    "1.0",
    "no",
    built,
    "HitPkg_1.0_R_x86_64-pc-linux-gnu.tar.gz"
  )
  make_test_archive(
    paths[[5L]],
    "cran-bin/src/contrib",
    "FilePkg",
    "3.0",
    "no",
    built,
    "FilePkg_3.0_R_x86_64-pc-linux-gnu.tar.gz"
  )
  inventory_path <- revdeprunner:::write_cache_inventory(
    paths[[5L]],
    paths[[4L]],
    paths[[1L]]
  )$inventory_path
  lane <- revdeprunner:::new_compatibility_lane(
    "4.5",
    "x86_64-pc-linux-gnu",
    "x86_64",
    "linux-glibc-2.39",
    "gcc-15.2.1"
  )
  path_plan <- revdeprunner:::new_runtime_root_plan(
    paths[[1L]],
    paths[[2L]],
    paths[[3L]],
    "run-20260829-wp3d",
    paths[[5L]]
  )
  contracts <- source_acquisition_fixture_contracts()
  requirements <- revdeprunner:::derive_preparation_requirements(
    contracts$universe
  )
  requests <- revdeprunner:::preparation_required_packages(requirements)
  requests <- requests[!is.na(requests$version), , drop = FALSE]
  if (!is.null(request_packages)) {
    requests <- requests[requests$package %in% request_packages, , drop = FALSE]
  }
  rownames(requests) <- NULL
  bindings <- data.frame(
    inventory_path = inventory_path,
    lane_id = lane$lane_id,
    priority = 1L,
    stringsAsFactors = FALSE
  )
  binary_reuse <- revdeprunner:::reuse_inventory_binaries(
    requests,
    bindings,
    lane,
    path_plan
  )

  c(
    list(
      root = root,
      paths = paths,
      lane = lane,
      path_plan = path_plan,
      binary_reuse = binary_reuse
    ),
    contracts
  )
}

build_source_acquisition_plan <- function(fixture, contracts = fixture) {
  revdeprunner:::new_source_acquisition_plan(
    contracts$universe,
    contracts$cohort,
    contracts$snapshot,
    fixture$binary_reuse,
    fixture$lane,
    fixture$path_plan
  )
}

validate_source_acquisition_fixture_plan <- function(
  plan,
  fixture,
  contracts = fixture
) {
  revdeprunner:::validate_source_acquisition_plan(
    plan,
    contracts$universe,
    contracts$cohort,
    contracts$snapshot,
    fixture$binary_reuse,
    fixture$lane,
    fixture$path_plan
  )
}
# nolint end

test_that("source plans bind every available requirement and binary status", {
  fixture <- make_source_acquisition_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  before <- snapshot_test_cache(fixture$root) # nolint: object_usage_linter.

  plan <- build_source_acquisition_plan(fixture)

  expect_s3_class(plan, "revdeprunner_source_acquisition_plan")
  expect_identical(
    names(plan),
    c(
      "schema_version",
      "source_plan_id",
      "snapshot_id",
      "cohort_id",
      "universe_id",
      "lane_id",
      "path_plan_id",
      "binary_reuse_id",
      "requirements",
      "sources"
    )
  )
  expect_identical(
    plan$schema_version,
    "revdeprunner-source-acquisition-plan/v1"
  )
  expect_match(plan$source_plan_id, "^sha256:[a-f0-9]{64}$")
  expect_match(plan$binary_reuse_id, "^sha256:[a-f0-9]{64}$")
  expect_identical(
    plan$sources$package,
    c("BuildPkg", "FilePkg", "HitPkg")
  )
  expect_identical(
    plan$sources$binary_status,
    c("missing", "selected", "selected")
  )
  expect_identical(plan$sources$build_required, c("true", "false", "false"))
  unavailable <- plan$requirements[plan$requirements$package == "MissingPkg", ]
  expect_true(nrow(unavailable) >= 1L)
  expect_true(all(is.na(unavailable$version)))
  expect_true(all(unavailable$disposition == "unavailable"))
  expect_invisible(validate_source_acquisition_fixture_plan(plan, fixture))
  expect_identical(snapshot_test_cache(fixture$root), before) # nolint: object_usage_linter.
})

test_that("source plans retain frozen repository metadata and priority", {
  fixture <- make_source_acquisition_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)

  sources <- build_source_acquisition_plan(fixture)$sources
  build <- sources[sources$package == "BuildPkg", , drop = FALSE]
  custom <- sources[sources$package == "FilePkg", , drop = FALSE]

  expect_identical(build$version, "2.0")
  expect_identical(build$repository, "CRAN")
  expect_identical(
    build$source_url,
    "https://primary.example.test/src/contrib/BuildPkg_2.0.tar.gz"
  )
  expect_identical(build$expected_md5, strrep("c", 32L))
  expect_identical(build$needs_compilation, "yes")
  expect_identical(build$system_requirements, "libxml2 (>= 2.9)")
  expect_identical(
    custom$source_url,
    paste0(
      "https://primary.example.test/src/contrib/",
      "custom/FilePkg_3.0.tar.gz"
    )
  )
  expect_identical(custom$expected_md5, strrep("a", 32L))
  expect_true(is.na(custom$system_requirements))

  minimal_database <- source_acquisition_fixture_database()[,
    setdiff(
      names(source_acquisition_fixture_database()),
      c("File", "MD5sum", "NeedsCompilation", "SystemRequirements")
    ),
    drop = FALSE
  ]
  minimal <- build_source_acquisition_plan(
    fixture,
    source_acquisition_fixture_contracts(minimal_database)
  )$sources
  minimal_custom <- minimal[minimal$package == "FilePkg", , drop = FALSE]
  expect_identical(
    minimal_custom$source_url,
    "https://primary.example.test/src/contrib/FilePkg_3.0.tar.gz"
  )
  expect_true(is.na(minimal_custom$expected_md5))
  expect_identical(minimal_custom$needs_compilation, "unknown")
  expect_true(is.na(minimal_custom$system_requirements))
})

test_that("source plans require exact binary reuse coverage", {
  fixture <- make_source_acquisition_fixture(c("FilePkg", "HitPkg"))
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)

  expect_error(
    build_source_acquisition_plan(fixture),
    "cover every available requirement",
    fixed = TRUE
  )
})

test_that("source plans reject malformed frozen source metadata", {
  fixture <- make_source_acquisition_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  database <- source_acquisition_fixture_database()

  invalid_file <- database
  invalid_file$File[invalid_file$Package == "FilePkg"] <- "../escape.tar.gz"
  expect_error(
    build_source_acquisition_plan(
      fixture,
      source_acquisition_fixture_contracts(invalid_file)
    ),
    "safe relative URL path",
    fixed = TRUE
  )

  invalid_md5 <- database
  invalid_md5$MD5sum[invalid_md5$Package == "BuildPkg"] <- "not-an-md5"
  expect_error(
    build_source_acquisition_plan(
      fixture,
      source_acquisition_fixture_contracts(invalid_md5)
    ),
    "one MD5 checksum",
    fixed = TRUE
  )

  invalid_compilation <- database
  invalid_compilation$NeedsCompilation[
    invalid_compilation$Package == "BuildPkg"
  ] <- "sometimes"
  expect_error(
    build_source_acquisition_plan(
      fixture,
      source_acquisition_fixture_contracts(invalid_compilation)
    ),
    "must be `yes`, `no`, or unavailable",
    fixed = TRUE
  )

  relative_repositories <- c(CRAN = "relative", Secondary = "secondary")
  relative_database <- database
  relative_database$Repository <- ifelse(
    relative_database$Repository ==
      source_acquisition_fixture_repositories()[["CRAN"]],
    "relative/src/contrib",
    "secondary/src/contrib"
  )
  expect_error(
    build_source_acquisition_plan(
      fixture,
      source_acquisition_fixture_contracts(
        relative_database,
        relative_repositories
      )
    ),
    "absolute fragment-free URL",
    fixed = TRUE
  )
})

test_that("source plan identity is presentation- and locale-independent", {
  fixture <- make_source_acquisition_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  database <- source_acquisition_fixture_database()
  reordered <- database[
    rev(seq_len(nrow(database))),
    rev(names(database)),
    drop = FALSE
  ]
  reordered_contracts <- source_acquisition_fixture_contracts(reordered)
  expect_identical(reordered_contracts$snapshot, fixture$snapshot)
  expect_identical(reordered_contracts$cohort, fixture$cohort)
  expect_identical(reordered_contracts$universe, fixture$universe)

  original <- Sys.getlocale("LC_COLLATE")
  on.exit(
    suppressWarnings(Sys.setlocale("LC_COLLATE", original)),
    add = TRUE
  )
  available <- character()
  for (locale in unique(c("C", "C.UTF-8", original))) {
    selected <- suppressWarnings(Sys.setlocale("LC_COLLATE", locale))
    if (!is.na(selected)) {
      available <- c(available, locale)
    }
  }
  plans <- lapply(seq_along(available), function(index) {
    expect_false(is.na(Sys.setlocale("LC_COLLATE", available[[index]])))
    contracts <- if (index %% 2L == 0L) reordered_contracts else fixture
    build_source_acquisition_plan(fixture, contracts)
  })

  expect_true(length(plans) >= 1L)
  expect_true(all(vapply(plans, identical, logical(1L), plans[[1L]])))
})

test_that("source plan validation rejects context and content mutation", {
  fixture <- make_source_acquisition_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  plan <- build_source_acquisition_plan(fixture)

  changed <- plan
  changed$sources$build_required[
    changed$sources$package == "BuildPkg"
  ] <- "false"
  expect_error(
    validate_source_acquisition_fixture_plan(changed, fixture),
    "sources are not normalized",
    fixed = TRUE
  )

  changed <- plan
  changed$binary_reuse_id <- paste0("sha256:", strrep("0", 64L))
  expect_error(
    validate_source_acquisition_fixture_plan(changed, fixture),
    "does not match its binary reuse",
    fixed = TRUE
  )

  changed <- plan
  changed$source_plan_id <- paste0("sha256:", strrep("0", 64L))
  expect_error(
    validate_source_acquisition_fixture_plan(changed, fixture),
    "identity does not match its fields",
    fixed = TRUE
  )

  changed <- plan
  changed$snapshot_id <- paste0("sha256:", strrep("0", 64L))
  expect_error(
    validate_source_acquisition_fixture_plan(changed, fixture),
    "context bindings do not match",
    fixed = TRUE
  )

  changed <- plan
  changed$unexpected <- "field"
  expect_error(
    validate_source_acquisition_fixture_plan(changed, fixture),
    "invalid structure",
    fixed = TRUE
  )
})
