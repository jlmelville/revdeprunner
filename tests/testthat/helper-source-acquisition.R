# These private fixtures compose the accepted dependency, binary-reuse, path,
# and source-plan contracts for WP3 source preparation tests.

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

make_source_acquisition_fixture <- function(
  request_packages = NULL,
  run_id = "run-20260829-wp3d",
  missing_binary_packages = character(),
  lane = NULL,
  database = source_acquisition_fixture_database()
) {
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

  if (is.null(lane)) {
    lane <- revdeprunner:::new_compatibility_lane(
      "4.5",
      "x86_64-pc-linux-gnu",
      "x86_64",
      "linux-glibc-2.39",
      "gcc-15.2.1"
    )
    built_version <- "4.5.2"
  } else {
    revdeprunner:::validate_compatibility_lane(lane)
    built_version <- paste0(lane$r_major_minor, ".0")
  }
  built <- paste(
    paste("R", built_version),
    lane$r_platform,
    "2026-08-29",
    "unix",
    sep = "; "
  )
  if (!"HitPkg" %in% missing_binary_packages) {
    make_test_archive(
      paths[[5L]],
      "cran-bin/src/contrib",
      "HitPkg",
      "1.0",
      "no",
      built,
      paste0("HitPkg_1.0_R_", lane$r_platform, ".tar.gz")
    )
  }
  if (!"FilePkg" %in% missing_binary_packages) {
    make_test_archive(
      paths[[5L]],
      "cran-bin/src/contrib",
      "FilePkg",
      "3.0",
      "no",
      built,
      paste0("FilePkg_3.0_R_", lane$r_platform, ".tar.gz")
    )
  }
  inventory_path <- revdeprunner:::write_cache_inventory(
    paths[[5L]],
    paths[[4L]],
    paths[[1L]]
  )$inventory_path
  path_plan <- revdeprunner:::new_runtime_root_plan(
    paths[[1L]],
    paths[[2L]],
    paths[[3L]],
    run_id,
    paths[[5L]]
  )
  contracts <- source_acquisition_fixture_contracts(database)
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
