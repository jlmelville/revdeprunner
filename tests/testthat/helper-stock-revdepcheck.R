# Local stock-runner fixtures shared by behavior and validation-cost tests.

stock_fixture_database <- function() {
  database <- source_acquisition_fixture_database()
  primary <- source_acquisition_fixture_repositories()[["CRAN"]]
  database$Version[
    database$Package == "BuildPkg" & database$Repository == primary
  ] <- "2.0-1"
  database$Imports[database$Package == "HitPkg"] <- "SubjectPkg"
  database$Suggests[
    database$Package == "BuildPkg" & database$Repository == primary
  ] <- "HitPkg"
  database$NeedsCompilation[
    database$Package == "BuildPkg" & database$Repository == primary
  ] <- "no"
  database
}

make_stock_repository_fixture <- function() {
  fixture <- make_source_preparation_fixture(
    missing_binary_packages = c("BuildPkg", "FilePkg", "HitPkg"),
    database = stock_fixture_database(),
    build_imports = "SubjectPkg"
  )
  source_cache <- file.path(fixture$root, "source-cache")
  dir.create(source_cache)
  fixture$path_plan <- revdeprunner:::new_runtime_root_plan(
    fixture$paths[[1L]],
    fixture$paths[[2L]],
    fixture$paths[[3L]],
    "run-20260829-wp3f",
    c(fixture$paths[[4L]], source_cache)
  )
  requests <- revdeprunner:::preparation_required_packages(
    revdeprunner:::derive_preparation_requirements(
      fixture$download_contracts$universe
    )
  )
  requests <- requests[!is.na(requests$version), , drop = FALSE]
  initial_observations <- revdeprunner:::observe_cache_roots(
    c(fixture$paths[[4L]], source_cache),
    requests
  )
  fixture$binary_reuse <- revdeprunner:::reuse_cached_binaries(
    requests,
    initial_observations,
    fixture$lane,
    fixture$path_plan
  )
  fixture$source_plan <- revdeprunner:::new_source_acquisition_plan(
    fixture$download_contracts$universe,
    fixture$download_contracts$cohort,
    fixture$download_contracts$snapshot,
    fixture$binary_reuse,
    fixture$lane,
    fixture$path_plan
  )
  context <- source_preparation_context(fixture)
  bootstrap <- do.call(
    revdeprunner:::prepare_dependency_universe,
    c(
      context,
      list(
        baseline_source = fixture$baseline_source,
        timeout_seconds = 60L
      )
    )
  )
  cache_binary <- file.path(
    fixture$paths[[4L]],
    "cran-bin",
    "src",
    "contrib",
    basename(bootstrap$source_preparations$HitPkg$binary_path)
  )
  dir.create(dirname(cache_binary), recursive = TRUE)
  if (
    !file.copy(
      bootstrap$source_preparations$HitPkg$binary_path,
      cache_binary,
      overwrite = TRUE
    )
  ) {
    stop("Unable to seed the stock fixture binary cache.", call. = FALSE)
  }
  cache_file_binary <- file.path(
    source_cache,
    "cran-bin",
    "src",
    "contrib",
    basename(bootstrap$source_preparations$FilePkg$binary_path)
  )
  dir.create(dirname(cache_file_binary), recursive = TRUE)
  if (
    !file.copy(
      bootstrap$source_preparations$FilePkg$binary_path,
      cache_file_binary,
      overwrite = TRUE
    )
  ) {
    stop("Unable to seed the second stock fixture binary cache.", call. = FALSE)
  }
  cache_source <- file.path(
    source_cache,
    "cran",
    "src",
    "contrib",
    basename(fixture$source_archives$HitPkg)
  )
  dir.create(dirname(cache_source), recursive = TRUE)
  if (!file.copy(fixture$source_archives$HitPkg, cache_source)) {
    stop("Unable to seed the stock fixture source cache.", call. = FALSE)
  }
  observations <- revdeprunner:::observe_cache_roots(
    c(fixture$paths[[4L]], source_cache),
    requests
  )
  fixture$binary_reuse <- revdeprunner:::reuse_cached_binaries(
    requests,
    observations,
    fixture$lane,
    fixture$path_plan
  )
  fixture$source_plan <- revdeprunner:::new_source_acquisition_plan(
    context$universe,
    context$cohort,
    context$snapshot,
    fixture$binary_reuse,
    fixture$lane,
    fixture$path_plan
  )
  context <- source_preparation_context(fixture)
  fixture$gate <- do.call(
    revdeprunner:::prepare_dependency_universe,
    c(
      context,
      list(
        baseline_source = fixture$baseline_source,
        timeout_seconds = 60L
      )
    )
  )
  write_stock_candidate(context$path_plan$package_root)
  fixture$baseline <- fixture$source_archives$SubjectPkg
  fixture$stock_cached_hit_source <- cache_source
  fixture
}

write_stock_candidate <- function(path) {
  dir.create(file.path(path, "R"), showWarnings = FALSE)
  writeLines(
    c(
      "Package: SubjectPkg",
      "Type: Package",
      "Title: Stock Adapter Subject Fixture",
      "Version: 0.2",
      paste0(
        "Authors@R: person('Fixture', 'Author', role = c('aut', 'cre'), ",
        "email = 'fixture@example.test')"
      ),
      "Description: A pure-R package-under-test fixture.",
      "License: MIT",
      "Encoding: UTF-8",
      "Imports: HitPkg",
      "NeedsCompilation: no"
    ),
    file.path(path, "DESCRIPTION")
  )
  writeLines("export(subject_value)", file.path(path, "NAMESPACE"))
  writeLines(
    "subject_value <- function() 42L",
    file.path(path, "R", "subject.R")
  )
}
