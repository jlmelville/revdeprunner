# These private fixtures exercise one real Linux source-to-binary preparation
# path while keeping repositories, libraries, logs, and caches local.

source_preparation_runner_lane <- function() {
  architecture <- R.version$arch
  if (is.null(architecture) || !nzchar(architecture)) {
    architecture <- sub("-.*$", "", R.version$platform)
  }
  os_abi <- R.version$os
  if (is.null(os_abi) || !nzchar(os_abi)) {
    os_abi <- tolower(Sys.info()[["sysname"]])
  }
  revdeprunner:::new_compatibility_lane(
    sub("^([0-9]+[.][0-9]+).*$", "\\1", as.character(getRversion())),
    R.version$platform,
    architecture,
    os_abi,
    "fixture-toolchain"
  )
}

make_installable_source_archive <- function(
  repository_root,
  package = "BuildPkg",
  version = "2.0",
  needs_compilation = "yes",
  relative_directory = "",
  imports = NA_character_,
  suggests = NA_character_
) {
  staging_root <- tempfile("source-preparation-package-")
  dir.create(staging_root)
  on.exit(unlink(staging_root, recursive = TRUE), add = TRUE)
  package_root <- file.path(staging_root, package)
  dir.create(package_root)
  dir.create(file.path(package_root, "R"))
  if (identical(needs_compilation, "yes")) {
    dir.create(file.path(package_root, "src"))
  }
  description <- c(
    paste0("Package: ", package),
    "Type: Package",
    "Title: Source Preparation Fixture",
    paste0("Version: ", version),
    paste0(
      "Authors@R: person('Fixture', 'Author', role = c('aut', 'cre'), ",
      "email = 'fixture@example.test')"
    ),
    "Description: An installable fixture for source preparation tests.",
    "License: MIT",
    "Encoding: UTF-8",
    paste0("NeedsCompilation: ", needs_compilation)
  )
  if (!is.na(suggests)) {
    description <- c(description, paste0("Suggests: ", suggests))
  }
  if (!is.na(imports)) {
    description <- c(description, paste0("Imports: ", imports))
  }
  writeLines(description, file.path(package_root, "DESCRIPTION"))
  namespace <- "export(build_value)"
  if (identical(needs_compilation, "yes")) {
    namespace <- c(paste0("useDynLib(", package, ")"), namespace)
  }
  writeLines(namespace, file.path(package_root, "NAMESPACE"))
  writeLines(
    "build_value <- function() 42L",
    file.path(package_root, "R", "build.R")
  )
  if (identical(needs_compilation, "yes")) {
    writeLines(
      "void buildpkg_noop(void) {}",
      file.path(package_root, "src", "build.c")
    )
  }

  archive_root <- file.path(
    repository_root,
    "src",
    "contrib",
    relative_directory
  )
  dir.create(archive_root, recursive = TRUE, showWarnings = FALSE)
  archive <- file.path(
    archive_root,
    paste0(package, "_", version, ".tar.gz")
  )
  old_working_directory <- setwd(staging_root)
  on.exit(setwd(old_working_directory), add = TRUE)
  utils::tar(
    archive,
    files = package,
    compression = "gzip",
    tar = "internal"
  )
  normalizePath(archive, winslash = "/", mustWork = TRUE)
}

make_source_preparation_fixture <- function(
  missing_binary_packages = character(),
  database = source_acquisition_fixture_database(),
  build_imports = NA_character_
) {
  fixture <- make_source_acquisition_fixture(
    run_id = "run-20260829-wp3f",
    missing_binary_packages = missing_binary_packages,
    lane = source_preparation_runner_lane(),
    database = database
  )
  repository_root <- file.path(fixture$root, "repository")
  secondary_root <- file.path(fixture$root, "secondary-repository")
  dir.create(repository_root)
  dir.create(secondary_root)
  selected <- database[!duplicated(database$Package), , drop = FALSE]
  build_row <- selected[selected$Package == "BuildPkg", , drop = FALSE]
  file_row <- selected[selected$Package == "FilePkg", , drop = FALSE]
  hit_row <- selected[selected$Package == "HitPkg", , drop = FALSE]
  source_archives <- list(
    BuildPkg = make_installable_source_archive(
      repository_root,
      version = build_row$Version[[1L]],
      needs_compilation = build_row$NeedsCompilation[[1L]],
      imports = build_imports,
      suggests = build_row$Suggests[[1L]]
    ),
    FilePkg = make_installable_source_archive(
      repository_root,
      package = "FilePkg",
      version = file_row$Version[[1L]],
      needs_compilation = "no",
      relative_directory = "custom"
    ),
    HitPkg = make_installable_source_archive(
      repository_root,
      package = "HitPkg",
      version = hit_row$Version[[1L]],
      needs_compilation = "no"
    )
  )
  subject <- selected[selected$Package == "SubjectPkg", , drop = FALSE]
  source_archives$SubjectPkg <- make_installable_source_archive(
    repository_root,
    package = "SubjectPkg",
    version = subject$Version[[1L]],
    needs_compilation = subject$NeedsCompilation[[1L]]
  )

  primary <- paste0(
    "file://",
    normalizePath(
      file.path(repository_root, "src", "contrib"),
      winslash = "/",
      mustWork = TRUE
    )
  )
  secondary <- paste0(
    "file://",
    normalizePath(secondary_root, winslash = "/", mustWork = TRUE)
  )
  repositories <- c(CRAN = primary, Secondary = secondary)
  original <- source_acquisition_fixture_repositories()
  database$Repository[database$Repository == original[["CRAN"]]] <- primary
  database$Repository[database$Repository == original[["Secondary"]]] <-
    secondary
  for (package in names(source_archives)) {
    database$MD5sum[database$Package == package] <- digest::digest(
      source_archives[[package]],
      algo = "md5",
      file = TRUE,
      serialize = FALSE
    )
  }
  contracts <- source_acquisition_fixture_contracts(database, repositories)
  source_plan <- build_source_acquisition_plan(fixture, contracts)
  r_executable <- revdeprunner:::normalize_r_executable(
    file.path(R.home("bin"), "R")
  )

  fixture$download_contracts <- contracts
  fixture$source_plan <- source_plan
  fixture$r_executable <- r_executable
  fixture$repository_root <- repository_root
  fixture$source_archive <- source_archives$BuildPkg
  fixture$source_archives <- source_archives
  fixture$baseline_source <- source_archives$SubjectPkg
  fixture
}

source_preparation_context <- function(fixture) {
  contracts <- fixture$download_contracts
  list(
    source_plan = fixture$source_plan,
    universe = contracts$universe,
    cohort = contracts$cohort,
    snapshot = contracts$snapshot,
    binary_reuse = fixture$binary_reuse,
    lane = fixture$lane,
    path_plan = fixture$path_plan,
    r_executable = fixture$r_executable
  )
}

prepare_fixture_source_binary <- function(
  fixture,
  source_acquisition = NULL,
  previous = NULL,
  timeout_seconds = 60L
) {
  context <- source_preparation_context(fixture)
  if (is.null(source_acquisition)) {
    source_acquisition <- if (is.null(previous)) {
      acquire_fixture_build_source(fixture)
    } else {
      previous$source_acquisition
    }
  }
  revdeprunner:::prepare_source_binary_in_context(
    "BuildPkg",
    context,
    source_acquisition,
    previous,
    timeout_seconds
  )
}

acquire_fixture_build_source <- function(fixture) {
  revdeprunner:::acquire_source_artifact_in_context(
    "BuildPkg",
    fixture$source_plan,
    fixture$path_plan
  )
}

source_preparation_run_root <- function(fixture) {
  file.path(fixture$paths[[3L]], fixture$path_plan$run_id)
}

source_preparation_source_cache_snapshot <- function(fixture) {
  cache <- file.path(fixture$paths[[2L]], "source-cache")
  if (!dir.exists(cache)) {
    return(data.frame())
  }
  snapshot_test_cache(cache)
}

mock_source_preparation_process <- function(
  message,
  status,
  timed_out = FALSE,
  create_binary = NULL
) {
  force(message)
  force(status)
  force(timed_out)
  force(create_binary)
  function(
    r_executable,
    arguments,
    working_directory,
    stdout_path,
    stderr_path,
    timeout_seconds
  ) {
    writeLines("fixture stdout", stdout_path)
    writeLines(message, stderr_path)
    if (!is.null(create_binary)) {
      create_binary(working_directory)
    }
    list(
      command = revdeprunner:::render_source_preparation_command(
        r_executable,
        arguments
      ),
      started_at = "2026-08-29T12:00:00.000000Z",
      duration_ms = 10,
      status = as.integer(status),
      timed_out = timed_out,
      warnings = if (timed_out) "command timed out" else character()
    )
  }
}
