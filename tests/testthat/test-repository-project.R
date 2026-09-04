# These private tests protect exact repository projection before the stock
# adapter consumes the prepared binary archives.

repository_fixture_database <- function(
  stock_dependency = FALSE,
  pure_r_build = FALSE,
  hyphenated_build_version = FALSE
) {
  database <- source_acquisition_fixture_database()
  database$Imports[database$Package == "HitPkg"] <- "SubjectPkg"
  if (stock_dependency) {
    database$Suggests[
      database$Package == "BuildPkg" &
        database$Repository ==
          source_acquisition_fixture_repositories()[["CRAN"]]
    ] <- "HitPkg"
  }
  if (pure_r_build) {
    database$NeedsCompilation[
      database$Package == "BuildPkg" &
        database$Repository ==
          source_acquisition_fixture_repositories()[["CRAN"]]
    ] <- "no"
  }
  if (hyphenated_build_version) {
    database$Version[
      database$Package == "BuildPkg" &
        database$Repository ==
          source_acquisition_fixture_repositories()[["CRAN"]]
    ] <- "2.0-1"
  }
  database
}

make_repository_preparation_fixture <- function(
  stock_dependency = FALSE,
  pure_r_build = FALSE,
  hyphenated_build_version = FALSE
) {
  fixture <- make_source_preparation_fixture(
    missing_binary_packages = c("BuildPkg", "FilePkg", "HitPkg"),
    database = repository_fixture_database(
      stock_dependency,
      pure_r_build,
      hyphenated_build_version
    ),
    build_imports = "SubjectPkg"
  )
  fixture$gate <- do.call(
    revdeprunner:::prepare_dependency_universe,
    c(
      source_preparation_context(fixture),
      list(
        baseline_source = fixture$baseline_source,
        timeout_seconds = 60L
      )
    )
  )
  fixture
}

if (!identical(unname(Sys.info()[["sysname"]]), "Linux")) {
  test_that("repository projection reports its Linux boundary", {
    expect_error(
      revdeprunner:::require_linux_repository_projection(),
      "supported only on Linux",
      fixed = TRUE
    )
  })
} else {
  test_that("an exact repository reuses the preparation report", {
    fixture <- make_repository_preparation_fixture()
    on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
    context <- source_preparation_context(fixture)
    warehouse_before <- source_preparation_warehouse_snapshot(fixture)
    source_admissions <- 0L
    projection_admissions <- 0L
    real_source_admission <-
      revdeprunner:::validate_existing_warehouse_artifact
    real_projection_admission <- revdeprunner:::validate_warehouse_archive
    projections <- testthat::with_mocked_bindings(
      {
        first <- revdeprunner:::project_preparation_repository(
          fixture$gate,
          context
        )
        after_first <- c(
          source = source_admissions,
          projection = projection_admissions
        )
        second <- revdeprunner:::project_preparation_repository(
          fixture$gate,
          context
        )
        list(first = first, second = second, after_first = after_first)
      },
      # Count projection work, not validation of the already-prepared gate.
      validate_preparation_gate = function(...) invisible(NULL),
      validate_existing_warehouse_artifact = function(
        path,
        artifact,
        warehouse_root
      ) {
        source_admissions <<- source_admissions + 1L
        real_source_admission(path, artifact, warehouse_root)
      },
      validate_warehouse_archive = function(
        path,
        artifact,
        archive_name = basename(path)
      ) {
        projection_admissions <<- projection_admissions + 1L
        real_projection_admission(path, artifact, archive_name)
      },
      .package = "revdeprunner"
    )
    first <- projections$first
    second <- projections$second
    prepared <- revdeprunner:::prepare_repository_universe(
      fixture$gate,
      context
    )

    expect_false(first$reused)
    expect_true(second$reused)
    expect_true(prepared$projection$reused)
    expect_identical(
      projections$after_first,
      c(source = 3L, projection = 3L)
    )
    expect_identical(source_admissions, 3L)
    expect_identical(projection_admissions, 6L)
    expect_identical(first$manifest, second$manifest)
    expect_identical(
      first$manifest$package,
      c("BuildPkg", "FilePkg", "HitPkg")
    )
    expect_true(all(grepl("_R_", first$manifest$archive_name, fixed = TRUE)))
    expect_true(dir.exists(first$repository_path))
    expect_true(file.exists(file.path(
      first$repository_path,
      "src",
      "contrib",
      "PACKAGES"
    )))
    expect_false(any(file.exists(file.path(
      first$repository_path,
      "src",
      "contrib",
      c("PACKAGES.gz", "PACKAGES.rds")
    ))))
    expect_identical(
      names(prepared),
      c("prepared_gate", "projection", "report", "execution_order")
    )
    expect_identical(prepared$report, fixture$gate$report)
    expect_identical(prepared$report$results$outcome, rep("prepared", 3L))
    expect_false(dir.exists(file.path(
      fixture$paths[[3L]],
      fixture$path_plan$run_id,
      "repository-verification"
    )))
    expect_identical(
      source_preparation_warehouse_snapshot(fixture),
      warehouse_before
    )
    expect_invisible(
      revdeprunner:::validate_repository_preparation(prepared, context)
    )

    wrong_binding <- prepared
    wrong_binding$report$universe_id <- paste0(
      "sha256:",
      strrep("0", 64L)
    )
    expect_error(
      revdeprunner:::validate_repository_preparation(
        wrong_binding,
        context
      ),
      "does not match its preparation context",
      fixed = TRUE
    )

    escaped_projection <- prepared
    escaped_projection$projection$repository_path <- fixture$root
    expect_error(
      revdeprunner:::validate_repository_preparation(
        escaped_projection,
        context
      ),
      "projection path is inconsistent",
      fixed = TRUE
    )
  })

  test_that("repository projection preserves literal hyphenated versions", {
    fixture <- make_repository_preparation_fixture(
      hyphenated_build_version = TRUE
    )
    on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
    context <- source_preparation_context(fixture)

    prepared <- revdeprunner:::prepare_repository_universe(
      fixture$gate,
      context
    )

    build <- prepared$report$results[
      prepared$report$results$package == "BuildPkg",
      ,
      drop = FALSE
    ]
    manifest <- prepared$projection$manifest[
      prepared$projection$manifest$package == "BuildPkg",
      ,
      drop = FALSE
    ]
    packages <- read.dcf(
      file.path(
        prepared$projection$repository_path,
        "src",
        "contrib",
        "PACKAGES"
      ),
      fields = c("Package", "Version", "File")
    )
    expect_identical(build$version, "2.0-1")
    expect_identical(build$outcome, "prepared")
    expect_identical(manifest$version, "2.0-1")
    row <- which(packages[, "Package"] == "BuildPkg")
    expect_identical(
      unname(packages[row, ]),
      c(
        "BuildPkg",
        "2.0-1",
        manifest$archive_name
      )
    )
    expect_invisible(
      revdeprunner:::validate_repository_preparation(prepared, context)
    )
  })

  test_that("incomplete preparation stops before repository projection", {
    fixture <- make_source_preparation_fixture()
    on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
    gate <- do.call(
      revdeprunner:::prepare_dependency_universe,
      c(
        source_preparation_context(fixture),
        list(
          baseline_source = fixture$baseline_source,
          timeout_seconds = 60L
        )
      )
    )
    context <- source_preparation_context(fixture)

    expect_error(
      revdeprunner:::prepare_repository_universe(
        gate,
        context
      ),
      "every preparation result to be prepared",
      fixed = TRUE
    )
    expect_false(dir.exists(file.path(fixture$paths[[2L]], "repositories")))
  })

  test_that("invalid or stale repository metadata is never overwritten", {
    fixture <- make_repository_preparation_fixture()
    on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
    context <- source_preparation_context(fixture)
    expected_path <- file.path(
      fixture$paths[[2L]],
      "repositories",
      sub("^sha256:", "", fixture$gate$report$report_id)
    )
    staging <- file.path(fixture$paths[[2L]], "repositories", ".staging")
    real_write_packages <- revdeprunner:::repository_write_packages

    expect_error(
      testthat::with_mocked_bindings(
        revdeprunner:::project_preparation_repository(fixture$gate, context),
        repository_write_packages = function(contrib_path) {
          real_write_packages(contrib_path)
          archive <- list.files(
            contrib_path,
            pattern = "[.]tar[.]gz$",
            full.names = TRUE
          )[[1L]]
          connection <- file(archive, open = "ab")
          on.exit(close(connection), add = TRUE)
          writeBin(charToRaw("corrupt"), connection)
          invisible(NULL)
        },
        .package = "revdeprunner"
      ),
      "payload does not match its SHA-256 identity",
      fixed = TRUE
    )
    expect_false(file.exists(expected_path))
    expect_length(list.files(staging, all.files = TRUE, no.. = TRUE), 0L)

    expect_error(
      testthat::with_mocked_bindings(
        revdeprunner:::project_preparation_repository(fixture$gate, context),
        repository_write_packages = function(contrib_path) {
          writeLines(
            c("Package: WrongPkg", "Version: 9.9", "File: wrong.tar.gz"),
            file.path(contrib_path, "PACKAGES")
          )
        },
        .package = "revdeprunner"
      ),
      "PACKAGES metadata",
      fixed = TRUE
    )
    expect_false(file.exists(expected_path))
    expect_length(list.files(staging, all.files = TRUE, no.. = TRUE), 0L)
    projection <- revdeprunner:::project_preparation_repository(
      fixture$gate,
      context
    )
    alternate_index <- file.path(
      projection$repository_path,
      "src",
      "contrib",
      "PACKAGES.rds"
    )
    saveRDS(matrix(c("WrongPkg", "9.9"), nrow = 1L), alternate_index)
    expect_error(
      revdeprunner:::project_preparation_repository(fixture$gate, context),
      "unexpected entries",
      fixed = TRUE
    )
    unlink(alternate_index)
    writeLines(
      c("Package: WrongPkg", "Version: 9.9", "File: wrong.tar.gz"),
      file.path(projection$repository_path, "src", "contrib", "PACKAGES")
    )
    expect_error(
      revdeprunner:::project_preparation_repository(fixture$gate, context),
      "PACKAGES metadata",
      fixed = TRUE
    )
  })
}
