# These private tests protect the first mutating warehouse boundary. Later WP3
# chunks will consume it without making artifact selection or preparation part
# of this primitive.

make_warehouse_fixture <- function() {
  root <- tempfile("warehouse-promotion-")
  dir.create(root)
  paths <- file.path(root, c("package", "data", "runs", "cache", "outside"))
  invisible(lapply(paths, dir.create))
  writeLines(
    c("Package: fixture", "Version: 1.0.0"),
    file.path(paths[[1L]], "DESCRIPTION")
  )
  source_path <- make_test_archive(
    paths[[4L]],
    repository = "cran/src/contrib",
    package = "promoted",
    version = "1.2.3",
    needs_compilation = "no"
  )
  path_plan <- revdeprunner:::new_runtime_root_plan(
    package_root = paths[[1L]],
    data_root = paths[[2L]],
    runs_root = paths[[3L]],
    run_id = "run-20260829-wp3a",
    source_cache_roots = paths[[4L]]
  )
  sha256 <- digest::digest(
    source_path,
    algo = "sha256",
    file = TRUE,
    serialize = FALSE
  )
  artifact <- revdeprunner:::new_artifact_identity(
    package = "promoted",
    version = "1.2.3",
    sha256 = sha256,
    archive_type = "source"
  )

  list(
    root = root,
    package = paths[[1L]],
    data = paths[[2L]],
    runs = paths[[3L]],
    cache = paths[[4L]],
    outside = paths[[5L]],
    source = normalizePath(source_path, winslash = "/", mustWork = TRUE),
    path_plan = path_plan,
    artifact = artifact
  )
}

warehouse_fixture_destination <- function(
  fixture,
  artifact = fixture$artifact
) {
  digest <- sub("^sha256:", "", artifact$artifact_id)
  file.path(
    fixture$data,
    "warehouse",
    "artifacts",
    "sha256",
    substr(digest, 1L, 2L),
    digest
  )
}

warehouse_staging_files <- function(fixture) {
  staging <- file.path(fixture$data, "warehouse", ".staging")
  if (!dir.exists(staging)) {
    return(character())
  }
  list.files(staging, all.files = TRUE, no.. = TRUE, full.names = TRUE)
}

test_that("warehouse promotion copies, publishes, and reuses exact artifacts", {
  fixture <- make_warehouse_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  source_before <- snapshot_test_cache(fixture$cache)

  first <- revdeprunner:::promote_warehouse_artifact(
    fixture$source,
    fixture$artifact,
    fixture$path_plan
  )
  first_payload <- readBin(
    first$warehouse_path,
    what = "raw",
    n = file.info(first$warehouse_path)$size
  )
  second <- revdeprunner:::promote_warehouse_artifact(
    fixture$source,
    fixture$artifact,
    fixture$path_plan
  )

  expect_s3_class(first, "revdeprunner_warehouse_promotion")
  expect_identical(
    names(first),
    c(
      "artifact_id",
      "source_path",
      "warehouse_path",
      "transfer_policy",
      "reused"
    )
  )
  expect_identical(first$artifact_id, fixture$artifact$artifact_id)
  expect_identical(first$source_path, fixture$source)
  expect_identical(first$warehouse_path, warehouse_fixture_destination(fixture))
  expect_identical(first$transfer_policy, "copy")
  expect_false(first$reused)
  expect_true(second$reused)
  expect_identical(second$warehouse_path, first$warehouse_path)
  expect_identical(
    digest::digest(
      first$warehouse_path,
      algo = "sha256",
      file = TRUE,
      serialize = FALSE
    ),
    fixture$artifact$sha256
  )
  expect_identical(
    readBin(
      second$warehouse_path,
      what = "raw",
      n = file.info(second$warehouse_path)$size
    ),
    first_payload
  )
  expect_identical(snapshot_test_cache(fixture$cache), source_before)
  expect_length(warehouse_staging_files(fixture), 0L)
})

test_that("warehouse promotion accepts an exact per-run build output", {
  fixture <- make_warehouse_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  run_root <- file.path(fixture$runs, fixture$path_plan$run_id)
  build_root <- file.path(run_root, "build")
  dir.create(build_root, recursive = TRUE)
  run_source <- file.path(build_root, basename(fixture$source))
  expect_true(file.copy(fixture$source, run_source))
  run_source <- normalizePath(run_source, winslash = "/", mustWork = TRUE)
  run_before <- snapshot_test_cache(run_root)

  promotion <- revdeprunner:::promote_warehouse_artifact(
    run_source,
    fixture$artifact,
    fixture$path_plan
  )

  expect_false(promotion$reused)
  expect_identical(promotion$source_path, run_source)
  expect_identical(snapshot_test_cache(run_root), run_before)
})

test_that("warehouse promotion validates policies, identities, and boundaries", {
  fixture <- make_warehouse_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  source_before <- snapshot_test_cache(fixture$cache)

  expect_error(
    revdeprunner:::promote_warehouse_artifact(
      fixture$source,
      fixture$artifact,
      fixture$path_plan,
      transfer_policy = "hard-link"
    ),
    "exactly `copy`",
    fixed = TRUE
  )
  expect_error(
    revdeprunner:::promote_warehouse_artifact(
      basename(fixture$source),
      fixture$artifact,
      fixture$path_plan
    ),
    "absolute forward-slash path",
    fixed = TRUE
  )
  expect_error(
    revdeprunner:::promote_warehouse_artifact(
      fixture$source,
      fixture$artifact,
      fixture$path_plan,
      archive_name = "../promoted_1.2.3.tar.gz"
    ),
    "one archive basename",
    fixed = TRUE
  )

  outside_source <- file.path(fixture$outside, basename(fixture$source))
  expect_true(file.copy(fixture$source, outside_source))
  outside_source <- normalizePath(
    outside_source,
    winslash = "/",
    mustWork = TRUE
  )
  expect_error(
    revdeprunner:::promote_warehouse_artifact(
      outside_source,
      fixture$artifact,
      fixture$path_plan
    ),
    "within a cache or run root",
    fixed = TRUE
  )

  wrong_hash <- revdeprunner:::new_artifact_identity(
    "promoted",
    "1.2.3",
    paste(rep("a", 64L), collapse = ""),
    "source"
  )
  expect_error(
    revdeprunner:::promote_warehouse_artifact(
      fixture$source,
      wrong_hash,
      fixture$path_plan
    ),
    "SHA-256 identity",
    fixed = TRUE
  )
  wrong_package <- revdeprunner:::new_artifact_identity(
    "another",
    "1.2.3",
    fixture$artifact$sha256,
    "source"
  )
  expect_error(
    revdeprunner:::promote_warehouse_artifact(
      fixture$source,
      wrong_package,
      fixture$path_plan
    ),
    "metadata does not match",
    fixed = TRUE
  )

  expect_identical(snapshot_test_cache(fixture$cache), source_before)
  expect_false(file.exists(warehouse_fixture_destination(fixture)))
})

test_that("warehouse promotion refuses linked source and warehouse paths", {
  fixture <- make_warehouse_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  source_before <- snapshot_test_cache(fixture$cache)

  linked_source <- file.path(fixture$cache, "linked.tar.gz")
  source_linked <- suppressWarnings(file.symlink(fixture$source, linked_source))
  if (isTRUE(source_linked)) {
    expect_error(
      revdeprunner:::promote_warehouse_artifact(
        linked_source,
        fixture$artifact,
        fixture$path_plan
      ),
      "source must not be a symbolic link",
      fixed = TRUE
    )
  } else {
    succeed()
  }
  unlink(linked_source)

  warehouse <- file.path(fixture$data, "warehouse")
  warehouse_linked <- suppressWarnings(file.symlink(fixture$outside, warehouse))
  if (isTRUE(warehouse_linked)) {
    expect_error(
      revdeprunner:::promote_warehouse_artifact(
        fixture$source,
        fixture$artifact,
        fixture$path_plan
      ),
      "must not traverse a symbolic link",
      fixed = TRUE
    )
  } else {
    succeed()
  }
  expect_length(list.files(fixture$outside, all.files = TRUE, no.. = TRUE), 0L)
  expect_identical(snapshot_test_cache(fixture$cache), source_before)
})

test_that("staged validation failures clean up without publication", {
  fixture <- make_warehouse_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  source_before <- snapshot_test_cache(fixture$cache)
  real_file_copy <- base::file.copy
  testthat::local_mocked_bindings(
    warehouse_copy_file = function(from, to, ...) {
      copied <- real_file_copy(from, to, ...)
      if (isTRUE(copied)) {
        connection <- file(to, open = "ab")
        on.exit(close(connection), add = TRUE)
        writeBin(charToRaw("corrupt"), connection)
      }
      copied
    },
    .package = "revdeprunner"
  )

  expect_error(
    revdeprunner:::promote_warehouse_artifact(
      fixture$source,
      fixture$artifact,
      fixture$path_plan
    ),
    "SHA-256 identity",
    fixed = TRUE
  )
  expect_false(file.exists(warehouse_fixture_destination(fixture)))
  expect_length(warehouse_staging_files(fixture), 0L)
  expect_identical(snapshot_test_cache(fixture$cache), source_before)
})

test_that("atomic publication failure leaves no visible artifact", {
  fixture <- make_warehouse_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  source_before <- snapshot_test_cache(fixture$cache)
  testthat::local_mocked_bindings(
    warehouse_publish_link = function(from, to) FALSE,
    .package = "revdeprunner"
  )

  expect_error(
    revdeprunner:::promote_warehouse_artifact(
      fixture$source,
      fixture$artifact,
      fixture$path_plan
    ),
    "publish the staged artifact atomically",
    fixed = TRUE
  )
  expect_false(file.exists(warehouse_fixture_destination(fixture)))
  expect_length(warehouse_staging_files(fixture), 0L)
  expect_identical(snapshot_test_cache(fixture$cache), source_before)
})

test_that("existing warehouse collisions fail without overwrite", {
  fixture <- make_warehouse_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  source_before <- snapshot_test_cache(fixture$cache)
  promotion <- revdeprunner:::promote_warehouse_artifact(
    fixture$source,
    fixture$artifact,
    fixture$path_plan
  )
  corrupt <- charToRaw("different payload")
  writeBin(corrupt, promotion$warehouse_path)

  expect_error(
    revdeprunner:::promote_warehouse_artifact(
      fixture$source,
      fixture$artifact,
      fixture$path_plan
    ),
    "does not match its identity",
    fixed = TRUE
  )
  expect_identical(
    readBin(
      promotion$warehouse_path,
      what = "raw",
      n = file.info(promotion$warehouse_path)$size
    ),
    corrupt
  )
  expect_identical(snapshot_test_cache(fixture$cache), source_before)
  expect_length(warehouse_staging_files(fixture), 0L)
})

test_that("warehouse identity paths refuse symbolic-link occupants", {
  fixture <- make_warehouse_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  source_before <- snapshot_test_cache(fixture$cache)
  promotion <- revdeprunner:::promote_warehouse_artifact(
    fixture$source,
    fixture$artifact,
    fixture$path_plan
  )
  unlink(promotion$warehouse_path)
  target <- file.path(fixture$outside, "target")
  writeLines("outside", target)
  target_before <- readBin(target, what = "raw", n = file.info(target)$size)
  linked <- suppressWarnings(file.symlink(target, promotion$warehouse_path))
  if (isTRUE(linked)) {
    expect_error(
      revdeprunner:::promote_warehouse_artifact(
        fixture$source,
        fixture$artifact,
        fixture$path_plan
      ),
      "must not be a symbolic link",
      fixed = TRUE
    )
  } else {
    succeed()
  }
  expect_identical(
    readBin(target, what = "raw", n = file.info(target)$size),
    target_before
  )
  expect_identical(snapshot_test_cache(fixture$cache), source_before)
})
