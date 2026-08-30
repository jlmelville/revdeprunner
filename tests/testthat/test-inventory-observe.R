# These tests use the internal observation layer because Work Package 2 owns the
# public command and schema contracts. The protected behavior is that inventory
# observation reads source roots without mutating them.

test_that("cache observation records archives and repository metadata", {
  cache_root <- make_test_cache()
  before <- snapshot_test_cache(cache_root)

  observation <- revdeprunner:::observe_cache(cache_root)
  repeated <- revdeprunner:::observe_cache(cache_root)

  expect_s3_class(observation, "revdeprunner_cache_observation")
  expect_identical(observation, repeated)
  expect_identical(snapshot_test_cache(cache_root), before)

  artifacts <- observation$artifacts
  expect_identical(
    artifacts$relative_path,
    c(
      "cran-bin/src/contrib/compiled_2.0.0_R_x86_64-pc-linux-gnu.tar.gz",
      "cran/src/contrib/broken_3.0.0.tar.gz",
      "cran/src/contrib/incomplete_4.0.0.tar.gz",
      "cran/src/contrib/pureR_1.0.0.tar.gz"
    )
  )
  expect_identical(
    artifacts$status,
    c("ok", "unreadable_archive", "incomplete_metadata", "ok")
  )
  expect_identical(
    artifacts$package,
    c("compiled", "broken", "incomplete", "pureR")
  )
  expect_identical(
    artifacts$version,
    c("2.0.0", "3.0.0", "4.0.0", "1.0.0")
  )
  expect_identical(
    artifacts$archive_type,
    c("binary", "unknown", "source", "source")
  )
  expect_identical(artifacts$needs_compilation, c(TRUE, NA, NA, FALSE))
  expect_identical(
    artifacts$platform,
    c("x86_64-pc-linux-gnu", NA, NA, NA)
  )
  expect_match(
    artifacts$error[[2L]],
    "Tar archive",
    fixed = TRUE
  )
  expect_match(
    artifacts$error[[3L]],
    "DESCRIPTION has no Package field",
    fixed = TRUE
  )
  expect_match(
    artifacts$error[[3L]],
    "DESCRIPTION has an invalid NeedsCompilation field",
    fixed = TRUE
  )
  expect_true(all(nchar(artifacts$sha256) == 64L))

  repository_metadata <- observation$repository_metadata
  expect_identical(
    repository_metadata$relative_path,
    c("_meta/example/PACKAGES.gz", "_meta/example/PACKAGES.rds")
  )
  expect_identical(repository_metadata$status, c("ok", "ok"))
  expect_true(all(nchar(repository_metadata$sha256) == 64L))
})

test_that("ZIP package metadata is read without extracting into the cache", {
  cache_root <- tempfile("zip-cache-")
  dir.create(cache_root)
  make_test_archive(
    cache_root,
    repository = "cran-bin/src/contrib",
    package = "windows",
    version = "5.0.0",
    needs_compilation = "yes",
    built = "R 4.5.2; x86_64-w64-mingw32; 2026-08-29; windows",
    filename = "windows_5.0.0.zip"
  )
  before <- snapshot_test_cache(cache_root)

  observation <- revdeprunner:::observe_cache(cache_root)

  expect_identical(snapshot_test_cache(cache_root), before)
  expect_identical(observation$artifacts$archive_type, "binary")
  expect_identical(observation$artifacts$platform, "x86_64-w64-mingw32")
  expect_identical(observation$artifacts$needs_compilation, TRUE)
  expect_identical(observation$artifacts$status, "ok")
})

test_that("pure-R binaries use their explicit filename platform", {
  cache_root <- tempfile("pure-r-binary-cache-")
  dir.create(cache_root)
  repository <- "cran-bin/src/contrib"
  built <- "R 4.5.2; ; 2026-08-30; unix"
  make_test_archive(
    cache_root,
    repository,
    package = "portable",
    version = "1.0.0",
    needs_compilation = "no",
    built = built,
    filename = "portable_1.0.0_R_x86_64-pc-linux-gnu.tar.gz"
  )
  make_test_archive(
    cache_root,
    repository,
    package = "compiled",
    version = "1.0.0",
    needs_compilation = "yes",
    built = built,
    filename = "compiled_1.0.0_R_x86_64-pc-linux-gnu.tar.gz"
  )

  artifacts <- revdeprunner:::observe_cache(cache_root)$artifacts
  portable <- artifacts[artifacts$package == "portable", , drop = FALSE]
  compiled <- artifacts[artifacts$package == "compiled", , drop = FALSE]

  expect_identical(portable$archive_type, "binary")
  expect_identical(portable$platform, "x86_64-pc-linux-gnu")
  expect_identical(portable$needs_compilation, FALSE)
  expect_identical(portable$status, "ok")
  expect_true(is.na(portable$error))
  expect_identical(compiled$status, "incomplete_metadata")
  expect_match(compiled$error, "Built field without a platform", fixed = TRUE)
})

test_that("cache observation handles empty roots and rejects invalid roots", {
  empty_root <- tempfile("empty-cache-")
  dir.create(empty_root)

  observation <- revdeprunner:::observe_cache(empty_root)
  expect_equal(nrow(observation$artifacts), 0L)
  expect_equal(nrow(observation$repository_metadata), 0L)

  expect_error(
    revdeprunner:::observe_cache(character()),
    "must be one non-empty path",
    fixed = TRUE
  )
  expect_error(
    revdeprunner:::observe_cache(tempfile("missing-cache-")),
    "must identify an existing directory",
    fixed = TRUE
  )

  file_root <- tempfile("file-cache-")
  writeLines("not a directory", file_root)
  expect_error(
    revdeprunner:::observe_cache(file_root),
    "must identify a directory",
    fixed = TRUE
  )
})

test_that("archive member selection rejects unsafe or ambiguous paths", {
  expect_identical(
    revdeprunner:::description_archive_member("package/DESCRIPTION"),
    "package/DESCRIPTION"
  )
  expect_identical(
    revdeprunner:::description_archive_member("./package/DESCRIPTION"),
    "./package/DESCRIPTION"
  )
  expect_error(
    revdeprunner:::description_archive_member("../DESCRIPTION"),
    "exactly one top-level package DESCRIPTION",
    fixed = TRUE
  )
  expect_error(
    revdeprunner:::description_archive_member(
      c("one/DESCRIPTION", "two/DESCRIPTION")
    ),
    "exactly one top-level package DESCRIPTION",
    fixed = TRUE
  )
})

test_that("cache traversal fails closed on unreadable directories", {
  skip_on_os("windows")
  cache_root <- tempfile("unreadable-cache-")
  locked <- file.path(cache_root, "locked")
  dir.create(locked, recursive = TRUE)
  writeBin(charToRaw("archive"), file.path(locked, "hidden_1.0.0.tar.gz"))
  Sys.chmod(locked, mode = "0000", use_umask = FALSE)
  on.exit(Sys.chmod(locked, mode = "0700", use_umask = FALSE), add = TRUE)
  if (file.access(locked, mode = 4L) == 0L) {
    skip("The test process can still read a mode-0000 directory")
  }

  expect_error(
    revdeprunner:::observe_cache(cache_root),
    "cannot read directory",
    fixed = TRUE
  )
})

test_that("cache traversal refuses linked directories", {
  skip_on_os("windows")
  cache_root <- tempfile("linked-directory-cache-")
  outside <- tempfile("outside-cache-")
  dir.create(cache_root)
  dir.create(outside)
  writeBin(charToRaw("archive"), file.path(outside, "outside_1.0.0.tar.gz"))
  linked <- file.symlink(outside, file.path(cache_root, "linked"))
  skip_if_not(linked, "This platform cannot create directory symlinks")

  expect_error(
    revdeprunner:::observe_cache(cache_root),
    "refuses symbolic-link directories",
    fixed = TRUE
  )
})

test_that("tar links are never materialized while reading DESCRIPTION", {
  skip_on_os("windows")
  skip_if(Sys.which("tar") == "", "A system tar is required for link fixtures")
  cache_root <- tempfile("tar-link-cache-")
  dir.create(cache_root)

  linked <- make_linked_test_archive(cache_root)
  skip_if_not(linked$supported, "This platform cannot create archive links")
  target_hash <- digest::digest(
    linked$target,
    algo = "sha256",
    file = TRUE,
    serialize = FALSE
  )
  target_before <- file.info(linked$target, extra_cols = FALSE)
  observation <- revdeprunner:::observe_cache(cache_root)
  expect_identical(observation$artifacts$status, "ok")
  expect_identical(file.info(linked$target, extra_cols = FALSE), target_before)
  expect_identical(
    digest::digest(
      linked$target,
      algo = "sha256",
      file = TRUE,
      serialize = FALSE
    ),
    target_hash
  )

  linked_description <- make_linked_test_archive(
    cache_root,
    description_is_link = TRUE
  )
  skip_if_not(
    linked_description$supported,
    "This platform cannot create archive links"
  )
  observation <- revdeprunner:::observe_cache(cache_root)
  description_row <- observation$artifacts$package == "linkeddesc"
  expect_identical(
    observation$artifacts$status[description_row],
    "unreadable_archive"
  )
  expect_match(
    observation$artifacts$error[description_row],
    "regular tar member",
    fixed = TRUE
  )
})

# These internal-writer tests protect the source-root and output-boundary safety
# contract until Work Package 2 introduces a public manifest API.

test_that("cache inventories are deterministic, immutable, and source-safe", {
  cache_root <- make_test_cache()
  staging_root <- tempfile("inventory-staging-")
  dir.create(staging_root)
  package_root <- make_test_package_root()
  source_before <- snapshot_test_cache(cache_root)

  first <- revdeprunner:::write_cache_inventory(
    cache_root,
    staging_root,
    package_root
  )
  first_payload <- readBin(
    first$inventory_path,
    what = "raw",
    n = file.info(first$inventory_path)$size
  )
  second <- revdeprunner:::write_cache_inventory(
    cache_root,
    staging_root,
    package_root
  )

  expect_s3_class(first, "revdeprunner_inventory_write")
  expect_false(first$reused)
  expect_true(second$reused)
  expect_identical(second$inventory_path, first$inventory_path)
  expect_identical(second$inventory_sha256, first$inventory_sha256)
  expect_identical(second$source_sha256, first$source_sha256)
  expect_identical(snapshot_test_cache(cache_root), source_before)
  expect_identical(
    readRDS(first$inventory_path),
    revdeprunner:::observe_cache(cache_root)
  )
  expect_identical(
    readBin(
      second$inventory_path,
      what = "raw",
      n = file.info(second$inventory_path)$size
    ),
    first_payload
  )
  expect_length(
    list.files(staging_root, pattern = "\\.rds$", recursive = TRUE),
    1L
  )
})

test_that("changed observations publish new inventories without overwriting", {
  cache_root <- make_test_cache()
  staging_root <- tempfile("inventory-staging-")
  dir.create(staging_root)
  package_root <- make_test_package_root()

  first <- revdeprunner:::write_cache_inventory(
    cache_root,
    staging_root,
    package_root
  )
  first_payload <- readBin(
    first$inventory_path,
    what = "raw",
    n = file.info(first$inventory_path)$size
  )
  make_test_archive(
    cache_root,
    repository = "other/src/contrib",
    package = "added",
    version = "1.0.0",
    needs_compilation = "no"
  )

  second <- revdeprunner:::write_cache_inventory(
    cache_root,
    staging_root,
    package_root
  )

  expect_false(second$reused)
  expect_false(identical(second$inventory_path, first$inventory_path))
  expect_identical(
    readBin(
      first$inventory_path,
      what = "raw",
      n = file.info(first$inventory_path)$size
    ),
    first_payload
  )
  expect_length(
    list.files(staging_root, pattern = "\\.rds$", recursive = TRUE),
    2L
  )
})

test_that("inventory paths must remain outside source and package roots", {
  cache_root <- make_test_cache()
  staging_root <- tempfile("inventory-staging-")
  non_package_root <- tempfile("non-package-root-")
  dir.create(staging_root)
  dir.create(non_package_root)
  package_root <- make_test_package_root()

  expect_error(
    revdeprunner:::write_cache_inventory(
      cache_root,
      cache_root,
      package_root
    ),
    "must not overlap `cache_root`",
    fixed = TRUE
  )
  expect_error(
    revdeprunner:::write_cache_inventory(
      cache_root,
      package_root,
      package_root
    ),
    "must not overlap `package_root`",
    fixed = TRUE
  )
  expect_error(
    revdeprunner:::write_cache_inventory(
      cache_root,
      tempfile("missing-staging-"),
      package_root
    ),
    "must identify an existing directory",
    fixed = TRUE
  )
  expect_error(
    revdeprunner:::write_cache_inventory(
      cache_root,
      staging_root,
      non_package_root
    ),
    "must identify an R package checkout",
    fixed = TRUE
  )
  expect_length(list.files(staging_root, all.files = TRUE, no.. = TRUE), 0L)
})

test_that("inventory staging refuses linked directories", {
  skip_on_os("windows")
  cache_root <- make_test_cache()
  staging_root <- tempfile("inventory-staging-")
  outside <- tempfile("inventory-outside-")
  dir.create(staging_root)
  dir.create(outside)
  package_root <- make_test_package_root()
  linked <- file.symlink(
    outside,
    file.path(staging_root, "cache-inventories")
  )
  skip_if_not(linked, "This platform cannot create directory symlinks")

  expect_error(
    revdeprunner:::write_cache_inventory(
      cache_root,
      staging_root,
      package_root
    ),
    "must not be symbolic links",
    fixed = TRUE
  )
  expect_length(list.files(outside, all.files = TRUE, no.. = TRUE), 0L)
})

test_that("inventory publication refuses linked content addresses", {
  skip_on_os("windows")
  cache_root <- make_test_cache()
  staging_root <- tempfile("inventory-staging-")
  dir.create(staging_root)
  package_root <- make_test_package_root()
  inventory <- revdeprunner:::write_cache_inventory(
    cache_root,
    staging_root,
    package_root
  )
  missing_target <- tempfile("missing-inventory-target-")
  unlink(inventory$inventory_path)
  linked <- file.symlink(missing_target, inventory$inventory_path)
  skip_if_not(linked, "This platform cannot create file symlinks")

  expect_error(
    revdeprunner:::write_cache_inventory(
      cache_root,
      staging_root,
      package_root
    ),
    "must not be a symbolic link",
    fixed = TRUE
  )
  expect_false(file.exists(missing_target))
})

test_that("source changes prevent inventory publication", {
  cache_root <- make_test_cache()
  staging_root <- tempfile("inventory-staging-")
  dir.create(staging_root)
  package_root <- make_test_package_root()
  observe_cache <- revdeprunner:::observe_cache
  testthat::local_mocked_bindings(
    observe_cache = function(cache_root) {
      observation <- observe_cache(cache_root)
      writeLines("changed", file.path(cache_root, "changed-during-read"))
      observation
    },
    .package = "revdeprunner"
  )

  expect_error(
    revdeprunner:::write_cache_inventory(
      cache_root,
      staging_root,
      package_root
    ),
    "changed during inventory serialization",
    fixed = TRUE
  )
  expect_length(list.files(staging_root, all.files = TRUE, no.. = TRUE), 0L)
})

test_that("content-address collisions fail without overwriting", {
  cache_root <- make_test_cache()
  staging_root <- tempfile("inventory-staging-")
  dir.create(staging_root)
  package_root <- make_test_package_root()
  inventory <- revdeprunner:::write_cache_inventory(
    cache_root,
    staging_root,
    package_root
  )
  corrupt_payload <- charToRaw("corrupt")
  writeBin(corrupt_payload, inventory$inventory_path)

  expect_error(
    revdeprunner:::write_cache_inventory(
      cache_root,
      staging_root,
      package_root
    ),
    "does not match its identity",
    fixed = TRUE
  )
  expect_identical(
    readBin(
      inventory$inventory_path,
      what = "raw",
      n = file.info(inventory$inventory_path)$size
    ),
    corrupt_payload
  )
})
