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
