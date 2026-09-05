# These private tests protect the read-only cache scan shared by planning,
# preparation, and stock source reuse.

test_that("cache observation records package archives without mutation", {
  cache_root <- make_test_cache()
  before <- snapshot_test_cache(cache_root)

  artifacts <- revdeprunner:::observe_cache_artifacts(cache_root)
  repeated <- revdeprunner:::observe_cache_artifacts(cache_root)

  expect_identical(artifacts, repeated)
  expect_identical(snapshot_test_cache(cache_root), before)
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
    artifacts$archive_type,
    c("binary", "unknown", "source", "source")
  )
  expect_identical(artifacts$needs_compilation, c(TRUE, NA, NA, FALSE))
  expect_false(any(startsWith(artifacts$relative_path, "_meta/")))
})

test_that("request-scoped observations carry cache priority", {
  first <- make_test_cache()
  second <- tempfile("second-cache-")
  dir.create(second)
  make_test_archive(
    second,
    "cran/src/contrib",
    "pureR",
    "1.0.0",
    "no"
  )
  requests <- data.frame(
    package = c("compiled", "pureR"),
    version = c("2.0.0", "1.0.0"),
    stringsAsFactors = FALSE
  )

  observations <- revdeprunner:::observe_cache_roots(
    c(first, second),
    requests
  )

  expect_setequal(observations$package, c("compiled", "pureR", "pureR"))
  expect_identical(unique(observations$priority), c(1L, 2L))
  expect_false(any(observations$package %in% c("broken", "incomplete")))
})

test_that("ZIP package metadata is read without cache extraction", {
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

  artifacts <- revdeprunner:::observe_cache_artifacts(cache_root)

  expect_identical(snapshot_test_cache(cache_root), before)
  expect_identical(artifacts$archive_type, "binary")
  expect_identical(artifacts$platform, "x86_64-w64-mingw32")
  expect_identical(artifacts$needs_compilation, TRUE)
  expect_identical(artifacts$status, "ok")
})

test_that("binary filenames supply a missing Built platform", {
  cache_root <- tempfile("pure-r-binary-cache-")
  dir.create(cache_root)
  built <- "R 4.5.2; ; 2026-08-30; unix"
  make_test_archive(
    cache_root,
    "cran-bin/src/contrib",
    "portable",
    "1.0.0",
    "no",
    built,
    "portable_1.0.0_R_x86_64-pc-linux-gnu.tar.gz"
  )
  make_test_archive(
    cache_root,
    "cran-bin/src/contrib",
    "unlabeled",
    "1.0.0",
    "yes",
    built,
    "unlabeled_1.0.0.tar.gz"
  )

  artifacts <- revdeprunner:::observe_cache_artifacts(cache_root)
  portable <- artifacts[artifacts$package == "portable", , drop = FALSE]
  unlabeled <- artifacts[artifacts$package == "unlabeled", , drop = FALSE]

  expect_identical(portable$archive_type, "binary")
  expect_identical(portable$platform, "x86_64-pc-linux-gnu")
  expect_identical(portable$status, "ok")
  expect_identical(unlabeled$status, "incomplete_metadata")
  expect_match(unlabeled$error, "Built field without a platform", fixed = TRUE)
})

test_that("cache observation handles empty and invalid roots", {
  empty_root <- tempfile("empty-cache-")
  dir.create(empty_root)

  expect_equal(
    nrow(revdeprunner:::observe_cache_artifacts(empty_root)),
    0L
  )
  expect_error(
    revdeprunner:::observe_cache_artifacts(character()),
    "must be one non-empty path",
    fixed = TRUE
  )
  expect_error(
    revdeprunner:::observe_cache_artifacts(tempfile("missing-cache-")),
    "must identify an existing directory",
    fixed = TRUE
  )
})

test_that("archive member selection rejects unsafe or ambiguous paths", {
  expect_identical(
    revdeprunner:::description_archive_member("package/DESCRIPTION"),
    "package/DESCRIPTION"
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

test_that("tar links are not materialized while reading DESCRIPTION", {
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

  artifacts <- revdeprunner:::observe_cache_artifacts(cache_root)

  expect_identical(artifacts$status, "ok")
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
  artifacts <- revdeprunner:::observe_cache_artifacts(cache_root)
  description_row <- artifacts$package == "linkeddesc"
  expect_identical(artifacts$status[description_row], "unreadable_archive")
  expect_match(
    artifacts$error[description_row],
    "regular tar member",
    fixed = TRUE
  )
})
