# These private tests protect exact cache selection and publication into the
# runner-owned binary cache. The exported prepare/check path is tested
# separately in test-revdep-run.R.

cache_reuse_requests <- function(...) {
  values <- list(...)
  data.frame(
    package = vapply(values, `[[`, character(1L), 1L),
    version = vapply(values, `[[`, character(1L), 2L),
    stringsAsFactors = FALSE
  )
}

make_cache_reuse_fixture <- function() {
  root <- tempfile("cache-reuse-")
  paths <- file.path(root, c("package", "data", "runs", "cache-a", "cache-b"))
  dir.create(root)
  invisible(lapply(paths, dir.create))
  writeLines(
    c("Package: fixture", "Version: 1.0.0"),
    file.path(paths[[1L]], "DESCRIPTION")
  )
  built <- "R 4.5.2; x86_64-pc-linux-gnu; 2026-08-29; unix"
  old_built <- "R 4.4.3; x86_64-pc-linux-gnu; 2026-08-29; unix"
  alpha <- make_test_archive(
    paths[[4L]],
    "cran-bin/src/contrib",
    "alpha",
    "1.0.0",
    "no",
    built,
    "alpha_1.0.0_R_x86_64-pc-linux-gnu.tar.gz"
  )
  alpha_second <- make_test_archive(
    paths[[5L]],
    "cran-bin/src/contrib",
    "alpha",
    "1.0.0",
    "yes",
    built,
    "alpha_1.0.0_R_x86_64-pc-linux-gnu.tar.gz"
  )
  zulu <- make_test_archive(
    paths[[4L]],
    "cran-bin/src/contrib",
    "Zulu",
    "2.0.0",
    "yes",
    built,
    "Zulu_2.0.0_R_x86_64-pc-linux-gnu.tar.gz"
  )
  duplicate <- make_test_archive(
    paths[[4L]],
    "z-repo/src/contrib",
    "duplicate",
    "1.0.0",
    "no",
    built,
    "duplicate_1.0.0_R_x86_64-pc-linux-gnu.tar.gz"
  )
  duplicate_copy <- file.path(
    paths[[4L]],
    "a-repo/src/contrib",
    basename(duplicate)
  )
  dir.create(dirname(duplicate_copy), recursive = TRUE)
  stopifnot(file.copy(duplicate, duplicate_copy))
  make_test_archive(
    paths[[4L]],
    "first/src/contrib",
    "conflict",
    "1.0.0",
    "yes",
    built,
    "conflict_1.0.0_R_x86_64-pc-linux-gnu.tar.gz"
  )
  make_test_archive(
    paths[[4L]],
    "second/src/contrib",
    "conflict",
    "1.0.0",
    "no",
    built,
    "conflict_1.0.0_R_x86_64-pc-linux-gnu.tar.gz"
  )
  make_test_archive(
    paths[[4L]],
    "old/src/contrib",
    "oldr",
    "1.0.0",
    "no",
    old_built,
    "oldr_1.0.0_R_x86_64-pc-linux-gnu.tar.gz"
  )
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
    "run-20260829-cache-reuse",
    paths[4:5]
  )
  requests <- cache_reuse_requests(
    c("alpha", "1.0.0"),
    c("Zulu", "2.0.0"),
    c("duplicate", "1.0.0"),
    c("conflict", "1.0.0"),
    c("oldr", "1.0.0")
  )
  observations <- revdeprunner:::observe_cache_roots(paths[4:5], requests)

  list(
    root = root,
    paths = paths,
    cache = paths[[4L]],
    alpha = normalizePath(alpha, winslash = "/", mustWork = TRUE),
    alpha_second = normalizePath(
      alpha_second,
      winslash = "/",
      mustWork = TRUE
    ),
    zulu = normalizePath(zulu, winslash = "/", mustWork = TRUE),
    lane = lane,
    path_plan = path_plan,
    observations = observations
  )
}

reuse_fixture_binaries <- function(fixture, requests, observations = NULL) {
  if (is.null(observations)) {
    observations <- fixture$observations
  }
  revdeprunner:::reuse_cached_binaries(
    requests,
    observations,
    fixture$lane,
    fixture$path_plan
  )
}

test_that("cache selection is exact, prioritized, and deterministic", {
  fixture <- make_cache_reuse_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  requests <- cache_reuse_requests(
    c("oldr", "1.0.0"),
    c("absent", "1.0.0"),
    c("alpha", "1.0.0"),
    c("duplicate", "1.0.0")
  )

  selections <- revdeprunner:::select_cached_binaries(
    requests,
    fixture$observations,
    fixture$lane,
    fixture$path_plan
  )

  expect_identical(names(selections), sort(requests$package, method = "radix"))
  expect_identical(selections$alpha$status, "selected")
  expect_identical(selections$alpha$source_path, fixture$alpha)
  expect_identical(selections$alpha$priority, 1L)
  expect_identical(selections$duplicate$status, "selected")
  expect_match(selections$duplicate$relative_path, "a-repo", fixed = TRUE)
  expect_identical(selections$oldr$status, "missing")
  expect_identical(selections$absent$status, "missing")

  reversed <- revdeprunner:::observe_cache_roots(
    rev(fixture$paths[4:5]),
    requests
  )
  selected <- revdeprunner:::select_cached_binaries(
    requests,
    reversed,
    fixture$lane,
    fixture$path_plan
  )
  expect_identical(selected$alpha$source_path, fixture$alpha_second)
})

test_that("conflicting hashes prevent runner-cache publication", {
  fixture <- make_cache_reuse_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  requests <- cache_reuse_requests(
    c("alpha", "1.0.0"),
    c("conflict", "1.0.0")
  )

  expect_error(
    reuse_fixture_binaries(fixture, requests),
    "conflicting artifact hashes",
    fixed = TRUE
  )
  expect_false(dir.exists(file.path(fixture$paths[[2L]], "binary-cache")))
})

test_that("binary reuse publishes hits and preserves misses", {
  fixture <- make_cache_reuse_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  source_before <- snapshot_test_cache(fixture$cache)
  requests <- cache_reuse_requests(
    c("alpha", "1.0.0"),
    c("absent", "1.0.0"),
    c("Zulu", "2.0.0")
  )

  reuse <- reuse_fixture_binaries(fixture, requests[c(2L, 1L, 3L), ])

  expect_s3_class(reuse, "revdeprunner_binary_reuse")
  expect_identical(
    names(reuse),
    c(
      "lane_id",
      "path_plan_id",
      "requests",
      "observations",
      "selections",
      "cache_paths"
    )
  )
  expect_identical(reuse$observations, fixture$observations)
  expect_identical(reuse$selections$absent$status, "missing")
  expect_true(is.na(reuse$cache_paths[["absent"]]))
  expect_true(all(file.exists(reuse$cache_paths[c("alpha", "Zulu")])))
  expect_identical(snapshot_test_cache(fixture$cache), source_before)
  expect_invisible(
    revdeprunner:::validate_binary_reuse(
      reuse,
      fixture$lane,
      fixture$path_plan
    )
  )
})

test_that("binary reuse rejects cache changes observed before publication", {
  fixture <- make_cache_reuse_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  requests <- cache_reuse_requests(c("alpha", "1.0.0"))
  connection <- file(fixture$alpha, open = "ab")
  on.exit(if (!is.null(connection)) close(connection), add = TRUE)
  writeBin(charToRaw("changed"), connection)
  close(connection)
  connection <- NULL

  expect_error(
    reuse_fixture_binaries(fixture, requests),
    "payload does not match its SHA-256 identity",
    fixed = TRUE
  )
  expect_false(dir.exists(file.path(fixture$paths[[2L]], "binary-cache")))
})

test_that("cache observations and reuse inputs reject inconsistent structure", {
  fixture <- make_cache_reuse_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  requests <- cache_reuse_requests(c("alpha", "1.0.0"))

  expect_error(
    reuse_fixture_binaries(fixture, requests[c(1L, 1L), ]),
    "unique packages",
    fixed = TRUE
  )
  observations <- fixture$observations
  observations$priority[[1L]] <- 0L
  expect_error(
    reuse_fixture_binaries(fixture, requests, observations),
    "invalid values",
    fixed = TRUE
  )
  observations <- fixture$observations
  observations$cache_root[[1L]] <- fixture$paths[[2L]]
  expect_error(
    reuse_fixture_binaries(fixture, requests, observations),
    "do not match the runtime cache roots",
    fixed = TRUE
  )
})
