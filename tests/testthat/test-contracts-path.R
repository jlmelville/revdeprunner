# These internal tests protect the filesystem boundary that later mutating
# commands must revalidate before they act.

make_runtime_root_fixture <- function() {
  root <- tempfile("runtime-root-contract-")
  dir.create(root)
  paths <- file.path(
    root,
    c("package", "data", "runs", "cache-a", "cache-b", "outside")
  )
  invisible(lapply(paths, dir.create))
  writeLines(
    c("Package: fixture", "Version: 1.0.0"),
    file.path(paths[[1L]], "DESCRIPTION")
  )

  list(
    root = root,
    package = paths[[1L]],
    data = paths[[2L]],
    runs = paths[[3L]],
    cache_a = paths[[4L]],
    cache_b = paths[[5L]],
    outside = paths[[6L]]
  )
}

new_fixture_runtime_root_plan <- function(fixture, ...) {
  arguments <- list(
    package_root = fixture$package,
    data_root = fixture$data,
    runs_root = fixture$runs,
    run_id = "run-20260829-a1",
    source_cache_roots = c(fixture$cache_b, fixture$cache_a)
  )
  arguments[names(list(...))] <- list(...)
  do.call(revdeprunner:::new_runtime_root_plan, arguments)
}

test_that("runtime root plans freeze exact safe operational roots", {
  fixture <- make_runtime_root_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)

  before <- snapshot_test_cache(fixture$root)
  plan <- new_fixture_runtime_root_plan(fixture)
  repeated <- new_fixture_runtime_root_plan(fixture)

  expect_s3_class(plan, "revdeprunner_runtime_root_plan")
  expect_identical(plan, repeated)
  expect_identical(snapshot_test_cache(fixture$root), before)
  expect_identical(
    names(plan),
    c(
      "schema_version",
      "path_plan_id",
      "run_id",
      "package_root",
      "data_root",
      "runs_root",
      "source_cache_roots",
      "paths"
    )
  )
  expect_identical(
    plan$schema_version,
    "revdeprunner-runtime-root-plan/v5"
  )
  expect_match(plan$path_plan_id, "^sha256:[a-f0-9]{64}$")
  expect_identical(
    plan$source_cache_roots,
    sort(normalizePath(c(fixture$cache_a, fixture$cache_b)), method = "radix")
  )
  expect_identical(
    names(plan$paths),
    c("role", "path")
  )
  expect_identical(
    plan$paths$role,
    c(
      "package-checkout",
      "source-cache-000001",
      "source-cache-000002",
      "source-cache",
      "binary-cache",
      "run"
    )
  )
  expect_identical(
    plan$paths$path,
    c(
      normalizePath(fixture$package),
      plan$source_cache_roots,
      file.path(normalizePath(fixture$data), "source-cache"),
      file.path(normalizePath(fixture$data), "binary-cache"),
      file.path(normalizePath(fixture$runs), plan$run_id)
    )
  )
  expect_invisible(revdeprunner:::validate_runtime_root_plan(plan))
})

test_that("source-cache ordering and identities are locale-independent", {
  fixture <- make_runtime_root_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  mixed <- file.path(fixture$root, c("cache-z", "Cache-A"))
  invisible(lapply(mixed, dir.create))

  forward <- new_fixture_runtime_root_plan(
    fixture,
    source_cache_roots = mixed
  )
  reversed <- new_fixture_runtime_root_plan(
    fixture,
    source_cache_roots = rev(mixed)
  )
  expect_identical(forward, reversed)

  original_locale <- Sys.getlocale("LC_COLLATE")
  on.exit(
    suppressWarnings(Sys.setlocale("LC_COLLATE", original_locale)),
    add = TRUE
  )
  plans <- list()
  for (locale in c("C", "C.UTF-8")) {
    selected <- suppressWarnings(Sys.setlocale("LC_COLLATE", locale))
    if (!is.na(selected) && nzchar(selected)) {
      plans[[selected]] <- new_fixture_runtime_root_plan(
        fixture,
        source_cache_roots = rev(mixed)
      )
    }
  }
  expect_gte(length(plans), 1L)
  expect_true(all(vapply(
    plans,
    identical,
    logical(1L),
    y = forward
  )))
})

test_that("runtime anchors resolve physical symlink aliases when available", {
  fixture <- make_runtime_root_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  alias <- file.path(fixture$root, "cache-alias")
  linked <- file.symlink(fixture$cache_a, alias)

  if (isTRUE(linked)) {
    plan <- new_fixture_runtime_root_plan(
      fixture,
      source_cache_roots = alias
    )
    expect_identical(plan$source_cache_roots, normalizePath(fixture$cache_a))
  } else {
    succeed()
  }
})

test_that("runtime root plans require existing valid anchors and run ids", {
  fixture <- make_runtime_root_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)

  missing <- file.path(fixture$root, "missing")
  expect_error(
    new_fixture_runtime_root_plan(fixture, data_root = missing),
    "existing directory",
    fixed = TRUE
  )
  file_anchor <- file.path(fixture$root, "not-a-directory")
  writeLines("file", file_anchor)
  expect_error(
    new_fixture_runtime_root_plan(fixture, runs_root = file_anchor),
    "directory",
    fixed = TRUE
  )
  non_package <- file.path(fixture$root, "non-package")
  dir.create(non_package)
  expect_error(
    new_fixture_runtime_root_plan(fixture, package_root = non_package),
    "R package checkout",
    fixed = TRUE
  )
  expect_error(
    new_fixture_runtime_root_plan(fixture, source_cache_roots = character()),
    "one or more paths",
    fixed = TRUE
  )

  for (run_id in c("../escape", "nested/run", "C:drive", "trail.", "CON")) {
    expect_error(
      new_fixture_runtime_root_plan(fixture, run_id = run_id),
      "portable path component",
      fixed = TRUE,
      info = paste("must reject", run_id)
    )
  }
  expect_error(
    new_fixture_runtime_root_plan(
      fixture,
      run_id = paste(rep("a", 129L), collapse = "")
    ),
    "portable path component",
    fixed = TRUE
  )
})

test_that("all runtime anchor trees must be disjoint", {
  fixture <- make_runtime_root_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)

  package_data <- file.path(fixture$package, "data")
  package_runs <- file.path(fixture$package, "runs")
  package_cache <- file.path(fixture$package, "cache")
  data_runs <- file.path(fixture$data, "runs")
  data_cache <- file.path(fixture$data, "cache")
  runs_cache <- file.path(fixture$runs, "cache")
  nested_cache <- file.path(fixture$cache_a, "nested")
  nested <- c(
    package_data,
    package_runs,
    package_cache,
    data_runs,
    data_cache,
    runs_cache,
    nested_cache
  )
  invisible(lapply(nested, dir.create))

  cases <- list(
    list(data_root = package_data),
    list(runs_root = package_runs),
    list(runs_root = data_runs),
    list(source_cache_roots = package_cache),
    list(source_cache_roots = data_cache),
    list(source_cache_roots = runs_cache),
    list(source_cache_roots = c(fixture$cache_a, nested_cache)),
    list(source_cache_roots = c(fixture$cache_a, fixture$cache_a))
  )
  expected <- c(rep("must not overlap", 7L), "must be unique")
  for (index in seq_along(cases)) {
    expect_error(
      do.call(
        new_fixture_runtime_root_plan,
        c(list(fixture = fixture), cases[[index]])
      ),
      expected[[index]],
      fixed = TRUE,
      info = paste("overlap case", index)
    )
  }
})

test_that("runner package caches can seed later runtime plans", {
  fixture <- make_runtime_root_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)

  binary_cache_root <- file.path(fixture$data, "binary-cache")
  source_cache_root <- file.path(fixture$data, "source-cache")
  published <- file.path(
    c(source_cache_root, binary_cache_root),
    "src",
    "contrib"
  )
  invisible(lapply(published, dir.create, recursive = TRUE))

  plan <- new_fixture_runtime_root_plan(
    fixture,
    source_cache_roots = c(published, fixture$cache_a)
  )
  expect_true(all(normalizePath(published) %in% plan$source_cache_roots))
  expect_invisible(revdeprunner:::validate_runtime_root_plan(plan))

  rejected <- c(
    binary_cache_root,
    source_cache_root
  )
  for (path in rejected) {
    expect_error(
      new_fixture_runtime_root_plan(
        fixture,
        source_cache_roots = c(path, fixture$cache_a)
      ),
      "must not overlap",
      fixed = TRUE,
      info = paste("must reject managed data path", path)
    )
  }
})

test_that("existing safe derived directories remain valid", {
  fixture <- make_runtime_root_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  invisible(lapply(
    file.path(fixture$data, c("source-cache", "binary-cache")),
    dir.create
  ))
  dir.create(file.path(fixture$runs, "run-20260829-a1"))

  plan <- new_fixture_runtime_root_plan(fixture)
  expect_invisible(revdeprunner:::validate_runtime_root_plan(plan))
})

test_that("derived paths reject files and symbolic links", {
  fixture <- make_runtime_root_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)

  source_cache <- file.path(fixture$data, "source-cache")
  writeLines("not a directory", source_cache)
  expect_error(
    new_fixture_runtime_root_plan(fixture),
    "must identify a directory",
    fixed = TRUE
  )
  unlink(source_cache)

  run_root <- file.path(fixture$runs, "run-20260829-a1")
  writeLines("not a directory", run_root)
  expect_error(
    new_fixture_runtime_root_plan(fixture),
    "must identify a directory",
    fixed = TRUE
  )
  unlink(run_root)

  linked <- file.symlink(fixture$outside, source_cache)
  if (isTRUE(linked)) {
    expect_error(
      new_fixture_runtime_root_plan(fixture),
      "must not traverse a symbolic link",
      fixed = TRUE
    )
    unlink(source_cache)
  } else {
    succeed()
  }
})

test_that("validation rechecks changed filesystem boundaries", {
  fixture <- make_runtime_root_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  plan <- new_fixture_runtime_root_plan(fixture)

  source_cache <- file.path(fixture$data, "source-cache")
  linked <- file.symlink(fixture$outside, source_cache)
  if (isTRUE(linked)) {
    expect_error(
      revdeprunner:::validate_runtime_root_plan(plan),
      "must not traverse a symbolic link",
      fixed = TRUE
    )
    unlink(source_cache)
  } else {
    succeed()
  }

  unlink(fixture$cache_a, recursive = TRUE)
  expect_invisible(revdeprunner:::validate_runtime_root_plan(plan))
  expect_error(
    new_fixture_runtime_root_plan(fixture),
    "existing directory",
    fixed = TRUE
  )
})

test_that("validation rejects structural semantic and identity mutation", {
  fixture <- make_runtime_root_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  plan <- new_fixture_runtime_root_plan(fixture)

  invalid <- plan[-length(plan)]
  expect_error(
    revdeprunner:::validate_runtime_root_plan(invalid),
    "invalid structure",
    fixed = TRUE
  )
  invalid <- plan
  invalid$extra <- "field"
  expect_error(
    revdeprunner:::validate_runtime_root_plan(invalid),
    "invalid structure",
    fixed = TRUE
  )
  invalid <- unclass(plan)
  expect_error(
    revdeprunner:::validate_runtime_root_plan(invalid),
    "invalid structure",
    fixed = TRUE
  )
  invalid <- plan
  invalid$schema_version <- "revdeprunner-runtime-root-plan/v1"
  expect_error(
    revdeprunner:::validate_runtime_root_plan(invalid),
    "unsupported",
    fixed = TRUE
  )
  invalid <- plan
  invalid$path_plan_id <- paste0("sha256:", paste(rep("0", 64L), collapse = ""))
  expect_error(
    revdeprunner:::validate_runtime_root_plan(invalid),
    "identity does not match",
    fixed = TRUE
  )
  invalid <- plan
  invalid$run_id <- "changed-run"
  expect_error(
    revdeprunner:::validate_runtime_root_plan(invalid),
    "does not match its anchors",
    fixed = TRUE
  )
  invalid <- plan
  invalid$source_cache_roots <- rev(invalid$source_cache_roots)
  expect_error(
    revdeprunner:::validate_runtime_root_plan(invalid),
    "not normalized",
    fixed = TRUE
  )

  for (field in c("role", "path")) {
    invalid <- plan
    invalid$paths[[field]][[1L]] <- paste0("changed-", field)
    expect_error(
      revdeprunner:::validate_runtime_root_plan(invalid),
      "does not match its anchors",
      fixed = TRUE,
      info = paste("must reject changed path-table", field)
    )
  }
  invalid <- plan
  invalid$paths$extra <- "field"
  expect_error(
    revdeprunner:::validate_runtime_root_plan(invalid),
    "invalid structure",
    fixed = TRUE
  )
})

test_that("different accepted roots and run ids change path identity", {
  fixture <- make_runtime_root_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  other_data <- file.path(fixture$root, "other-data")
  other_runs <- file.path(fixture$root, "other-runs")
  other_cache <- file.path(fixture$root, "other-cache")
  invisible(lapply(c(other_data, other_runs, other_cache), dir.create))

  baseline <- new_fixture_runtime_root_plan(fixture)
  alternatives <- list(
    new_fixture_runtime_root_plan(fixture, data_root = other_data),
    new_fixture_runtime_root_plan(fixture, runs_root = other_runs),
    new_fixture_runtime_root_plan(fixture, run_id = "run-20260829-b2"),
    new_fixture_runtime_root_plan(
      fixture,
      source_cache_roots = other_cache
    )
  )
  ids <- vapply(alternatives, `[[`, character(1L), "path_plan_id")

  expect_false(any(ids == baseline$path_plan_id))
  expect_length(unique(ids), length(alternatives))
})
