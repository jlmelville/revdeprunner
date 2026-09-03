# These private tests protect read-only selection between immutable cache
# inventory and the mutating warehouse promotion boundary.

# nolint start: object_usage_linter.
make_inventory_selection_fixture <- function() {
  root <- tempfile("inventory-selection-")
  paths <- file.path(
    root,
    c("package", "data", "runs", "inventories", "cache-a", "cache-b", "cache-c")
  )
  dir.create(root)
  invisible(lapply(paths, dir.create))
  writeLines(
    c("Package: fixture", "Version: 1.0.0"),
    file.path(paths[[1L]], "DESCRIPTION")
  )
  built <- "R 4.5.2; x86_64-pc-linux-gnu; 2026-08-29; unix"
  old_built <- "R 4.4.3; x86_64-pc-linux-gnu; 2026-08-29; unix"
  windows_built <- "R 4.5.2; x86_64-w64-mingw32; 2026-08-29; windows"

  selected_a <- make_test_archive(
    paths[[5L]],
    "repo-a/src/contrib",
    "selected",
    "1.0.0",
    "yes",
    built,
    "selected_1.0.0_R_x86_64-pc-linux-gnu.tar.gz"
  )
  selected_b <- make_test_archive(
    paths[[6L]],
    "repo-b/src/contrib",
    "selected",
    "1.0.0",
    "no",
    built,
    "selected_1.0.0_R_x86_64-pc-linux-gnu.tar.gz"
  )
  duplicate <- make_test_archive(
    paths[[5L]],
    "z-repo/src/contrib",
    "duplicate",
    "1.0.0",
    "no",
    built,
    "duplicate_1.0.0_R_x86_64-pc-linux-gnu.tar.gz"
  )
  duplicate_copy <- file.path(
    paths[[5L]],
    "a-repo/src/contrib",
    basename(duplicate)
  )
  dir.create(dirname(duplicate_copy), recursive = TRUE)
  stopifnot(file.copy(duplicate, duplicate_copy))
  make_test_archive(
    paths[[5L]],
    "first/src/contrib",
    "conflict",
    "1.0.0",
    "yes",
    built,
    "conflict_1.0.0_R_x86_64-pc-linux-gnu.tar.gz"
  )
  make_test_archive(
    paths[[5L]],
    "second/src/contrib",
    "conflict",
    "1.0.0",
    "no",
    built,
    "conflict_1.0.0_R_x86_64-pc-linux-gnu.tar.gz"
  )
  make_test_archive(
    paths[[5L]],
    "old/src/contrib",
    "oldr",
    "1.0.0",
    "no",
    old_built,
    "oldr_1.0.0_R_x86_64-pc-linux-gnu.tar.gz"
  )
  make_test_archive(
    paths[[5L]],
    "windows/src/contrib",
    "windows",
    "1.0.0",
    "no",
    windows_built,
    "windows_1.0.0_R_x86_64-w64-mingw32.zip"
  )
  make_test_archive(
    paths[[5L]],
    "incomplete/src/contrib",
    "incomplete",
    "1.0.0",
    "sometimes",
    built,
    "incomplete_1.0.0_R_x86_64-pc-linux-gnu.tar.gz"
  )
  selected_c <- make_test_archive(
    paths[[7L]],
    "repo-c/src/contrib",
    "selected",
    "1.0.0",
    "yes",
    built,
    "selected_1.0.0_R_x86_64-pc-linux-gnu.tar.gz"
  )

  inventory_paths <- vapply(
    paths[5:7],
    function(cache_root) {
      revdeprunner:::write_cache_inventory(
        cache_root,
        paths[[4L]],
        paths[[1L]]
      )$inventory_path
    },
    character(1L)
  )
  lane <- revdeprunner:::new_compatibility_lane(
    "4.5",
    "x86_64-pc-linux-gnu",
    "x86_64",
    "linux-glibc-2.39",
    "gcc-15.2.1"
  )
  other_lane <- revdeprunner:::new_compatibility_lane(
    "4.5",
    "x86_64-pc-linux-gnu",
    "x86_64",
    "linux-glibc-2.39",
    "clang-21.1.0"
  )
  path_plan <- revdeprunner:::new_runtime_root_plan(
    paths[[1L]],
    paths[[2L]],
    paths[[3L]],
    "run-20260829-wp3b",
    paths[5:7]
  )
  bindings <- data.frame(
    inventory_path = inventory_paths,
    lane_id = c(lane$lane_id, lane$lane_id, other_lane$lane_id),
    priority = 1:3,
    stringsAsFactors = FALSE
  )

  list(
    root = root,
    paths = paths,
    selected_a = normalizePath(selected_a, winslash = "/", mustWork = TRUE),
    selected_b = normalizePath(selected_b, winslash = "/", mustWork = TRUE),
    selected_c = normalizePath(selected_c, winslash = "/", mustWork = TRUE),
    duplicate_copy = normalizePath(
      duplicate_copy,
      winslash = "/",
      mustWork = TRUE
    ),
    inventory_paths = inventory_paths,
    lane = lane,
    other_lane = other_lane,
    path_plan = path_plan,
    bindings = bindings
  )
}
# nolint end

select_fixture_binary <- function(
  fixture,
  package = "selected",
  version = "1.0.0",
  bindings = fixture$bindings,
  lane = fixture$lane,
  path_plan = fixture$path_plan
) {
  requests <- data.frame(
    package = package,
    version = version,
    stringsAsFactors = FALSE
  )
  revdeprunner:::select_inventory_binaries(
    requests,
    bindings,
    lane,
    path_plan
  )[[package]]
}

test_that("inventory selection returns one exact compatible binary", {
  fixture <- make_inventory_selection_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  before <- snapshot_test_cache(fixture$root)

  selection <- select_fixture_binary(fixture)

  expect_s3_class(
    selection,
    "revdeprunner_inventory_artifact_selection"
  )
  expect_identical(
    names(selection),
    c(
      "status",
      "package",
      "version",
      "lane_id",
      "artifact",
      "inventory_path",
      "inventory_sha256",
      "cache_root",
      "relative_path",
      "source_path",
      "priority"
    )
  )
  expect_identical(selection$status, "selected")
  expect_identical(selection$source_path, fixture$selected_a)
  expect_identical(selection$priority, 1L)
  expect_identical(selection$artifact$archive_type, "binary")
  expect_identical(selection$artifact$lane_id, fixture$lane$lane_id)
  expect_identical(
    selection$artifact$sha256,
    digest::digest(
      fixture$selected_a,
      algo = "sha256",
      file = TRUE,
      serialize = FALSE
    )
  )
  expect_invisible(
    revdeprunner:::validate_inventory_artifact_selection(selection)
  )
  expect_identical(snapshot_test_cache(fixture$root), before)
})

test_that("inventory selection reports an explicit deterministic miss", {
  fixture <- make_inventory_selection_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)

  missing <- select_fixture_binary(fixture, package = "absent")

  expect_identical(missing$status, "missing")
  expect_null(missing$artifact)
  expect_true(is.na(missing$source_path))
  expect_true(is.na(missing$priority))
  expect_invisible(
    revdeprunner:::validate_inventory_artifact_selection(missing)
  )
})

test_that("explicit cache priority selects between different valid hashes", {
  fixture <- make_inventory_selection_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  bindings <- fixture$bindings
  bindings$priority[1:2] <- c(2L, 1L)

  selection <- select_fixture_binary(
    fixture,
    bindings = bindings[c(3L, 1L, 2L), ]
  )

  expect_identical(selection$source_path, fixture$selected_b)
  expect_identical(selection$priority, 1L)
})

test_that("identical duplicates are stable and conflicting hashes fail", {
  fixture <- make_inventory_selection_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)

  duplicate <- select_fixture_binary(fixture, package = "duplicate")
  expect_identical(duplicate$source_path, fixture$duplicate_copy)
  expect_error(
    select_fixture_binary(fixture, package = "conflict"),
    "conflicting artifact hashes",
    fixed = TRUE
  )
})

test_that("lane binding and archive Built fields must all match", {
  fixture <- make_inventory_selection_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  other_bindings <- fixture$bindings
  other_bindings$lane_id[] <- fixture$other_lane$lane_id

  expect_identical(
    select_fixture_binary(fixture, bindings = other_bindings)$status,
    "missing"
  )
  expect_identical(
    select_fixture_binary(fixture, package = "oldr")$status,
    "missing"
  )
  expect_identical(
    select_fixture_binary(fixture, package = "windows")$status,
    "missing"
  )
  expect_identical(
    select_fixture_binary(fixture, package = "incomplete")$status,
    "missing"
  )
})

test_that("selection rejects malformed bindings and undeclared roots", {
  fixture <- make_inventory_selection_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)

  duplicate_priority <- fixture$bindings
  duplicate_priority$priority[[2L]] <- 1L
  expect_error(
    select_fixture_binary(fixture, bindings = duplicate_priority),
    "invalid structure",
    fixed = TRUE
  )
  wrong_lane <- fixture$bindings
  wrong_lane$lane_id[[1L]] <- "not-an-identity"
  expect_error(
    select_fixture_binary(fixture, bindings = wrong_lane),
    "SHA-256 record identity",
    fixed = TRUE
  )

  reduced_plan <- revdeprunner:::new_runtime_root_plan(
    fixture$paths[[1L]],
    fixture$paths[[2L]],
    fixture$paths[[3L]],
    "run-20260829-reduced",
    fixture$paths[[5L]]
  )
  expect_error(
    select_fixture_binary(
      fixture,
      bindings = fixture$bindings[2L, , drop = FALSE],
      path_plan = reduced_plan
    ),
    "declared source-cache root",
    fixed = TRUE
  )
})

test_that("selection rejects changed inventory content", {
  fixture <- make_inventory_selection_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  inventory <- fixture$bindings$inventory_path[[1L]]
  connection <- file(inventory, open = "ab")
  on.exit(if (!is.null(connection)) close(connection), add = TRUE)
  writeBin(charToRaw("changed"), connection)
  close(connection)
  connection <- NULL
  expect_error(
    select_fixture_binary(fixture),
    "filename identity",
    fixed = TRUE
  )
})

test_that("selection rejects a linked live source where links are supported", {
  fixture <- make_inventory_selection_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  original <- paste0(fixture$selected_a, ".original")
  expect_true(file.rename(fixture$selected_a, original))
  linked <- suppressWarnings(file.symlink(original, fixture$selected_a))

  if (isTRUE(linked)) {
    expect_error(
      select_fixture_binary(fixture),
      "source must not be a symbolic link",
      fixed = TRUE
    )
  } else {
    succeed()
  }
})

test_that("selection is stable under input order and available collations", {
  fixture <- make_inventory_selection_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  original <- Sys.getlocale("LC_COLLATE")
  on.exit(
    suppressWarnings(Sys.setlocale("LC_COLLATE", original)),
    add = TRUE
  )
  available <- character()
  for (locale in unique(c("C", "C.UTF-8", original))) {
    selected <- suppressWarnings(Sys.setlocale("LC_COLLATE", locale))
    if (!is.na(selected)) {
      available <- c(available, locale)
    }
  }
  selections <- lapply(available, function(locale) {
    expect_false(is.na(Sys.setlocale("LC_COLLATE", locale)))
    select_fixture_binary(
      fixture,
      bindings = fixture$bindings[c(3L, 1L, 2L), ]
    )
  })

  expect_true(length(selections) >= 1L)
  expect_true(all(vapply(
    selections,
    identical,
    logical(1L),
    selections[[1L]]
  )))
})

test_that("selection validation rejects inconsistent operational records", {
  fixture <- make_inventory_selection_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  selection <- select_fixture_binary(fixture)

  changed <- selection
  changed$artifact$version <- "2.0.0"
  expect_error(
    revdeprunner:::validate_inventory_artifact_selection(changed),
    "identity does not match",
    fixed = TRUE
  )
  changed <- selection
  changed$source_path <- fixture$selected_b
  expect_error(
    revdeprunner:::validate_inventory_artifact_selection(changed),
    "provenance is inconsistent",
    fixed = TRUE
  )
  missing <- select_fixture_binary(fixture, package = "absent")
  missing$source_path <- fixture$selected_a
  expect_error(
    revdeprunner:::validate_inventory_artifact_selection(missing),
    "fields are inconsistent",
    fixed = TRUE
  )
})
