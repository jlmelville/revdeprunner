# These private tests protect the boundary that composes content-addressed
# inventory selection with copy-only warehouse promotion for a request set.

# nolint start: object_usage_linter.
make_inventory_reuse_fixture <- function() {
  root <- tempfile("inventory-reuse-")
  paths <- file.path(
    root,
    c("package", "data", "runs", "inventories", "cache")
  )
  dir.create(root)
  invisible(lapply(paths, dir.create))
  writeLines(
    c("Package: fixture", "Version: 1.0.0"),
    file.path(paths[[1L]], "DESCRIPTION")
  )
  built <- "R 4.5.2; x86_64-pc-linux-gnu; 2026-08-29; unix"

  alpha <- make_test_archive(
    paths[[5L]],
    "cran-bin/src/contrib",
    "alpha",
    "1.0.0",
    "no",
    built,
    "alpha_1.0.0_R_x86_64-pc-linux-gnu.tar.gz"
  )
  zulu <- make_test_archive(
    paths[[5L]],
    "cran-bin/src/contrib",
    "Zulu",
    "2.0.0",
    "yes",
    built,
    "Zulu_2.0.0_R_x86_64-pc-linux-gnu.tar.gz"
  )
  make_test_archive(
    paths[[5L]],
    "first-bin/src/contrib",
    "conflict",
    "3.0.0",
    "no",
    built,
    "conflict_3.0.0_R_x86_64-pc-linux-gnu.tar.gz"
  )
  make_test_archive(
    paths[[5L]],
    "second-bin/src/contrib",
    "conflict",
    "3.0.0",
    "yes",
    built,
    "conflict_3.0.0_R_x86_64-pc-linux-gnu.tar.gz"
  )

  inventory_path <- revdeprunner:::write_cache_inventory(
    paths[[5L]],
    paths[[4L]],
    paths[[1L]]
  )$inventory_path
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
    "run-20260829-wp3c",
    paths[[5L]]
  )
  bindings <- data.frame(
    inventory_path = inventory_path,
    lane_id = lane$lane_id,
    priority = 1L,
    stringsAsFactors = FALSE
  )

  list(
    root = root,
    paths = paths,
    cache = paths[[5L]],
    inventory_path = inventory_path,
    alpha = normalizePath(alpha, winslash = "/", mustWork = TRUE),
    zulu = normalizePath(zulu, winslash = "/", mustWork = TRUE),
    lane = lane,
    path_plan = path_plan,
    bindings = bindings
  )
}
# nolint end

inventory_reuse_requests <- function(...) {
  values <- list(...)
  data.frame(
    package = vapply(values, `[[`, character(1L), 1L),
    version = vapply(values, `[[`, character(1L), 2L),
    stringsAsFactors = FALSE
  )
}

reuse_fixture_binaries <- function(fixture, requests) {
  revdeprunner:::reuse_inventory_binaries(
    requests,
    fixture$bindings,
    fixture$lane,
    fixture$path_plan
  )
}

inventory_reuse_staging_files <- function(fixture) {
  staging <- file.path(fixture$paths[[2L]], "warehouse", ".staging")
  if (!dir.exists(staging)) {
    return(character())
  }
  list.files(staging, all.files = TRUE, no.. = TRUE, full.names = TRUE)
}

inventory_reuse_source_snapshot <- function(cache_root) {
  snapshot <- snapshot_test_cache(cache_root) # nolint: object_usage_linter.
  snapshot <- snapshot[order(snapshot$path, method = "radix"), , drop = FALSE]
  rownames(snapshot) <- NULL
  snapshot
}

inventory_reuse_signature <- function(reuse) {
  list(
    requests = reuse$requests,
    statuses = vapply(reuse$selections, `[[`, character(1L), "status"),
    artifact_ids = vapply(
      reuse$selections,
      function(selection) {
        if (is.null(selection$artifact)) NA_character_ else
          selection$artifact$artifact_id
      },
      character(1L)
    ),
    inventory_sha256 = vapply(
      reuse$selections,
      `[[`,
      character(1L),
      "inventory_sha256"
    ),
    source_paths = vapply(
      reuse$selections,
      `[[`,
      character(1L),
      "source_path"
    ),
    warehouse_paths = vapply(
      reuse$promotions,
      function(promotion) {
        if (is.null(promotion)) NA_character_ else promotion$warehouse_path
      },
      character(1L)
    )
  )
}

test_that("inventory reuse promotes hits and preserves explicit misses", {
  fixture <- make_inventory_reuse_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  source_before <- inventory_reuse_source_snapshot(fixture$cache)
  requests <- inventory_reuse_requests(
    c("alpha", "1.0.0"),
    c("absent", "1.0.0"),
    c("Zulu", "2.0.0")
  )

  reuse <- reuse_fixture_binaries(fixture, requests[c(2L, 1L, 3L), ])

  expect_s3_class(reuse, "revdeprunner_inventory_binary_reuse")
  expect_identical(
    names(reuse),
    c(
      "lane_id",
      "path_plan_id",
      "warehouse_root",
      "transfer_policy",
      "requests",
      "selections",
      "promotions"
    )
  )
  expect_identical(
    reuse$requests$package,
    sort(requests$package, method = "radix")
  )
  expect_identical(names(reuse$selections), reuse$requests$package)
  expect_identical(names(reuse$promotions), reuse$requests$package)
  expect_identical(reuse$selections$absent$status, "missing")
  expect_null(reuse$promotions$absent)
  expect_false(reuse$promotions$alpha$reused)
  expect_false(reuse$promotions$Zulu$reused)
  expect_true(file.exists(reuse$promotions$alpha$warehouse_path))
  expect_true(file.exists(reuse$promotions$Zulu$warehouse_path))
  expect_identical(
    digest::digest(
      reuse$promotions$alpha$warehouse_path,
      algo = "sha256",
      file = TRUE,
      serialize = FALSE
    ),
    reuse$selections$alpha$artifact$sha256
  )
  expect_invisible(
    revdeprunner:::validate_inventory_binary_reuse(
      reuse,
      fixture$lane,
      fixture$path_plan
    )
  )
  expect_identical(
    inventory_reuse_source_snapshot(fixture$cache),
    source_before
  )
  expect_length(inventory_reuse_staging_files(fixture), 0L)
})

test_that("inventory reuse resumes from exact warehouse payloads", {
  fixture <- make_inventory_reuse_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  requests <- inventory_reuse_requests(
    c("Zulu", "2.0.0"),
    c("alpha", "1.0.0")
  )

  first <- reuse_fixture_binaries(fixture, requests)
  second <- reuse_fixture_binaries(fixture, requests)

  expect_true(all(!vapply(first$promotions, `[[`, logical(1L), "reused")))
  expect_true(all(vapply(second$promotions, `[[`, logical(1L), "reused")))
  expect_identical(
    vapply(first$promotions, `[[`, character(1L), "warehouse_path"),
    vapply(second$promotions, `[[`, character(1L), "warehouse_path")
  )
  expect_length(inventory_reuse_staging_files(fixture), 0L)
})

test_that("binary reuse reads once and admits each new artifact once", {
  fixture <- make_inventory_reuse_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  requests <- inventory_reuse_requests(
    c("alpha", "1.0.0"),
    c("Zulu", "2.0.0")
  )
  inventory_reads <- character()
  archive_admissions <- character()
  real_read <- revdeprunner:::read_cache_inventory
  real_admit <- revdeprunner:::validate_warehouse_archive
  testthat::local_mocked_bindings(
    read_cache_inventory = function(path) {
      inventory_reads <<- c(inventory_reads, path)
      real_read(path)
    },
    validate_warehouse_archive = function(
      path,
      artifact,
      archive_name = basename(path)
    ) {
      archive_admissions <<- c(archive_admissions, path)
      real_admit(path, artifact, archive_name)
    },
    .package = "revdeprunner"
  )

  reuse <- reuse_fixture_binaries(fixture, requests)

  expect_identical(inventory_reads, fixture$inventory_path)
  expect_length(archive_admissions, 4L)
  expect_identical(
    sum(vapply(
      archive_admissions,
      function(path) revdeprunner:::path_is_within(fixture$cache, path),
      logical(1L)
    )),
    2L
  )
  expect_identical(
    sum(grepl("/warehouse/[.]staging/", archive_admissions)),
    2L
  )
  expect_true(all(!vapply(reuse$promotions, `[[`, logical(1L), "reused")))
})

test_that("inventory reuse validates exact unique requests", {
  fixture <- make_inventory_reuse_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  valid <- inventory_reuse_requests(c("alpha", "1.0.0"))

  expect_error(
    reuse_fixture_binaries(fixture, valid[FALSE, ]),
    "invalid structure",
    fixed = TRUE
  )
  expect_error(
    reuse_fixture_binaries(fixture, valid[c(1L, 1L), ]),
    "unique packages",
    fixed = TRUE
  )
  malformed <- valid
  malformed$package <- "not a package"
  expect_error(
    reuse_fixture_binaries(fixture, malformed),
    "valid package name",
    fixed = TRUE
  )
  wrong_fields <- valid[c("version", "package")]
  expect_error(
    reuse_fixture_binaries(fixture, wrong_fields),
    "invalid structure",
    fixed = TRUE
  )
  expect_false(dir.exists(file.path(fixture$paths[[2L]], "warehouse")))
})

test_that("selection ambiguity prevents every warehouse write", {
  fixture <- make_inventory_reuse_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  source_before <- inventory_reuse_source_snapshot(fixture$cache)
  requests <- inventory_reuse_requests(
    c("alpha", "1.0.0"),
    c("conflict", "3.0.0")
  )

  expect_error(
    reuse_fixture_binaries(fixture, requests),
    "conflicting artifact hashes",
    fixed = TRUE
  )

  expect_false(dir.exists(file.path(fixture$paths[[2L]], "warehouse")))
  expect_identical(
    inventory_reuse_source_snapshot(fixture$cache),
    source_before
  )
})

test_that("binary reuse admits exact cache payloads before publication", {
  fixture <- make_inventory_reuse_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  requests <- inventory_reuse_requests(c("alpha", "1.0.0"))
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
  expect_false(dir.exists(file.path(fixture$paths[[2L]], "warehouse")))
})

test_that("inventory reuse ordering is input- and locale-stable", {
  fixture <- make_inventory_reuse_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  source_before <- inventory_reuse_source_snapshot(fixture$cache)
  requests <- inventory_reuse_requests(
    c("alpha", "1.0.0"),
    c("absent", "1.0.0"),
    c("Zulu", "2.0.0")
  )
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
  signatures <- lapply(seq_along(available), function(index) {
    expect_false(is.na(Sys.setlocale("LC_COLLATE", available[[index]])))
    rows <- if (index %% 2L == 0L) seq_len(nrow(requests)) else
      rev(seq_len(nrow(requests)))
    inventory_reuse_signature(reuse_fixture_binaries(fixture, requests[rows, ]))
  })

  expect_true(length(signatures) >= 1L)
  expect_true(all(vapply(
    signatures,
    identical,
    logical(1L),
    signatures[[1L]]
  )))
  expect_identical(
    inventory_reuse_source_snapshot(fixture$cache),
    source_before
  )
})

test_that("inventory reuse validation rejects inconsistent relationships", {
  fixture <- make_inventory_reuse_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  requests <- inventory_reuse_requests(
    c("alpha", "1.0.0"),
    c("absent", "1.0.0")
  )
  reuse <- reuse_fixture_binaries(fixture, requests)

  changed <- reuse
  changed$requests <- changed$requests[rev(seq_len(nrow(changed$requests))), ]
  rownames(changed$requests) <- NULL
  expect_error(
    revdeprunner:::validate_inventory_binary_reuse(
      changed,
      fixture$lane,
      fixture$path_plan
    ),
    "not normalized",
    fixed = TRUE
  )

  changed <- reuse
  changed$requests$version[changed$requests$package == "alpha"] <- "2.0.0"
  expect_error(
    revdeprunner:::validate_inventory_binary_reuse(
      changed,
      fixture$lane,
      fixture$path_plan
    ),
    "selection does not match",
    fixed = TRUE
  )

  changed <- reuse
  changed$promotions$absent <- changed$promotions$alpha
  expect_error(
    revdeprunner:::validate_inventory_binary_reuse(
      changed,
      fixture$lane,
      fixture$path_plan
    ),
    "must not have a promotion",
    fixed = TRUE
  )

  changed <- reuse
  changed$promotions$alpha$artifact_id <- reuse$lane_id
  expect_error(
    revdeprunner:::validate_inventory_binary_reuse(
      changed,
      fixture$lane,
      fixture$path_plan
    ),
    "does not match its selection",
    fixed = TRUE
  )

  changed <- reuse
  undeclared_root <- file.path(fixture$root, "undeclared-cache")
  changed$selections$alpha$cache_root <- undeclared_root
  changed$selections$alpha$source_path <- file.path(
    undeclared_root,
    changed$selections$alpha$relative_path
  )
  changed$promotions$alpha$source_path <- changed$selections$alpha$source_path
  expect_error(
    revdeprunner:::validate_inventory_binary_reuse(
      changed,
      fixture$lane,
      fixture$path_plan
    ),
    "undeclared source-cache root",
    fixed = TRUE
  )

  other_lane <- revdeprunner:::new_compatibility_lane(
    "4.5",
    "x86_64-pc-linux-gnu",
    "x86_64",
    "linux-glibc-2.39",
    "clang-21.1.0"
  )
  expect_error(
    revdeprunner:::validate_inventory_binary_reuse(
      reuse,
      other_lane,
      fixture$path_plan
    ),
    "does not match its lane",
    fixed = TRUE
  )
  other_plan <- revdeprunner:::new_runtime_root_plan(
    fixture$paths[[1L]],
    fixture$paths[[2L]],
    fixture$paths[[3L]],
    "run-20260829-other",
    fixture$cache
  )
  expect_error(
    revdeprunner:::validate_inventory_binary_reuse(
      reuse,
      fixture$lane,
      other_plan
    ),
    "runtime-root plan",
    fixed = TRUE
  )
})
