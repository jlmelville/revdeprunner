# These internal-report tests protect the complete Work Package 1 observation
# contract while Work Package 2 still owns the public schema and API.

test_that("cross-root inventory reports are complete and deterministic", {
  fixture <- make_report_inventory_fixture()
  before <- snapshot_report_inputs(fixture)

  report <- revdeprunner:::report_cache_inventories(fixture$inventory_paths)
  reordered <- revdeprunner:::report_cache_inventories(
    rev(fixture$inventory_paths)
  )

  expect_s3_class(report, "revdeprunner_inventory_report")
  expect_identical(report, reordered)
  expect_identical(snapshot_report_inputs(fixture), before)
  expect_identical(report$inventories$cache_root, sort(fixture$cache_roots))

  expect_equal(nrow(report$duplicate_hashes), 3L)
  expect_true(all(report$duplicate_hashes$sha256 == fixture$hashes[[1L]]))
  expect_identical(
    report$duplicate_hashes$cache_root,
    sort(fixture$cache_roots)
  )

  collision_counts <- table(report$hash_collisions$package)
  expect_identical(
    as.integer(collision_counts[c(
      "collision",
      "missingdimension",
      "mixed",
      "platformconflict",
      "rconflict"
    )]),
    rep(2L, 5L)
  )
  expect_false("duplicate" %in% report$hash_collisions$package)

  expect_identical(
    report$artifact_issues$status,
    c("incomplete_metadata", "unreadable_archive")
  )
  expect_identical(
    report$artifact_issues$package,
    c("incomplete", "broken")
  )

  conflict_counts <- table(report$compatibility_conflicts$package)
  expect_identical(
    as.integer(conflict_counts[c("platformconflict", "rconflict")]),
    c(2L, 2L)
  )
  expect_false(
    any(
      c("missingdimension", "mixed") %in%
        report$compatibility_conflicts$package
    )
  )
  expect_identical(
    unique(
      report$compatibility_conflicts$conflict_dimensions[
        report$compatibility_conflicts$package == "platformconflict"
      ]
    ),
    "platform"
  )
  expect_identical(
    unique(
      report$compatibility_conflicts$conflict_dimensions[
        report$compatibility_conflicts$package == "rconflict"
      ]
    ),
    "r_major_minor"
  )
  expect_identical(
    report$compatibility_conflicts$r_major_minor[
      report$compatibility_conflicts$package == "rconflict"
    ],
    c("4.4", "4.5")
  )
})

test_that("cross-root reports reject invalid inventory identities", {
  fixture <- make_report_inventory_fixture()
  invalid_name <- file.path(
    dirname(fixture$inventory_paths[[1L]]),
    "invalid.rds"
  )
  file.copy(fixture$inventory_paths[[1L]], invalid_name)

  expect_error(
    revdeprunner:::report_cache_inventories(
      c(invalid_name, fixture$inventory_paths[[2L]])
    ),
    "filename must contain its lowercase SHA-256 identity",
    fixed = TRUE
  )

  changed <- fixture$inventory_paths[[1L]]
  connection <- file(changed, open = "ab")
  writeBin(as.raw(0L), connection)
  close(connection)
  expect_error(
    revdeprunner:::report_cache_inventories(
      c(changed, fixture$inventory_paths[[2L]])
    ),
    "content does not match its filename identity",
    fixed = TRUE
  )
})

test_that("cross-root reports reject incompatible inventory structures", {
  fixture <- make_report_inventory_fixture()
  malformed <- structure(
    list(
      cache_root = tempfile("malformed-root-"),
      artifacts = data.frame(cache_root = "wrong"),
      repository_metadata = revdeprunner:::empty_repository_metadata_observations()
    ),
    class = "revdeprunner_cache_observation"
  )
  malformed_path <- write_report_inventory(
    malformed,
    file.path(fixture$root, "inventories")
  )

  expect_error(
    revdeprunner:::report_cache_inventories(
      c(malformed_path, fixture$inventory_paths[[2L]])
    ),
    "artifact rows have an invalid structure",
    fixed = TRUE
  )
})

test_that("cross-root reports reject correctly addressed non-inventories", {
  fixture <- make_report_inventory_fixture()
  payload <- charToRaw("not serialized R data")
  sha256 <- digest::digest(payload, algo = "sha256", serialize = FALSE)
  invalid_path <- file.path(
    fixture$root,
    "inventories",
    paste0(sha256, ".rds")
  )
  writeBin(payload, invalid_path)

  expect_error(
    revdeprunner:::report_cache_inventories(
      c(invalid_path, fixture$inventory_paths[[2L]])
    ),
    "Unable to deserialize the cache inventory",
    fixed = TRUE
  )
})

test_that("cross-root reports reject duplicate cache roots", {
  fixture <- make_report_inventory_fixture()
  first <- revdeprunner:::read_cache_inventory(
    fixture$inventory_paths[[1L]]
  )$observation
  first$repository_metadata <- data.frame(
    cache_root = first$cache_root,
    relative_path = "_meta/changed/PACKAGES",
    filename = "PACKAGES",
    size_bytes = 1,
    modified_at = "2026-08-29T00:00:00.000000Z",
    sha256 = paste(rep("4", 64L), collapse = ""),
    status = "ok",
    error = NA_character_,
    stringsAsFactors = FALSE
  )
  duplicate_root_path <- write_report_inventory(
    first,
    file.path(fixture$root, "inventories")
  )

  expect_error(
    revdeprunner:::report_cache_inventories(
      c(fixture$inventory_paths[[1L]], duplicate_root_path)
    ),
    "must describe a different cache root",
    fixed = TRUE
  )
})

test_that("cross-root reports detect inventory mutation while reading", {
  fixture <- make_report_inventory_fixture()
  read_cache_inventory <- revdeprunner:::read_cache_inventory
  mutated <- FALSE
  testthat::local_mocked_bindings(
    read_cache_inventory = function(path) {
      inventory <- read_cache_inventory(path)
      if (!mutated) {
        connection <- file(path, open = "ab")
        writeBin(as.raw(0L), connection)
        close(connection)
        mutated <<- TRUE
      }
      inventory
    },
    .package = "revdeprunner"
  )

  expect_error(
    revdeprunner:::report_cache_inventories(fixture$inventory_paths),
    "changed while reports were generated",
    fixed = TRUE
  )
})

test_that("cross-root reports require distinct regular inventory files", {
  fixture <- make_report_inventory_fixture()

  expect_error(
    revdeprunner:::report_cache_inventories(fixture$inventory_paths[[1L]]),
    "must contain at least two paths",
    fixed = TRUE
  )
  expect_error(
    revdeprunner:::report_cache_inventories(
      rep(fixture$inventory_paths[[1L]], 2L)
    ),
    "must be unique",
    fixed = TRUE
  )
})

test_that("cross-root reports refuse linked inventory inputs", {
  skip_on_os("windows")
  fixture <- make_report_inventory_fixture()
  linked_root <- file.path(fixture$root, "linked")
  dir.create(linked_root)
  linked_path <- file.path(
    linked_root,
    basename(fixture$inventory_paths[[1L]])
  )
  linked <- file.symlink(fixture$inventory_paths[[1L]], linked_path)
  skip_if_not(linked, "This platform cannot create file symlinks")

  expect_error(
    revdeprunner:::report_cache_inventories(
      c(linked_path, fixture$inventory_paths[[2L]])
    ),
    "must not be symbolic links",
    fixed = TRUE
  )
})

test_that("cross-root reports handle inventories without artifacts", {
  fixture_root <- tempfile("empty-inventory-report-")
  inventory_root <- file.path(fixture_root, "inventories")
  dir.create(inventory_root, recursive = TRUE)
  observations <- lapply(
    file.path(fixture_root, c("cache-a", "cache-b")),
    function(root) {
      make_report_observation(
        root,
        revdeprunner:::empty_artifact_observations()
      )
    }
  )
  inventory_paths <- vapply(
    observations,
    write_report_inventory,
    character(1L),
    directory = inventory_root
  )

  report <- revdeprunner:::report_cache_inventories(inventory_paths)

  expect_equal(nrow(report$duplicate_hashes), 0L)
  expect_equal(nrow(report$hash_collisions), 0L)
  expect_equal(nrow(report$artifact_issues), 0L)
  expect_equal(nrow(report$compatibility_conflicts), 0L)
})
