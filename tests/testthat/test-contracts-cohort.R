# These internal tests protect the frozen snapshot and cohort machine records
# without exposing a public R API before the command layer exists.

cohort_fixture_database <- function() {
  data.frame(
    Package = c(
      "WeakOnly",
      "TransitiveTwo",
      "DirectTwo",
      "SubjectPkg",
      "DeepConsumer",
      "DirectSuggest",
      "DirectOne",
      "TransitiveOne"
    ),
    Version = c("1.0", "2.0", "3.0", "4.0", "5.0", "6.0", "7.0", "8.0"),
    Depends = c(
      NA,
      NA,
      NA,
      "R (>= 4.3)",
      "TransitiveOne",
      NA,
      "SubjectPkg",
      NA
    ),
    Imports = c(
      NA,
      "DirectTwo",
      "SubjectPkg",
      NA,
      NA,
      NA,
      NA,
      "DirectOne"
    ),
    LinkingTo = rep(NA_character_, 8L),
    Suggests = c(
      "DirectOne",
      NA,
      NA,
      NA,
      NA,
      "SubjectPkg",
      NA,
      NA
    ),
    Repository = rep("https://example.test/cran/src/contrib", 8L),
    NeedsCompilation = c("no", "no", "yes", "no", "no", "no", "no", "no"),
    SystemRequirements = c(NA, NA, "libexample", NA, NA, NA, NA, NA),
    stringsAsFactors = FALSE
  )
}

cohort_fixture_repositories <- function() {
  c(
    CRAN = "https://example.test/cran/src/contrib",
    BioCsoft = "https://example.test/bioc/src/contrib"
  )
}

test_that("repository snapshots normalize presentation and retain metadata", {
  database <- cohort_fixture_database()
  repositories <- cohort_fixture_repositories()
  snapshot <- revdeprunner:::new_repository_snapshot(repositories, database)
  reordered <- revdeprunner:::new_repository_snapshot(
    repositories,
    database[rev(seq_len(nrow(database))), rev(names(database)), drop = FALSE]
  )

  expect_s3_class(snapshot, "revdeprunner_repository_snapshot")
  expect_identical(snapshot, reordered)
  expect_identical(
    names(snapshot),
    c("schema_version", "snapshot_id", "filters", "repositories", "packages")
  )
  expect_identical(
    snapshot$schema_version,
    "revdeprunner-repository-snapshot/v1"
  )
  expect_identical(snapshot$filters, "available.packages(filters=list())")
  expect_identical(snapshot$repositories, repositories)
  expect_identical(
    snapshot$packages$Package,
    sort(database$Package, method = "radix")
  )
  expect_identical(
    names(snapshot$packages),
    sort(names(database), method = "radix")
  )
  expect_match(snapshot$snapshot_id, "^sha256:[a-f0-9]{64}$")
  expect_invisible(revdeprunner:::validate_repository_snapshot(snapshot))
})

test_that("tabular identity fields retain their canonical column-major form", {
  table <- data.frame(
    B = c("x", NA_character_),
    A = c("\u00e9", ""),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  expected <- c(
    "table.nrow" = "2",
    "table.ncol" = "2",
    "table.column.000001.name" = "utf8hex:42",
    "table.column.000001.row.000001" = "utf8hex:78",
    "table.column.000001.row.000002" = NA_character_,
    "table.column.000002.name" = "utf8hex:41",
    "table.column.000002.row.000001" = "utf8hex:c3a9",
    "table.column.000002.row.000002" = "utf8hex:"
  )

  expect_identical(
    revdeprunner:::tabular_identity_fields("table", table),
    expected
  )
})

test_that("snapshots collapse exact canonical Recommended duplicates", {
  database <- cohort_fixture_database()
  database$MD5sum <- rep(strrep("a", 32L), nrow(database))
  repositories <- cohort_fixture_repositories()
  canonical <- database[database$Package == "DirectOne", , drop = FALSE]
  recommended <- canonical
  recommended$Repository <- paste0(
    repositories[["CRAN"]],
    "/4.7.0/Recommended"
  )
  recommended$Depends <- "R (>= 4.7.0), SubjectPkg"

  baseline <- revdeprunner:::new_repository_snapshot(repositories, database)
  repeated <- revdeprunner:::new_repository_snapshot(
    repositories,
    rbind(recommended, database)
  )
  expect_identical(repeated, baseline)

  changed <- recommended
  changed$Version <- "7.1"
  expect_error(
    revdeprunner:::new_repository_snapshot(
      repositories,
      rbind(changed, database)
    ),
    "duplicate package rows within a repository",
    fixed = TRUE
  )

  changed_checksum <- recommended
  changed_checksum$MD5sum <- strrep("b", 32L)
  expect_error(
    revdeprunner:::new_repository_snapshot(
      repositories,
      rbind(changed_checksum, database)
    ),
    "duplicate package rows within a repository",
    fixed = TRUE
  )

  other_recommended <- recommended
  other_recommended$Repository <- paste0(
    repositories[["CRAN"]],
    "/4.8.0/Recommended"
  )
  without_canonical <- database[
    database$Package != "DirectOne",
    ,
    drop = FALSE
  ]
  expect_error(
    revdeprunner:::new_repository_snapshot(
      repositories,
      rbind(recommended, other_recommended, without_canonical)
    ),
    "duplicate package rows within a repository",
    fixed = TRUE
  )
})

test_that("snapshot identity covers repository priority and every metadata cell", {
  database <- cohort_fixture_database()
  repositories <- cohort_fixture_repositories()
  baseline <- revdeprunner:::new_repository_snapshot(repositories, database)
  reversed_repositories <- revdeprunner:::new_repository_snapshot(
    rev(repositories),
    database
  )
  changed_url <- repositories
  changed_url[[1L]] <- "https://mirror.test/cran/src/contrib"
  changed_url_database <- database
  changed_url_database$Repository <- sub(
    repositories[[1L]],
    changed_url[[1L]],
    changed_url_database$Repository,
    fixed = TRUE
  )
  changed_repository <- revdeprunner:::new_repository_snapshot(
    changed_url,
    changed_url_database
  )
  changed_database <- database
  changed_database$SystemRequirements[
    changed_database$Package == "DirectTwo"
  ] <-
    "libexample >= 2"
  changed_metadata <- revdeprunner:::new_repository_snapshot(
    repositories,
    changed_database
  )

  expect_false(identical(
    baseline$snapshot_id,
    reversed_repositories$snapshot_id
  ))
  expect_false(identical(baseline$snapshot_id, changed_repository$snapshot_id))
  expect_false(identical(baseline$snapshot_id, changed_metadata$snapshot_id))
})

test_that("snapshot constructors reject filtered or ambiguous inputs", {
  database <- cohort_fixture_database()
  repositories <- cohort_fixture_repositories()

  expect_error(
    revdeprunner:::new_repository_snapshot(
      repositories,
      database,
      filters = "duplicates"
    ),
    "exactly `list()`",
    fixed = TRUE
  )
  expect_error(
    revdeprunner:::new_repository_snapshot(unname(repositories), database),
    "named, ordered set",
    fixed = TRUE
  )
  duplicate_repositories <- c(repositories, Mirror = repositories[[1L]])
  expect_error(
    revdeprunner:::new_repository_snapshot(duplicate_repositories, database),
    "unique URLs",
    fixed = TRUE
  )

  missing_field <- database[,
    setdiff(names(database), "Suggests"),
    drop = FALSE
  ]
  expect_error(
    revdeprunner:::new_repository_snapshot(repositories, missing_field),
    "missing required fields: Suggests",
    fixed = TRUE
  )
  duplicate_package <- rbind(database, database[1L, , drop = FALSE])
  expect_error(
    revdeprunner:::new_repository_snapshot(repositories, duplicate_package),
    "duplicate package rows within a repository",
    fixed = TRUE
  )
  unattributed <- database
  unattributed$Repository[[1L]] <- "https://unknown.test/src/contrib"
  expect_error(
    revdeprunner:::new_repository_snapshot(repositories, unattributed),
    "belong to exactly one configured repository",
    fixed = TRUE
  )
  malformed_package <- database
  malformed_package$Package[[1L]] <- "bad_name"
  expect_error(
    revdeprunner:::new_repository_snapshot(repositories, malformed_package),
    "valid package name",
    fixed = TRUE
  )
  malformed_version <- database
  malformed_version$Version[[1L]] <- "not a version"
  expect_error(
    revdeprunner:::new_repository_snapshot(repositories, malformed_version),
    "valid package version",
    fixed = TRUE
  )
})

test_that("snapshots preserve cross-repository duplicates in priority order", {
  repositories <- cohort_fixture_repositories()
  database <- cohort_fixture_database()
  duplicate <- database[database$Package == "DirectOne", , drop = FALSE]
  duplicate$Version <- "7.5"
  duplicate$Repository <- repositories[["BioCsoft"]]
  combined <- rbind(duplicate, database)

  snapshot <- revdeprunner:::new_repository_snapshot(repositories, combined)
  reversed <- revdeprunner:::new_repository_snapshot(
    rev(repositories),
    combined
  )
  selected <- revdeprunner:::new_reverse_dependency_cohort(
    "SubjectPkg",
    snapshot
  )
  reversed_selected <- revdeprunner:::new_reverse_dependency_cohort(
    "SubjectPkg",
    reversed
  )

  expect_identical(sum(snapshot$packages$Package == "DirectOne"), 2L)
  expect_identical(
    snapshot$packages$Version[snapshot$packages$Package == "DirectOne"],
    c("7.0", "7.5")
  )
  expect_identical(
    reversed$packages$Version[reversed$packages$Package == "DirectOne"],
    c("7.5", "7.0")
  )
  expect_identical(
    selected$targets$version[selected$targets$package == "DirectOne"],
    "7.0"
  )
  expect_identical(
    reversed_selected$targets$version[
      reversed_selected$targets$package == "DirectOne"
    ],
    "7.5"
  )
  expect_false(identical(snapshot$snapshot_id, reversed$snapshot_id))
  expect_false(identical(selected$cohort_id, reversed_selected$cohort_id))
})

test_that("mixed-case snapshot identity is independent of collation locale", {
  repositories <- cohort_fixture_repositories()
  database <- cohort_fixture_database()[1:2, , drop = FALSE]
  database$Package <- c("aPkg", "BPkg")
  database$Version <- c("1.0", "2.0")
  database$Depends <- NA_character_
  database$Imports <- NA_character_
  database$LinkingTo <- NA_character_
  database$Suggests <- NA_character_
  database$aField <- c("alpha", "beta")
  database$BField <- c("gamma", "delta")

  original <- Sys.getlocale("LC_COLLATE")
  on.exit(Sys.setlocale("LC_COLLATE", original), add = TRUE)
  candidates <- unique(c(
    "C",
    "C.UTF-8",
    "en_US.UTF-8",
    "English_United States.1252",
    "English_United States.utf8",
    original
  ))
  available <- character()
  for (candidate in candidates) {
    selected <- suppressWarnings(Sys.setlocale("LC_COLLATE", candidate))
    if (!is.na(selected) && !selected %in% available) {
      available <- c(available, selected)
    }
  }
  expect_gte(length(available), 2L)

  snapshots <- lapply(
    available,
    function(locale) {
      expect_false(is.na(Sys.setlocale("LC_COLLATE", locale)))
      revdeprunner:::new_repository_snapshot(repositories, database)
    }
  )
  expect_true(all(vapply(
    snapshots,
    identical,
    logical(1L),
    snapshots[[1L]]
  )))
  expect_identical(snapshots[[1L]]$packages$Package, c("BPkg", "aPkg"))
  expect_identical(
    names(snapshots[[1L]]$packages),
    sort(names(database), method = "radix")
  )

  for (locale in available) {
    expect_false(is.na(Sys.setlocale("LC_COLLATE", locale)))
    expect_invisible(
      revdeprunner:::validate_repository_snapshot(snapshots[[1L]])
    )
  }
})

test_that("snapshot validation detects structural and identity mutation", {
  snapshot <- revdeprunner:::new_repository_snapshot(
    cohort_fixture_repositories(),
    cohort_fixture_database()
  )

  extra <- snapshot
  extra$unexpected <- "field"
  expect_error(
    revdeprunner:::validate_repository_snapshot(extra),
    "invalid structure",
    fixed = TRUE
  )

  changed <- snapshot
  changed$packages$NeedsCompilation[
    changed$packages$Package == "DirectTwo"
  ] <- "no"
  expect_error(
    revdeprunner:::validate_repository_snapshot(changed),
    "identity does not match",
    fixed = TRUE
  )

  denormalized <- snapshot
  denormalized$packages <- denormalized$packages[
    rev(seq_len(nrow(
      denormalized$packages
    ))),
    ,
    drop = FALSE
  ]
  expect_error(
    revdeprunner:::validate_repository_snapshot(denormalized),
    "not normalized",
    fixed = TRUE
  )
})

test_that("cohorts preserve direct and recursive-strong-only targets", {
  snapshot <- revdeprunner:::new_repository_snapshot(
    cohort_fixture_repositories(),
    cohort_fixture_database()
  )
  cohort <- revdeprunner:::new_reverse_dependency_cohort(
    "SubjectPkg",
    snapshot
  )

  expect_s3_class(cohort, "revdeprunner_reverse_dependency_cohort")
  expect_identical(
    names(cohort),
    c(
      "schema_version",
      "cohort_id",
      "snapshot_id",
      "package",
      "direct_query",
      "recursive_strong_query",
      "targets"
    )
  )
  expect_identical(
    cohort$schema_version,
    "revdeprunner-reverse-dependency-cohort/v1"
  )
  expect_identical(
    cohort$direct_query,
    c(which = "most", recursive = "false", reverse = "true")
  )
  expect_identical(
    cohort$recursive_strong_query,
    c(which = "most", recursive = "strong", reverse = "true")
  )
  expect_identical(
    cohort$targets$package[cohort$targets$role == "direct"],
    c("DirectOne", "DirectSuggest", "DirectTwo")
  )
  expect_identical(
    cohort$targets$package[cohort$targets$role == "recursive-strong-only"],
    c("DeepConsumer", "TransitiveOne", "TransitiveTwo")
  )
  expect_false("WeakOnly" %in% cohort$targets$package)
  expect_identical(
    cohort$targets$version,
    snapshot$packages$Version[
      match(cohort$targets$package, snapshot$packages$Package)
    ]
  )
  expect_identical(anyDuplicated(cohort$targets$package), 0L)
  expect_match(cohort$cohort_id, "^sha256:[a-f0-9]{64}$")
  expect_invisible(
    revdeprunner:::validate_reverse_dependency_cohort(cohort, snapshot)
  )
})

test_that("cohort records match the exact tools queries", {
  snapshot <- revdeprunner:::new_repository_snapshot(
    cohort_fixture_repositories(),
    cohort_fixture_database()
  )
  cohort <- revdeprunner:::new_reverse_dependency_cohort(
    "SubjectPkg",
    snapshot
  )
  database <- as.matrix(snapshot$packages)
  direct <- sort(
    tools::package_dependencies(
      "SubjectPkg",
      db = database,
      which = "most",
      recursive = FALSE,
      reverse = TRUE
    )$SubjectPkg,
    method = "radix"
  )
  recursive <- sort(
    tools::package_dependencies(
      "SubjectPkg",
      db = database,
      which = "most",
      recursive = "strong",
      reverse = TRUE
    )$SubjectPkg,
    method = "radix"
  )

  expect_identical(
    cohort$targets$package[cohort$targets$role == "direct"],
    direct
  )
  expect_identical(
    cohort$targets$package[cohort$targets$role == "recursive-strong-only"],
    setdiff(recursive, direct)
  )
})

test_that("cohort identities bind package, snapshot, membership, and versions", {
  repositories <- cohort_fixture_repositories()
  database <- cohort_fixture_database()
  snapshot <- revdeprunner:::new_repository_snapshot(repositories, database)
  repeated <- revdeprunner:::new_reverse_dependency_cohort(
    "SubjectPkg",
    snapshot
  )
  expect_identical(
    repeated,
    revdeprunner:::new_reverse_dependency_cohort("SubjectPkg", snapshot)
  )

  changed_database <- database
  changed_database$Version[changed_database$Package == "DirectOne"] <- "7.1"
  changed_snapshot <- revdeprunner:::new_repository_snapshot(
    repositories,
    changed_database
  )
  changed_version <- revdeprunner:::new_reverse_dependency_cohort(
    "SubjectPkg",
    changed_snapshot
  )
  other_package <- revdeprunner:::new_reverse_dependency_cohort(
    "DirectOne",
    snapshot
  )

  expect_false(identical(repeated$cohort_id, changed_version$cohort_id))
  expect_false(identical(repeated$cohort_id, other_package$cohort_id))
})

test_that("cohort validation rejects query, snapshot, and target mutation", {
  snapshot <- revdeprunner:::new_repository_snapshot(
    cohort_fixture_repositories(),
    cohort_fixture_database()
  )
  cohort <- revdeprunner:::new_reverse_dependency_cohort(
    "SubjectPkg",
    snapshot
  )

  changed_query <- cohort
  changed_query$direct_query[["which"]] <- "strong"
  expect_error(
    revdeprunner:::validate_reverse_dependency_cohort(changed_query, snapshot),
    "query contract is unsupported",
    fixed = TRUE
  )

  changed_target <- cohort
  direct_index <- which(changed_target$targets$role == "direct")[[1L]]
  changed_target$targets$role[[direct_index]] <- "recursive-strong-only"
  expect_error(
    revdeprunner:::validate_reverse_dependency_cohort(changed_target, snapshot),
    "targets do not match",
    fixed = TRUE
  )

  missing <- cohort[-length(cohort)]
  expect_error(
    revdeprunner:::validate_reverse_dependency_cohort(missing, snapshot),
    "invalid structure",
    fixed = TRUE
  )

  other_snapshot <- revdeprunner:::new_repository_snapshot(
    rev(cohort_fixture_repositories()),
    cohort_fixture_database()
  )
  expect_error(
    revdeprunner:::validate_reverse_dependency_cohort(cohort, other_snapshot),
    "does not belong",
    fixed = TRUE
  )
})

test_that("cohorts can explicitly record an empty result", {
  snapshot <- revdeprunner:::new_repository_snapshot(
    cohort_fixture_repositories(),
    cohort_fixture_database()
  )
  cohort <- revdeprunner:::new_reverse_dependency_cohort(
    "NoConsumers",
    snapshot
  )

  expect_identical(
    cohort$targets,
    revdeprunner:::empty_reverse_dependency_targets()
  )
  expect_invisible(
    revdeprunner:::validate_reverse_dependency_cohort(cohort, snapshot)
  )
})
