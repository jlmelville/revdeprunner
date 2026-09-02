# These internal tests protect the frozen stock-runner dependency-universe
# record without exposing a public R API before the command layer exists.

dependency_fixture_repositories <- function() {
  c(
    CRAN = "https://example.test/cran/src/contrib",
    BioCsoft = "https://example.test/bioc/src/contrib"
  )
}

dependency_fixture_base_packages <- function() {
  c(
    "base",
    "compiler",
    "datasets",
    "graphics",
    "grDevices",
    "grid",
    "methods",
    "parallel",
    "splines",
    "stats",
    "stats4",
    "tcltk",
    "tools",
    "utils"
  )
}

dependency_fixture_database <- function() {
  packages <- c(
    "WeakOnly",
    "SuggestedOne",
    "DirectB",
    "SubjectPkg",
    "DeepHard",
    "RecursiveOnly",
    "DirectA",
    "Shared",
    "HardOne",
    "LinkOne",
    "SubjectHard",
    "SuggestHard",
    "IgnoredWeak"
  )
  data.frame(
    Package = packages,
    Version = stats::setNames(
      paste0(seq_along(packages), ".0"),
      packages
    )[packages],
    Depends = c(
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      "R (>= 4.3), SubjectPkg, HardOne, stats",
      "DeepHard",
      NA,
      NA,
      NA,
      NA,
      NA
    ),
    Imports = c(
      NA,
      "SuggestHard",
      "SubjectPkg",
      "SubjectHard",
      "HardOne, MissingHard",
      "DirectA",
      "Shared",
      NA,
      "DeepHard, DirectA",
      NA,
      "MissingRunnerHard",
      NA,
      NA
    ),
    LinkingTo = c(
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      "LinkOne",
      NA,
      NA,
      NA,
      NA,
      NA,
      NA
    ),
    Suggests = c(
      "DirectA",
      NA,
      "SuggestedOne",
      NA,
      NA,
      NA,
      "SuggestedOne, MissingSuggest",
      NA,
      "IgnoredWeak",
      NA,
      NA,
      NA,
      NA
    ),
    Repository = rep(dependency_fixture_repositories()[["CRAN"]], 13L),
    NeedsCompilation = rep("no", 13L),
    stringsAsFactors = FALSE
  )
}

dependency_fixture_contracts <- function(
  database = dependency_fixture_database(),
  repositories = dependency_fixture_repositories()
) {
  snapshot <- revdeprunner:::new_repository_snapshot(repositories, database)
  cohort <- revdeprunner:::new_reverse_dependency_cohort(
    "SubjectPkg",
    snapshot
  )
  list(snapshot = snapshot, cohort = cohort)
}

stock_fixture_parse_dependencies <- function(values, base_packages) {
  values[is.na(values)] <- ""
  values <- gsub("\\s+", "", values)
  values <- gsub("\\([^)]+\\)", "", values)
  values <- strsplit(values[nzchar(values)], ",", fixed = TRUE)
  setdiff(unique(unlist(values, use.names = FALSE)), c("R", base_packages))
}

# This independently translates revdepcheck 1.0.0.9002's cran_deps() and
# deps_opts() closure operations after the frozen snapshot has resolved one row
# per package by repository priority.
stock_fixture_install_set <- function(
  target,
  packages,
  base_packages,
  runner_supplied
) {
  packages <- packages[!duplicated(packages$Package), , drop = FALSE]
  current <- dependencies <- target
  fields <- c("Depends", "Imports", "LinkingTo", "Suggests")
  repeat {
    records <- packages[
      packages$Package %in% dependencies,
      fields,
      drop = FALSE
    ]
    reached <- stock_fixture_parse_dependencies(
      as.matrix(records),
      base_packages
    )
    dependencies <- sort(
      unique(c(dependencies, reached)),
      method = "radix"
    )
    if (identical(current, dependencies)) {
      break
    }
    fields <- c("Depends", "Imports", "LinkingTo")
    current <- dependencies
  }
  dependencies <- setdiff(
    dependencies,
    c(target, base_packages, runner_supplied)
  )
  sort(
    intersect(dependencies, packages$Package),
    method = "radix"
  )
}

test_that("dependency universes freeze the stock-runner query contract", {
  contracts <- dependency_fixture_contracts()
  universe <- revdeprunner:::new_dependency_universe(
    contracts$cohort,
    contracts$snapshot,
    "direct",
    rev(dependency_fixture_base_packages())
  )

  expect_s3_class(universe, "revdeprunner_dependency_universe")
  expect_identical(
    names(universe),
    c(
      "schema_version",
      "universe_id",
      "snapshot_id",
      "cohort_id",
      "cohort_policy",
      "first_level_fields",
      "recursive_fields",
      "runner_supplied",
      "base_packages",
      "targets",
      "dependencies",
      "edges"
    )
  )
  expect_identical(
    universe$schema_version,
    "revdeprunner-dependency-universe/v1"
  )
  expect_identical(
    universe$first_level_fields,
    c("Depends", "Imports", "LinkingTo", "Suggests")
  )
  expect_identical(
    universe$recursive_fields,
    c("Depends", "Imports", "LinkingTo")
  )
  expect_identical(universe$runner_supplied, "SubjectPkg")
  expect_identical(
    universe$base_packages,
    sort(dependency_fixture_base_packages(), method = "radix")
  )
  expect_identical(
    universe$targets$package,
    contracts$cohort$targets$package[contracts$cohort$targets$role == "direct"]
  )
  expect_match(universe$universe_id, "^sha256:[a-f0-9]{64}$")
  expect_invisible(
    revdeprunner:::validate_dependency_universe(
      universe,
      contracts$cohort,
      contracts$snapshot
    )
  )
})

test_that("direct target graphs preserve paths, cycles, and field policy", {
  contracts <- dependency_fixture_contracts()
  universe <- revdeprunner:::new_dependency_universe(
    contracts$cohort,
    contracts$snapshot,
    "direct",
    dependency_fixture_base_packages()
  )
  direct_a <- universe$edges[universe$edges$target == "DirectA", , drop = FALSE]

  expect_true(any(
    direct_a$from_package == "DirectA" &
      direct_a$dependency == "SuggestedOne" &
      direct_a$relationship == "Suggests"
  ))
  expect_true(any(
    direct_a$from_package == "SuggestedOne" &
      direct_a$dependency == "SuggestHard" &
      direct_a$relationship == "Imports"
  ))
  expect_false(any(
    direct_a$from_package == "HardOne" &
      direct_a$dependency == "IgnoredWeak"
  ))
  expect_true(any(
    direct_a$from_package == "HardOne" &
      direct_a$dependency == "DirectA"
  ))
  expect_true(any(
    direct_a$from_package == "DirectA" &
      direct_a$dependency == "HardOne"
  ))
  expect_true(any(
    direct_a$from_package == "HardOne" &
      direct_a$dependency == "DeepHard"
  ))
  expect_true(any(
    direct_a$from_package == "Shared" &
      direct_a$dependency == "DeepHard"
  ))
  expect_identical(
    anyDuplicated(direct_a),
    0L
  )
})

test_that("dependency dispositions expose every stock-runner exclusion", {
  contracts <- dependency_fixture_contracts()
  universe <- revdeprunner:::new_dependency_universe(
    contracts$cohort,
    contracts$snapshot,
    "direct",
    dependency_fixture_base_packages()
  )
  direct_a <- universe$dependencies[
    universe$dependencies$target == "DirectA",
    ,
    drop = FALSE
  ]
  disposition <- stats::setNames(
    direct_a$disposition,
    direct_a$dependency
  )

  expect_identical(disposition[["HardOne"]], "install")
  expect_identical(disposition[["SubjectPkg"]], "runner-supplied")
  expect_identical(disposition[["R"]], "base")
  expect_identical(disposition[["stats"]], "base")
  expect_identical(disposition[["DirectA"]], "target-supplied")
  expect_identical(disposition[["MissingSuggest"]], "unavailable")
  expect_identical(disposition[["MissingHard"]], "unavailable")
  expect_identical(disposition[["MissingRunnerHard"]], "unavailable")
  expect_identical(
    direct_a$version[direct_a$dependency == "HardOne"],
    contracts$snapshot$packages$Version[
      match("HardOne", contracts$snapshot$packages$Package)
    ]
  )
  expect_true(all(is.na(direct_a$version[direct_a$disposition != "install"])))
})

test_that("preparation requires unavailable hard dependencies, not Suggests", {
  contracts <- dependency_fixture_contracts()
  universe <- revdeprunner:::new_dependency_universe(
    contracts$cohort,
    contracts$snapshot,
    "direct",
    dependency_fixture_base_packages()
  )

  requirements <- revdeprunner:::derive_preparation_requirements(universe)
  unavailable <- requirements$package[
    requirements$disposition == "unavailable"
  ]

  expect_setequal(
    unique(unavailable),
    c("MissingHard", "MissingRunnerHard")
  )
  expect_false("MissingSuggest" %in% requirements$package)
  expect_true(any(
    universe$dependencies$dependency == "MissingSuggest" &
      universe$dependencies$disposition == "unavailable"
  ))
})

test_that("install dispositions match the observed stock dependency set", {
  contracts <- dependency_fixture_contracts()
  universe <- revdeprunner:::new_dependency_universe(
    contracts$cohort,
    contracts$snapshot,
    "direct",
    dependency_fixture_base_packages()
  )

  for (target in universe$targets$package) {
    observed <- universe$dependencies$dependency[
      universe$dependencies$target == target &
        universe$dependencies$disposition == "install"
    ]
    expected <- stock_fixture_install_set(
      target,
      contracts$snapshot$packages,
      dependency_fixture_base_packages(),
      contracts$cohort$package
    )
    expect_identical(sort(observed, method = "radix"), expected)
  }
})

test_that("recursive-strong policy adds every transitive cohort target", {
  contracts <- dependency_fixture_contracts()
  direct <- revdeprunner:::new_dependency_universe(
    contracts$cohort,
    contracts$snapshot,
    "direct",
    dependency_fixture_base_packages()
  )
  recursive <- revdeprunner:::new_dependency_universe(
    contracts$cohort,
    contracts$snapshot,
    "recursive-strong",
    dependency_fixture_base_packages()
  )

  expect_identical(
    direct$targets$package,
    contracts$cohort$targets$package[contracts$cohort$targets$role == "direct"]
  )
  expect_identical(recursive$targets, contracts$cohort$targets)
  expect_true(all(direct$targets$package %in% recursive$targets$package))
  expect_true("RecursiveOnly" %in% recursive$targets$package)
  expect_false("WeakOnly" %in% recursive$targets$package)
  expect_false(identical(direct$universe_id, recursive$universe_id))

  recursive_only_edges <- recursive$edges[
    recursive$edges$target == "RecursiveOnly",
    ,
    drop = FALSE
  ]
  expect_false(any(
    recursive_only_edges$from_package == "DirectA" &
      recursive_only_edges$relationship == "Suggests"
  ))
})

test_that("selected policy freezes an exact direct-preserving target subset", {
  contracts <- dependency_fixture_contracts()
  direct <- contracts$cohort$targets[
    contracts$cohort$targets$role == "direct",
    ,
    drop = FALSE
  ]
  rownames(direct) <- NULL

  selected <- revdeprunner:::new_dependency_universe(
    contracts$cohort,
    contracts$snapshot,
    "selected",
    dependency_fixture_base_packages(),
    targets = direct
  )

  expect_identical(selected$cohort_policy, "selected")
  expect_identical(selected$targets, direct)
  expect_false("RecursiveOnly" %in% selected$targets$package)
  expect_invisible(
    revdeprunner:::validate_dependency_universe(
      selected,
      contracts$cohort,
      contracts$snapshot
    )
  )

  expect_error(
    revdeprunner:::new_dependency_universe(
      contracts$cohort,
      contracts$snapshot,
      "selected",
      dependency_fixture_base_packages(),
      targets = direct[-1L, , drop = FALSE]
    ),
    "must retain every direct target",
    fixed = TRUE
  )

  changed <- direct
  changed$version[[1L]] <- "999.0"
  expect_error(
    revdeprunner:::new_dependency_universe(
      contracts$cohort,
      contracts$snapshot,
      "selected",
      dependency_fixture_base_packages(),
      targets = changed
    ),
    "must be exact cohort rows",
    fixed = TRUE
  )
})

test_that("repository priority selects dependency versions", {
  repositories <- dependency_fixture_repositories()
  database <- dependency_fixture_database()
  duplicate <- database[database$Package == "HardOne", , drop = FALSE]
  duplicate$Version <- "99.0"
  duplicate$Imports <- "AlternateOnly"
  duplicate$Repository <- repositories[["BioCsoft"]]
  database <- rbind(duplicate, database)

  primary <- dependency_fixture_contracts(database, repositories)
  secondary <- dependency_fixture_contracts(database, rev(repositories))
  primary_universe <- revdeprunner:::new_dependency_universe(
    primary$cohort,
    primary$snapshot,
    "direct",
    dependency_fixture_base_packages()
  )
  secondary_universe <- revdeprunner:::new_dependency_universe(
    secondary$cohort,
    secondary$snapshot,
    "direct",
    dependency_fixture_base_packages()
  )

  selected_version <- function(universe) {
    universe$dependencies$version[
      universe$dependencies$target == "DirectA" &
        universe$dependencies$dependency == "HardOne"
    ]
  }
  expect_identical(selected_version(primary_universe), "9.0")
  expect_identical(selected_version(secondary_universe), "99.0")
  expect_false(any(primary_universe$edges$dependency == "AlternateOnly"))
  expect_true(any(secondary_universe$edges$dependency == "AlternateOnly"))
  expect_false(identical(
    primary_universe$universe_id,
    secondary_universe$universe_id
  ))
})

test_that("universe identities bind normalized base-package membership", {
  contracts <- dependency_fixture_contracts()
  base_packages <- dependency_fixture_base_packages()
  baseline <- revdeprunner:::new_dependency_universe(
    contracts$cohort,
    contracts$snapshot,
    "direct",
    base_packages
  )
  reordered <- revdeprunner:::new_dependency_universe(
    contracts$cohort,
    contracts$snapshot,
    "direct",
    rev(base_packages)
  )
  added <- revdeprunner:::new_dependency_universe(
    contracts$cohort,
    contracts$snapshot,
    "direct",
    c(base_packages, "RecommendedPkg")
  )

  expect_identical(baseline, reordered)
  expect_false(identical(baseline$universe_id, added$universe_id))
})

test_that("dependency universe normalization is locale-independent", {
  database <- dependency_fixture_database()
  database$Package[database$Package == "HardOne"] <- "aHard"
  database$Package[database$Package == "Shared"] <- "BShared"
  database$Depends[database$Package == "DirectA"] <-
    "R (>= 4.3), SubjectPkg, aHard, stats"
  database$Imports[database$Package == "DirectA"] <- "BShared"
  database$Imports[database$Package == "aHard"] <- "DeepHard, DirectA"
  contracts <- dependency_fixture_contracts(database)

  original <- Sys.getlocale("LC_COLLATE")
  on.exit(Sys.setlocale("LC_COLLATE", original), add = TRUE)
  candidates <- unique(c("C", "C.UTF-8", original))
  available <- character()
  for (candidate in candidates) {
    selected <- suppressWarnings(Sys.setlocale("LC_COLLATE", candidate))
    if (!is.na(selected) && !selected %in% available) {
      available <- c(available, selected)
    }
  }
  expect_gte(length(available), 2L)

  universes <- lapply(
    available,
    function(locale) {
      expect_false(is.na(Sys.setlocale("LC_COLLATE", locale)))
      revdeprunner:::new_dependency_universe(
        contracts$cohort,
        contracts$snapshot,
        "direct",
        rev(c("zBase", "ABase"))
      )
    }
  )
  expect_true(all(vapply(
    universes,
    identical,
    logical(1L),
    universes[[1L]]
  )))
  expect_identical(universes[[1L]]$base_packages, c("ABase", "zBase"))

  for (locale in available) {
    expect_false(is.na(Sys.setlocale("LC_COLLATE", locale)))
    expect_invisible(
      revdeprunner:::validate_dependency_universe(
        universes[[1L]],
        contracts$cohort,
        contracts$snapshot
      )
    )
  }
})

test_that("dependency entries require complete valid constraint grammar", {
  expect_identical(
    revdeprunner:::parse_stock_dependency_field(
      paste(
        "PlainPkg",
        "R (>= 4.3)",
        "OpGE(>= 1.0)",
        "OpLE (<= 2.0)",
        "OpEQ (== 3.0)",
        "OpNE (!= 4.0)",
        "OpGT (> 1.0-1)",
        "OpLT (< 9.0)",
        "R (>= r123)",
        sep = ", "
      ),
      "Imports"
    ),
    c(
      "PlainPkg",
      "R",
      "OpGE",
      "OpLE",
      "OpEQ",
      "OpNE",
      "OpGT",
      "OpLT"
    )
  )

  expect_identical(
    revdeprunner:::parse_stock_dependency_field(
      "PlainPkg, R (>= 4.3),\n",
      "Imports"
    ),
    c("PlainPkg", "R")
  )

  invalid_entries <- c(
    "DepPkg (banana)",
    "DepPkg (>= banana)",
    "DepPkg (=> 1.0)",
    "DepPkg (>= 1.0)(>= 2.0)",
    "DepPkg (>= 1.0) garbage",
    "DepPkg ( >= 1.0)",
    "DepPkg (>= 1.0 )"
  )
  for (entry in invalid_entries) {
    database <- dependency_fixture_database()
    database$Imports[database$Package == "DirectA"] <- entry
    contracts <- dependency_fixture_contracts(database)
    expect_error(
      revdeprunner:::new_dependency_universe(
        contracts$cohort,
        contracts$snapshot,
        "direct",
        dependency_fixture_base_packages()
      ),
      "malformed dependency syntax",
      fixed = TRUE,
      info = entry
    )
  }
})

test_that("constructors reject unsupported or malformed inputs", {
  contracts <- dependency_fixture_contracts()

  expect_error(
    revdeprunner:::new_dependency_universe(
      contracts$cohort,
      contracts$snapshot,
      "all",
      dependency_fixture_base_packages()
    ),
    "must be `direct`, `recursive-strong`, or `selected`",
    fixed = TRUE
  )
  expect_error(
    revdeprunner:::new_dependency_universe(
      contracts$cohort,
      contracts$snapshot,
      "direct",
      character()
    ),
    "non-empty set",
    fixed = TRUE
  )
  expect_error(
    revdeprunner:::new_dependency_universe(
      contracts$cohort,
      contracts$snapshot,
      "direct",
      c("stats", "stats")
    ),
    "unique package names",
    fixed = TRUE
  )

  malformed <- dependency_fixture_database()
  malformed$Imports[malformed$Package == "DirectA"] <- "Shared,,"
  malformed_contracts <- dependency_fixture_contracts(malformed)
  expect_error(
    revdeprunner:::new_dependency_universe(
      malformed_contracts$cohort,
      malformed_contracts$snapshot,
      "direct",
      dependency_fixture_base_packages()
    ),
    "malformed dependency syntax",
    fixed = TRUE
  )

  other_snapshot <- revdeprunner:::new_repository_snapshot(
    rev(dependency_fixture_repositories()),
    dependency_fixture_database()
  )
  expect_error(
    revdeprunner:::new_dependency_universe(
      contracts$cohort,
      other_snapshot,
      "direct",
      dependency_fixture_base_packages()
    ),
    "does not belong",
    fixed = TRUE
  )
})

test_that("validation detects structural, semantic, and identity mutation", {
  contracts <- dependency_fixture_contracts()
  universe <- revdeprunner:::new_dependency_universe(
    contracts$cohort,
    contracts$snapshot,
    "direct",
    dependency_fixture_base_packages()
  )
  validate <- function(value) {
    revdeprunner:::validate_dependency_universe(
      value,
      contracts$cohort,
      contracts$snapshot
    )
  }

  extra <- universe
  extra$unexpected <- "field"
  expect_error(validate(extra), "invalid structure", fixed = TRUE)

  changed_fields <- universe
  changed_fields$first_level_fields[[4L]] <- "Enhances"
  expect_error(changed_fields |> validate(), "unsupported", fixed = TRUE)

  changed_disposition <- universe
  install_index <- which(
    changed_disposition$dependencies$disposition == "install"
  )[[1L]]
  changed_disposition$dependencies$disposition[[install_index]] <- "unavailable"
  expect_error(
    changed_disposition |> validate(),
    "dispositions do not match",
    fixed = TRUE
  )

  changed_edge <- universe
  changed_edge$edges$relationship[[1L]] <- "Suggests"
  expect_error(changed_edge |> validate(), "edges do not match", fixed = TRUE)

  denormalized <- universe
  denormalized$base_packages <- rev(denormalized$base_packages)
  expect_error(denormalized |> validate(), "not normalized", fixed = TRUE)

  changed_identity <- universe
  changed_identity$universe_id <- paste0(
    "sha256:",
    paste(rep("0", 64L), collapse = "")
  )
  expect_error(
    changed_identity |> validate(),
    "identity does not match",
    fixed = TRUE
  )
})

test_that("empty cohorts produce explicit empty universe tables", {
  snapshot <- revdeprunner:::new_repository_snapshot(
    dependency_fixture_repositories(),
    dependency_fixture_database()
  )
  cohort <- revdeprunner:::new_reverse_dependency_cohort(
    "NoConsumers",
    snapshot
  )
  universe <- revdeprunner:::new_dependency_universe(
    cohort,
    snapshot,
    "recursive-strong",
    dependency_fixture_base_packages()
  )

  expect_identical(universe$targets, cohort$targets)
  expect_identical(
    universe$dependencies,
    revdeprunner:::empty_dependency_dispositions()
  )
  expect_identical(universe$edges, revdeprunner:::empty_dependency_edges())
  expect_invisible(
    revdeprunner:::validate_dependency_universe(universe, cohort, snapshot)
  )
})
