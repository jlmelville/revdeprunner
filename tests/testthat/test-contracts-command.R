# These internal tests protect the command boundary that future entry points
# must validate before they perform any work.

make_command_contract_fixture <- function() {
  root <- tempfile("command-contract-")
  dir.create(root)
  paths <- file.path(
    root,
    c("package", "data", "runs", "cache", "bin", "outside")
  )
  invisible(lapply(paths, dir.create))
  writeLines(
    c("Package: fixture", "Version: 1.0.0"),
    file.path(paths[[1L]], "DESCRIPTION")
  )
  r_executable <- file.path(paths[[5L]], "R-fixture")
  other_r_executable <- file.path(paths[[5L]], "R-other")
  writeLines("fixture R executable", r_executable)
  writeLines("other fixture R executable", other_r_executable)

  list(
    root = root,
    package = paths[[1L]],
    data = paths[[2L]],
    runs = paths[[3L]],
    cache = paths[[4L]],
    bin = paths[[5L]],
    outside = paths[[6L]],
    r_executable = r_executable,
    other_r_executable = other_r_executable
  )
}

command_fixture_empty_table <- function(fields) {
  values <- stats::setNames(
    replicate(length(fields), character(), simplify = FALSE),
    fields
  )
  as.data.frame(values, stringsAsFactors = FALSE, optional = TRUE)
}

command_fixture_contracts <- function(
  fixture,
  cohort_policy = "direct",
  subject_version = "1.0.0"
) {
  repositories <- c(CRAN = "https://example.test/cran/src/contrib")
  database <- data.frame(
    Package = "SubjectPkg",
    Version = subject_version,
    Depends = NA_character_,
    Imports = NA_character_,
    LinkingTo = NA_character_,
    Suggests = NA_character_,
    NeedsCompilation = "no",
    SystemRequirements = NA_character_,
    Repository = unname(repositories),
    stringsAsFactors = FALSE
  )
  snapshot <- revdeprunner:::new_repository_snapshot(repositories, database)
  cohort <- revdeprunner:::new_reverse_dependency_cohort(
    "SubjectPkg",
    snapshot
  )
  universe <- revdeprunner:::new_dependency_universe(
    cohort,
    snapshot,
    cohort_policy,
    c("base", "methods", "stats", "utils")
  )
  lane <- revdeprunner:::new_compatibility_lane(
    r_major_minor = "4.5",
    r_platform = "x86_64-pc-linux-gnu",
    architecture = "x86_64",
    os_abi = "linux-glibc-2.39",
    toolchain_tag = "gcc-15.2.1"
  )
  report <- revdeprunner:::new_preparation_report(
    universe,
    cohort,
    snapshot,
    lane,
    artifacts = list(),
    sources = command_fixture_empty_table(
      revdeprunner:::preparation_source_fields()
    ),
    attempts = list(),
    results = command_fixture_empty_table(
      revdeprunner:::preparation_result_fields()
    )
  )
  path_plan <- revdeprunner:::new_runtime_root_plan(
    package_root = fixture$package,
    data_root = fixture$data,
    runs_root = fixture$runs,
    run_id = "run-20260829-command",
    source_cache_roots = fixture$cache
  )

  list(
    snapshot = snapshot,
    cohort = cohort,
    universe = universe,
    lane = lane,
    report = report,
    path_plan = path_plan
  )
}

new_fixture_command_plan <- function(
  fixture,
  contracts,
  operation,
  dry_run = FALSE,
  r_executable = fixture$r_executable
) {
  arguments <- list(
    operation = operation,
    path_plan = contracts$path_plan,
    r_executable = r_executable,
    dry_run = dry_run
  )
  if (!identical(operation, "inventory")) {
    arguments <- c(
      arguments,
      contracts[c("snapshot", "cohort", "universe", "lane")]
    )
  }
  if (operation %in% c("compare", "verify")) {
    arguments$preparation_report <- contracts$report
  }
  do.call(revdeprunner:::new_command_plan, arguments)
}

validate_fixture_command_plan <- function(plan, contracts) {
  arguments <- list(plan = plan, path_plan = contracts$path_plan)
  if (!identical(plan$operation, "inventory")) {
    arguments <- c(
      arguments,
      contracts[c("snapshot", "cohort", "universe", "lane")]
    )
  }
  if (plan$operation %in% c("compare", "verify")) {
    arguments$preparation_report <- contracts$report
  }
  do.call(revdeprunner:::validate_command_plan, arguments)
}

test_that("command plans freeze exact operation bindings", {
  fixture <- make_command_contract_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  contracts <- command_fixture_contracts(fixture)
  before <- snapshot_test_cache(fixture$root)

  operations <- c("inventory", "prepare", "compare", "verify")
  plans <- lapply(
    operations,
    new_fixture_command_plan,
    fixture = fixture,
    contracts = contracts
  )
  names(plans) <- operations
  repeated <- new_fixture_command_plan(fixture, contracts, "compare")

  expect_s3_class(plans$compare, "revdeprunner_command_plan")
  expect_identical(plans$compare, repeated)
  expect_identical(snapshot_test_cache(fixture$root), before)
  expect_identical(
    names(plans$compare),
    c(
      "schema_version",
      "command_plan_id",
      "operation",
      "command_name",
      "write_scope",
      "dry_run",
      "r_executable",
      "path_plan_id",
      "snapshot_id",
      "cohort_id",
      "universe_id",
      "cohort_policy",
      "lane_id",
      "preparation_report_id",
      "exit_catalog_id"
    )
  )
  expect_identical(plans$compare$schema_version, "revdeprunner-command-plan/v1")
  expect_identical(
    vapply(plans, `[[`, character(1L), "command_name"),
    stats::setNames(paste("revdep-runner", operations), operations)
  )
  expect_identical(
    vapply(plans, `[[`, character(1L), "write_scope"),
    c(
      inventory = "durable-and-run",
      prepare = "durable-and-run",
      compare = "run-only",
      verify = "run-only"
    )
  )
  expect_true(all(vapply(
    plans,
    function(plan)
      identical(plan$path_plan_id, contracts$path_plan$path_plan_id),
    logical(1L)
  )))
  expect_true(all(vapply(
    plans,
    function(plan) identical(plan$dry_run, "false"),
    logical(1L)
  )))
  expect_true(all(vapply(
    plans,
    function(plan)
      identical(plan$exit_catalog_id, plans$inventory$exit_catalog_id),
    logical(1L)
  )))
  expect_true(all(is.na(unlist(plans$inventory[9:14], use.names = FALSE))))
  expect_true(all(!is.na(unlist(plans$prepare[9:13], use.names = FALSE))))
  expect_true(is.na(plans$prepare$preparation_report_id))
  expect_identical(
    plans$compare$preparation_report_id,
    contracts$report$report_id
  )
  expect_identical(
    plans$verify$preparation_report_id,
    contracts$report$report_id
  )
  invisible(lapply(plans, validate_fixture_command_plan, contracts = contracts))
})

test_that("command construction performs one deep binding-validation pass", {
  fixture <- make_command_contract_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  contracts <- command_fixture_contracts(fixture)
  original <- revdeprunner:::command_binding_ids
  binding_calls <- 0L

  # This private counter protects a material performance-path invariant that
  # cannot be observed through the small public facade fixtures.
  testthat::local_mocked_bindings(
    command_binding_ids = function(...) {
      binding_calls <<- binding_calls + 1L
      original(...)
    },
    .package = "revdeprunner"
  )

  plan <- new_fixture_command_plan(fixture, contracts, "compare")
  expect_identical(binding_calls, 1L)

  expect_invisible(validate_fixture_command_plan(plan, contracts))
  expect_identical(binding_calls, 2L)
})

test_that("dry-run intent and cohort policy affect command identity", {
  fixture <- make_command_contract_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  direct <- command_fixture_contracts(fixture, "direct")
  recursive <- command_fixture_contracts(fixture, "recursive-strong")

  ordinary <- new_fixture_command_plan(fixture, direct, "prepare")
  dry <- new_fixture_command_plan(fixture, direct, "prepare", dry_run = TRUE)
  recursive_plan <- new_fixture_command_plan(fixture, recursive, "prepare")
  other_r <- new_fixture_command_plan(
    fixture,
    direct,
    "prepare",
    r_executable = fixture$other_r_executable
  )

  expect_identical(dry$dry_run, "true")
  expect_identical(ordinary$cohort_policy, "direct")
  expect_identical(recursive_plan$cohort_policy, "recursive-strong")
  ids <- vapply(
    list(ordinary, dry, recursive_plan, other_r),
    `[[`,
    character(1L),
    "command_plan_id"
  )
  expect_length(unique(ids), 4L)
  for (value in list(NA, 1L, "true", logical())) {
    expect_error(
      revdeprunner:::new_command_plan(
        "inventory",
        direct$path_plan,
        fixture$r_executable,
        value
      ),
      "explicit logical",
      fixed = TRUE
    )
  }
})

test_that("operations reject missing and surplus bindings", {
  fixture <- make_command_contract_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  contracts <- command_fixture_contracts(fixture)
  common <- list(
    path_plan = contracts$path_plan,
    r_executable = fixture$r_executable,
    dry_run = FALSE
  )

  expect_error(
    do.call(
      revdeprunner:::new_command_plan,
      c(list(operation = "inventory", snapshot = contracts$snapshot), common)
    ),
    "forbids manifest bindings",
    fixed = TRUE
  )
  expect_error(
    do.call(
      revdeprunner:::new_command_plan,
      c(
        list(
          operation = "prepare",
          snapshot = contracts$snapshot,
          cohort = contracts$cohort,
          universe = contracts$universe
        ),
        common
      )
    ),
    "requires snapshot, cohort, universe, and lane",
    fixed = TRUE
  )
  expect_error(
    do.call(
      revdeprunner:::new_command_plan,
      c(
        list(
          operation = "prepare",
          snapshot = contracts$snapshot,
          cohort = contracts$cohort,
          universe = contracts$universe,
          lane = contracts$lane,
          preparation_report = contracts$report
        ),
        common
      )
    ),
    "forbids a preparation report",
    fixed = TRUE
  )
  for (operation in c("compare", "verify")) {
    expect_error(
      do.call(
        revdeprunner:::new_command_plan,
        c(
          list(
            operation = operation,
            snapshot = contracts$snapshot,
            cohort = contracts$cohort,
            universe = contracts$universe,
            lane = contracts$lane
          ),
          common
        )
      ),
      "requires a preparation report",
      fixed = TRUE
    )
  }
  expect_error(
    revdeprunner:::new_command_plan(
      "unsupported",
      contracts$path_plan,
      fixture$r_executable,
      FALSE
    ),
    "supported command operation",
    fixed = TRUE
  )

  other <- command_fixture_contracts(
    fixture,
    "recursive-strong",
    subject_version = "2.0.0"
  )
  expect_error(
    revdeprunner:::new_command_plan(
      "prepare",
      contracts$path_plan,
      fixture$r_executable,
      FALSE,
      snapshot = other$snapshot,
      cohort = other$cohort,
      universe = contracts$universe,
      lane = contracts$lane
    ),
    "does not belong to this snapshot",
    fixed = TRUE
  )
})

test_that("R executable locators resolve physically and are revalidated", {
  fixture <- make_command_contract_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  contracts <- command_fixture_contracts(fixture)
  plan <- new_fixture_command_plan(fixture, contracts, "inventory")
  expect_identical(plan$r_executable, normalizePath(fixture$r_executable))

  alias <- file.path(fixture$bin, "R-alias")
  linked <- file.symlink(fixture$r_executable, alias)
  if (isTRUE(linked)) {
    aliased <- new_fixture_command_plan(
      fixture,
      contracts,
      "inventory",
      r_executable = alias
    )
    expect_identical(aliased, plan)
  } else {
    succeed()
  }
  expect_error(
    new_fixture_command_plan(
      fixture,
      contracts,
      "inventory",
      r_executable = file.path(fixture$root, "missing-R")
    ),
    "existing file",
    fixed = TRUE
  )
  expect_error(
    new_fixture_command_plan(
      fixture,
      contracts,
      "inventory",
      r_executable = fixture$bin
    ),
    "existing file",
    fixed = TRUE
  )

  unlink(fixture$r_executable)
  expect_error(
    validate_fixture_command_plan(plan, contracts),
    "existing file",
    fixed = TRUE
  )
})

test_that("typed exit states have one exact versioned catalog", {
  catalog <- revdeprunner:::new_command_exit_catalog()
  repeated <- revdeprunner:::new_command_exit_catalog()

  expect_s3_class(catalog, "revdeprunner_command_exit_catalog")
  expect_identical(catalog, repeated)
  expect_identical(
    names(catalog),
    c("schema_version", "exit_catalog_id", "states")
  )
  expect_identical(
    catalog$schema_version,
    "revdeprunner-command-exit-catalog/v1"
  )
  expect_identical(
    names(catalog$states),
    c("state", "exit_code", "classification", "applies_to")
  )
  expect_identical(
    catalog$states$state,
    c(
      "success",
      "plan-ready",
      "invalid-invocation",
      "precondition-failed",
      "inventory-incomplete",
      "preparation-incomplete",
      "comparison-changes",
      "comparison-incomplete",
      "verification-failed",
      "internal-error",
      "interrupted"
    )
  )
  expect_identical(
    catalog$states$exit_code,
    c("0", "0", "2", "3", "20", "21", "22", "23", "24", "70", "130")
  )
  expect_identical(
    catalog$states$applies_to,
    c(
      rep("inventory,prepare,compare,verify", 4L),
      "inventory",
      "prepare,compare",
      "compare",
      "compare",
      "verify",
      "inventory,prepare,compare,verify",
      "inventory,prepare,compare,verify"
    )
  )
  expect_invisible(revdeprunner:::validate_command_exit_catalog(catalog))

  for (field in names(catalog$states)) {
    changed <- catalog
    changed$states[[field]][[1L]] <- paste0("changed-", field)
    expect_error(
      revdeprunner:::validate_command_exit_catalog(changed),
      "do not match version 1",
      fixed = TRUE,
      info = paste("must reject changed", field)
    )
  }
  changed <- catalog
  changed$exit_catalog_id <- paste0(
    "sha256:",
    paste(rep("0", 64L), collapse = "")
  )
  expect_error(
    revdeprunner:::validate_command_exit_catalog(changed),
    "identity does not match",
    fixed = TRUE
  )
  changed <- catalog[-length(catalog)]
  class(changed) <- class(catalog)
  expect_error(
    revdeprunner:::validate_command_exit_catalog(changed),
    "invalid structure",
    fixed = TRUE
  )
})

test_that("command validation rejects structure semantics and identity mutation", {
  fixture <- make_command_contract_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  contracts <- command_fixture_contracts(fixture)
  plan <- new_fixture_command_plan(fixture, contracts, "compare")
  validate <- function(value) validate_fixture_command_plan(value, contracts)

  changed <- plan[-length(plan)]
  class(changed) <- class(plan)
  expect_error(validate(changed), "invalid structure", fixed = TRUE)
  changed <- plan
  changed$extra <- "field"
  expect_error(validate(changed), "invalid structure", fixed = TRUE)
  expect_error(validate(unclass(plan)), "invalid structure", fixed = TRUE)

  changes <- list(
    schema_version = "revdeprunner-command-plan/v2",
    operation = "verify",
    command_name = "revdep-runner verify",
    write_scope = "durable-and-run",
    dry_run = "TRUE",
    path_plan_id = paste0("sha256:", paste(rep("0", 64L), collapse = "")),
    snapshot_id = NA_character_,
    exit_catalog_id = paste0("sha256:", paste(rep("1", 64L), collapse = "")),
    command_plan_id = paste0("sha256:", paste(rep("2", 64L), collapse = ""))
  )
  patterns <- c(
    schema_version = "unsupported",
    operation = "name does not match",
    command_name = "name does not match",
    write_scope = "write scope",
    dry_run = "not normalized",
    path_plan_id = "runtime-root plan",
    snapshot_id = "manifest bindings",
    exit_catalog_id = "exit catalog",
    command_plan_id = "identity does not match"
  )
  for (field in names(changes)) {
    changed <- plan
    changed[[field]] <- changes[[field]]
    expect_error(
      validate(changed),
      patterns[[field]],
      fixed = TRUE,
      info = paste("must reject changed", field)
    )
  }
})

test_that("command identities are locale-independent", {
  fixture <- make_command_contract_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  contracts <- command_fixture_contracts(fixture)
  baseline <- new_fixture_command_plan(fixture, contracts, "compare")

  original_locale <- Sys.getlocale("LC_COLLATE")
  on.exit(
    suppressWarnings(Sys.setlocale("LC_COLLATE", original_locale)),
    add = TRUE
  )
  plans <- list()
  for (locale in c("C", "C.UTF-8")) {
    selected <- suppressWarnings(Sys.setlocale("LC_COLLATE", locale))
    if (!is.na(selected) && nzchar(selected)) {
      plans[[selected]] <- new_fixture_command_plan(
        fixture,
        contracts,
        "compare"
      )
      validate_fixture_command_plan(plans[[selected]], contracts)
    }
  }
  expect_gte(length(plans), 1L)
  expect_true(all(vapply(plans, identical, logical(1L), y = baseline)))
})
