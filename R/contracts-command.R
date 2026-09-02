# This file composes private helpers defined in other package source files.
# nolint start: object_usage_linter.

command_plan_schema_version <- function() {
  "revdeprunner-command-plan/v1"
}

command_exit_catalog_schema_version <- function() {
  "revdeprunner-command-exit-catalog/v1"
}

command_operation_table <- function() {
  data.frame(
    operation = c("inventory", "prepare", "compare", "verify"),
    command_name = c(
      "revdep-runner inventory",
      "revdep-runner prepare",
      "revdep-runner compare",
      "revdep-runner verify"
    ),
    write_scope = c(
      "durable-and-run",
      "durable-and-run",
      "run-only",
      "run-only"
    ),
    requires_snapshot = c("false", "true", "true", "true"),
    requires_report = c("false", "false", "true", "true"),
    stringsAsFactors = FALSE
  )
}

command_exit_state_table <- function() {
  all_operations <- "inventory,prepare,compare,verify"
  data.frame(
    state = c(
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
    ),
    exit_code = c(
      "0",
      "0",
      "2",
      "3",
      "20",
      "21",
      "22",
      "23",
      "24",
      "70",
      "130"
    ),
    classification = c(
      "success",
      "success",
      "caller-error",
      "precondition-error",
      "inventory-failure",
      "preparation-failure",
      "comparison-finding",
      "comparison-failure",
      "verification-failure",
      "internal-error",
      "interruption"
    ),
    applies_to = c(
      all_operations,
      all_operations,
      all_operations,
      all_operations,
      "inventory",
      "prepare,compare",
      "compare",
      "compare",
      "verify",
      all_operations,
      all_operations
    ),
    stringsAsFactors = FALSE
  )
}

new_command_exit_catalog <- function() {
  schema_version <- command_exit_catalog_schema_version()
  states <- command_exit_state_table()
  catalog <- structure(
    list(
      schema_version = schema_version,
      exit_catalog_id = record_identity(
        schema_version,
        tabular_identity_fields("exit", states)
      ),
      states = states
    ),
    class = "revdeprunner_command_exit_catalog"
  )
  validate_command_exit_catalog(catalog)
  catalog
}

validate_command_exit_catalog <- function(catalog) {
  validate_composite_contract_record(
    catalog,
    c("schema_version", "exit_catalog_id", "states"),
    "revdeprunner_command_exit_catalog",
    "command exit catalog"
  )
  if (
    !identical(
      catalog$schema_version,
      command_exit_catalog_schema_version()
    )
  ) {
    stop("Command exit catalog schema version is unsupported.", call. = FALSE)
  }
  validate_sha256_identity(catalog$exit_catalog_id, "exit_catalog_id")
  expected_states <- command_exit_state_table()
  if (!identical(catalog$states, expected_states)) {
    stop("Command exit catalog states do not match version 1.", call. = FALSE)
  }

  expected_id <- record_identity(
    catalog$schema_version,
    tabular_identity_fields("exit", catalog$states)
  )
  if (!identical(catalog$exit_catalog_id, expected_id)) {
    stop(
      "Command exit catalog identity does not match its states.",
      call. = FALSE
    )
  }

  invisible(catalog)
}

new_command_plan <- function(
  operation,
  path_plan,
  r_executable,
  dry_run,
  snapshot = NULL,
  cohort = NULL,
  universe = NULL,
  lane = NULL,
  preparation_report = NULL
) {
  operation <- validate_command_operation(operation)
  validate_runtime_root_plan(path_plan)
  r_executable <- normalize_command_r_executable(r_executable)
  dry_run <- normalize_command_dry_run(dry_run)
  bindings <- command_binding_ids(
    operation,
    snapshot,
    cohort,
    universe,
    lane,
    preparation_report
  )
  specification <- command_operation_specification(operation)
  exit_catalog <- new_command_exit_catalog()
  schema_version <- command_plan_schema_version()
  fields <- command_plan_identity_fields(
    operation,
    specification$command_name,
    specification$write_scope,
    dry_run,
    r_executable,
    path_plan$path_plan_id,
    bindings,
    exit_catalog$exit_catalog_id
  )
  plan <- structure(
    list(
      schema_version = schema_version,
      command_plan_id = record_identity(schema_version, fields),
      operation = operation,
      command_name = specification$command_name,
      write_scope = specification$write_scope,
      dry_run = dry_run,
      r_executable = r_executable,
      path_plan_id = path_plan$path_plan_id,
      snapshot_id = bindings$snapshot_id,
      cohort_id = bindings$cohort_id,
      universe_id = bindings$universe_id,
      cohort_policy = bindings$cohort_policy,
      lane_id = bindings$lane_id,
      preparation_report_id = bindings$preparation_report_id,
      exit_catalog_id = exit_catalog$exit_catalog_id
    ),
    class = "revdeprunner_command_plan"
  )
  plan
}

validate_command_plan <- function(
  plan,
  path_plan,
  snapshot = NULL,
  cohort = NULL,
  universe = NULL,
  lane = NULL,
  preparation_report = NULL
) {
  fields <- c(
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
  validate_contract_record(
    plan,
    fields,
    "revdeprunner_command_plan",
    "command plan",
    allow_na = c(
      "snapshot_id",
      "cohort_id",
      "universe_id",
      "cohort_policy",
      "lane_id",
      "preparation_report_id"
    )
  )
  if (!identical(plan$schema_version, command_plan_schema_version())) {
    stop("Command plan schema version is unsupported.", call. = FALSE)
  }
  validate_sha256_identity(plan$command_plan_id, "command_plan_id")

  operation <- validate_command_operation(plan$operation)
  specification <- command_operation_specification(operation)
  if (!identical(plan$command_name, specification$command_name)) {
    stop("Command plan name does not match its operation.", call. = FALSE)
  }
  if (!identical(plan$write_scope, specification$write_scope)) {
    stop(
      "Command plan write scope does not match its operation.",
      call. = FALSE
    )
  }
  dry_run <- validate_normalized_command_dry_run(plan$dry_run)
  r_executable <- normalize_command_r_executable(plan$r_executable)
  if (!identical(plan$r_executable, r_executable)) {
    stop(
      "Command plan R executable is not a resolved physical path.",
      call. = FALSE
    )
  }

  validate_runtime_root_plan(path_plan)
  if (!identical(plan$path_plan_id, path_plan$path_plan_id)) {
    stop(
      "Command plan does not belong to this runtime-root plan.",
      call. = FALSE
    )
  }
  bindings <- command_binding_ids(
    operation,
    snapshot,
    cohort,
    universe,
    lane,
    preparation_report
  )
  binding_fields <- names(bindings)
  for (field in binding_fields) {
    if (!identical(plan[[field]], bindings[[field]])) {
      stop(
        "Command plan manifest bindings do not match its operation.",
        call. = FALSE
      )
    }
  }

  exit_catalog <- new_command_exit_catalog()
  if (!identical(plan$exit_catalog_id, exit_catalog$exit_catalog_id)) {
    stop("Command plan exit catalog is unsupported.", call. = FALSE)
  }
  identity_fields <- command_plan_identity_fields(
    operation,
    specification$command_name,
    specification$write_scope,
    dry_run,
    r_executable,
    path_plan$path_plan_id,
    bindings,
    exit_catalog$exit_catalog_id
  )
  expected_id <- record_identity(plan$schema_version, identity_fields)
  if (!identical(plan$command_plan_id, expected_id)) {
    stop("Command plan identity does not match its fields.", call. = FALSE)
  }

  invisible(plan)
}

validate_command_operation <- function(operation) {
  operation <- validate_contract_text(operation, "operation")
  if (!operation %in% command_operation_table()$operation) {
    stop("`operation` is not a supported command operation.", call. = FALSE)
  }

  operation
}

command_operation_specification <- function(operation) {
  operation <- validate_command_operation(operation)
  specifications <- command_operation_table()
  specification <- specifications[
    specifications$operation == operation,
    ,
    drop = FALSE
  ]
  if (nrow(specification) != 1L) {
    stop("Command operation specification is ambiguous.", call. = FALSE)
  }

  as.list(specification[1L, , drop = TRUE])
}

normalize_command_dry_run <- function(dry_run) {
  if (!is.logical(dry_run) || length(dry_run) != 1L || is.na(dry_run)) {
    stop("`dry_run` must be one explicit logical value.", call. = FALSE)
  }

  if (dry_run) "true" else "false"
}

validate_normalized_command_dry_run <- function(dry_run) {
  dry_run <- validate_contract_text(dry_run, "dry_run")
  if (!dry_run %in% c("true", "false")) {
    stop("Command plan dry-run value is not normalized.", call. = FALSE)
  }

  dry_run
}

normalize_command_r_executable <- function(path) {
  path <- validate_contract_text(path, "r_executable")
  expanded <- path.expand(path)
  if (
    !file.exists(expanded) ||
      dir.exists(expanded) ||
      !utils::file_test("-f", expanded)
  ) {
    stop("`r_executable` must identify an existing file.", call. = FALSE)
  }
  normalized <- normalizePath(expanded, winslash = "/", mustWork = TRUE)
  if (!runtime_path_is_absolute(normalized) || grepl("\\\\", normalized)) {
    stop("`r_executable` must resolve to an absolute path.", call. = FALSE)
  }

  normalized
}

command_binding_ids <- function(
  operation,
  snapshot,
  cohort,
  universe,
  lane,
  preparation_report
) {
  specification <- command_operation_specification(operation)
  supplied <- list(
    snapshot = snapshot,
    cohort = cohort,
    universe = universe,
    lane = lane
  )
  requires_snapshot <- identical(specification$requires_snapshot, "true")
  if (requires_snapshot && any(vapply(supplied, is.null, logical(1L)))) {
    stop(
      "This command operation requires snapshot, cohort, universe, and lane bindings.",
      call. = FALSE
    )
  }
  if (!requires_snapshot && any(!vapply(supplied, is.null, logical(1L)))) {
    stop("This command operation forbids manifest bindings.", call. = FALSE)
  }
  requires_report <- identical(specification$requires_report, "true")
  if (requires_report && is.null(preparation_report)) {
    stop("This command operation requires a preparation report.", call. = FALSE)
  }
  if (!requires_report && !is.null(preparation_report)) {
    stop("This command operation forbids a preparation report.", call. = FALSE)
  }

  if (!requires_snapshot) {
    return(command_empty_binding_ids())
  }

  validate_repository_snapshot(snapshot)
  validate_reverse_dependency_cohort(cohort, snapshot)
  validate_dependency_universe(universe, cohort, snapshot)
  validate_compatibility_lane(lane)
  if (requires_report) {
    validate_preparation_report(
      preparation_report,
      universe,
      cohort,
      snapshot,
      lane
    )
  }

  list(
    snapshot_id = snapshot$snapshot_id,
    cohort_id = cohort$cohort_id,
    universe_id = universe$universe_id,
    cohort_policy = universe$cohort_policy,
    lane_id = lane$lane_id,
    preparation_report_id = if (requires_report) {
      preparation_report$report_id
    } else {
      NA_character_
    }
  )
}

command_empty_binding_ids <- function() {
  list(
    snapshot_id = NA_character_,
    cohort_id = NA_character_,
    universe_id = NA_character_,
    cohort_policy = NA_character_,
    lane_id = NA_character_,
    preparation_report_id = NA_character_
  )
}

command_plan_identity_fields <- function(
  operation,
  command_name,
  write_scope,
  dry_run,
  r_executable,
  path_plan_id,
  bindings,
  exit_catalog_id
) {
  c(
    operation = operation,
    command_name = command_name,
    write_scope = write_scope,
    dry_run = dry_run,
    r_executable = r_executable,
    path_plan_id = path_plan_id,
    unlist(bindings, use.names = TRUE),
    exit_catalog_id = exit_catalog_id
  )
}

# nolint end
