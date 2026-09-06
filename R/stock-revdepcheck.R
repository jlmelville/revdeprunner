initialize_stock_revdepcheck <- function(
  preparation_gate,
  context,
  baseline_source,
  exclude_targets = character(),
  source_archives = character(),
  workspace = "stock-revdepcheck"
) {
  require_linux_revdep_runner()
  require_stock_adapter_tools()
  validate_preparation_gate(preparation_gate, context)
  require_prepared_stock_report(preparation_gate$report, context)
  r_executable <- normalize_r_executable(context$r_executable)
  selected_targets <- context$universe$targets
  exclude_targets <- normalize_stock_adapter_exclusions(
    exclude_targets,
    selected_targets$package
  )
  requested_targets <- selected_targets[
    !selected_targets$package %in% exclude_targets,
    ,
    drop = FALSE
  ]
  rownames(requested_targets) <- NULL
  require_prepared_stock_targets(
    preparation_gate$report,
    requested_targets
  )
  baseline <- validate_baseline_source(
    baseline_source,
    context$cohort,
    context$snapshot
  )
  paths <- create_stock_adapter_paths(context$path_plan, workspace)
  stock_adapter_copy_checkout(
    context$path_plan$package_root,
    paths$checkout
  )
  candidate <- checkout_identity(
    paths$checkout,
    context$cohort$package
  )

  cache_entries_before <- list.files(
    paths$cache,
    all.files = TRUE,
    no.. = TRUE
  )
  discovery <- initialize_stock_database(
    paths$checkout,
    requested_targets$package,
    paths$cache
  )
  if (
    length(cache_entries_before) != 0L ||
      length(list.files(paths$cache, all.files = TRUE, no.. = TRUE)) != 0L
  ) {
    stop("Stock discovery unexpectedly operated on cache state.", call. = FALSE)
  }
  seed_stock_subject_libraries(
    preparation_gate$report,
    context,
    paths
  )

  binary_manifest <- seed_stock_binary_cache(
    preparation_gate,
    context,
    paths$binary_contrib
  )
  source_manifest <- seed_stock_source_cache(
    preparation_gate,
    baseline,
    paths$source_contrib,
    context,
    source_archives
  )
  repository_settings <- initialize_stock_empty_repositories(
    paths$empty_repos,
    context$snapshot
  )
  environment <- stock_adapter_environment(paths, repository_settings)
  runtime <- observe_stock_runtime(
    r_executable,
    requested_targets$package,
    context$cohort$package,
    environment,
    repository_settings,
    paths$temp
  )
  stock_dependencies <- stock_dependencies_from_observation(
    runtime$dependencies,
    requested_targets$package,
    context$universe,
    context$snapshot
  )
  provenance <- runtime$provenance

  initialization <- structure(
    list(
      r_executable = r_executable,
      preparation_report = preparation_gate$report,
      package = context$cohort$package,
      baseline = baseline,
      candidate = candidate,
      selected_targets = selected_targets,
      requested_targets = requested_targets,
      excluded_targets = exclude_targets,
      paths = paths,
      discovery = discovery,
      binary_manifest = binary_manifest,
      source_manifest = source_manifest,
      stock_dependencies = stock_dependencies,
      environment = environment,
      repository_settings = repository_settings,
      provenance = provenance
    ),
    class = "revdeprunner_stock_initialization"
  )
  initialization
}

run_stock_revdepcheck <- function(
  initialization,
  context,
  worker_timeout_seconds = NULL,
  process_timeout_seconds = 7200L,
  verbose = FALSE
) {
  require_linux_revdep_runner()
  require_stock_adapter_tools()
  validate_stock_revdepcheck_initialization(
    initialization,
    context,
    require_pre_worker = FALSE
  )
  worker_timeout_seconds <- stock_adapter_worker_timeout(
    initialization,
    worker_timeout_seconds,
    verbose = verbose
  )
  process_timeout_seconds <- normalize_source_preparation_timeout(
    process_timeout_seconds
  )
  resume_stock_database(initialization)

  process <- run_stock_revdepcheck_process(
    initialization$r_executable,
    initialization$paths,
    initialization$environment,
    initialization$repository_settings,
    worker_timeout_seconds,
    process_timeout_seconds,
    verbose = verbose
  )
  database <- observe_stock_database(initialization$paths$checkout)

  results <- stock_adapter_results(initialization, process, database)
  diagnostics <- stock_adapter_incomplete_diagnostics(
    initialization,
    results,
    context$path_plan
  )
  validate_stock_private_libraries(
    initialization,
    process,
    database
  )
  logs <- stock_adapter_process_logs(initialization$paths, context$path_plan)
  state <- stock_adapter_result_state(process, results)
  result <- structure(
    list(
      initialization = initialization,
      state = state,
      process = process,
      logs = logs,
      database = database,
      results = results,
      diagnostics = diagnostics,
      changes = stock_adapter_changes(initialization$paths$checkout, results)
    ),
    class = "revdeprunner_stock_result"
  )
  validate_stock_revdepcheck_result(result, context)
  result
}

normalize_stock_adapter_exclusions <- function(exclusions, targets) {
  if (
    !is.character(exclusions) || anyNA(exclusions) || anyDuplicated(exclusions)
  ) {
    stop("`exclude_targets` must contain unique package names.", call. = FALSE)
  }
  exclusions <- vapply(
    exclusions,
    validate_package_name,
    character(1L)
  )
  if (any(!exclusions %in% targets)) {
    stop("Stock exclusions must be selected cohort targets.", call. = FALSE)
  }
  sort(unname(exclusions), method = "radix")
}

require_prepared_stock_targets <- function(report, requested_targets) {
  for (row in seq_len(nrow(requested_targets))) {
    target <- requested_targets[row, , drop = FALSE]
    result <- report$results[
      report$results$package == target$package,
      ,
      drop = FALSE
    ]
    if (
      nrow(result) != 1L ||
        !identical(result$version[[1L]], target$version[[1L]]) ||
        !identical(result$outcome[[1L]], "prepared")
    ) {
      stop(
        "Every requested stock target must have one exact prepared result.",
        call. = FALSE
      )
    }
  }
  invisible(report)
}

require_prepared_stock_report <- function(report, context) {
  bindings <- c(
    snapshot_id = context$snapshot$snapshot_id,
    cohort_id = context$cohort$cohort_id,
    universe_id = context$universe$universe_id,
    lane_id = context$lane$lane_id
  )
  if (
    !inherits(report, "revdeprunner_preparation_report") ||
      !identical(
        unlist(report[names(bindings)], use.names = TRUE),
        bindings
      ) ||
      nrow(report$results) == 0L ||
      any(report$results$outcome != "prepared") ||
      anyNA(report$results$artifact_id)
  ) {
    stop(
      "Stock initialization requires a completed preparation report.",
      call. = FALSE
    )
  }
  invisible(report)
}

validate_stock_revdepcheck_initialization <- function(
  initialization,
  context,
  require_pre_worker = TRUE
) {
  fields <- c(
    "r_executable",
    "preparation_report",
    "package",
    "baseline",
    "candidate",
    "selected_targets",
    "requested_targets",
    "excluded_targets",
    "paths",
    "discovery",
    "binary_manifest",
    "source_manifest",
    "stock_dependencies",
    "environment",
    "repository_settings",
    "provenance"
  )
  if (
    !inherits(initialization, "revdeprunner_stock_initialization") ||
      !is.list(initialization) ||
      !identical(names(initialization), fields)
  ) {
    stop(
      "Stock adapter initialization has an invalid structure.",
      call. = FALSE
    )
  }
  if (
    !is.logical(require_pre_worker) ||
      length(require_pre_worker) != 1L ||
      is.na(require_pre_worker)
  ) {
    stop("Stock initialization validation mode is invalid.", call. = FALSE)
  }
  require_prepared_stock_report(initialization$preparation_report, context)
  r_executable <- normalize_r_executable(initialization$r_executable)
  if (
    !identical(initialization$r_executable, r_executable) ||
      !identical(initialization$r_executable, context$r_executable) ||
      !identical(initialization$package, context$cohort$package) ||
      !identical(initialization$selected_targets, context$universe$targets)
  ) {
    stop("Stock adapter contract bindings are inconsistent.", call. = FALSE)
  }
  exclusions <- normalize_stock_adapter_exclusions(
    initialization$excluded_targets,
    initialization$selected_targets$package
  )
  requested <- initialization$selected_targets[
    !initialization$selected_targets$package %in% exclusions,
    ,
    drop = FALSE
  ]
  rownames(requested) <- NULL
  if (
    nrow(requested) == 0L ||
      !identical(initialization$excluded_targets, exclusions) ||
      !identical(initialization$requested_targets, requested)
  ) {
    stop(
      "Stock requested and excluded targets are inconsistent.",
      call. = FALSE
    )
  }
  require_prepared_stock_targets(
    initialization$preparation_report,
    initialization$requested_targets
  )
  validate_baseline_binding(
    initialization$baseline,
    context$cohort,
    context$snapshot
  )
  candidate <- checkout_identity(
    initialization$paths$checkout,
    initialization$package
  )
  if (!identical(initialization$candidate, candidate)) {
    stop("Stock candidate checkout identity changed.", call. = FALSE)
  }
  validate_stock_adapter_paths(initialization$paths, context$path_plan)
  validate_stock_subject_libraries(
    initialization$preparation_report,
    context,
    initialization$paths
  )
  validate_stock_cache_repository(
    initialization$paths$binary_contrib,
    initialization$binary_manifest
  )
  validate_stock_cache_repository(
    initialization$paths$source_contrib,
    initialization$source_manifest
  )
  if (
    !identical(
      initialization$environment,
      stock_adapter_environment(
        initialization$paths,
        initialization$repository_settings
      )
    )
  ) {
    stop("Stock adapter environment is inconsistent.", call. = FALSE)
  }
  validate_stock_repository_settings(
    initialization$repository_settings,
    initialization$paths
  )
  validate_stock_dependencies(
    initialization$stock_dependencies,
    context$universe,
    initialization$requested_targets$package,
    context$snapshot
  )
  if (
    !identical(
      initialization$provenance,
      stock_adapter_expected_provenance()
    )
  ) {
    stop("Stock tool provenance changed.", call. = FALSE)
  }
  if (require_pre_worker) {
    database <- observe_stock_database(initialization$paths$checkout)
    if (
      !identical(database$stage, "install") ||
        !identical(database$todo, initialization$discovery$todo) ||
        any(database$todo$status != "todo") ||
        nrow(database$old) != 0L ||
        nrow(database$new) != 0L ||
        nrow(database$stock) != 0L
    ) {
      stop("Stock database changed before worker launch.", call. = FALSE)
    }
  }
  invisible(initialization)
}
