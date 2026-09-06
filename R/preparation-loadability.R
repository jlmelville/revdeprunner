check_preparation_loadability <- function(
  gate,
  context,
  timeout_seconds,
  checkpoint = NULL,
  verbose = FALSE
) {
  library <- source_preparation_build_library(context$path_plan)
  results <- lapply(seq_len(nrow(gate$report$results)), function(row) {
    value <- gate$report$results[row, , drop = FALSE]
    rownames(value) <- NULL
    value
  })
  names(results) <- gate$report$results$package
  attempts <- preparation_gate_attempt_records(gate$report$attempts)
  for (package in names(results)) {
    result <- results[[package]]
    if (!identical(result$outcome, "prepared")) next
    revdep_progress(
      verbose,
      "Checking loadability: %s %s.",
      package,
      result$version
    )
    attempt <- load_prepared_package(
      package,
      result$version,
      library,
      context,
      timeout_seconds,
      verbose = verbose
    )
    attempts <- preparation_gate_append_attempts(attempts, list(attempt))
    if (!identical(attempt$outcome, "success")) {
      result$outcome <- if (identical(attempt$outcome, "timeout"))
        "timeout" else "load-failure"
      result$evidence_attempt_id <- attempt$attempt_id
      result$diagnostic_excerpt <- attempt$diagnostic_excerpt
      results[[package]] <- result
      gate <- preparation_gate_record(
        context,
        gate$source_acquisitions,
        gate$source_preparations,
        attempts,
        results,
        gate$execution_order
      )
      if (!is.null(checkpoint)) checkpoint(gate)
    }
  }
  preparation_gate_record(
    context,
    gate$source_acquisitions,
    gate$source_preparations,
    attempts,
    results,
    gate$execution_order
  )
}

load_prepared_package <- function(
  package,
  version,
  library,
  context,
  timeout_seconds,
  verbose = FALSE
) {
  root <- source_preparation_attempt_directory(
    context$path_plan,
    package,
    version
  )
  logs <- source_preparation_log_paths(root, "load")
  expression <- paste(
    "args <- commandArgs(TRUE)",
    ".libPaths(c(args[[3L]], .Library), include.site = FALSE)",
    "ns <- loadNamespace(args[[1L]], lib.loc = args[[3L]])",
    "expected <- normalizePath(file.path(args[[3L]], args[[1L]]), winslash = '/', mustWork = TRUE)",
    "stopifnot(identical(getNamespaceInfo(ns, 'path'), expected))",
    "stopifnot(identical(as.character(getNamespaceVersion(ns)), args[[2L]]))",
    sep = "; "
  )
  process <- with_source_preparation_libraries(
    library,
    run_source_preparation_process(
      context$r_executable,
      c(
        "--vanilla",
        "--slave",
        "-e",
        expression,
        "--args",
        package,
        version,
        library
      ),
      root,
      logs$stdout,
      logs$stderr,
      timeout_seconds,
      verbose = verbose
    )
  )
  source_preparation_attempt_from_process(
    package,
    version,
    "load",
    process,
    logs,
    context$path_plan
  )
}

restore_prepared_library <- function(state, timeout_seconds, verbose = FALSE) {
  context <- state$context
  ensure_revdep_directory(context$path_plan$runs_root, "run directory")
  validate_resolved_runtime_anchor(context$path_plan$runs_root, "runs_root")
  library <- source_preparation_build_library(context$path_plan)
  packages <- c(state$gate$report$results$package, state$baseline$package)
  versions <- c(state$gate$report$results$version, state$baseline$version)
  present <- vapply(
    seq_along(packages),
    function(index) {
      source_preparation_library_has_package(
        library,
        packages[[index]],
        versions[[index]]
      )
    },
    logical(1L)
  )
  if (!all(present)) {
    state$gate <- do.call(
      prepare_dependency_universe,
      c(
        context,
        list(
          baseline_source = state$baseline$path,
          previous = state$gate,
          timeout_seconds = timeout_seconds,
          verbose = verbose
        )
      )
    )
  }
  state
}
