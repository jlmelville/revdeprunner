#' Run reverse-dependency comparisons
#'
#' Verify a completed preparation and compare the frozen CRAN baseline with the
#' current development checkout using the package's guarded stock
#' `revdepcheck` adapter.
#'
#' @param prepared A ready object returned by [revdep_prepare()].
#' @param repeat_checks Repeat both baseline and candidate checks, including
#'   completed comparisons. Use after repairing external libraries. Compatible
#'   prepared binaries are retained. Defaults to `FALSE`, which resumes unfinished
#'   targets and reuses completed changed and unchanged comparisons.
#' @param worker_timeout_seconds Positive whole seconds allowed per stock check
#'   worker, or `NULL` to derive a budget from preparation evidence.
#' @param process_timeout_seconds Positive whole seconds allowed for the entire
#'   stock comparison process, including subject installation. Defaults to 7200.
#'   Also bounds each library restoration or loadability subprocess during admission.
#'   Increasing either budget allows unfinished targets another attempt without
#'   invalidating completed comparisons.
#' @inheritParams revdep_prepare
#'
#' @return A `revdep_result` object with `summary`, `results`, `changes`, `diagnostics`,
#'   the frozen `plan`, and advanced `evidence`.
#'   `summary$elapsed_seconds` measures the stock comparison adapter, excluding
#'   stock initialization.
#'   `changes` is a data frame with `package`, `severity` (`error`, `warning`,
#'   `note`), `change` (`added`, `removed`), and `message`, using stock's normalized
#'   comparison. These details persist in the saved result. Stock calls a pair
#'   `unchanged` when it has no new problems, so it may still have removed problems.
#'
#' @details
#' Ordinary retries retain complete pairs and retry unfinished pairs. Keep the
#' run directory to resume an unfinished comparison. Completed results can be
#' reused without the old comparison workspace or logs. Binary admission checks
#' the current R major/minor version, platform, architecture, and OS tag;
#' comparison identity also includes the full R version and candidate source.
#' After other environmental repairs, use `repeat_checks = TRUE` to run both sides
#' again. Changing candidate hard dependencies or constraints requires a new plan
#' and preparation; ordinary source edits can reuse preparation.
#'
#' @examples
#' \dontrun{
#' prepared <- revdep_prepare("/path/to/package")
#' if (nrow(prepared$problems) == 0L) {
#'   result <- revdep_check(prepared, verbose = TRUE)
#'   result$results
#'   result$changes
#' }
#' }
#' @export
revdep_check <- function(
  prepared,
  repeat_checks = FALSE,
  worker_timeout_seconds = NULL,
  process_timeout_seconds = 7200L,
  verbose = FALSE
) {
  verbose <- revdep_verbose(verbose)
  if (
    !is.logical(repeat_checks) ||
      length(repeat_checks) != 1L ||
      is.na(repeat_checks)
  ) {
    stop("`repeat_checks` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(worker_timeout_seconds)) {
    worker_timeout_seconds <- revdep_execution_timeout(
      worker_timeout_seconds,
      "worker_timeout_seconds"
    )
  }
  process_timeout_seconds <- revdep_execution_timeout(
    process_timeout_seconds,
    "process_timeout_seconds"
  )
  state <- revdep_prepared_state(prepared)
  if (
    is.null(state$gate) ||
      !is.null(state$problem) ||
      any(state$gate$report$results$outcome != "prepared")
  ) {
    stop(
      "Preparation is incomplete; resolve `prepared$problems` and run `revdep_prepare()` again.",
      call. = FALSE
    )
  }
  # Admit saved metadata before loadability reports reuse its context.
  validate_preparation_report(
    state$gate$report,
    state$context$universe,
    state$context$cohort,
    state$context$snapshot,
    state$context$lane
  )
  require_stock_adapter_tools()

  revdep_progress(
    verbose,
    "Validating prepared packages in the current environment."
  )
  state <- restore_prepared_library(state, process_timeout_seconds, verbose)
  admitted <- check_preparation_loadability(
    state$gate,
    state$context,
    process_timeout_seconds,
    verbose = verbose
  )
  if (any(admitted$report$results$outcome != "prepared")) {
    problems <- revdep_report_problems(admitted$report, state$context)
    stop(
      paste0(
        "Prepared packages are not loadable in the current environment: ",
        paste(problems$package, collapse = ", "),
        ". Run `revdep_prepare()` again for package logs.\n",
        paste(problems$diagnostic_excerpt, collapse = "\n")
      ),
      call. = FALSE
    )
  }

  checkpoint <- attr(prepared, "checkpoint", exact = TRUE)
  candidate <- checkout_identity(
    state$context$path_plan$package_root,
    state$context$cohort$package
  )
  environment <- revdep_compatibility_lane()
  comparison_id <- revdep_request_id(list(
    request_id = state$request_id,
    candidate = candidate,
    environment = environment,
    artifacts = state$gate$report$artifacts
  ))
  check_checkpoint <- file.path(
    dirname(checkpoint),
    paste0("check-v4-", comparison_id, ".rds")
  )
  check_state <- if (file.exists(check_checkpoint)) {
    value <- read_revdep_checkpoint(check_checkpoint, "comparison")
    validate_revdep_check_state(value, state$request_id, candidate, environment)
    value
  } else {
    list(
      version = "revdeprunner-check-state/v4",
      request_id = state$request_id,
      candidate = candidate,
      environment = environment,
      workspace = NULL,
      initialization = NULL,
      result = NULL,
      elapsed_seconds = NA_real_
    )
  }

  if (!is.null(check_state$initialization)) {
    initialization <- check_state$initialization
    if (
      !identical(initialization$candidate, candidate) ||
        !identical(initialization$baseline, state$baseline) ||
        !identical(
          initialization$selected_targets,
          state$context$universe$targets
        ) ||
        !identical(
          initialization$preparation_report$artifacts,
          state$gate$report$artifacts
        )
    ) {
      stop(
        "The saved comparison inputs differ from this preparation.",
        call. = FALSE
      )
    }
  }

  retry_installation <- FALSE
  if (
    !repeat_checks &&
      !is.null(check_state$initialization) &&
      (is.null(check_state$result) ||
        identical(check_state$result$state, "comparison-incomplete"))
  ) {
    initialization <- check_state$initialization
    validate_stock_adapter_paths(initialization$paths, state$context$path_plan)
    database <- observe_stock_database(initialization$paths$checkout)
    retry_installation <- identical(database$stage, "install")
    if (retry_installation) {
      # No target pair exists yet. Abandon interrupted subject installations in
      # their owned workspace, retaining prepared binaries for fresh initialization.
      validate_stock_revdepcheck_initialization(initialization, state$context)
    }
  }
  if (repeat_checks || retry_installation) {
    check_state[c("workspace", "initialization", "result")] <- list(
      NULL,
      NULL,
      NULL
    )
    check_state$elapsed_seconds <- NA_real_
  }

  if (is.null(check_state$initialization)) {
    revdep_progress(verbose, "Initializing comparison workspace.")
    if (is.null(check_state$workspace)) {
      check_state$workspace <- paste0(
        "stock-",
        substr(comparison_id, 1L, 16L),
        "-",
        basename(tempfile())
      )
      # Reserve ownership durably before creating fallible initialization state.
      write_revdep_checkpoint(check_state, check_checkpoint)
    }
    recover_stock_initialization_workspace(
      check_state$workspace,
      state$context$path_plan
    )
    check_state$initialization <- initialize_stock_revdepcheck(
      state$gate,
      state$context,
      state$baseline$path,
      workspace = check_state$workspace
    )
    write_revdep_checkpoint(check_state, check_checkpoint)
  }

  if (
    is.null(check_state$result) ||
      identical(check_state$result$state, "comparison-incomplete")
  ) {
    started <- proc.time()[["elapsed"]]
    check_state$result <- run_stock_revdepcheck(
      check_state$initialization,
      state$context,
      worker_timeout_seconds = worker_timeout_seconds,
      process_timeout_seconds = process_timeout_seconds,
      verbose = verbose
    )
    check_state$elapsed_seconds <- unname(
      proc.time()[["elapsed"]] - started
    )
    write_revdep_checkpoint(check_state, check_checkpoint)
  } else {
    revdep_progress(verbose, "Reusing completed comparisons.")
  }

  revdep_progress(verbose, "Comparison complete: %s.", check_state$result$state)
  new_revdep_result(
    state,
    check_state,
    check_checkpoint
  )
}
