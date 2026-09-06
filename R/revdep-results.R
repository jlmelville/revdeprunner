#' @export
print.revdep_prepared <- function(x, ...) {
  summary <- x$summary[1L, , drop = FALSE]
  cat(sprintf(
    "Reverse-dependency preparation for %s (baseline %s)\n",
    summary$package,
    summary$baseline_version
  ))
  cat(sprintf(
    "State: %s; %d targets; %d requirements; %d problems\n",
    summary$state,
    summary$selected_targets,
    summary$requirements,
    summary$problems
  ))
  invisible(x)
}

#' @export
print.revdep_result <- function(x, ...) {
  summary <- x$summary[1L, , drop = FALSE]
  cat(sprintf(
    "Reverse-dependency result for %s: %s\n",
    summary$package,
    summary$state
  ))
  cat(sprintf(
    "%d unchanged; %d changed; %d incomplete; %d not checked\n",
    summary$unchanged,
    summary$changed,
    summary$incomplete,
    summary$not_checked
  ))
  invisible(x)
}

new_revdep_prepared <- function(state, checkpoint) {
  results <- if (is.null(state$gate)) data.frame(outcome = character()) else
    state$gate$report$results
  problems <- if (is.null(state$gate)) empty_revdep_problems() else
    revdep_report_problems(state$gate$report, state$context)
  problems <- rbind(state$problem, problems)
  summary <- data.frame(
    package = state$plan$summary$package,
    development_version = state$plan$summary$development_version,
    baseline_version = state$plan$summary$baseline_version,
    snapshot_id = state$plan$summary$snapshot_id,
    state = if (nrow(problems) == 0L) "ready" else "preparation-incomplete",
    selected_targets = state$plan$summary$selected_targets,
    requirements = nrow(preparation_required_packages(
      state$context$source_plan$requirements
    )),
    prepared = sum(results$outcome == "prepared"),
    problems = nrow(problems),
    blocked = sum(results$outcome == "blocked"),
    stringsAsFactors = FALSE
  )
  prepared <- structure(
    list(
      summary = summary,
      problems = problems,
      plan = state$plan,
      evidence = list(
        checkpoint = checkpoint,
        baseline = state$baseline,
        report = state$gate$report
      )
    ),
    class = "revdep_prepared",
    checkpoint = checkpoint,
    request_id = state$request_id
  )
  prepared
}

revdep_report_problems <- function(report, context) {
  results <- report$results
  failed <- results$outcome != "prepared"
  results <- results[failed, , drop = FALSE]
  if (nrow(results) == 0L) {
    return(empty_revdep_problems())
  }
  attempts <- report$attempts
  attempt_rows <- match(results$evidence_attempt_id, attempts$attempt_id)
  present <- !is.na(attempt_rows)
  stage <- stdout <- stderr <- rep(NA_character_, nrow(results))
  stage[present] <- attempts$stage[attempt_rows[present]]
  stdout[present] <- attempts$stdout_path[attempt_rows[present]]
  stderr[present] <- attempts$stderr_path[attempt_rows[present]]
  run_root <- runtime_role_path(context$path_plan, "run")
  stdout <- revdep_problem_log_paths(stdout, run_root)
  stderr <- revdep_problem_log_paths(stderr, run_root)
  data.frame(
    package = results$package,
    version = results$version,
    outcome = results$outcome,
    blocking_dependency = results$blocking_dependency,
    stage = stage,
    diagnostic_excerpt = results$diagnostic_excerpt,
    stdout_path = stdout,
    stderr_path = stderr,
    stringsAsFactors = FALSE
  )
}

empty_revdep_problems <- function() {
  data.frame(
    package = character(),
    version = character(),
    outcome = character(),
    blocking_dependency = character(),
    stage = character(),
    diagnostic_excerpt = character(),
    stdout_path = character(),
    stderr_path = character(),
    stringsAsFactors = FALSE
  )
}

revdep_problem_log_paths <- function(paths, run_root) {
  present <- !is.na(paths)
  paths[present] <- file.path(run_root, paths[present])
  paths
}

new_revdep_result <- function(state, check_state, checkpoint) {
  result <- check_state$result
  outcomes <- result$results$outcome
  summary <- data.frame(
    package = state$plan$summary$package,
    development_version = check_state$candidate$version,
    baseline_version = state$plan$summary$baseline_version,
    snapshot_id = state$plan$summary$snapshot_id,
    state = result$state,
    selected_targets = nrow(result$results),
    unchanged = sum(outcomes == "unchanged"),
    changed = sum(outcomes == "changed"),
    incomplete = sum(outcomes == "incomplete"),
    not_checked = sum(outcomes == "not_checked"),
    elapsed_seconds = check_state$elapsed_seconds,
    stringsAsFactors = FALSE
  )
  structure(
    list(
      summary = summary,
      results = result$results,
      changes = result$changes,
      diagnostics = result$diagnostics,
      plan = state$plan,
      evidence = list(
        checkpoint = checkpoint,
        logs = result$logs
      )
    ),
    class = "revdep_result"
  )
}
