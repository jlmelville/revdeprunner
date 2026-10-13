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
  timings <- x$timings
  if (is.null(timings)) {
    timings <- revdep_preparation_timings(x$evidence$report, x$plan)
  }
  cat(paste0(revdep_preparation_timing_lines(timings), "\n"), sep = "")
  invisible(x)
}

revdep_preparation_timings <- function(report, plan) {
  packages <- if (is.null(report)) character() else report$results$package
  selected <- plan$targets$package[plan$targets$selected]
  rows <- lapply(packages, function(package) {
    attempts <- report$attempts[
      report$attempts$package == package,
      ,
      drop = FALSE
    ]
    attempts <- attempts[!duplicated(attempts$attempt_id), , drop = FALSE]
    builds <- attempts[attempts$stage == "build", , drop = FALSE]
    successful <- builds[builds$outcome == "success", , drop = FALSE]
    latest <- if (nrow(successful)) {
      index <- utils::tail(
        order(successful$started_at, successful$attempt_id, method = "radix"),
        1L
      )
      as.numeric(successful$duration_ms[[index]]) / 1000
    } else {
      NA_real_
    }
    seconds <- function(stage) {
      sum(as.numeric(attempts$duration_ms[attempts$stage == stage])) / 1000
    }
    data.frame(
      package = package,
      role = if (package %in% selected) "target" else "dependency",
      build_attempts = nrow(builds),
      build_seconds = seconds("build"),
      install_seconds = seconds("install"),
      load_seconds = seconds("load"),
      latest_successful_build_seconds = latest,
      stringsAsFactors = FALSE
    )
  })
  if (!length(rows)) {
    return(data.frame(
      package = character(),
      role = character(),
      build_attempts = integer(),
      build_seconds = numeric(),
      install_seconds = numeric(),
      load_seconds = numeric(),
      latest_successful_build_seconds = numeric()
    ))
  }
  result <- do.call(rbind, rows)
  result <- result[order(result$package, method = "radix"), , drop = FALSE]
  rownames(result) <- NULL
  result
}

revdep_preparation_timing_lines <- function(timings) {
  lines <- if (!nrow(timings)) {
    "No preparation subprocess timings recorded."
  } else {
    sprintf(
      "Recorded preparation subprocess time (cumulative across resumes): build %.1f s; install %.1f s; load %.1f s.",
      sum(timings$build_seconds),
      sum(timings$install_seconds),
      sum(timings$load_seconds)
    )
  }
  builds <- timings[timings$build_attempts > 0L, , drop = FALSE]
  builds <- builds[
    order(-builds$build_seconds, builds$package, method = "radix"),
    ,
    drop = FALSE
  ]
  if (nrow(builds)) {
    lines <- c(
      lines,
      "Slowest recorded preparation builds (all attempts):",
      vapply(
        seq_len(min(3L, nrow(builds))),
        function(row) {
          item <- builds[row, ]
          sprintf(
            "  %s (%s): %.1f s; build attempts: %d.",
            item$package,
            item$role,
            item$build_seconds,
            item$build_attempts
          )
        },
        character(1L)
      )
    )
  }
  targets <- timings[timings$role == "target", , drop = FALSE]
  if (nrow(targets)) {
    targets <- targets[
      order(
        -targets$latest_successful_build_seconds,
        targets$package,
        na.last = TRUE,
        method = "radix"
      ),
      ,
      drop = FALSE
    ]
    lines <- c(
      lines,
      "Selected-target preparation build references (latest successful attempt):",
      vapply(
        seq_len(min(3L, nrow(targets))),
        function(row) {
          item <- targets[row, ]
          if (is.na(item$latest_successful_build_seconds)) {
            sprintf(
              "  %s: no successful build timing recorded (a binary may have been reused).",
              item$package
            )
          } else {
            sprintf(
              "  %s: %.1f s.",
              item$package,
              item$latest_successful_build_seconds
            )
          }
        },
        character(1L)
      )
    )
  }
  c(
    lines,
    "Timings exclude downloads, validation, and released-subject setup; they are not this call's wall time or a check ETA.",
    "Comparison checks build each selected target from source on both old/new sides; dependency binaries are reused."
  )
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
  results <- if (is.null(state$gate)) {
    data.frame(outcome = character())
  } else {
    state$gate$report$results
  }
  problems <- if (is.null(state$gate)) {
    empty_revdep_problems()
  } else {
    revdep_report_problems(state$gate$report, state$context)
  }
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
      ),
      timings = revdep_preparation_timings(state$gate$report, state$plan)
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
