stock_adapter_results <- function(initialization, process, database) {
  selected <- initialization$selected_targets
  rows <- lapply(seq_len(nrow(selected)), function(row) {
    target <- selected[row, , drop = FALSE]
    package <- target$package[[1L]]
    if (package %in% initialization$excluded_targets) {
      return(data.frame(
        package = package,
        expected_version = target$version[[1L]],
        role = target$role[[1L]],
        outcome = "not_checked",
        old_version = NA_character_,
        old_status = NA_character_,
        new_version = NA_character_,
        new_status = NA_character_,
        stock_status = NA_character_,
        diagnostic_excerpt = "Owner excluded this target before comparison.",
        stringsAsFactors = FALSE
      ))
    }
    old <- database$old[database$old$package == package, , drop = FALSE]
    new <- database$new[database$new$package == package, , drop = FALSE]
    stock <- database$stock[
      database$stock$package == package,
      ,
      drop = FALSE
    ]
    complete <- nrow(old) == 1L &&
      nrow(new) == 1L &&
      nrow(stock) == 1L &&
      stock$stock_status[[1L]] %in% c("+", "-") &&
      old$status[[1L]] %in% c("OK", "NOTE", "WARNING", "ERROR") &&
      new$status[[1L]] %in% c("OK", "NOTE", "WARNING", "ERROR")
    if (complete) {
      if (
        !identical(old$version[[1L]], target$version[[1L]]) ||
          !identical(new$version[[1L]], target$version[[1L]])
      ) {
        stop(
          "Stock result version differs from the frozen target.",
          call. = FALSE
        )
      }
    }
    if (!complete) {
      stock_status <- if (nrow(stock) == 1L) {
        stock$stock_status[[1L]]
      } else {
        NA_character_
      }
      outcome <- "incomplete"
    } else {
      stock_status <- stock$stock_status[[1L]]
      outcome <- if (identical(stock_status, "+")) {
        "unchanged"
      } else if (identical(stock_status, "-")) {
        "changed"
      } else if (stock_status %in% c("i+", "i-", "t+", "t-", "?")) {
        "incomplete"
      } else {
        stop("Stock comparison returned an unsupported status.", call. = FALSE)
      }
    }
    diagnostic <- if (identical(outcome, "incomplete")) {
      stock_adapter_incomplete_diagnostic(process, old, new, stock_status)
    } else {
      NA_character_
    }
    data.frame(
      package = package,
      expected_version = target$version[[1L]],
      role = target$role[[1L]],
      outcome = outcome,
      old_version = if (nrow(old) == 1L) old$version[[1L]] else NA_character_,
      old_status = if (nrow(old) == 1L) old$status[[1L]] else NA_character_,
      new_version = if (nrow(new) == 1L) new$version[[1L]] else NA_character_,
      new_status = if (nrow(new) == 1L) new$status[[1L]] else NA_character_,
      stock_status = stock_status,
      diagnostic_excerpt = diagnostic,
      stringsAsFactors = FALSE
    )
  })
  results <- do.call(rbind, rows)
  results <- results[order(results$package, method = "radix"), , drop = FALSE]
  rownames(results) <- NULL
  results
}

stock_adapter_incomplete_diagnostic <- function(process, old, new, status) {
  evidence <- c(
    if (process$timed_out) "Stock comparison process timed out." else NULL,
    if (process$status != 0L) {
      paste0(
        "Stock comparison process exited with status ",
        process$status,
        "."
      )
    } else {
      NULL
    },
    if (nrow(old) == 1L) paste0("Old status: ", old$status[[1L]], ".") else
      "Old result is missing.",
    if (nrow(new) == 1L) paste0("New status: ", new$status[[1L]], ".") else
      "New result is missing.",
    if (!is.na(status)) paste0("Stock status: ", status, ".") else NULL
  )
  paste(evidence, collapse = " ")
}

stock_adapter_result_state <- function(process, results) {
  requested <- results$outcome != "not_checked"
  if (process$status != 0L || any(results$outcome[requested] == "incomplete")) {
    "comparison-incomplete"
  } else if (any(results$outcome[requested] == "changed")) {
    "comparison-changes"
  } else {
    "success"
  }
}

empty_stock_changes <- function() {
  data.frame(
    package = character(),
    severity = character(),
    change = character(),
    message = character(),
    stringsAsFactors = FALSE
  )
}

normalize_stock_comparison <- function(comparison) {
  if (
    !inherits(comparison$new, "rcmdcheck") ||
      !length(comparison$old) ||
      !all(vapply(comparison$old, inherits, logical(1L), "rcmdcheck"))
  )
    return(comparison)
  checks <- c(comparison$old, list(comparison$new))
  missing_version <- vapply(
    checks,
    function(check) {
      length(check$rversion) == 0L
    },
    logical(1L)
  )
  if (!any(missing_version)) return(comparison)
  # rcmdcheck 1.4.0 parses some R-devel banners as character(0). Its table
  # constructor then drops every problem row; keep missing display metadata scalar.
  for (index in which(missing_version))
    checks[[index]]$rversion <- NA_character_
  rcmdcheck::compare_checks(
    checks[-length(checks)],
    checks[[length(checks)]]
  )
}

stock_adapter_changes <- function(checkout, results) {
  packages <- results$package[results$outcome %in% c("changed", "unchanged")]
  rows <- lapply(packages, function(package) {
    comparison <- normalize_stock_comparison(
      suppressMessages(revdepcheck::revdep_details(checkout, package))
    )$cmp
    comparison <- comparison[comparison$change %in% c(-1, 1), , drop = FALSE]
    if (!nrow(comparison)) return(empty_stock_changes())
    data.frame(
      package = package,
      severity = comparison$type,
      change = ifelse(comparison$change == 1, "added", "removed"),
      message = comparison$output,
      stringsAsFactors = FALSE
    )
  })
  if (!length(rows)) return(empty_stock_changes())
  changes <- do.call(rbind, rows)
  rownames(changes) <- NULL
  changes
}

validate_stock_revdepcheck_result <- function(result, context) {
  validate_stock_result_evidence(result)
  validate_stock_revdepcheck_initialization(
    result$initialization,
    context,
    require_pre_worker = FALSE
  )
  validate_stock_result_logs(
    result$logs,
    result$initialization,
    context$path_plan
  )
  invisible(result)
}

validate_stock_result_evidence <- function(result) {
  fields <- c(
    "initialization",
    "state",
    "process",
    "logs",
    "database",
    "results",
    "diagnostics",
    "changes"
  )
  if (
    !inherits(result, "revdeprunner_stock_result") ||
      !is.list(result) ||
      anyDuplicated(names(result)) ||
      !all(fields %in% names(result))
  ) {
    stop("Stock comparison result has an invalid structure.", call. = FALSE)
  }
  validate_source_preparation_process(result$process)
  expected_results <- stock_adapter_results(
    result$initialization,
    result$process,
    result$database
  )
  if (!identical(result$results, expected_results)) {
    stop("Stock target result evidence changed.", call. = FALSE)
  }
  if (
    !identical(
      result$state,
      stock_adapter_result_state(result$process, result$results)
    )
  ) {
    stop("Stock comparison state is inconsistent.", call. = FALSE)
  }
  validate_stock_result_database(
    result$database,
    result$process,
    result$initialization
  )
  validate_stock_result_rows(result$results, result$initialization)
  validate_stock_diagnostics(result$diagnostics, result$results)
  validate_stock_changes(result$changes, result$results)
  invisible(result)
}

validate_stock_changes <- function(changes, results) {
  if (
    !is.data.frame(changes) ||
      !identical(names(changes), names(empty_stock_changes())) ||
      any(!vapply(changes, is.character, logical(1L))) ||
      anyNA(changes) ||
      any(
        !changes$package %in%
          results$package[results$outcome %in% c("changed", "unchanged")]
      ) ||
      any(!changes$severity %in% c("error", "warning", "note")) ||
      any(!changes$change %in% c("added", "removed"))
  ) {
    stop("Stock change details have an invalid structure.", call. = FALSE)
  }
  invisible(changes)
}

validate_stock_result_database <- function(database, process, initialization) {
  if (
    !is.list(database) ||
      !identical(names(database), c("stage", "todo", "old", "new", "stock"))
  ) {
    stop("Stock database evidence has an invalid structure.", call. = FALSE)
  }
  requested <- initialization$requested_targets$package
  if (
    !identical(database$todo$package, requested) ||
      any(!database$old$package %in% requested) ||
      any(!database$new$package %in% requested) ||
      any(!database$stock$package %in% requested)
  ) {
    stop("Stock database target coverage is inconsistent.", call. = FALSE)
  }
  if (process$status == 0L) {
    if (
      !identical(database$stage, "done") ||
        any(database$todo$status != "done") ||
        !identical(database$old$package, requested) ||
        !identical(database$new$package, requested) ||
        !identical(database$stock$package, requested)
    ) {
      stop("Completed stock database evidence is incomplete.", call. = FALSE)
    }
  }
  invisible(database)
}

validate_stock_result_rows <- function(results, initialization) {
  fields <- c(
    "package",
    "expected_version",
    "role",
    "outcome",
    "old_version",
    "old_status",
    "new_version",
    "new_status",
    "stock_status",
    "diagnostic_excerpt"
  )
  expected <- initialization$selected_targets[
    order(initialization$selected_targets$package, method = "radix"),
    ,
    drop = FALSE
  ]
  if (
    !is.data.frame(results) ||
      !identical(names(results), fields) ||
      !identical(results$package, expected$package) ||
      !identical(results$expected_version, expected$version) ||
      !identical(results$role, expected$role) ||
      any(
        !results$outcome %in%
          c("unchanged", "changed", "incomplete", "not_checked")
      )
  ) {
    stop("Stock target result rows are inconsistent.", call. = FALSE)
  }
  excluded <- results$package %in% initialization$excluded_targets
  if (
    any(results$outcome[excluded] != "not_checked") ||
      any(results$outcome[!excluded] == "not_checked") ||
      any(!is.na(results$stock_status[excluded])) ||
      any(is.na(results$diagnostic_excerpt[
        results$outcome %in% c("incomplete", "not_checked")
      ])) ||
      any(
        !is.na(results$diagnostic_excerpt[
          results$outcome %in% c("unchanged", "changed")
        ])
      )
  ) {
    stop("Stock target outcome evidence is inconsistent.", call. = FALSE)
  }
  invisible(results)
}
