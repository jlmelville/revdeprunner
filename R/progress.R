revdep_verbose <- function(verbose) {
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("`verbose` must be TRUE or FALSE.", call. = FALSE)
  }
  verbose
}

revdep_progress <- function(verbose, format, ...) {
  if (verbose) {
    message(sprintf(format, ...))
  }
  invisible(NULL)
}

revdep_progress_phase <- function(verbose, label, code) {
  if (!verbose) {
    return(force(code))
  }
  revdep_progress(verbose, "%s.", label)
  started <- proc.time()[["elapsed"]]
  value <- force(code)
  revdep_progress(
    verbose,
    "%s finished in %.1f s.",
    label,
    proc.time()[["elapsed"]] - started
  )
  value
}

stock_comparison_activity <- function(checkout, packages, since) {
  if (!length(packages)) {
    return(NULL)
  }
  packages <- vapply(packages, validate_package_name, character(1L))
  candidates <- expand.grid(
    which = c("old", "new"),
    package = packages,
    stringsAsFactors = FALSE
  )
  checks <- stock_namespace_function("dir_find")(checkout, "checks")
  paths <- file.path(
    checks,
    candidates$package,
    candidates$which,
    paste0(candidates$package, ".Rcheck"),
    "00check.log"
  )
  info <- file.info(paths, extra_cols = FALSE)
  present <- which(!is.na(info$mtime) & !info$isdir & info$mtime >= since)
  if (!length(present)) {
    return(NULL)
  }
  index <- present[[which.max(as.numeric(info$mtime[present]))]]
  lines <- strsplit(
    source_preparation_log_tail(paths[[index]], 65536L),
    "\n",
    fixed = TRUE
  )[[1L]]
  stage <- if (any(grepl("^\\* DONE$", lines))) {
    "complete"
  } else {
    stock_adapter_last_check(lines)
  }
  if (is.na(stage)) {
    stage <- "starting check"
  }
  list(
    package = candidates$package[[index]],
    which = candidates$which[[index]],
    stage = stage,
    log = paths[[index]]
  )
}

stock_comparison_progress <- function(
  checkout,
  clock = function() proc.time()[["elapsed"]]
) {
  last <- NULL
  last_activity <- NULL
  started <- clock()
  since <- Sys.time()
  last_emitted <- -Inf
  function() {
    # A missed observation while the writer holds the database must not affect
    # the comparison or its final evidence validation.
    on.exit(
      tryCatch(
        stock_namespace_function("db_disconnect")(checkout),
        error = function(e) NULL
      ),
      add = TRUE
    )
    database <- tryCatch(
      list(
        stage = stock_namespace_function("db_metadata_get")(checkout, "todo"),
        todo = stock_adapter_normalize_todo(revdepcheck::revdep_todo(checkout))
      ),
      error = function(e) NULL
    )
    if (is.null(database)) {
      return(invisible(NULL))
    }
    completed <- sum(database$todo$status == "done")
    pending <- database$todo$package[database$todo$status != "done"]
    value <- sprintf(
      "Comparison %s: %d/%d targets complete%s.",
      database$stage,
      completed,
      nrow(database$todo),
      if (length(pending)) paste0("; next unfinished: ", pending[[1L]]) else ""
    )
    now <- clock()
    activity <- tryCatch(
      suppressWarnings(stock_comparison_activity(checkout, pending, since)),
      error = function(e) NULL
    )
    changed <- !identical(last, value) || !identical(last_activity, activity)
    if (changed || now - last_emitted >= 60) {
      if (!identical(last, value)) {
        message(value)
      }
      if (!is.null(activity)) {
        message(sprintf(
          "Comparison check: %s (%s); %s; elapsed %.1f s; log: %s",
          activity$package,
          activity$which,
          activity$stage,
          now - started,
          activity$log
        ))
      } else if (!changed) {
        message(sprintf("%s Elapsed %.1f s.", value, now - started))
      }
      last <<- value
      last_activity <<- activity
      last_emitted <<- now
    }
    invisible(NULL)
  }
}
