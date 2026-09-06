revdep_verbose <- function(verbose) {
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("`verbose` must be TRUE or FALSE.", call. = FALSE)
  }
  verbose
}

revdep_progress <- function(verbose, format, ...) {
  if (verbose) message(sprintf(format, ...))
  invisible(NULL)
}

stock_comparison_progress <- function(checkout) {
  last <- NULL
  function() {
    # A missed observation while the writer holds the database must not affect
    # the comparison or its final evidence validation.
    on.exit(stock_namespace_function("db_disconnect")(checkout), add = TRUE)
    database <- tryCatch(
      list(
        stage = stock_namespace_function("db_metadata_get")(checkout, "todo"),
        todo = stock_adapter_normalize_todo(revdepcheck::revdep_todo(checkout))
      ),
      error = function(e) NULL
    )
    if (is.null(database)) return(invisible(NULL))
    completed <- sum(database$todo$status == "done")
    pending <- database$todo$package[database$todo$status != "done"]
    value <- sprintf(
      "Comparison %s: %d/%d targets complete%s.",
      database$stage,
      completed,
      nrow(database$todo),
      if (length(pending)) paste0("; next unfinished: ", pending[[1L]]) else ""
    )
    if (!identical(last, value)) {
      message(value)
      last <<- value
    }
    invisible(NULL)
  }
}
