initialize_stock_database <- function(checkout, targets, cache_path) {
  environment <- c(
    CRANCACHE_DISABLE = "yes",
    CRANCACHE_DIR = cache_path
  )
  stock_adapter_with_environment(environment, {
    revdepcheck::dir_setup(checkout)
    stock_namespace_function("db_setup")(checkout)
    stock_namespace_function("db_todo_add")(checkout, targets)
    stock_namespace_function("db_metadata_set")(checkout, "todo", "install")
    stock_namespace_function("db_metadata_set")(checkout, "bioc", "FALSE")
    stock_namespace_function("db_metadata_set")(
      checkout,
      "dependencies",
      paste(stock_runner_first_level_fields(), collapse = ";")
    )
    stock_namespace_function("db_disconnect")(checkout)
  })
  observed <- observe_stock_database(checkout)
  if (
    !identical(observed$stage, "install") ||
      !identical(observed$todo$package, targets) ||
      any(observed$todo$status != "todo") ||
      nrow(observed$old) != 0L ||
      nrow(observed$new) != 0L
  ) {
    stop(
      "Stock discovery database does not match the frozen cohort.",
      call. = FALSE
    )
  }
  list(stage = observed$stage, todo = observed$todo)
}

observe_stock_database <- function(checkout) {
  stage <- stock_namespace_function("db_metadata_get")(checkout, "todo")
  todo <- revdepcheck::revdep_todo(checkout)
  raw <- stock_namespace_function("db_get_results")(checkout, NULL)
  summaries <- lapply(
    suppressMessages(revdepcheck::revdep_summary(checkout)),
    normalize_stock_comparison
  )
  stock <- if (length(summaries) == 0L) {
    data.frame(
      package = character(),
      stock_status = character(),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      package = names(summaries),
      stock_status = vapply(
        summaries,
        stock_namespace_function("rcmdcheck_status"),
        character(1L)
      ),
      stringsAsFactors = FALSE
    )
  }
  stock_namespace_function("db_disconnect")(checkout)
  todo <- stock_adapter_normalize_todo(todo)
  old <- stock_adapter_normalize_database_results(raw$old)
  new <- stock_adapter_normalize_database_results(raw$new)
  stock <- stock[order(stock$package, method = "radix"), , drop = FALSE]
  rownames(stock) <- NULL
  list(stage = stage, todo = todo, old = old, new = new, stock = stock)
}

resume_stock_database <- function(initialization) {
  checkout <- initialization$paths$checkout
  database <- observe_stock_database(checkout)
  validate_stock_result_database(database, list(status = 1L), initialization)
  if (
    !database$stage %in% c("install", "run", "report", "done") ||
      any(!database$todo$status %in% c("todo", "done"))
  ) {
    stop("Stock database has an unsupported recovery stage.", call. = FALSE)
  }
  results <- stock_adapter_results(
    initialization,
    list(status = 1L, timed_out = FALSE),
    database
  )
  unfinished <- results$package[results$outcome == "incomplete"]
  completed <- results$package[results$outcome %in% c("unchanged", "changed")]
  db <- stock_namespace_function("db")(checkout)
  on.exit(stock_namespace_function("db_disconnect")(checkout), add = TRUE)
  DBI::dbWithTransaction(db, {
    for (package in unfinished) {
      # A retry owns an entire pair: discard both sides before scheduling it.
      DBI::dbExecute(
        db,
        "DELETE FROM revdeps WHERE package = ?",
        params = list(package)
      )
      DBI::dbExecute(
        db,
        "UPDATE todo SET status = 'todo' WHERE package = ?",
        params = list(package)
      )
    }
    for (package in completed) {
      DBI::dbExecute(
        db,
        "UPDATE todo SET status = 'done' WHERE package = ?",
        params = list(package)
      )
    }
    if (length(unfinished) && !identical(database$stage, "install")) {
      DBI::dbExecute(
        db,
        "UPDATE metadata SET value = 'run' WHERE name = 'todo'"
      )
    }
  })
  invisible(unfinished)
}

stock_adapter_normalize_todo <- function(todo) {
  if (
    !is.data.frame(todo) ||
      !all(c("package", "status") %in% names(todo))
  ) {
    stop("Stock todo evidence has an invalid structure.", call. = FALSE)
  }
  todo <- todo[c("package", "status")]
  todo[] <- lapply(todo, as.character)
  todo <- todo[order(todo$package, method = "radix"), , drop = FALSE]
  rownames(todo) <- NULL
  todo
}

stock_adapter_normalize_database_results <- function(results) {
  fields <- c("package", "version", "status", "which")
  if (!is.data.frame(results) || !all(fields %in% names(results))) {
    stop(
      "Stock database result evidence has an invalid structure.",
      call. = FALSE
    )
  }
  results <- results[fields]
  results[] <- lapply(results, as.character)
  results <- results[order(results$package, method = "radix"), , drop = FALSE]
  rownames(results) <- NULL
  results
}
