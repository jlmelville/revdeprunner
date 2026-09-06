signal_preparation_failure <- function(
  package,
  version,
  outcome,
  stage,
  diagnostic,
  logs
) {
  problem <- data.frame(
    package = package,
    version = version,
    outcome = outcome,
    blocking_dependency = NA_character_,
    stage = stage,
    diagnostic_excerpt = substr(diagnostic, 1L, 1000L),
    stdout_path = logs$stdout,
    stderr_path = logs$stderr,
    stringsAsFactors = FALSE
  )
  stop(structure(
    list(message = diagnostic, call = NULL, problem = problem),
    class = c("revdeprunner_preparation_failure", "error", "condition")
  ))
}

download_preparation_source <- function(
  url,
  destination,
  package,
  version,
  path_plan
) {
  root <- source_preparation_attempt_directory(path_plan, package, version)
  logs <- list(
    stdout = file.path(root, "download.stdout.log"),
    stderr = file.path(root, "download.stderr.log")
  )
  writeLines(paste("Source:", url), logs$stdout)
  diagnostics <- character()
  status <- tryCatch(
    withCallingHandlers(
      source_download_file(url, destination),
      warning = function(warning) {
        diagnostics <<- c(diagnostics, conditionMessage(warning))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(error) {
      diagnostics <<- c(diagnostics, conditionMessage(error))
      1L
    }
  )
  writeLines(diagnostics, logs$stderr)
  if (!is.numeric(status) || length(status) != 1L || is.na(status)) {
    stop("Source download returned an invalid status.", call. = FALSE)
  }
  if (status != 0) {
    diagnostic <- paste(
      c(
        sprintf(
          "Source download returned a nonzero status for %s %s: %s",
          package,
          version,
          url
        ),
        diagnostics
      ),
      collapse = "\n"
    )
    signal_preparation_failure(
      package,
      version,
      "download-failure",
      "download",
      diagnostic,
      logs
    )
  }
  invisible(NULL)
}
