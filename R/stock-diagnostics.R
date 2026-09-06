empty_stock_diagnostics <- function() {
  data.frame(
    package = character(),
    which = character(),
    reason = character(),
    duration_seconds = numeric(),
    status = integer(),
    last_check = character(),
    last_compilation = character(),
    error_excerpt = character(),
    check_log = character(),
    install_log = character(),
    stringsAsFactors = FALSE
  )
}

stock_adapter_incomplete_diagnostics <- function(
  initialization,
  results,
  path_plan
) {
  packages <- results$package[results$outcome == "incomplete"]
  if (length(packages) == 0L) {
    return(empty_stock_diagnostics())
  }
  run_root <- runtime_role_path(path_plan, "run")
  rows <- lapply(packages, function(package) {
    details <- tryCatch(
      suppressMessages(
        revdepcheck::revdep_details(initialization$paths$checkout, package)
      ),
      error = function(error) NULL
    )
    if (is.null(details)) {
      return(empty_stock_diagnostics())
    }
    stock_adapter_detail_diagnostics(package, details, run_root)
  })
  diagnostics <- do.call(rbind, rows)
  diagnostics <- diagnostics[
    order(diagnostics$package, diagnostics$which, method = "radix"),
    ,
    drop = FALSE
  ]
  rownames(diagnostics) <- NULL
  diagnostics
}

stock_adapter_detail_diagnostics <- function(package, details, run_root) {
  rows <- lapply(c("old", "new"), function(which) {
    detail <- details[[which]]
    if (
      !inherits(detail, "rcmdcheck") &&
        is.list(detail) &&
        length(detail) == 1L
    ) {
      detail <- detail[[1L]]
    }
    if (!inherits(detail, "rcmdcheck")) {
      return(NULL)
    }
    stock_adapter_detail_diagnostic(package, which, detail, run_root)
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) {
    return(empty_stock_diagnostics())
  }
  diagnostics <- do.call(rbind, rows)
  rownames(diagnostics) <- NULL
  diagnostics
}

stock_adapter_detail_diagnostic <- function(package, which, detail, run_root) {
  reason <- if (isTRUE(detail$timeout)) {
    "timeout"
  } else if (detail$status != 0L) {
    "error"
  } else {
    "incomplete"
  }
  check_output <- strsplit(detail$stdout, "\n", fixed = TRUE)[[1L]]
  install_output <- strsplit(detail$install_out, "\n", fixed = TRUE)[[1L]]
  check_log <- stock_adapter_diagnostic_log_path(
    file.path(detail$checkdir, "00check.log"),
    run_root
  )
  install_log <- stock_adapter_diagnostic_log_path(
    file.path(detail$checkdir, "00install.out"),
    run_root
  )
  data.frame(
    package = package,
    which = which,
    reason = reason,
    duration_seconds = as.numeric(detail$duration),
    status = as.integer(detail$status),
    last_check = stock_adapter_last_check(check_output),
    last_compilation = stock_adapter_last_compilation(install_output),
    error_excerpt = stock_adapter_error_excerpt(detail$errors),
    check_log = check_log,
    install_log = install_log,
    stringsAsFactors = FALSE
  )
}

stock_adapter_last_check <- function(lines) {
  checks <- grep("^\\* checking ", lines, value = TRUE)
  if (length(checks) == 0L) {
    return(NA_character_)
  }
  check <- sub("^\\* ", "", checks[[length(checks)]])
  sub(" \\.\\.\\.$", "", check)
}

stock_adapter_last_compilation <- function(lines) {
  compilations <- grep(
    paste0(
      "^[^[:space:]].*[[:space:]]-c[[:space:]]+",
      "[^[:space:]]+[[:space:]]+-o[[:space:]]+"
    ),
    lines,
    value = TRUE
  )
  if (length(compilations) == 0L) {
    return(NA_character_)
  }
  sub(
    paste0(
      ".*[[:space:]]-c[[:space:]]+([^[:space:]]+)",
      "[[:space:]]+-o[[:space:]].*$"
    ),
    "\\1",
    compilations[[length(compilations)]]
  )
}

stock_adapter_error_excerpt <- function(errors) {
  if (!is.character(errors) || length(errors) == 0L || all(is.na(errors))) {
    return(NA_character_)
  }
  excerpt <- paste(errors[!is.na(errors)], collapse = " | ")
  excerpt <- gsub("[[:space:]]+", " ", excerpt)
  if (nchar(excerpt) > 1000L) {
    excerpt <- paste0(substr(excerpt, 1L, 997L), "...")
  }
  excerpt
}

stock_adapter_diagnostic_log_path <- function(path, run_root) {
  if (!utils::file_test("-f", path)) {
    return(NA_character_)
  }
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (!path_is_within(run_root, path)) {
    return(NA_character_)
  }
  source_preparation_relative_path(path, run_root)
}

stock_adapter_process_logs <- function(paths, path_plan) {
  run_root <- runtime_role_path(path_plan, "run")
  records <- lapply(
    c(stdout = paths$stdout, stderr = paths$stderr),
    function(path) {
      path <- validate_source_preparation_log(path, run_root)
      list(
        path = source_preparation_relative_path(path, run_root),
        sha256 = digest::digest(
          path,
          algo = "sha256",
          file = TRUE,
          serialize = FALSE
        )
      )
    }
  )
  records
}

validate_stock_result_logs <- function(logs, initialization, path_plan) {
  if (!is.list(logs) || !identical(names(logs), c("stdout", "stderr"))) {
    stop("Stock process log evidence has an invalid structure.", call. = FALSE)
  }
  run_root <- runtime_role_path(path_plan, "run")
  for (name in names(logs)) {
    record <- logs[[name]]
    if (!is.list(record) || !identical(names(record), c("path", "sha256"))) {
      stop("Stock process log record has an invalid structure.", call. = FALSE)
    }
    validate_sha256(record$sha256, paste(name, "log sha256"))
    path <- file.path(run_root, record$path)
    expected <- initialization$paths[[name]]
    if (
      !identical(
        normalizePath(path, winslash = "/", mustWork = TRUE),
        expected
      ) ||
        !identical(
          digest::digest(path, algo = "sha256", file = TRUE, serialize = FALSE),
          record$sha256
        )
    ) {
      stop("Stock process log evidence changed.", call. = FALSE)
    }
  }
  invisible(logs)
}

validate_stock_diagnostics <- function(diagnostics, results) {
  fields <- names(empty_stock_diagnostics())
  character_fields <- c(
    "package",
    "which",
    "reason",
    "last_check",
    "last_compilation",
    "error_excerpt",
    "check_log",
    "install_log"
  )
  if (
    !is.data.frame(diagnostics) ||
      !identical(names(diagnostics), fields) ||
      any(!vapply(diagnostics[character_fields], is.character, logical(1L))) ||
      !is.numeric(diagnostics$duration_seconds) ||
      !is.integer(diagnostics$status) ||
      anyNA(diagnostics[c("package", "which", "reason")]) ||
      anyDuplicated(diagnostics[c("package", "which")])
  ) {
    stop("Stock diagnostic evidence has an invalid structure.", call. = FALSE)
  }
  incomplete <- results$package[results$outcome == "incomplete"]
  log_paths <- unlist(diagnostics[c("check_log", "install_log")])
  if (
    any(!diagnostics$package %in% incomplete) ||
      any(!diagnostics$which %in% c("old", "new")) ||
      any(!diagnostics$reason %in% c("timeout", "error", "incomplete")) ||
      any(
        !is.na(diagnostics$duration_seconds) &
          (!is.finite(diagnostics$duration_seconds) |
            diagnostics$duration_seconds < 0)
      ) ||
      any(
        !is.na(diagnostics$error_excerpt) &
          nchar(diagnostics$error_excerpt) > 1000L
      ) ||
      any(
        !is.na(log_paths) &
          (startsWith(log_paths, "/") |
            grepl("(^|/)\\.\\.(/|$)", log_paths))
      )
  ) {
    stop("Stock diagnostic evidence is inconsistent.", call. = FALSE)
  }
  invisible(diagnostics)
}
