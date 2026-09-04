prepare_source_binary_in_context <- function(
  package,
  context,
  source_acquisition,
  previous = NULL,
  timeout_seconds = 600L
) {
  source_plan <- context$source_plan
  lane <- context$lane
  path_plan <- context$path_plan
  r_executable <- context$r_executable
  source <- source_preparation_planned_row(package, context)
  package <- source$package[[1L]]
  version <- source$version[[1L]]
  timeout_seconds <- normalize_source_preparation_timeout(timeout_seconds)

  if (!is.null(previous)) {
    validate_source_preparation_record(previous, context)
    if (
      !identical(previous$package, package) ||
        !identical(previous$result$outcome[[1L]], "prepared")
    ) {
      stop(
        "The previous source preparation is not a prepared result for this package.",
        call. = FALSE
      )
    }
    if (!identical(source_acquisition, previous$source_acquisition)) {
      stop(
        "The supplied source acquisition differs from the previous preparation.",
        call. = FALSE
      )
    }
    build_library <- source_preparation_build_library(path_plan)
    if (
      source_preparation_library_has_package(
        build_library,
        package,
        version
      )
    ) {
      return(previous)
    }
  }

  acquisition <- source_acquisition
  validate_source_acquisition_record(
    acquisition,
    source_plan,
    path_plan
  )
  if (
    !identical(acquisition$package, package) ||
      !identical(acquisition$version, version)
  ) {
    stop("Source preparation acquisition is inconsistent.", call. = FALSE)
  }
  attempt_root <- source_preparation_attempt_directory(
    path_plan,
    package,
    version
  )
  source_path <- stage_source_preparation_archive(
    acquisition,
    attempt_root
  )
  build_library <- source_preparation_build_library(path_plan)
  build_logs <- source_preparation_log_paths(attempt_root, "build")
  build_args <- c(
    "CMD",
    "INSTALL",
    "--use-vanilla",
    "--build",
    paste0("--library=", build_library),
    source_path
  )
  build_process <- with_source_preparation_libraries(
    build_library,
    run_source_preparation_process(
      r_executable,
      build_args,
      attempt_root,
      build_logs$stdout,
      build_logs$stderr,
      timeout_seconds
    )
  )
  build_attempt <- source_preparation_attempt_from_process(
    package,
    version,
    "build",
    build_process,
    build_logs,
    path_plan
  )

  if (!identical(build_attempt$outcome, "success")) {
    outcome <- if (identical(build_attempt$outcome, "timeout")) {
      "timeout"
    } else {
      "compilation-failure"
    }
    preparation <- new_source_preparation_bundle(
      package,
      version,
      acquisition,
      binary_artifact = NULL,
      binary_path = NA_character_,
      attempts = list(build_attempt),
      result = source_preparation_result(
        package,
        version,
        outcome,
        attempt = build_attempt
      ),
      promotion = NULL
    )
    validate_source_preparation_record(preparation, context)
    return(preparation)
  }
  binary_path <- source_preparation_binary_output(attempt_root)
  binary_artifact <- source_preparation_binary_artifact(
    binary_path,
    package,
    version,
    lane
  )
  validate_source_preparation_library_package(
    build_library,
    package,
    version
  )
  verification_library <- ensure_source_acquisition_directory(
    file.path(attempt_root, "verification-library"),
    attempt_root,
    "source verification library"
  )
  install_logs <- source_preparation_log_paths(attempt_root, "install")
  install_args <- c(
    "CMD",
    "INSTALL",
    "--use-vanilla",
    paste0("--library=", verification_library),
    binary_path
  )
  install_process <- with_source_preparation_libraries(
    c(verification_library, build_library),
    run_source_preparation_process(
      r_executable,
      install_args,
      attempt_root,
      install_logs$stdout,
      install_logs$stderr,
      timeout_seconds
    )
  )
  install_attempt <- source_preparation_attempt_from_process(
    package,
    version,
    "install",
    install_process,
    install_logs,
    path_plan
  )
  attempts <- list(build_attempt, install_attempt)

  if (!identical(install_attempt$outcome, "success")) {
    outcome <- if (identical(install_attempt$outcome, "timeout")) {
      "timeout"
    } else {
      "installation-failure"
    }
    preparation <- new_source_preparation_bundle(
      package,
      version,
      acquisition,
      binary_artifact,
      binary_path,
      attempts,
      source_preparation_result(
        package,
        version,
        outcome,
        binary_artifact,
        install_attempt
      ),
      promotion = NULL
    )
    validate_source_preparation_record(preparation, context)
    return(preparation)
  }

  promotion <- promote_warehouse_artifact(
    binary_path,
    binary_artifact,
    path_plan,
    archive_name = basename(binary_path)
  )
  preparation <- new_source_preparation_bundle(
    package,
    version,
    acquisition,
    binary_artifact,
    binary_path,
    attempts,
    source_preparation_result(
      package,
      version,
      "prepared",
      binary_artifact,
      build_attempt
    ),
    promotion
  )
  validate_source_preparation_record(preparation, context)
  preparation
}

validate_source_preparation_record <- function(preparation, context) {
  validate_source_preparation_context_record(context)
  source_plan <- context$source_plan
  lane <- context$lane
  path_plan <- context$path_plan
  fields <- c(
    "package",
    "version",
    "source_acquisition",
    "binary_artifact",
    "binary_path",
    "attempts",
    "result",
    "promotion"
  )
  if (!is.list(preparation) || !identical(names(preparation), fields)) {
    stop("Source preparation has an invalid structure.", call. = FALSE)
  }
  source <- source_preparation_planned_row(preparation$package, context)
  if (!identical(preparation$version, source$version[[1L]])) {
    stop("Source preparation version does not match its plan.", call. = FALSE)
  }
  validate_source_acquisition_record(
    preparation$source_acquisition,
    source_plan,
    path_plan
  )
  if (
    !identical(preparation$source_acquisition$package, preparation$package) ||
      !identical(preparation$source_acquisition$version, preparation$version)
  ) {
    stop("Source preparation acquisition is inconsistent.", call. = FALSE)
  }

  attempts <- normalize_preparation_attempts(
    preparation$attempts,
    source_plan$requirements
  )
  if (nrow(attempts) < 1L || nrow(attempts) > 2L) {
    stop(
      "Source preparation requires one or two process attempts.",
      call. = FALSE
    )
  }
  if (
    any(attempts$package != preparation$package) ||
      any(attempts$version != preparation$version)
  ) {
    stop("Source preparation attempts are inconsistent.", call. = FALSE)
  }
  validate_source_preparation_logs(preparation$attempts, path_plan)

  artifacts <- if (is.null(preparation$binary_artifact)) {
    normalize_preparation_artifacts(list(), lane)
  } else {
    validate_artifact_identity(preparation$binary_artifact)
    if (
      !identical(preparation$binary_artifact$package, preparation$package) ||
        !identical(preparation$binary_artifact$version, preparation$version) ||
        !identical(preparation$binary_artifact$archive_type, "binary") ||
        !identical(preparation$binary_artifact$lane_id, lane$lane_id)
    ) {
      stop("Source preparation binary identity is inconsistent.", call. = FALSE)
    }
    validate_source_preparation_binary(
      preparation$binary_path,
      preparation$binary_artifact,
      lane,
      path_plan
    )
    normalize_preparation_artifacts(list(preparation$binary_artifact), lane)
  }
  if (
    is.null(preparation$binary_artifact) &&
      !(is.character(preparation$binary_path) &&
        length(preparation$binary_path) == 1L &&
        is.na(preparation$binary_path))
  ) {
    stop("Source preparation binary path is inconsistent.", call. = FALSE)
  }

  result <- validate_preparation_table(
    preparation$result,
    preparation_result_fields(),
    "source preparation result",
    allow_na = c(
      "artifact_id",
      "evidence_attempt_id",
      "blocking_dependency",
      "diagnostic_excerpt"
    )
  )
  if (
    nrow(result) != 1L ||
      !identical(result$package[[1L]], preparation$package) ||
      !identical(result$version[[1L]], preparation$version)
  ) {
    stop("Source preparation result is inconsistent.", call. = FALSE)
  }
  outcome <- validate_preparation_result_outcome(result$outcome[[1L]])
  diagnostic <- validate_preparation_diagnostic(
    result$diagnostic_excerpt[[1L]],
    "diagnostic_excerpt"
  )
  validate_preparation_result_row(
    result,
    outcome,
    diagnostic,
    artifacts,
    attempts
  )
  stages <- vapply(preparation$attempts, `[[`, character(1L), "stage")
  attempt_outcomes <- vapply(
    preparation$attempts,
    `[[`,
    character(1L),
    "outcome"
  )
  expected_stages <- if (is.null(preparation$binary_artifact)) {
    "build"
  } else {
    c("build", "install")
  }
  if (
    !identical(stages, expected_stages) ||
      (!is.null(preparation$binary_artifact) &&
        !identical(attempt_outcomes[[1L]], "success")) ||
      (identical(outcome, "prepared") &&
        !identical(attempt_outcomes, c("success", "success")))
  ) {
    stop("Source preparation attempt sequence is inconsistent.", call. = FALSE)
  }

  if (identical(outcome, "prepared")) {
    validate_source_preparation_promotion(
      preparation$promotion,
      preparation$binary_path,
      preparation$binary_artifact,
      path_plan
    )
  } else if (!is.null(preparation$promotion)) {
    stop("Failed source preparation must not publish a binary.", call. = FALSE)
  }

  invisible(preparation)
}

source_preparation_planned_row <- function(package, context) {
  validate_source_preparation_context_record(context)
  package <- validate_package_name(package)
  source <- source_acquisition_planned_row(context$source_plan, package)
  if (!identical(source$build_required[[1L]], "true")) {
    stop("Source preparation requires a planned binary miss.", call. = FALSE)
  }
  source
}

validate_source_preparation_context_record <- function(context) {
  fields <- c(
    "source_plan",
    "universe",
    "cohort",
    "snapshot",
    "binary_reuse",
    "lane",
    "path_plan",
    "r_executable"
  )
  if (!is.list(context) || !identical(names(context), fields)) {
    stop("Source preparation context has an invalid structure.", call. = FALSE)
  }

  invisible(context)
}

normalize_source_preparation_timeout <- function(timeout_seconds) {
  value <- normalize_contract_integer(timeout_seconds, "timeout_seconds")
  if (identical(value, "0")) {
    stop("`timeout_seconds` must be positive.", call. = FALSE)
  }
  as.integer(value)
}

source_preparation_attempt_directory <- function(path_plan, package, version) {
  run_root <- runtime_role_path(path_plan, "run")
  run_root <- ensure_source_acquisition_directory(
    run_root,
    path_plan$runs_root,
    "source preparation run root"
  )
  preparation_root <- ensure_source_acquisition_directory(
    file.path(run_root, "preparation"),
    run_root,
    "source preparation directory"
  )
  package_root <- ensure_source_acquisition_directory(
    file.path(preparation_root, paste(package, version, sep = "-")),
    preparation_root,
    "package preparation directory"
  )
  attempt_root <- tempfile("attempt-", tmpdir = package_root)
  if (!dir.create(attempt_root, recursive = FALSE)) {
    stop("Unable to create a source preparation attempt.", call. = FALSE)
  }
  attempt_root <- normalizePath(
    attempt_root,
    winslash = "/",
    mustWork = TRUE
  )
  if (!path_is_within(package_root, attempt_root)) {
    stop("Source preparation attempt escapes its managed root.", call. = FALSE)
  }

  attempt_root
}

source_preparation_build_library <- function(path_plan) {
  run_root <- runtime_role_path(path_plan, "run")
  run_root <- ensure_source_acquisition_directory(
    run_root,
    path_plan$runs_root,
    "source preparation run root"
  )
  ensure_source_acquisition_directory(
    file.path(run_root, "build-library"),
    run_root,
    "source build library"
  )
}

install_runner_supplied_baseline <- function(
  baseline_source,
  context,
  library,
  timeout_seconds
) {
  validate_source_preparation_context_record(context)
  baseline <- validate_stock_baseline_source(
    baseline_source,
    context$cohort,
    context$snapshot
  )
  library <- normalizePath(library, winslash = "/", mustWork = TRUE)
  if (
    source_preparation_library_has_package(
      library,
      baseline$package,
      baseline$version
    )
  ) {
    return(invisible(baseline))
  }

  source_before <- warehouse_file_snapshot(baseline$path)
  attempt_root <- source_preparation_attempt_directory(
    context$path_plan,
    baseline$package,
    baseline$version
  )
  logs <- source_preparation_log_paths(attempt_root, "install")
  arguments <- c(
    "CMD",
    "INSTALL",
    "--use-vanilla",
    paste0("--library=", library),
    baseline$path
  )
  process <- with_source_preparation_libraries(
    library,
    run_source_preparation_process(
      context$r_executable,
      arguments,
      attempt_root,
      logs$stdout,
      logs$stderr,
      timeout_seconds
    )
  )
  attempt <- source_preparation_attempt_from_process(
    baseline$package,
    baseline$version,
    "install",
    process,
    logs,
    context$path_plan
  )
  source_after <- warehouse_file_snapshot(baseline$path)
  if (!identical(source_after, source_before)) {
    stop(
      "The runner-supplied baseline changed during installation.",
      call. = FALSE
    )
  }
  if (!identical(attempt$outcome, "success")) {
    stop(
      sprintf(
        "Unable to install runner-supplied baseline %s %s.\n%s",
        baseline$package,
        baseline$version,
        attempt$diagnostic_excerpt
      ),
      call. = FALSE
    )
  }
  validate_source_preparation_library_package(
    library,
    baseline$package,
    baseline$version
  )
  invisible(baseline)
}

source_preparation_library_has_package <- function(
  library,
  package,
  version
) {
  package <- validate_package_name(package)
  version <- validate_package_version(version)
  path <- find.package(package, lib.loc = library, quiet = TRUE)
  if (length(path) != 1L || !nzchar(path)) {
    return(FALSE)
  }
  expected_path <- normalizePath(
    file.path(library, package),
    winslash = "/",
    mustWork = TRUE
  )
  if (!identical(path, expected_path)) {
    return(FALSE)
  }
  description <- tryCatch(
    read.dcf(
      file.path(path, "DESCRIPTION"),
      fields = c("Package", "Version")
    ),
    error = function(error) NULL
  )
  !is.null(description) &&
    nrow(description) == 1L &&
    identical(unname(description[[1L, "Package"]]), package) &&
    identical(unname(description[[1L, "Version"]]), version)
}

validate_source_preparation_library_package <- function(
  library,
  package,
  version
) {
  if (!source_preparation_library_has_package(library, package, version)) {
    stop(
      "Source preparation did not install the exact package version.",
      call. = FALSE
    )
  }
  invisible(library)
}

with_source_preparation_libraries <- function(libraries, code) {
  libraries <- unique(vapply(
    libraries,
    normalizePath,
    character(1L),
    winslash = "/",
    mustWork = TRUE
  ))
  variables <- c("R_LIBS", "R_LIBS_SITE", "R_LIBS_USER")
  previous <- Sys.getenv(variables, unset = NA_character_)
  on.exit(
    {
      Sys.unsetenv(variables)
      present <- !is.na(previous)
      if (any(present)) {
        do.call(Sys.setenv, as.list(previous[present]))
      }
    },
    add = TRUE
  )
  value <- paste(libraries, collapse = .Platform$path.sep)
  do.call(
    Sys.setenv,
    as.list(stats::setNames(rep(value, length(variables)), variables))
  )
  force(code)
}

stage_source_preparation_archive <- function(acquisition, attempt_root) {
  source_root <- ensure_source_acquisition_directory(
    file.path(attempt_root, "source"),
    attempt_root,
    "source preparation input directory"
  )
  archive_name <- source_acquisition_archive_name(acquisition$source_url)
  suffix <- warehouse_archive_suffix(archive_name)
  destination <- file.path(
    source_root,
    paste0(acquisition$package, "_", acquisition$version, suffix)
  )
  source_before <- warehouse_file_snapshot(acquisition$warehouse_path)
  copied <- warehouse_copy_file(
    acquisition$warehouse_path,
    destination,
    overwrite = FALSE,
    copy.mode = FALSE,
    copy.date = FALSE
  )
  if (!isTRUE(copied)) {
    stop(
      "Unable to copy the source archive into the preparation attempt.",
      call. = FALSE
    )
  }
  destination <- validate_source_download_payload(destination, source_root)
  validate_warehouse_archive(
    destination,
    acquisition$artifact,
    archive_name
  )
  validate_source_acquisition_md5(destination, acquisition$expected_md5)
  validate_warehouse_source_unchanged(
    acquisition$warehouse_path,
    source_before
  )

  destination
}

source_preparation_log_paths <- function(attempt_root, stage) {
  stage <- validate_preparation_attempt_stage(stage)
  paths <- list(
    stdout = file.path(attempt_root, paste0(stage, ".stdout.log")),
    stderr = file.path(attempt_root, paste0(stage, ".stderr.log"))
  )
  created <- vapply(paths, file.create, logical(1L), showWarnings = FALSE)
  if (!all(created)) {
    stop("Unable to create source preparation logs.", call. = FALSE)
  }
  lapply(paths, normalizePath, winslash = "/", mustWork = TRUE)
}

normalize_r_executable <- function(path) {
  path <- validate_contract_text(path, "r_executable")
  expanded <- path.expand(path)
  if (
    !file.exists(expanded) ||
      dir.exists(expanded) ||
      !utils::file_test("-f", expanded)
  ) {
    stop("`r_executable` must identify an existing file.", call. = FALSE)
  }
  normalized <- normalizePath(expanded, winslash = "/", mustWork = TRUE)
  if (!runtime_path_is_absolute(normalized) || grepl("\\\\", normalized)) {
    stop("`r_executable` must resolve to an absolute path.", call. = FALSE)
  }

  normalized
}

run_source_preparation_process <- function(
  r_executable,
  arguments,
  working_directory,
  stdout_path,
  stderr_path,
  timeout_seconds
) {
  r_executable <- normalize_r_executable(r_executable)
  timeout_seconds <- normalize_source_preparation_timeout(timeout_seconds)
  if (
    !is.character(arguments) ||
      length(arguments) == 0L ||
      anyNA(arguments)
  ) {
    stop(
      "Source preparation arguments must be non-empty strings.",
      call. = FALSE
    )
  }
  old_working_directory <- setwd(working_directory)
  on.exit(setwd(old_working_directory), add = TRUE)

  started_at <- format(
    Sys.time(),
    format = "%Y-%m-%dT%H:%M:%OS6Z",
    tz = "UTC"
  )
  started <- proc.time()[["elapsed"]]
  warnings <- character()
  status <- tryCatch(
    withCallingHandlers(
      system2(
        r_executable,
        vapply(arguments, shQuote, character(1L)),
        stdout = stdout_path,
        stderr = stderr_path,
        timeout = timeout_seconds
      ),
      warning = function(warning) {
        warnings <<- c(warnings, conditionMessage(warning))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(error) {
      stop(
        sprintf(
          "Unable to invoke the source preparation command: %s",
          conditionMessage(error)
        ),
        call. = FALSE
      )
    }
  )
  duration_ms <- round((proc.time()[["elapsed"]] - started) * 1000)
  if (
    !is.numeric(status) ||
      length(status) != 1L ||
      is.na(status) ||
      status < 0 ||
      status != floor(status)
  ) {
    stop(
      "Source preparation command returned an invalid status.",
      call. = FALSE
    )
  }
  timed_out <- status == 124L &&
    any(
      grepl("timed out", tolower(warnings), fixed = TRUE)
    )

  list(
    command = render_source_preparation_command(r_executable, arguments),
    started_at = started_at,
    duration_ms = duration_ms,
    status = as.integer(status),
    timed_out = timed_out,
    warnings = warnings
  )
}

render_source_preparation_command <- function(r_executable, arguments) {
  paste(
    vapply(c(r_executable, arguments), shQuote, character(1L)),
    collapse = " "
  )
}

source_preparation_attempt_from_process <- function(
  package,
  version,
  stage,
  process,
  logs,
  path_plan
) {
  validate_source_preparation_process(process)
  if (process$timed_out) {
    outcome <- "timeout"
    exit_status <- NA_character_
  } else if (process$status == 0L) {
    outcome <- "success"
    exit_status <- "0"
  } else {
    outcome <- "failure"
    exit_status <- as.character(process$status)
  }
  diagnostic <- if (identical(outcome, "success")) {
    NA_character_
  } else {
    source_preparation_diagnostic(process, logs)
  }
  run_root <- runtime_role_path(path_plan, "run")
  stdout <- validate_source_preparation_log(logs$stdout, run_root)
  stderr <- validate_source_preparation_log(logs$stderr, run_root)

  new_preparation_attempt(
    package,
    version,
    stage,
    process$command,
    process$started_at,
    process$duration_ms,
    exit_status,
    outcome,
    source_preparation_relative_path(stdout, run_root),
    digest::digest(stdout, algo = "sha256", file = TRUE, serialize = FALSE),
    source_preparation_relative_path(stderr, run_root),
    digest::digest(stderr, algo = "sha256", file = TRUE, serialize = FALSE),
    diagnostic
  )
}

validate_source_preparation_process <- function(process) {
  if (
    !is.list(process) ||
      !identical(
        names(process),
        c(
          "command",
          "started_at",
          "duration_ms",
          "status",
          "timed_out",
          "warnings"
        )
      )
  ) {
    stop(
      "Source preparation process result has an invalid structure.",
      call. = FALSE
    )
  }
  validate_contract_text(process$command, "command")
  validate_preparation_timestamp(process$started_at)
  normalize_contract_integer(process$duration_ms, "duration_ms")
  if (
    !is.integer(process$status) ||
      length(process$status) != 1L ||
      is.na(process$status) ||
      process$status < 0L ||
      !is.logical(process$timed_out) ||
      length(process$timed_out) != 1L ||
      is.na(process$timed_out) ||
      !is.character(process$warnings) ||
      anyNA(process$warnings) ||
      (process$timed_out && process$status != 124L)
  ) {
    stop("Source preparation process result has invalid values.", call. = FALSE)
  }

  invisible(process)
}

source_preparation_diagnostic <- function(process, logs) {
  sections <- character()
  for (stream in c("stdout", "stderr")) {
    text <- source_preparation_log_tail(logs[[stream]], 4096L)
    if (nzchar(text)) {
      sections <- c(sections, paste0(stream, ":\n", text))
    }
  }
  fallback <- if (process$timed_out) {
    "R package preparation timed out."
  } else {
    sprintf("R package preparation exited with status %d.", process$status)
  }
  if (length(process$warnings) > 0L) {
    sections <- c(sections, paste(process$warnings, collapse = "\n"))
  }
  excerpt <- if (length(sections) == 0L) {
    fallback
  } else {
    paste(c(fallback, sections), collapse = "\n\n")
  }
  source_preparation_utf8_tail(excerpt, 4096L)
}

source_preparation_log_tail <- function(path, max_bytes) {
  size <- file.info(path, extra_cols = FALSE)$size
  if (is.na(size) || size == 0) {
    return("")
  }
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  offset <- max(0, size - max_bytes)
  seek(connection, where = offset, origin = "start")
  payload <- readBin(connection, what = "raw", n = max_bytes)
  text <- rawToChar(payload)
  text <- iconv(text, from = "", to = "UTF-8", sub = "")
  if (is.na(text)) "" else text
}

source_preparation_utf8_tail <- function(text, max_bytes) {
  text <- enc2utf8(text)
  while (length(charToRaw(text)) > max_bytes) {
    text <- substring(text, 2L)
  }
  text
}

source_preparation_binary_output <- function(attempt_root) {
  candidates <- list.files(
    attempt_root,
    pattern = "\\.(?:tar\\.(?:gz|bz2|xz)|tgz|zip)$",
    full.names = TRUE,
    recursive = FALSE,
    ignore.case = TRUE
  )
  if (length(candidates) != 1L) {
    stop(
      "Source preparation must produce exactly one binary archive.",
      call. = FALSE
    )
  }
  if (warehouse_path_is_link(candidates)) {
    stop(
      "Source preparation binary must not be a symbolic link.",
      call. = FALSE
    )
  }
  path <- normalizePath(candidates, winslash = "/", mustWork = TRUE)
  if (
    !path_is_within(attempt_root, path) ||
      !utils::file_test("-f", path) ||
      dir.exists(path)
  ) {
    stop("Source preparation binary escapes its attempt root.", call. = FALSE)
  }

  path
}

source_preparation_binary_artifact <- function(
  binary_path,
  package,
  version,
  lane
) {
  sha256 <- digest::digest(
    binary_path,
    algo = "sha256",
    file = TRUE,
    serialize = FALSE
  )
  artifact <- new_artifact_identity(
    package,
    version,
    sha256,
    "binary",
    lane
  )
  validate_source_preparation_binary_payload(binary_path, artifact, lane)
  artifact
}

validate_source_preparation_binary_payload <- function(path, artifact, lane) {
  metadata <- validate_warehouse_archive(
    path,
    artifact,
    archive_name = basename(path)
  )
  r_major_minor <- built_r_major_minor(metadata$built)
  if (
    length(r_major_minor) != 1L ||
      is.na(r_major_minor) ||
      !identical(r_major_minor, lane$r_major_minor) ||
      is.na(metadata$platform) ||
      !identical(metadata$platform, lane$r_platform)
  ) {
    stop(
      "Source preparation binary does not match its compatibility lane.",
      call. = FALSE
    )
  }

  invisible(metadata)
}

validate_source_preparation_binary <- function(
  binary_path,
  artifact,
  lane,
  path_plan
) {
  binary_path <- normalize_warehouse_source(binary_path, path_plan)
  validate_source_preparation_binary_payload(binary_path, artifact, lane)
  invisible(binary_path)
}

source_preparation_result <- function(
  package,
  version,
  outcome,
  artifact = NULL,
  attempt = NULL
) {
  diagnostic <- if (is.null(attempt)) NA_character_ else
    attempt$diagnostic_excerpt
  data.frame(
    package = package,
    version = version,
    outcome = outcome,
    artifact_id = if (is.null(artifact)) NA_character_ else
      artifact$artifact_id,
    evidence_attempt_id = if (is.null(attempt)) NA_character_ else
      attempt$attempt_id,
    blocking_dependency = NA_character_,
    diagnostic_excerpt = diagnostic,
    stringsAsFactors = FALSE
  )
}

new_source_preparation_bundle <- function(
  package,
  version,
  source_acquisition,
  binary_artifact,
  binary_path,
  attempts,
  result,
  promotion
) {
  list(
    package = package,
    version = version,
    source_acquisition = source_acquisition,
    binary_artifact = binary_artifact,
    binary_path = binary_path,
    attempts = attempts,
    result = result,
    promotion = promotion
  )
}

validate_source_preparation_logs <- function(attempts, path_plan) {
  run_root <- runtime_role_path(path_plan, "run")
  for (attempt in attempts) {
    for (stream in c("stdout", "stderr")) {
      relative_path <- attempt[[paste0(stream, "_path")]]
      path <- file.path(run_root, relative_path)
      path <- validate_source_preparation_log(path, run_root)
      observed <- digest::digest(
        path,
        algo = "sha256",
        file = TRUE,
        serialize = FALSE
      )
      if (!identical(observed, attempt[[paste0(stream, "_sha256")]])) {
        stop(
          "Source preparation log does not match its SHA-256.",
          call. = FALSE
        )
      }
    }
  }

  invisible(NULL)
}

validate_source_preparation_log <- function(path, run_root) {
  if (
    warehouse_path_is_link(path) ||
      !file.exists(path) ||
      !utils::file_test("-f", path) ||
      dir.exists(path)
  ) {
    stop("Source preparation log must be one regular file.", call. = FALSE)
  }
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (!path_is_within(run_root, path) || file.access(path, mode = 4L) != 0L) {
    stop("Source preparation log escapes its run root.", call. = FALSE)
  }

  path
}

source_preparation_relative_path <- function(path, run_root) {
  if (!path_is_within(run_root, path) || identical(path, run_root)) {
    stop("Source preparation evidence escapes its run root.", call. = FALSE)
  }
  substring(path, nchar(run_root) + 2L)
}

validate_source_preparation_promotion <- function(
  promotion,
  binary_path,
  artifact,
  path_plan
) {
  fields <- c(
    "artifact_id",
    "source_path",
    "warehouse_path",
    "transfer_policy",
    "reused"
  )
  if (
    !inherits(promotion, "revdeprunner_warehouse_promotion") ||
      !is.list(promotion) ||
      !identical(names(promotion), fields) ||
      !is.logical(promotion$reused) ||
      length(promotion$reused) != 1L ||
      is.na(promotion$reused) ||
      !identical(promotion$artifact_id, artifact$artifact_id) ||
      !identical(promotion$source_path, binary_path) ||
      !identical(promotion$transfer_policy, "copy")
  ) {
    stop("Source preparation promotion is inconsistent.", call. = FALSE)
  }
  expected <- source_acquisition_warehouse_path(path_plan, artifact)
  if (!identical(promotion$warehouse_path, expected)) {
    stop("Source preparation warehouse path is inconsistent.", call. = FALSE)
  }
  validate_existing_warehouse_artifact(
    promotion$warehouse_path,
    artifact,
    runtime_role_path(path_plan, "warehouse")
  )

  invisible(promotion)
}
