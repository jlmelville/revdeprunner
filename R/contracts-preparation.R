preparation_attempt_schema_version <- function() {
  "revdeprunner-preparation-attempt/v1"
}

preparation_report_schema_version <- function() {
  "revdeprunner-preparation-report/v1"
}

preparation_attempt_stages <- function() {
  c("build", "install")
}

preparation_attempt_outcomes <- function() {
  c("success", "failure", "timeout")
}

preparation_result_outcomes <- function() {
  c(
    "prepared",
    "unavailable",
    "compilation-failure",
    "installation-failure",
    "timeout",
    "blocked"
  )
}

new_preparation_attempt <- function(
  package,
  version,
  stage,
  command,
  started_at,
  duration_ms,
  exit_status,
  outcome,
  stdout_path,
  stdout_sha256,
  stderr_path,
  stderr_sha256,
  diagnostic_excerpt = NA_character_
) {
  package <- validate_package_name(package)
  version <- validate_package_version(version)
  stage <- validate_preparation_attempt_stage(stage)
  command <- validate_contract_text(command, "command")
  started_at <- validate_preparation_timestamp(started_at)
  duration_ms <- normalize_contract_integer(duration_ms, "duration_ms")
  exit_status <- normalize_preparation_exit_status(exit_status)
  outcome <- validate_preparation_attempt_outcome(outcome)
  stdout_path <- validate_preparation_log_path(stdout_path, "stdout_path")
  stdout_sha256 <- validate_sha256(stdout_sha256, "stdout_sha256")
  stderr_path <- validate_preparation_log_path(stderr_path, "stderr_path")
  stderr_sha256 <- validate_sha256(stderr_sha256, "stderr_sha256")
  diagnostic_excerpt <- validate_preparation_diagnostic(
    diagnostic_excerpt,
    "diagnostic_excerpt"
  )
  validate_preparation_attempt_semantics(
    outcome,
    exit_status,
    diagnostic_excerpt
  )
  if (identical(stdout_path, stderr_path)) {
    stop("Preparation stdout and stderr paths must differ.", call. = FALSE)
  }

  schema_version <- preparation_attempt_schema_version()
  fields <- c(
    package = package,
    version = version,
    stage = stage,
    command = command,
    started_at = started_at,
    duration_ms = duration_ms,
    exit_status = exit_status,
    outcome = outcome,
    stdout_path = stdout_path,
    stdout_sha256 = stdout_sha256,
    stderr_path = stderr_path,
    stderr_sha256 = stderr_sha256,
    diagnostic_excerpt = encode_contract_cell(diagnostic_excerpt)
  )
  attempt <- structure(
    list(
      schema_version = schema_version,
      attempt_id = record_identity(schema_version, fields),
      package = package,
      version = version,
      stage = stage,
      command = command,
      started_at = started_at,
      duration_ms = duration_ms,
      exit_status = exit_status,
      outcome = outcome,
      stdout_path = stdout_path,
      stdout_sha256 = stdout_sha256,
      stderr_path = stderr_path,
      stderr_sha256 = stderr_sha256,
      diagnostic_excerpt = diagnostic_excerpt
    ),
    class = "revdeprunner_preparation_attempt"
  )
  validate_preparation_attempt(attempt)
  attempt
}

validate_preparation_attempt <- function(attempt) {
  validate_contract_record(
    attempt,
    c(
      "schema_version",
      "attempt_id",
      "package",
      "version",
      "stage",
      "command",
      "started_at",
      "duration_ms",
      "exit_status",
      "outcome",
      "stdout_path",
      "stdout_sha256",
      "stderr_path",
      "stderr_sha256",
      "diagnostic_excerpt"
    ),
    "revdeprunner_preparation_attempt",
    "preparation attempt",
    allow_na = c("exit_status", "diagnostic_excerpt")
  )
  if (
    !identical(attempt$schema_version, preparation_attempt_schema_version())
  ) {
    stop("Preparation attempt schema version is unsupported.", call. = FALSE)
  }

  validate_sha256_identity(attempt$attempt_id, "attempt_id")
  validate_package_name(attempt$package)
  validate_package_version(attempt$version)
  validate_preparation_attempt_stage(attempt$stage)
  validate_contract_text(attempt$command, "command")
  validate_preparation_timestamp(attempt$started_at)
  duration_ms <- normalize_contract_integer(attempt$duration_ms, "duration_ms")
  exit_status <- normalize_preparation_exit_status(attempt$exit_status)
  validate_preparation_attempt_outcome(attempt$outcome)
  validate_preparation_log_path(attempt$stdout_path, "stdout_path")
  validate_sha256(attempt$stdout_sha256, "stdout_sha256")
  validate_preparation_log_path(attempt$stderr_path, "stderr_path")
  validate_sha256(attempt$stderr_sha256, "stderr_sha256")
  diagnostic_excerpt <- validate_preparation_diagnostic(
    attempt$diagnostic_excerpt,
    "diagnostic_excerpt"
  )
  if (!identical(attempt$duration_ms, duration_ms)) {
    stop("Preparation attempt duration is not normalized.", call. = FALSE)
  }
  if (!identical(attempt$exit_status, exit_status)) {
    stop("Preparation attempt exit status is not normalized.", call. = FALSE)
  }
  validate_preparation_attempt_semantics(
    attempt$outcome,
    attempt$exit_status,
    diagnostic_excerpt
  )
  if (identical(attempt$stdout_path, attempt$stderr_path)) {
    stop("Preparation stdout and stderr paths must differ.", call. = FALSE)
  }

  fields <- preparation_attempt_identity_fields(attempt)
  expected <- record_identity(attempt$schema_version, fields)
  if (!identical(attempt$attempt_id, expected)) {
    stop(
      "Preparation attempt identity does not match its fields.",
      call. = FALSE
    )
  }

  invisible(attempt)
}

new_preparation_report <- function(
  universe,
  cohort,
  snapshot,
  lane,
  artifacts,
  sources,
  attempts,
  results
) {
  validate_repository_snapshot(snapshot)
  validate_reverse_dependency_cohort(cohort, snapshot)
  validate_dependency_universe(universe, cohort, snapshot)
  validate_compatibility_lane(lane)
  requirements <- derive_preparation_requirements(universe)
  artifacts <- normalize_preparation_artifacts(artifacts, lane)
  sources <- normalize_preparation_sources(
    sources,
    requirements,
    artifacts
  )
  attempts <- normalize_preparation_attempts(attempts, requirements)
  results <- normalize_preparation_results(
    results,
    requirements,
    artifacts,
    attempts,
    universe
  )
  validate_preparation_artifact_references(artifacts, sources, results)
  schema_version <- preparation_report_schema_version()
  fields <- preparation_report_identity_fields(
    snapshot$snapshot_id,
    cohort$cohort_id,
    universe$universe_id,
    lane$lane_id,
    requirements,
    artifacts,
    sources,
    attempts,
    results
  )

  report <- structure(
    list(
      schema_version = schema_version,
      report_id = record_identity(schema_version, fields),
      snapshot_id = snapshot$snapshot_id,
      cohort_id = cohort$cohort_id,
      universe_id = universe$universe_id,
      lane_id = lane$lane_id,
      requirements = requirements,
      artifacts = artifacts,
      sources = sources,
      attempts = attempts,
      results = results
    ),
    class = "revdeprunner_preparation_report"
  )
  validate_preparation_report(report, universe, cohort, snapshot, lane)
  report
}

validate_preparation_report <- function(
  report,
  universe,
  cohort,
  snapshot,
  lane
) {
  validate_composite_contract_record(
    report,
    c(
      "schema_version",
      "report_id",
      "snapshot_id",
      "cohort_id",
      "universe_id",
      "lane_id",
      "requirements",
      "artifacts",
      "sources",
      "attempts",
      "results"
    ),
    "revdeprunner_preparation_report",
    "preparation report"
  )
  if (!identical(report$schema_version, preparation_report_schema_version())) {
    stop("Preparation report schema version is unsupported.", call. = FALSE)
  }

  validate_sha256_identity(report$report_id, "report_id")
  validate_sha256_identity(report$snapshot_id, "snapshot_id")
  validate_sha256_identity(report$cohort_id, "cohort_id")
  validate_sha256_identity(report$universe_id, "universe_id")
  validate_sha256_identity(report$lane_id, "lane_id")
  validate_repository_snapshot(snapshot)
  validate_reverse_dependency_cohort(cohort, snapshot)
  validate_dependency_universe(universe, cohort, snapshot)
  validate_compatibility_lane(lane)
  if (!identical(report$snapshot_id, snapshot$snapshot_id)) {
    stop("Preparation report does not belong to this snapshot.", call. = FALSE)
  }
  if (!identical(report$cohort_id, cohort$cohort_id)) {
    stop("Preparation report does not belong to this cohort.", call. = FALSE)
  }
  if (!identical(report$universe_id, universe$universe_id)) {
    stop("Preparation report does not belong to this universe.", call. = FALSE)
  }
  if (!identical(report$lane_id, lane$lane_id)) {
    stop("Preparation report does not belong to this lane.", call. = FALSE)
  }

  requirements <- derive_preparation_requirements(universe)
  if (!identical(report$requirements, requirements)) {
    stop(
      "Preparation report requirements do not match its universe.",
      call. = FALSE
    )
  }
  artifacts <- normalize_preparation_artifact_table(report$artifacts, lane)
  if (!identical(report$artifacts, artifacts)) {
    stop("Preparation report artifacts are not normalized.", call. = FALSE)
  }
  sources <- normalize_preparation_sources(
    report$sources,
    requirements,
    artifacts
  )
  if (!identical(report$sources, sources)) {
    stop("Preparation report sources are not normalized.", call. = FALSE)
  }
  attempts <- normalize_preparation_attempt_table(
    report$attempts,
    requirements
  )
  if (!identical(report$attempts, attempts)) {
    stop("Preparation report attempts are not normalized.", call. = FALSE)
  }
  results <- normalize_preparation_results(
    report$results,
    requirements,
    artifacts,
    attempts,
    universe
  )
  if (!identical(report$results, results)) {
    stop("Preparation report results are not normalized.", call. = FALSE)
  }
  validate_preparation_artifact_references(artifacts, sources, results)

  fields <- preparation_report_identity_fields(
    report$snapshot_id,
    report$cohort_id,
    report$universe_id,
    report$lane_id,
    report$requirements,
    report$artifacts,
    report$sources,
    report$attempts,
    report$results
  )
  expected <- record_identity(report$schema_version, fields)
  if (!identical(report$report_id, expected)) {
    stop(
      "Preparation report identity does not match its fields.",
      call. = FALSE
    )
  }

  invisible(report)
}

validate_preparation_attempt_stage <- function(stage) {
  stage <- validate_contract_text(stage, "stage")
  if (!stage %in% preparation_attempt_stages()) {
    stop("Preparation attempt stage is unsupported.", call. = FALSE)
  }
  stage
}

validate_preparation_attempt_outcome <- function(outcome) {
  outcome <- validate_contract_text(outcome, "outcome")
  if (!outcome %in% preparation_attempt_outcomes()) {
    stop("Preparation attempt outcome is unsupported.", call. = FALSE)
  }
  outcome
}

validate_preparation_result_outcome <- function(outcome) {
  outcome <- validate_contract_text(outcome, "outcome")
  if (!outcome %in% preparation_result_outcomes()) {
    stop("Preparation result outcome is unsupported.", call. = FALSE)
  }
  outcome
}

validate_preparation_timestamp <- function(started_at) {
  started_at <- validate_contract_text(started_at, "started_at")
  pattern <- paste0(
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}T",
    "([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]",
    "(\\.[0-9]{1,9})?Z$"
  )
  if (!grepl(pattern, started_at)) {
    stop("`started_at` must be an ISO 8601 UTC timestamp.", call. = FALSE)
  }
  parsed <- as.POSIXct(started_at, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  if (is.na(parsed)) {
    stop("`started_at` must be an ISO 8601 UTC timestamp.", call. = FALSE)
  }
  started_at
}

normalize_contract_integer <- function(value, argument, allow_na = FALSE) {
  if (length(value) != 1L || is.complex(value)) {
    stop(
      sprintf("`%s` must be one non-negative integer.", argument),
      call. = FALSE
    )
  }
  if (is.na(value)) {
    if (allow_na) {
      return(NA_character_)
    }
    stop(
      sprintf("`%s` must be one non-negative integer.", argument),
      call. = FALSE
    )
  }
  if (is.character(value)) {
    if (!grepl("^(0|[1-9][0-9]*)$", value)) {
      stop(
        sprintf("`%s` must be one non-negative integer.", argument),
        call. = FALSE
      )
    }
    numeric_value <- suppressWarnings(as.numeric(value))
  } else if (is.numeric(value)) {
    numeric_value <- value
  } else {
    stop(
      sprintf("`%s` must be one non-negative integer.", argument),
      call. = FALSE
    )
  }
  if (
    !is.finite(numeric_value) ||
      numeric_value < 0 ||
      numeric_value != floor(numeric_value) ||
      numeric_value > .Machine$integer.max
  ) {
    stop(
      sprintf("`%s` must be one non-negative integer.", argument),
      call. = FALSE
    )
  }
  format(numeric_value, scientific = FALSE, trim = TRUE)
}

normalize_preparation_exit_status <- function(exit_status) {
  normalize_contract_integer(exit_status, "exit_status", allow_na = TRUE)
}

validate_preparation_log_path <- function(path, argument) {
  path <- validate_contract_text(path, argument)
  if (
    startsWith(path, "/") ||
      grepl("^[A-Za-z]:", path) ||
      grepl("\\\\", path) ||
      grepl("//", path, fixed = TRUE)
  ) {
    stop(
      sprintf("`%s` must be a portable relative artifact path.", argument),
      call. = FALSE
    )
  }
  parts <- strsplit(path, "/", fixed = TRUE)[[1L]]
  if (
    any(parts %in% c("", ".", "..")) ||
      any(!grepl("^[A-Za-z0-9][A-Za-z0-9._+-]*$", parts))
  ) {
    stop(
      sprintf("`%s` must be a portable relative artifact path.", argument),
      call. = FALSE
    )
  }
  path
}

validate_preparation_diagnostic <- function(value, argument) {
  if (length(value) != 1L || !is.character(value)) {
    stop(sprintf("`%s` must be one string or `NA`.", argument), call. = FALSE)
  }
  if (is.na(value)) {
    return(NA_character_)
  }
  value <- enc2utf8(value)
  if (!nzchar(value)) {
    stop(
      sprintf("`%s` must be one non-empty string or `NA`.", argument),
      call. = FALSE
    )
  }
  if (length(charToRaw(value)) > 4096L) {
    stop(
      sprintf("`%s` must not exceed 4,096 UTF-8 bytes.", argument),
      call. = FALSE
    )
  }
  value
}

validate_preparation_attempt_semantics <- function(
  outcome,
  exit_status,
  diagnostic_excerpt
) {
  if (identical(outcome, "success")) {
    if (!identical(exit_status, "0") || !is.na(diagnostic_excerpt)) {
      stop(
        "Successful preparation attempts require status 0 and no diagnostic excerpt.",
        call. = FALSE
      )
    }
  } else if (identical(outcome, "failure")) {
    if (
      is.na(exit_status) ||
        identical(exit_status, "0") ||
        is.na(diagnostic_excerpt)
    ) {
      stop(
        "Failed preparation attempts require a nonzero status and diagnostic excerpt.",
        call. = FALSE
      )
    }
  } else if (!is.na(exit_status) || is.na(diagnostic_excerpt)) {
    stop(
      "Timed-out preparation attempts require an absent status and diagnostic excerpt.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

preparation_attempt_identity_fields <- function(attempt) {
  c(
    package = attempt$package,
    version = attempt$version,
    stage = attempt$stage,
    command = attempt$command,
    started_at = attempt$started_at,
    duration_ms = attempt$duration_ms,
    exit_status = attempt$exit_status,
    outcome = attempt$outcome,
    stdout_path = attempt$stdout_path,
    stdout_sha256 = attempt$stdout_sha256,
    stderr_path = attempt$stderr_path,
    stderr_sha256 = attempt$stderr_sha256,
    diagnostic_excerpt = encode_contract_cell(attempt$diagnostic_excerpt)
  )
}

derive_preparation_requirements <- function(universe) {
  targets <- data.frame(
    target = universe$targets$package,
    package = universe$targets$package,
    version = universe$targets$version,
    role = rep("target", nrow(universe$targets)),
    disposition = rep("target", nrow(universe$targets)),
    stringsAsFactors = FALSE
  )
  dependencies <- preparation_required_dependencies(universe)
  closure <- data.frame(
    target = dependencies$target,
    package = dependencies$dependency,
    version = dependencies$version,
    role = rep("closure", nrow(dependencies)),
    disposition = dependencies$disposition,
    stringsAsFactors = FALSE
  )
  requirements <- rbind(targets, closure)
  if (nrow(requirements) == 0L) {
    return(empty_preparation_requirements())
  }
  requirements <- unique(requirements)
  requirements <- requirements[
    order(
      requirements$target,
      requirements$package,
      requirements$role,
      requirements$disposition,
      method = "radix"
    ),
    ,
    drop = FALSE
  ]
  rownames(requirements) <- NULL
  requirements
}

preparation_required_dependencies <- function(universe) {
  dependencies <- universe$dependencies[
    universe$dependencies$disposition %in% c("install", "unavailable"),
    ,
    drop = FALSE
  ]
  optional <- preparation_optional_unavailable_keys(universe)
  keys <- preparation_dependency_keys(
    dependencies$target,
    dependencies$dependency
  )
  dependencies[!keys %in% optional, , drop = FALSE]
}

preparation_required_dependency_edges <- function(universe) {
  optional <- preparation_optional_unavailable_keys(universe)
  keys <- preparation_dependency_keys(
    universe$edges$target,
    universe$edges$dependency
  )
  universe$edges[!keys %in% optional, , drop = FALSE]
}

preparation_optional_unavailable_keys <- function(universe) {
  unavailable <- universe$dependencies[
    universe$dependencies$disposition == "unavailable",
    c("target", "dependency"),
    drop = FALSE
  ]
  optional <- vapply(
    seq_len(nrow(unavailable)),
    function(index) {
      relationships <- universe$edges$relationship[
        universe$edges$target == unavailable$target[[index]] &
          universe$edges$dependency == unavailable$dependency[[index]]
      ]
      length(relationships) > 0L && all(relationships == "Suggests")
    },
    logical(1L)
  )
  preparation_dependency_keys(
    unavailable$target[optional],
    unavailable$dependency[optional]
  )
}

preparation_dependency_keys <- function(target, dependency) {
  paste(target, dependency, sep = "\r")
}

empty_preparation_requirements <- function() {
  data.frame(
    target = character(),
    package = character(),
    version = character(),
    role = character(),
    disposition = character(),
    stringsAsFactors = FALSE
  )
}

preparation_required_packages <- function(requirements) {
  if (nrow(requirements) == 0L) {
    return(data.frame(
      package = character(),
      version = character(),
      stringsAsFactors = FALSE
    ))
  }
  names <- sort(unique(requirements$package), method = "radix")
  versions <- vapply(
    names,
    function(package) {
      values <- unique(requirements$version[requirements$package == package])
      present <- values[!is.na(values)]
      if (length(present) > 1L || (length(present) == 1L && anyNA(values))) {
        stop(
          "Preparation requirements contain inconsistent package versions.",
          call. = FALSE
        )
      }
      if (length(present) == 0L) NA_character_ else present[[1L]]
    },
    character(1L)
  )
  data.frame(
    package = names,
    version = unname(versions),
    stringsAsFactors = FALSE
  )
}

preparation_artifact_table_fields <- function() {
  c(
    "schema_version",
    "artifact_id",
    "package",
    "version",
    "archive_type",
    "sha256",
    "lane_id"
  )
}

normalize_preparation_artifacts <- function(artifacts, lane) {
  if (!is.list(artifacts)) {
    stop("`artifacts` must be a list of artifact identities.", call. = FALSE)
  }
  if (length(artifacts) == 0L) {
    values <- stats::setNames(
      replicate(
        length(preparation_artifact_table_fields()),
        character(),
        simplify = FALSE
      ),
      preparation_artifact_table_fields()
    )
    return(as.data.frame(values, stringsAsFactors = FALSE, optional = TRUE))
  }
  rows <- lapply(artifacts, function(artifact) {
    validate_artifact_identity(artifact)
    if (
      identical(artifact$archive_type, "binary") &&
        !identical(artifact$lane_id, lane$lane_id)
    ) {
      stop(
        "A preparation binary artifact belongs to another lane.",
        call. = FALSE
      )
    }
    as.data.frame(unclass(artifact), stringsAsFactors = FALSE, optional = TRUE)
  })
  table <- do.call(rbind, rows)
  rownames(table) <- NULL
  normalize_preparation_artifact_table(table, lane)
}

normalize_preparation_artifact_table <- function(artifacts, lane) {
  artifacts <- validate_preparation_table(
    artifacts,
    preparation_artifact_table_fields(),
    "artifacts",
    allow_na = "lane_id"
  )
  if (anyDuplicated(artifacts$artifact_id)) {
    stop("Preparation artifacts must have unique identities.", call. = FALSE)
  }
  for (row in seq_len(nrow(artifacts))) {
    artifact <- structure(
      as.list(artifacts[row, , drop = FALSE]),
      class = "revdeprunner_artifact_identity"
    )
    validate_artifact_identity(artifact)
    if (
      identical(artifact$archive_type, "binary") &&
        !identical(artifact$lane_id, lane$lane_id)
    ) {
      stop(
        "A preparation binary artifact belongs to another lane.",
        call. = FALSE
      )
    }
  }
  artifacts <- artifacts[
    order(artifacts$artifact_id, method = "radix"),
    ,
    drop = FALSE
  ]
  rownames(artifacts) <- NULL
  artifacts
}

preparation_source_fields <- function() {
  c(
    "package",
    "version",
    "source_origin",
    "source_url",
    "sha256",
    "artifact_id",
    "needs_compilation",
    "system_requirements"
  )
}

normalize_preparation_sources <- function(sources, requirements, artifacts) {
  sources <- validate_preparation_table(
    sources,
    preparation_source_fields(),
    "sources",
    allow_na = "system_requirements"
  )
  required <- preparation_required_packages(requirements)
  available <- required[!is.na(required$version), , drop = FALSE]
  if (
    any(!sources$package %in% available$package) ||
      anyDuplicated(sources$package)
  ) {
    stop(
      paste(
        "Preparation sources must identify available required packages",
        "uniquely."
      ),
      call. = FALSE
    )
  }
  for (row in seq_len(nrow(sources))) {
    package <- validate_package_name(sources$package[[row]])
    version <- validate_package_version(sources$version[[row]])
    expected_version <- available$version[match(package, available$package)]
    if (!identical(version, expected_version)) {
      stop(
        "Preparation source version does not match its requirement.",
        call. = FALSE
      )
    }
    if (!sources$source_origin[[row]] %in% c("repository", "archive")) {
      stop("Preparation source origin is unsupported.", call. = FALSE)
    }
    validate_preparation_source_url(sources$source_url[[row]])
    validate_sha256(sources$sha256[[row]], "sha256")
    validate_sha256_identity(sources$artifact_id[[row]], "artifact_id")
    if (!sources$needs_compilation[[row]] %in% c("yes", "no", "unknown")) {
      stop("Preparation compilation requirement is unsupported.", call. = FALSE)
    }
    validate_preparation_optional_text(
      sources$system_requirements[[row]],
      "system_requirements"
    )
    artifact <- artifacts[
      artifacts$artifact_id == sources$artifact_id[[row]],
      ,
      drop = FALSE
    ]
    if (
      nrow(artifact) != 1L ||
        !identical(artifact$archive_type[[1L]], "source") ||
        !identical(artifact$package[[1L]], package) ||
        !identical(artifact$version[[1L]], version) ||
        !identical(artifact$sha256[[1L]], sources$sha256[[row]])
    ) {
      stop(
        "Preparation source does not match its source artifact.",
        call. = FALSE
      )
    }
  }
  sources <- sources[order(sources$package, method = "radix"), , drop = FALSE]
  rownames(sources) <- NULL
  sources
}

validate_preparation_source_url <- function(source_url) {
  source_url <- validate_contract_text(source_url, "source_url")
  if (
    !grepl("^[A-Za-z][A-Za-z0-9+.-]*://[^[:space:]]+$", source_url) ||
      grepl("#", source_url, fixed = TRUE)
  ) {
    stop("`source_url` must be an absolute fragment-free URL.", call. = FALSE)
  }
  source_url
}

validate_preparation_optional_text <- function(value, argument) {
  if (!is.character(value) || length(value) != 1L) {
    stop(sprintf("`%s` must be one string or `NA`.", argument), call. = FALSE)
  }
  if (is.na(value)) {
    return(NA_character_)
  }
  value <- enc2utf8(value)
  if (!nzchar(value)) {
    stop(
      sprintf("`%s` must be one non-empty string or `NA`.", argument),
      call. = FALSE
    )
  }
  if (length(charToRaw(value)) > 4096L) {
    stop(
      sprintf("`%s` must not exceed 4,096 UTF-8 bytes.", argument),
      call. = FALSE
    )
  }
  value
}

preparation_attempt_table_fields <- function() {
  c(
    "schema_version",
    "attempt_id",
    "package",
    "version",
    "stage",
    "command",
    "started_at",
    "duration_ms",
    "exit_status",
    "outcome",
    "stdout_path",
    "stdout_sha256",
    "stderr_path",
    "stderr_sha256",
    "diagnostic_excerpt"
  )
}

normalize_preparation_attempts <- function(attempts, requirements) {
  if (!is.list(attempts)) {
    stop("`attempts` must be a list of preparation attempts.", call. = FALSE)
  }
  if (length(attempts) == 0L) {
    values <- stats::setNames(
      replicate(
        length(preparation_attempt_table_fields()),
        character(),
        simplify = FALSE
      ),
      preparation_attempt_table_fields()
    )
    values$exit_status <- rep(NA_character_, 0L)
    values$diagnostic_excerpt <- rep(NA_character_, 0L)
    return(as.data.frame(values, stringsAsFactors = FALSE, optional = TRUE))
  }
  rows <- lapply(attempts, function(attempt) {
    validate_preparation_attempt(attempt)
    as.data.frame(unclass(attempt), stringsAsFactors = FALSE, optional = TRUE)
  })
  table <- do.call(rbind, rows)
  rownames(table) <- NULL
  normalize_preparation_attempt_table(table, requirements)
}

normalize_preparation_attempt_table <- function(attempts, requirements) {
  attempts <- validate_preparation_table(
    attempts,
    preparation_attempt_table_fields(),
    "attempts",
    allow_na = c("exit_status", "diagnostic_excerpt")
  )
  required <- preparation_required_packages(requirements)
  if (anyDuplicated(attempts$attempt_id)) {
    stop("Preparation attempts must have unique identities.", call. = FALSE)
  }
  paths <- c(attempts$stdout_path, attempts$stderr_path)
  if (anyDuplicated(paths)) {
    stop("Preparation raw-log paths must be unique.", call. = FALSE)
  }
  for (row in seq_len(nrow(attempts))) {
    attempt <- structure(
      as.list(attempts[row, , drop = FALSE]),
      class = "revdeprunner_preparation_attempt"
    )
    validate_preparation_attempt(attempt)
    index <- match(attempt$package, required$package)
    if (
      is.na(index) ||
        is.na(required$version[[index]]) ||
        !identical(attempt$version, required$version[[index]])
    ) {
      stop(
        "Preparation attempt does not match a required package.",
        call. = FALSE
      )
    }
  }
  attempts <- attempts[
    order(
      attempts$package,
      attempts$version,
      attempts$started_at,
      attempts$stage,
      attempts$attempt_id,
      method = "radix"
    ),
    ,
    drop = FALSE
  ]
  rownames(attempts) <- NULL
  attempts
}

preparation_result_fields <- function() {
  c(
    "package",
    "version",
    "outcome",
    "artifact_id",
    "evidence_attempt_id",
    "blocking_dependency",
    "diagnostic_excerpt"
  )
}

normalize_preparation_results <- function(
  results,
  requirements,
  artifacts,
  attempts,
  universe
) {
  results <- validate_preparation_table(
    results,
    preparation_result_fields(),
    "results",
    allow_na = c(
      "version",
      "artifact_id",
      "evidence_attempt_id",
      "blocking_dependency",
      "diagnostic_excerpt"
    )
  )
  required <- preparation_required_packages(requirements)
  if (
    !identical(
      sort(results$package, method = "radix"),
      sort(required$package, method = "radix")
    ) ||
      anyDuplicated(results$package)
  ) {
    stop(
      "Preparation results must cover each required package once.",
      call. = FALSE
    )
  }
  for (row in seq_len(nrow(results))) {
    package <- validate_package_name(results$package[[row]])
    expected_version <- required$version[match(package, required$package)]
    if (!identical(results$version[[row]], expected_version)) {
      stop(
        "Preparation result version does not match its requirement.",
        call. = FALSE
      )
    }
    outcome <- validate_preparation_result_outcome(results$outcome[[row]])
    diagnostic <- validate_preparation_diagnostic(
      results$diagnostic_excerpt[[row]],
      "diagnostic_excerpt"
    )
    validate_preparation_result_row(
      results[row, , drop = FALSE],
      outcome,
      diagnostic,
      artifacts,
      attempts
    )
  }
  validate_preparation_blockers(results, universe)
  results <- results[order(results$package, method = "radix"), , drop = FALSE]
  rownames(results) <- NULL
  results
}

validate_preparation_result_row <- function(
  result,
  outcome,
  diagnostic,
  artifacts,
  attempts
) {
  package <- result$package[[1L]]
  version <- result$version[[1L]]
  artifact_id <- result$artifact_id[[1L]]
  attempt_id <- result$evidence_attempt_id[[1L]]
  blocker <- result$blocking_dependency[[1L]]

  artifact <- if (is.na(artifact_id)) {
    artifacts[FALSE, , drop = FALSE]
  } else {
    artifacts[artifacts$artifact_id == artifact_id, , drop = FALSE]
  }
  if (!is.na(artifact_id)) {
    validate_sha256_identity(artifact_id, "artifact_id")
    if (
      nrow(artifact) != 1L ||
        !identical(artifact$package[[1L]], package) ||
        !identical(artifact$version[[1L]], version) ||
        !identical(artifact$archive_type[[1L]], "binary")
    ) {
      stop(
        "Preparation result does not match its binary artifact.",
        call. = FALSE
      )
    }
  }
  attempt <- if (is.na(attempt_id)) {
    attempts[FALSE, , drop = FALSE]
  } else {
    attempts[attempts$attempt_id == attempt_id, , drop = FALSE]
  }
  if (!is.na(attempt_id)) {
    validate_sha256_identity(attempt_id, "evidence_attempt_id")
    if (
      nrow(attempt) != 1L ||
        !identical(attempt$package[[1L]], package) ||
        !identical(attempt$version[[1L]], version)
    ) {
      stop(
        "Preparation result does not match its evidence attempt.",
        call. = FALSE
      )
    }
  }

  if (identical(outcome, "prepared") && is.na(artifact_id)) {
    stop("Prepared results require a binary artifact.", call. = FALSE)
  }
  if (identical(outcome, "prepared")) {
    if (!is.na(blocker) || !is.na(diagnostic)) {
      stop("Prepared result fields are inconsistent.", call. = FALSE)
    }
    if (
      !is.na(attempt_id) &&
        (!identical(attempt$outcome[[1L]], "success") ||
          !identical(attempt$stage[[1L]], "build"))
    ) {
      stop("Prepared result evidence is inconsistent.", call. = FALSE)
    }
  } else if (identical(outcome, "unavailable")) {
    if (
      !is.na(version) ||
        !is.na(artifact_id) ||
        !is.na(attempt_id) ||
        !is.na(blocker) ||
        is.na(diagnostic)
    ) {
      stop("Unavailable result fields are inconsistent.", call. = FALSE)
    }
  } else if (identical(outcome, "blocked")) {
    if (
      !is.na(artifact_id) ||
        !is.na(attempt_id) ||
        is.na(blocker) ||
        is.na(diagnostic)
    ) {
      stop("Blocked result fields are inconsistent.", call. = FALSE)
    }
    validate_package_name(blocker)
  } else {
    validate_preparation_failure_result(
      outcome,
      artifact_id,
      attempt,
      blocker,
      diagnostic
    )
  }
  invisible(NULL)
}

validate_preparation_failure_result <- function(
  outcome,
  artifact_id,
  attempt,
  blocker,
  diagnostic
) {
  if (nrow(attempt) != 1L || !is.na(blocker) || is.na(diagnostic)) {
    stop("Preparation failure evidence is inconsistent.", call. = FALSE)
  }
  if (!identical(diagnostic, attempt$diagnostic_excerpt[[1L]])) {
    stop(
      "Preparation failure excerpt does not match its attempt.",
      call. = FALSE
    )
  }
  if (identical(outcome, "timeout")) {
    if (!identical(attempt$outcome[[1L]], "timeout")) {
      stop("Timeout result evidence is inconsistent.", call. = FALSE)
    }
    return(invisible(NULL))
  }
  if (!identical(attempt$outcome[[1L]], "failure")) {
    stop("Preparation failure evidence is inconsistent.", call. = FALSE)
  }
  allowed_stages <- switch(
    outcome,
    "compilation-failure" = "build",
    "installation-failure" = "install"
  )
  if (!attempt$stage[[1L]] %in% allowed_stages) {
    stop("Preparation failure stage is inconsistent.", call. = FALSE)
  }
  if (identical(outcome, "compilation-failure") && !is.na(artifact_id)) {
    stop(
      "This preparation failure must not identify a prepared artifact.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

validate_preparation_blockers <- function(results, universe) {
  blocked <- which(results$outcome == "blocked")
  if (length(blocked) == 0L) {
    return(invisible(NULL))
  }
  successful <- "prepared"
  for (row in blocked) {
    package <- results$package[[row]]
    blocker <- results$blocking_dependency[[row]]
    blocker_row <- match(blocker, results$package)
    if (
      is.na(blocker_row) ||
        identical(package, blocker) ||
        results$outcome[[blocker_row]] %in% successful ||
        !preparation_dependency_reachable(package, blocker, universe$edges)
    ) {
      stop("Preparation blocking dependency is inconsistent.", call. = FALSE)
    }
  }
  current <- stats::setNames(results$blocking_dependency, results$package)
  for (package in results$package[blocked]) {
    seen <- character()
    cursor <- package
    while (!is.na(current[[cursor]])) {
      if (cursor %in% seen) {
        stop(
          "Preparation blocking dependencies contain a cycle.",
          call. = FALSE
        )
      }
      seen <- c(seen, cursor)
      cursor <- current[[cursor]]
    }
  }
  invisible(NULL)
}

preparation_dependency_reachable <- function(package, blocker, edges) {
  roots <- unique(edges$target[
    edges$from_package == package | edges$dependency == package
  ])
  for (root in roots) {
    root_edges <- edges[edges$target == root, , drop = FALSE]
    pending <- package
    visited <- character()
    while (length(pending) > 0L) {
      current <- pending[[1L]]
      pending <- pending[-1L]
      if (current %in% visited) {
        next
      }
      visited <- c(visited, current)
      dependencies <- root_edges$dependency[
        root_edges$from_package == current
      ]
      if (blocker %in% dependencies) {
        return(TRUE)
      }
      pending <- c(pending, setdiff(dependencies, visited))
    }
  }
  FALSE
}

validate_preparation_artifact_references <- function(
  artifacts,
  sources,
  results
) {
  referenced <- unique(c(
    sources$artifact_id,
    results$artifact_id[!is.na(results$artifact_id)]
  ))
  if (
    !identical(
      sort(artifacts$artifact_id, method = "radix"),
      sort(referenced, method = "radix")
    )
  ) {
    stop("Preparation artifacts must all be referenced.", call. = FALSE)
  }
  invisible(NULL)
}

validate_preparation_table <- function(
  table,
  fields,
  argument,
  allow_na = character()
) {
  if (!is.data.frame(table) || !identical(names(table), fields)) {
    stop(
      sprintf("`%s` has an invalid table structure.", argument),
      call. = FALSE
    )
  }
  for (field in fields) {
    value <- table[[field]]
    if (!is.character(value) || (anyNA(value) && !field %in% allow_na)) {
      stop(
        sprintf("`%s` has invalid column types.", argument),
        call. = FALSE
      )
    }
    value[!is.na(value)] <- enc2utf8(value[!is.na(value)])
    table[[field]] <- value
  }
  rownames(table) <- NULL
  table
}

preparation_report_identity_fields <- function(
  snapshot_id,
  cohort_id,
  universe_id,
  lane_id,
  requirements,
  artifacts,
  sources,
  attempts,
  results
) {
  c(
    snapshot_id = snapshot_id,
    cohort_id = cohort_id,
    universe_id = universe_id,
    lane_id = lane_id,
    tabular_identity_fields("requirement", requirements),
    tabular_identity_fields("artifact", artifacts),
    tabular_identity_fields("source", sources),
    tabular_identity_fields("attempt", attempts),
    tabular_identity_fields("result", results)
  )
}
