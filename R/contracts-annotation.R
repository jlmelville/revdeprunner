# This file composes private helpers defined in other package source files.
# nolint start: object_usage_linter.

preparation_annotation_schema_version <- function() {
  "revdeprunner-preparation-annotation/v1"
}

preparation_annotation_ledger_schema_version <- function() {
  "revdeprunner-preparation-annotation-ledger/v1"
}

preparation_annotation_types <- function() {
  c("diagnosis", "remediation", "note")
}

preparation_annotation_author_kinds <- function() {
  c("human", "agent")
}

preparation_annotation_log_streams <- function() {
  c("stdout", "stderr")
}

preparation_annotation_package_managers <- function() {
  c("apt", "homebrew", "dnf", "yum", "apk", "pacman", "zypper", "other")
}

preparation_annotation_suggestion_bases <- function() {
  c("raw-log", "declared-system-requirements", "author-analysis")
}

preparation_annotation_suggestion_fields <- function() {
  c("package_manager", "command", "basis", "execution_policy")
}

empty_preparation_annotation_suggestions <- function() {
  values <- stats::setNames(
    replicate(
      length(preparation_annotation_suggestion_fields()),
      character(),
      simplify = FALSE
    ),
    preparation_annotation_suggestion_fields()
  )
  as.data.frame(values, stringsAsFactors = FALSE, optional = TRUE)
}

new_preparation_annotation <- function(
  report,
  universe,
  cohort,
  snapshot,
  lane,
  attempt_id,
  log_stream,
  recorded_at,
  author_kind,
  provenance,
  annotation_type,
  interpretation,
  suggestions = empty_preparation_annotation_suggestions()
) {
  validate_annotation_contract_context(
    report,
    universe,
    cohort,
    snapshot,
    lane
  )
  attempt <- annotation_report_attempt(report, attempt_id)
  log_stream <- validate_preparation_annotation_log_stream(log_stream)
  recorded_at <- validate_preparation_timestamp(recorded_at)
  author_kind <- validate_preparation_annotation_author_kind(author_kind)
  provenance <- validate_preparation_annotation_text(
    provenance,
    "provenance",
    1024L
  )
  annotation_type <- validate_preparation_annotation_type(annotation_type)
  interpretation <- validate_preparation_annotation_text(
    interpretation,
    "interpretation",
    4096L
  )
  suggestions <- normalize_preparation_annotation_suggestions(
    suggestions,
    annotation_type
  )
  log_sha256 <- attempt[[paste0(log_stream, "_sha256")]]
  schema_version <- preparation_annotation_schema_version()
  fields <- preparation_annotation_identity_fields(
    report$report_id,
    attempt$attempt_id,
    attempt$package,
    attempt$version,
    log_stream,
    log_sha256,
    recorded_at,
    author_kind,
    provenance,
    annotation_type,
    interpretation,
    suggestions
  )
  annotation <- structure(
    list(
      schema_version = schema_version,
      annotation_id = record_identity(schema_version, fields),
      preparation_report_id = report$report_id,
      attempt_id = attempt$attempt_id,
      package = attempt$package,
      version = attempt$version,
      log_stream = log_stream,
      log_sha256 = log_sha256,
      recorded_at = recorded_at,
      author_kind = author_kind,
      provenance = provenance,
      annotation_type = annotation_type,
      interpretation = interpretation,
      suggestions = suggestions
    ),
    class = "revdeprunner_preparation_annotation"
  )
  validate_preparation_annotation_record(annotation, report)
  annotation
}

validate_preparation_annotation <- function(
  annotation,
  report,
  universe,
  cohort,
  snapshot,
  lane
) {
  validate_annotation_contract_context(
    report,
    universe,
    cohort,
    snapshot,
    lane
  )
  validate_preparation_annotation_record(annotation, report)
  invisible(annotation)
}

new_preparation_annotation_ledger <- function(
  report,
  universe,
  cohort,
  snapshot,
  lane
) {
  validate_annotation_contract_context(
    report,
    universe,
    cohort,
    snapshot,
    lane
  )
  ledger <- new_preparation_annotation_ledger_record(
    report$report_id,
    previous_ledger_id = NA_character_,
    previous_annotation_count = "0",
    annotations = list()
  )
  validate_preparation_annotation_ledger_record(ledger, report)
  validate_preparation_annotation_ledger_snapshot(ledger, report)
  ledger
}

append_preparation_annotations <- function(
  ledger,
  annotations,
  report,
  universe,
  cohort,
  snapshot,
  lane,
  previous = NULL
) {
  validate_preparation_annotation_ledger(
    ledger,
    report,
    universe,
    cohort,
    snapshot,
    lane,
    previous
  )
  additions <- normalize_preparation_annotation_batch(annotations, report)
  if (length(additions) == 0L) {
    stop(
      "An annotation ledger append requires at least one annotation.",
      call. = FALSE
    )
  }
  existing_ids <- preparation_annotation_ids(ledger$annotations)
  addition_ids <- preparation_annotation_ids(additions)
  if (any(addition_ids %in% existing_ids)) {
    stop(
      "An annotation ledger cannot append an existing annotation.",
      call. = FALSE
    )
  }
  validate_preparation_annotation_chronology(ledger$annotations, additions)

  appended <- new_preparation_annotation_ledger_record(
    report$report_id,
    previous_ledger_id = ledger$ledger_id,
    previous_annotation_count = as.character(length(ledger$annotations)),
    annotations = c(ledger$annotations, additions)
  )
  validate_preparation_annotation_ledger(
    appended,
    report,
    universe,
    cohort,
    snapshot,
    lane,
    ledger
  )
  appended
}

validate_preparation_annotation_ledger <- function(
  ledger,
  report,
  universe,
  cohort,
  snapshot,
  lane,
  previous = NULL
) {
  validate_annotation_contract_context(
    report,
    universe,
    cohort,
    snapshot,
    lane
  )
  validate_preparation_annotation_ledger_record(ledger, report)
  validate_preparation_annotation_ledger_snapshot(ledger, report)

  if (is.na(ledger$previous_ledger_id)) {
    if (!is.null(previous)) {
      stop(
        "A genesis annotation ledger must not have a predecessor.",
        call. = FALSE
      )
    }
    return(invisible(ledger))
  }

  if (is.null(previous)) {
    stop(
      "A non-genesis annotation ledger requires its predecessor.",
      call. = FALSE
    )
  }
  validate_preparation_annotation_ledger_record(previous, report)
  validate_preparation_annotation_ledger_snapshot(previous, report)
  if (!identical(ledger$previous_ledger_id, previous$ledger_id)) {
    stop(
      "Annotation ledger predecessor identity does not match.",
      call. = FALSE
    )
  }
  previous_count <- length(previous$annotations)
  if (
    !identical(ledger$previous_annotation_count, as.character(previous_count))
  ) {
    stop("Annotation ledger predecessor count does not match.", call. = FALSE)
  }
  if (length(ledger$annotations) <= previous_count) {
    stop("An annotation ledger append must add an annotation.", call. = FALSE)
  }
  if (
    previous_count > 0L &&
      !identical(
        ledger$annotations[seq_len(previous_count)],
        previous$annotations
      )
  ) {
    stop("Annotation ledger history is not an exact prefix.", call. = FALSE)
  }

  invisible(ledger)
}

validate_preparation_annotation_ledger_snapshot <- function(ledger, report) {
  if (is.na(ledger$previous_ledger_id)) {
    if (
      !identical(ledger$previous_annotation_count, "0") ||
        length(ledger$annotations) != 0L
    ) {
      stop("A genesis annotation ledger must be empty.", call. = FALSE)
    }
    return(invisible(ledger))
  }

  previous_count <- as.integer(ledger$previous_annotation_count)
  additions <- ledger$annotations[
    seq.int(previous_count + 1L, length(ledger$annotations))
  ]
  normalized_additions <- normalize_preparation_annotation_batch(
    additions,
    report
  )
  if (!identical(additions, normalized_additions)) {
    stop("Annotation ledger additions are not normalized.", call. = FALSE)
  }
  prefix <- if (previous_count == 0L) {
    list()
  } else {
    ledger$annotations[seq_len(previous_count)]
  }
  validate_preparation_annotation_chronology(prefix, additions)
  invisible(ledger)
}

validate_annotation_contract_context <- function(
  report,
  universe,
  cohort,
  snapshot,
  lane
) {
  validate_preparation_report(report, universe, cohort, snapshot, lane)
  invisible(NULL)
}

annotation_report_attempt <- function(report, attempt_id) {
  validate_sha256_identity(attempt_id, "attempt_id")
  index <- which(report$attempts$attempt_id == attempt_id)
  if (length(index) != 1L) {
    stop(
      "Annotation attempt identity is absent from the preparation report.",
      call. = FALSE
    )
  }
  as.list(report$attempts[index, , drop = FALSE])
}

validate_preparation_annotation_record <- function(annotation, report) {
  fields <- c(
    "schema_version",
    "annotation_id",
    "preparation_report_id",
    "attempt_id",
    "package",
    "version",
    "log_stream",
    "log_sha256",
    "recorded_at",
    "author_kind",
    "provenance",
    "annotation_type",
    "interpretation",
    "suggestions"
  )
  validate_composite_contract_record(
    annotation,
    fields,
    "revdeprunner_preparation_annotation",
    "preparation annotation"
  )
  scalar_fields <- fields[fields != "suggestions"]
  valid_scalars <- vapply(
    annotation[scalar_fields],
    function(value) is.character(value) && length(value) == 1L && !is.na(value),
    logical(1L)
  )
  if (!all(valid_scalars)) {
    stop(
      "The preparation annotation record has invalid field types.",
      call. = FALSE
    )
  }
  if (
    !identical(
      annotation$schema_version,
      preparation_annotation_schema_version()
    )
  ) {
    stop("Preparation annotation schema version is unsupported.", call. = FALSE)
  }
  validate_sha256_identity(annotation$annotation_id, "annotation_id")
  validate_sha256_identity(
    annotation$preparation_report_id,
    "preparation_report_id"
  )
  if (!identical(annotation$preparation_report_id, report$report_id)) {
    stop(
      "Preparation annotation does not belong to this report.",
      call. = FALSE
    )
  }
  attempt <- annotation_report_attempt(report, annotation$attempt_id)
  validate_package_name(annotation$package)
  validate_package_version(annotation$version)
  log_stream <- validate_preparation_annotation_log_stream(
    annotation$log_stream
  )
  validate_sha256(annotation$log_sha256, "log_sha256")
  validate_preparation_timestamp(annotation$recorded_at)
  validate_preparation_annotation_author_kind(annotation$author_kind)
  validate_preparation_annotation_text(
    annotation$provenance,
    "provenance",
    1024L
  )
  annotation_type <- validate_preparation_annotation_type(
    annotation$annotation_type
  )
  validate_preparation_annotation_text(
    annotation$interpretation,
    "interpretation",
    4096L
  )
  suggestions <- normalize_preparation_annotation_suggestions(
    annotation$suggestions,
    annotation_type
  )
  if (!identical(annotation$suggestions, suggestions)) {
    stop(
      "Preparation annotation suggestions are not normalized.",
      call. = FALSE
    )
  }
  expected_log_sha256 <- attempt[[paste0(log_stream, "_sha256")]]
  if (
    !identical(annotation$package, attempt$package) ||
      !identical(annotation$version, attempt$version) ||
      !identical(annotation$log_sha256, expected_log_sha256)
  ) {
    stop(
      "Preparation annotation raw-log binding does not match its attempt.",
      call. = FALSE
    )
  }
  if (
    preparation_annotation_timestamp_key(annotation$recorded_at) <
      preparation_annotation_timestamp_key(attempt$started_at)
  ) {
    stop(
      "Preparation annotation cannot predate its process attempt.",
      call. = FALSE
    )
  }

  identity_fields <- preparation_annotation_identity_fields(
    annotation$preparation_report_id,
    annotation$attempt_id,
    annotation$package,
    annotation$version,
    annotation$log_stream,
    annotation$log_sha256,
    annotation$recorded_at,
    annotation$author_kind,
    annotation$provenance,
    annotation$annotation_type,
    annotation$interpretation,
    annotation$suggestions
  )
  expected_id <- record_identity(annotation$schema_version, identity_fields)
  if (!identical(annotation$annotation_id, expected_id)) {
    stop(
      "Preparation annotation identity does not match its fields.",
      call. = FALSE
    )
  }

  invisible(annotation)
}

validate_preparation_annotation_log_stream <- function(log_stream) {
  log_stream <- validate_contract_text(log_stream, "log_stream")
  if (!log_stream %in% preparation_annotation_log_streams()) {
    stop("Preparation annotation log stream is unsupported.", call. = FALSE)
  }
  log_stream
}

validate_preparation_annotation_author_kind <- function(author_kind) {
  author_kind <- validate_contract_text(author_kind, "author_kind")
  if (!author_kind %in% preparation_annotation_author_kinds()) {
    stop("Preparation annotation author kind is unsupported.", call. = FALSE)
  }
  author_kind
}

validate_preparation_annotation_type <- function(annotation_type) {
  annotation_type <- validate_contract_text(annotation_type, "annotation_type")
  if (!annotation_type %in% preparation_annotation_types()) {
    stop("Preparation annotation type is unsupported.", call. = FALSE)
  }
  annotation_type
}

validate_preparation_annotation_text <- function(value, argument, max_bytes) {
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    stop(sprintf("`%s` must be one non-empty string.", argument), call. = FALSE)
  }
  value <- enc2utf8(value)
  if (!nzchar(trimws(value)) || length(charToRaw(value)) > max_bytes) {
    stop(
      sprintf("`%s` must contain at most %s UTF-8 bytes.", argument, max_bytes),
      call. = FALSE
    )
  }
  value
}

normalize_preparation_annotation_suggestions <- function(
  suggestions,
  annotation_type
) {
  suggestions <- validate_preparation_table(
    suggestions,
    preparation_annotation_suggestion_fields(),
    "suggestions"
  )
  for (row in seq_len(nrow(suggestions))) {
    manager <- validate_contract_token(
      suggestions$package_manager[[row]],
      "package_manager"
    )
    if (!manager %in% preparation_annotation_package_managers()) {
      stop(
        "Preparation annotation package manager is unsupported.",
        call. = FALSE
      )
    }
    suggestions$package_manager[[row]] <- manager
    suggestions$command[[row]] <- validate_preparation_annotation_command(
      suggestions$command[[row]]
    )
    basis <- validate_contract_text(suggestions$basis[[row]], "basis")
    if (!basis %in% preparation_annotation_suggestion_bases()) {
      stop(
        "Preparation annotation suggestion basis is unsupported.",
        call. = FALSE
      )
    }
    suggestions$basis[[row]] <- basis
    if (!identical(suggestions$execution_policy[[row]], "advisory-only")) {
      stop(
        "Preparation annotation suggestions must be advisory-only.",
        call. = FALSE
      )
    }
  }
  if (nrow(suggestions) > 0L && !identical(annotation_type, "remediation")) {
    stop("Only remediation annotations may contain suggestions.", call. = FALSE)
  }
  if (anyDuplicated(suggestions)) {
    stop("Preparation annotation suggestions must be unique.", call. = FALSE)
  }
  suggestions <- suggestions[
    order(
      suggestions$package_manager,
      suggestions$command,
      suggestions$basis,
      suggestions$execution_policy,
      method = "radix"
    ),
    ,
    drop = FALSE
  ]
  rownames(suggestions) <- NULL
  suggestions
}

validate_preparation_annotation_command <- function(command) {
  command <- validate_contract_text(command, "command")
  if (length(charToRaw(command)) > 4096L) {
    stop(
      "Preparation annotation commands must not exceed 4,096 UTF-8 bytes.",
      call. = FALSE
    )
  }
  command
}

preparation_annotation_identity_fields <- function(
  preparation_report_id,
  attempt_id,
  package,
  version,
  log_stream,
  log_sha256,
  recorded_at,
  author_kind,
  provenance,
  annotation_type,
  interpretation,
  suggestions
) {
  c(
    preparation_report_id = preparation_report_id,
    attempt_id = attempt_id,
    package = package,
    version = version,
    log_stream = log_stream,
    log_sha256 = log_sha256,
    recorded_at = recorded_at,
    author_kind = author_kind,
    provenance = encode_contract_cell(provenance),
    annotation_type = annotation_type,
    interpretation = encode_contract_cell(interpretation),
    tabular_identity_fields("suggestion", suggestions)
  )
}

normalize_preparation_annotation_batch <- function(annotations, report) {
  if (!is.list(annotations)) {
    stop(
      "`annotations` must be a list of preparation annotations.",
      call. = FALSE
    )
  }
  if (length(annotations) == 0L) {
    return(list())
  }
  invisible(lapply(
    annotations,
    validate_preparation_annotation_record,
    report = report
  ))
  ids <- preparation_annotation_ids(annotations)
  if (anyDuplicated(ids)) {
    stop("Preparation annotations must have unique identities.", call. = FALSE)
  }
  timestamp_keys <- vapply(
    annotations,
    function(annotation) {
      preparation_annotation_timestamp_key(annotation$recorded_at)
    },
    character(1L)
  )
  annotations[order(timestamp_keys, ids, method = "radix")]
}

preparation_annotation_ids <- function(annotations) {
  if (length(annotations) == 0L) {
    return(character())
  }
  vapply(annotations, `[[`, character(1L), "annotation_id")
}

new_preparation_annotation_ledger_record <- function(
  preparation_report_id,
  previous_ledger_id,
  previous_annotation_count,
  annotations
) {
  schema_version <- preparation_annotation_ledger_schema_version()
  fields <- preparation_annotation_ledger_identity_fields(
    preparation_report_id,
    previous_ledger_id,
    previous_annotation_count,
    annotations
  )
  structure(
    list(
      schema_version = schema_version,
      ledger_id = record_identity(schema_version, fields),
      preparation_report_id = preparation_report_id,
      previous_ledger_id = previous_ledger_id,
      previous_annotation_count = previous_annotation_count,
      annotations = annotations
    ),
    class = "revdeprunner_preparation_annotation_ledger"
  )
}

validate_preparation_annotation_ledger_record <- function(ledger, report) {
  fields <- c(
    "schema_version",
    "ledger_id",
    "preparation_report_id",
    "previous_ledger_id",
    "previous_annotation_count",
    "annotations"
  )
  validate_composite_contract_record(
    ledger,
    fields,
    "revdeprunner_preparation_annotation_ledger",
    "preparation annotation ledger"
  )
  scalar_fields <- fields[fields != "annotations"]
  valid_scalars <- vapply(
    ledger[scalar_fields],
    function(value) is.character(value) && length(value) == 1L,
    logical(1L)
  )
  if (
    !all(valid_scalars) ||
      anyNA(ledger[setdiff(scalar_fields, "previous_ledger_id")])
  ) {
    stop(
      "The preparation annotation ledger has invalid field types.",
      call. = FALSE
    )
  }
  if (
    !identical(
      ledger$schema_version,
      preparation_annotation_ledger_schema_version()
    )
  ) {
    stop(
      "Preparation annotation ledger schema version is unsupported.",
      call. = FALSE
    )
  }
  validate_sha256_identity(ledger$ledger_id, "ledger_id")
  validate_sha256_identity(
    ledger$preparation_report_id,
    "preparation_report_id"
  )
  if (!is.na(ledger$previous_ledger_id)) {
    validate_sha256_identity(ledger$previous_ledger_id, "previous_ledger_id")
  }
  previous_count <- normalize_contract_integer(
    ledger$previous_annotation_count,
    "previous_annotation_count"
  )
  if (!identical(previous_count, ledger$previous_annotation_count)) {
    stop(
      "Annotation ledger predecessor count is not normalized.",
      call. = FALSE
    )
  }
  if (!identical(ledger$preparation_report_id, report$report_id)) {
    stop(
      "Preparation annotation ledger does not belong to this report.",
      call. = FALSE
    )
  }
  if (!is.list(ledger$annotations)) {
    stop("Annotation ledger entries must be a list.", call. = FALSE)
  }
  invisible(lapply(
    ledger$annotations,
    validate_preparation_annotation_record,
    report = report
  ))
  ids <- preparation_annotation_ids(ledger$annotations)
  if (anyDuplicated(ids)) {
    stop(
      "Annotation ledger entries must have unique identities.",
      call. = FALSE
    )
  }
  if (
    !is.na(ledger$previous_ledger_id) &&
      as.numeric(previous_count) >= length(ledger$annotations)
  ) {
    stop(
      "An annotation ledger append must increase its annotation count.",
      call. = FALSE
    )
  }
  identity_fields <- preparation_annotation_ledger_identity_fields(
    ledger$preparation_report_id,
    ledger$previous_ledger_id,
    ledger$previous_annotation_count,
    ledger$annotations
  )
  expected_id <- record_identity(ledger$schema_version, identity_fields)
  if (!identical(ledger$ledger_id, expected_id)) {
    stop(
      "Preparation annotation ledger identity does not match its fields.",
      call. = FALSE
    )
  }
  invisible(ledger)
}

preparation_annotation_ledger_identity_fields <- function(
  preparation_report_id,
  previous_ledger_id,
  previous_annotation_count,
  annotations
) {
  c(
    preparation_report_id = preparation_report_id,
    previous_ledger_id = previous_ledger_id,
    previous_annotation_count = previous_annotation_count,
    indexed_vector_identity_fields(
      "annotation",
      preparation_annotation_ids(annotations),
      include_names = FALSE
    )
  )
}

validate_preparation_annotation_chronology <- function(existing, additions) {
  if (length(existing) == 0L || length(additions) == 0L) {
    return(invisible(NULL))
  }
  existing_timestamp <- existing[[length(existing)]]$recorded_at
  addition_timestamp <- additions[[1L]]$recorded_at
  if (
    preparation_annotation_timestamp_key(addition_timestamp) <
      preparation_annotation_timestamp_key(existing_timestamp)
  ) {
    stop("Annotation ledger timestamps cannot move backwards.", call. = FALSE)
  }
  invisible(NULL)
}

preparation_annotation_timestamp_key <- function(timestamp) {
  validate_preparation_timestamp(timestamp)
  fraction <- if (substr(timestamp, 20L, 20L) == ".") {
    substr(timestamp, 21L, nchar(timestamp) - 1L)
  } else {
    ""
  }
  paste0(
    substr(timestamp, 1L, 19L),
    ".",
    fraction,
    strrep("0", 9L - nchar(fraction))
  )
}

# nolint end
