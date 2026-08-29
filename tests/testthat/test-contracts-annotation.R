# These internal tests protect the immutable interpretation overlay without
# exposing a public R API or executing any suggested command.

annotation_fixture_hash <- function(...) {
  digest::digest(
    charToRaw(paste(..., collapse = "\r")),
    algo = "sha256",
    serialize = FALSE
  )
}

annotation_fixture_context <- function(target_version = "2.0.0") {
  repositories <- c(CRAN = "https://example.test/cran/src/contrib")
  packages <- data.frame(
    Package = c("SubjectPkg", "TargetA"),
    Version = c("1.0.0", target_version),
    Depends = NA_character_,
    Imports = c(NA_character_, "SubjectPkg"),
    LinkingTo = NA_character_,
    Suggests = NA_character_,
    NeedsCompilation = c("no", "yes"),
    SystemRequirements = c(NA_character_, "libxml2 (>= 2.9)"),
    Repository = unname(repositories)[[1L]],
    stringsAsFactors = FALSE
  )
  snapshot <- revdeprunner:::new_repository_snapshot(repositories, packages)
  cohort <- revdeprunner:::new_reverse_dependency_cohort(
    "SubjectPkg",
    snapshot
  )
  universe <- revdeprunner:::new_dependency_universe(
    cohort,
    snapshot,
    "direct",
    c("base", "methods", "stats", "utils")
  )
  lane <- revdeprunner:::new_compatibility_lane(
    r_major_minor = "4.5",
    r_platform = "x86_64-pc-linux-gnu",
    architecture = "x86_64",
    os_abi = "linux-glibc-2.39",
    toolchain_tag = "gcc-15.2.1"
  )
  source_hash <- annotation_fixture_hash("TargetA", target_version, "source")
  source_artifact <- revdeprunner:::new_artifact_identity(
    "TargetA",
    target_version,
    source_hash,
    "source"
  )
  sources <- data.frame(
    package = "TargetA",
    version = target_version,
    source_origin = "repository",
    source_url = paste0(
      repositories[[1L]],
      "/TargetA_",
      target_version,
      ".tar.gz"
    ),
    sha256 = source_hash,
    artifact_id = source_artifact$artifact_id,
    needs_compilation = "yes",
    system_requirements = "libxml2 (>= 2.9)",
    stringsAsFactors = FALSE
  )
  diagnostic <- paste0(
    "configure: error: libxml2 headers were not found\n",
    "Try installing libxml2-dev before retrying."
  )
  attempt <- revdeprunner:::new_preparation_attempt(
    package = "TargetA",
    version = target_version,
    stage = "install",
    command = "R CMD INSTALL TargetA",
    started_at = "2026-08-29T12:00:00Z",
    duration_ms = 1250L,
    exit_status = 1L,
    outcome = "failure",
    stdout_path = "logs/TargetA/install.stdout.log",
    stdout_sha256 = annotation_fixture_hash(
      "TargetA",
      target_version,
      "stdout"
    ),
    stderr_path = "logs/TargetA/install.stderr.log",
    stderr_sha256 = annotation_fixture_hash(
      "TargetA",
      target_version,
      "stderr"
    ),
    diagnostic_excerpt = diagnostic
  )
  results <- data.frame(
    package = "TargetA",
    version = target_version,
    outcome = "missing-system-requirements",
    artifact_id = NA_character_,
    evidence_attempt_id = attempt$attempt_id,
    blocking_dependency = NA_character_,
    diagnostic_excerpt = diagnostic,
    stringsAsFactors = FALSE
  )
  report <- revdeprunner:::new_preparation_report(
    universe,
    cohort,
    snapshot,
    lane,
    artifacts = list(source_artifact),
    sources = sources,
    attempts = list(attempt),
    results = results
  )

  list(
    snapshot = snapshot,
    cohort = cohort,
    universe = universe,
    lane = lane,
    attempt = attempt,
    report = report
  )
}

annotation_fixture_suggestions <- function(reverse_input = FALSE) {
  suggestions <- data.frame(
    package_manager = c("homebrew", "apt", "dnf"),
    command = c(
      "brew install libxml2",
      "sudo apt install libxml2-dev",
      "sudo dnf install libxml2-devel"
    ),
    basis = c(
      "author-analysis",
      "raw-log",
      "declared-system-requirements"
    ),
    execution_policy = rep("advisory-only", 3L),
    stringsAsFactors = FALSE
  )
  if (reverse_input) {
    suggestions <- suggestions[rev(seq_len(nrow(suggestions))), , drop = FALSE]
  }
  rownames(suggestions) <- NULL
  suggestions
}

new_fixture_annotation <- function(
  context,
  log_stream = "stderr",
  recorded_at = "2026-08-29T13:00:00Z",
  author_kind = "agent",
  provenance = "Agent review of the captured installation failure.",
  annotation_type = "diagnosis",
  interpretation = "The package is missing the libxml2 development headers.",
  suggestions = revdeprunner:::empty_preparation_annotation_suggestions()
) {
  revdeprunner:::new_preparation_annotation(
    context$report,
    context$universe,
    context$cohort,
    context$snapshot,
    context$lane,
    context$attempt$attempt_id,
    log_stream,
    recorded_at,
    author_kind,
    provenance,
    annotation_type,
    interpretation,
    suggestions
  )
}

validate_fixture_annotation <- function(annotation, context) {
  revdeprunner:::validate_preparation_annotation(
    annotation,
    context$report,
    context$universe,
    context$cohort,
    context$snapshot,
    context$lane
  )
}

new_fixture_annotation_ledger <- function(context) {
  revdeprunner:::new_preparation_annotation_ledger(
    context$report,
    context$universe,
    context$cohort,
    context$snapshot,
    context$lane
  )
}

append_fixture_annotations <- function(
  ledger,
  annotations,
  context,
  previous = NULL
) {
  revdeprunner:::append_preparation_annotations(
    ledger,
    annotations,
    context$report,
    context$universe,
    context$cohort,
    context$snapshot,
    context$lane,
    previous
  )
}

validate_fixture_annotation_ledger <- function(
  ledger,
  context,
  previous = NULL
) {
  revdeprunner:::validate_preparation_annotation_ledger(
    ledger,
    context$report,
    context$universe,
    context$cohort,
    context$snapshot,
    context$lane,
    previous
  )
}

rehash_fixture_annotation <- function(annotation) {
  fields <- revdeprunner:::preparation_annotation_identity_fields(
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
  annotation$annotation_id <- revdeprunner:::record_identity(
    annotation$schema_version,
    fields
  )
  annotation
}

rehash_fixture_annotation_ledger <- function(ledger) {
  fields <- revdeprunner:::preparation_annotation_ledger_identity_fields(
    ledger$preparation_report_id,
    ledger$previous_ledger_id,
    ledger$previous_annotation_count,
    ledger$annotations
  )
  ledger$ledger_id <- revdeprunner:::record_identity(
    ledger$schema_version,
    fields
  )
  ledger
}

test_that("annotations bind exact immutable preparation evidence", {
  context <- annotation_fixture_context()
  stderr <- new_fixture_annotation(context)
  stdout <- new_fixture_annotation(
    context,
    log_stream = "stdout",
    recorded_at = "2026-08-29T13:01:00Z",
    author_kind = "human",
    provenance = "Operator inspection of the captured standard output.",
    annotation_type = "note",
    interpretation = "Standard output contains no additional root cause."
  )

  expect_s3_class(stderr, "revdeprunner_preparation_annotation")
  expect_identical(
    names(stderr),
    c(
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
  )
  expect_identical(
    stderr$schema_version,
    "revdeprunner-preparation-annotation/v1"
  )
  expect_identical(stderr$preparation_report_id, context$report$report_id)
  expect_identical(stderr$attempt_id, context$attempt$attempt_id)
  expect_identical(stderr$package, "TargetA")
  expect_identical(stderr$version, "2.0.0")
  expect_identical(stderr$log_sha256, context$attempt$stderr_sha256)
  expect_identical(stdout$log_sha256, context$attempt$stdout_sha256)
  expect_identical(stderr$author_kind, "agent")
  expect_identical(stdout$author_kind, "human")
  expect_false(identical(stderr$annotation_id, stdout$annotation_id))
  expect_invisible(validate_fixture_annotation(stderr, context))
  expect_invisible(validate_fixture_annotation(stdout, context))
})

test_that("remediation suggestions are normalized advisory data", {
  context <- annotation_fixture_context()
  forward <- new_fixture_annotation(
    context,
    annotation_type = "remediation",
    interpretation = "Install a matching development package and retry.",
    suggestions = annotation_fixture_suggestions()
  )
  reversed <- new_fixture_annotation(
    context,
    annotation_type = "remediation",
    interpretation = "Install a matching development package and retry.",
    suggestions = annotation_fixture_suggestions(reverse_input = TRUE)
  )

  expect_identical(forward, reversed)
  expect_identical(
    names(forward$suggestions),
    c("package_manager", "command", "basis", "execution_policy")
  )
  expect_identical(
    forward$suggestions$package_manager,
    c("apt", "dnf", "homebrew")
  )
  expect_identical(
    sort(forward$suggestions$basis, method = "radix"),
    sort(
      c("raw-log", "declared-system-requirements", "author-analysis"),
      method = "radix"
    )
  )
  expect_true(all(forward$suggestions$execution_policy == "advisory-only"))
  expect_invisible(validate_fixture_annotation(forward, context))

  mixed_case <- annotation_fixture_suggestions()
  mixed_case$package_manager[[1L]] <- "Homebrew"
  expect_error(
    new_fixture_annotation(
      context,
      annotation_type = "remediation",
      interpretation = "Install a matching development package and retry.",
      suggestions = mixed_case
    ),
    "manager is unsupported",
    fixed = TRUE
  )
})

test_that("suggestion validation fails closed", {
  context <- annotation_fixture_context()
  suggestions <- annotation_fixture_suggestions()[1L, , drop = FALSE]

  expect_error(
    new_fixture_annotation(context, suggestions = suggestions),
    "Only remediation annotations",
    fixed = TRUE
  )
  changed <- suggestions
  changed$execution_policy <- "execute"
  expect_error(
    new_fixture_annotation(
      context,
      annotation_type = "remediation",
      suggestions = changed
    ),
    "advisory-only",
    fixed = TRUE
  )
  changed <- suggestions
  changed$package_manager <- "ports"
  expect_error(
    new_fixture_annotation(
      context,
      annotation_type = "remediation",
      suggestions = changed
    ),
    "manager is unsupported",
    fixed = TRUE
  )
  changed <- suggestions
  changed$basis <- "guess"
  expect_error(
    new_fixture_annotation(
      context,
      annotation_type = "remediation",
      suggestions = changed
    ),
    "basis is unsupported",
    fixed = TRUE
  )
  changed <- suggestions
  changed$command <- "brew install\nlibxml2"
  expect_error(
    new_fixture_annotation(
      context,
      annotation_type = "remediation",
      suggestions = changed
    ),
    "one non-empty string",
    fixed = TRUE
  )
  duplicate <- rbind(suggestions, suggestions)
  expect_error(
    new_fixture_annotation(
      context,
      annotation_type = "remediation",
      suggestions = duplicate
    ),
    "must be unique",
    fixed = TRUE
  )
  expect_error(
    new_fixture_annotation(
      context,
      annotation_type = "remediation",
      suggestions = suggestions[, -1L, drop = FALSE]
    ),
    "invalid table structure",
    fixed = TRUE
  )
})

test_that("annotation relationships and bounded vocabularies fail closed", {
  context <- annotation_fixture_context()
  other <- annotation_fixture_context(target_version = "2.0.1")

  expect_error(
    revdeprunner:::new_preparation_annotation(
      context$report,
      context$universe,
      context$cohort,
      context$snapshot,
      context$lane,
      other$attempt$attempt_id,
      "stderr",
      "2026-08-29T13:00:00Z",
      "agent",
      "Agent review.",
      "diagnosis",
      "Missing development headers."
    ),
    "absent from the preparation report",
    fixed = TRUE
  )
  for (arguments in list(
    list(log_stream = "combined"),
    list(author_kind = "tool"),
    list(annotation_type = "approval"),
    list(recorded_at = "2026-08-29T13:00:99Z"),
    list(provenance = ""),
    list(provenance = "  "),
    list(interpretation = ""),
    list(interpretation = "\n")
  )) {
    expect_error(do.call(new_fixture_annotation, c(list(context), arguments)))
  }
  expect_error(
    new_fixture_annotation(
      context,
      recorded_at = "2026-08-29T11:59:59.999999999Z"
    ),
    "cannot predate its process attempt",
    fixed = TRUE
  )
  expect_error(
    new_fixture_annotation(context, provenance = strrep("p", 1025L)),
    "at most 1024",
    fixed = TRUE
  )
  expect_error(
    new_fixture_annotation(context, interpretation = strrep("i", 4097L)),
    "at most 4096",
    fixed = TRUE
  )
  annotation <- new_fixture_annotation(context)
  expect_error(
    validate_fixture_annotation(annotation, other),
    "does not belong to this report",
    fixed = TRUE
  )
})

test_that("annotation mutation is rejected structurally and semantically", {
  context <- annotation_fixture_context()
  annotation <- new_fixture_annotation(context)

  missing <- annotation[-1L]
  expect_error(
    validate_fixture_annotation(missing, context),
    "invalid structure",
    fixed = TRUE
  )
  extra <- annotation
  extra$unexpected <- "value"
  expect_error(
    validate_fixture_annotation(extra, context),
    "invalid structure",
    fixed = TRUE
  )
  changed <- annotation
  changed$package <- "OtherPkg"
  expect_error(
    validate_fixture_annotation(changed, context),
    "raw-log binding",
    fixed = TRUE
  )
  changed <- annotation
  changed$log_sha256 <- context$attempt$stdout_sha256
  expect_error(
    validate_fixture_annotation(changed, context),
    "raw-log binding",
    fixed = TRUE
  )
  changed <- annotation
  changed$interpretation <- "A different diagnosis."
  expect_error(
    validate_fixture_annotation(changed, context),
    "identity does not match",
    fixed = TRUE
  )
  changed <- annotation
  changed$annotation_id <- sub(".$", "0", changed$annotation_id)
  expect_error(validate_fixture_annotation(changed, context))

  remediation <- new_fixture_annotation(
    context,
    annotation_type = "remediation",
    interpretation = "Install a development package.",
    suggestions = annotation_fixture_suggestions()
  )
  changed <- remediation
  changed$suggestions <- changed$suggestions[rev(seq_len(3L)), , drop = FALSE]
  changed <- rehash_fixture_annotation(changed)
  expect_error(
    validate_fixture_annotation(changed, context),
    "suggestions are not normalized",
    fixed = TRUE
  )
})

test_that("genesis ledgers are empty deterministic report bindings", {
  context <- annotation_fixture_context()
  before <- snapshot_test_cache(".")
  ledger <- new_fixture_annotation_ledger(context)
  repeated <- new_fixture_annotation_ledger(context)

  expect_s3_class(
    ledger,
    "revdeprunner_preparation_annotation_ledger"
  )
  expect_identical(ledger, repeated)
  expect_identical(
    names(ledger),
    c(
      "schema_version",
      "ledger_id",
      "preparation_report_id",
      "previous_ledger_id",
      "previous_annotation_count",
      "annotations"
    )
  )
  expect_identical(
    ledger$schema_version,
    "revdeprunner-preparation-annotation-ledger/v1"
  )
  expect_identical(ledger$preparation_report_id, context$report$report_id)
  expect_true(is.na(ledger$previous_ledger_id))
  expect_identical(ledger$previous_annotation_count, "0")
  expect_length(ledger$annotations, 0L)
  expect_invisible(validate_fixture_annotation_ledger(ledger, context))
  expect_identical(snapshot_test_cache("."), before)
})

test_that("ledger appends preserve an exact normalized prefix", {
  context <- annotation_fixture_context()
  genesis <- new_fixture_annotation_ledger(context)
  later <- new_fixture_annotation(
    context,
    recorded_at = "2026-08-29T13:02:00Z",
    annotation_type = "note",
    interpretation = "The diagnostic names the missing header family."
  )
  earlier <- new_fixture_annotation(
    context,
    recorded_at = "2026-08-29T13:01:00Z"
  )
  forward <- append_fixture_annotations(
    genesis,
    list(earlier, later),
    context
  )
  reversed <- append_fixture_annotations(
    genesis,
    list(later, earlier),
    context
  )

  expect_identical(forward, reversed)
  expect_identical(forward$previous_ledger_id, genesis$ledger_id)
  expect_identical(forward$previous_annotation_count, "0")
  expect_identical(
    revdeprunner:::preparation_annotation_ids(forward$annotations),
    c(earlier$annotation_id, later$annotation_id)
  )
  expect_invisible(
    validate_fixture_annotation_ledger(forward, context, genesis)
  )

  remediation <- new_fixture_annotation(
    context,
    recorded_at = "2026-08-29T13:03:00Z",
    annotation_type = "remediation",
    interpretation = "Install a development package and retry.",
    suggestions = annotation_fixture_suggestions()
  )
  appended <- append_fixture_annotations(
    forward,
    list(remediation),
    context,
    previous = genesis
  )
  expect_identical(appended$previous_ledger_id, forward$ledger_id)
  expect_identical(appended$previous_annotation_count, "2")
  expect_identical(appended$annotations[1:2], forward$annotations)
  expect_identical(appended$annotations[[3L]], remediation)
  expect_invisible(
    validate_fixture_annotation_ledger(appended, context, forward)
  )

  fractional_later <- new_fixture_annotation(
    context,
    recorded_at = "2026-08-29T13:04:00.1Z",
    annotation_type = "note",
    interpretation = "This is later than the whole-second annotation."
  )
  fractional_earlier <- new_fixture_annotation(
    context,
    recorded_at = "2026-08-29T13:04:00Z",
    annotation_type = "note",
    interpretation = "This is the whole-second annotation."
  )
  fractional <- append_fixture_annotations(
    appended,
    list(fractional_later, fractional_earlier),
    context,
    previous = forward
  )
  expect_identical(
    fractional$annotations[4:5],
    list(fractional_earlier, fractional_later)
  )
})

test_that("ledger append boundaries fail closed", {
  context <- annotation_fixture_context()
  other <- annotation_fixture_context(target_version = "2.0.1")
  genesis <- new_fixture_annotation_ledger(context)
  annotation <- new_fixture_annotation(context)
  first <- append_fixture_annotations(genesis, list(annotation), context)

  expect_error(
    append_fixture_annotations(genesis, list(), context),
    "requires at least one",
    fixed = TRUE
  )
  expect_error(
    append_fixture_annotations(
      first,
      list(annotation),
      context,
      previous = genesis
    ),
    "existing annotation",
    fixed = TRUE
  )
  earlier <- new_fixture_annotation(
    context,
    recorded_at = "2026-08-29T12:59:59.999999999Z"
  )
  expect_error(
    append_fixture_annotations(
      first,
      list(earlier),
      context,
      previous = genesis
    ),
    "cannot move backwards",
    fixed = TRUE
  )
  expect_error(
    validate_fixture_annotation_ledger(first, context),
    "requires its predecessor",
    fixed = TRUE
  )
  expect_error(
    validate_fixture_annotation_ledger(first, context, first),
    "predecessor identity",
    fixed = TRUE
  )
  expect_error(
    validate_fixture_annotation_ledger(genesis, context, genesis),
    "must not have a predecessor",
    fixed = TRUE
  )
  expect_error(
    validate_fixture_annotation_ledger(genesis, other),
    "does not belong to this report",
    fixed = TRUE
  )
})

test_that("a child rejects locally invalid predecessor snapshots", {
  context <- annotation_fixture_context()
  genesis <- new_fixture_annotation_ledger(context)
  first_annotation <- new_fixture_annotation(context)
  second_annotation <- new_fixture_annotation(
    context,
    recorded_at = "2026-08-29T13:01:00Z",
    annotation_type = "note",
    interpretation = "This note follows the diagnosis."
  )
  third_annotation <- new_fixture_annotation(
    context,
    recorded_at = "2026-08-29T13:02:00Z",
    annotation_type = "note",
    interpretation = "This note follows the predecessor snapshot."
  )

  invalid_genesis <- genesis
  invalid_genesis$annotations <- list(first_annotation)
  invalid_genesis <- rehash_fixture_annotation_ledger(invalid_genesis)
  expect_error(
    validate_fixture_annotation_ledger(invalid_genesis, context),
    "genesis annotation ledger must be empty",
    fixed = TRUE
  )
  genesis_child <- revdeprunner:::new_preparation_annotation_ledger_record(
    context$report$report_id,
    invalid_genesis$ledger_id,
    "1",
    list(first_annotation, second_annotation)
  )
  expect_error(
    validate_fixture_annotation_ledger(
      genesis_child,
      context,
      invalid_genesis
    ),
    "genesis annotation ledger must be empty",
    fixed = TRUE
  )

  invalid_order <- revdeprunner:::new_preparation_annotation_ledger_record(
    context$report$report_id,
    genesis$ledger_id,
    "0",
    list(second_annotation, first_annotation)
  )
  expect_error(
    validate_fixture_annotation_ledger(invalid_order, context, genesis),
    "additions are not normalized",
    fixed = TRUE
  )
  order_child <- revdeprunner:::new_preparation_annotation_ledger_record(
    context$report$report_id,
    invalid_order$ledger_id,
    "2",
    list(second_annotation, first_annotation, third_annotation)
  )
  expect_error(
    validate_fixture_annotation_ledger(order_child, context, invalid_order),
    "additions are not normalized",
    fixed = TRUE
  )
})

test_that("ledger structural ancestry and identity mutation is rejected", {
  context <- annotation_fixture_context()
  genesis <- new_fixture_annotation_ledger(context)
  first_annotation <- new_fixture_annotation(context)
  first <- append_fixture_annotations(genesis, list(first_annotation), context)
  second_annotation <- new_fixture_annotation(
    context,
    recorded_at = "2026-08-29T13:01:00Z",
    annotation_type = "note",
    interpretation = "This note was appended after the diagnosis."
  )
  second <- append_fixture_annotations(
    first,
    list(second_annotation),
    context,
    previous = genesis
  )

  missing <- second[-1L]
  expect_error(
    validate_fixture_annotation_ledger(missing, context, first),
    "invalid structure",
    fixed = TRUE
  )
  changed <- second
  changed$previous_annotation_count <- "0"
  changed <- rehash_fixture_annotation_ledger(changed)
  expect_error(
    validate_fixture_annotation_ledger(changed, context, first),
    "predecessor count",
    fixed = TRUE
  )
  replacement_prefix <- new_fixture_annotation(
    context,
    recorded_at = "2026-08-29T13:00:30Z",
    annotation_type = "note",
    interpretation = "This is not the predecessor's retained annotation."
  )
  changed <- second
  changed$annotations[[1L]] <- replacement_prefix
  changed <- rehash_fixture_annotation_ledger(changed)
  expect_error(
    validate_fixture_annotation_ledger(changed, context, first),
    "not an exact prefix",
    fixed = TRUE
  )
  changed <- second
  changed$annotations[[2L]] <- first_annotation
  changed <- rehash_fixture_annotation_ledger(changed)
  expect_error(
    validate_fixture_annotation_ledger(changed, context, first),
    "unique identities",
    fixed = TRUE
  )
  changed <- second
  changed$ledger_id <- sub(".$", "0", changed$ledger_id)
  expect_error(validate_fixture_annotation_ledger(changed, context, first))

  changed <- first
  changed$annotations <- list()
  changed <- rehash_fixture_annotation_ledger(changed)
  expect_error(
    validate_fixture_annotation_ledger(changed, context, genesis),
    "increase its annotation count",
    fixed = TRUE
  )
})

test_that("annotation and ledger identities are locale-independent", {
  context <- annotation_fixture_context()
  baseline_annotation <- new_fixture_annotation(
    context,
    annotation_type = "remediation",
    interpretation = "Install a development package and retry.",
    suggestions = annotation_fixture_suggestions(reverse_input = TRUE)
  )
  baseline_genesis <- new_fixture_annotation_ledger(context)
  baseline_ledger <- append_fixture_annotations(
    baseline_genesis,
    list(baseline_annotation),
    context
  )

  original <- Sys.getlocale("LC_COLLATE")
  on.exit(
    suppressWarnings(Sys.setlocale("LC_COLLATE", original)),
    add = TRUE
  )
  ledgers <- list()
  for (locale in c("C", "C.UTF-8")) {
    selected <- suppressWarnings(Sys.setlocale("LC_COLLATE", locale))
    if (!is.na(selected) && nzchar(selected)) {
      annotation <- new_fixture_annotation(
        context,
        annotation_type = "remediation",
        interpretation = "Install a development package and retry.",
        suggestions = annotation_fixture_suggestions(reverse_input = TRUE)
      )
      genesis <- new_fixture_annotation_ledger(context)
      ledgers[[selected]] <- append_fixture_annotations(
        genesis,
        list(annotation),
        context
      )
      validate_fixture_annotation_ledger(
        ledgers[[selected]],
        context,
        genesis
      )
    }
  }
  expect_gte(length(ledgers), 1L)
  expect_true(all(vapply(
    ledgers,
    identical,
    logical(1L),
    y = baseline_ledger
  )))
})
