# These internal tests protect the frozen preparation evidence records without
# exposing a public R API or performing preparation work.

preparation_fixture_repositories <- function() {
  c(CRAN = "https://example.test/cran/src/contrib")
}

preparation_fixture_database <- function(empty = FALSE) {
  packages <- if (empty) {
    "SubjectPkg"
  } else {
    c("SubjectPkg", "TargetA", "TargetB", "rootDep", "LeafDep")
  }
  data <- data.frame(
    Package = packages,
    Version = c(
      SubjectPkg = "1.0",
      TargetA = "2.0",
      TargetB = "3.0",
      rootDep = "4.0",
      LeafDep = "5.0"
    )[packages],
    Depends = c(
      SubjectPkg = NA,
      TargetA = "SubjectPkg",
      TargetB = NA,
      rootDep = NA,
      LeafDep = NA
    )[packages],
    Imports = c(
      SubjectPkg = NA,
      TargetA = "rootDep, MissingPkg",
      TargetB = "SubjectPkg, rootDep",
      rootDep = "LeafDep",
      LeafDep = NA
    )[packages],
    LinkingTo = NA_character_,
    Suggests = c(
      SubjectPkg = NA,
      TargetA = "MissingSuggest",
      TargetB = NA,
      rootDep = NA,
      LeafDep = NA
    )[packages],
    NeedsCompilation = c(
      SubjectPkg = "no",
      TargetA = "no",
      TargetB = "no",
      rootDep = "yes",
      LeafDep = "no"
    )[packages],
    SystemRequirements = c(
      SubjectPkg = NA,
      TargetA = NA,
      TargetB = NA,
      rootDep = "libxml2 (>= 2.9)",
      LeafDep = NA
    )[packages],
    Repository = preparation_fixture_repositories()[["CRAN"]],
    stringsAsFactors = FALSE
  )
  rownames(data) <- NULL
  data
}

preparation_fixture_contracts <- function(empty = FALSE) {
  snapshot <- revdeprunner:::new_repository_snapshot(
    preparation_fixture_repositories(),
    preparation_fixture_database(empty)
  )
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
  list(
    snapshot = snapshot,
    cohort = cohort,
    universe = universe,
    lane = lane
  )
}

preparation_fixture_hash <- function(...) {
  digest::digest(
    charToRaw(paste(..., collapse = "\r")),
    algo = "sha256",
    serialize = FALSE
  )
}

preparation_fixture_artifacts <- function(required, lane, binary_packages) {
  available <- required[!is.na(required$version), , drop = FALSE]
  source <- lapply(seq_len(nrow(available)), function(row) {
    revdeprunner:::new_artifact_identity(
      available$package[[row]],
      available$version[[row]],
      preparation_fixture_hash(available$package[[row]], "source"),
      "source"
    )
  })
  binary <- lapply(binary_packages, function(package) {
    version <- available$version[match(package, available$package)]
    revdeprunner:::new_artifact_identity(
      package,
      version,
      preparation_fixture_hash(package, "binary"),
      "binary",
      lane
    )
  })
  c(source, binary)
}

preparation_fixture_artifact <- function(artifacts, package, archive_type) {
  selected <- vapply(
    artifacts,
    function(artifact) {
      identical(artifact$package, package) &&
        identical(artifact$archive_type, archive_type)
    },
    logical(1L)
  )
  stopifnot(sum(selected) == 1L)
  artifacts[[which(selected)]]
}

preparation_fixture_sources <- function(required, artifacts) {
  available <- required[!is.na(required$version), , drop = FALSE]
  rows <- lapply(seq_len(nrow(available)), function(row) {
    package <- available$package[[row]]
    version <- available$version[[row]]
    artifact <- preparation_fixture_artifact(artifacts, package, "source")
    archive <- identical(package, "rootDep")
    data.frame(
      package = package,
      version = version,
      source_origin = if (archive) "archive" else "repository",
      source_url = if (archive) {
        paste0(
          "https://example.test/cran/src/contrib/Archive/rootDep/",
          "rootDep_4.0.tar.gz"
        )
      } else {
        paste0(
          "https://example.test/cran/src/contrib/",
          package,
          "_",
          version,
          ".tar.gz"
        )
      },
      sha256 = artifact$sha256,
      artifact_id = artifact$artifact_id,
      needs_compilation = if (archive) "yes" else "no",
      system_requirements = if (archive) {
        "libxml2 (>= 2.9)\nxmlsec1"
      } else {
        NA_character_
      },
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

preparation_fixture_attempt <- function(package, version, stage, outcome) {
  diagnostic <- switch(
    outcome,
    success = NA_character_,
    failure = paste0(
      "configure: error: libxml2 headers were not found\n",
      "Try installing libxml2-dev before retrying."
    ),
    timeout = "Process exceeded its preparation deadline."
  )
  exit_status <- switch(
    outcome,
    success = 0L,
    failure = 1L,
    timeout = NA_integer_
  )
  revdeprunner:::new_preparation_attempt(
    package = package,
    version = version,
    stage = stage,
    command = paste("R CMD", stage, package),
    started_at = paste0(
      "2026-08-29T12:00:",
      sprintf(
        "%02d",
        match(
          package,
          sort(
            c(
              "LeafDep",
              "TargetA",
              "TargetB",
              "rootDep"
            ),
            method = "radix"
          )
        )
      ),
      "Z"
    ),
    duration_ms = 1250L,
    exit_status = exit_status,
    outcome = outcome,
    stdout_path = paste0("logs/", package, "/", stage, ".stdout.log"),
    stdout_sha256 = preparation_fixture_hash(package, stage, "stdout"),
    stderr_path = paste0("logs/", package, "/", stage, ".stderr.log"),
    stderr_sha256 = preparation_fixture_hash(package, stage, "stderr"),
    diagnostic_excerpt = diagnostic
  )
}

preparation_fixture_result_row <- function(
  package,
  version,
  outcome,
  artifacts,
  attempt = NULL,
  blocker = NA_character_
) {
  artifact_outcomes <- c(
    "prepared",
    "installation-failure"
  )
  artifact_id <- if (outcome %in% artifact_outcomes) {
    preparation_fixture_artifact(artifacts, package, "binary")$artifact_id
  } else {
    NA_character_
  }
  diagnostic <- if (identical(outcome, "prepared")) {
    NA_character_
  } else if (!is.null(attempt)) {
    attempt$diagnostic_excerpt
  } else if (identical(outcome, "unavailable")) {
    "Package is absent from the frozen repository snapshot."
  } else if (identical(outcome, "blocked")) {
    paste("Preparation is blocked by", blocker)
  } else {
    "Package was excluded by an owner decision."
  }
  data.frame(
    package = package,
    version = version,
    outcome = outcome,
    artifact_id = artifact_id,
    evidence_attempt_id = if (is.null(attempt)) {
      NA_character_
    } else {
      attempt$attempt_id
    },
    blocking_dependency = blocker,
    diagnostic_excerpt = diagnostic,
    stringsAsFactors = FALSE
  )
}

preparation_fixture_report <- function(
  selected_outcome = "prepared",
  selected_package = "TargetA",
  blocked = FALSE,
  reverse_input = FALSE
) {
  contracts <- preparation_fixture_contracts()
  required <- revdeprunner:::preparation_required_packages(
    revdeprunner:::derive_preparation_requirements(contracts$universe)
  )
  outcomes <- stats::setNames(
    rep("prepared", nrow(required)),
    required$package
  )
  outcomes[["MissingPkg"]] <- "unavailable"
  blockers <- stats::setNames(
    rep(NA_character_, nrow(required)),
    required$package
  )
  if (blocked) {
    outcomes[["rootDep"]] <- "compilation-failure"
    outcomes[["TargetA"]] <- "blocked"
    outcomes[["TargetB"]] <- "blocked"
    blockers[c("TargetA", "TargetB")] <- "rootDep"
  } else {
    outcomes[[selected_package]] <- selected_outcome
  }
  binary_outcomes <- c(
    "prepared",
    "installation-failure"
  )
  binary_packages <- names(outcomes)[outcomes %in% binary_outcomes]
  artifacts <- preparation_fixture_artifacts(
    required,
    contracts$lane,
    binary_packages
  )
  sources <- preparation_fixture_sources(required, artifacts)
  attempts <- list()
  results <- list()
  for (row in seq_len(nrow(required))) {
    package <- required$package[[row]]
    version <- required$version[[row]]
    outcome <- outcomes[[package]]
    attempt <- NULL
    if (!outcome %in% c("unavailable", "blocked")) {
      attempt_outcome <- if (identical(outcome, "prepared")) {
        "success"
      } else if (identical(outcome, "timeout")) {
        "timeout"
      } else {
        "failure"
      }
      stage <- switch(
        outcome,
        prepared = "build",
        `compilation-failure` = "build",
        `installation-failure` = "install",
        timeout = "build"
      )
      attempt <- preparation_fixture_attempt(
        package,
        version,
        stage,
        attempt_outcome
      )
      attempts[[length(attempts) + 1L]] <- attempt
    }
    results[[length(results) + 1L]] <- preparation_fixture_result_row(
      package,
      version,
      outcome,
      artifacts,
      attempt,
      blockers[[package]]
    )
  }
  results <- do.call(rbind, results)
  if (reverse_input) {
    artifacts <- rev(artifacts)
    sources <- sources[rev(seq_len(nrow(sources))), , drop = FALSE]
    attempts <- rev(attempts)
    results <- results[rev(seq_len(nrow(results))), , drop = FALSE]
  }
  report <- revdeprunner:::new_preparation_report(
    contracts$universe,
    contracts$cohort,
    contracts$snapshot,
    contracts$lane,
    artifacts,
    sources,
    attempts,
    results
  )
  c(
    contracts,
    list(
      report = report,
      artifact_records = artifacts,
      source_input = sources,
      attempt_records = attempts,
      result_input = results
    )
  )
}

preparation_empty_table <- function(fields) {
  values <- stats::setNames(
    replicate(length(fields), character(), simplify = FALSE),
    fields
  )
  as.data.frame(values, stringsAsFactors = FALSE, optional = TRUE)
}

test_that("attempts content-address complete raw process evidence", {
  failure <- preparation_fixture_attempt(
    "rootDep",
    "4.0",
    "install",
    "failure"
  )
  repeated <- preparation_fixture_attempt(
    "rootDep",
    "4.0",
    "install",
    "failure"
  )
  timeout <- preparation_fixture_attempt(
    "TargetA",
    "2.0",
    "build",
    "timeout"
  )

  expect_s3_class(failure, "revdeprunner_preparation_attempt")
  expect_identical(failure, repeated)
  expect_identical(
    names(failure),
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
  )
  expect_identical(
    failure$schema_version,
    "revdeprunner-preparation-attempt/v1"
  )
  expect_match(failure$attempt_id, "^sha256:[a-f0-9]{64}$")
  expect_identical(failure$duration_ms, "1250")
  expect_identical(failure$exit_status, "1")
  expect_match(failure$diagnostic_excerpt, "libxml2-dev", fixed = TRUE)
  expect_false(identical(failure$stdout_path, failure$stderr_path))
  expect_true(is.na(timeout$exit_status))
  expect_invisible(revdeprunner:::validate_preparation_attempt(failure))
  expect_invisible(revdeprunner:::validate_preparation_attempt(timeout))
})

test_that("attempt constructors reject malformed or contradictory evidence", {
  args <- list(
    package = "TargetA",
    version = "2.0",
    stage = "build",
    command = "R CMD INSTALL TargetA",
    started_at = "2026-08-29T12:00:00Z",
    duration_ms = 100L,
    exit_status = 0L,
    outcome = "success",
    stdout_path = "logs/TargetA/build.stdout.log",
    stdout_sha256 = preparation_fixture_hash("out"),
    stderr_path = "logs/TargetA/build.stderr.log",
    stderr_sha256 = preparation_fixture_hash("err")
  )

  invalid <- args
  invalid$stage <- "compile"
  expect_error(
    do.call(revdeprunner:::new_preparation_attempt, invalid),
    "stage is unsupported",
    fixed = TRUE
  )
  for (stage in c(
    "source-resolution",
    "download",
    "system-requirements",
    "verify"
  )) {
    expect_error(
      revdeprunner:::validate_preparation_attempt_stage(stage),
      "stage is unsupported",
      fixed = TRUE,
      info = stage
    )
  }
  invalid <- args
  invalid$started_at <- "2026-08-29 12:00:00"
  expect_error(
    do.call(revdeprunner:::new_preparation_attempt, invalid),
    "ISO 8601 UTC",
    fixed = TRUE
  )
  for (timestamp in c(
    "2026-08-29T24:00:00Z",
    "2026-08-29T23:60:00Z",
    "2026-08-29T23:59:60Z",
    "2026-08-29T12:00:99Z"
  )) {
    invalid <- args
    invalid$started_at <- timestamp
    expect_error(
      do.call(revdeprunner:::new_preparation_attempt, invalid),
      "ISO 8601 UTC",
      fixed = TRUE,
      info = timestamp
    )
  }
  valid <- args
  valid$started_at <- "2026-08-29T23:59:59.123456789Z"
  expect_identical(
    do.call(revdeprunner:::new_preparation_attempt, valid)$started_at,
    valid$started_at
  )
  invalid <- args
  invalid$duration_ms <- 1.5
  expect_error(
    do.call(revdeprunner:::new_preparation_attempt, invalid),
    "non-negative integer",
    fixed = TRUE
  )
  for (path in c("/tmp/output.log", "../output.log", "C:/output.log")) {
    invalid <- args
    invalid$stdout_path <- path
    expect_error(
      do.call(revdeprunner:::new_preparation_attempt, invalid),
      "portable relative artifact path",
      fixed = TRUE
    )
  }
  invalid <- args
  invalid$stderr_path <- invalid$stdout_path
  expect_error(
    do.call(revdeprunner:::new_preparation_attempt, invalid),
    "must differ",
    fixed = TRUE
  )
  invalid <- args
  invalid$exit_status <- 1L
  expect_error(
    do.call(revdeprunner:::new_preparation_attempt, invalid),
    "status 0",
    fixed = TRUE
  )
  invalid <- args
  invalid$outcome <- "failure"
  invalid$exit_status <- 1L
  expect_error(
    do.call(revdeprunner:::new_preparation_attempt, invalid),
    "diagnostic excerpt",
    fixed = TRUE
  )
  invalid <- args
  invalid$outcome <- "timeout"
  invalid$exit_status <- NA_integer_
  invalid$diagnostic_excerpt <- paste(rep("x", 4097L), collapse = "")
  expect_error(
    do.call(revdeprunner:::new_preparation_attempt, invalid),
    "4,096 UTF-8 bytes",
    fixed = TRUE
  )
})

test_that("reports bind sources, artifacts, requirements, logs, and results", {
  fixture <- preparation_fixture_report()
  report <- fixture$report

  expect_s3_class(report, "revdeprunner_preparation_report")
  expect_identical(
    names(report),
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
    )
  )
  expect_identical(
    report$schema_version,
    "revdeprunner-preparation-report/v1"
  )
  expect_match(report$report_id, "^sha256:[a-f0-9]{64}$")
  expect_identical(report$snapshot_id, fixture$snapshot$snapshot_id)
  expect_identical(report$cohort_id, fixture$cohort$cohort_id)
  expect_identical(report$universe_id, fixture$universe$universe_id)
  expect_identical(report$lane_id, fixture$lane$lane_id)

  expect_setequal(
    report$requirements$package,
    c("TargetA", "TargetB", "rootDep", "LeafDep", "MissingPkg")
  )
  expect_false(any(report$requirements$package == "SubjectPkg"))
  expect_identical(
    sum(
      report$requirements$package == "rootDep" &
        report$requirements$role == "closure"
    ),
    2L
  )
  expect_identical(
    report$requirements$disposition[
      report$requirements$package == "MissingPkg"
    ],
    "unavailable"
  )
  expect_setequal(
    report$sources$source_origin,
    c("repository", "archive")
  )
  expect_identical(
    report$sources$needs_compilation[report$sources$package == "rootDep"],
    "yes"
  )
  expect_match(
    report$sources$system_requirements[report$sources$package == "rootDep"],
    "libxml2 (>= 2.9)\nxmlsec1",
    fixed = TRUE
  )
  expect_true(all(report$attempts$stdout_path != report$attempts$stderr_path))
  expect_true(all(nchar(report$attempts$stdout_sha256) == 64L))
  expect_true(all(nchar(report$attempts$stderr_sha256) == 64L))
  expect_identical(
    report$results$outcome[report$results$package == "MissingPkg"],
    "unavailable"
  )
  expect_invisible(
    revdeprunner:::validate_preparation_report(
      report,
      fixture$universe,
      fixture$cohort,
      fixture$snapshot,
      fixture$lane
    )
  )
})

test_that("report identities are independent of input order and locale", {
  baseline <- preparation_fixture_report()
  reversed <- preparation_fixture_report(reverse_input = TRUE)

  expect_identical(reversed$report, baseline$report)

  original <- Sys.getlocale("LC_COLLATE")
  on.exit(Sys.setlocale("LC_COLLATE", original), add = TRUE)
  candidates <- unique(c(
    "C",
    "C.UTF-8",
    "en_US.UTF-8",
    "English_United States.1252",
    "English_United States.utf8",
    original
  ))
  available <- character()
  for (candidate in candidates) {
    selected <- suppressWarnings(Sys.setlocale("LC_COLLATE", candidate))
    if (!is.na(selected) && !selected %in% available) {
      available <- c(available, selected)
    }
  }
  expect_gte(length(available), 2L)

  reports <- lapply(available, function(locale) {
    expect_false(is.na(Sys.setlocale("LC_COLLATE", locale)))
    preparation_fixture_report()$report
  })
  expect_true(all(vapply(reports, identical, logical(1L), reports[[1L]])))
})

test_that("reports distinguish every preparation outcome", {
  for (outcome in c("missing-system-requirements", "not_checked")) {
    expect_error(
      revdeprunner:::validate_preparation_result_outcome(outcome),
      "result outcome is unsupported",
      fixed = TRUE,
      info = outcome
    )
  }

  outcomes <- c(
    "prepared",
    "compilation-failure",
    "installation-failure",
    "timeout"
  )
  for (outcome in outcomes) {
    fixture <- preparation_fixture_report(selected_outcome = outcome)
    result <- fixture$report$results[
      fixture$report$results$package == "TargetA",
      ,
      drop = FALSE
    ]
    expect_identical(result$outcome, outcome, info = outcome)
    expect_invisible(
      revdeprunner:::validate_preparation_report(
        fixture$report,
        fixture$universe,
        fixture$cohort,
        fixture$snapshot,
        fixture$lane
      )
    )
  }

  blocked <- preparation_fixture_report(blocked = TRUE)
  blocked_results <- blocked$report$results[
    blocked$report$results$outcome == "blocked",
    ,
    drop = FALSE
  ]
  expect_setequal(blocked_results$package, c("TargetA", "TargetB"))
  expect_true(all(blocked_results$blocking_dependency == "rootDep"))
  expect_identical(
    blocked$report$results$outcome[
      blocked$report$results$package == "rootDep"
    ],
    "compilation-failure"
  )

  reused <- preparation_fixture_report(selected_outcome = "prepared")
  target_row <- which(reused$result_input$package == "TargetA")
  reused$result_input$evidence_attempt_id[[target_row]] <- NA_character_
  target_attempt <- vapply(
    reused$attempt_records,
    function(attempt) identical(attempt$package, "TargetA"),
    logical(1L)
  )
  reused$attempt_records <- reused$attempt_records[!target_attempt]
  reused_report <- revdeprunner:::new_preparation_report(
    reused$universe,
    reused$cohort,
    reused$snapshot,
    reused$lane,
    reused$artifact_records,
    reused$source_input,
    reused$attempt_records,
    reused$result_input
  )
  expect_identical(
    reused_report$results$outcome[
      reused_report$results$package == "TargetA"
    ],
    "prepared"
  )
  expect_false(any(reused_report$attempts$package == "TargetA"))
})

test_that("report construction rejects inconsistent cross-record evidence", {
  fixture <- preparation_fixture_report()
  build <- function(
    artifacts = fixture$artifact_records,
    sources = fixture$source_input,
    attempts = fixture$attempt_records,
    results = fixture$result_input,
    lane = fixture$lane
  ) {
    revdeprunner:::new_preparation_report(
      fixture$universe,
      fixture$cohort,
      fixture$snapshot,
      lane,
      artifacts,
      sources,
      attempts,
      results
    )
  }

  omitted_source <- fixture$source_input$artifact_id[[1L]]
  partial_report <- build(
    artifacts = Filter(
      function(artifact) !identical(artifact$artifact_id, omitted_source),
      fixture$artifact_records
    ),
    sources = fixture$source_input[-1L, , drop = FALSE]
  )
  expect_s3_class(partial_report, "revdeprunner_preparation_report")
  expect_false(omitted_source %in% partial_report$artifacts$artifact_id)

  invalid_sources <- fixture$source_input
  invalid_sources$package[[1L]] <- "UnknownPkg"
  expect_error(
    build(sources = invalid_sources),
    "identify available required packages uniquely",
    fixed = TRUE
  )
  expect_error(
    build(results = fixture$result_input[-1L, , drop = FALSE]),
    "cover each required package once",
    fixed = TRUE
  )
  invalid_results <- fixture$result_input
  invalid_results$outcome[[1L]] <- "failed"
  expect_error(
    build(results = invalid_results),
    "result outcome is unsupported",
    fixed = TRUE
  )
  extra <- revdeprunner:::new_artifact_identity(
    "ExtraPkg",
    "1.0",
    preparation_fixture_hash("extra"),
    "source"
  )
  expect_error(
    build(artifacts = c(fixture$artifact_records, list(extra))),
    "must all be referenced",
    fixed = TRUE
  )
  invalid_sources <- fixture$source_input
  invalid_sources$source_url[[1L]] <- "relative/source.tar.gz"
  expect_error(
    build(sources = invalid_sources),
    "absolute fragment-free URL",
    fixed = TRUE
  )
  invalid_sources <- fixture$source_input
  invalid_sources$sha256[[1L]] <- preparation_fixture_hash("wrong")
  expect_error(
    build(sources = invalid_sources),
    "does not match its source artifact",
    fixed = TRUE
  )
  duplicate_logs <- fixture$attempt_records
  duplicate_logs[[2L]]$stdout_path <- duplicate_logs[[1L]]$stdout_path
  duplicate_logs[[2L]]$attempt_id <- revdeprunner:::record_identity(
    duplicate_logs[[2L]]$schema_version,
    revdeprunner:::preparation_attempt_identity_fields(duplicate_logs[[2L]])
  )
  expect_error(
    build(attempts = duplicate_logs),
    "raw-log paths must be unique",
    fixed = TRUE
  )

  other_lane <- revdeprunner:::new_compatibility_lane(
    "4.5",
    "x86_64-pc-linux-gnu",
    "x86_64",
    "linux-glibc-2.39",
    "clang-21.1.0"
  )
  expect_error(
    build(lane = other_lane),
    "belongs to another lane",
    fixed = TRUE
  )
})

test_that("result evidence cannot change failure meaning or blocker ancestry", {
  fixture <- preparation_fixture_report(
    selected_outcome = "compilation-failure"
  )
  results <- fixture$result_input
  row <- which(results$package == "TargetA")
  results$outcome[[row]] <- "installation-failure"
  expect_error(
    revdeprunner:::new_preparation_report(
      fixture$universe,
      fixture$cohort,
      fixture$snapshot,
      fixture$lane,
      fixture$artifact_records,
      fixture$source_input,
      fixture$attempt_records,
      results
    ),
    "failure stage is inconsistent",
    fixed = TRUE
  )

  blocked <- preparation_fixture_report(blocked = TRUE)
  results <- blocked$result_input
  row <- which(results$package == "TargetA")
  results$blocking_dependency[[row]] <- "TargetB"
  expect_error(
    revdeprunner:::new_preparation_report(
      blocked$universe,
      blocked$cohort,
      blocked$snapshot,
      blocked$lane,
      blocked$artifact_records,
      blocked$source_input,
      blocked$attempt_records,
      results
    ),
    "blocking dependency is inconsistent",
    fixed = TRUE
  )
})

test_that("report validation detects structure, semantics, and identity mutation", {
  fixture <- preparation_fixture_report()
  validate <- function(report) {
    revdeprunner:::validate_preparation_report(
      report,
      fixture$universe,
      fixture$cohort,
      fixture$snapshot,
      fixture$lane
    )
  }

  changed <- fixture$report
  changed$results$outcome[changed$results$package == "TargetA"] <-
    "installation-failure"
  expect_error(
    validate(changed),
    "failure evidence is inconsistent",
    fixed = TRUE
  )

  changed <- fixture$report
  changed$attempts$stdout_sha256[[1L]] <- preparation_fixture_hash("changed")
  expect_error(validate(changed), "identity does not match", fixed = TRUE)

  changed <- fixture$report
  target_row <- which(changed$requirements$role == "target")[[1L]]
  changed$requirements$role[[target_row]] <- "closure"
  expect_error(validate(changed), "do not match its universe", fixed = TRUE)

  changed <- fixture$report
  changed$sources <- changed$sources[
    rev(seq_len(nrow(changed$sources))),
    ,
    drop = FALSE
  ]
  expect_error(validate(changed), "sources are not normalized", fixed = TRUE)

  changed <- fixture$report
  changed$report_id <- paste0("sha256:", preparation_fixture_hash("changed"))
  expect_error(validate(changed), "identity does not match", fixed = TRUE)

  changed <- fixture$report[-length(fixture$report)]
  class(changed) <- class(fixture$report)
  expect_error(validate(changed), "invalid structure", fixed = TRUE)
})

test_that("an empty selected cohort produces a valid empty report", {
  contracts <- preparation_fixture_contracts(empty = TRUE)
  sources <- preparation_empty_table(
    revdeprunner:::preparation_source_fields()
  )
  results <- preparation_empty_table(
    revdeprunner:::preparation_result_fields()
  )
  report <- revdeprunner:::new_preparation_report(
    contracts$universe,
    contracts$cohort,
    contracts$snapshot,
    contracts$lane,
    artifacts = list(),
    sources = sources,
    attempts = list(),
    results = results
  )

  expect_equal(nrow(report$requirements), 0L)
  expect_equal(nrow(report$artifacts), 0L)
  expect_equal(nrow(report$sources), 0L)
  expect_equal(nrow(report$attempts), 0L)
  expect_equal(nrow(report$results), 0L)
  expect_invisible(
    revdeprunner:::validate_preparation_report(
      report,
      contracts$universe,
      contracts$cohort,
      contracts$snapshot,
      contracts$lane
    )
  )
})
