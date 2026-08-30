# This file composes private helpers defined in other package source files.
# nolint start: object_usage_linter.

prepare_dependency_universe <- function(
  source_plan,
  universe,
  cohort,
  snapshot,
  binary_reuse,
  lane,
  path_plan,
  command_plan,
  previous = NULL,
  timeout_seconds = 600L
) {
  context <- list(
    source_plan = source_plan,
    universe = universe,
    cohort = cohort,
    snapshot = snapshot,
    binary_reuse = binary_reuse,
    lane = lane,
    path_plan = path_plan,
    command_plan = command_plan
  )
  validate_preparation_gate_context(context)
  timeout_seconds <- normalize_source_preparation_timeout(timeout_seconds)
  execution_order <- preparation_dependency_order(universe)
  if (!is.null(previous)) {
    validate_preparation_gate(previous, context)
  }

  source_packages <- source_plan$sources$package
  source_acquisitions <- lapply(source_packages, function(package) {
    prior <- if (is.null(previous)) {
      NULL
    } else {
      previous$source_acquisitions[[package]]
    }
    acquire_source_artifact(
      package,
      source_plan,
      universe,
      cohort,
      snapshot,
      binary_reuse,
      lane,
      path_plan,
      previous = prior
    )
  })
  names(source_acquisitions) <- source_packages

  attempts <- if (is.null(previous)) {
    list()
  } else {
    preparation_gate_attempt_records(previous$report$attempts)
  }
  source_preparations <- list()
  results <- list()
  requirements <- preparation_required_packages(source_plan$requirements)

  for (package in execution_order) {
    version <- requirements$version[requirements$package == package]
    if (is.na(version)) {
      results[[package]] <- preparation_gate_unavailable_result(package)
      next
    }

    blocker <- preparation_gate_blocker(package, results, universe)
    if (!is.na(blocker)) {
      results[[package]] <- preparation_gate_blocked_result(
        package,
        version,
        blocker
      )
      next
    }

    selection <- binary_reuse$selections[[package]]
    if (identical(selection$status, "selected")) {
      results[[package]] <- preparation_gate_hit_result(
        package,
        version,
        selection$artifact
      )
      next
    }

    prior <- if (
      !is.null(previous) &&
        package %in% names(previous$source_preparations) &&
        identical(
          previous$source_preparations[[package]]$result$outcome,
          "prepared"
        )
    ) {
      previous$source_preparations[[package]]
    } else {
      NULL
    }
    preparation <- prepare_source_binary(
      package,
      source_plan,
      universe,
      cohort,
      snapshot,
      binary_reuse,
      lane,
      path_plan,
      command_plan,
      source_acquisitions[[package]],
      previous = prior,
      timeout_seconds = timeout_seconds
    )
    source_preparations[[package]] <- preparation
    attempts <- preparation_gate_append_attempts(
      attempts,
      preparation$attempts
    )
    results[[package]] <- preparation$result
  }

  if (length(source_preparations) > 0L) {
    source_preparations <- source_preparations[
      sort(names(source_preparations), method = "radix")
    ]
  }
  result_table <- do.call(rbind, unname(results))
  rownames(result_table) <- NULL
  artifacts <- preparation_gate_artifacts(
    source_acquisitions,
    source_preparations,
    result_table,
    binary_reuse
  )
  report <- new_preparation_report(
    universe,
    cohort,
    snapshot,
    lane,
    artifacts,
    preparation_gate_source_rows(source_acquisitions, source_plan),
    attempts,
    result_table
  )
  gate <- list(
    report = report,
    source_acquisitions = source_acquisitions,
    source_preparations = source_preparations,
    execution_order = execution_order
  )
  validate_preparation_gate(gate, context)
  gate
}

validate_preparation_gate_context <- function(context) {
  validate_source_preparation_context_record(context)
  validate_source_acquisition_plan(
    context$source_plan,
    context$universe,
    context$cohort,
    context$snapshot,
    context$binary_reuse,
    context$lane,
    context$path_plan
  )
  validate_command_plan(
    context$command_plan,
    context$path_plan,
    context$snapshot,
    context$cohort,
    context$universe,
    context$lane
  )
  if (
    !identical(context$command_plan$operation, "prepare") ||
      !identical(context$command_plan$dry_run, "false")
  ) {
    stop(
      "Preparation gate requires an executable prepare command plan.",
      call. = FALSE
    )
  }

  invisible(context)
}

preparation_dependency_order <- function(universe) {
  requirements <- preparation_required_packages(
    derive_preparation_requirements(universe)
  )
  packages <- requirements$package
  edges <- unique(universe$edges[c("from_package", "dependency")])
  edges <- edges[
    edges$from_package %in%
      packages &
      edges$dependency %in% packages &
      edges$from_package != edges$dependency,
    ,
    drop = FALSE
  ]
  remaining <- packages
  completed <- character()
  while (length(remaining) > 0L) {
    ready <- remaining[vapply(
      remaining,
      function(package) {
        dependencies <- edges$dependency[edges$from_package == package]
        all(dependencies %in% completed)
      },
      logical(1L)
    )]
    if (length(ready) == 0L) {
      stop(
        "Preparation dependency graph contains a cycle.",
        call. = FALSE
      )
    }
    ready <- sort(ready, method = "radix")
    completed <- c(completed, ready)
    remaining <- setdiff(remaining, ready)
  }

  completed
}

preparation_gate_blocker <- function(package, results, universe) {
  result_packages <- names(results)
  if (is.null(result_packages)) {
    result_packages <- character()
  }
  dependencies <- unique(
    universe$edges$dependency[universe$edges$from_package == package]
  )
  dependencies <- sort(
    intersect(dependencies, result_packages),
    method = "radix"
  )
  unsuccessful <- dependencies[vapply(
    dependencies,
    function(dependency) {
      !results[[dependency]]$outcome[[1L]] %in% c("prepared", "ready")
    },
    logical(1L)
  )]
  if (length(unsuccessful) == 0L) NA_character_ else unsuccessful[[1L]]
}

preparation_gate_unavailable_result <- function(package) {
  data.frame(
    package = package,
    version = NA_character_,
    outcome = "unavailable",
    artifact_id = NA_character_,
    evidence_attempt_id = NA_character_,
    blocking_dependency = NA_character_,
    diagnostic_excerpt = paste(
      "Package is absent from the frozen repository snapshot."
    ),
    stringsAsFactors = FALSE
  )
}

preparation_gate_blocked_result <- function(package, version, blocker) {
  data.frame(
    package = package,
    version = version,
    outcome = "blocked",
    artifact_id = NA_character_,
    evidence_attempt_id = NA_character_,
    blocking_dependency = blocker,
    diagnostic_excerpt = paste("Preparation is blocked by", blocker),
    stringsAsFactors = FALSE
  )
}

preparation_gate_hit_result <- function(package, version, artifact) {
  validate_artifact_identity(artifact)
  data.frame(
    package = package,
    version = version,
    outcome = "prepared",
    artifact_id = artifact$artifact_id,
    evidence_attempt_id = NA_character_,
    blocking_dependency = NA_character_,
    diagnostic_excerpt = NA_character_,
    stringsAsFactors = FALSE
  )
}

preparation_gate_source_rows <- function(acquisitions, source_plan) {
  rows <- lapply(names(acquisitions), function(package) {
    acquisition <- acquisitions[[package]]
    source <- source_acquisition_planned_row(source_plan, package)
    data.frame(
      package = package,
      version = acquisition$version,
      source_origin = "repository",
      source_url = acquisition$source_url,
      sha256 = acquisition$artifact$sha256,
      artifact_id = acquisition$artifact$artifact_id,
      needs_compilation = source$needs_compilation[[1L]],
      system_requirements = source$system_requirements[[1L]],
      stringsAsFactors = FALSE
    )
  })
  sources <- do.call(rbind, rows)
  rownames(sources) <- NULL
  sources[order(sources$package, method = "radix"), , drop = FALSE]
}

preparation_gate_artifacts <- function(
  acquisitions,
  preparations,
  results,
  binary_reuse
) {
  artifacts <- lapply(acquisitions, `[[`, "artifact")
  for (row in seq_len(nrow(results))) {
    artifact_id <- results$artifact_id[[row]]
    if (is.na(artifact_id)) {
      next
    }
    package <- results$package[[row]]
    artifact <- if (package %in% names(preparations)) {
      preparations[[package]]$binary_artifact
    } else {
      binary_reuse$selections[[package]]$artifact
    }
    if (is.null(artifact) || !identical(artifact$artifact_id, artifact_id)) {
      stop("Preparation gate binary evidence is inconsistent.", call. = FALSE)
    }
    artifacts[[length(artifacts) + 1L]] <- artifact
  }
  ids <- vapply(artifacts, `[[`, character(1L), "artifact_id")
  artifacts[!duplicated(ids)]
}

preparation_gate_attempt_records <- function(attempts) {
  lapply(seq_len(nrow(attempts)), function(row) {
    attempt <- structure(
      as.list(attempts[row, , drop = FALSE]),
      class = "revdeprunner_preparation_attempt"
    )
    validate_preparation_attempt(attempt)
    attempt
  })
}

preparation_gate_append_attempts <- function(attempts, additions) {
  existing <- vapply(attempts, `[[`, character(1L), "attempt_id")
  for (attempt in additions) {
    if (!attempt$attempt_id %in% existing) {
      attempts[[length(attempts) + 1L]] <- attempt
      existing <- c(existing, attempt$attempt_id)
    }
  }
  attempts
}

validate_preparation_gate <- function(gate, context) {
  validate_preparation_gate_context(context)
  fields <- c(
    "report",
    "source_acquisitions",
    "source_preparations",
    "execution_order"
  )
  if (!is.list(gate) || !identical(names(gate), fields)) {
    stop("Preparation gate has an invalid structure.", call. = FALSE)
  }
  validate_preparation_report(
    gate$report,
    context$universe,
    context$cohort,
    context$snapshot,
    context$lane
  )
  execution_order <- preparation_dependency_order(context$universe)
  if (!identical(gate$execution_order, execution_order)) {
    stop("Preparation gate execution order is inconsistent.", call. = FALSE)
  }

  source_packages <- context$source_plan$sources$package
  if (
    !is.list(gate$source_acquisitions) ||
      !identical(names(gate$source_acquisitions), source_packages)
  ) {
    stop("Preparation gate source acquisitions are incomplete.", call. = FALSE)
  }
  for (acquisition in gate$source_acquisitions) {
    validate_source_acquisition(
      acquisition,
      context$source_plan,
      context$universe,
      context$cohort,
      context$snapshot,
      context$binary_reuse,
      context$lane,
      context$path_plan
    )
  }
  expected_sources <- preparation_gate_source_rows(
    gate$source_acquisitions,
    context$source_plan
  )
  if (!identical(gate$report$sources, expected_sources)) {
    stop("Preparation gate source evidence is inconsistent.", call. = FALSE)
  }

  preparations <- gate$source_preparations
  preparation_names <- names(preparations)
  if (is.null(preparation_names)) {
    preparation_names <- character()
  }
  if (
    !is.list(preparations) ||
      anyNA(preparation_names) ||
      any(!nzchar(preparation_names)) ||
      anyDuplicated(preparation_names) ||
      !identical(
        preparation_names,
        sort(preparation_names, method = "radix")
      )
  ) {
    stop("Preparation gate source preparations are invalid.", call. = FALSE)
  }
  for (preparation in preparations) {
    validate_source_preparation(preparation, context)
  }
  attempts <- preparation_gate_attempt_records(gate$report$attempts)
  validate_source_preparation_logs(attempts, context$path_plan)

  results <- list()
  expected_preparations <- character()
  requirements <- preparation_required_packages(
    context$source_plan$requirements
  )
  for (package in execution_order) {
    version <- requirements$version[requirements$package == package]
    if (is.na(version)) {
      expected <- preparation_gate_unavailable_result(package)
    } else {
      blocker <- preparation_gate_blocker(package, results, context$universe)
      if (!is.na(blocker)) {
        expected <- preparation_gate_blocked_result(package, version, blocker)
      } else {
        selection <- context$binary_reuse$selections[[package]]
        if (identical(selection$status, "selected")) {
          expected <- preparation_gate_hit_result(
            package,
            version,
            selection$artifact
          )
        } else {
          expected_preparations <- c(expected_preparations, package)
          if (!package %in% preparation_names) {
            stop(
              "Preparation gate is missing an eligible source preparation.",
              call. = FALSE
            )
          }
          expected <- preparations[[package]]$result
        }
      }
    }
    observed <- gate$report$results[
      gate$report$results$package == package,
      ,
      drop = FALSE
    ]
    rownames(observed) <- NULL
    if (!identical(observed, expected)) {
      stop("Preparation gate package result is inconsistent.", call. = FALSE)
    }
    results[[package]] <- expected
  }
  expected_preparations <- sort(expected_preparations, method = "radix")
  if (!identical(preparation_names, expected_preparations)) {
    stop(
      "Preparation gate source preparations are inconsistent.",
      call. = FALSE
    )
  }
  preparation_attempt_ids <- unlist(lapply(preparations, function(preparation) {
    vapply(preparation$attempts, `[[`, character(1L), "attempt_id")
  }))
  if (!all(preparation_attempt_ids %in% gate$report$attempts$attempt_id)) {
    stop("Preparation gate attempt history is incomplete.", call. = FALSE)
  }

  artifacts <- preparation_gate_artifacts(
    gate$source_acquisitions,
    preparations,
    gate$report$results,
    context$binary_reuse
  )
  expected_artifacts <- normalize_preparation_artifacts(
    artifacts,
    context$lane
  )
  if (!identical(gate$report$artifacts, expected_artifacts)) {
    stop("Preparation gate artifact evidence is inconsistent.", call. = FALSE)
  }

  invisible(gate)
}

# nolint end
