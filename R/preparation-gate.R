prepare_dependency_universe <- function(
  source_plan,
  universe,
  cohort,
  snapshot,
  binary_reuse,
  lane,
  path_plan,
  r_executable,
  baseline_source,
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
    r_executable = r_executable
  )
  validate_preparation_gate_context(context)
  timeout_seconds <- normalize_source_preparation_timeout(timeout_seconds)
  execution_steps <- preparation_dependency_steps(universe)
  execution_order <- preparation_dependency_order(universe)
  if (!is.null(previous)) {
    validate_preparation_gate_record(
      previous,
      context,
      require_hit_install_attempts = FALSE
    )
  }

  source_packages <- source_plan$sources$package[
    source_plan$sources$build_required == "true"
  ]
  source_acquisitions <- lapply(source_packages, function(package) {
    prior <- if (is.null(previous)) {
      NULL
    } else {
      previous$source_acquisitions[[package]]
    }
    acquire_source_artifact_in_context(
      package,
      source_plan,
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
  build_library <- source_preparation_build_library(path_plan)

  for (package in execution_steps) {
    if (identical(package, universe$runner_supplied)) {
      install_runner_supplied_baseline(
        baseline_source,
        context,
        build_library,
        timeout_seconds
      )
      next
    }
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
      installation <- preparation_gate_install_binary_hit(
        package,
        version,
        selection,
        context,
        build_library,
        timeout_seconds,
        previous
      )
      attempts <- preparation_gate_append_attempts(
        attempts,
        installation$attempts
      )
      results[[package]] <- installation$result
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
    if (
      !is.null(prior) &&
        !source_preparation_library_has_package(
          build_library,
          package,
          version
        )
    ) {
      installation <- preparation_gate_install_binary_artifact(
        package,
        version,
        prior$binary_artifact,
        prior$binary_path,
        context,
        build_library,
        timeout_seconds
      )
      attempts <- preparation_gate_append_attempts(
        attempts,
        list(installation)
      )
      if (!identical(installation$outcome, "success")) {
        prior <- NULL
      }
    }
    preparation <- prepare_source_binary_in_context(
      package,
      context,
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
  validate_preparation_gate_record(gate, context)
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
  r_executable <- normalize_r_executable(context$r_executable)
  if (!identical(context$r_executable, r_executable)) {
    stop(
      "Preparation R executable must be a resolved physical path.",
      call. = FALSE
    )
  }

  invisible(context)
}

preparation_dependency_order <- function(universe) {
  setdiff(preparation_dependency_steps(universe), universe$runner_supplied)
}

preparation_dependency_steps <- function(universe) {
  requirements <- preparation_required_packages(
    derive_preparation_requirements(universe)
  )
  packages <- requirements$package
  edges <- preparation_required_dependency_edges(universe)
  edges <- unique(edges[c("from_package", "dependency")])
  package_edges <- edges[
    edges$from_package %in%
      packages &
      edges$dependency %in% packages &
      edges$from_package != edges$dependency,
    ,
    drop = FALSE
  ]
  ordinary_order <- preparation_topological_order(packages, package_edges)
  runner_supplied <- universe$runner_supplied
  nodes <- c(packages, runner_supplied)
  edges <- edges[
    edges$from_package %in%
      nodes &
      edges$dependency %in% nodes &
      edges$from_package != edges$dependency,
    ,
    drop = FALSE
  ]
  priority <- stats::setNames(seq_along(ordinary_order), ordinary_order)
  priority[[runner_supplied]] <- 0L
  remaining <- nodes
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
    ready <- ready[
      order(priority[ready], ready, method = "radix")
    ]
    completed <- c(completed, ready[[1L]])
    remaining <- setdiff(remaining, ready[[1L]])
  }

  completed
}

preparation_topological_order <- function(packages, edges) {
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
  edges <- preparation_required_dependency_edges(universe)
  dependencies <- unique(edges$dependency[edges$from_package == package])
  dependencies <- sort(
    intersect(dependencies, result_packages),
    method = "radix"
  )
  unsuccessful <- dependencies[vapply(
    dependencies,
    function(dependency) {
      !identical(results[[dependency]]$outcome[[1L]], "prepared")
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

preparation_gate_install_binary_hit <- function(
  package,
  version,
  selection,
  context,
  build_library,
  timeout_seconds,
  previous = NULL
) {
  validate_inventory_artifact_selection(selection)
  if (
    !identical(selection$status, "selected") ||
      !identical(selection$package, package) ||
      !identical(selection$version, version) ||
      !identical(selection$lane_id, context$lane$lane_id)
  ) {
    stop("Preparation binary selection is inconsistent.", call. = FALSE)
  }
  previous_result <- if (is.null(previous)) {
    NULL
  } else {
    previous$report$results[
      previous$report$results$package == package,
      ,
      drop = FALSE
    ]
  }
  can_reuse <- !is.null(previous_result) &&
    identical(previous_result$outcome[[1L]], "prepared") &&
    identical(
      previous_result$artifact_id[[1L]],
      selection$artifact$artifact_id
    ) &&
    preparation_gate_has_successful_hit_install(
      previous$report,
      selection,
      context
    )
  if (
    can_reuse &&
      source_preparation_library_has_package(build_library, package, version)
  ) {
    return(list(
      result = preparation_gate_hit_result(
        package,
        version,
        selection$artifact
      ),
      attempts = list()
    ))
  }

  attempt <- preparation_gate_install_binary_artifact(
    package,
    version,
    selection$artifact,
    preparation_gate_hit_cache_path(selection, context),
    context,
    build_library,
    timeout_seconds
  )

  result <- if (identical(attempt$outcome, "success")) {
    preparation_gate_hit_result(package, version, selection$artifact)
  } else {
    outcome <- if (identical(attempt$outcome, "timeout")) {
      "timeout"
    } else {
      "installation-failure"
    }
    source_preparation_result(
      package,
      version,
      outcome,
      selection$artifact,
      attempt
    )
  }
  list(result = result, attempts = list(attempt))
}

preparation_gate_install_binary_artifact <- function(
  package,
  version,
  artifact,
  source_path,
  context,
  build_library,
  timeout_seconds
) {
  source_path <- normalize_warehouse_source(source_path, context$path_plan)
  source_before <- warehouse_file_snapshot(source_path)
  validate_warehouse_archive(source_path, artifact, basename(source_path))
  attempt_root <- source_preparation_attempt_directory(
    context$path_plan,
    package,
    version
  )
  logs <- source_preparation_log_paths(attempt_root, "install")
  arguments <- c(
    "CMD",
    "INSTALL",
    "--use-vanilla",
    paste0("--library=", build_library),
    source_path
  )
  process <- with_source_preparation_libraries(
    build_library,
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
    package,
    version,
    "install",
    process,
    logs,
    context$path_plan
  )
  validate_warehouse_source_unchanged(source_path, source_before)
  if (identical(attempt$outcome, "success")) {
    validate_source_preparation_library_package(
      build_library,
      package,
      version
    )
  }
  attempt
}

preparation_gate_source_rows <- function(acquisitions, source_plan) {
  if (length(acquisitions) == 0L) {
    values <- stats::setNames(
      replicate(
        length(preparation_source_fields()),
        character(),
        simplify = FALSE
      ),
      preparation_source_fields()
    )
    return(as.data.frame(
      values,
      stringsAsFactors = FALSE,
      optional = TRUE
    ))
  }
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
  validate_preparation_gate_record(gate, context)
}

validate_preparation_gate_record <- function(
  gate,
  context,
  require_hit_install_attempts = TRUE
) {
  validate_source_preparation_context_record(context)
  if (
    !is.logical(require_hit_install_attempts) ||
      length(require_hit_install_attempts) != 1L ||
      is.na(require_hit_install_attempts)
  ) {
    stop("Hit-install validation policy is invalid.", call. = FALSE)
  }
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

  source_packages <- context$source_plan$sources$package[
    context$source_plan$sources$build_required == "true"
  ]
  if (
    !is.list(gate$source_acquisitions) ||
      !identical(names(gate$source_acquisitions), source_packages)
  ) {
    stop("Preparation gate source acquisitions are incomplete.", call. = FALSE)
  }
  for (acquisition in gate$source_acquisitions) {
    validate_source_acquisition_record(
      acquisition,
      context$source_plan,
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
    validate_source_preparation_record(preparation, context)
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
          expected <- preparation_gate_expected_hit_result(
            gate$report,
            package,
            version,
            selection,
            context,
            require_hit_install_attempts
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

preparation_gate_expected_hit_result <- function(
  report,
  package,
  version,
  selection,
  context,
  require_hit_install_attempts
) {
  observed <- report$results[
    report$results$package == package,
    ,
    drop = FALSE
  ]
  rownames(observed) <- NULL
  if (identical(observed$outcome[[1L]], "prepared")) {
    if (
      require_hit_install_attempts &&
        !preparation_gate_has_successful_hit_install(
          report,
          selection,
          context
        )
    ) {
      stop(
        "Prepared binary hit lacks a successful binary-hit install attempt.",
        call. = FALSE
      )
    }
    return(preparation_gate_hit_result(package, version, selection$artifact))
  }
  if (
    !observed$outcome[[1L]] %in% c("installation-failure", "timeout") ||
      !identical(
        observed$artifact_id[[1L]],
        selection$artifact$artifact_id
      )
  ) {
    stop("Preparation gate binary-hit result is inconsistent.", call. = FALSE)
  }
  attempt <- report$attempts[
    report$attempts$attempt_id == observed$evidence_attempt_id[[1L]],
    ,
    drop = FALSE
  ]
  if (nrow(attempt) != 1L || !identical(attempt$stage[[1L]], "install")) {
    stop("Preparation gate binary-hit attempt is inconsistent.", call. = FALSE)
  }
  observed
}

preparation_gate_has_successful_hit_install <- function(
  report,
  selection,
  context
) {
  build_library <- file.path(
    runtime_role_path(context$path_plan, "run"),
    "build-library"
  )
  command <- render_source_preparation_command(
    context$r_executable,
    c(
      "CMD",
      "INSTALL",
      "--use-vanilla",
      paste0("--library=", build_library),
      preparation_gate_hit_cache_path(selection, context)
    )
  )
  matching <- report$attempts$package == selection$package &
    report$attempts$version == selection$version &
    report$attempts$stage == "install" &
    report$attempts$outcome == "success" &
    report$attempts$command == command
  any(matching)
}

preparation_gate_hit_cache_path <- function(selection, context) {
  cache_path <- context$binary_reuse$cache_paths[[selection$package]]
  validate_binary_cache_artifact(
    cache_path,
    selection$artifact,
    context$path_plan
  )
  cache_path
}
