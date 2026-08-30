# This file composes private helpers defined in other package source files.
# nolint start: object_usage_linter.

project_preparation_repository <- function(gate, context) {
  require_linux_repository_projection()
  validate_preparation_gate(gate, context)
  require_prepared_repository_report(gate$report)
  manifest <- repository_projection_manifest(gate, context)
  repositories_root <- repository_projection_root(context$path_plan)
  repository_path <- repository_projection_path(
    repositories_root,
    gate$report$report_id
  )

  if (repository_projection_path_exists(repository_path)) {
    projection <- new_repository_projection(
      gate$report,
      context$lane,
      manifest,
      repository_path,
      reused = TRUE
    )
    validate_repository_projection(projection, gate, context)
    return(projection)
  }

  staging_root <- ensure_repository_projection_directory(
    file.path(repositories_root, ".staging"),
    repositories_root,
    "repository staging root"
  )
  staging_path <- tempfile(".projection-", tmpdir = staging_root)
  if (!dir.create(staging_path, recursive = FALSE)) {
    stop("Unable to create repository projection staging.", call. = FALSE)
  }
  staging_path <- normalizePath(staging_path, winslash = "/", mustWork = TRUE)
  remove_staging <- TRUE
  on.exit(
    if (remove_staging) unlink(staging_path, recursive = TRUE, force = TRUE),
    add = TRUE
  )

  contrib_path <- file.path(staging_path, "src", "contrib")
  if (!dir.create(contrib_path, recursive = TRUE)) {
    stop("Unable to create repository contribution staging.", call. = FALSE)
  }
  contrib_path <- normalizePath(contrib_path, winslash = "/", mustWork = TRUE)
  for (row in seq_len(nrow(manifest))) {
    repository_projection_copy_artifact(
      manifest[row, , drop = FALSE],
      contrib_path,
      context$lane,
      context$path_plan
    )
  }
  repository_write_packages(contrib_path)
  validate_repository_projection_contents(
    staging_path,
    manifest,
    context$lane,
    repositories_root
  )

  published <- suppressWarnings(file.rename(staging_path, repository_path))
  if (!isTRUE(published)) {
    if (repository_projection_path_exists(repository_path)) {
      validate_repository_projection_contents(
        repository_path,
        manifest,
        context$lane,
        repositories_root
      )
      projection <- new_repository_projection(
        gate$report,
        context$lane,
        manifest,
        repository_path,
        reused = TRUE
      )
      validate_repository_projection(projection, gate, context)
      return(projection)
    }
    stop("Unable to publish the repository projection.", call. = FALSE)
  }
  remove_staging <- FALSE

  projection <- new_repository_projection(
    gate$report,
    context$lane,
    manifest,
    repository_path,
    reused = FALSE
  )
  validate_repository_projection(projection, gate, context)
  projection
}

prepare_repository_universe <- function(
  gate,
  context,
  timeout_seconds = 600L
) {
  require_linux_repository_projection()
  validate_preparation_gate(gate, context)
  require_prepared_repository_report(gate$report)
  timeout_seconds <- normalize_source_preparation_timeout(timeout_seconds)
  projection <- project_preparation_repository(gate, context)
  execution_order <- gate$execution_order
  verification <- repository_verification_paths(context$path_plan)
  attempts <- preparation_gate_attempt_records(gate$report$attempts)
  install_results <- list()

  for (package in execution_order) {
    prepared_result <- repository_prepared_result(gate$report, package)
    blocker <- preparation_gate_blocker(
      package,
      install_results,
      context$universe
    )
    if (!is.na(blocker)) {
      install_results[[package]] <- preparation_gate_blocked_result(
        package,
        prepared_result$version[[1L]],
        blocker
      )
      next
    }

    install_attempt <- run_repository_package_attempt(
      package,
      prepared_result$version[[1L]],
      "install",
      projection,
      verification,
      context,
      timeout_seconds
    )
    attempts <- preparation_gate_append_attempts(
      attempts,
      list(install_attempt)
    )
    install_results[[package]] <- if (
      identical(install_attempt$outcome, "success")
    ) {
      prepared_result
    } else {
      repository_attempt_result(
        package,
        prepared_result$version[[1L]],
        if (identical(install_attempt$outcome, "timeout")) {
          "timeout"
        } else {
          "installation-failure"
        },
        prepared_result$artifact_id[[1L]],
        install_attempt
      )
    }
  }

  results <- list()
  for (package in execution_order) {
    installed_result <- install_results[[package]]
    if (!identical(installed_result$outcome[[1L]], "prepared")) {
      results[[package]] <- installed_result
      next
    }

    blocker <- preparation_gate_blocker(package, results, context$universe)
    if (!is.na(blocker)) {
      results[[package]] <- preparation_gate_blocked_result(
        package,
        installed_result$version[[1L]],
        blocker
      )
      next
    }

    namespace_attempt <- run_repository_package_attempt(
      package,
      installed_result$version[[1L]],
      "namespace-load",
      projection,
      verification,
      context,
      timeout_seconds
    )
    attempts <- preparation_gate_append_attempts(
      attempts,
      list(namespace_attempt)
    )
    results[[package]] <- repository_attempt_result(
      package,
      installed_result$version[[1L]],
      if (identical(namespace_attempt$outcome, "success")) {
        "ready"
      } else if (identical(namespace_attempt$outcome, "timeout")) {
        "timeout"
      } else {
        "namespace-load-failure"
      },
      installed_result$artifact_id[[1L]],
      namespace_attempt
    )
  }

  ready_report <- new_preparation_report(
    context$universe,
    context$cohort,
    context$snapshot,
    context$lane,
    repository_report_artifact_records(gate$report, results),
    gate$report$sources,
    attempts,
    do.call(rbind, unname(results))
  )
  bundle <- list(
    prepared_gate = gate,
    projection = projection,
    report = ready_report,
    verification_root = verification$root,
    library_path = verification$library,
    execution_order = execution_order
  )
  validate_repository_preparation(bundle, context)
  bundle
}

require_linux_repository_projection <- function() {
  if (!identical(unname(Sys.info()[["sysname"]]), "Linux")) {
    stop(
      "Exact repository projection is currently supported only on Linux.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

require_prepared_repository_report <- function(report) {
  if (
    nrow(report$results) == 0L ||
      any(report$results$outcome != "prepared") ||
      anyNA(report$results$artifact_id)
  ) {
    stop(
      "Repository projection requires every preparation result to be prepared.",
      call. = FALSE
    )
  }

  invisible(report)
}

repository_projection_manifest <- function(gate, context) {
  results <- gate$report$results
  rows <- lapply(seq_len(nrow(results)), function(row) {
    package <- results$package[[row]]
    artifact_id <- results$artifact_id[[row]]
    artifact <- gate$report$artifacts[
      gate$report$artifacts$artifact_id == artifact_id,
      ,
      drop = FALSE
    ]
    if (
      nrow(artifact) != 1L ||
        !identical(artifact$archive_type[[1L]], "binary") ||
        !identical(artifact$lane_id[[1L]], context$lane$lane_id)
    ) {
      stop("Repository projection binary evidence is ambiguous.", call. = FALSE)
    }
    archive_name <- repository_projection_archive_name(package, gate, context)
    warehouse_path <- source_acquisition_warehouse_path(
      context$path_plan,
      repository_manifest_artifact(artifact, context$lane)
    )
    data.frame(
      package = package,
      version = artifact$version[[1L]],
      artifact_id = artifact_id,
      sha256 = artifact$sha256[[1L]],
      archive_name = archive_name,
      warehouse_path = warehouse_path,
      stringsAsFactors = FALSE
    )
  })
  manifest <- do.call(rbind, rows)
  rownames(manifest) <- NULL
  normalize_repository_projection_manifest(
    manifest,
    context$lane,
    context$path_plan
  )
}

repository_projection_archive_name <- function(package, gate, context) {
  if (package %in% names(gate$source_preparations)) {
    path <- gate$source_preparations[[package]]$binary_path
  } else {
    selection <- context$binary_reuse$selections[[package]]
    if (!identical(selection$status, "selected")) {
      stop(
        "Repository projection has no binary filename evidence.",
        call. = FALSE
      )
    }
    path <- selection$source_path
  }
  archive_name <- validate_warehouse_archive_name(basename(path))
  fields <- archive_filename_fields(archive_name)
  if (
    !identical(fields$package, package) ||
      is.na(fields$platform)
  ) {
    stop(
      "Repository projection binary filename is inconsistent.",
      call. = FALSE
    )
  }
  archive_name
}

repository_projection_manifest_fields <- function() {
  c(
    "package",
    "version",
    "artifact_id",
    "sha256",
    "archive_name",
    "warehouse_path"
  )
}

normalize_repository_projection_manifest <- function(
  manifest,
  lane,
  path_plan
) {
  fields <- repository_projection_manifest_fields()
  if (
    !is.data.frame(manifest) ||
      !identical(names(manifest), fields) ||
      !all(vapply(manifest, is.character, logical(1L))) ||
      nrow(manifest) == 0L ||
      anyNA(manifest)
  ) {
    stop(
      "Repository projection manifest has an invalid structure.",
      call. = FALSE
    )
  }
  if (
    anyDuplicated(manifest$package) ||
      anyDuplicated(manifest$artifact_id) ||
      anyDuplicated(manifest$archive_name) ||
      anyDuplicated(manifest$warehouse_path)
  ) {
    stop("Repository projection manifest is ambiguous.", call. = FALSE)
  }

  warehouse_root <- runtime_role_path(path_plan, "warehouse")
  for (row in seq_len(nrow(manifest))) {
    validate_package_name(manifest$package[[row]])
    validate_package_version(manifest$version[[row]])
    validate_sha256_identity(manifest$artifact_id[[row]], "artifact_id")
    validate_sha256(manifest$sha256[[row]], "sha256")
    archive_name <- validate_warehouse_archive_name(
      manifest$archive_name[[row]]
    )
    artifact <- repository_manifest_artifact(
      manifest[row, , drop = FALSE],
      lane
    )
    expected_path <- source_acquisition_warehouse_path(path_plan, artifact)
    if (!identical(manifest$warehouse_path[[row]], expected_path)) {
      stop(
        "Repository projection warehouse path is inconsistent.",
        call. = FALSE
      )
    }
    validate_existing_warehouse_artifact(
      expected_path,
      artifact,
      warehouse_root
    )
    validate_warehouse_archive(expected_path, artifact, archive_name)
  }

  manifest <- manifest[
    order(manifest$package, method = "radix"),
    ,
    drop = FALSE
  ]
  rownames(manifest) <- NULL
  manifest
}

repository_manifest_artifact <- function(row, lane) {
  artifact <- new_artifact_identity(
    row$package[[1L]],
    row$version[[1L]],
    row$sha256[[1L]],
    "binary",
    lane
  )
  if (!identical(artifact$artifact_id, row$artifact_id[[1L]])) {
    stop(
      "Repository projection artifact identity is inconsistent.",
      call. = FALSE
    )
  }
  artifact
}

repository_projection_root <- function(path_plan) {
  repositories_root <- runtime_role_path(path_plan, "repositories")
  ensure_repository_projection_directory(
    repositories_root,
    dirname(repositories_root),
    "repository projection root"
  )
}

repository_projection_path <- function(repositories_root, report_id) {
  validate_sha256_identity(report_id, "report_id")
  file.path(repositories_root, sub("^sha256:", "", report_id))
}

ensure_repository_projection_directory <- function(path, anchor, label) {
  if (warehouse_path_is_link(path)) {
    stop(sprintf("The %s must not be a symbolic link.", label), call. = FALSE)
  }
  if (file.exists(path) && !dir.exists(path)) {
    stop(sprintf("The %s must identify a directory.", label), call. = FALSE)
  }
  if (!dir.exists(path) && !dir.create(path, recursive = FALSE)) {
    stop(sprintf("Unable to create the %s.", label), call. = FALSE)
  }
  resolved <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (!identical(path, resolved) || !path_is_within(anchor, resolved)) {
    stop(sprintf("The %s escapes its managed root.", label), call. = FALSE)
  }
  resolved
}

repository_projection_path_exists <- function(path) {
  file.exists(path) || warehouse_path_is_link(path)
}

repository_projection_copy_artifact <- function(
  row,
  contrib_path,
  lane,
  path_plan
) {
  artifact <- repository_manifest_artifact(row, lane)
  source_path <- row$warehouse_path[[1L]]
  source_before <- warehouse_file_snapshot(source_path)
  validate_existing_warehouse_artifact(
    source_path,
    artifact,
    runtime_role_path(path_plan, "warehouse")
  )
  destination <- file.path(contrib_path, row$archive_name[[1L]])
  copied <- warehouse_copy_file(
    source_path,
    destination,
    overwrite = FALSE,
    copy.mode = FALSE,
    copy.date = FALSE
  )
  if (!isTRUE(copied)) {
    stop("Unable to copy an artifact into repository staging.", call. = FALSE)
  }
  validate_warehouse_archive(destination, artifact, row$archive_name[[1L]])
  validate_warehouse_source_unchanged(source_path, source_before)
  invisible(destination)
}

repository_write_packages <- function(contrib_path) {
  tryCatch(
    withCallingHandlers(
      tools::write_PACKAGES(
        contrib_path,
        type = "source",
        latestOnly = FALSE,
        addFiles = TRUE,
        verbose = FALSE
      ),
      warning = function(warning) {
        stop(conditionMessage(warning), call. = FALSE)
      }
    ),
    error = function(error) {
      stop(
        sprintf(
          "Unable to generate repository metadata: %s",
          conditionMessage(error)
        ),
        call. = FALSE
      )
    }
  )
}

validate_repository_projection_contents <- function(
  repository_path,
  manifest,
  lane,
  repositories_root
) {
  if (
    warehouse_path_is_link(repository_path) ||
      !dir.exists(repository_path)
  ) {
    stop("Repository projection must identify a directory.", call. = FALSE)
  }
  repository_path <- normalizePath(
    repository_path,
    winslash = "/",
    mustWork = TRUE
  )
  if (!path_is_within(repositories_root, repository_path)) {
    stop("Repository projection escapes its managed root.", call. = FALSE)
  }
  contrib_path <- file.path(repository_path, "src", "contrib")
  if (warehouse_path_is_link(contrib_path) || !dir.exists(contrib_path)) {
    stop("Repository projection has no contribution directory.", call. = FALSE)
  }
  contrib_path <- normalizePath(contrib_path, winslash = "/", mustWork = TRUE)
  if (!path_is_within(repository_path, contrib_path)) {
    stop(
      "Repository contribution directory escapes its projection.",
      call. = FALSE
    )
  }

  entries <- list.files(
    contrib_path,
    all.files = TRUE,
    full.names = FALSE,
    recursive = FALSE,
    include.dirs = TRUE,
    no.. = TRUE
  )
  allowed_metadata <- c("PACKAGES", "PACKAGES.gz", "PACKAGES.rds")
  if (
    !"PACKAGES" %in% entries ||
      any(!entries %in% c(manifest$archive_name, allowed_metadata)) ||
      anyDuplicated(entries)
  ) {
    stop("Repository projection contains unexpected entries.", call. = FALSE)
  }
  if (
    !setequal(intersect(entries, manifest$archive_name), manifest$archive_name)
  ) {
    stop("Repository projection artifact set is incomplete.", call. = FALSE)
  }

  index <- tryCatch(
    read.dcf(file.path(contrib_path, "PACKAGES")),
    error = function(error) {
      stop("Repository PACKAGES metadata is unreadable.", call. = FALSE)
    },
    warning = function(warning) {
      stop("Repository PACKAGES metadata is unreadable.", call. = FALSE)
    }
  )
  if (
    !all(c("Package", "Version", "File") %in% colnames(index)) ||
      nrow(index) != nrow(manifest) ||
      anyDuplicated(index[, "Package"])
  ) {
    stop("Repository PACKAGES metadata is not exact.", call. = FALSE)
  }
  observed <- data.frame(
    package = unname(index[, "Package"]),
    version = unname(index[, "Version"]),
    archive_name = unname(index[, "File"]),
    stringsAsFactors = FALSE
  )
  observed <- observed[
    order(observed$package, method = "radix"),
    ,
    drop = FALSE
  ]
  rownames(observed) <- NULL
  expected <- manifest[c("package", "version", "archive_name")]
  if (!identical(observed, expected)) {
    stop(
      "Repository PACKAGES metadata differs from its manifest.",
      call. = FALSE
    )
  }

  for (row in seq_len(nrow(manifest))) {
    path <- file.path(contrib_path, manifest$archive_name[[row]])
    if (warehouse_path_is_link(path) || !utils::file_test("-f", path)) {
      stop(
        "Repository projection artifact is not a regular file.",
        call. = FALSE
      )
    }
    validate_warehouse_archive(
      path,
      repository_manifest_artifact(manifest[row, , drop = FALSE], lane),
      manifest$archive_name[[row]]
    )
  }

  invisible(repository_path)
}

new_repository_projection <- function(
  report,
  lane,
  manifest,
  repository_path,
  reused
) {
  structure(
    list(
      report_id = report$report_id,
      lane_id = lane$lane_id,
      manifest = manifest,
      repository_path = normalizePath(
        repository_path,
        winslash = "/",
        mustWork = TRUE
      ),
      contrib_url = paste0(
        "file://",
        file.path(repository_path, "src", "contrib")
      ),
      reused = reused
    ),
    class = "revdeprunner_repository_projection"
  )
}

validate_repository_projection <- function(projection, gate, context) {
  fields <- c(
    "report_id",
    "lane_id",
    "manifest",
    "repository_path",
    "contrib_url",
    "reused"
  )
  if (
    !inherits(projection, "revdeprunner_repository_projection") ||
      !is.list(projection) ||
      !identical(names(projection), fields) ||
      !is.logical(projection$reused) ||
      length(projection$reused) != 1L ||
      is.na(projection$reused) ||
      !identical(projection$report_id, gate$report$report_id) ||
      !identical(projection$lane_id, context$lane$lane_id)
  ) {
    stop("Repository projection has an invalid structure.", call. = FALSE)
  }
  expected_manifest <- repository_projection_manifest(gate, context)
  if (!identical(projection$manifest, expected_manifest)) {
    stop("Repository projection manifest is inconsistent.", call. = FALSE)
  }
  repositories_root <- repository_projection_root(context$path_plan)
  expected_path <- repository_projection_path(
    repositories_root,
    gate$report$report_id
  )
  if (
    !identical(projection$repository_path, expected_path) ||
      !identical(
        projection$contrib_url,
        paste0("file://", file.path(expected_path, "src", "contrib"))
      )
  ) {
    stop("Repository projection path is inconsistent.", call. = FALSE)
  }
  validate_repository_projection_contents(
    projection$repository_path,
    projection$manifest,
    context$lane,
    repositories_root
  )
  invisible(projection)
}

repository_verification_paths <- function(path_plan) {
  run_root <- runtime_role_path(path_plan, "run")
  run_root <- ensure_source_acquisition_directory(
    run_root,
    path_plan$runs_root,
    "repository verification run root"
  )
  verification_root <- ensure_source_acquisition_directory(
    file.path(run_root, "repository-verification"),
    run_root,
    "repository verification directory"
  )
  root <- tempfile("attempt-", tmpdir = verification_root)
  if (!dir.create(root, recursive = FALSE)) {
    stop("Unable to create repository verification state.", call. = FALSE)
  }
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  library <- ensure_source_acquisition_directory(
    file.path(root, "library"),
    root,
    "repository verification library"
  )
  logs <- ensure_source_acquisition_directory(
    file.path(root, "logs"),
    root,
    "repository verification logs"
  )
  list(root = root, library = library, logs = logs)
}

run_repository_package_attempt <- function(
  package,
  version,
  stage,
  projection,
  verification,
  context,
  timeout_seconds
) {
  logs <- repository_package_log_paths(verification$logs, package, stage)
  arguments <- if (identical(stage, "install")) {
    repository_install_arguments(
      package,
      version,
      verification$library,
      projection$contrib_url
    )
  } else {
    repository_namespace_arguments(package, version, verification$library)
  }
  process <- run_repository_preparation_process(
    context$command_plan$r_executable,
    arguments,
    verification$root,
    logs$stdout,
    logs$stderr,
    timeout_seconds
  )
  source_preparation_attempt_from_process(
    package,
    version,
    stage,
    process,
    logs,
    context$path_plan
  )
}

repository_package_log_paths <- function(log_root, package, stage) {
  package_root <- ensure_source_acquisition_directory(
    file.path(log_root, package),
    log_root,
    "repository package log directory"
  )
  source_preparation_log_paths(package_root, stage)
}

repository_install_arguments <- function(
  package,
  version,
  library,
  contrib_url
) {
  expression <- paste(
    "args <- commandArgs(TRUE)",
    paste0(
      "utils::install.packages(args[[1L]], lib = args[[2L]], ",
      "contriburl = args[[3L]], repos = NULL, type = 'source', ",
      "dependencies = FALSE, quiet = TRUE)"
    ),
    "path <- find.package(args[[1L]], lib.loc = args[[2L]], quiet = TRUE)",
    paste0(
      "expected <- normalizePath(file.path(args[[2L]], args[[1L]]), ",
      "winslash = '/', mustWork = TRUE)"
    ),
    "observed <- normalizePath(path, winslash = '/', mustWork = TRUE)",
    "if (!identical(observed, expected)) stop('installed path mismatch')",
    paste0(
      "installed <- as.character(utils::packageVersion(args[[1L]], ",
      "lib.loc = args[[2L]]))"
    ),
    "if (!identical(installed, args[[4L]])) stop('installed version mismatch')",
    sep = "; "
  )
  c(
    "--vanilla",
    "--slave",
    "-e",
    expression,
    "--args",
    package,
    library,
    contrib_url,
    version
  )
}

repository_namespace_arguments <- function(package, version, library) {
  expression <- paste(
    "args <- commandArgs(TRUE)",
    ".libPaths(unique(c(args[[2L]], .Library, .Library.site)))",
    paste0(
      "expected <- normalizePath(file.path(args[[2L]], args[[1L]]), ",
      "winslash = '/', mustWork = TRUE)"
    ),
    "namespace <- loadNamespace(args[[1L]], lib.loc = args[[2L]])",
    paste0(
      "observed <- normalizePath(getNamespaceInfo(namespace, 'path'), ",
      "winslash = '/', mustWork = TRUE)"
    ),
    "if (!identical(observed, expected)) stop('namespace path mismatch')",
    "loaded <- as.character(getNamespaceVersion(namespace))",
    "if (!identical(loaded, args[[3L]])) stop('namespace version mismatch')",
    "unloadNamespace(args[[1L]])",
    sep = "; "
  )
  c(
    "--vanilla",
    "--slave",
    "-e",
    expression,
    "--args",
    package,
    library,
    version
  )
}

run_repository_preparation_process <- function(
  r_executable,
  arguments,
  working_directory,
  stdout_path,
  stderr_path,
  timeout_seconds
) {
  run_source_preparation_process(
    r_executable,
    arguments,
    working_directory,
    stdout_path,
    stderr_path,
    timeout_seconds
  )
}

repository_attempt_result <- function(
  package,
  version,
  outcome,
  artifact_id,
  attempt
) {
  data.frame(
    package = package,
    version = version,
    outcome = outcome,
    artifact_id = artifact_id,
    evidence_attempt_id = attempt$attempt_id,
    blocking_dependency = NA_character_,
    diagnostic_excerpt = if (identical(outcome, "ready")) {
      NA_character_
    } else {
      attempt$diagnostic_excerpt
    },
    stringsAsFactors = FALSE
  )
}

repository_prepared_result <- function(report, package) {
  result <- report$results[report$results$package == package, , drop = FALSE]
  rownames(result) <- NULL
  if (nrow(result) != 1L || !identical(result$outcome[[1L]], "prepared")) {
    stop(
      "Repository verification prepared result is inconsistent.",
      call. = FALSE
    )
  }
  result
}

preparation_report_artifact_records <- function(artifacts) {
  lapply(seq_len(nrow(artifacts)), function(row) {
    artifact <- structure(
      as.list(artifacts[row, , drop = FALSE]),
      class = "revdeprunner_artifact_identity"
    )
    validate_artifact_identity(artifact)
    artifact
  })
}

repository_report_artifact_records <- function(report, results) {
  result_table <- do.call(rbind, unname(results))
  referenced <- unique(c(
    report$sources$artifact_id,
    result_table$artifact_id[!is.na(result_table$artifact_id)]
  ))
  preparation_report_artifact_records(
    report$artifacts[
      report$artifacts$artifact_id %in% referenced,
      ,
      drop = FALSE
    ]
  )
}

validate_repository_preparation <- function(bundle, context) {
  fields <- c(
    "prepared_gate",
    "projection",
    "report",
    "verification_root",
    "library_path",
    "execution_order"
  )
  if (!is.list(bundle) || !identical(names(bundle), fields)) {
    stop("Repository preparation has an invalid structure.", call. = FALSE)
  }
  validate_preparation_gate(bundle$prepared_gate, context)
  require_prepared_repository_report(bundle$prepared_gate$report)
  validate_repository_projection(
    bundle$projection,
    bundle$prepared_gate,
    context
  )
  validate_preparation_report(
    bundle$report,
    context$universe,
    context$cohort,
    context$snapshot,
    context$lane
  )
  if (
    !identical(bundle$execution_order, bundle$prepared_gate$execution_order) ||
      !identical(
        bundle$report$requirements,
        bundle$prepared_gate$report$requirements
      ) ||
      !identical(bundle$report$sources, bundle$prepared_gate$report$sources) ||
      any(bundle$report$results$outcome == "prepared")
  ) {
    stop("Repository preparation evidence is inconsistent.", call. = FALSE)
  }
  expected_artifacts <- bundle$prepared_gate$report$artifacts[
    bundle$prepared_gate$report$artifacts$artifact_id %in%
      bundle$report$artifacts$artifact_id,
    ,
    drop = FALSE
  ]
  rownames(expected_artifacts) <- NULL
  if (!identical(bundle$report$artifacts, expected_artifacts)) {
    stop(
      "Repository preparation artifact evidence is inconsistent.",
      call. = FALSE
    )
  }

  prior_attempts <- bundle$prepared_gate$report$attempts
  observed_prior <- bundle$report$attempts[
    bundle$report$attempts$attempt_id %in% prior_attempts$attempt_id,
    ,
    drop = FALSE
  ]
  rownames(observed_prior) <- NULL
  if (!identical(observed_prior, prior_attempts)) {
    stop(
      "Repository preparation discarded prior attempt evidence.",
      call. = FALSE
    )
  }
  new_attempts <- bundle$report$attempts[
    !bundle$report$attempts$attempt_id %in% prior_attempts$attempt_id,
    ,
    drop = FALSE
  ]
  if (
    nrow(new_attempts) == 0L ||
      any(!new_attempts$stage %in% c("install", "namespace-load"))
  ) {
    stop(
      "Repository preparation attempt evidence is incomplete.",
      call. = FALSE
    )
  }
  validate_source_preparation_logs(
    preparation_gate_attempt_records(bundle$report$attempts),
    context$path_plan
  )

  run_root <- runtime_role_path(context$path_plan, "run")
  for (path in c(bundle$verification_root, bundle$library_path)) {
    if (
      warehouse_path_is_link(path) ||
        !dir.exists(path) ||
        !identical(
          path,
          normalizePath(path, winslash = "/", mustWork = TRUE)
        ) ||
        !path_is_within(run_root, path)
    ) {
      stop("Repository verification state escapes its run root.", call. = FALSE)
    }
  }
  for (package in bundle$report$results$package[
    bundle$report$results$outcome == "ready"
  ]) {
    version <- bundle$report$results$version[
      bundle$report$results$package == package
    ]
    path <- find.package(package, lib.loc = bundle$library_path, quiet = TRUE)
    if (
      !nzchar(path) ||
        !identical(
          normalizePath(path, winslash = "/", mustWork = TRUE),
          file.path(bundle$library_path, package)
        ) ||
        !identical(
          as.character(utils::packageVersion(
            package,
            lib.loc = bundle$library_path
          )),
          version
        )
    ) {
      stop(
        "Repository preparation installed package is inconsistent.",
        call. = FALSE
      )
    }
  }

  invisible(bundle)
}

# nolint end
