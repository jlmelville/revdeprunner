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
    validate_repository_projection_contents(
      repository_path,
      manifest,
      context$lane,
      repositories_root
    )
    return(new_repository_projection(
      gate$report,
      context$lane,
      manifest,
      repository_path,
      reused = TRUE
    ))
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
      return(projection)
    }
    stop("Unable to publish the repository projection.", call. = FALSE)
  }
  remove_staging <- FALSE

  new_repository_projection(
    gate$report,
    context$lane,
    manifest,
    repository_path,
    reused = FALSE
  )
}

prepare_repository_universe <- function(gate, context) {
  projection <- project_preparation_repository(gate, context)
  bundle <- list(
    prepared_gate = gate,
    projection = projection,
    report = gate$report,
    execution_order = gate$execution_order
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

  for (row in seq_len(nrow(manifest))) {
    validate_package_name(manifest$package[[row]])
    validate_package_version(manifest$version[[row]])
    validate_sha256_identity(manifest$artifact_id[[row]], "artifact_id")
    validate_sha256(manifest$sha256[[row]], "sha256")
    validate_warehouse_archive_name(manifest$archive_name[[row]])
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
  alternate_indexes <- file.path(
    contrib_path,
    c("PACKAGES.gz", "PACKAGES.rds")
  )
  unlink(alternate_indexes[file.exists(alternate_indexes)], force = TRUE)
  if (any(file.exists(alternate_indexes))) {
    stop("Unable to remove alternate repository metadata.", call. = FALSE)
  }
  invisible(NULL)
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
  if (
    !"PACKAGES" %in% entries ||
      any(!entries %in% c(manifest$archive_name, "PACKAGES")) ||
      anyDuplicated(entries)
  ) {
    stop("Repository projection contains unexpected entries.", call. = FALSE)
  }
  if (
    !setequal(intersect(entries, manifest$archive_name), manifest$archive_name)
  ) {
    stop("Repository projection artifact set is incomplete.", call. = FALSE)
  }

  index_path <- file.path(contrib_path, "PACKAGES")
  if (
    warehouse_path_is_link(index_path) ||
      !utils::file_test("-f", index_path) ||
      dir.exists(index_path)
  ) {
    stop("Repository PACKAGES metadata is not a regular file.", call. = FALSE)
  }
  index <- tryCatch(
    read.dcf(index_path),
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

validate_repository_preparation <- function(bundle, context) {
  fields <- c(
    "prepared_gate",
    "projection",
    "report",
    "execution_order"
  )
  validate_source_preparation_context_record(context)
  if (
    !is.list(bundle) ||
      !identical(names(bundle), fields) ||
      !is.list(bundle$prepared_gate) ||
      !inherits(
        bundle$prepared_gate$report,
        "revdeprunner_preparation_report"
      ) ||
      !inherits(
        bundle$projection,
        "revdeprunner_repository_projection"
      ) ||
      !inherits(bundle$report, "revdeprunner_preparation_report")
  ) {
    stop("Repository preparation has an invalid structure.", call. = FALSE)
  }
  projection_fields <- c(
    "report_id",
    "lane_id",
    "manifest",
    "repository_path",
    "contrib_url",
    "reused"
  )
  projection <- bundle$projection
  validate_composite_contract_record(
    projection,
    projection_fields,
    "revdeprunner_repository_projection",
    "repository projection"
  )
  if (
    !is.logical(projection$reused) ||
      length(projection$reused) != 1L ||
      is.na(projection$reused)
  ) {
    stop("Repository projection has an invalid structure.", call. = FALSE)
  }
  bindings <- c(
    snapshot_id = context$snapshot$snapshot_id,
    cohort_id = context$cohort$cohort_id,
    universe_id = context$universe$universe_id,
    lane_id = context$lane$lane_id
  )
  if (
    !identical(
      unlist(bundle$report[names(bindings)], use.names = TRUE),
      bindings
    ) ||
      !identical(
        unlist(
          bundle$prepared_gate$report[names(bindings)],
          use.names = TRUE
        ),
        bindings
      ) ||
      !identical(bundle$report, bundle$prepared_gate$report) ||
      !identical(
        projection$report_id,
        bundle$prepared_gate$report$report_id
      ) ||
      !identical(projection$lane_id, context$lane$lane_id) ||
      !identical(
        bundle$execution_order,
        bundle$prepared_gate$execution_order
      )
  ) {
    stop(
      "Repository preparation does not match its preparation context.",
      call. = FALSE
    )
  }

  require_prepared_repository_report(bundle$report)
  repositories_root <- repository_projection_root(context$path_plan)
  expected_path <- repository_projection_path(
    repositories_root,
    bundle$prepared_gate$report$report_id
  )
  validate_resolved_runtime_anchor(
    projection$repository_path,
    "repository_path"
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

  invisible(bundle)
}

# nolint end
