runtime_root_plan_schema_version <- function() {
  "revdeprunner-runtime-root-plan/v5"
}

new_runtime_root_plan <- function(
  package_root,
  data_root,
  runs_root,
  run_id,
  source_cache_roots
) {
  package_root <- normalize_runtime_anchor(package_root, "package_root")
  data_root <- normalize_runtime_anchor(data_root, "data_root")
  runs_root <- normalize_runtime_anchor(runs_root, "runs_root")
  run_id <- validate_runtime_run_id(run_id)
  source_cache_roots <- normalize_runtime_source_cache_roots(
    source_cache_roots
  )

  validate_runtime_package_root(package_root)
  validate_runtime_anchor_boundaries(
    package_root,
    data_root,
    runs_root,
    source_cache_roots
  )
  paths <- runtime_root_path_table(
    package_root,
    data_root,
    runs_root,
    run_id,
    source_cache_roots
  )
  validate_runtime_derived_paths(paths, data_root, runs_root)

  schema_version <- runtime_root_plan_schema_version()
  identity_fields <- runtime_root_plan_identity_fields(
    run_id,
    package_root,
    data_root,
    runs_root,
    source_cache_roots,
    paths
  )
  plan <- structure(
    list(
      schema_version = schema_version,
      path_plan_id = record_identity(schema_version, identity_fields),
      run_id = run_id,
      package_root = package_root,
      data_root = data_root,
      runs_root = runs_root,
      source_cache_roots = source_cache_roots,
      paths = paths
    ),
    class = "revdeprunner_runtime_root_plan"
  )
  validate_runtime_root_plan(plan)
  plan
}

validate_runtime_root_plan <- function(plan) {
  fields <- c(
    "schema_version",
    "path_plan_id",
    "run_id",
    "package_root",
    "data_root",
    "runs_root",
    "source_cache_roots",
    "paths"
  )
  validate_composite_contract_record(
    plan,
    fields,
    "revdeprunner_runtime_root_plan",
    "runtime root plan"
  )
  if (!identical(plan$schema_version, runtime_root_plan_schema_version())) {
    stop("Runtime root plan schema version is unsupported.", call. = FALSE)
  }
  validate_sha256_identity(plan$path_plan_id, "path_plan_id")
  run_id <- validate_runtime_run_id(plan$run_id)

  package_root <- validate_resolved_runtime_anchor(
    plan$package_root,
    "package_root"
  )
  data_root <- validate_resolved_runtime_anchor(plan$data_root, "data_root")
  runs_root <- validate_resolved_runtime_anchor(plan$runs_root, "runs_root")
  # Cache providers are historical provenance once artifacts have been adopted.
  # Live selection validates the physical provider before reading its bytes.
  source_cache_roots <- normalize_runtime_source_cache_roots(
    plan$source_cache_roots,
    historical = TRUE
  )
  if (!identical(source_cache_roots, plan$source_cache_roots)) {
    stop("Runtime source-cache roots are not normalized.", call. = FALSE)
  }

  validate_runtime_package_root(package_root)
  validate_runtime_anchor_boundaries(
    package_root,
    data_root,
    runs_root,
    source_cache_roots
  )
  expected_paths <- runtime_root_path_table(
    package_root,
    data_root,
    runs_root,
    run_id,
    source_cache_roots
  )
  validate_runtime_path_table(plan$paths)
  if (!identical(plan$paths, expected_paths)) {
    stop("Runtime root path table does not match its anchors.", call. = FALSE)
  }
  validate_runtime_derived_paths(plan$paths, data_root, runs_root)

  identity_fields <- runtime_root_plan_identity_fields(
    run_id,
    package_root,
    data_root,
    runs_root,
    source_cache_roots,
    plan$paths
  )
  expected_id <- record_identity(plan$schema_version, identity_fields)
  if (!identical(plan$path_plan_id, expected_id)) {
    stop("Runtime root plan identity does not match its fields.", call. = FALSE)
  }

  invisible(plan)
}

validate_runtime_run_id <- function(run_id) {
  run_id <- validate_contract_text(run_id, "run_id")
  portable <- grepl(
    "^[A-Za-z0-9](?:[A-Za-z0-9._-]*[A-Za-z0-9])?$",
    run_id
  )
  reserved <- grepl(
    "^(?:CON|PRN|AUX|NUL|COM[1-9]|LPT[1-9])(?:\\..*)?$",
    run_id,
    ignore.case = TRUE
  )
  if (!portable || reserved || nchar(run_id, type = "bytes") > 128L) {
    stop("`run_id` must be one portable path component.", call. = FALSE)
  }

  run_id
}

normalize_runtime_anchor <- function(path, argument) {
  normalized <- normalize_existing_directory(path, argument)
  if (!runtime_path_is_absolute(normalized) || grepl("\\\\", normalized)) {
    stop(
      sprintf("`%s` must resolve to an absolute forward-slash path.", argument),
      call. = FALSE
    )
  }

  normalized
}

validate_resolved_runtime_anchor <- function(path, argument) {
  if (
    !is.character(path) ||
      length(path) != 1L ||
      is.na(path) ||
      !nzchar(path)
  ) {
    stop(sprintf("`%s` must be one resolved path.", argument), call. = FALSE)
  }
  normalized <- normalize_runtime_anchor(path, argument)
  if (!identical(path, normalized)) {
    stop(
      sprintf("`%s` is not a resolved physical path.", argument),
      call. = FALSE
    )
  }

  path
}

runtime_path_is_absolute <- function(path) {
  startsWith(path, "/") || grepl("^[A-Za-z]:/", path)
}

runtime_role_path <- function(path_plan, role) {
  selected <- path_plan$paths$path[path_plan$paths$role == role]
  if (length(selected) != 1L) {
    stop(sprintf("Runtime plan has no unique `%s` path.", role), call. = FALSE)
  }

  selected
}

normalize_existing_directory <- function(path, argument) {
  if (
    length(path) != 1L ||
      is.na(path) ||
      !is.character(path) ||
      !nzchar(path)
  ) {
    stop(sprintf("`%s` must be one non-empty path.", argument), call. = FALSE)
  }

  path <- path.expand(path)
  if (!file.exists(path)) {
    stop(
      sprintf("`%s` must identify an existing directory.", argument),
      call. = FALSE
    )
  }
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (!dir.exists(path)) {
    stop(sprintf("`%s` must identify a directory.", argument), call. = FALSE)
  }

  path
}

ensure_revdep_directory <- function(path, label) {
  path <- path.expand(validate_contract_text(path, label))
  if (!dir.exists(path) && !dir.create(path, recursive = TRUE)) {
    stop(sprintf("Unable to create the %s: %s", label, path), call. = FALSE)
  }
  normalize_runtime_anchor(path, label)
}

path_trees_overlap <- function(first, second) {
  path_is_within(first, second) || path_is_within(second, first)
}

path_is_within <- function(root, path) {
  identical(path, root) || startsWith(path, paste0(sub("/$", "", root), "/"))
}

normalize_runtime_source_cache_roots <- function(
  source_cache_roots,
  historical = FALSE
) {
  if (
    !is.character(source_cache_roots) ||
      length(source_cache_roots) == 0L ||
      anyNA(source_cache_roots)
  ) {
    stop(
      "`source_cache_roots` must contain one or more paths.",
      call. = FALSE
    )
  }
  roots <- vapply(
    unname(source_cache_roots),
    if (historical) validate_historical_cache_root else
      normalize_runtime_anchor,
    character(1L),
    argument = "source_cache_roots"
  )
  roots <- sort(unname(roots), method = "radix")
  if (anyDuplicated(roots)) {
    stop("Runtime source-cache roots must be unique.", call. = FALSE)
  }

  roots
}

validate_historical_cache_root <- function(path, argument) {
  path <- validate_contract_text(path, argument)
  components <- strsplit(sub("^/|^[A-Za-z]:/", "", path), "/", fixed = TRUE)[[
    1L
  ]]
  if (
    !runtime_path_is_absolute(path) ||
      grepl("\\\\", path) ||
      any(!nzchar(components) | components %in% c(".", "..")) ||
      (nchar(path) > 1L && endsWith(path, "/"))
  ) {
    stop(
      "Historical cache roots must be normalized absolute paths.",
      call. = FALSE
    )
  }
  path
}

validate_runtime_package_root <- function(package_root) {
  description <- file.path(package_root, "DESCRIPTION")
  if (!file.exists(description) || dir.exists(description)) {
    stop("`package_root` must identify an R package checkout.", call. = FALSE)
  }

  invisible(package_root)
}

validate_runtime_anchor_boundaries <- function(
  package_root,
  data_root,
  runs_root,
  source_cache_roots
) {
  anchors <- c(
    package_root = package_root,
    data_root = data_root,
    runs_root = runs_root,
    stats::setNames(
      source_cache_roots,
      sprintf("source_cache_roots[%d]", seq_along(source_cache_roots))
    )
  )
  if (length(anchors) < 2L) {
    return(invisible(NULL))
  }

  for (first in seq_len(length(anchors) - 1L)) {
    for (second in seq.int(first + 1L, length(anchors))) {
      overlaps <- path_trees_overlap(
        anchors[[first]],
        anchors[[second]]
      )
      allowed <- runtime_managed_source_overlap(
        names(anchors)[[first]],
        anchors[[first]],
        names(anchors)[[second]],
        anchors[[second]],
        data_root
      )
      if (overlaps && !allowed) {
        stop(
          sprintf(
            "Runtime anchor trees `%s` and `%s` must not overlap.",
            names(anchors)[[first]],
            names(anchors)[[second]]
          ),
          call. = FALSE
        )
      }
    }
  }

  invisible(NULL)
}

runtime_managed_source_overlap <- function(
  first_name,
  first_path,
  second_name,
  second_path,
  data_root
) {
  first_is_cache <- startsWith(first_name, "source_cache_roots[")
  second_is_cache <- startsWith(second_name, "source_cache_roots[")
  data_cache_pair <-
    identical(first_name, "data_root") &&
    second_is_cache ||
    identical(second_name, "data_root") && first_is_cache
  if (!data_cache_pair) {
    return(FALSE)
  }

  cache_path <- if (first_is_cache) first_path else second_path
  managed_roots <- file.path(data_root, c("binary-cache", "source-cache"))
  any(vapply(
    managed_roots,
    function(root)
      !identical(cache_path, root) && path_is_within(root, cache_path),
    logical(1L)
  ))
}

runtime_root_path_table <- function(
  package_root,
  data_root,
  runs_root,
  run_id,
  source_cache_roots
) {
  source_count <- length(source_cache_roots)
  data.frame(
    role = c(
      "package-checkout",
      sprintf("source-cache-%06d", seq_len(source_count)),
      "source-cache",
      "binary-cache",
      "run"
    ),
    path = c(
      package_root,
      source_cache_roots,
      file.path(data_root, "source-cache"),
      file.path(data_root, "binary-cache"),
      file.path(runs_root, run_id)
    ),
    stringsAsFactors = FALSE
  )
}

validate_runtime_path_table <- function(paths) {
  fields <- c("role", "path")
  if (
    !is.data.frame(paths) ||
      !identical(names(paths), fields) ||
      !all(vapply(paths, is.character, logical(1L))) ||
      nrow(paths) < 5L ||
      anyNA(paths) ||
      anyDuplicated(paths$role) ||
      anyDuplicated(paths$path)
  ) {
    stop("Runtime root path table has an invalid structure.", call. = FALSE)
  }

  invisible(paths)
}

validate_runtime_derived_paths <- function(paths, data_root, runs_root) {
  validate_runtime_path_table(paths)
  for (role in c("source-cache", "binary-cache")) {
    validate_runtime_derived_path(
      paths$path[paths$role == role],
      data_root,
      role
    )
  }
  validate_runtime_derived_path(
    paths$path[paths$role == "run"],
    runs_root,
    "run"
  )

  invisible(paths)
}

validate_runtime_derived_path <- function(path, anchor, role) {
  if (
    length(path) != 1L ||
      !runtime_path_is_absolute(path) ||
      identical(path, anchor) ||
      !path_is_within(anchor, path)
  ) {
    stop(
      sprintf("Runtime `%s` path must remain below its anchor.", role),
      call. = FALSE
    )
  }

  relative <- substring(path, nchar(sub("/$", "", anchor)) + 2L)
  components <- strsplit(relative, "/", fixed = TRUE)[[1L]]
  current <- anchor
  for (component in components) {
    current <- file.path(current, component)
    link_target <- Sys.readlink(current)
    if (
      length(link_target) == 1L &&
        !is.na(link_target) &&
        nzchar(link_target)
    ) {
      stop(
        sprintf("Runtime `%s` path must not traverse a symbolic link.", role),
        call. = FALSE
      )
    }
    if (file.exists(current)) {
      if (!dir.exists(current)) {
        stop(
          sprintf("Runtime `%s` path must identify a directory.", role),
          call. = FALSE
        )
      }
      resolved <- normalizePath(current, winslash = "/", mustWork = TRUE)
      if (!path_is_within(anchor, resolved)) {
        stop(
          sprintf("Runtime `%s` path escapes its anchor.", role),
          call. = FALSE
        )
      }
    }
  }

  invisible(path)
}

runtime_root_plan_identity_fields <- function(
  run_id,
  package_root,
  data_root,
  runs_root,
  source_cache_roots,
  paths
) {
  c(
    run_id = run_id,
    package_root = package_root,
    data_root = data_root,
    runs_root = runs_root,
    indexed_vector_identity_fields(
      "source_cache_root",
      source_cache_roots,
      include_names = FALSE
    ),
    tabular_identity_fields("path", paths)
  )
}
