dependency_universe_schema_version <- function() {
  "revdeprunner-dependency-universe/v1"
}

stock_runner_first_level_fields <- function() {
  c("Depends", "Imports", "LinkingTo", "Suggests")
}

stock_runner_recursive_fields <- function() {
  c("Depends", "Imports", "LinkingTo")
}

dependency_universe_policies <- function() {
  c("direct", "recursive-strong")
}

dependency_dispositions <- function() {
  c(
    "install",
    "unavailable",
    "runner-supplied",
    "base",
    "target-supplied"
  )
}

new_dependency_universe <- function(
  cohort,
  snapshot,
  cohort_policy,
  base_packages
) {
  validate_repository_snapshot(snapshot) # nolint: object_usage_linter.
  validate_reverse_dependency_cohort(cohort, snapshot) # nolint: object_usage_linter.
  cohort_policy <- validate_dependency_universe_policy(cohort_policy)
  base_packages <- normalize_dependency_base_packages(base_packages)
  targets <- select_dependency_universe_targets(cohort, cohort_policy)
  discovered <- discover_dependency_universe(
    targets,
    snapshot$packages,
    cohort$package,
    base_packages
  )
  schema_version <- dependency_universe_schema_version()
  first_level_fields <- stock_runner_first_level_fields()
  recursive_fields <- stock_runner_recursive_fields()
  fields <- dependency_universe_identity_fields(
    snapshot$snapshot_id,
    cohort$cohort_id,
    cohort_policy,
    first_level_fields,
    recursive_fields,
    cohort$package,
    base_packages,
    targets,
    discovered$dependencies,
    discovered$edges
  )

  universe <- structure(
    list(
      schema_version = schema_version,
      universe_id = record_identity(schema_version, fields), # nolint: object_usage_linter.
      snapshot_id = snapshot$snapshot_id,
      cohort_id = cohort$cohort_id,
      cohort_policy = cohort_policy,
      first_level_fields = first_level_fields,
      recursive_fields = recursive_fields,
      runner_supplied = cohort$package,
      base_packages = base_packages,
      targets = targets,
      dependencies = discovered$dependencies,
      edges = discovered$edges
    ),
    class = "revdeprunner_dependency_universe"
  )
  validate_dependency_universe(universe, cohort, snapshot)
  universe
}

validate_dependency_universe <- function(universe, cohort, snapshot) {
  # nolint start: object_usage_linter.
  validate_composite_contract_record(
    universe,
    c(
      "schema_version",
      "universe_id",
      "snapshot_id",
      "cohort_id",
      "cohort_policy",
      "first_level_fields",
      "recursive_fields",
      "runner_supplied",
      "base_packages",
      "targets",
      "dependencies",
      "edges"
    ),
    "revdeprunner_dependency_universe",
    "dependency universe"
  )
  # nolint end
  if (
    !identical(
      universe$schema_version,
      dependency_universe_schema_version()
    )
  ) {
    stop("Dependency universe schema version is unsupported.", call. = FALSE)
  }

  validate_sha256_identity(universe$universe_id, "universe_id") # nolint: object_usage_linter.
  validate_sha256_identity(universe$snapshot_id, "snapshot_id") # nolint: object_usage_linter.
  validate_sha256_identity(universe$cohort_id, "cohort_id") # nolint: object_usage_linter.
  validate_repository_snapshot(snapshot) # nolint: object_usage_linter.
  validate_reverse_dependency_cohort(cohort, snapshot) # nolint: object_usage_linter.
  if (!identical(universe$snapshot_id, snapshot$snapshot_id)) {
    stop("Dependency universe does not belong to this snapshot.", call. = FALSE)
  }
  if (!identical(universe$cohort_id, cohort$cohort_id)) {
    stop("Dependency universe does not belong to this cohort.", call. = FALSE)
  }

  cohort_policy <- validate_dependency_universe_policy(
    universe$cohort_policy
  )
  if (
    !identical(
      universe$first_level_fields,
      stock_runner_first_level_fields()
    )
  ) {
    stop("First-level dependency fields are unsupported.", call. = FALSE)
  }
  if (
    !identical(
      universe$recursive_fields,
      stock_runner_recursive_fields()
    )
  ) {
    stop("Recursive dependency fields are unsupported.", call. = FALSE)
  }
  if (!identical(universe$runner_supplied, cohort$package)) {
    stop("Runner-supplied package does not match the cohort.", call. = FALSE)
  }

  base_packages <- normalize_dependency_base_packages(universe$base_packages)
  if (!identical(universe$base_packages, base_packages)) {
    stop("Dependency universe base packages are not normalized.", call. = FALSE)
  }
  targets <- select_dependency_universe_targets(cohort, cohort_policy)
  if (!identical(universe$targets, targets)) {
    stop("Dependency universe targets do not match its policy.", call. = FALSE)
  }
  discovered <- discover_dependency_universe(
    targets,
    snapshot$packages,
    cohort$package,
    base_packages
  )
  if (!identical(universe$dependencies, discovered$dependencies)) {
    stop(
      "Dependency universe package dispositions do not match.",
      call. = FALSE
    )
  }
  if (!identical(universe$edges, discovered$edges)) {
    stop("Dependency universe edges do not match.", call. = FALSE)
  }

  fields <- dependency_universe_identity_fields(
    universe$snapshot_id,
    universe$cohort_id,
    universe$cohort_policy,
    universe$first_level_fields,
    universe$recursive_fields,
    universe$runner_supplied,
    universe$base_packages,
    universe$targets,
    universe$dependencies,
    universe$edges
  )
  expected <- record_identity(universe$schema_version, fields) # nolint: object_usage_linter.
  if (!identical(universe$universe_id, expected)) {
    stop(
      "Dependency universe identity does not match its fields.",
      call. = FALSE
    )
  }

  invisible(universe)
}

validate_dependency_universe_policy <- function(cohort_policy) {
  cohort_policy <- validate_contract_text(cohort_policy, "cohort_policy") # nolint: object_usage_linter.
  if (!cohort_policy %in% dependency_universe_policies()) {
    stop(
      "`cohort_policy` must be `direct` or `recursive-strong`.",
      call. = FALSE
    )
  }

  cohort_policy
}

normalize_dependency_base_packages <- function(base_packages) {
  if (
    !is.character(base_packages) ||
      length(base_packages) == 0L ||
      anyNA(base_packages) ||
      anyDuplicated(base_packages)
  ) {
    stop(
      "`base_packages` must be a non-empty set of unique package names.",
      call. = FALSE
    )
  }

  base_packages <- vapply(
    base_packages,
    validate_package_name, # nolint: object_usage_linter.
    character(1L)
  )
  sort(unname(base_packages), method = "radix")
}

select_dependency_universe_targets <- function(cohort, cohort_policy) {
  cohort_policy <- validate_dependency_universe_policy(cohort_policy)
  if (identical(cohort_policy, "direct")) {
    targets <- cohort$targets[
      cohort$targets$role == "direct",
      ,
      drop = FALSE
    ]
  } else {
    targets <- cohort$targets
  }
  rownames(targets) <- NULL
  targets
}

discover_dependency_universe <- function(
  targets,
  packages,
  runner_supplied,
  base_packages
) {
  selected_packages <- packages[!duplicated(packages$Package), , drop = FALSE]
  rownames(selected_packages) <- NULL
  edges <- lapply(
    targets$package,
    discover_target_dependency_edges,
    packages = selected_packages,
    base_packages = base_packages
  )
  edges <- normalize_dependency_edges(edges)
  dependencies <- dependency_disposition_table(
    edges,
    selected_packages,
    runner_supplied,
    base_packages
  )
  list(dependencies = dependencies, edges = edges)
}

discover_target_dependency_edges <- function(
  target,
  packages,
  base_packages
) {
  pending <- target
  visited <- character()
  edges <- list()

  while (length(pending) > 0L) {
    from_package <- pending[[1L]]
    pending <- pending[-1L]
    if (from_package %in% visited) {
      next
    }
    visited <- c(visited, from_package)

    package_index <- match(from_package, packages$Package)
    if (is.na(package_index)) {
      next
    }
    fields <- if (identical(from_package, target)) {
      stock_runner_first_level_fields()
    } else {
      stock_runner_recursive_fields()
    }
    for (field in fields) {
      dependencies <- parse_stock_dependency_field(
        packages[[field]][[package_index]],
        field
      )
      for (dependency in dependencies) {
        edges[[length(edges) + 1L]] <- data.frame(
          target = target,
          from_package = from_package,
          dependency = dependency,
          relationship = field,
          stringsAsFactors = FALSE
        )
      }

      expandable <- dependencies[
        !dependencies %in% c("R", base_packages) &
          dependencies %in% packages$Package &
          !dependencies %in% visited
      ]
      pending <- c(pending, expandable)
    }
  }

  edges
}

parse_stock_dependency_field <- function(value, field) {
  if (!is.character(value) || length(value) != 1L) {
    stop(
      "Dependency metadata fields must be scalar character values.",
      call. = FALSE
    )
  }
  if (is.na(value) || !nzchar(value)) {
    return(character())
  }

  entries <- trimws(strsplit(value, ",", fixed = TRUE)[[1L]])
  if (any(!nzchar(entries)) || grepl(",$", value)) {
    stop(
      sprintf("`%s` contains malformed dependency syntax.", field),
      call. = FALSE
    )
  }

  dependencies <- vapply(
    entries,
    parse_stock_dependency_entry,
    character(1L),
    field = field
  )
  unique(unname(dependencies))
}

parse_stock_dependency_entry <- function(entry, field) {
  package_pattern <- "([A-Za-z][A-Za-z0-9.]*[A-Za-z0-9]|R)"
  if (grepl(paste0("^", package_pattern, "$"), entry)) {
    dependency <- entry
  } else {
    constraint_pattern <- paste0(
      "^",
      package_pattern,
      "[[:space:]]*\\((>=|<=|==|!=|>|<)",
      "[[:space:]]+([^[:space:]()]+)\\)$"
    )
    match <- regmatches(
      entry,
      regexec(constraint_pattern, entry)
    )[[1L]]
    if (
      length(match) == 0L ||
        !valid_dependency_version(match[[4L]], match[[2L]])
    ) {
      stop(
        sprintf("`%s` contains malformed dependency syntax.", field),
        call. = FALSE
      )
    }
    dependency <- match[[2L]]
  }

  if (identical(dependency, "R")) {
    return(dependency)
  }
  validate_package_name(dependency) # nolint: object_usage_linter.
}

valid_dependency_version <- function(version, dependency) {
  if (identical(dependency, "R") && grepl("^r[0-9]+$", version)) {
    return(TRUE)
  }
  tryCatch(
    {
      package_version(version)
      TRUE
    },
    error = function(error) FALSE,
    warning = function(warning) FALSE
  )
}

normalize_dependency_edges <- function(edges) {
  if (length(edges) == 0L || all(lengths(edges) == 0L)) {
    return(empty_dependency_edges())
  }
  edges <- do.call(rbind, unlist(edges, recursive = FALSE))
  edge_key <- paste(
    edges$target,
    edges$from_package,
    edges$dependency,
    edges$relationship,
    sep = "\r"
  )
  edges <- edges[!duplicated(edge_key), , drop = FALSE]
  edges <- edges[
    order(
      edges$target,
      edges$from_package,
      edges$dependency,
      edges$relationship,
      method = "radix"
    ),
    ,
    drop = FALSE
  ]
  rownames(edges) <- NULL
  edges
}

dependency_disposition_table <- function(
  edges,
  packages,
  runner_supplied,
  base_packages
) {
  if (nrow(edges) == 0L) {
    return(empty_dependency_dispositions())
  }
  dependencies <- unique(edges[c("target", "dependency")])
  package_index <- match(dependencies$dependency, packages$Package)
  dependencies$disposition <- vapply(
    seq_len(nrow(dependencies)),
    function(index) {
      dependency <- dependencies$dependency[[index]]
      target <- dependencies$target[[index]]
      if (identical(dependency, target)) {
        return("target-supplied")
      }
      if (dependency %in% c("R", base_packages)) {
        return("base")
      }
      if (identical(dependency, runner_supplied)) {
        return("runner-supplied")
      }
      if (is.na(package_index[[index]])) {
        return("unavailable")
      }
      "install"
    },
    character(1L)
  )
  dependencies$version <- NA_character_
  install <- dependencies$disposition == "install"
  dependencies$version[install] <- packages$Version[package_index[install]]
  dependencies <- dependencies[
    order(dependencies$target, dependencies$dependency, method = "radix"),
    c("target", "dependency", "version", "disposition"),
    drop = FALSE
  ]
  rownames(dependencies) <- NULL
  dependencies
}

empty_dependency_edges <- function() {
  data.frame(
    target = character(),
    from_package = character(),
    dependency = character(),
    relationship = character(),
    stringsAsFactors = FALSE
  )
}

empty_dependency_dispositions <- function() {
  data.frame(
    target = character(),
    dependency = character(),
    version = character(),
    disposition = character(),
    stringsAsFactors = FALSE
  )
}

dependency_universe_identity_fields <- function(
  snapshot_id,
  cohort_id,
  cohort_policy,
  first_level_fields,
  recursive_fields,
  runner_supplied,
  base_packages,
  targets,
  dependencies,
  edges
) {
  # nolint start: object_usage_linter.
  c(
    snapshot_id = snapshot_id,
    cohort_id = cohort_id,
    cohort_policy = cohort_policy,
    indexed_vector_identity_fields(
      "first_level_field",
      first_level_fields,
      include_names = FALSE
    ),
    indexed_vector_identity_fields(
      "recursive_field",
      recursive_fields,
      include_names = FALSE
    ),
    runner_supplied = runner_supplied,
    indexed_vector_identity_fields(
      "base_package",
      base_packages,
      include_names = FALSE
    ),
    tabular_identity_fields("target", targets), # nolint: object_usage_linter.
    tabular_identity_fields("dependency", dependencies), # nolint: object_usage_linter.
    tabular_identity_fields("edge", edges) # nolint: object_usage_linter.
  )
  # nolint end
}
