# This file composes private helpers defined in other package source files.
# nolint start: object_usage_linter.

#' Plan a reverse-dependency check
#'
#' Discover reverse dependencies and estimate the preparation work without
#' downloading, building, or checking packages. Direct reverse dependencies
#' are always selected. Recursive-strong-only targets are opt-in and can be
#' limited with a reproducible sample.
#'
#' @param package Path to the development package checkout.
#' @param recursive Include recursive strong reverse dependencies as candidates.
#' @param max_recursive Optional maximum number of recursive-only targets to
#'   select. All direct targets remain selected.
#' @param sample_seed Optional non-negative integer used to choose a different
#'   reproducible recursive sample. It can be supplied only with
#'   `max_recursive`.
#' @param cache Cache directories to inspect for compatible binaries. `NULL`
#'   uses the ordinary `crancache` directory when it exists; `character()`
#'   disables cache inspection.
#' @param repos Named source repository base URLs. The default uses
#'   `getOption("repos")`; an unset CRAN mirror uses
#'   `https://cloud.r-project.org`.
#'
#' @return A `revdep_plan` list with `summary`, `targets`, `requirements`,
#'   `unavailable`, and `repository_alternates` tables. Per-target requirement
#'   counts overlap; use `summary` for totals over the unique selected
#'   requirement set. Declared system requirements are metadata clues, not a
#'   platform-readiness check.
#'
#' @examples
#' \dontrun{
#' plan <- revdep_plan("/path/to/package")
#' plan$summary
#'
#' sampled <- revdep_plan(
#'   "/path/to/package",
#'   recursive = TRUE,
#'   max_recursive = 20
#' )
#' sampled$targets
#' }
#' @export
revdep_plan <- function(
  package,
  recursive = FALSE,
  max_recursive = NULL,
  sample_seed = NULL,
  cache = NULL,
  repos = getOption("repos")
) {
  package_root <- normalize_runtime_anchor(package, "package")
  package_description <- revdep_plan_description(package_root)
  settings <- revdep_plan_settings(recursive, max_recursive, sample_seed)
  repositories <- revdep_plan_repositories(repos)

  database <- revdep_plan_package_database(repositories$bases)
  canonical <- revdep_plan_canonical_rows(database, repositories$contrib)
  enriched <- revdep_plan_system_requirements(
    canonical$database,
    repositories$contrib
  )
  snapshot <- new_repository_snapshot(repositories$contrib, enriched$database)
  package_name <- package_description[["Package"]]
  baseline <- revdep_plan_baseline(package_name, snapshot)
  cohort <- new_reverse_dependency_cohort(package_name, snapshot)

  targets <- revdep_plan_targets(cohort, snapshot, settings)
  selected_targets <- cohort$targets[
    match(targets$package[targets$selected], cohort$targets$package),
    ,
    drop = FALSE
  ]
  discovered <- discover_dependency_universe(
    selected_targets,
    snapshot$packages,
    package_name,
    rownames(utils::installed.packages(priority = "base"))
  )
  cache_roots <- revdep_plan_cache_roots(cache)
  requirements <- revdep_plan_requirements(
    discovered,
    snapshot,
    cache_roots
  )
  unavailable <- revdep_plan_unavailable(discovered)
  targets <- revdep_plan_target_burden(targets, discovered, requirements)
  summary <- revdep_plan_summary(
    package_description,
    baseline,
    snapshot,
    settings,
    targets,
    requirements,
    unavailable,
    canonical$alternates,
    enriched$status,
    cache_roots
  )

  plan <- structure(
    list(
      summary = summary,
      targets = targets,
      requirements = requirements,
      unavailable = unavailable,
      repository_alternates = canonical$alternates
    ),
    class = "revdep_plan"
  )
  attr(plan, "package_root") <- package_root
  attr(plan, "snapshot") <- snapshot
  attr(plan, "cohort") <- cohort
  attr(plan, "selected_targets") <- selected_targets
  attr(plan, "discovered") <- discovered
  attr(plan, "cache_roots") <- cache_roots
  plan
}

#' @export
print.revdep_plan <- function(x, ...) {
  summary <- x$summary[1L, , drop = FALSE]
  cat(sprintf(
    "Reverse-dependency plan for %s %s (repository baseline %s)\n",
    summary$package,
    summary$development_version,
    summary$baseline_version
  ))
  cat(sprintf(
    "Targets: %d selected (%d direct; %d recursive-only candidates)\n",
    summary$selected_targets,
    summary$direct_targets,
    summary$recursive_only_targets
  ))
  cat(sprintf(
    "Preparation: %d requirements; %d reusable; %d source builds (%d native)\n",
    summary$preparation_requirements,
    summary$reusable_binaries,
    summary$source_builds,
    summary$native_source_builds
  ))
  if (summary$unavailable_references > 0L) {
    cat(sprintf(
      "Unavailable repository references: %d\n",
      summary$unavailable_references
    ))
  }
  invisible(x)
}

revdep_plan_description <- function(package_root) {
  description <- tryCatch(
    read.dcf(
      file.path(package_root, "DESCRIPTION"),
      fields = c(
        "Package",
        "Version"
      )
    ),
    error = function(error) {
      stop("Unable to read the package DESCRIPTION.", call. = FALSE)
    }
  )
  if (
    nrow(description) != 1L ||
      anyNA(description) ||
      any(!nzchar(description))
  ) {
    stop(
      "The package DESCRIPTION must contain Package and Version.",
      call. = FALSE
    )
  }
  c(
    Package = validate_package_name(description[[1L, "Package"]]),
    Version = validate_package_version(description[[1L, "Version"]])
  )
}

revdep_plan_settings <- function(recursive, max_recursive, sample_seed) {
  if (!is.logical(recursive) || length(recursive) != 1L || is.na(recursive)) {
    stop("`recursive` must be `TRUE` or `FALSE`.", call. = FALSE)
  }
  max_recursive <- revdep_plan_integer(max_recursive, "max_recursive")
  sample_seed <- revdep_plan_integer(sample_seed, "sample_seed")
  if (!recursive && !is.null(max_recursive)) {
    stop("`max_recursive` requires `recursive = TRUE`.", call. = FALSE)
  }
  if (is.null(max_recursive) && !is.null(sample_seed)) {
    stop("`sample_seed` requires `max_recursive`.", call. = FALSE)
  }
  list(
    recursive = recursive,
    max_recursive = max_recursive,
    sample_seed = sample_seed
  )
}

revdep_plan_integer <- function(value, argument) {
  if (is.null(value)) {
    return(NULL)
  }
  if (
    !is.numeric(value) ||
      is.complex(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !is.finite(value) ||
      value < 0 ||
      value != floor(value) ||
      value > .Machine$integer.max
  ) {
    stop(
      sprintf("`%s` must be one non-negative integer or `NULL`.", argument),
      call. = FALSE
    )
  }
  as.integer(value)
}

revdep_plan_repositories <- function(repos) {
  if (
    !is.character(repos) ||
      length(repos) == 0L ||
      is.null(names(repos)) ||
      anyNA(repos) ||
      anyNA(names(repos)) ||
      any(!nzchar(repos)) ||
      any(!nzchar(names(repos))) ||
      anyDuplicated(names(repos)) ||
      anyDuplicated(repos)
  ) {
    stop(
      "`repos` must be a named set of unique repository URLs.",
      call. = FALSE
    )
  }
  repos[repos == "@CRAN@"] <- "https://cloud.r-project.org"
  bases <- stats::setNames(
    vapply(unname(repos), validate_contract_text, character(1L), "repos"),
    names(repos)
  )
  contrib <- stats::setNames(
    utils::contrib.url(unname(bases), type = "source"),
    names(bases)
  )
  list(bases = bases, contrib = contrib)
}

revdep_plan_package_database <- function(repos) {
  utils::available.packages(repos = repos, filters = list(), type = "source")
}

revdep_plan_cran_database <- function() {
  suppressWarnings(tryCatch(
    tools::CRAN_package_db(),
    error = function(error) NULL
  ))
}

revdep_plan_canonical_rows <- function(database, repositories) {
  empty <- data.frame(
    package = character(),
    repository = character(),
    selected_version = character(),
    discarded_version = character(),
    discarded_repository = character(),
    stringsAsFactors = FALSE
  )
  if (
    (!is.matrix(database) && !is.data.frame(database)) ||
      !all(c("Package", "Version", "Repository") %in% colnames(database))
  ) {
    return(list(database = database, alternates = empty))
  }
  packages <- as.data.frame(database, stringsAsFactors = FALSE, optional = TRUE)
  priority <- vapply(
    packages$Repository,
    snapshot_repository_priority,
    integer(1L),
    repositories = repositories
  )
  groups <- split(
    seq_len(nrow(packages)),
    paste(priority, packages$Package, sep = "\r")
  )
  keep <- rep(TRUE, nrow(packages))
  alternates <- list()
  for (rows in unname(groups)) {
    if (length(rows) < 2L) {
      next
    }
    canonical <- rows[
      packages$Repository[rows] == repositories[[priority[[rows[[1L]]]]]]
    ]
    if (length(canonical) != 1L) {
      next
    }
    discarded <- setdiff(rows, canonical)
    keep[discarded] <- FALSE
    alternates[[length(alternates) + 1L]] <- data.frame(
      package = packages$Package[discarded],
      repository = names(repositories)[priority[discarded]],
      selected_version = packages$Version[canonical],
      discarded_version = packages$Version[discarded],
      discarded_repository = packages$Repository[discarded],
      stringsAsFactors = FALSE
    )
  }
  alternates <- if (length(alternates) == 0L) {
    empty
  } else {
    result <- do.call(rbind, alternates)
    result <- result[
      order(result$repository, result$package, method = "radix"),
      ,
      drop = FALSE
    ]
    rownames(result) <- NULL
    result
  }
  list(database = packages[keep, , drop = FALSE], alternates = alternates)
}

revdep_plan_system_requirements <- function(database, repositories) {
  packages <- as.data.frame(database, stringsAsFactors = FALSE, optional = TRUE)
  metadata <- revdep_plan_cran_database()
  packages$SystemRequirements <- NA_character_
  packages$SystemRequirementsStatus <- "unknown"
  if (
    is.null(metadata) ||
      !is.data.frame(metadata) ||
      !all(c("Package", "Version") %in% names(metadata))
  ) {
    return(list(database = packages, status = "unavailable"))
  }
  metadata <- as.data.frame(metadata, stringsAsFactors = FALSE, optional = TRUE)
  if (!"SystemRequirements" %in% names(metadata)) {
    metadata$SystemRequirements <- NA_character_
  }
  package_keys <- package_version_key(packages$Package, packages$Version)
  metadata_keys <- package_version_key(metadata$Package, metadata$Version)
  matched <- match(package_keys, metadata_keys)
  repository_names <- names(repositories)[vapply(
    packages$Repository,
    snapshot_repository_priority,
    integer(1L),
    repositories = repositories
  )]
  known <- !is.na(matched) & tolower(repository_names) == "cran"
  system_requirements <- trimws(as.character(
    metadata$SystemRequirements[matched]
  ))
  system_requirements[is.na(system_requirements)] <- NA_character_
  system_requirements[
    !is.na(system_requirements) & !nzchar(system_requirements)
  ] <- NA_character_
  packages$SystemRequirements[known] <- system_requirements[known]
  declared <- known &
    !is.na(packages$SystemRequirements) &
    nzchar(packages$SystemRequirements)
  packages$SystemRequirementsStatus[known] <- "none"
  packages$SystemRequirementsStatus[declared] <- "declared"
  list(database = packages, status = "available")
}

revdep_plan_baseline <- function(package, snapshot) {
  packages <- snapshot$packages[!duplicated(snapshot$packages$Package), ]
  index <- match(package, packages$Package)
  if (is.na(index)) {
    stop(
      sprintf(
        "Package `%s` is absent from the configured repositories.",
        package
      ),
      call. = FALSE
    )
  }
  packages[index, , drop = FALSE]
}

revdep_plan_targets <- function(cohort, snapshot, settings) {
  targets <- cohort$targets
  package_rows <- snapshot$packages[!duplicated(snapshot$packages$Package), ]
  package_rows <- package_rows[match(targets$package, package_rows$Package), ]
  direct <- targets$role == "direct"
  relationships <- rep(NA_character_, nrow(targets))
  relationships[direct] <- vapply(
    which(direct),
    function(index) {
      fields <- stock_runner_first_level_fields()
      present <- vapply(
        fields,
        function(field) {
          cohort$package %in%
            parse_stock_dependency_field(
              package_rows[[field]][[index]],
              field
            )
        },
        logical(1L)
      )
      paste(fields[present], collapse = ", ")
    },
    character(1L)
  )
  roots <- revdep_plan_roots(cohort, snapshot)
  sample <- revdep_plan_sample(cohort, snapshot$snapshot_id, settings)
  result <- data.frame(
    package = targets$package,
    version = targets$version,
    role = targets$role,
    relationship = relationships,
    direct_root_count = as.integer(lengths(roots)),
    direct_roots = vapply(roots, revdep_plan_root_display, character(1L)),
    selected = sample$selected,
    selection_reason = sample$reason,
    sample_rank = sample$rank,
    needs_compilation = vapply(
      seq_len(nrow(package_rows)),
      function(index) source_acquisition_compilation(package_rows[index, ]),
      character(1L)
    ),
    preparation_requirements = rep(NA_integer_, nrow(targets)),
    native_requirements = rep(NA_integer_, nrow(targets)),
    stringsAsFactors = FALSE
  )
  recursive_roots <- roots[targets$role == "recursive-strong-only"]
  root_counts <- table(unlist(recursive_roots))
  if (length(root_counts) > 0L) {
    root_counts <- root_counts[order(
      -as.integer(root_counts),
      names(root_counts),
      method = "radix"
    )]
  }
  attr(result, "root_counts") <- root_counts
  attr(result, "sample_key") <- sample$key
  result
}

revdep_plan_roots <- function(cohort, snapshot) {
  direct <- cohort$targets$package[cohort$targets$role == "direct"]
  recursive <- cohort$targets$package[
    cohort$targets$role == "recursive-strong-only"
  ]
  roots <- stats::setNames(as.list(direct), direct)
  if (length(recursive) == 0L) {
    return(unname(roots[cohort$targets$package]))
  }
  reachable <- tools::package_dependencies(
    direct,
    db = as.matrix(
      snapshot$packages[!duplicated(snapshot$packages$Package), ]
    ),
    which = "strong",
    recursive = "strong",
    reverse = TRUE
  )
  recursive_roots <- lapply(recursive, function(package) {
    selected <- direct[vapply(
      reachable,
      function(packages) package %in% packages,
      logical(1L)
    )]
    if (length(selected) == 0L) {
      stop("A recursive target has no direct-root provenance.", call. = FALSE)
    }
    sort(selected, method = "radix")
  })
  roots[recursive] <- recursive_roots
  unname(roots[cohort$targets$package])
}

revdep_plan_root_display <- function(roots, limit = 8L) {
  if (length(roots) <= limit) {
    return(paste(roots, collapse = ", "))
  }
  paste0(
    paste(roots[seq_len(limit)], collapse = ", "),
    sprintf(", ... (+%d more)", length(roots) - limit)
  )
}

revdep_plan_sample <- function(cohort, snapshot_id, settings) {
  targets <- cohort$targets
  recursive <- which(targets$role == "recursive-strong-only")
  seed <- if (is.null(settings$sample_seed)) "default" else
    as.character(settings$sample_seed)
  key <- record_identity(
    "revdeprunner-recursive-sample-key/v1",
    c(snapshot_id = snapshot_id, package = cohort$package, seed = seed)
  )
  scores <- vapply(
    targets$package[recursive],
    function(package) {
      digest::digest(
        charToRaw(paste(key, package, sep = "\n")),
        algo = "sha256",
        serialize = FALSE
      )
    },
    character(1L)
  )
  order_index <- order(scores, targets$package[recursive], method = "radix")
  ranks <- integer(length(recursive))
  ranks[order_index] <- seq_along(order_index)
  selected <- targets$role == "direct"
  reason <- ifelse(selected, "direct", "recursive-not-requested")
  if (settings$recursive) {
    limit <- if (is.null(settings$max_recursive)) {
      length(recursive)
    } else {
      min(settings$max_recursive, length(recursive))
    }
    chosen <- recursive[ranks <= limit]
    selected[chosen] <- TRUE
    reason[recursive] <- "outside-recursive-bound"
    reason[chosen] <- "recursive-selected"
  }
  list(
    selected = selected,
    reason = reason,
    rank = replace(rep(NA_integer_, nrow(targets)), recursive, ranks),
    key = key
  )
}

revdep_plan_cache_roots <- function(cache) {
  if (is.null(cache)) {
    cache <- character()
    if (requireNamespace("crancache", quietly = TRUE)) {
      candidate <- tryCatch(
        get("get_cache_dir", asNamespace("crancache"))(),
        error = function(error) NA_character_
      )
      if (!is.na(candidate) && dir.exists(candidate)) {
        cache <- candidate
      }
    }
  }
  if (!is.character(cache) || anyNA(cache) || any(!nzchar(cache))) {
    stop(
      "`cache` must contain directory paths, `character()`, or `NULL`.",
      call. = FALSE
    )
  }
  if (length(cache) == 0L) {
    return(character())
  }
  roots <- vapply(cache, normalize_cache_root, character(1L))
  if (anyDuplicated(roots)) {
    stop("`cache` paths must be unique.", call. = FALSE)
  }
  unname(roots)
}

revdep_plan_cache_artifacts <- function(cache_roots, requested) {
  rows <- lapply(cache_roots, function(cache_root) {
    paths <- walk_cache_files(cache_root)
    paths <- paths[is_package_archive(paths)]
    if (length(paths) == 0L) {
      return(NULL)
    }
    fields <- lapply(basename(paths), archive_filename_fields)
    package <- vapply(fields, `[[`, character(1L), "package")
    version <- vapply(fields, `[[`, character(1L), "version")
    candidates <- package_version_key(package, version) %in% requested
    paths <- paths[candidates]
    if (length(paths) == 0L) {
      return(NULL)
    }
    observe_artifacts(cache_root, paths, cache_relative_path(cache_root, paths))
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) empty_artifact_observations() else
    do.call(rbind, rows)
}

revdep_plan_requirements <- function(discovered, snapshot, cache_roots) {
  install <- discovered$dependencies$disposition == "install"
  dependencies <- discovered$dependencies[install, , drop = FALSE]
  if (nrow(dependencies) == 0L) {
    return(data.frame(
      package = character(),
      version = character(),
      required_by = integer(),
      needs_compilation = character(),
      system_requirements_status = character(),
      system_requirements = character(),
      action = character(),
      cache_source = character(),
      stringsAsFactors = FALSE
    ))
  }
  requirements <- unique(dependencies[c("dependency", "version")])
  names(requirements)[[1L]] <- "package"
  requirements <- requirements[order(requirements$package, method = "radix"), ]
  required_by <- table(dependencies$dependency)
  requirements$required_by <- as.integer(required_by[requirements$package])
  packages <- snapshot$packages[!duplicated(snapshot$packages$Package), ]
  packages <- packages[match(requirements$package, packages$Package), ]
  requirements$needs_compilation <- vapply(
    seq_len(nrow(packages)),
    function(index) source_acquisition_compilation(packages[index, ]),
    character(1L)
  )
  requirements$system_requirements_status <- packages$SystemRequirementsStatus
  requirements$system_requirements <- packages$SystemRequirements

  keys <- package_version_key(requirements$package, requirements$version)
  artifacts <- revdep_plan_cache_artifacts(cache_roots, keys)
  built <- built_r_major_minor(artifacts$built)
  compatible <- artifacts$status == "ok" &
    artifacts$archive_type == "binary" &
    !is.na(built) &
    built == revdep_plan_r_major_minor() &
    !is.na(artifacts$platform) &
    artifacts$platform == R.version$platform
  artifacts <- artifacts[compatible, , drop = FALSE]
  artifact_keys <- package_version_key(artifacts$package, artifacts$version)
  hit <- match(keys, artifact_keys)
  requirements$action <- ifelse(is.na(hit), "download-build", "reuse")
  requirements$cache_source <- NA_character_
  requirements$cache_source[!is.na(hit)] <- artifacts$cache_root[hit[
    !is.na(hit)
  ]]
  rownames(requirements) <- NULL
  requirements
}

revdep_plan_r_major_minor <- function() {
  sub("^([0-9]+[.][0-9]+).*$", "\\1", as.character(getRversion()))
}

revdep_plan_unavailable <- function(discovered) {
  unavailable <- discovered$dependencies[
    discovered$dependencies$disposition == "unavailable",
    c("target", "dependency"),
    drop = FALSE
  ]
  if (nrow(unavailable) == 0L) {
    return(data.frame(
      dependency = character(),
      relationship = character(),
      affected_targets = character(),
      stringsAsFactors = FALSE
    ))
  }
  edges <- merge(
    unavailable,
    discovered$edges,
    by.x = c("target", "dependency"),
    by.y = c("target", "dependency"),
    all.x = TRUE,
    sort = FALSE
  )
  groups <- split(
    seq_len(nrow(edges)),
    paste(edges$dependency, edges$relationship, sep = "\r")
  )
  rows <- lapply(unname(groups), function(index) {
    data.frame(
      dependency = edges$dependency[index[[1L]]],
      relationship = edges$relationship[index[[1L]]],
      affected_targets = paste(
        sort(unique(edges$target[index]), method = "radix"),
        collapse = ", "
      ),
      stringsAsFactors = FALSE
    )
  })
  result <- do.call(rbind, rows)
  result <- result[
    order(result$dependency, result$relationship, method = "radix"),
    ,
    drop = FALSE
  ]
  rownames(result) <- NULL
  result
}

revdep_plan_target_burden <- function(targets, discovered, requirements) {
  install <- discovered$dependencies[
    discovered$dependencies$disposition == "install",
    c("target", "dependency"),
    drop = FALSE
  ]
  compiled <- requirements$package[requirements$needs_compilation == "yes"]
  for (index in which(targets$selected)) {
    dependencies <- unique(install$dependency[
      install$target == targets$package[[index]]
    ])
    targets$preparation_requirements[[index]] <- length(dependencies)
    targets$native_requirements[[index]] <- sum(dependencies %in% compiled)
  }
  targets
}

revdep_plan_summary <- function(
  package_description,
  baseline,
  snapshot,
  settings,
  targets,
  requirements,
  unavailable,
  alternates,
  metadata_status,
  cache_roots
) {
  selected <- targets$selected
  recursive <- targets$role == "recursive-strong-only"
  root_table <- attr(targets, "root_counts")
  sample_key <- attr(targets, "sample_key")
  data.frame(
    package = package_description[["Package"]],
    development_version = package_description[["Version"]],
    baseline_version = baseline$Version[[1L]],
    snapshot_id = snapshot$snapshot_id,
    recursive = settings$recursive,
    max_recursive = if (is.null(settings$max_recursive)) NA_integer_ else
      settings$max_recursive,
    sample_seed = if (is.null(settings$sample_seed)) NA_integer_ else
      settings$sample_seed,
    sample_key = sample_key,
    direct_targets = sum(targets$role == "direct"),
    recursive_only_targets = sum(recursive),
    selected_targets = sum(selected),
    compiled_targets = sum(
      selected & targets$needs_compilation == "yes"
    ),
    preparation_requirements = nrow(requirements),
    reusable_binaries = sum(requirements$action == "reuse"),
    source_builds = sum(requirements$action == "download-build"),
    native_source_builds = sum(
      requirements$action == "download-build" &
        requirements$needs_compilation == "yes"
    ),
    declared_system_requirements = sum(
      requirements$system_requirements_status == "declared"
    ),
    unknown_compilation_metadata = sum(
      requirements$needs_compilation == "unknown"
    ),
    unknown_system_requirements = sum(
      requirements$system_requirements_status == "unknown"
    ),
    unavailable_references = length(unique(unavailable$dependency)),
    largest_recursive_root = if (length(root_table) == 0L) NA_character_ else
      names(root_table)[[1L]],
    largest_recursive_root_targets = if (length(root_table) == 0L) 0L else
      as.integer(root_table[[1L]]),
    repository_alternates = nrow(alternates),
    cran_metadata = metadata_status,
    cache_roots = length(cache_roots),
    stringsAsFactors = FALSE
  )
}

# nolint end
