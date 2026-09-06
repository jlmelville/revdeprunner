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
#'   uses the ordinary `crancache` directory and the current runner-managed
#'   binary cache when they exist; `character()` disables cache inspection.
#'   An explicit value replaces default discovery.
#' @param repos Named source repository base URLs. `NULL` combines the
#'   configured repositories with the standard Bioconductor repositories. An
#'   explicit value is used exactly. When a named `CRAN` repository is present,
#'   reverse targets come from CRAN while dependencies can come from every
#'   configured repository.
#'
#' @return A `revdep_plan` list with `summary`, `targets`, `requirements`,
#'   `unavailable`, and `repository_alternates` tables. Per-target requirement
#'   counts overlap; use `summary` for totals over unique selected targets and
#'   their installation dependencies in `requirements`. Declared system
#'   requirements are metadata clues, not a
#'   platform-readiness check. Repository-unavailable `Suggests` remain in
#'   `unavailable`, but do not enter preparation requirements.
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
  repos = NULL
) {
  package_root <- normalize_runtime_anchor(package, "package")
  package_description <- revdep_plan_description(package_root)
  candidate_dependencies <- read_candidate_dependencies(package_root)
  settings <- revdep_plan_settings(recursive, max_recursive, sample_seed)
  repositories <- revdep_plan_repositories(repos)

  database <- revdep_plan_package_database(repositories$bases)
  canonical <- revdep_plan_canonical_rows(database, repositories$contrib)
  enriched <- revdep_plan_system_requirements(
    canonical$database,
    repositories$contrib
  )
  snapshot <- new_repository_snapshot(repositories$contrib, enriched$database)
  validate_candidate_dependency_versions(candidate_dependencies, snapshot)
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
    rownames(utils::installed.packages(priority = "base")),
    snapshot$repositories,
    candidate_dependencies
  )
  cache_roots <- revdep_plan_cache_roots(cache)
  requirements <- revdep_plan_requirements(
    discovered,
    snapshot,
    cache_roots,
    selected_targets
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
  attr(plan, "candidate_dependencies") <- candidate_dependencies
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
  if (is.null(repos)) {
    repos <- revdep_plan_default_repositories()
  }
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

revdep_plan_default_repositories <- function() {
  suppressMessages(BiocManager::repositories())
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

revdep_plan_recommended_repository <- function(repository, canonical) {
  canonical <- sub("/+$", "", canonical)
  prefix <- paste0(canonical, "/")
  if (!startsWith(repository, prefix)) {
    return(FALSE)
  }
  relative <- substring(repository, nchar(prefix) + 1L)
  grepl("^[0-9]+(\\.[0-9]+){1,2}/Recommended/?$", relative)
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
    recommended <- vapply(
      packages$Repository[discarded],
      revdep_plan_recommended_repository,
      logical(1L),
      canonical = repositories[[priority[[rows[[1L]]]]]]
    )
    if (!all(recommended)) {
      next
    }
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
  packages <- reverse_dependency_target_packages(
    snapshot$packages,
    snapshot$repositories
  )
  packages <- packages[!duplicated(packages$Package), , drop = FALSE]
  index <- match(package, packages$Package)
  if (is.na(index)) {
    packages <- snapshot$packages[
      !duplicated(snapshot$packages$Package),
      ,
      drop = FALSE
    ]
    index <- match(package, packages$Package)
  }
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
  package_rows <- reverse_dependency_target_packages(
    snapshot$packages,
    snapshot$repositories
  )
  package_rows <- package_rows[
    !duplicated(package_rows$Package),
    ,
    drop = FALSE
  ]
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
    db = as.matrix(reverse_dependency_target_packages(
      snapshot$packages,
      snapshot$repositories
    )),
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
    cache <- revdep_plan_default_cache_roots()
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

revdep_plan_default_cache_roots <- function() {
  roots <- character()
  if (requireNamespace("crancache", quietly = TRUE)) {
    candidate <- tryCatch(
      get("get_cache_dir", asNamespace("crancache"))(),
      error = function(error) NA_character_
    )
    if (!is.na(candidate) && dir.exists(candidate)) {
      roots <- candidate
    }
  }
  roots <- c(roots, revdep_plan_runner_cache_roots())
  unique(roots)
}

revdep_plan_runner_cache_roots <- function() {
  data_root <- Sys.getenv(
    "REVDEP_RUNNER_DATA",
    unset = tools::R_user_dir("revdeprunner", "data")
  )
  if (!nzchar(data_root)) {
    return(character())
  }
  data_root <- path.expand(data_root)
  candidates <- c(
    runner_source_cache_contrib_path(data_root),
    runner_binary_cache_contrib_path(data_root)
  )
  available <- vapply(
    candidates,
    function(root) dir.exists(root) && file.exists(file.path(root, "PACKAGES")),
    logical(1L)
  )
  vapply(candidates[available], normalize_cache_root, character(1L))
}

revdep_plan_cache_artifacts <- function(cache_roots, requested) {
  rows <- lapply(
    cache_roots,
    observe_cache_artifacts,
    requested = requested
  )
  rows <- Filter(function(rows) nrow(rows) > 0L, rows)
  if (length(rows) == 0L) {
    empty_artifact_observations()
  } else {
    do.call(rbind, rows)
  }
}

revdep_plan_requirements <- function(
  discovered,
  snapshot,
  cache_roots,
  targets
) {
  install <- discovered$dependencies$disposition == "install"
  dependencies <- discovered$dependencies[install, , drop = FALSE]
  requirements <- dependencies[c("dependency", "version")]
  names(requirements)[[1L]] <- "package"
  requirements <- rbind(targets[c("package", "version")], requirements)
  requirements <- requirements[
    !duplicated(requirements$package),
    ,
    drop = FALSE
  ]
  if (nrow(requirements) == 0L) {
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
  requirements <- requirements[order(requirements$package, method = "radix"), ]
  required_by <- table(dependencies$dependency)
  requirements$required_by <- as.integer(required_by[requirements$package])
  requirements$required_by[is.na(requirements$required_by)] <- 0L
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

validate_public_revdep_plan <- function(plan) {
  fields <- c(
    "summary",
    "targets",
    "requirements",
    "unavailable",
    "repository_alternates"
  )
  package_root <- attr(plan, "package_root", exact = TRUE)
  snapshot <- attr(plan, "snapshot", exact = TRUE)
  cohort <- attr(plan, "cohort", exact = TRUE)
  selected <- attr(plan, "selected_targets", exact = TRUE)
  discovered <- attr(plan, "discovered", exact = TRUE)
  cache_roots <- attr(plan, "cache_roots", exact = TRUE)
  candidate_dependencies <- attr(plan, "candidate_dependencies", exact = TRUE)
  if (
    !inherits(plan, "revdep_plan") ||
      !is.list(plan) ||
      !identical(names(plan), fields) ||
      !is.data.frame(plan$summary) ||
      nrow(plan$summary) != 1L ||
      !is.data.frame(plan$targets) ||
      !is.data.frame(plan$requirements) ||
      !is.data.frame(plan$unavailable) ||
      !is.data.frame(plan$repository_alternates) ||
      !is.character(cache_roots)
  ) {
    stop("`package` is not a valid `revdep_plan` object.", call. = FALSE)
  }
  package_root <- normalize_runtime_anchor(package_root, "package")
  if (
    !identical(
      candidate_dependencies,
      read_candidate_dependencies(package_root)
    )
  ) {
    stop(
      "The candidate's dependency requirements have changed since planning. Prepare again before checking.",
      call. = FALSE
    )
  }
  validate_candidate_dependency_versions(candidate_dependencies, snapshot)
  validate_reverse_dependency_cohort(cohort, snapshot)
  selected <- normalize_selected_dependency_targets(selected, cohort)
  selected_rows <- plan$targets$selected
  if (!is.logical(selected_rows) || anyNA(selected_rows)) {
    stop("`package` is not a valid `revdep_plan` object.", call. = FALSE)
  }
  visible_selected <- cohort$targets[
    match(plan$targets$package[selected_rows], cohort$targets$package),
    ,
    drop = FALSE
  ]
  rownames(visible_selected) <- NULL
  expected_discovered <- discover_dependency_universe(
    selected,
    snapshot$packages,
    cohort$package,
    rownames(utils::installed.packages(priority = "base")),
    snapshot$repositories,
    candidate_dependencies
  )
  description <- revdep_plan_description(package_root)
  if (
    !identical(selected, visible_selected) ||
      !identical(discovered, expected_discovered) ||
      !identical(plan$summary$package, description[["Package"]]) ||
      !identical(plan$summary$snapshot_id, snapshot$snapshot_id) ||
      !identical(plan$summary$selected_targets, nrow(selected)) ||
      !identical(attr(plan, "package_root", exact = TRUE), package_root)
  ) {
    stop("`package` is not a valid `revdep_plan` object.", call. = FALSE)
  }
  invisible(plan)
}
