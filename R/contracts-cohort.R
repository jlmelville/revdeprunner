repository_snapshot_schema_version <- function() {
  "revdeprunner-repository-snapshot/v1"
}

reverse_dependency_cohort_schema_version <- function() {
  "revdeprunner-reverse-dependency-cohort/v1"
}

unfiltered_package_database_contract <- function() {
  "available.packages(filters=list())"
}

direct_reverse_dependency_query <- function() {
  c(which = "most", recursive = "false", reverse = "true")
}

recursive_strong_reverse_dependency_query <- function() {
  c(which = "most", recursive = "strong", reverse = "true")
}

new_repository_snapshot <- function(
  repositories,
  package_database,
  filters = list()
) {
  repositories <- normalize_snapshot_repositories(repositories)
  if (!identical(filters, list())) {
    stop("`filters` must be exactly `list()`.", call. = FALSE)
  }
  packages <- normalize_snapshot_packages(package_database, repositories)
  schema_version <- repository_snapshot_schema_version()
  fields <- repository_snapshot_identity_fields(
    unfiltered_package_database_contract(),
    repositories,
    packages
  )

  snapshot <- structure(
    list(
      schema_version = schema_version,
      snapshot_id = record_identity(schema_version, fields),
      filters = unfiltered_package_database_contract(),
      repositories = repositories,
      packages = packages
    ),
    class = "revdeprunner_repository_snapshot"
  )
  validate_repository_snapshot(snapshot)
  snapshot
}

validate_repository_snapshot <- function(snapshot) {
  validate_composite_contract_record(
    snapshot,
    c(
      "schema_version",
      "snapshot_id",
      "filters",
      "repositories",
      "packages"
    ),
    "revdeprunner_repository_snapshot",
    "repository snapshot"
  )
  if (
    !identical(snapshot$schema_version, repository_snapshot_schema_version())
  ) {
    stop("Repository snapshot schema version is unsupported.", call. = FALSE)
  }

  validate_sha256_identity(snapshot$snapshot_id, "snapshot_id")
  if (!identical(snapshot$filters, unfiltered_package_database_contract())) {
    stop(
      "Repository snapshots require the unfiltered package-database policy.",
      call. = FALSE
    )
  }

  repositories <- normalize_snapshot_repositories(snapshot$repositories)
  if (!identical(snapshot$repositories, repositories)) {
    stop("Repository snapshot repositories are not normalized.", call. = FALSE)
  }

  packages <- normalize_snapshot_packages(
    snapshot$packages,
    snapshot$repositories
  )
  if (!identical(snapshot$packages, packages)) {
    stop("Repository snapshot packages are not normalized.", call. = FALSE)
  }

  fields <- repository_snapshot_identity_fields(
    snapshot$filters,
    snapshot$repositories,
    snapshot$packages
  )
  expected <- record_identity(snapshot$schema_version, fields)
  if (!identical(snapshot$snapshot_id, expected)) {
    stop(
      "Repository snapshot identity does not match its fields.",
      call. = FALSE
    )
  }

  invisible(snapshot)
}

new_reverse_dependency_cohort <- function(
  package,
  snapshot,
  target_repos = NULL
) {
  package <- validate_package_name(package)
  validate_repository_snapshot(snapshot)
  target_repos <- normalize_target_repos(target_repos, snapshot$repositories)
  targets <- discover_reverse_dependency_targets(
    package,
    snapshot$packages,
    snapshot$repositories,
    target_repos
  )
  schema_version <- reverse_dependency_cohort_schema_version()
  direct_query <- direct_reverse_dependency_query()
  recursive_query <- recursive_strong_reverse_dependency_query()
  fields <- reverse_dependency_cohort_identity_fields(
    snapshot$snapshot_id,
    package,
    direct_query,
    recursive_query,
    targets,
    target_repos
  )

  cohort <- structure(
    list(
      schema_version = schema_version,
      cohort_id = record_identity(schema_version, fields),
      snapshot_id = snapshot$snapshot_id,
      package = package,
      direct_query = direct_query,
      recursive_strong_query = recursive_query,
      targets = targets
    ),
    class = "revdeprunner_reverse_dependency_cohort"
  )
  cohort$target_repos <- target_repos
  validate_reverse_dependency_cohort(cohort, snapshot)
  cohort
}

validate_reverse_dependency_cohort <- function(cohort, snapshot) {
  validate_composite_contract_record(
    cohort,
    c(
      "schema_version",
      "cohort_id",
      "snapshot_id",
      "package",
      "direct_query",
      "recursive_strong_query",
      "targets",
      if (!is.null(cohort$target_repos)) "target_repos"
    ),
    "revdeprunner_reverse_dependency_cohort",
    "reverse-dependency cohort"
  )
  if (
    !identical(
      cohort$schema_version,
      reverse_dependency_cohort_schema_version()
    )
  ) {
    stop(
      "Reverse-dependency cohort schema version is unsupported.",
      call. = FALSE
    )
  }

  validate_sha256_identity(cohort$cohort_id, "cohort_id")
  validate_sha256_identity(cohort$snapshot_id, "snapshot_id")
  validate_package_name(cohort$package)
  validate_repository_snapshot(snapshot)
  if (!identical(cohort$snapshot_id, snapshot$snapshot_id)) {
    stop(
      "Reverse-dependency cohort does not belong to this snapshot.",
      call. = FALSE
    )
  }
  if (!identical(cohort$direct_query, direct_reverse_dependency_query())) {
    stop(
      "Direct reverse-dependency query contract is unsupported.",
      call. = FALSE
    )
  }
  if (
    !identical(
      cohort$recursive_strong_query,
      recursive_strong_reverse_dependency_query()
    )
  ) {
    stop(
      "Recursive-strong reverse-dependency query contract is unsupported.",
      call. = FALSE
    )
  }

  if (
    !identical(
      cohort$target_repos,
      normalize_target_repos(cohort$target_repos, snapshot$repositories)
    )
  ) {
    stop(
      "Cohort target repositories are not in target priority order.",
      call. = FALSE
    )
  }
  expected_targets <- discover_reverse_dependency_targets(
    cohort$package,
    snapshot$packages,
    snapshot$repositories,
    cohort$target_repos
  )
  if (!identical(cohort$targets, expected_targets)) {
    stop(
      "Reverse-dependency cohort targets do not match the snapshot queries.",
      call. = FALSE
    )
  }

  fields <- reverse_dependency_cohort_identity_fields(
    cohort$snapshot_id,
    cohort$package,
    cohort$direct_query,
    cohort$recursive_strong_query,
    cohort$targets,
    cohort$target_repos
  )
  expected <- record_identity(cohort$schema_version, fields)
  if (!identical(cohort$cohort_id, expected)) {
    stop(
      "Reverse-dependency cohort identity does not match its fields.",
      call. = FALSE
    )
  }

  invisible(cohort)
}

validate_composite_contract_record <- function(
  record,
  fields,
  class_name,
  label
) {
  if (
    !inherits(record, class_name) ||
      !is.list(record) ||
      !identical(names(record), fields)
  ) {
    stop(
      sprintf("The %s record has an invalid structure.", label),
      call. = FALSE
    )
  }

  invisible(record)
}

normalize_snapshot_repositories <- function(repositories) {
  if (
    !is.character(repositories) ||
      length(repositories) == 0L ||
      is.null(names(repositories)) ||
      any(is.na(repositories)) ||
      any(is.na(names(repositories))) ||
      any(!nzchar(names(repositories))) ||
      anyDuplicated(names(repositories)) ||
      anyDuplicated(repositories)
  ) {
    stop(
      "`repositories` must be a named, ordered set of unique URLs.",
      call. = FALSE
    )
  }

  normalized_names <- vapply(
    names(repositories),
    validate_contract_token,
    character(1L),
    argument = "repository name"
  )
  normalized_urls <- vapply(
    unname(repositories),
    validate_contract_text,
    character(1L),
    argument = "repository URL"
  )
  stats::setNames(unname(normalized_urls), unname(normalized_names))
}

normalize_snapshot_packages <- function(package_database, repositories) {
  repositories <- normalize_snapshot_repositories(repositories)
  if (!is.matrix(package_database) && !is.data.frame(package_database)) {
    stop(
      "`package_database` must be a character matrix or data frame.",
      call. = FALSE
    )
  }
  if (
    is.null(colnames(package_database)) ||
      any(is.na(colnames(package_database))) ||
      any(!nzchar(colnames(package_database))) ||
      anyDuplicated(colnames(package_database))
  ) {
    stop("Package-database columns must have unique names.", call. = FALSE)
  }

  packages <- as.data.frame(
    package_database,
    stringsAsFactors = FALSE,
    optional = TRUE
  )
  if (!all(vapply(packages, is.character, logical(1L)))) {
    stop("Every package-database column must be character.", call. = FALSE)
  }
  missing_fields <- setdiff(
    c(
      "Package",
      "Version",
      "Depends",
      "Imports",
      "LinkingTo",
      "Suggests",
      "Repository"
    ),
    names(packages)
  )
  if (length(missing_fields) > 0L) {
    stop(
      sprintf(
        "Package database is missing required fields: %s.",
        paste(missing_fields, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (nrow(packages) > 0L) {
    packages$Package <- vapply(
      packages$Package,
      validate_package_name,
      character(1L)
    )
    packages$Version <- vapply(
      packages$Version,
      validate_package_version,
      character(1L)
    )
    packages$Repository <- vapply(
      packages$Repository,
      validate_contract_text,
      character(1L),
      argument = "Repository"
    )
    repository_priority <- vapply(
      packages$Repository,
      snapshot_repository_priority,
      integer(1L),
      repositories = repositories
    )
    repository_package <- paste(
      repository_priority,
      packages$Package,
      sep = "\r"
    )
  } else {
    repository_priority <- integer()
    repository_package <- character()
  }

  for (field in names(packages)) {
    value <- packages[[field]]
    value[!is.na(value)] <- enc2utf8(value[!is.na(value)])
    packages[[field]] <- value
  }
  duplicates <- normalize_snapshot_repository_duplicates(
    packages,
    repository_priority,
    repository_package,
    repositories
  )
  packages <- duplicates$packages
  repository_priority <- duplicates$repository_priority
  packages <- packages[
    order(repository_priority, packages$Package, method = "radix"),
    sort(names(packages), method = "radix"),
    drop = FALSE
  ]
  rownames(packages) <- NULL
  packages
}

normalize_snapshot_repository_duplicates <- function(
  packages,
  repository_priority,
  repository_package,
  repositories
) {
  duplicated_rows <- duplicated(repository_package) |
    duplicated(repository_package, fromLast = TRUE)
  if (!any(duplicated_rows)) {
    return(list(
      packages = packages,
      repository_priority = repository_priority
    ))
  }

  groups <- split(
    which(duplicated_rows),
    repository_package[duplicated_rows]
  )
  keep <- rep(TRUE, nrow(packages))
  for (rows in unname(groups)) {
    priority <- unique(repository_priority[rows])
    canonical <- rows[
      packages$Repository[rows] == repositories[[priority]]
    ]
    source_fields <- c("Package", "Version", "MD5sum")
    identical_source <- length(canonical) == 1L &&
      all(source_fields %in% names(packages)) &&
      !anyNA(packages[rows, source_fields, drop = FALSE]) &&
      all(vapply(
        source_fields,
        function(field) {
          identical(
            packages[[field]][rows],
            rep(packages[[field]][canonical], length(rows))
          )
        },
        logical(1L)
      ))
    if (!identical_source) {
      stop(
        "Package database contains duplicate package rows within a repository.",
        call. = FALSE
      )
    }
    keep[setdiff(rows, canonical)] <- FALSE
  }

  list(
    packages = packages[keep, , drop = FALSE],
    repository_priority = repository_priority[keep]
  )
}

snapshot_repository_priority <- function(repository, repositories) {
  matches <- which(
    repository == repositories |
      startsWith(repository, paste0(repositories, "/"))
  )
  if (length(matches) != 1L) {
    stop(
      "Each package row must belong to exactly one configured repository.",
      call. = FALSE
    )
  }

  matches
}

discover_reverse_dependency_targets <- function(
  package,
  packages,
  repositories,
  target_repos = NULL
) {
  package <- validate_package_name(package)
  packages <- reverse_dependency_target_packages(
    packages,
    repositories,
    target_repos
  )
  database <- as.matrix(packages)

  direct <- tools::package_dependencies(
    package,
    db = database,
    which = "most",
    recursive = FALSE,
    reverse = TRUE
  )[[package]]
  recursive <- tools::package_dependencies(
    package,
    db = database,
    which = "most",
    recursive = "strong",
    reverse = TRUE
  )[[package]]
  direct <- normalize_reverse_dependency_names(direct)
  recursive <- normalize_reverse_dependency_names(recursive)
  if (!all(direct %in% recursive)) {
    stop(
      "Recursive-strong reverse dependencies do not contain every direct target.",
      call. = FALSE
    )
  }

  recursive_only <- setdiff(recursive, direct)
  target_packages <- c(direct, recursive_only)
  roles <- c(
    rep("direct", length(direct)),
    rep("recursive-strong-only", length(recursive_only))
  )
  if (length(target_packages) == 0L) {
    return(empty_reverse_dependency_targets())
  }

  index <- match(target_packages, packages$Package)
  if (anyNA(index)) {
    stop(
      "A reverse-dependency target is absent from the snapshot.",
      call. = FALSE
    )
  }
  targets <- data.frame(
    package = target_packages,
    version = packages$Version[index],
    role = roles,
    stringsAsFactors = FALSE
  )
  targets <- targets[
    order(targets$package, method = "radix"),
    ,
    drop = FALSE
  ]
  rownames(targets) <- NULL
  targets
}

normalize_target_repos <- function(target_repos, repositories) {
  if (is.null(target_repos)) {
    return(NULL)
  }
  if (
    !is.character(target_repos) ||
      !length(target_repos) ||
      anyNA(target_repos) ||
      anyDuplicated(target_repos) ||
      !all(target_repos %in% names(repositories))
  ) {
    stop(
      "`target_repos` must contain unique names from `repos`.",
      call. = FALSE
    )
  }
  configured <- intersect(names(repositories), target_repos)
  c(intersect("CRAN", configured), setdiff(configured, "CRAN"))
}

target_repo_names <- function(target_repos, repositories) {
  if (!is.null(target_repos)) {
    target_repos
  } else if ("CRAN" %in% names(repositories)) {
    "CRAN"
  } else {
    names(repositories)
  }
}

reverse_dependency_target_packages <- function(
  packages,
  repositories,
  target_repos = NULL
) {
  # Snapshot rows are already ordered by repository priority.
  urls <- unique(packages$Repository)
  priority <- vapply(
    urls,
    snapshot_repository_priority,
    integer(1L),
    repositories = repositories
  )
  eligible <- target_repo_names(target_repos, repositories)
  rank <- match(names(repositories)[priority], eligible)
  package_rank <- rank[match(packages$Repository, urls)]
  packages <- packages[
    order(package_rank, na.last = NA, method = "radix"),
    ,
    drop = FALSE
  ]
  packages[!duplicated(packages$Package), , drop = FALSE]
}

# A selected target can also be another target's dependency. Use its selected
# repository row throughout preparation, even if another repo shadows it.
preparation_package_rows <- function(
  packages,
  repositories,
  targets,
  target_repos
) {
  if (!is.null(target_repos)) {
    eligible <- reverse_dependency_target_packages(
      packages,
      repositories,
      target_repos
    )
    packages <- rbind(
      eligible[eligible$Package %in% targets, , drop = FALSE],
      packages
    )
  }
  packages[!duplicated(packages$Package), , drop = FALSE]
}

normalize_reverse_dependency_names <- function(packages) {
  if (is.null(packages)) {
    return(character())
  }
  if (!is.character(packages) || anyNA(packages)) {
    stop(
      "Reverse-dependency query returned invalid package names.",
      call. = FALSE
    )
  }
  if (length(packages) == 0L) {
    return(character())
  }

  packages <- vapply(
    packages,
    validate_package_name,
    character(1L)
  )
  sort(unique(unname(packages)), method = "radix")
}

empty_reverse_dependency_targets <- function() {
  data.frame(
    package = character(),
    version = character(),
    role = character(),
    stringsAsFactors = FALSE
  )
}

repository_snapshot_identity_fields <- function(
  filters,
  repositories,
  packages
) {
  c(
    filters = filters,
    indexed_vector_identity_fields(
      "repository",
      repositories,
      include_names = TRUE
    ),
    tabular_identity_fields("package", packages)
  )
}

reverse_dependency_cohort_identity_fields <- function(
  snapshot_id,
  package,
  direct_query,
  recursive_query,
  targets,
  target_repos = NULL
) {
  c(
    snapshot_id = snapshot_id,
    package = package,
    indexed_vector_identity_fields(
      "direct_query",
      direct_query,
      include_names = TRUE
    ),
    indexed_vector_identity_fields(
      "recursive_query",
      recursive_query,
      include_names = TRUE
    ),
    if (!is.null(target_repos)) {
      indexed_vector_identity_fields("target_repo", target_repos, FALSE)
    },
    tabular_identity_fields("target", targets)
  )
}

indexed_vector_identity_fields <- function(prefix, value, include_names) {
  fields <- stats::setNames(
    as.character(length(value)),
    paste0(prefix, ".count")
  )
  if (length(value) == 0L) {
    return(fields)
  }

  for (index in seq_along(value)) {
    key <- sprintf("%s.%06d", prefix, index)
    if (include_names) {
      fields <- c(
        fields,
        stats::setNames(
          encode_contract_cell(names(value)[[index]]),
          paste0(key, ".name")
        )
      )
    }
    fields <- c(
      fields,
      stats::setNames(
        encode_contract_cell(value[[index]]),
        paste0(key, ".value")
      )
    )
  }
  fields
}

tabular_identity_fields <- function(prefix, table) {
  fields <- c(
    stats::setNames(as.character(nrow(table)), paste0(prefix, ".nrow")),
    stats::setNames(as.character(ncol(table)), paste0(prefix, ".ncol"))
  )
  columns <- lapply(seq_along(table), function(column) {
    column_key <- sprintf("%s.column.%06d", prefix, column)
    # Repository columns repeat versions, requirements, and missing cells often.
    # Encode each distinct cell once without changing row order or field names.
    cells <- table[[column]]
    unique_cells <- unique(cells)
    encoded <- vapply(unique_cells, encode_contract_cell, character(1L))
    values <- c(
      encode_contract_cell(names(table)[[column]]),
      unname(encoded[match(cells, unique_cells)])
    )
    names(values) <- c(
      paste0(column_key, ".name"),
      sprintf("%s.row.%06d", column_key, seq_len(nrow(table)))
    )
    values
  })
  c(fields, unlist(columns, recursive = FALSE, use.names = TRUE))
}

encode_contract_cell <- function(value) {
  if (length(value) != 1L) {
    stop("Contract table cells must be scalar.", call. = FALSE)
  }
  if (is.na(value)) {
    return(NA_character_)
  }
  if (!is.character(value)) {
    stop("Contract table cells must be character.", call. = FALSE)
  }

  bytes <- charToRaw(enc2utf8(value))
  paste0("utf8hex:", paste(format(bytes), collapse = ""))
}
