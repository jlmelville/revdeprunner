source_acquisition_plan_schema_version <- function() {
  "revdeprunner-source-acquisition-plan/v2"
}

new_source_acquisition_plan <- function(
  universe,
  cohort,
  snapshot,
  binary_reuse,
  lane,
  path_plan
) {
  validate_dependency_universe(universe, cohort, snapshot)
  validate_binary_reuse(binary_reuse, lane, path_plan)

  requirements <- derive_preparation_requirements(universe)
  validate_source_acquisition_reuse_coverage(requirements, binary_reuse)
  sources <- derive_source_acquisition_rows(
    requirements,
    snapshot,
    binary_reuse
  )
  binary_reuse_id <- source_acquisition_binary_reuse_id(
    binary_reuse,
    lane,
    path_plan
  )
  schema_version <- source_acquisition_plan_schema_version()
  fields <- source_acquisition_plan_identity_fields(
    snapshot$snapshot_id,
    cohort$cohort_id,
    universe$universe_id,
    lane$lane_id,
    path_plan$path_plan_id,
    binary_reuse_id,
    requirements,
    sources
  )
  plan <- structure(
    list(
      schema_version = schema_version,
      source_plan_id = record_identity(schema_version, fields),
      snapshot_id = snapshot$snapshot_id,
      cohort_id = cohort$cohort_id,
      universe_id = universe$universe_id,
      lane_id = lane$lane_id,
      path_plan_id = path_plan$path_plan_id,
      binary_reuse_id = binary_reuse_id,
      requirements = requirements,
      sources = sources
    ),
    class = "revdeprunner_source_acquisition_plan"
  )
  validate_source_acquisition_plan(
    plan,
    universe,
    cohort,
    snapshot,
    binary_reuse,
    lane,
    path_plan
  )
  plan
}

validate_source_acquisition_plan <- function(
  plan,
  universe,
  cohort,
  snapshot,
  binary_reuse,
  lane,
  path_plan
) {
  validate_composite_contract_record(
    plan,
    c(
      "schema_version",
      "source_plan_id",
      "snapshot_id",
      "cohort_id",
      "universe_id",
      "lane_id",
      "path_plan_id",
      "binary_reuse_id",
      "requirements",
      "sources"
    ),
    "revdeprunner_source_acquisition_plan",
    "source acquisition plan"
  )
  if (
    !identical(
      plan$schema_version,
      source_acquisition_plan_schema_version()
    )
  ) {
    stop(
      "Source acquisition plan schema version is unsupported.",
      call. = FALSE
    )
  }
  identity_fields <- c(
    "source_plan_id",
    "snapshot_id",
    "cohort_id",
    "universe_id",
    "lane_id",
    "path_plan_id",
    "binary_reuse_id"
  )
  invisible(vapply(
    plan[identity_fields],
    validate_sha256_identity,
    character(1L),
    argument = "source acquisition identity"
  ))

  validate_dependency_universe(universe, cohort, snapshot)
  validate_binary_reuse(binary_reuse, lane, path_plan)
  bindings <- c(
    snapshot_id = snapshot$snapshot_id,
    cohort_id = cohort$cohort_id,
    universe_id = universe$universe_id,
    lane_id = lane$lane_id,
    path_plan_id = path_plan$path_plan_id
  )
  if (!identical(unlist(plan[names(bindings)], use.names = TRUE), bindings)) {
    stop(
      "Source acquisition plan context bindings do not match.",
      call. = FALSE
    )
  }

  requirements <- derive_preparation_requirements(universe)
  if (!identical(plan$requirements, requirements)) {
    stop(
      "Source acquisition plan requirements do not match its universe.",
      call. = FALSE
    )
  }
  validate_source_acquisition_reuse_coverage(requirements, binary_reuse)
  binary_reuse_id <- source_acquisition_binary_reuse_id(
    binary_reuse,
    lane,
    path_plan
  )
  if (!identical(plan$binary_reuse_id, binary_reuse_id)) {
    stop(
      "Source acquisition plan does not match its binary reuse.",
      call. = FALSE
    )
  }
  sources <- derive_source_acquisition_rows(
    requirements,
    snapshot,
    binary_reuse
  )
  if (!identical(plan$sources, sources)) {
    stop(
      "Source acquisition plan sources are not normalized.",
      call. = FALSE
    )
  }

  fields <- source_acquisition_plan_identity_fields(
    plan$snapshot_id,
    plan$cohort_id,
    plan$universe_id,
    plan$lane_id,
    plan$path_plan_id,
    plan$binary_reuse_id,
    plan$requirements,
    plan$sources
  )
  expected <- record_identity(plan$schema_version, fields)
  if (!identical(plan$source_plan_id, expected)) {
    stop(
      "Source acquisition plan identity does not match its fields.",
      call. = FALSE
    )
  }

  invisible(plan)
}

validate_source_acquisition_reuse_coverage <- function(
  requirements,
  binary_reuse
) {
  required <- preparation_required_packages(requirements)
  required <- required[!is.na(required$version), , drop = FALSE]
  rownames(required) <- NULL
  if (!identical(binary_reuse$requests, required)) {
    stop(
      "Binary reuse requests must cover every available requirement.",
      call. = FALSE
    )
  }

  invisible(required)
}

source_acquisition_fields <- function() {
  c(
    "package",
    "version",
    "repository",
    "source_url",
    "expected_md5",
    "needs_compilation",
    "system_requirements",
    "binary_status",
    "build_required"
  )
}

derive_source_acquisition_rows <- function(
  requirements,
  snapshot,
  binary_reuse
) {
  available <- preparation_required_packages(requirements)
  available <- available[!is.na(available$version), , drop = FALSE]
  if (nrow(available) == 0L) {
    values <- stats::setNames(
      replicate(
        length(source_acquisition_fields()),
        character(),
        simplify = FALSE
      ),
      source_acquisition_fields()
    )
    return(as.data.frame(values, stringsAsFactors = FALSE, optional = TRUE))
  }

  selected_packages <- snapshot$packages[
    !duplicated(snapshot$packages$Package),
    ,
    drop = FALSE
  ]
  rows <- lapply(seq_len(nrow(available)), function(index) {
    package <- available$package[[index]]
    version <- available$version[[index]]
    package_row <- selected_packages[
      selected_packages$Package == package,
      ,
      drop = FALSE
    ]
    if (
      nrow(package_row) != 1L ||
        !identical(package_row$Version[[1L]], version)
    ) {
      stop(
        "An available requirement does not match its repository-priority row.",
        call. = FALSE
      )
    }
    repository_priority <- snapshot_repository_priority(
      package_row$Repository[[1L]],
      snapshot$repositories
    )
    status <- binary_reuse$selections[[package]]$status
    data.frame(
      package = package,
      version = version,
      repository = names(snapshot$repositories)[[repository_priority]],
      source_url = source_acquisition_url(package_row),
      expected_md5 = source_acquisition_md5(package_row),
      needs_compilation = source_acquisition_compilation(package_row),
      system_requirements = source_acquisition_system_requirements(package_row),
      binary_status = status,
      build_required = if (identical(status, "missing")) "true" else "false",
      stringsAsFactors = FALSE
    )
  })
  sources <- do.call(rbind, rows)
  sources <- sources[order(sources$package, method = "radix"), , drop = FALSE]
  rownames(sources) <- NULL
  sources
}

source_acquisition_url <- function(package_row) {
  repository <- validate_preparation_source_url(
    package_row$Repository[[1L]]
  )
  filename <- if (
    "File" %in% names(package_row) && !is.na(package_row$File[[1L]])
  ) {
    validate_source_acquisition_file(package_row$File[[1L]])
  } else {
    paste0(
      package_row$Package[[1L]],
      "_",
      package_row$Version[[1L]],
      ".tar.gz"
    )
  }
  validate_preparation_source_url(
    paste0(sub("/+$", "", repository), "/", filename)
  )
}

validate_source_acquisition_file <- function(file) {
  file <- validate_contract_text(file, "File")
  if (
    runtime_path_is_absolute(file) ||
      grepl("\\\\", file) ||
      grepl("[?#]", file) ||
      grepl("^[A-Za-z][A-Za-z0-9+.-]*://", file)
  ) {
    stop("Repository `File` must be a safe relative URL path.", call. = FALSE)
  }
  validate_source_acquisition_percent_escapes(file)
  components <- strsplit(file, "/", fixed = TRUE)[[1L]]
  if (
    any(!nzchar(components)) ||
      any(components %in% c(".", "..")) ||
      any(grepl("[[:space:]]", components))
  ) {
    stop("Repository `File` must be a safe relative URL path.", call. = FALSE)
  }

  file
}

validate_source_acquisition_percent_escapes <- function(file) {
  if (!grepl("%", file, fixed = TRUE)) {
    return(invisible(file))
  }
  if (grepl("%(?![A-Fa-f0-9]{2})", file, perl = TRUE)) {
    stop("Repository `File` must be a safe relative URL path.", call. = FALSE)
  }

  escapes <- regmatches(
    file,
    gregexpr("%[A-Fa-f0-9]{2}", file, perl = TRUE)
  )[[1L]]
  bytes <- strtoi(substring(escapes, 2L, 3L), base = 16L)
  unreserved <-
    (bytes >= 48L & bytes <= 57L) |
    (bytes >= 65L & bytes <= 90L) |
    (bytes >= 97L & bytes <= 122L) |
    bytes %in% c(45L, 46L, 95L, 126L)
  decoded_dots <- gsub("%2e", ".", file, ignore.case = TRUE)
  decoded_components <- strsplit(decoded_dots, "/", fixed = TRUE)[[1L]]
  if (
    any(!unreserved) ||
      any(decoded_components %in% c(".", ".."))
  ) {
    stop("Repository `File` must be a safe relative URL path.", call. = FALSE)
  }

  invisible(file)
}

source_acquisition_md5 <- function(package_row) {
  if (!"MD5sum" %in% names(package_row) || is.na(package_row$MD5sum[[1L]])) {
    return(NA_character_)
  }
  checksum <- package_row$MD5sum[[1L]]
  if (!grepl("^[A-Fa-f0-9]{32}$", checksum)) {
    stop("Repository `MD5sum` must be one MD5 checksum.", call. = FALSE)
  }

  tolower(checksum)
}

source_acquisition_compilation <- function(package_row) {
  if (
    !"NeedsCompilation" %in% names(package_row) ||
      is.na(package_row$NeedsCompilation[[1L]])
  ) {
    return("unknown")
  }
  value <- package_row$NeedsCompilation[[1L]]
  if (!value %in% c("yes", "no")) {
    stop(
      "Repository `NeedsCompilation` must be `yes`, `no`, or unavailable.",
      call. = FALSE
    )
  }

  value
}

source_acquisition_system_requirements <- function(package_row) {
  if (
    !"SystemRequirements" %in% names(package_row) ||
      is.na(package_row$SystemRequirements[[1L]])
  ) {
    return(NA_character_)
  }
  validate_preparation_optional_text(
    package_row$SystemRequirements[[1L]],
    "SystemRequirements"
  )
}

source_acquisition_binary_reuse_id <- function(
  binary_reuse,
  lane,
  path_plan
) {
  validate_binary_reuse(binary_reuse, lane, path_plan)
  rows <- lapply(binary_reuse$requests$package, function(package) {
    selection <- binary_reuse$selections[[package]]
    cache_path <- binary_reuse$cache_paths[[package]]
    data.frame(
      package = selection$package,
      version = selection$version,
      status = selection$status,
      artifact_id = if (is.null(selection$artifact)) {
        NA_character_
      } else {
        selection$artifact$artifact_id
      },
      cache_root = selection$cache_root,
      relative_path = selection$relative_path,
      source_path = selection$source_path,
      priority = if (is.na(selection$priority)) {
        NA_character_
      } else {
        as.character(selection$priority)
      },
      cache_path = cache_path,
      stringsAsFactors = FALSE
    )
  })
  binding <- do.call(rbind, rows)
  rownames(binding) <- NULL
  record_identity(
    "revdeprunner-binary-reuse-binding/v1",
    c(
      lane_id = binary_reuse$lane_id,
      path_plan_id = binary_reuse$path_plan_id,
      tabular_identity_fields("reuse", binding)
    )
  )
}

source_acquisition_plan_identity_fields <- function(
  snapshot_id,
  cohort_id,
  universe_id,
  lane_id,
  path_plan_id,
  binary_reuse_id,
  requirements,
  sources
) {
  c(
    snapshot_id = snapshot_id,
    cohort_id = cohort_id,
    universe_id = universe_id,
    lane_id = lane_id,
    path_plan_id = path_plan_id,
    binary_reuse_id = binary_reuse_id,
    tabular_identity_fields("requirement", requirements),
    tabular_identity_fields("source", sources)
  )
}
