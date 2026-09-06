seed_stock_binary_cache <- function(gate, context, contrib_path) {
  manifest <- stock_binary_manifest(gate, context)
  seed_stock_cache_repository(
    manifest$cache_path,
    manifest[c("package", "version", "archive_name", "sha256")],
    contrib_path
  )
}

stock_binary_manifest <- function(gate, context) {
  results <- gate$report$results
  rows <- lapply(seq_len(nrow(results)), function(row) {
    result <- results[row, , drop = FALSE]
    artifact <- gate$report$artifacts[
      gate$report$artifacts$artifact_id == result$artifact_id,
      ,
      drop = FALSE
    ]
    if (
      nrow(artifact) != 1L ||
        !identical(artifact$package[[1L]], result$package[[1L]]) ||
        !identical(artifact$version[[1L]], result$version[[1L]]) ||
        !identical(artifact$archive_type[[1L]], "binary") ||
        !identical(artifact$lane_id[[1L]], context$lane$lane_id)
    ) {
      stop("Stock binary evidence is ambiguous.", call. = FALSE)
    }
    identity <- new_artifact_identity(
      artifact$package[[1L]],
      artifact$version[[1L]],
      artifact$sha256[[1L]],
      "binary",
      context$lane
    )
    if (!identical(identity$artifact_id, artifact$artifact_id[[1L]])) {
      stop("Stock binary artifact identity is inconsistent.", call. = FALSE)
    }
    cache_path <- stock_prepared_binary_path(
      result$package[[1L]],
      gate,
      context
    )
    data.frame(
      package = artifact$package[[1L]],
      version = artifact$version[[1L]],
      archive_name = stock_binary_archive_name(
        result$package[[1L]],
        cache_path
      ),
      sha256 = artifact$sha256[[1L]],
      cache_path = cache_path,
      stringsAsFactors = FALSE
    )
  })
  manifest <- do.call(rbind, rows)
  manifest <- manifest[
    order(manifest$package, method = "radix"),
    ,
    drop = FALSE
  ]
  rownames(manifest) <- NULL
  manifest
}

stock_prepared_binary_path <- function(package, gate, context) {
  if (package %in% names(gate$source_preparations)) {
    path <- gate$source_preparations[[package]]$binary_path
  } else {
    selection <- context$binary_reuse$selections[[package]]
    if (!identical(selection$status, "selected")) {
      stop("Stock binary filename evidence is unavailable.", call. = FALSE)
    }
    path <- preparation_gate_hit_cache_path(selection, context)
  }
  path
}

stock_binary_archive_name <- function(package, path) {
  archive_name <- validate_package_archive_name(basename(path))
  fields <- archive_filename_fields(archive_name)
  if (!identical(fields$package, package) || is.na(fields$platform)) {
    stop("Stock binary filename is inconsistent.", call. = FALSE)
  }
  archive_name
}

seed_stock_source_cache <- function(
  gate,
  baseline,
  contrib_path,
  context,
  source_archives = character()
) {
  acquisitions <- gate$source_acquisitions
  source_rows <- context$source_plan$sources
  source_archives <- normalize_stock_source_archives(
    source_archives,
    source_rows
  )
  observations <- normalize_cache_observations(
    context$binary_reuse$observations,
    context$path_plan
  )
  sources <- lapply(seq_len(nrow(source_rows)), function(row) {
    package <- source_rows$package[[row]]
    acquisition <- acquisitions[[package]]
    if (!is.null(acquisition)) {
      return(list(
        path = acquisition$cache_path,
        package = acquisition$package,
        version = acquisition$version,
        sha256 = acquisition$artifact$sha256
      ))
    }
    if (package %in% names(source_archives)) {
      return(stock_source_archive_override(
        source_archives[[package]],
        source_rows[row, , drop = FALSE]
      ))
    }
    cached <- stock_cached_source_for_binary(
      source_rows[row, , drop = FALSE],
      observations,
      context$path_plan
    )
    if (!is.null(cached)) {
      return(cached)
    }
    stock_acquire_source_for_binary(
      package,
      context$source_plan,
      context$path_plan
    )
  })
  paths <- vapply(sources, `[[`, character(1L), "path")
  packages <- vapply(sources, `[[`, character(1L), "package")
  versions <- vapply(sources, `[[`, character(1L), "version")
  hashes <- vapply(sources, `[[`, character(1L), "sha256")
  paths <- c(baseline$path, paths)
  packages <- c(baseline$package, packages)
  versions <- c(baseline$version, versions)
  hashes <- c(baseline$sha256, hashes)
  expected <- data.frame(
    package = packages,
    version = versions,
    archive_name = paste0(packages, "_", versions, ".tar.gz"),
    sha256 = hashes,
    stringsAsFactors = FALSE
  )
  if (anyDuplicated(expected$package) || anyDuplicated(expected$archive_name)) {
    stop("Stock source cache package identities are ambiguous.", call. = FALSE)
  }
  seed_stock_cache_repository(paths, expected, contrib_path)
}

normalize_stock_source_archives <- function(source_archives, source_rows) {
  if (length(source_archives) == 0L) {
    return(character())
  }
  packages <- names(source_archives)
  if (
    !is.character(source_archives) ||
      is.null(packages) ||
      anyNA(source_archives) ||
      anyNA(packages) ||
      any(!nzchar(source_archives)) ||
      any(!nzchar(packages)) ||
      anyDuplicated(packages)
  ) {
    stop(
      "`source_archives` must be a named vector of exact source paths.",
      call. = FALSE
    )
  }
  invisible(vapply(packages, validate_package_name, character(1L)))
  if (any(!packages %in% source_rows$package)) {
    stop(
      "Stock source overrides must be frozen dependency packages.",
      call. = FALSE
    )
  }
  source_archives
}

stock_source_archive_override <- function(path, source) {
  if (nrow(source) != 1L) {
    stop("Stock source override selection is inconsistent.", call. = FALSE)
  }
  path <- normalize_regular_artifact_file(path, "source override archive")
  archive_name <- paste0(
    source$package[[1L]],
    "_",
    source$version[[1L]],
    ".tar.gz"
  )
  before <- artifact_file_snapshot(path)
  metadata <- read_archive_metadata(
    path,
    archive_filename_fields(archive_name),
    archive_name
  )
  md5 <- digest::digest(
    path,
    algo = "md5",
    file = TRUE,
    serialize = FALSE
  )
  validate_artifact_file_unchanged(path, before)
  if (
    !identical(metadata$status, "ok") ||
      !identical(metadata$archive_type, "source") ||
      !identical(metadata$package, source$package[[1L]]) ||
      !identical(metadata$version, source$version[[1L]]) ||
      !identical(md5, source$expected_md5[[1L]])
  ) {
    stop(
      "Stock source override differs from the frozen source.",
      call. = FALSE
    )
  }
  list(
    path = path,
    package = metadata$package,
    version = metadata$version,
    sha256 = before$sha256
  )
}

stock_cached_source_for_binary <- function(source, observations, path_plan) {
  if (nrow(source) != 1L) {
    stop("Stock cached-source selection is inconsistent.", call. = FALSE)
  }
  candidates <- observations[
    observations$status == "ok" &
      observations$archive_type == "source" &
      !is.na(observations$package) &
      observations$package == source$package[[1L]] &
      !is.na(observations$version) &
      observations$version == source$version[[1L]] &
      !is.na(observations$sha256),
    ,
    drop = FALSE
  ]
  if (nrow(candidates) == 0L) {
    return(NULL)
  }
  observed <- lapply(seq_len(nrow(candidates)), function(row) {
    path <- file.path(
      candidates$cache_root[[row]],
      candidates$relative_path[[row]]
    )
    path <- tryCatch(
      normalize_artifact_path(path, path_plan),
      error = function(error) NULL
    )
    if (
      is.null(path) ||
        !path_is_within(candidates$cache_root[[row]], path)
    ) {
      return(NULL)
    }
    before <- artifact_file_snapshot(path)
    sha256 <- digest::digest(
      path,
      algo = "sha256",
      file = TRUE,
      serialize = FALSE
    )
    md5 <- digest::digest(
      path,
      algo = "md5",
      file = TRUE,
      serialize = FALSE
    )
    validate_artifact_file_unchanged(path, before)
    list(
      path = path,
      relative_path = candidates$relative_path[[row]],
      sha256 = sha256,
      md5 = md5,
      recorded_sha256 = candidates$sha256[[row]],
      priority = candidates$priority[[row]]
    )
  })
  observed <- Filter(Negate(is.null), observed)
  matches <- vapply(
    observed,
    function(candidate) {
      identical(candidate$sha256, candidate$recorded_sha256) &&
        identical(candidate$md5, source$expected_md5[[1L]])
    },
    logical(1L)
  )
  observed <- observed[matches]
  if (length(observed) == 0L) {
    return(NULL)
  }
  hashes <- vapply(observed, `[[`, character(1L), "sha256")
  if (length(unique(hashes)) != 1L) {
    stop(
      sprintf(
        "Stock cached-source identity is ambiguous for %s %s.",
        source$package[[1L]],
        source$version[[1L]]
      ),
      call. = FALSE
    )
  }
  priorities <- vapply(observed, `[[`, integer(1L), "priority")
  relative_paths <- vapply(
    observed,
    `[[`,
    character(1L),
    "relative_path"
  )
  selected <- observed[[
    order(priorities, relative_paths, method = "radix")[[1L]]
  ]]
  list(
    path = selected$path,
    package = source$package[[1L]],
    version = source$version[[1L]],
    sha256 = selected$sha256
  )
}

stock_acquire_source_for_binary <- function(package, source_plan, path_plan) {
  source <- source_acquisition_planned_row(source_plan, package)
  acquisition <- tryCatch(
    acquire_source_artifact_in_context(package, source_plan, path_plan),
    error = function(error) {
      stop(
        sprintf(
          "Unable to resolve stock source for %s %s: %s",
          source$package[[1L]],
          source$version[[1L]],
          conditionMessage(error)
        ),
        call. = FALSE
      )
    }
  )
  list(
    path = acquisition$cache_path,
    package = acquisition$package,
    version = acquisition$version,
    sha256 = acquisition$artifact$sha256
  )
}

seed_stock_cache_repository <- function(sources, expected, contrib_path) {
  if (
    !is.character(sources) ||
      length(sources) != nrow(expected) ||
      nrow(expected) == 0L
  ) {
    stop("Stock cache seed inputs are inconsistent.", call. = FALSE)
  }
  if (!dir.create(contrib_path, recursive = TRUE)) {
    stop(
      "Unable to create a stock cache contribution directory.",
      call. = FALSE
    )
  }
  contrib_path <- normalizePath(contrib_path, winslash = "/", mustWork = TRUE)
  cranlike::create_empty_PACKAGES(contrib_path)
  for (row in seq_len(nrow(expected))) {
    source <- normalize_regular_artifact_file(
      sources[[row]],
      "cache seed artifact"
    )
    source_before <- artifact_file_snapshot(source)
    destination <- file.path(contrib_path, expected$archive_name[[row]])
    copied <- file.copy(
      source,
      destination,
      overwrite = FALSE,
      copy.mode = FALSE,
      copy.date = FALSE
    )
    if (!isTRUE(copied)) {
      stop("Unable to copy an artifact into stock cache state.", call. = FALSE)
    }
    observed <- digest::digest(
      destination,
      algo = "sha256",
      file = TRUE,
      serialize = FALSE
    )
    if (!identical(observed, expected$sha256[[row]])) {
      stop(
        "Stock cache artifact hash differs from its frozen input.",
        call. = FALSE
      )
    }
    validate_artifact_file_unchanged(source, source_before)
  }
  cranlike::add_PACKAGES(expected$archive_name, dir = contrib_path)
  expected <- expected[
    order(expected$package, method = "radix"),
    ,
    drop = FALSE
  ]
  rownames(expected) <- NULL
  validate_stock_cache_repository(contrib_path, expected)
  expected
}

validate_stock_cache_repository <- function(contrib_path, expected) {
  contrib_path <- normalize_runtime_anchor(
    contrib_path,
    "stock cache repository"
  )
  required_indexes <- c(
    "PACKAGES",
    "PACKAGES.gz",
    "PACKAGES.rds",
    "PACKAGES.db"
  )
  if (any(!file.exists(file.path(contrib_path, required_indexes)))) {
    stop("Stock cache repository indexes are incomplete.", call. = FALSE)
  }
  for (entry in c(expected$archive_name, required_indexes)) {
    path <- file.path(contrib_path, entry)
    if (path_is_link(path) || !utils::file_test("-f", path)) {
      stop(
        "Stock cache repository contains a non-regular entry.",
        call. = FALSE
      )
    }
  }
  readers <- list(
    plain = function() read.dcf(file.path(contrib_path, "PACKAGES")),
    gzip = function() {
      connection <- gzfile(file.path(contrib_path, "PACKAGES.gz"), open = "rt")
      on.exit(close(connection), add = TRUE)
      read.dcf(connection)
    },
    rds = function() readRDS(file.path(contrib_path, "PACKAGES.rds")),
    available = function() {
      utils::available.packages(
        contriburl = paste0("file://", contrib_path),
        filters = list()
      )
    }
  )
  for (reader in readers) {
    index <- tryCatch(
      reader(),
      error = function(error) {
        stop("Stock cache repository index is unreadable.", call. = FALSE)
      },
      warning = function(warning) {
        stop("Stock cache repository index is unreadable.", call. = FALSE)
      }
    )
    observed <- stock_adapter_index_manifest(index)
    if (
      !identical(observed, expected[c("package", "version", "archive_name")])
    ) {
      stop(
        "Stock cache repository indexes differ from frozen artifacts.",
        call. = FALSE
      )
    }
  }
  database <- cranlike::package_versions(contrib_path)
  if (!all(c("Package", "Version", "MD5sum") %in% names(database))) {
    stop("Stock cache repository database is incomplete.", call. = FALSE)
  }
  database_manifest <- data.frame(
    package = as.character(database$Package),
    version = as.character(database$Version),
    stringsAsFactors = FALSE
  )
  database_manifest <- database_manifest[
    order(database_manifest$package, method = "radix"),
    ,
    drop = FALSE
  ]
  rownames(database_manifest) <- NULL
  if (
    !identical(
      database_manifest,
      expected[c("package", "version")]
    )
  ) {
    stop(
      "Stock cache repository database differs from frozen artifacts.",
      call. = FALSE
    )
  }
  invisible(contrib_path)
}

stock_adapter_index_manifest <- function(index) {
  if (!is.matrix(index) && !is.data.frame(index)) {
    stop("Stock cache index has an invalid structure.", call. = FALSE)
  }
  index <- as.data.frame(index, stringsAsFactors = FALSE)
  if (!all(c("Package", "Version", "File") %in% names(index))) {
    stop("Stock cache index is missing identity fields.", call. = FALSE)
  }
  manifest <- data.frame(
    package = as.character(index$Package),
    version = as.character(index$Version),
    archive_name = as.character(index$File),
    stringsAsFactors = FALSE
  )
  manifest <- manifest[
    order(manifest$package, method = "radix"),
    ,
    drop = FALSE
  ]
  rownames(manifest) <- NULL
  manifest
}

initialize_stock_empty_repositories <- function(root, snapshot) {
  validate_repository_snapshot(snapshot)
  cran_root <- file.path(root, "cran")
  bioc_root <- file.path(root, "bioc")
  dir.create(cran_root)
  dir.create(bioc_root)
  cran_contrib <- file.path(cran_root, "src", "contrib")
  dir.create(cran_contrib, recursive = TRUE)
  cranlike::create_empty_PACKAGES(cran_contrib)
  settings <- c(
    CRAN = paste0("file://", normalizePath(cran_root, winslash = "/")),
    BioC_mirror = paste0("file://", normalizePath(bioc_root, winslash = "/")),
    R_BIOC_VERSION = stock_adapter_bioc_version(snapshot)
  )
  repositories <- stock_adapter_with_environment(
    c(R_BIOC_VERSION = settings[["R_BIOC_VERSION"]]),
    stock_adapter_with_options(
      list(
        repos = c(CRAN = settings[["CRAN"]]),
        BioC_mirror = settings[["BioC_mirror"]]
      ),
      stock_namespace_function("get_repos")(bioc = TRUE, cran = TRUE)
    )
  )
  repositories <- unique(unname(repositories))
  bioc_urls <- repositories[startsWith(repositories, settings[["BioC_mirror"]])]
  for (url in bioc_urls) {
    repository_root <- sub("^file://", "", url)
    contrib <- file.path(repository_root, "src", "contrib")
    dir.create(contrib, recursive = TRUE, showWarnings = FALSE)
    cranlike::create_empty_PACKAGES(contrib)
  }
  settings
}

stock_adapter_bioc_version <- function(snapshot) {
  matches <- regmatches(
    snapshot$repositories,
    regexec(
      "/packages/([0-9]+[.][0-9]+)(/|$)",
      snapshot$repositories
    )
  )
  versions <- unique(vapply(
    matches[lengths(matches) >= 2L],
    `[[`,
    character(1L),
    2L
  ))
  if (length(versions) == 1L) versions else "3.21"
}

validate_stock_repository_settings <- function(settings, paths) {
  if (
    !is.character(settings) ||
      !identical(
        names(settings),
        c("CRAN", "BioC_mirror", "R_BIOC_VERSION")
      ) ||
      anyNA(settings) ||
      any(!startsWith(settings[c("CRAN", "BioC_mirror")], "file://")) ||
      !grepl("^[0-9]+[.][0-9]+$", settings[["R_BIOC_VERSION"]])
  ) {
    stop("Stock repository settings are invalid.", call. = FALSE)
  }
  roots <- sub("^file://", "", settings[c("CRAN", "BioC_mirror")])
  if (
    any(
      !vapply(
        roots,
        function(path) {
          path_is_within(paths$empty_repos, path)
        },
        logical(1L)
      )
    )
  ) {
    stop("Stock repository fallback escapes run-local state.", call. = FALSE)
  }
  validate_stock_empty_repositories(paths$empty_repos)
  invisible(settings)
}

validate_stock_empty_repositories <- function(root) {
  root <- normalize_runtime_anchor(root, "empty stock repositories")
  contribution_directories <- list.dirs(
    root,
    full.names = TRUE,
    recursive = TRUE
  )
  contribution_directories <- contribution_directories[
    basename(contribution_directories) == "contrib" &
      basename(dirname(contribution_directories)) == "src"
  ]
  required_indexes <- c(
    "PACKAGES",
    "PACKAGES.db",
    "PACKAGES.gz",
    "PACKAGES.rds"
  )
  if (length(contribution_directories) == 0L) {
    stop("Empty stock repository indexes are unavailable.", call. = FALSE)
  }
  for (contrib in contribution_directories) {
    entries <- sort(
      list.files(contrib, all.files = TRUE, no.. = TRUE),
      method = "radix"
    )
    if (!identical(entries, sort(required_indexes, method = "radix"))) {
      stop("Stock repository fallback is not empty.", call. = FALSE)
    }
    available <- utils::available.packages(
      contriburl = paste0("file://", contrib),
      filters = list()
    )
    if (nrow(available) != 0L) {
      stop("Stock repository fallback is not empty.", call. = FALSE)
    }
  }
  invisible(root)
}
