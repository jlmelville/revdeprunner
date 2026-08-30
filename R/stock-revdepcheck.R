# This file composes private helpers defined in other package source files and
# guarded internals from the pinned stock revdepcheck toolchain.
# nolint start: object_usage_linter.

stock_adapter_versions <- function() {
  c(
    revdepcheck = "1.0.0.9002",
    crancache = "0.0.0.9001"
  )
}

stock_adapter_remote_shas <- function() {
  c(
    revdepcheck = "fe363843c340191f56aaf03514af4301f2ccf5c1",
    crancache = "fc51e313db93780b1efd870f1b6bd3b638c909d3"
  )
}

stock_adapter_internals <- function() {
  c(
    "db_disconnect",
    "db_get_results",
    "db_metadata_get",
    "db_metadata_set",
    "db_setup",
    "db_todo_add",
    "deps_opts",
    "dir_setup",
    "rcmdcheck_status"
  )
}

stock_adapter_expected_provenance <- function() {
  versions <- stock_adapter_versions()
  data.frame(
    package = names(versions),
    version = unname(versions),
    remote_sha = unname(stock_adapter_remote_shas()),
    stringsAsFactors = FALSE
  )
}

initialize_stock_revdepcheck <- function(
  repository_preparation,
  context,
  baseline_source,
  exclude_targets = character()
) {
  require_linux_repository_projection()
  require_stock_adapter_tools()
  validate_repository_preparation(repository_preparation, context)
  command_plan <- new_command_plan(
    "compare",
    context$path_plan,
    context$command_plan$r_executable,
    FALSE,
    context$snapshot,
    context$cohort,
    context$universe,
    context$lane,
    repository_preparation$report
  )
  selected_targets <- context$universe$targets
  exclude_targets <- normalize_stock_adapter_exclusions(
    exclude_targets,
    selected_targets$package
  )
  requested_targets <- selected_targets[
    !selected_targets$package %in% exclude_targets,
    ,
    drop = FALSE
  ]
  rownames(requested_targets) <- NULL
  require_ready_stock_targets(
    repository_preparation$report,
    requested_targets
  )
  baseline <- validate_stock_baseline_source(
    baseline_source,
    context$cohort,
    context$snapshot
  )
  paths <- create_stock_adapter_paths(context$path_plan)
  projection_before <- stock_adapter_directory_snapshot(
    repository_preparation$projection$repository_path
  )
  stock_adapter_copy_checkout(
    context$path_plan$package_root,
    paths$checkout
  )
  candidate <- stock_adapter_checkout_identity(
    paths$checkout,
    context$cohort$package
  )

  cache_entries_before <- list.files(
    paths$cache,
    all.files = TRUE,
    no.. = TRUE
  )
  discovery <- initialize_stock_database(
    paths$checkout,
    requested_targets$package,
    paths$cache
  )
  if (
    length(cache_entries_before) != 0L ||
      length(list.files(paths$cache, all.files = TRUE, no.. = TRUE)) != 0L
  ) {
    stop("Stock discovery unexpectedly operated on cache state.", call. = FALSE)
  }

  binary_manifest <- seed_stock_binary_cache(
    repository_preparation$projection,
    paths$binary_contrib
  )
  source_manifest <- seed_stock_source_cache(
    repository_preparation$prepared_gate,
    baseline,
    paths$source_contrib,
    context
  )
  repository_settings <- initialize_stock_empty_repositories(
    paths$empty_repos,
    context$snapshot
  )
  compiler_tools <- create_stock_compiler_wrappers(
    command_plan$r_executable,
    paths$compiler_bin,
    paths$compiler_log
  )
  environment <- stock_adapter_environment(paths, repository_settings)
  runtime <- observe_stock_runtime(
    command_plan$r_executable,
    requested_targets$package,
    context$cohort$package,
    environment,
    repository_settings,
    paths$temp
  )
  stock_dependencies <- stock_dependencies_from_observation(
    runtime$dependencies,
    requested_targets$package,
    context$universe
  )
  cache_before <- stock_adapter_cache_snapshot(paths)
  provenance <- runtime$provenance

  initialization <- structure(
    list(
      command_plan = command_plan,
      repository_preparation = repository_preparation,
      package = context$cohort$package,
      baseline = baseline,
      candidate = candidate,
      selected_targets = selected_targets,
      requested_targets = requested_targets,
      excluded_targets = exclude_targets,
      paths = paths,
      discovery = discovery,
      binary_manifest = binary_manifest,
      source_manifest = source_manifest,
      stock_dependencies = stock_dependencies,
      compiler_tools = compiler_tools,
      environment = environment,
      repository_settings = repository_settings,
      projection_before = projection_before,
      cache_before = cache_before,
      provenance = provenance
    ),
    class = "revdeprunner_stock_initialization"
  )
  validate_stock_revdepcheck_initialization(initialization, context)
  initialization
}

run_stock_revdepcheck <- function(
  initialization,
  context,
  worker_timeout_seconds = 600L,
  process_timeout_seconds = 7200L
) {
  require_linux_repository_projection()
  require_stock_adapter_tools()
  validate_stock_revdepcheck_initialization(initialization, context)
  worker_timeout_seconds <- normalize_source_preparation_timeout(
    worker_timeout_seconds
  )
  process_timeout_seconds <- normalize_source_preparation_timeout(
    process_timeout_seconds
  )
  if (process_timeout_seconds <= worker_timeout_seconds) {
    stop(
      "Stock process timeout must exceed the per-worker timeout.",
      call. = FALSE
    )
  }

  process <- run_stock_revdepcheck_process(
    initialization$command_plan$r_executable,
    initialization$paths,
    initialization$environment,
    initialization$repository_settings,
    worker_timeout_seconds,
    process_timeout_seconds
  )
  database <- observe_stock_database(initialization$paths$checkout)
  projection_after <- stock_adapter_directory_snapshot(
    initialization$repository_preparation$projection$repository_path
  )
  cache_after <- stock_adapter_cache_snapshot(initialization$paths)
  if (!identical(projection_after, initialization$projection_before)) {
    stop(
      "The exact repository projection changed during comparison.",
      call. = FALSE
    )
  }
  if (!identical(cache_after, initialization$cache_before)) {
    stop(
      "The run-local cache artifact views changed during comparison.",
      call. = FALSE
    )
  }
  validate_repository_preparation(
    initialization$repository_preparation,
    context
  )

  results <- stock_adapter_results(initialization, process, database)
  private_libraries <- observe_stock_private_libraries(
    initialization,
    process,
    database
  )
  compiler <- stock_adapter_compiler_evidence(
    initialization$paths$compiler_log
  )
  logs <- stock_adapter_process_logs(initialization$paths, context$path_plan)
  state <- stock_adapter_result_state(process, results)
  result <- structure(
    list(
      initialization = initialization,
      state = state,
      process = process,
      logs = logs,
      compiler = compiler,
      database = database,
      results = results,
      private_libraries = private_libraries,
      projection_after = projection_after,
      cache_after = cache_after
    ),
    class = "revdeprunner_stock_result"
  )
  validate_stock_revdepcheck_result(result, context)
  result
}

require_stock_adapter_tools <- function() {
  required <- c("revdepcheck", "crancache", "cranlike")
  missing <- required[
    !vapply(required, requireNamespace, logical(1L), quietly = TRUE)
  ]
  if (length(missing) > 0L) {
    stop(
      sprintf(
        "The stock adapter requires installed packages: %s.",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  observed <- stock_adapter_provenance()
  expected <- stock_adapter_expected_provenance()
  if (!identical(observed, expected)) {
    stop(
      sprintf(
        paste0(
          "The stock adapter requires the pinned revdepcheck %s ",
          "and crancache %s revisions."
        ),
        expected$version[expected$package == "revdepcheck"],
        expected$version[expected$package == "crancache"]
      ),
      call. = FALSE
    )
  }
  namespace <- asNamespace("revdepcheck")
  if (
    any(
      !vapply(
        stock_adapter_internals(),
        exists,
        logical(1L),
        envir = namespace,
        inherits = FALSE
      )
    )
  ) {
    stop(
      "The installed stock runner has an unsupported internal API.",
      call. = FALSE
    )
  }
  invisible(observed)
}

normalize_stock_adapter_exclusions <- function(exclusions, targets) {
  if (
    !is.character(exclusions) || anyNA(exclusions) || anyDuplicated(exclusions)
  ) {
    stop("`exclude_targets` must contain unique package names.", call. = FALSE)
  }
  exclusions <- vapply(
    exclusions,
    validate_package_name,
    character(1L)
  )
  if (any(!exclusions %in% targets)) {
    stop("Stock exclusions must be selected cohort targets.", call. = FALSE)
  }
  sort(unname(exclusions), method = "radix")
}

require_ready_stock_targets <- function(report, requested_targets) {
  for (row in seq_len(nrow(requested_targets))) {
    target <- requested_targets[row, , drop = FALSE]
    result <- report$results[
      report$results$package == target$package,
      ,
      drop = FALSE
    ]
    if (
      nrow(result) != 1L ||
        !identical(result$version[[1L]], target$version[[1L]]) ||
        !identical(result$outcome[[1L]], "ready")
    ) {
      stop(
        "Every requested stock target must have one exact ready result.",
        call. = FALSE
      )
    }
  }
  invisible(report)
}

validate_stock_baseline_source <- function(path, cohort, snapshot) {
  validate_reverse_dependency_cohort(cohort, snapshot)
  path <- normalize_stock_regular_file(path, "baseline source archive")
  filename <- archive_filename_fields(basename(path))
  metadata <- read_archive_metadata(path, filename)
  package_rows <- snapshot$packages[
    !duplicated(snapshot$packages$Package) &
      snapshot$packages$Package == cohort$package,
    ,
    drop = FALSE
  ]
  if (
    nrow(package_rows) != 1L ||
      !identical(metadata$status, "ok") ||
      !identical(metadata$archive_type, "source") ||
      !identical(metadata$package, cohort$package) ||
      !identical(metadata$version, package_rows$Version[[1L]])
  ) {
    stop(
      "Baseline source archive does not match the frozen package version.",
      call. = FALSE
    )
  }
  expected_md5 <- source_acquisition_md5(package_rows)
  if (is.na(expected_md5)) {
    stop(
      "The frozen baseline source checksum is unavailable.",
      call. = FALSE
    )
  }
  observed_md5 <- digest::digest(
    path,
    algo = "md5",
    file = TRUE,
    serialize = FALSE
  )
  if (!identical(observed_md5, expected_md5)) {
    stop(
      "Baseline source archive differs from its frozen checksum.",
      call. = FALSE
    )
  }
  list(
    path = path,
    package = metadata$package,
    version = metadata$version,
    md5 = observed_md5,
    sha256 = digest::digest(
      path,
      algo = "sha256",
      file = TRUE,
      serialize = FALSE
    )
  )
}

normalize_stock_regular_file <- function(path, label) {
  path <- validate_contract_text(path, label)
  expanded <- path.expand(path)
  if (
    warehouse_path_is_link(expanded) ||
      !utils::file_test("-f", expanded) ||
      dir.exists(expanded)
  ) {
    stop(
      sprintf("The %s must be a regular non-link file.", label),
      call. = FALSE
    )
  }
  normalizePath(expanded, winslash = "/", mustWork = TRUE)
}

create_stock_adapter_paths <- function(path_plan) {
  validate_runtime_root_plan(path_plan)
  run_root <- runtime_role_path(path_plan, "run")
  run_root <- ensure_source_acquisition_directory(
    run_root,
    path_plan$runs_root,
    "stock adapter run root"
  )
  root <- file.path(run_root, "stock-revdepcheck")
  if (file.exists(root) || warehouse_path_is_link(root)) {
    stop("Stock adapter state already exists for this run.", call. = FALSE)
  }
  if (!dir.create(root, recursive = FALSE)) {
    stop("Unable to create stock adapter state.", call. = FALSE)
  }
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  roles <- c(
    checkout = "checkout",
    cache = "crancache",
    home = "home",
    temp = "tmp",
    xdg_cache = "xdg-cache",
    xdg_config = "xdg-config",
    xdg_data = "xdg-data",
    xdg_state = "xdg-state",
    logs = "logs",
    compiler_bin = "compiler-bin",
    empty_repos = "empty-repositories"
  )
  paths <- lapply(roles, function(relative) {
    path <- file.path(root, relative)
    if (!dir.create(path, recursive = FALSE)) {
      stop("Unable to create stock adapter subdirectory.", call. = FALSE)
    }
    normalizePath(path, winslash = "/", mustWork = TRUE)
  })
  paths$root <- root
  paths$binary_contrib <- file.path(paths$cache, "cran-bin", "src", "contrib")
  paths$source_contrib <- file.path(paths$cache, "cran", "src", "contrib")
  paths$stdout <- file.path(paths$logs, "comparison.stdout.log")
  paths$stderr <- file.path(paths$logs, "comparison.stderr.log")
  paths$compiler_log <- file.path(paths$logs, "compiler.log")
  for (path in c(paths$stdout, paths$stderr, paths$compiler_log)) {
    if (!file.create(path)) {
      stop("Unable to create stock adapter log files.", call. = FALSE)
    }
  }
  for (path in c("empty-Renviron", "empty-Rprofile")) {
    if (!file.create(file.path(root, path))) {
      stop("Unable to create empty stock R startup files.", call. = FALSE)
    }
  }
  paths
}

stock_adapter_copy_checkout <- function(source, destination) {
  source <- normalize_runtime_anchor(source, "package_root")
  excluded <- c(".git", ".Rproj.user", "revdep")
  entries <- list.files(
    source,
    all.files = TRUE,
    full.names = TRUE,
    no.. = TRUE
  )
  entries <- entries[!basename(entries) %in% excluded]
  if (length(entries) == 0L) {
    stop("Package checkout has no copyable content.", call. = FALSE)
  }
  copied <- file.copy(
    entries,
    destination,
    recursive = TRUE,
    copy.mode = TRUE,
    copy.date = TRUE
  )
  if (!all(copied)) {
    stop("Unable to copy the package checkout into run state.", call. = FALSE)
  }
  invisible(destination)
}

stock_adapter_checkout_identity <- function(path, package) {
  description <- file.path(path, "DESCRIPTION")
  if (!utils::file_test("-f", description)) {
    stop("Copied package checkout has no DESCRIPTION.", call. = FALSE)
  }
  record <- read.dcf(description)
  if (
    nrow(record) != 1L ||
      !all(c("Package", "Version") %in% colnames(record)) ||
      !identical(unname(record[1L, "Package"]), package)
  ) {
    stop("Copied package checkout identity is inconsistent.", call. = FALSE)
  }
  manifest <- stock_adapter_directory_snapshot(path)
  manifest <- manifest[
    manifest$relative_path != "revdep" &
      !startsWith(manifest$relative_path, "revdep/"),
    ,
    drop = FALSE
  ]
  rownames(manifest) <- NULL
  list(
    package = package,
    version = validate_package_version(unname(record[1L, "Version"])),
    description_sha256 = digest::digest(
      description,
      algo = "sha256",
      file = TRUE,
      serialize = FALSE
    ),
    manifest = manifest
  )
}

initialize_stock_database <- function(checkout, targets, cache_path) {
  environment <- c(
    CRANCACHE_DISABLE = "yes",
    CRANCACHE_DIR = cache_path
  )
  stock_adapter_with_environment(environment, {
    revdepcheck::dir_setup(checkout)
    stock_namespace_function("db_setup")(checkout)
    stock_namespace_function("db_todo_add")(checkout, targets)
    stock_namespace_function("db_metadata_set")(checkout, "todo", "install")
    stock_namespace_function("db_metadata_set")(checkout, "bioc", "FALSE")
    stock_namespace_function("db_metadata_set")(
      checkout,
      "dependencies",
      paste(stock_runner_first_level_fields(), collapse = ";")
    )
    stock_namespace_function("db_disconnect")(checkout)
  })
  observed <- observe_stock_database(checkout)
  if (
    !identical(observed$stage, "install") ||
      !identical(observed$todo$package, targets) ||
      any(observed$todo$status != "todo") ||
      nrow(observed$old) != 0L ||
      nrow(observed$new) != 0L
  ) {
    stop(
      "Stock discovery database does not match the frozen cohort.",
      call. = FALSE
    )
  }
  list(stage = observed$stage, todo = observed$todo)
}

stock_namespace_function <- function(name) {
  get(name, envir = asNamespace("revdepcheck"), inherits = FALSE)
}

seed_stock_binary_cache <- function(projection, contrib_path) {
  sources <- file.path(
    projection$repository_path,
    "src",
    "contrib",
    projection$manifest$archive_name
  )
  expected <- data.frame(
    package = projection$manifest$package,
    version = projection$manifest$version,
    archive_name = projection$manifest$archive_name,
    sha256 = projection$manifest$sha256,
    stringsAsFactors = FALSE
  )
  seed_stock_cache_repository(sources, expected, contrib_path)
}

seed_stock_source_cache <- function(
  gate,
  baseline,
  contrib_path,
  context
) {
  validate_preparation_gate(gate, context)
  acquisitions <- gate$source_acquisitions
  paths <- vapply(acquisitions, `[[`, character(1L), "warehouse_path")
  packages <- vapply(acquisitions, `[[`, character(1L), "package")
  versions <- vapply(acquisitions, `[[`, character(1L), "version")
  hashes <- vapply(
    acquisitions,
    function(acquisition) {
      acquisition$artifact$sha256
    },
    character(1L)
  )
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
    source <- normalize_stock_regular_file(
      sources[[row]],
      "cache seed artifact"
    )
    source_before <- warehouse_file_snapshot(source)
    destination <- file.path(contrib_path, expected$archive_name[[row]])
    copied <- warehouse_copy_file(
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
    validate_warehouse_source_unchanged(source, source_before)
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
    if (warehouse_path_is_link(path) || !utils::file_test("-f", path)) {
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

stock_adapter_environment <- function(paths, repository_settings) {
  c(
    HOME = paths$home,
    TMPDIR = paths$temp,
    XDG_CACHE_HOME = paths$xdg_cache,
    XDG_CONFIG_HOME = paths$xdg_config,
    XDG_DATA_HOME = paths$xdg_data,
    XDG_STATE_HOME = paths$xdg_state,
    CRANCACHE_DIR = paths$cache,
    CRANCACHE_REPOS = "cran",
    CRANCACHE_DISABLE = "",
    CRANCACHE_DISABLE_UPDATES = "yes",
    CRANCACHE_QUIET = "yes",
    R_BIOC_VERSION = repository_settings[["R_BIOC_VERSION"]],
    R_ENVIRON_USER = file.path(paths$root, "empty-Renviron"),
    R_PROFILE_USER = file.path(paths$root, "empty-Rprofile"),
    PATH = paste(
      paths$compiler_bin,
      Sys.getenv("PATH"),
      sep = .Platform$path.sep
    )
  )
}

create_stock_compiler_wrappers <- function(
  r_executable,
  wrapper_root,
  log_path
) {
  configurations <- c(
    "CC",
    "CXX",
    "CXX11",
    "CXX14",
    "CXX17",
    "CXX20",
    "CXX23",
    "FC",
    "F77",
    "OBJC",
    "OBJCXX"
  )
  tools <- lapply(configurations, stock_adapter_compiler_tool, r_executable)
  tools <- tools[!vapply(tools, is.null, logical(1L))]
  if (length(tools) == 0L) {
    stop(
      "The selected R reports no observable compiler commands.",
      call. = FALSE
    )
  }
  tools <- do.call(rbind, tools)
  tools <- tools[!duplicated(tools$command), , drop = FALSE]
  if (anyDuplicated(basename(tools$command))) {
    stop("Compiler command basenames are ambiguous.", call. = FALSE)
  }
  tools$wrapper <- file.path(wrapper_root, basename(tools$command))
  for (row in seq_len(nrow(tools))) {
    script <- c(
      "#!/bin/sh",
      "set -eu",
      sprintf(
        "printf '%%s' %s >> %s",
        shQuote(tools$command[[row]]),
        shQuote(log_path)
      ),
      sprintf(
        "for revdeprunner_arg do printf '\\t%%s' \"$revdeprunner_arg\" >> %s; done",
        shQuote(log_path)
      ),
      sprintf("printf '\\n' >> %s", shQuote(log_path)),
      sprintf("exec %s \"$@\"", shQuote(tools$executable[[row]]))
    )
    writeLines(script, tools$wrapper[[row]], useBytes = TRUE)
    Sys.chmod(tools$wrapper[[row]], mode = "0755")
    syntax_status <- system2("/bin/sh", c("-n", shQuote(tools$wrapper[[row]])))
    if (syntax_status != 0L) {
      stop(
        "Generated compiler wrapper has invalid POSIX shell syntax.",
        call. = FALSE
      )
    }
  }
  tools$sha256 <- vapply(
    tools$wrapper,
    digest::digest,
    character(1L),
    algo = "sha256",
    file = TRUE,
    serialize = FALSE
  )
  rownames(tools) <- NULL
  tools
}

stock_adapter_compiler_tool <- function(configuration, r_executable) {
  output <- suppressWarnings(system2(
    r_executable,
    c("CMD", "config", configuration),
    stdout = TRUE,
    stderr = TRUE
  ))
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L) {
    return(NULL)
  }
  output <- trimws(paste(output, collapse = " "))
  if (!nzchar(output)) {
    return(NULL)
  }
  command <- strsplit(output, "[[:space:]]+")[[1L]][[1L]]
  if (
    !grepl("^[A-Za-z0-9_+.-]+$", command) ||
      grepl("/", command, fixed = TRUE)
  ) {
    stop("R reports an unsupported compiler command.", call. = FALSE)
  }
  executable <- Sys.which(command)
  if (!nzchar(executable)) {
    stop("A compiler command reported by R is unavailable.", call. = FALSE)
  }
  data.frame(
    configuration = configuration,
    command = command,
    executable = normalizePath(executable, winslash = "/", mustWork = TRUE),
    stringsAsFactors = FALSE
  )
}

observe_stock_runtime <- function(
  r_executable,
  targets,
  runner_supplied,
  environment,
  repository_settings,
  temp_root
) {
  r_executable <- normalize_command_r_executable(r_executable)
  temp_root <- normalize_runtime_anchor(temp_root, "stock runtime temp root")
  files <- c(
    request = tempfile("stock-runtime-request-", tmpdir = temp_root),
    result = tempfile("stock-runtime-result-", tmpdir = temp_root),
    stdout = tempfile("stock-runtime-stdout-", tmpdir = temp_root),
    stderr = tempfile("stock-runtime-stderr-", tmpdir = temp_root)
  )
  on.exit(unlink(files, force = TRUE), add = TRUE)
  if (!all(file.create(files[c("stdout", "stderr")]))) {
    stop("Unable to create stock runtime probe logs.", call. = FALSE)
  }
  saveRDS(
    list(
      packages = names(stock_adapter_versions()),
      internals = stock_adapter_internals(),
      targets = targets,
      runner_supplied = runner_supplied
    ),
    files[["request"]]
  )
  arguments <- c(
    "--vanilla",
    "--slave",
    "-e",
    stock_adapter_runtime_expression(),
    "--args",
    files[["request"]],
    files[["result"]],
    repository_settings[["CRAN"]],
    repository_settings[["BioC_mirror"]]
  )
  process <- stock_adapter_with_environment(environment, {
    run_source_preparation_process(
      r_executable,
      arguments,
      temp_root,
      files[["stdout"]],
      files[["stderr"]],
      120L
    )
  })
  if (
    process$status != 0L || process$timed_out || !file.exists(files[["result"]])
  ) {
    diagnostic <- paste(
      utils::tail(
        c(
          readLines(files[["stdout"]], warn = FALSE),
          readLines(files[["stderr"]], warn = FALSE)
        ),
        10L
      ),
      collapse = " "
    )
    if (!nzchar(diagnostic)) {
      diagnostic <- paste("exit status", process$status)
    }
    stop(
      paste("Selected stock runtime probe failed:", diagnostic),
      call. = FALSE
    )
  }
  observation <- tryCatch(
    readRDS(files[["result"]]),
    error = function(error) {
      stop("Selected stock runtime evidence is unreadable.", call. = FALSE)
    }
  )
  validate_stock_runtime_observation(observation, targets, temp_root)
  observation
}

stock_adapter_runtime_expression <- function() {
  paste(
    "args <- commandArgs(TRUE)",
    "request <- readRDS(args[[1L]])",
    "missing <- request$packages[!vapply(request$packages, requireNamespace, logical(1L), quietly = TRUE)]",
    "if (length(missing) > 0L) stop(paste('missing stock packages:', paste(missing, collapse = ', ')))",
    "namespace <- asNamespace('revdepcheck')",
    "supported <- vapply(request$internals, exists, logical(1L), envir = namespace, inherits = FALSE)",
    "if (any(!supported)) stop('unsupported stock internal API')",
    paste0(
      "provenance <- data.frame(package = request$packages, ",
      "version = vapply(request$packages, function(package) ",
      "as.character(utils::packageVersion(package)), character(1L)), ",
      "remote_sha = vapply(request$packages, function(package) { ",
      "sha <- utils::packageDescription(package)[['RemoteSha']]; ",
      "if (is.null(sha) || length(sha) != 1L || is.na(sha) || ",
      "!nzchar(sha)) NA_character_ else sha }, character(1L)), ",
      "stringsAsFactors = FALSE)"
    ),
    "rownames(provenance) <- NULL",
    "options(repos = c(CRAN = args[[3L]]), BioC_mirror = args[[4L]])",
    paste0(
      "dependencies <- lapply(request$targets, function(target) ",
      "sort(revdepcheck:::deps_opts(target, ",
      "exclude = request$runner_supplied)$package, method = 'radix'))"
    ),
    "names(dependencies) <- request$targets",
    paste0(
      "saveRDS(list(provenance = provenance, dependencies = dependencies, ",
      "tempdir = normalizePath(tempdir(), winslash = '/', mustWork = TRUE)), ",
      "args[[2L]])"
    ),
    sep = "; "
  )
}

validate_stock_runtime_observation <- function(
  observation,
  targets,
  temp_root
) {
  if (
    !is.list(observation) ||
      !identical(
        names(observation),
        c("provenance", "dependencies", "tempdir")
      ) ||
      !identical(observation$provenance, stock_adapter_expected_provenance()) ||
      !is.list(observation$dependencies) ||
      !identical(names(observation$dependencies), targets) ||
      !is.character(observation$tempdir) ||
      length(observation$tempdir) != 1L ||
      is.na(observation$tempdir) ||
      !path_is_within(temp_root, observation$tempdir)
  ) {
    stop("Selected stock runtime evidence is inconsistent.", call. = FALSE)
  }
  normalized <- lapply(observation$dependencies, function(dependencies) {
    if (
      !is.character(dependencies) ||
        anyNA(dependencies) ||
        anyDuplicated(dependencies)
    ) {
      stop("Selected stock dependency evidence is invalid.", call. = FALSE)
    }
    sort(dependencies, method = "radix")
  })
  if (!identical(observation$dependencies, normalized)) {
    stop("Selected stock dependency evidence is not normalized.", call. = FALSE)
  }
  invisible(observation)
}

stock_dependencies_from_observation <- function(observed, targets, universe) {
  rows <- list()
  for (target in targets) {
    expected <- universe$dependencies[
      universe$dependencies$target == target &
        universe$dependencies$disposition == "install",
      c("dependency", "version"),
      drop = FALSE
    ]
    expected <- expected[
      order(expected$dependency, method = "radix"),
      ,
      drop = FALSE
    ]
    rownames(expected) <- NULL
    if (!identical(observed[[target]], expected$dependency)) {
      stop(
        "Stock dependency requests differ from the frozen universe.",
        call. = FALSE
      )
    }
    if (nrow(expected) > 0L) {
      rows[[target]] <- data.frame(
        target = target,
        dependency = expected$dependency,
        version = expected$version,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0L) {
    return(data.frame(
      target = character(),
      dependency = character(),
      version = character(),
      stringsAsFactors = FALSE
    ))
  }
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

stock_adapter_with_environment <- function(environment, code) {
  if (
    !is.character(environment) ||
      is.null(names(environment)) ||
      anyNA(environment) ||
      any(!nzchar(names(environment))) ||
      anyDuplicated(names(environment))
  ) {
    stop("Stock adapter environment is invalid.", call. = FALSE)
  }
  current <- Sys.getenv()
  existed <- names(environment) %in% names(current)
  previous <- current[names(environment)[existed]]
  on.exit(
    {
      missing <- names(environment)[!existed]
      if (length(missing) > 0L) {
        Sys.unsetenv(missing)
      }
      if (length(previous) > 0L) {
        do.call(Sys.setenv, as.list(previous))
      }
    },
    add = TRUE
  )
  do.call(Sys.setenv, as.list(environment))
  force(code)
}

stock_adapter_with_options <- function(settings, code) {
  previous <- options(settings)
  on.exit(options(previous), add = TRUE)
  force(code)
}

stock_adapter_provenance <- function() {
  packages <- names(stock_adapter_versions())
  provenance <- data.frame(
    package = packages,
    version = vapply(
      packages,
      function(package) {
        as.character(utils::packageVersion(package))
      },
      character(1L)
    ),
    remote_sha = vapply(
      packages,
      function(package) {
        description <- utils::packageDescription(package)
        sha <- description[["RemoteSha"]]
        if (is.null(sha) || !nzchar(sha)) NA_character_ else sha
      },
      character(1L)
    ),
    stringsAsFactors = FALSE
  )
  rownames(provenance) <- NULL
  provenance
}

run_stock_revdepcheck_process <- function(
  r_executable,
  paths,
  environment,
  repository_settings,
  worker_timeout_seconds,
  process_timeout_seconds
) {
  expression <- paste(
    "args <- commandArgs(TRUE)",
    "options(repos = c(CRAN = args[[2L]]), BioC_mirror = args[[3L]])",
    paste0(
      "revdepcheck::revdep_check(args[[1L]], quiet = TRUE, ",
      "timeout = as.difftime(as.numeric(args[[4L]]), units = 'secs'), ",
      "num_workers = 1L, bioc = FALSE, cran = FALSE)"
    ),
    sep = "; "
  )
  arguments <- c(
    "--vanilla",
    "--slave",
    "-e",
    expression,
    "--args",
    paths$checkout,
    repository_settings[["CRAN"]],
    repository_settings[["BioC_mirror"]],
    as.character(worker_timeout_seconds)
  )
  stock_adapter_with_environment(environment, {
    run_source_preparation_process(
      r_executable,
      arguments,
      paths$root,
      paths$stdout,
      paths$stderr,
      process_timeout_seconds
    )
  })
}

observe_stock_database <- function(checkout) {
  stage <- stock_namespace_function("db_metadata_get")(checkout, "todo")
  todo <- revdepcheck::revdep_todo(checkout)
  raw <- stock_namespace_function("db_get_results")(checkout, NULL)
  summaries <- suppressMessages(revdepcheck::revdep_summary(checkout))
  stock <- if (length(summaries) == 0L) {
    data.frame(
      package = character(),
      stock_status = character(),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      package = names(summaries),
      stock_status = vapply(
        summaries,
        stock_namespace_function("rcmdcheck_status"),
        character(1L)
      ),
      stringsAsFactors = FALSE
    )
  }
  stock_namespace_function("db_disconnect")(checkout)
  todo <- stock_adapter_normalize_todo(todo)
  old <- stock_adapter_normalize_database_results(raw$old)
  new <- stock_adapter_normalize_database_results(raw$new)
  stock <- stock[order(stock$package, method = "radix"), , drop = FALSE]
  rownames(stock) <- NULL
  list(stage = stage, todo = todo, old = old, new = new, stock = stock)
}

stock_adapter_normalize_todo <- function(todo) {
  if (
    !is.data.frame(todo) ||
      !all(c("package", "status") %in% names(todo))
  ) {
    stop("Stock todo evidence has an invalid structure.", call. = FALSE)
  }
  todo <- todo[c("package", "status")]
  todo[] <- lapply(todo, as.character)
  todo <- todo[order(todo$package, method = "radix"), , drop = FALSE]
  rownames(todo) <- NULL
  todo
}

stock_adapter_normalize_database_results <- function(results) {
  fields <- c("package", "version", "status", "which")
  if (!is.data.frame(results) || !all(fields %in% names(results))) {
    stop(
      "Stock database result evidence has an invalid structure.",
      call. = FALSE
    )
  }
  results <- results[fields]
  results[] <- lapply(results, as.character)
  results <- results[order(results$package, method = "radix"), , drop = FALSE]
  rownames(results) <- NULL
  results
}

stock_adapter_results <- function(initialization, process, database) {
  selected <- initialization$selected_targets
  rows <- lapply(seq_len(nrow(selected)), function(row) {
    target <- selected[row, , drop = FALSE]
    package <- target$package[[1L]]
    if (package %in% initialization$excluded_targets) {
      return(data.frame(
        package = package,
        expected_version = target$version[[1L]],
        role = target$role[[1L]],
        outcome = "not_checked",
        old_version = NA_character_,
        old_status = NA_character_,
        new_version = NA_character_,
        new_status = NA_character_,
        stock_status = NA_character_,
        diagnostic_excerpt = "Owner excluded this target before comparison.",
        stringsAsFactors = FALSE
      ))
    }
    old <- database$old[database$old$package == package, , drop = FALSE]
    new <- database$new[database$new$package == package, , drop = FALSE]
    stock <- database$stock[
      database$stock$package == package,
      ,
      drop = FALSE
    ]
    complete <- nrow(old) == 1L && nrow(new) == 1L && nrow(stock) == 1L
    if (complete) {
      if (
        !identical(old$version[[1L]], target$version[[1L]]) ||
          !identical(new$version[[1L]], target$version[[1L]])
      ) {
        stop(
          "Stock result version differs from the frozen target.",
          call. = FALSE
        )
      }
    }
    if (process$status != 0L || process$timed_out || !complete) {
      stock_status <- if (nrow(stock) == 1L) {
        stock$stock_status[[1L]]
      } else {
        NA_character_
      }
      outcome <- "incomplete"
    } else {
      stock_status <- stock$stock_status[[1L]]
      outcome <- if (identical(stock_status, "+")) {
        "unchanged"
      } else if (identical(stock_status, "-")) {
        "changed"
      } else if (stock_status %in% c("i+", "i-", "t+", "t-", "?")) {
        "incomplete"
      } else {
        stop("Stock comparison returned an unsupported status.", call. = FALSE)
      }
    }
    diagnostic <- if (identical(outcome, "incomplete")) {
      stock_adapter_incomplete_diagnostic(process, old, new, stock_status)
    } else {
      NA_character_
    }
    data.frame(
      package = package,
      expected_version = target$version[[1L]],
      role = target$role[[1L]],
      outcome = outcome,
      old_version = if (nrow(old) == 1L) old$version[[1L]] else NA_character_,
      old_status = if (nrow(old) == 1L) old$status[[1L]] else NA_character_,
      new_version = if (nrow(new) == 1L) new$version[[1L]] else NA_character_,
      new_status = if (nrow(new) == 1L) new$status[[1L]] else NA_character_,
      stock_status = stock_status,
      diagnostic_excerpt = diagnostic,
      stringsAsFactors = FALSE
    )
  })
  results <- do.call(rbind, rows)
  results <- results[order(results$package, method = "radix"), , drop = FALSE]
  rownames(results) <- NULL
  results
}

stock_adapter_incomplete_diagnostic <- function(process, old, new, status) {
  evidence <- c(
    if (process$timed_out) "Stock comparison process timed out." else NULL,
    if (process$status != 0L) {
      paste0(
        "Stock comparison process exited with status ",
        process$status,
        "."
      )
    } else {
      NULL
    },
    if (nrow(old) == 1L) paste0("Old status: ", old$status[[1L]], ".") else
      "Old result is missing.",
    if (nrow(new) == 1L) paste0("New status: ", new$status[[1L]], ".") else
      "New result is missing.",
    if (!is.na(status)) paste0("Stock status: ", status, ".") else NULL
  )
  paste(evidence, collapse = " ")
}

stock_adapter_result_state <- function(process, results) {
  requested <- results$outcome != "not_checked"
  if (process$status != 0L || any(results$outcome[requested] == "incomplete")) {
    "comparison-incomplete"
  } else if (any(results$outcome[requested] == "changed")) {
    "comparison-changes"
  } else {
    "success"
  }
}

observe_stock_private_libraries <- function(initialization, process, database) {
  if (process$status != 0L || !identical(database$stage, "done")) {
    return(empty_stock_private_libraries())
  }
  rows <- list()
  for (target in initialization$requested_targets$package) {
    for (which in c("old", "new")) {
      path <- file.path(
        initialization$paths$checkout,
        "revdep",
        "checks",
        target,
        which,
        "libraries.txt"
      )
      observed <- read_stock_libraries(path)
      private <- observed[basename(observed$library) == target, , drop = FALSE]
      private <- private[private$package != target, , drop = FALSE]
      expected <- initialization$stock_dependencies[
        initialization$stock_dependencies$target == target,
        c("dependency", "version"),
        drop = FALSE
      ]
      expected <- expected[
        order(expected$dependency, method = "radix"),
        ,
        drop = FALSE
      ]
      private <- private[
        order(private$package, method = "radix"),
        ,
        drop = FALSE
      ]
      if (
        !identical(private$package, expected$dependency) ||
          !identical(private$version, expected$version)
      ) {
        stop(
          "Stock private-library versions differ from the frozen universe.",
          call. = FALSE
        )
      }
      if (nrow(private) > 0L) {
        rows[[paste(target, which, sep = "\r")]] <- data.frame(
          target = target,
          which = which,
          package = private$package,
          version = private$version,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (length(rows) == 0L) {
    return(empty_stock_private_libraries())
  }
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

read_stock_libraries <- function(path) {
  path <- normalize_stock_regular_file(path, "stock library evidence")
  lines <- readLines(path, warn = FALSE)
  library <- NA_character_
  rows <- list()
  for (line in lines) {
    if (startsWith(line, "Library: ")) {
      library <- substring(line, 10L)
      next
    }
    match <- regexec("^([^[:space:]]+)[[:space:]]+[(]([^)]+)[)]$", line)
    fields <- regmatches(line, match)[[1L]]
    if (length(fields) == 3L) {
      if (is.na(library)) {
        stop("Stock library evidence has no owning library.", call. = FALSE)
      }
      rows[[length(rows) + 1L]] <- data.frame(
        library = library,
        package = fields[[2L]],
        version = fields[[3L]],
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0L) {
    return(data.frame(
      library = character(),
      package = character(),
      version = character(),
      stringsAsFactors = FALSE
    ))
  }
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

empty_stock_private_libraries <- function() {
  data.frame(
    target = character(),
    which = character(),
    package = character(),
    version = character(),
    stringsAsFactors = FALSE
  )
}

stock_adapter_compiler_evidence <- function(path) {
  path <- normalize_stock_regular_file(path, "compiler log")
  lines <- readLines(path, warn = FALSE)
  compilation <- grepl("(?:^|\\t)-c(?:\\t|$)", lines, perl = TRUE)
  probes <- stock_adapter_compiler_probes(lines, compilation)
  list(
    invocation_count = length(lines),
    probe_count = sum(probes),
    compilation_count = sum(compilation & !probes),
    invocations = lines,
    sha256 = digest::digest(
      path,
      algo = "sha256",
      file = TRUE,
      serialize = FALSE
    )
  )
}

stock_adapter_compiler_probes <- function(lines, compilation) {
  probes <- rep(FALSE, length(lines))
  candidates <- which(compilation)
  for (index in candidates) {
    if (index == 1L || index == length(lines)) {
      next
    }
    probe_compile <- grepl(
      "(?:^|\\t)foo[.]c(?:\\t|$)",
      lines[[index]],
      perl = TRUE
    ) &&
      grepl("(?:^|\\t)foo[.]o(?:\\t|$)", lines[[index]], perl = TRUE)
    version_probe <- grepl(
      "(?:^|\\t)--version(?:\\t|$)",
      lines[[index - 1L]],
      perl = TRUE
    )
    probe_link <- grepl(
      "(?:^|\\t)-shared(?:\\t|$)",
      lines[[index + 1L]],
      perl = TRUE
    ) &&
      grepl("(?:^|\\t)foo[.]so(?:\\t|$)", lines[[index + 1L]], perl = TRUE) &&
      grepl("(?:^|\\t)foo[.]o(?:\\t|$)", lines[[index + 1L]], perl = TRUE)
    probes[[index]] <- probe_compile && version_probe && probe_link
  }
  probes
}

stock_adapter_process_logs <- function(paths, path_plan) {
  run_root <- runtime_role_path(path_plan, "run")
  records <- lapply(
    c(stdout = paths$stdout, stderr = paths$stderr),
    function(path) {
      path <- validate_source_preparation_log(path, run_root)
      list(
        path = source_preparation_relative_path(path, run_root),
        sha256 = digest::digest(
          path,
          algo = "sha256",
          file = TRUE,
          serialize = FALSE
        )
      )
    }
  )
  records
}

stock_adapter_directory_snapshot <- function(root) {
  root <- normalize_runtime_anchor(root, "snapshot root")
  paths <- list.files(
    root,
    all.files = TRUE,
    full.names = TRUE,
    recursive = TRUE,
    include.dirs = FALSE,
    no.. = TRUE
  )
  if (length(paths) == 0L) {
    return(data.frame(
      relative_path = character(),
      size_bytes = numeric(),
      sha256 = character(),
      stringsAsFactors = FALSE
    ))
  }
  if (any(vapply(paths, warehouse_path_is_link, logical(1L)))) {
    stop("Snapshot roots must not contain symbolic links.", call. = FALSE)
  }
  info <- file.info(paths, extra_cols = FALSE)
  if (anyNA(info$isdir) || any(info$isdir)) {
    stop("Snapshot roots contain an unreadable entry.", call. = FALSE)
  }
  relative <- substring(paths, nchar(sub("/$", "", root)) + 2L)
  snapshot <- data.frame(
    relative_path = relative,
    size_bytes = unname(info$size),
    sha256 = vapply(
      paths,
      function(path) {
        digest::digest(path, algo = "sha256", file = TRUE, serialize = FALSE)
      },
      character(1L)
    ),
    stringsAsFactors = FALSE
  )
  snapshot <- snapshot[
    order(snapshot$relative_path, method = "radix"),
    ,
    drop = FALSE
  ]
  rownames(snapshot) <- NULL
  snapshot
}

stock_adapter_cache_snapshot <- function(paths) {
  repositories <- c(
    "cran-bin",
    "cran",
    "bioc-bin",
    "bioc",
    "other-bin",
    "other"
  )
  snapshots <- lapply(repositories, function(repository) {
    root <- file.path(paths$cache, repository)
    if (!dir.exists(root)) {
      stop("Stock cache repository evidence is incomplete.", call. = FALSE)
    }
    snapshot <- stock_adapter_directory_snapshot(root)
    snapshot$relative_path <- file.path(
      "cache",
      repository,
      snapshot$relative_path
    )
    snapshot
  })
  fallback <- stock_adapter_directory_snapshot(paths$empty_repos)
  fallback$relative_path <- file.path(
    "fallback",
    fallback$relative_path
  )
  snapshots[[length(snapshots) + 1L]] <- fallback
  result <- do.call(rbind, snapshots)
  result <- result[
    order(result$relative_path, method = "radix"),
    ,
    drop = FALSE
  ]
  rownames(result) <- NULL
  result
}

validate_stock_revdepcheck_initialization <- function(
  initialization,
  context,
  require_pre_worker = TRUE
) {
  fields <- c(
    "command_plan",
    "repository_preparation",
    "package",
    "baseline",
    "candidate",
    "selected_targets",
    "requested_targets",
    "excluded_targets",
    "paths",
    "discovery",
    "binary_manifest",
    "source_manifest",
    "stock_dependencies",
    "compiler_tools",
    "environment",
    "repository_settings",
    "projection_before",
    "cache_before",
    "provenance"
  )
  if (
    !inherits(initialization, "revdeprunner_stock_initialization") ||
      !is.list(initialization) ||
      !identical(names(initialization), fields)
  ) {
    stop(
      "Stock adapter initialization has an invalid structure.",
      call. = FALSE
    )
  }
  if (
    !is.logical(require_pre_worker) ||
      length(require_pre_worker) != 1L ||
      is.na(require_pre_worker)
  ) {
    stop("Stock initialization validation mode is invalid.", call. = FALSE)
  }
  validate_repository_preparation(
    initialization$repository_preparation,
    context
  )
  validate_command_plan(
    initialization$command_plan,
    context$path_plan,
    context$snapshot,
    context$cohort,
    context$universe,
    context$lane,
    initialization$repository_preparation$report
  )
  if (
    !identical(initialization$command_plan$operation, "compare") ||
      !identical(initialization$command_plan$dry_run, "false") ||
      !identical(initialization$package, context$cohort$package) ||
      !identical(initialization$selected_targets, context$universe$targets)
  ) {
    stop("Stock adapter contract bindings are inconsistent.", call. = FALSE)
  }
  exclusions <- normalize_stock_adapter_exclusions(
    initialization$excluded_targets,
    initialization$selected_targets$package
  )
  requested <- initialization$selected_targets[
    !initialization$selected_targets$package %in% exclusions,
    ,
    drop = FALSE
  ]
  rownames(requested) <- NULL
  if (
    nrow(requested) == 0L ||
      !identical(initialization$excluded_targets, exclusions) ||
      !identical(initialization$requested_targets, requested)
  ) {
    stop(
      "Stock requested and excluded targets are inconsistent.",
      call. = FALSE
    )
  }
  require_ready_stock_targets(
    initialization$repository_preparation$report,
    initialization$requested_targets
  )
  baseline <- validate_stock_baseline_source(
    initialization$baseline$path,
    context$cohort,
    context$snapshot
  )
  if (!identical(initialization$baseline, baseline)) {
    stop("Stock baseline source evidence changed.", call. = FALSE)
  }
  candidate <- stock_adapter_checkout_identity(
    initialization$paths$checkout,
    initialization$package
  )
  if (!identical(initialization$candidate, candidate)) {
    stop("Stock candidate checkout identity changed.", call. = FALSE)
  }
  validate_stock_adapter_paths(initialization$paths, context$path_plan)
  validate_stock_cache_repository(
    initialization$paths$binary_contrib,
    initialization$binary_manifest
  )
  validate_stock_cache_repository(
    initialization$paths$source_contrib,
    initialization$source_manifest
  )
  projection <- stock_adapter_directory_snapshot(
    initialization$repository_preparation$projection$repository_path
  )
  if (!identical(initialization$projection_before, projection)) {
    stop("Stock projection evidence changed before comparison.", call. = FALSE)
  }
  cache <- stock_adapter_cache_snapshot(initialization$paths)
  if (!identical(initialization$cache_before, cache)) {
    stop(
      "Stock cache artifact evidence changed before comparison.",
      call. = FALSE
    )
  }
  validate_stock_compiler_tools(
    initialization$compiler_tools,
    initialization$paths
  )
  if (
    !identical(
      initialization$environment,
      stock_adapter_environment(
        initialization$paths,
        initialization$repository_settings
      )
    )
  ) {
    stop("Stock adapter environment is inconsistent.", call. = FALSE)
  }
  validate_stock_repository_settings(
    initialization$repository_settings,
    initialization$paths
  )
  validate_stock_dependencies(
    initialization$stock_dependencies,
    context$universe,
    initialization$requested_targets$package
  )
  runtime <- observe_stock_runtime(
    initialization$command_plan$r_executable,
    initialization$requested_targets$package,
    initialization$package,
    initialization$environment,
    initialization$repository_settings,
    initialization$paths$temp
  )
  observed_dependencies <- stock_dependencies_from_observation(
    runtime$dependencies,
    initialization$requested_targets$package,
    context$universe
  )
  if (!identical(initialization$stock_dependencies, observed_dependencies)) {
    stop("Stock dependency requests changed.", call. = FALSE)
  }
  if (!identical(initialization$provenance, runtime$provenance)) {
    stop("Stock tool provenance changed.", call. = FALSE)
  }
  if (require_pre_worker) {
    database <- observe_stock_database(initialization$paths$checkout)
    if (
      !identical(database$stage, "install") ||
        !identical(database$todo, initialization$discovery$todo) ||
        any(database$todo$status != "todo") ||
        nrow(database$old) != 0L ||
        nrow(database$new) != 0L ||
        nrow(database$stock) != 0L
    ) {
      stop("Stock database changed before worker launch.", call. = FALSE)
    }
  }
  invisible(initialization)
}

validate_stock_adapter_paths <- function(paths, path_plan) {
  required <- c(
    "checkout",
    "cache",
    "home",
    "temp",
    "xdg_cache",
    "xdg_config",
    "xdg_data",
    "xdg_state",
    "logs",
    "compiler_bin",
    "empty_repos",
    "root",
    "binary_contrib",
    "source_contrib",
    "stdout",
    "stderr",
    "compiler_log"
  )
  if (!is.list(paths) || !identical(names(paths), required)) {
    stop("Stock adapter paths have an invalid structure.", call. = FALSE)
  }
  run_root <- runtime_role_path(path_plan, "run")
  for (name in required) {
    path <- paths[[name]]
    if (
      !is.character(path) ||
        length(path) != 1L ||
        is.na(path) ||
        !nzchar(path) ||
        warehouse_path_is_link(path) ||
        !path_is_within(
          run_root,
          normalizePath(path, winslash = "/", mustWork = TRUE)
        )
    ) {
      stop("Stock adapter path escapes its run root.", call. = FALSE)
    }
  }
  directories <- setdiff(required, c("stdout", "stderr", "compiler_log"))
  if (any(!vapply(paths[directories], dir.exists, logical(1L)))) {
    stop("Stock adapter directory evidence is incomplete.", call. = FALSE)
  }
  for (name in c("stdout", "stderr", "compiler_log")) {
    normalize_stock_regular_file(paths[[name]], paste("stock", name))
  }
  invisible(paths)
}

validate_stock_compiler_tools <- function(tools, paths) {
  fields <- c(
    "configuration",
    "command",
    "executable",
    "wrapper",
    "sha256"
  )
  if (
    !is.data.frame(tools) ||
      !identical(names(tools), fields) ||
      nrow(tools) == 0L ||
      anyNA(tools) ||
      anyDuplicated(tools$command) ||
      anyDuplicated(tools$wrapper)
  ) {
    stop("Stock compiler evidence has an invalid structure.", call. = FALSE)
  }
  for (row in seq_len(nrow(tools))) {
    normalize_stock_regular_file(tools$executable[[row]], "real compiler")
    wrapper <- normalize_stock_regular_file(
      tools$wrapper[[row]],
      "compiler wrapper"
    )
    if (
      !path_is_within(paths$compiler_bin, wrapper) ||
        !utils::file_test("-x", wrapper) ||
        !identical(
          digest::digest(
            wrapper,
            algo = "sha256",
            file = TRUE,
            serialize = FALSE
          ),
          tools$sha256[[row]]
        )
    ) {
      stop("Stock compiler wrapper is unavailable.", call. = FALSE)
    }
  }
  invisible(tools)
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

validate_stock_dependencies <- function(dependencies, universe, targets) {
  fields <- c("target", "dependency", "version")
  if (
    !is.data.frame(dependencies) ||
      !identical(names(dependencies), fields) ||
      anyNA(dependencies) ||
      anyDuplicated(dependencies[c("target", "dependency")])
  ) {
    stop("Stock dependency evidence has an invalid structure.", call. = FALSE)
  }
  expected <- universe$dependencies[
    universe$dependencies$target %in%
      targets &
      universe$dependencies$disposition == "install",
    c("target", "dependency", "version"),
    drop = FALSE
  ]
  expected <- expected[
    order(expected$target, expected$dependency, method = "radix"),
    ,
    drop = FALSE
  ]
  rownames(expected) <- NULL
  if (!identical(dependencies, expected)) {
    stop(
      "Stock dependency evidence differs from the frozen universe.",
      call. = FALSE
    )
  }
  invisible(dependencies)
}

validate_stock_revdepcheck_result <- function(result, context) {
  fields <- c(
    "initialization",
    "state",
    "process",
    "logs",
    "compiler",
    "database",
    "results",
    "private_libraries",
    "projection_after",
    "cache_after"
  )
  if (
    !inherits(result, "revdeprunner_stock_result") ||
      !is.list(result) ||
      !identical(names(result), fields)
  ) {
    stop("Stock comparison result has an invalid structure.", call. = FALSE)
  }
  validate_stock_revdepcheck_initialization(
    result$initialization,
    context,
    require_pre_worker = FALSE
  )
  validate_source_preparation_process(result$process)
  expected_results <- stock_adapter_results(
    result$initialization,
    result$process,
    result$database
  )
  if (!identical(result$results, expected_results)) {
    stop("Stock target result evidence changed.", call. = FALSE)
  }
  if (
    !identical(
      result$state,
      stock_adapter_result_state(result$process, result$results)
    )
  ) {
    stop("Stock comparison state is inconsistent.", call. = FALSE)
  }
  if (
    !identical(
      result$projection_after,
      result$initialization$projection_before
    ) ||
      !identical(result$cache_after, result$initialization$cache_before)
  ) {
    stop("Stock comparison input snapshots are inconsistent.", call. = FALSE)
  }
  validate_stock_result_logs(
    result$logs,
    result$initialization,
    context$path_plan
  )
  validate_stock_compiler_evidence(result$compiler, result$initialization$paths)
  validate_stock_result_database(
    result$database,
    result$process,
    result$initialization
  )
  validate_stock_result_rows(result$results, result$initialization)
  if (!is.data.frame(result$private_libraries)) {
    stop(
      "Stock private-library evidence has an invalid structure.",
      call. = FALSE
    )
  }
  invisible(result)
}

validate_stock_result_logs <- function(logs, initialization, path_plan) {
  if (!is.list(logs) || !identical(names(logs), c("stdout", "stderr"))) {
    stop("Stock process log evidence has an invalid structure.", call. = FALSE)
  }
  run_root <- runtime_role_path(path_plan, "run")
  for (name in names(logs)) {
    record <- logs[[name]]
    if (!is.list(record) || !identical(names(record), c("path", "sha256"))) {
      stop("Stock process log record has an invalid structure.", call. = FALSE)
    }
    validate_sha256(record$sha256, paste(name, "log sha256"))
    path <- file.path(run_root, record$path)
    expected <- initialization$paths[[name]]
    if (
      !identical(
        normalizePath(path, winslash = "/", mustWork = TRUE),
        expected
      ) ||
        !identical(
          digest::digest(path, algo = "sha256", file = TRUE, serialize = FALSE),
          record$sha256
        )
    ) {
      stop("Stock process log evidence changed.", call. = FALSE)
    }
  }
  invisible(logs)
}

validate_stock_compiler_evidence <- function(compiler, paths) {
  if (
    !is.list(compiler) ||
      !identical(
        names(compiler),
        c(
          "invocation_count",
          "probe_count",
          "compilation_count",
          "invocations",
          "sha256"
        )
      ) ||
      !is.numeric(compiler$invocation_count) ||
      length(compiler$invocation_count) != 1L ||
      compiler$invocation_count < 0L ||
      compiler$invocation_count != floor(compiler$invocation_count) ||
      !is.numeric(compiler$probe_count) ||
      length(compiler$probe_count) != 1L ||
      compiler$probe_count < 0L ||
      compiler$probe_count != floor(compiler$probe_count) ||
      !is.numeric(compiler$compilation_count) ||
      length(compiler$compilation_count) != 1L ||
      compiler$compilation_count < 0L ||
      compiler$compilation_count != floor(compiler$compilation_count) ||
      compiler$probe_count > compiler$invocation_count ||
      compiler$compilation_count > compiler$invocation_count ||
      !is.character(compiler$invocations) ||
      length(compiler$invocations) != compiler$invocation_count
  ) {
    stop("Stock compiler result has an invalid structure.", call. = FALSE)
  }
  validate_sha256(compiler$sha256, "compiler log sha256")
  observed <- stock_adapter_compiler_evidence(paths$compiler_log)
  if (!identical(compiler, observed)) {
    stop("Stock compiler evidence changed.", call. = FALSE)
  }
  invisible(compiler)
}

validate_stock_result_database <- function(database, process, initialization) {
  if (
    !is.list(database) ||
      !identical(names(database), c("stage", "todo", "old", "new", "stock"))
  ) {
    stop("Stock database evidence has an invalid structure.", call. = FALSE)
  }
  requested <- initialization$requested_targets$package
  if (
    !identical(database$todo$package, requested) ||
      any(!database$old$package %in% requested) ||
      any(!database$new$package %in% requested) ||
      any(!database$stock$package %in% requested)
  ) {
    stop("Stock database target coverage is inconsistent.", call. = FALSE)
  }
  if (process$status == 0L) {
    if (
      !identical(database$stage, "done") ||
        any(database$todo$status != "done") ||
        !identical(database$old$package, requested) ||
        !identical(database$new$package, requested) ||
        !identical(database$stock$package, requested)
    ) {
      stop("Completed stock database evidence is incomplete.", call. = FALSE)
    }
  }
  invisible(database)
}

validate_stock_result_rows <- function(results, initialization) {
  fields <- c(
    "package",
    "expected_version",
    "role",
    "outcome",
    "old_version",
    "old_status",
    "new_version",
    "new_status",
    "stock_status",
    "diagnostic_excerpt"
  )
  expected <- initialization$selected_targets[
    order(initialization$selected_targets$package, method = "radix"),
    ,
    drop = FALSE
  ]
  if (
    !is.data.frame(results) ||
      !identical(names(results), fields) ||
      !identical(results$package, expected$package) ||
      !identical(results$expected_version, expected$version) ||
      !identical(results$role, expected$role) ||
      any(
        !results$outcome %in%
          c("unchanged", "changed", "incomplete", "not_checked")
      )
  ) {
    stop("Stock target result rows are inconsistent.", call. = FALSE)
  }
  excluded <- results$package %in% initialization$excluded_targets
  if (
    any(results$outcome[excluded] != "not_checked") ||
      any(results$outcome[!excluded] == "not_checked") ||
      any(!is.na(results$stock_status[excluded])) ||
      any(is.na(results$diagnostic_excerpt[
        results$outcome %in% c("incomplete", "not_checked")
      ])) ||
      any(
        !is.na(results$diagnostic_excerpt[
          results$outcome %in% c("unchanged", "changed")
        ])
      )
  ) {
    stop("Stock target outcome evidence is inconsistent.", call. = FALSE)
  }
  invisible(results)
}

# nolint end
