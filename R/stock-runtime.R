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
    "db",
    "db_get_results",
    "db_metadata_get",
    "db_metadata_set",
    "db_setup",
    "db_todo_add",
    "deps_opts",
    "dir_setup",
    "dir_find",
    "revdep_install",
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

stock_adapter_worker_timeout <- function(
  initialization,
  worker_timeout_seconds,
  compiled_packages,
  verbose = TRUE
) {
  requested <- initialization$requested_targets$package
  attempts <- initialization$preparation_report$attempts
  builds <- attempts[
    attempts$package %in%
      requested &
      attempts$stage == "build" &
      attempts$outcome == "success",
    ,
    drop = FALSE
  ]

  seconds <- 600L
  reason <- "automatic fallback; no successful requested-target build timing"
  if (nrow(builds)) {
    durations <- as.numeric(builds$duration_ms) / 1000
    longest <- which.max(durations)
    seconds <- as.integer(max(
      seconds,
      300 * ceiling(2 * durations[[longest]] / 300)
    ))
    reason <- sprintf(
      "automatic; %s preparation build took %.1f seconds",
      builds$package[[longest]],
      durations[[longest]]
    )
  }
  untimed <- setdiff(intersect(requested, compiled_packages), builds$package)
  if (length(untimed) && seconds < 1800L) {
    seconds <- 1800L
    reason <- sprintf(
      "automatic fallback; compiled target %s has no successful build timing",
      untimed[[1L]]
    )
  }

  if (!is.null(worker_timeout_seconds)) {
    worker_timeout_seconds <- normalize_source_preparation_timeout(
      worker_timeout_seconds
    )
    reason <- if (worker_timeout_seconds < seconds) {
      sprintf(
        "explicit; %s; automatic recommendation: %d seconds",
        reason,
        seconds
      )
    } else {
      "explicit"
    }
    seconds <- worker_timeout_seconds
  }
  revdep_progress(
    verbose,
    "Stock worker timeout: %d seconds (%s).",
    seconds,
    reason
  )
  seconds
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

stock_namespace_function <- function(name) {
  get(name, envir = asNamespace("revdepcheck"), inherits = FALSE)
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
    R_PROFILE_USER = file.path(paths$root, "empty-Rprofile")
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
  r_executable <- normalize_r_executable(r_executable)
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

stock_target_dependencies <- function(
  universe,
  snapshot,
  targets,
  target_repos = NULL
) {
  dependencies <- universe$dependencies
  if (nrow(universe$candidate_dependencies)) {
    dependencies <- discover_dependency_universe(
      universe$targets,
      snapshot$packages,
      universe$runner_supplied,
      universe$base_packages,
      snapshot$repositories,
      target_repos = target_repos
    )$dependencies
  }
  expected <- dependencies[
    dependencies$disposition == "install" & dependencies$target %in% targets,
    c("target", "dependency", "version"),
    drop = FALSE
  ]
  rownames(expected) <- NULL
  expected
}

stock_dependencies_from_observation <- function(
  observed,
  targets,
  universe,
  snapshot,
  target_repos = NULL
) {
  dependencies <- stock_target_dependencies(
    universe,
    snapshot,
    targets,
    target_repos
  )
  rows <- list()
  for (target in targets) {
    expected <- dependencies[
      dependencies$target == target,
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
  process_timeout_seconds,
  verbose = FALSE
) {
  expression <- paste(
    "args <- commandArgs(TRUE)",
    "options(repos = c(CRAN = args[[2L]]), BioC_mirror = args[[3L]])",
    "env <- revdepcheck::revdep_env_vars()",
    paste0(
      "ns <- asNamespace('revdepcheck'); ",
      # A verbose progress read can overlap a commit in this writer process.
      # SQLite's busy timeout belongs to the connection that performs the write.
      "invisible(DBI::dbExecute(get('db', ns)(args[[1L]]), 'PRAGMA busy_timeout = 5000')); ",
      "if (identical(get('db_metadata_get', ns)(args[[1L]], 'todo'), 'install')) ",
      "{ get('revdep_install', ns)(args[[1L]], quiet = FALSE, env = env, bioc = FALSE, cran = FALSE); ",
      # On Apple Silicon the pinned runner does not turn install warnings into
      # errors. Keep a failed subject install resumable and out of comparisons.
      "subject <- read.dcf(file.path(args[[1L]], 'DESCRIPTION'), fields = 'Package')[[1L]]; ",
      "for (side in c('old', 'new')) { ",
      "description <- file.path(get('dir_find', ns)(args[[1L]], side), subject, 'DESCRIPTION'); ",
      "if (!file.exists(description)) { ",
      "get('db_metadata_set', ns)(args[[1L]], 'todo', 'install'); ",
      "stop(paste('Stock subject installation failed:', side, subject)) } } }"
    ),
    paste0(
      "revdepcheck::revdep_check(args[[1L]], quiet = TRUE, ",
      "timeout = as.difftime(as.numeric(args[[4L]]), units = 'secs'), ",
      "num_workers = 1L, bioc = FALSE, cran = FALSE, env = env)"
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
    call <- list(
      r_executable,
      arguments,
      paths$root,
      paths$stdout,
      paths$stderr,
      process_timeout_seconds
    )
    if (verbose) {
      call$on_tick <- stock_comparison_progress(paths$checkout)
    }
    do.call(run_source_preparation_process, call)
  })
}

validate_stock_dependencies <- function(
  dependencies,
  universe,
  targets,
  snapshot,
  target_repos = NULL
) {
  fields <- c("target", "dependency", "version")
  if (
    !is.data.frame(dependencies) ||
      !identical(names(dependencies), fields) ||
      anyNA(dependencies) ||
      anyDuplicated(dependencies[c("target", "dependency")])
  ) {
    stop("Stock dependency evidence has an invalid structure.", call. = FALSE)
  }
  expected <- stock_target_dependencies(
    universe,
    snapshot,
    targets,
    target_repos
  )
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

stock_worker_makevars <- function(root) {
  source <- Sys.getenv("R_MAKEVARS_USER")
  if (!nzchar(source)) {
    return(NULL)
  }
  source <- normalizePath(source, mustWork = TRUE)
  copy <- tempfile("Makevars-", tmpdir = root)
  if (!file.copy(source, copy)) {
    stop("Unable to retain the worker Makevars override.", call. = FALSE)
  }
  list(source = source, copy = copy, contents = readLines(copy, warn = FALSE))
}

validate_prior_check_timings <- function(timings) {
  fields <- c("package", "side", "version", "seconds")
  if (
    !is.data.frame(timings) ||
      !all(fields %in% names(timings)) ||
      !is.character(timings$package) ||
      !is.character(timings$version) ||
      !is.character(timings$side) ||
      !is.numeric(timings$seconds) ||
      anyNA(timings[fields]) ||
      any(!timings$side %in% c("old", "new")) ||
      any(!is.finite(timings$seconds) | timings$seconds < 0) ||
      anyDuplicated(timings[c("package", "side", "version")])
  ) {
    stop(
      "`prior_timings` needs one duration in seconds per package, version and side.",
      call. = FALSE
    )
  }
  invisible(timings)
}

stock_check_budget <- function(
  timings,
  targets,
  pending,
  process_timeout_seconds,
  verbose
) {
  if (is.null(timings) || !nrow(timings)) {
    return(invisible(NULL))
  }
  targets <- targets[targets$package %in% pending, , drop = FALSE]
  version <- targets$version[match(timings$package, targets$package)]
  observed <- timings[which(timings$version == version), , drop = FALSE]
  if (!nrow(observed)) {
    return(invisible(NULL))
  }
  seconds <- sum(observed$seconds)
  longest <- observed[which.max(observed$seconds), ]
  description <- sprintf(
    "Prior checks cover %d/%d pending sides: %.2f cumulative check-hours; longest %s %s, %.1f min.",
    nrow(observed),
    2L * nrow(targets),
    seconds / 3600,
    longest$package,
    longest$side,
    longest$seconds / 60
  )
  if (seconds >= process_timeout_seconds) {
    warning(
      description,
      sprintf(
        " The one-worker process budget is %.2f hours including setup. Consider increasing `process_timeout_seconds`.",
        process_timeout_seconds / 3600
      ),
      call. = FALSE
    )
  } else {
    revdep_progress(verbose, "%s", description)
  }
  invisible(NULL)
}
