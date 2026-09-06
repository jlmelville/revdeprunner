#' Prepare reverse dependencies
#'
#' Freeze and prepare the packages needed for a reverse-dependency comparison.
#' Preparation stops before any old/new checks begin. If a package cannot be
#' prepared, the call returns normally with actionable entries in `problems`.
#' Repeat the same call after resolving external prerequisites to resume the
#' frozen preparation.
#'
#' @param package A development package checkout or an existing
#'   [revdep_plan()] object.
#' @inheritParams revdep_plan
#' @param timeout_seconds Positive whole seconds allowed for each preparation
#'   subprocess (building, installing, or checking loadability). Increasing this
#'   budget retains compatible completed preparation. Defaults to 1800.
#' @param verbose Show phase, package, and completion progress. Defaults to `FALSE`.
#'
#' @return A `revdep_prepared` object with `summary`, `problems`, `plan`, and
#'   `evidence`. Raw preparation log paths are retained in `problems` and the
#'   complete private preparation report is available as advanced evidence.
#'   `evidence$report` is `NULL` if setup failed before a package report was saved.
#'
#' @details
#' The durable data directory defaults to
#' `tools::R_user_dir("revdeprunner", "data")`, and disposable run state
#' defaults to `tools::R_user_dir("revdeprunner", "cache")`. Set
#' `REVDEP_RUNNER_DATA` or `REVDEP_RUNNER_RUNS` to override them. Supplying a
#' plan freezes its selected targets; planning arguments cannot also be
#' supplied. A checkout call reuses its first matching frozen plan. Create and
#' pass a new [revdep_plan()] when a refreshed repository snapshot is wanted.
#' Repository-unavailable `Suggests` remain visible in the plan but do not
#' block preparation, matching stock checks with forced Suggests disabled.
#' Available suggestions are prepared; failures remain checking prerequisites
#' without blocking installation of their suggestors. Candidate hard dependency
#' declarations and constraints must still match the plan.
#'
#' Successful binaries and package checkpoints survive removal of working
#' libraries and historical logs. Readiness includes a fresh-process namespace
#' load check in the current environment; this does not exercise every compiled
#' function or detect every system change.
#'
#' This workflow currently supports Linux. It installs trusted package code in
#' isolated libraries but is not an operating-system security sandbox.
#'
#' @examples
#' \dontrun{
#' prepared <- revdep_prepare("/path/to/package", verbose = TRUE)
#' prepared$problems
#'
#' plan <- revdep_plan(
#'   "/path/to/package",
#'   recursive = TRUE,
#'   max_recursive = 20
#' )
#' prepared <- revdep_prepare(plan)
#' }
#' @export
revdep_prepare <- function(
  package,
  recursive = FALSE,
  max_recursive = NULL,
  sample_seed = NULL,
  cache = NULL,
  repos = NULL,
  timeout_seconds = 1800L,
  verbose = FALSE
) {
  require_linux_revdep_runner()
  verbose <- revdep_verbose(verbose)
  revdep_progress(verbose, "Planning preparation.")
  timeout_seconds <- revdep_execution_timeout(
    timeout_seconds,
    "timeout_seconds"
  )
  supplied_plan <- inherits(package, "revdep_plan")
  if (
    supplied_plan &&
      (!missing(recursive) ||
        !missing(max_recursive) ||
        !missing(sample_seed) ||
        !missing(cache) ||
        !missing(repos))
  ) {
    stop(
      "A supplied `revdep_plan` cannot be combined with planning arguments.",
      call. = FALSE
    )
  }
  if (supplied_plan) {
    plan <- validate_public_revdep_plan(package)
  }
  storage <- revdep_runtime_storage()

  request <- if (supplied_plan) {
    revdep_prepare_plan_request(plan, storage)
  } else {
    revdep_prepare_checkout_request(
      package,
      recursive,
      max_recursive,
      sample_seed,
      cache,
      repos,
      storage
    )
  }

  legacy_checkpoint <- file.path(
    dirname(request$checkpoint),
    sub(
      "^prepare-v5-",
      "prepare-v4-",
      basename(request$checkpoint)
    )
  )
  if (!file.exists(request$checkpoint) && file.exists(legacy_checkpoint)) {
    stop(
      paste(
        "The saved preparation checkpoint uses an unsupported version.",
        "Remove it and create a fresh preparation with `revdep_prepare()`:",
        legacy_checkpoint
      ),
      call. = FALSE
    )
  }

  if (file.exists(request$checkpoint)) {
    state <- read_revdep_checkpoint(request$checkpoint, "preparation")
    validate_revdep_prepare_state(state, request$id)
  } else {
    if (!supplied_plan) {
      plan <- revdep_plan(
        request$package_root,
        recursive = recursive,
        max_recursive = max_recursive,
        sample_seed = sample_seed,
        cache = cache,
        repos = request$repositories
      )
      validate_public_revdep_plan(plan)
    }
    state <- new_revdep_prepare_state(plan, request, storage)
    write_revdep_checkpoint(state, request$checkpoint)
  }

  state <- admit_revdep_preparation_environment(state)
  validate_revdep_candidate_requirements(state)
  state["problem"] <- list(NULL)
  tryCatch(
    {
      if (is.null(state$baseline)) {
        state$baseline <- acquire_revdep_baseline(
          state$context$cohort,
          state$context$snapshot,
          storage$data,
          state$context$path_plan
        )
        write_revdep_checkpoint(state, request$checkpoint)
      }
      gate <- do.call(
        prepare_dependency_universe,
        c(
          state$context,
          list(
            baseline_source = state$baseline$path,
            previous = state$gate,
            timeout_seconds = timeout_seconds,
            verbose = verbose,
            checkpoint = function(gate) {
              state$gate <<- gate
              write_revdep_checkpoint(state, request$checkpoint)
            }
          )
        )
      )
      state$gate <- check_preparation_loadability(
        gate,
        state$context,
        timeout_seconds,
        verbose = verbose,
        checkpoint = function(gate) {
          state$gate <<- gate
          write_revdep_checkpoint(state, request$checkpoint)
        }
      )
    },
    revdeprunner_preparation_failure = function(error) {
      state$problem <<- error$problem
    }
  )
  write_revdep_checkpoint(state, request$checkpoint)
  prepared <- new_revdep_prepared(state, request$checkpoint)
  revdep_progress(
    verbose,
    if (nrow(prepared$problems))
      "Preparation incomplete: %d/%d packages ready; see prepared$problems." else
      "Preparation complete: %d/%d packages ready.",
    prepared$summary$prepared,
    prepared$summary$requirements
  )
  prepared
}

#' Run reverse-dependency comparisons
#'
#' Verify a completed preparation and compare the frozen CRAN baseline with the
#' current development checkout using the package's guarded stock
#' `revdepcheck` adapter.
#'
#' @param prepared A ready object returned by [revdep_prepare()].
#' @param repeat_checks Repeat both baseline and candidate checks, including
#'   completed comparisons. Use after repairing external libraries. Compatible
#'   prepared binaries are retained. Defaults to `FALSE`, which resumes unfinished
#'   targets and reuses completed changed and unchanged comparisons.
#' @param worker_timeout_seconds Positive whole seconds allowed per stock check
#'   worker, or `NULL` to derive a budget from preparation evidence.
#' @param process_timeout_seconds Positive whole seconds allowed for the entire
#'   stock comparison process, including subject installation. Defaults to 7200.
#'   Also bounds each library restoration or loadability subprocess during admission.
#'   Increasing either budget allows unfinished targets another attempt without
#'   invalidating completed comparisons.
#' @inheritParams revdep_prepare
#'
#' @return A `revdep_result` object with `summary`, `results`, `changes`, `diagnostics`,
#'   the frozen `plan`, and advanced `evidence`.
#'   `summary$elapsed_seconds` measures the stock comparison adapter, excluding
#'   stock initialization.
#'   `changes` is a data frame with `package`, `severity` (`error`, `warning`,
#'   `note`), `change` (`added`, `removed`), and `message`, using stock's normalized
#'   comparison. These details persist in the saved result. Stock calls a pair
#'   `unchanged` when it has no new problems, so it may still have removed problems.
#'
#' @details
#' Ordinary retries retain complete pairs and retry unfinished pairs. Keep the
#' run directory to resume an unfinished comparison. Completed results can be
#' reused without the old comparison workspace or logs. Binary admission checks
#' the current R major/minor version, platform, architecture, and OS tag;
#' comparison identity also includes the full R version and candidate source.
#' After other environmental repairs, use `repeat_checks = TRUE` to run both sides
#' again. Changing candidate hard dependencies or constraints requires a new plan
#' and preparation; ordinary source edits can reuse preparation.
#'
#' @examples
#' \dontrun{
#' prepared <- revdep_prepare("/path/to/package")
#' if (nrow(prepared$problems) == 0L) {
#'   result <- revdep_check(prepared, verbose = TRUE)
#'   result$results
#'   result$changes
#' }
#' }
#' @export
revdep_check <- function(
  prepared,
  repeat_checks = FALSE,
  worker_timeout_seconds = NULL,
  process_timeout_seconds = 7200L,
  verbose = FALSE
) {
  verbose <- revdep_verbose(verbose)
  if (
    !is.logical(repeat_checks) ||
      length(repeat_checks) != 1L ||
      is.na(repeat_checks)
  ) {
    stop("`repeat_checks` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(worker_timeout_seconds)) {
    worker_timeout_seconds <- revdep_execution_timeout(
      worker_timeout_seconds,
      "worker_timeout_seconds"
    )
  }
  process_timeout_seconds <- revdep_execution_timeout(
    process_timeout_seconds,
    "process_timeout_seconds"
  )
  state <- revdep_prepared_state(prepared)
  if (
    is.null(state$gate) ||
      !is.null(state$problem) ||
      any(state$gate$report$results$outcome != "prepared")
  ) {
    stop(
      "Preparation is incomplete; resolve `prepared$problems` and run `revdep_prepare()` again.",
      call. = FALSE
    )
  }
  require_stock_adapter_tools()

  revdep_progress(
    verbose,
    "Validating prepared packages in the current environment."
  )
  state <- restore_prepared_library(state, process_timeout_seconds, verbose)
  admitted <- check_preparation_loadability(
    state$gate,
    state$context,
    process_timeout_seconds,
    verbose = verbose
  )
  if (any(admitted$report$results$outcome != "prepared")) {
    problems <- revdep_preparation_problems(admitted, state$context)
    stop(
      paste0(
        "Prepared packages are not loadable in the current environment: ",
        paste(problems$package, collapse = ", "),
        ". Run `revdep_prepare()` again for package logs.\n",
        paste(problems$diagnostic_excerpt, collapse = "\n")
      ),
      call. = FALSE
    )
  }

  checkpoint <- attr(prepared, "checkpoint", exact = TRUE)
  candidate <- revdep_source_candidate_identity(state$context)
  environment <- revdep_compatibility_lane()
  comparison_id <- revdep_request_id(list(
    request_id = state$request_id,
    candidate = candidate,
    environment = environment,
    artifacts = state$gate$report$artifacts
  ))
  check_checkpoint <- file.path(
    dirname(checkpoint),
    paste0("check-v4-", comparison_id, ".rds")
  )
  check_state <- if (file.exists(check_checkpoint)) {
    value <- read_revdep_checkpoint(check_checkpoint, "comparison")
    validate_revdep_check_state(value, state$request_id, candidate, environment)
    value
  } else {
    list(
      version = "revdeprunner-check-state/v4",
      request_id = state$request_id,
      candidate = candidate,
      environment = environment,
      workspace = NULL,
      initialization = NULL,
      result = NULL,
      elapsed_seconds = NA_real_
    )
  }

  if (!is.null(check_state$initialization)) {
    initialization <- check_state$initialization
    if (
      !identical(initialization$candidate, candidate) ||
        !identical(initialization$baseline, state$baseline) ||
        !identical(
          initialization$selected_targets,
          state$context$universe$targets
        ) ||
        !identical(
          initialization$preparation_report$artifacts,
          state$gate$report$artifacts
        )
    ) {
      stop(
        "The saved comparison inputs differ from this preparation.",
        call. = FALSE
      )
    }
  }

  if (repeat_checks) {
    check_state[c("workspace", "initialization", "result")] <- list(
      NULL,
      NULL,
      NULL
    )
    check_state$elapsed_seconds <- NA_real_
  }

  if (is.null(check_state$initialization)) {
    revdep_progress(verbose, "Initializing comparison workspace.")
    if (is.null(check_state$workspace)) {
      check_state$workspace <- paste0(
        "stock-",
        substr(comparison_id, 1L, 16L),
        "-",
        basename(tempfile())
      )
      # Reserve ownership durably before creating fallible initialization state.
      write_revdep_checkpoint(check_state, check_checkpoint)
    }
    recover_stock_initialization_workspace(
      check_state$workspace,
      state$context$path_plan
    )
    check_state$initialization <- initialize_stock_revdepcheck(
      state$gate,
      state$context,
      state$baseline$path,
      workspace = check_state$workspace
    )
    write_revdep_checkpoint(check_state, check_checkpoint)
  }

  if (
    is.null(check_state$result) ||
      identical(check_state$result$state, "comparison-incomplete")
  ) {
    started <- proc.time()[["elapsed"]]
    check_state$result <- run_stock_revdepcheck(
      check_state$initialization,
      state$context,
      worker_timeout_seconds = worker_timeout_seconds,
      process_timeout_seconds = process_timeout_seconds,
      verbose = verbose
    )
    check_state$elapsed_seconds <- unname(
      proc.time()[["elapsed"]] - started
    )
    write_revdep_checkpoint(check_state, check_checkpoint)
  } else {
    revdep_progress(verbose, "Reusing completed comparisons.")
  }

  revdep_progress(verbose, "Comparison complete: %s.", check_state$result$state)
  new_revdep_result(
    state,
    check_state,
    check_checkpoint
  )
}

revdep_execution_timeout <- function(value, argument) {
  if (
    !is.numeric(value) ||
      is.complex(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !is.finite(value) ||
      value <= 0 ||
      value != floor(value) ||
      value > .Machine$integer.max
  ) {
    stop(
      sprintf(
        "`%s` must be a positive whole number of seconds up to %d.",
        argument,
        .Machine$integer.max
      ),
      call. = FALSE
    )
  }
  as.integer(value)
}

recover_stock_initialization_workspace <- function(workspace, path_plan) {
  workspace <- validate_runtime_run_id(workspace)
  root <- file.path(runtime_role_path(path_plan, "run"), workspace)
  validate_runtime_derived_path(
    root,
    path_plan$runs_root,
    "unfinished stock workspace"
  )
  if (dir.exists(root) && unlink(root, recursive = TRUE) != 0L) {
    stop(
      "Unable to remove the owned unfinished stock workspace.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

require_linux_revdep_runner <- function() {
  if (!identical(unname(Sys.info()[["sysname"]]), "Linux")) {
    stop(
      "The reverse-dependency workflow is currently supported only on Linux.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' @export
print.revdep_prepared <- function(x, ...) {
  summary <- x$summary[1L, , drop = FALSE]
  cat(sprintf(
    "Reverse-dependency preparation for %s (baseline %s)\n",
    summary$package,
    summary$baseline_version
  ))
  cat(sprintf(
    "State: %s; %d targets; %d requirements; %d problems\n",
    summary$state,
    summary$selected_targets,
    summary$requirements,
    summary$problems
  ))
  invisible(x)
}

#' @export
print.revdep_result <- function(x, ...) {
  summary <- x$summary[1L, , drop = FALSE]
  cat(sprintf(
    "Reverse-dependency result for %s: %s\n",
    summary$package,
    summary$state
  ))
  cat(sprintf(
    "%d unchanged; %d changed; %d incomplete; %d not checked\n",
    summary$unchanged,
    summary$changed,
    summary$incomplete,
    summary$not_checked
  ))
  invisible(x)
}

revdep_runtime_storage <- function() {
  data <- Sys.getenv(
    "REVDEP_RUNNER_DATA",
    unset = tools::R_user_dir("revdeprunner", "data")
  )
  runs <- Sys.getenv(
    "REVDEP_RUNNER_RUNS",
    unset = tools::R_user_dir("revdeprunner", "cache")
  )
  list(
    data = ensure_revdep_directory(data, "durable data root"),
    runs = ensure_revdep_directory(runs, "run-state root")
  )
}

revdep_prepare_checkout_request <- function(
  package,
  recursive,
  max_recursive,
  sample_seed,
  cache,
  repos,
  storage
) {
  package_root <- normalize_runtime_anchor(package, "package")
  settings <- revdep_plan_settings(recursive, max_recursive, sample_seed)
  repositories <- revdep_plan_repositories(repos)
  cache_roots <- revdep_plan_cache_roots(cache)
  identity_cache_roots <- cache_roots
  if (is.null(cache)) {
    identity_cache_roots <- setdiff(
      identity_cache_roots,
      revdep_plan_runner_cache_roots()
    )
  }
  id <- revdep_request_id(list(
    kind = "checkout",
    package_root = package_root,
    settings = settings,
    repositories = repositories$bases,
    cache_roots = identity_cache_roots,
    environment = revdep_preparation_environment(),
    candidate_dependencies = read_candidate_dependencies(package_root)
  ))
  list(
    id = id,
    checkpoint = revdep_prepare_checkpoint(storage$data, id),
    package_root = package_root,
    repositories = repositories$bases
  )
}

revdep_prepare_plan_request <- function(plan, storage) {
  package_root <- attr(plan, "package_root", exact = TRUE)
  snapshot <- attr(plan, "snapshot", exact = TRUE)
  selected <- attr(plan, "selected_targets", exact = TRUE)
  cache_roots <- attr(plan, "cache_roots", exact = TRUE)
  id <- revdep_request_id(list(
    kind = "plan",
    package_root = package_root,
    snapshot_id = snapshot$snapshot_id,
    selected_targets = selected,
    cache_roots = cache_roots,
    environment = revdep_preparation_environment(),
    candidate_dependencies = attr(plan, "candidate_dependencies", exact = TRUE)
  ))
  list(
    id = id,
    checkpoint = revdep_prepare_checkpoint(storage$data, id),
    package_root = package_root
  )
}

revdep_request_id <- function(fields) {
  digest::digest(fields, algo = "sha256")
}

revdep_prepare_checkpoint <- function(data_root, request_id) {
  directory <- ensure_revdep_directory(
    file.path(data_root, "checkpoints"),
    "checkpoint directory"
  )
  file.path(directory, paste0("prepare-v5-", request_id, ".rds"))
}

write_revdep_checkpoint <- function(value, path) {
  directory <- ensure_revdep_directory(dirname(path), "checkpoint directory")
  temporary <- tempfile(
    pattern = paste0(".", basename(path), "-"),
    tmpdir = directory
  )
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  saveRDS(value, temporary, version = 3L)
  if (!file.rename(temporary, path)) {
    stop(sprintf("Unable to publish checkpoint: %s", path), call. = FALSE)
  }
  invisible(path)
}

read_revdep_checkpoint <- function(path, label) {
  tryCatch(
    readRDS(path),
    error = function(error) {
      stop(
        sprintf("Unable to read the saved %s checkpoint: %s", label, path),
        call. = FALSE
      )
    }
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
  validate_repository_snapshot(snapshot)
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

new_revdep_prepare_state <- function(plan, request, storage) {
  context <- revdep_prepare_context(plan, request, storage)
  state <- list(
    version = "revdeprunner-prepare-state/v5",
    request_id = request$id,
    plan = plan,
    context = context,
    baseline = NULL,
    gate = NULL,
    problem = NULL
  )
  validate_revdep_prepare_state(state, request$id)
  state
}

revdep_prepare_context <- function(plan, request, storage) {
  snapshot <- attr(plan, "snapshot", exact = TRUE)
  cohort <- attr(plan, "cohort", exact = TRUE)
  selected <- attr(plan, "selected_targets", exact = TRUE)
  cache_roots <- attr(plan, "cache_roots", exact = TRUE)
  policy <- if (
    identical(
      selected,
      select_dependency_universe_targets(cohort, "direct")
    )
  ) {
    "direct"
  } else if (identical(selected, cohort$targets)) {
    "recursive-strong"
  } else {
    "selected"
  }
  universe <- new_dependency_universe(
    cohort,
    snapshot,
    policy,
    revdep_base_packages(),
    targets = if (policy == "selected") selected else NULL,
    candidate_dependencies = attr(plan, "candidate_dependencies", exact = TRUE)
  )
  lane <- revdep_compatibility_lane()
  if (length(cache_roots) == 0L) {
    cache_roots <- ensure_revdep_directory(
      file.path(storage$data, "binary-cache", "empty"),
      "empty cache"
    )
  }
  run_id <- paste0(
    tolower(cohort$package),
    "-",
    substr(request$id, 1L, 16L)
  )
  path_plan <- new_runtime_root_plan(
    request$package_root,
    storage$data,
    storage$runs,
    run_id,
    cache_roots
  )
  requests <- preparation_required_packages(
    derive_preparation_requirements(universe)
  )
  requests <- requests[!is.na(requests$version), , drop = FALSE]
  rownames(requests) <- NULL
  observations <- observe_cache_roots(cache_roots, requests)
  binary_reuse <- reuse_cached_binaries(
    requests,
    observations,
    lane,
    path_plan
  )
  source_plan <- new_source_acquisition_plan(
    universe,
    cohort,
    snapshot,
    binary_reuse,
    lane,
    path_plan
  )
  r_executable <- normalize_r_executable(
    file.path(R.home("bin"), "R")
  )
  context <- list(
    source_plan = source_plan,
    universe = universe,
    cohort = cohort,
    snapshot = snapshot,
    binary_reuse = binary_reuse,
    lane = lane,
    path_plan = path_plan,
    r_executable = r_executable
  )
  context
}

revdep_base_packages <- function() {
  rownames(utils::installed.packages(priority = "base"))
}

revdep_compatibility_lane <- function() {
  architecture <- R.version$arch
  if (is.null(architecture) || !nzchar(architecture)) {
    architecture <- sub("-.*$", "", R.version$platform)
  }
  os_abi <- R.version$os
  if (is.null(os_abi) || !nzchar(os_abi)) {
    os_abi <- tolower(unname(Sys.info()[["sysname"]]))
  }
  new_compatibility_lane(
    revdep_plan_r_major_minor(),
    R.version$platform,
    architecture,
    os_abi,
    paste0("R-", as.character(getRversion()))
  )
}

revdep_preparation_environment <- function(lane = revdep_compatibility_lane()) {
  lane[c("r_major_minor", "r_platform", "architecture", "os_abi")]
}

admit_revdep_preparation_environment <- function(state) {
  if (
    !identical(
      revdep_preparation_environment(state$context$lane),
      revdep_preparation_environment()
    )
  ) {
    stop(
      "Prepared binaries are incompatible with the current R environment; run `revdep_prepare()` again.",
      call. = FALSE
    )
  }
  state$context$r_executable <- normalize_r_executable(file.path(
    R.home("bin"),
    "R"
  ))
  state
}

acquire_revdep_baseline <- function(cohort, snapshot, data_root, path_plan) {
  package_row <- revdep_plan_baseline(cohort$package, snapshot)
  if (nrow(package_row) != 1L) {
    stop("The frozen package baseline is unavailable.", call. = FALSE)
  }
  source_url <- source_acquisition_url(package_row)
  archive_name <- source_acquisition_archive_name(source_url)
  directory <- ensure_revdep_directory(
    file.path(
      data_root,
      "baselines",
      cohort$package,
      package_row$Version[[1L]]
    ),
    "baseline directory"
  )
  path <- file.path(directory, archive_name)
  if (file.exists(path)) {
    return(validate_stock_baseline_source(path, cohort, snapshot))
  }

  staging <- tempfile(".baseline-", tmpdir = directory)
  if (!dir.create(staging)) {
    stop("Unable to create baseline staging state.", call. = FALSE)
  }
  on.exit(unlink(staging, recursive = TRUE, force = TRUE), add = TRUE)
  temporary <- file.path(staging, archive_name)
  download_preparation_source(
    source_url,
    temporary,
    cohort$package,
    package_row$Version[[1L]],
    path_plan
  )
  validate_stock_baseline_source(temporary, cohort, snapshot)
  if (!file.rename(temporary, path)) {
    if (file.exists(path)) {
      return(validate_stock_baseline_source(path, cohort, snapshot))
    }
    stop("Unable to publish the frozen package baseline.", call. = FALSE)
  }
  validate_stock_baseline_source(path, cohort, snapshot)
}

validate_revdep_prepare_state <- function(state, request_id) {
  if (
    is.list(state) &&
      !is.null(state$version) &&
      !identical(state$version, "revdeprunner-prepare-state/v5")
  ) {
    stop(
      paste(
        "The saved preparation checkpoint uses an unsupported version.",
        "Create a fresh preparation with `revdep_prepare()`."
      ),
      call. = FALSE
    )
  }
  fields <- c(
    "version",
    "request_id",
    "plan",
    "context",
    "baseline",
    "gate",
    "problem"
  )
  if (
    !is.list(state) ||
      !identical(names(state), fields) ||
      !identical(state$version, "revdeprunner-prepare-state/v5") ||
      !identical(state$request_id, request_id) ||
      !inherits(state$plan, "revdep_plan") ||
      !is.list(state$context) ||
      !identical(
        state$context$snapshot$snapshot_id,
        state$plan$summary$snapshot_id
      ) ||
      !identical(
        state$context$cohort$package,
        state$plan$summary$package
      ) ||
      (!is.null(state$baseline) &&
        (!identical(state$baseline$package, state$plan$summary$package) ||
          !identical(
            state$baseline$version,
            state$plan$summary$baseline_version
          ))) ||
      (!is.null(state$gate) &&
        (!is.list(state$gate) || is.null(state$baseline))) ||
      (!is.null(state$problem) &&
        (!is.data.frame(state$problem) ||
          nrow(state$problem) != 1L ||
          !identical(names(state$problem), names(empty_revdep_problems()))))
  ) {
    stop(
      "The saved preparation checkpoint has an invalid structure.",
      call. = FALSE
    )
  }
  invisible(state)
}

new_revdep_prepared <- function(state, checkpoint) {
  results <- if (is.null(state$gate)) data.frame(outcome = character()) else
    state$gate$report$results
  problems <- if (is.null(state$gate)) empty_revdep_problems() else
    revdep_preparation_problems(state$gate, state$context)
  problems <- rbind(state$problem, problems)
  summary <- data.frame(
    package = state$plan$summary$package,
    development_version = state$plan$summary$development_version,
    baseline_version = state$plan$summary$baseline_version,
    snapshot_id = state$plan$summary$snapshot_id,
    state = if (nrow(problems) == 0L) "ready" else "preparation-incomplete",
    selected_targets = state$plan$summary$selected_targets,
    requirements = nrow(preparation_required_packages(
      state$context$source_plan$requirements
    )),
    prepared = sum(results$outcome == "prepared"),
    problems = nrow(problems),
    blocked = sum(results$outcome == "blocked"),
    stringsAsFactors = FALSE
  )
  prepared <- structure(
    list(
      summary = summary,
      problems = problems,
      plan = state$plan,
      evidence = list(
        checkpoint = checkpoint,
        baseline = state$baseline,
        report = state$gate$report
      )
    ),
    class = "revdep_prepared",
    checkpoint = checkpoint,
    request_id = state$request_id
  )
  prepared
}

revdep_preparation_problems <- function(gate, context) {
  revdep_report_problems(gate$report, context)
}

revdep_report_problems <- function(report, context) {
  results <- report$results
  failed <- results$outcome != "prepared"
  results <- results[failed, , drop = FALSE]
  if (nrow(results) == 0L) {
    return(empty_revdep_problems())
  }
  attempts <- report$attempts
  attempt_rows <- match(results$evidence_attempt_id, attempts$attempt_id)
  present <- !is.na(attempt_rows)
  stage <- stdout <- stderr <- rep(NA_character_, nrow(results))
  stage[present] <- attempts$stage[attempt_rows[present]]
  stdout[present] <- attempts$stdout_path[attempt_rows[present]]
  stderr[present] <- attempts$stderr_path[attempt_rows[present]]
  run_root <- runtime_role_path(context$path_plan, "run")
  stdout <- revdep_problem_log_paths(stdout, run_root)
  stderr <- revdep_problem_log_paths(stderr, run_root)
  data.frame(
    package = results$package,
    version = results$version,
    outcome = results$outcome,
    blocking_dependency = results$blocking_dependency,
    stage = stage,
    diagnostic_excerpt = results$diagnostic_excerpt,
    stdout_path = stdout,
    stderr_path = stderr,
    stringsAsFactors = FALSE
  )
}

empty_revdep_problems <- function() {
  data.frame(
    package = character(),
    version = character(),
    outcome = character(),
    blocking_dependency = character(),
    stage = character(),
    diagnostic_excerpt = character(),
    stdout_path = character(),
    stderr_path = character(),
    stringsAsFactors = FALSE
  )
}

revdep_problem_log_paths <- function(paths, run_root) {
  present <- !is.na(paths)
  paths[present] <- file.path(run_root, paths[present])
  paths
}

revdep_prepared_state <- function(prepared) {
  checkpoint <- attr(prepared, "checkpoint", exact = TRUE)
  request_id <- attr(prepared, "request_id", exact = TRUE)
  if (
    !inherits(prepared, "revdep_prepared") ||
      !is.list(prepared) ||
      !identical(
        names(prepared),
        c("summary", "problems", "plan", "evidence")
      ) ||
      !is.character(checkpoint) ||
      length(checkpoint) != 1L ||
      !file.exists(checkpoint) ||
      !is.character(request_id) ||
      length(request_id) != 1L
  ) {
    stop("`prepared` is not a valid preparation object.", call. = FALSE)
  }
  state <- read_revdep_checkpoint(checkpoint, "preparation")
  validate_revdep_prepare_state(state, request_id)
  if (!identical(prepared$plan, state$plan)) {
    stop("`prepared` differs from its saved preparation plan.", call. = FALSE)
  }
  validate_revdep_candidate_requirements(state)
  admit_revdep_preparation_environment(state)
}

revdep_source_candidate_identity <- function(context) {
  stock_adapter_checkout_identity(
    context$path_plan$package_root,
    context$cohort$package
  )
}

validate_revdep_check_state <- function(
  check_state,
  request_id,
  candidate,
  environment
) {
  if (
    is.list(check_state) &&
      !is.null(check_state$version) &&
      !identical(check_state$version, "revdeprunner-check-state/v4")
  ) {
    stop(
      paste(
        "The saved comparison checkpoint uses an unsupported version.",
        "Create a fresh preparation with `revdep_prepare()`."
      ),
      call. = FALSE
    )
  }
  fields <- c(
    "version",
    "request_id",
    "candidate",
    "environment",
    "workspace",
    "initialization",
    "result",
    "elapsed_seconds"
  )
  if (
    !is.list(check_state) ||
      !identical(names(check_state), fields) ||
      !identical(check_state$version, "revdeprunner-check-state/v4") ||
      !identical(check_state$request_id, request_id) ||
      !identical(check_state$candidate, candidate) ||
      !identical(check_state$environment, environment) ||
      !is.numeric(check_state$elapsed_seconds) ||
      length(check_state$elapsed_seconds) != 1L ||
      (!is.null(check_state$initialization) &&
        !inherits(
          check_state$initialization,
          "revdeprunner_stock_initialization"
        )) ||
      (!is.null(check_state$result) &&
        !inherits(check_state$result, "revdeprunner_stock_result"))
  ) {
    stop(
      "The saved comparison checkpoint has an invalid structure.",
      call. = FALSE
    )
  }
  if (!is.null(check_state$workspace)) {
    validate_runtime_run_id(check_state$workspace)
    if (!grepl("^stock-[0-9a-f]{16}-file[0-9a-f]+$", check_state$workspace)) {
      stop("The saved comparison workspace is invalid.", call. = FALSE)
    }
  }
  if (!is.null(check_state$result)) {
    if (
      !identical(check_state$result$initialization, check_state$initialization)
    ) {
      stop(
        "The saved comparison result belongs to another initialization.",
        call. = FALSE
      )
    }
    validate_stock_result_evidence(check_state$result)
    if (
      !is.finite(check_state$elapsed_seconds) || check_state$elapsed_seconds < 0
    ) {
      stop("The saved comparison duration is invalid.", call. = FALSE)
    }
  } else if (!is.na(check_state$elapsed_seconds)) {
    stop("An incomplete comparison cannot have a duration.", call. = FALSE)
  }
  invisible(check_state)
}

new_revdep_result <- function(state, check_state, checkpoint) {
  result <- check_state$result
  outcomes <- result$results$outcome
  summary <- data.frame(
    package = state$plan$summary$package,
    development_version = check_state$candidate$version,
    baseline_version = state$plan$summary$baseline_version,
    snapshot_id = state$plan$summary$snapshot_id,
    state = result$state,
    selected_targets = nrow(result$results),
    unchanged = sum(outcomes == "unchanged"),
    changed = sum(outcomes == "changed"),
    incomplete = sum(outcomes == "incomplete"),
    not_checked = sum(outcomes == "not_checked"),
    elapsed_seconds = check_state$elapsed_seconds,
    stringsAsFactors = FALSE
  )
  structure(
    list(
      summary = summary,
      results = result$results,
      changes = result$changes,
      diagnostics = result$diagnostics,
      plan = state$plan,
      evidence = list(
        checkpoint = checkpoint,
        logs = result$logs
      )
    ),
    class = "revdep_result"
  )
}
