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
