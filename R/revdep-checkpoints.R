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
