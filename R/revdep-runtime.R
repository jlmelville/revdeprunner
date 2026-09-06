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

require_linux_revdep_runner <- function() {
  if (!identical(unname(Sys.info()[["sysname"]]), "Linux")) {
    stop(
      "The reverse-dependency workflow is currently supported only on Linux.",
      call. = FALSE
    )
  }
  invisible(NULL)
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
