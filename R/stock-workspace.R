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

stock_subject_hard_dependencies <- function(report, context, checkout) {
  fields <- stock_runner_recursive_fields()
  candidate <- read.dcf(
    file.path(checkout, "DESCRIPTION"),
    fields = fields
  )
  baseline <- revdep_plan_baseline(context$cohort$package, context$snapshot)
  roots <- unique(c(
    unlist(lapply(fields, function(field) {
      parse_stock_dependency_field(candidate[[1L, field]], field)
    })),
    unlist(lapply(fields, function(field) {
      parse_stock_dependency_field(baseline[[field]][[1L]], field)
    }))
  ))
  excluded <- c(
    "R",
    context$universe$base_packages,
    context$cohort$package
  )
  roots <- setdiff(roots, excluded)
  dependencies <- roots
  pending <- roots
  visited <- character()
  edges <- unique(context$universe$edges[
    context$universe$edges$relationship %in% fields,
    c("from_package", "dependency"),
    drop = FALSE
  ])
  while (length(pending) > 0L) {
    package <- pending[[1L]]
    pending <- pending[-1L]
    if (package %in% visited) {
      next
    }
    visited <- c(visited, package)
    discovered <- edges$dependency[edges$from_package == package]
    discovered <- setdiff(discovered, excluded)
    new <- setdiff(discovered, dependencies)
    dependencies <- c(dependencies, new)
    pending <- c(pending, new)
  }
  dependencies <- sort(unique(dependencies), method = "radix")
  if (length(dependencies) == 0L) {
    return(data.frame(
      package = character(),
      version = character(),
      stringsAsFactors = FALSE
    ))
  }
  rows <- match(dependencies, report$results$package)
  invalid <- dependencies[is.na(rows)]
  present <- !is.na(rows)
  invalid <- unique(c(
    invalid,
    dependencies[present][report$results$outcome[rows[present]] != "prepared"]
  ))
  if (anyDuplicated(report$results$package)) {
    stop("Stock preparation results contain duplicate packages.", call. = FALSE)
  }
  if (length(invalid) > 0L) {
    stop(
      sprintf(
        "Every stock subject hard dependency must be exactly prepared: %s.",
        paste(invalid, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  data.frame(
    package = dependencies,
    version = report$results$version[rows],
    stringsAsFactors = FALSE
  )
}

stock_subject_library_paths <- function(paths, package) {
  root <- file.path(paths$checkout, "revdep", "library", package)
  c(old = file.path(root, "old"), new = file.path(root, "new"))
}

seed_stock_subject_libraries <- function(report, context, paths) {
  dependencies <- stock_subject_hard_dependencies(
    report,
    context,
    paths$checkout
  )
  build_library <- file.path(
    runtime_role_path(context$path_plan, "run"),
    "build-library"
  )
  build_library <- normalize_runtime_anchor(
    build_library,
    "source preparation build library"
  )
  libraries <- stock_subject_library_paths(paths, context$cohort$package)
  for (library in libraries) {
    if (!dir.create(library, recursive = TRUE)) {
      stop("Unable to create a stock subject library.", call. = FALSE)
    }
    for (row in seq_len(nrow(dependencies))) {
      package <- dependencies$package[[row]]
      version <- dependencies$version[[row]]
      validate_source_preparation_library_package(
        build_library,
        package,
        version
      )
      if (
        !isTRUE(file.copy(
          file.path(build_library, package),
          library,
          recursive = TRUE,
          copy.mode = TRUE,
          copy.date = TRUE
        ))
      ) {
        stop(
          "Unable to seed a prepared stock subject dependency.",
          call. = FALSE
        )
      }
      validate_source_preparation_library_package(library, package, version)
    }
  }
  invisible(dependencies)
}

validate_stock_subject_libraries <- function(report, context, paths) {
  dependencies <- stock_subject_hard_dependencies(
    report,
    context,
    paths$checkout
  )
  libraries <- stock_subject_library_paths(paths, context$cohort$package)
  if (any(!dir.exists(libraries))) {
    stop("Stock subject dependency libraries are unavailable.", call. = FALSE)
  }
  for (library in libraries) {
    for (row in seq_len(nrow(dependencies))) {
      validate_source_preparation_library_package(
        library,
        dependencies$package[[row]],
        dependencies$version[[row]]
      )
    }
  }
  invisible(dependencies)
}

create_stock_adapter_paths <- function(
  path_plan,
  workspace = "stock-revdepcheck"
) {
  validate_runtime_root_plan(path_plan)
  workspace <- validate_runtime_run_id(workspace)
  run_root <- runtime_role_path(path_plan, "run")
  run_root <- ensure_source_acquisition_directory(
    run_root,
    path_plan$runs_root,
    "stock adapter run root"
  )
  root <- file.path(run_root, workspace)
  if (file.exists(root) || path_is_link(root)) {
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
  for (path in c(paths$stdout, paths$stderr)) {
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

validate_stock_private_libraries <- function(
  initialization,
  process,
  database
) {
  if (process$status != 0L || !identical(database$stage, "done")) {
    return(invisible(NULL))
  }
  for (target in initialization$requested_targets$package) {
    expected <- initialization$stock_dependencies[
      initialization$stock_dependencies$target == target,
      c("dependency", "version"),
      drop = FALSE
    ]
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
    }
  }
  invisible(NULL)
}

read_stock_libraries <- function(path) {
  path <- normalize_regular_artifact_file(path, "stock library evidence")
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
    "empty_repos",
    "root",
    "binary_contrib",
    "source_contrib",
    "stdout",
    "stderr"
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
        path_is_link(path) ||
        !path_is_within(
          run_root,
          normalizePath(path, winslash = "/", mustWork = TRUE)
        )
    ) {
      stop("Stock adapter path escapes its run root.", call. = FALSE)
    }
  }
  directories <- setdiff(required, c("stdout", "stderr"))
  if (any(!vapply(paths[directories], dir.exists, logical(1L)))) {
    stop("Stock adapter directory evidence is incomplete.", call. = FALSE)
  }
  for (name in c("stdout", "stderr")) {
    normalize_regular_artifact_file(paths[[name]], paste("stock", name))
  }
  invisible(paths)
}
