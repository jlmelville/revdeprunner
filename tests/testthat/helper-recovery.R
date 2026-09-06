# Install the source under test when using load_all(); installed-package checks
# can launch the already installed copy. Both routes exercise fresh R sessions.
recovery_runner_library <- function(root) {
  package <- system.file(package = "revdeprunner")
  if (!file.exists(file.path(package, "R", "revdep-run.R"))) {
    return(dirname(package))
  }
  library <- file.path(root, "runner-library")
  dir.create(library)
  log <- file.path(root, "runner-install.log")
  status <- system2(
    file.path(R.home("bin"), "R"),
    c(
      "CMD",
      "INSTALL",
      paste0("--library=", shQuote(library)),
      shQuote(package)
    ),
    stdout = log,
    stderr = log
  )
  if (status != 0L) stop(paste(readLines(log, warn = FALSE), collapse = "\n"))
  library
}

recovery_fixture <- function(root, subject_install_block = NULL) {
  local <- make_revdep_run_fixture()
  if (!is.null(subject_install_block)) {
    archive <- make_installable_source_archive(
      local$fixture$repository_root,
      package = "SubjectPkg",
      version = "0.1",
      needs_compilation = "no",
      on_load = c(
        ".onLoad <- function(libname, pkgname) {",
        paste0("  block <- ", deparse(subject_install_block)),
        "  if (grepl('/SubjectPkg/old', libname, fixed = TRUE) && file.exists(block)) {",
        "    file.create(paste0(block, '-waiting'))",
        "    while (file.exists(block)) Sys.sleep(0.05)",
        "  }",
        "}"
      )
    )
    local$database$MD5sum[local$database$Package == "SubjectPkg"] <-
      unname(tools::md5sum(archive))
  }
  markers <- file.path(root, "markers")
  dir.create(markers)
  for (package in c("BuildPkg", "FilePkg", "HitPkg")) {
    row <- local$database[local$database$Package == package, , drop = FALSE]
    test <- c(
      paste0("root <- ", deparse(markers)),
      "version <- as.character(utils::packageVersion('SubjectPkg'))",
      paste0("marker <- file.path(root, paste0('", package, "-', version))"),
      "cat('run\\n', file = marker, append = TRUE)"
    )
    if (package == "BuildPkg") {
      test <- c(test, "stopifnot(version == '0.1')")
    }
    if (package == "HitPkg") {
      test <- c(
        test,
        "if (file.exists(file.path(root, 'block'))) {",
        "  file.create(file.path(root, 'waiting'))",
        "  while (file.exists(file.path(root, 'block'))) Sys.sleep(0.05)",
        "}"
      )
    }
    archive <- make_installable_source_archive(
      local$fixture$repository_root,
      package = package,
      version = row$Version[[1L]],
      needs_compilation = "no",
      relative_directory = if (package == "FilePkg") "custom" else "",
      imports = "SubjectPkg",
      suggests = row$Suggests[[1L]],
      tests = test
    )
    local$database$MD5sum[
      local$database$Package == package
    ] <- unname(tools::md5sum(archive))
  }
  local$markers <- markers
  local
}

recovery_check_process <- function(
  library,
  prepared,
  controls,
  root,
  initialization_marker = NULL
) {
  log <- tempfile("comparison-", tmpdir = root, fileext = ".log")
  callr::r_bg(
    function(prepared, controls, initialization_marker) {
      library("revdeprunner")
      if (!is.null(initialization_marker)) {
        create <- getFromNamespace("create_stock_adapter_paths", "revdeprunner")
        testthat::local_mocked_bindings(
          create_stock_adapter_paths = function(...) {
            paths <- create(...)
            file.create(file.path(paths$root, "unfinished-witness"))
            file.create(initialization_marker)
            while (file.exists(initialization_marker)) Sys.sleep(0.05)
            paths
          },
          .package = "revdeprunner"
        )
      }
      do.call(revdep_check, c(list(readRDS(prepared)), controls))
    },
    args = list(
      prepared = prepared,
      controls = controls,
      initialization_marker = initialization_marker
    ),
    libpath = c(library, .libPaths()),
    stdout = log,
    stderr = log,
    supervise = TRUE
  )
}

recovery_wait <- function(
  process,
  condition = function() !process$is_alive(),
  seconds = 180
) {
  deadline <- Sys.time() + seconds
  repeat {
    if (condition()) return(invisible(NULL))
    if (!process$is_alive()) {
      process$get_result()
      stop("Comparison exited before its completion marker.")
    }
    if (Sys.time() > deadline) {
      process$kill_tree()
      stop("Recovery fixture exceeded its observation deadline.")
    }
    Sys.sleep(0.1)
  }
}

recovery_markers <- function(
  root,
  packages = c("BuildPkg", "FilePkg", "HitPkg")
) {
  paths <- as.vector(outer(packages, c("0.1", "0.2"), paste, sep = "-"))
  stats::setNames(
    vapply(
      file.path(root, paths),
      function(path) {
        if (file.exists(path)) length(readLines(path, warn = FALSE)) else 0L
      },
      integer(1L)
    ),
    paths
  )
}
