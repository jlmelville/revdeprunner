# These private tests protect the pinned stock-runner bridge, its resumable
# pre-worker boundary, and its exact run-local artifact evidence.

# nolint start: object_usage_linter.
stock_fixture_database <- function() {
  database <- source_acquisition_fixture_database()
  primary <- source_acquisition_fixture_repositories()[["CRAN"]]
  database$Imports[database$Package == "HitPkg"] <- "SubjectPkg"
  database$Suggests[
    database$Package == "BuildPkg" & database$Repository == primary
  ] <- "HitPkg"
  database$NeedsCompilation[
    database$Package == "BuildPkg" & database$Repository == primary
  ] <- "no"
  database
}

make_stock_repository_fixture <- function() {
  fixture <- make_source_preparation_fixture(
    missing_binary_packages = c("BuildPkg", "FilePkg", "HitPkg"),
    database = stock_fixture_database(),
    include_subject_source = TRUE
  )
  context <- source_preparation_context(fixture)
  fixture$gate <- do.call(
    revdeprunner:::prepare_dependency_universe,
    c(context, list(timeout_seconds = 60L))
  )
  fixture$ready <- revdeprunner:::prepare_repository_universe(
    fixture$gate,
    context,
    timeout_seconds = 60L
  )
  write_stock_candidate(context$path_plan$package_root)
  fixture$baseline <- fixture$source_archives$SubjectPkg
  fixture
}

write_stock_candidate <- function(path) {
  dir.create(file.path(path, "R"), showWarnings = FALSE)
  writeLines(
    c(
      "Package: SubjectPkg",
      "Type: Package",
      "Title: Stock Adapter Subject Fixture",
      "Version: 0.2",
      paste0(
        "Authors@R: person('Fixture', 'Author', role = c('aut', 'cre'), ",
        "email = 'fixture@example.test')"
      ),
      "Description: A pure-R package-under-test fixture.",
      "License: MIT",
      "Encoding: UTF-8",
      "NeedsCompilation: no"
    ),
    file.path(path, "DESCRIPTION")
  )
  writeLines("export(subject_value)", file.path(path, "NAMESPACE"))
  writeLines(
    "subject_value <- function() 42L",
    file.path(path, "R", "subject.R")
  )
}

stock_tools_are_supported <- function() {
  required <- c("revdepcheck", "crancache", "cranlike")
  if (!all(vapply(required, requireNamespace, logical(1L), quietly = TRUE))) {
    return(FALSE)
  }
  tryCatch(
    {
      revdeprunner:::require_stock_adapter_tools()
      TRUE
    },
    error = function(error) FALSE
  )
}

if (!identical(unname(Sys.info()[["sysname"]]), "Linux")) {
  test_that("the stock adapter reports its Linux boundary", {
    expect_error(
      revdeprunner:::require_linux_repository_projection(),
      "supported only on Linux",
      fixed = TRUE
    )
  })
} else if (!stock_tools_are_supported()) {
  test_that("the stock adapter rejects an unsupported toolchain", {
    expect_error(
      revdeprunner:::require_stock_adapter_tools(),
      "requires revdepcheck",
      fixed = TRUE
    )
  })
} else {
  test_that("stock revdepcheck consumes exact prepared artifacts", {
    fixture <- make_stock_repository_fixture()
    on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
    context <- source_preparation_context(fixture)
    projection_before <- fixture$ready$projection

    initialization <- revdeprunner:::initialize_stock_revdepcheck(
      fixture$ready,
      context,
      fixture$baseline,
      exclude_targets = "FilePkg"
    )
    checkpoint <- file.path(fixture$root, "stock-initialization.rds")
    saveRDS(initialization, checkpoint)
    initialization <- readRDS(checkpoint)

    expect_identical(initialization$discovery$stage, "install")
    expect_identical(
      initialization$discovery$todo$package,
      c("BuildPkg", "HitPkg")
    )
    expect_identical(
      initialization$stock_dependencies,
      data.frame(
        target = "BuildPkg",
        dependency = "HitPkg",
        version = "1.0",
        stringsAsFactors = FALSE
      )
    )
    expect_identical(
      stats::setNames(
        initialization$provenance$remote_sha,
        initialization$provenance$package
      ),
      revdeprunner:::stock_adapter_remote_shas()
    )
    expect_identical(
      initialization$baseline$md5,
      digest::digest(
        fixture$baseline,
        algo = "md5",
        file = TRUE,
        serialize = FALSE
      )
    )
    expect_true(any(startsWith(
      initialization$cache_before$relative_path,
      "cache/bioc/"
    )))
    expect_true(any(startsWith(
      initialization$cache_before$relative_path,
      "fallback/"
    )))
    runtime <- revdeprunner:::observe_stock_runtime(
      initialization$command_plan$r_executable,
      initialization$requested_targets$package,
      initialization$package,
      initialization$environment,
      initialization$repository_settings,
      initialization$paths$temp
    )
    expect_true(revdeprunner:::path_is_within(
      initialization$paths$temp,
      runtime$tempdir
    ))
    expect_identical(runtime$provenance, initialization$provenance)
    expect_invisible(
      revdeprunner:::validate_stock_revdepcheck_initialization(
        initialization,
        context
      )
    )

    incomplete_process <- list(
      command = "fixture stock comparison",
      started_at = "2026-08-30T12:00:00.000000Z",
      duration_ms = 10,
      status = 1L,
      timed_out = FALSE,
      warnings = character()
    )
    incomplete <- revdeprunner:::stock_adapter_results(
      initialization,
      incomplete_process,
      revdeprunner:::observe_stock_database(initialization$paths$checkout)
    )
    expect_identical(
      incomplete$outcome,
      c("incomplete", "not_checked", "incomplete")
    )

    result <- revdeprunner:::run_stock_revdepcheck(
      initialization,
      context,
      worker_timeout_seconds = 60L,
      process_timeout_seconds = 300L
    )

    expect_identical(result$state, "success")
    expect_identical(
      result$results$outcome,
      c("unchanged", "not_checked", "unchanged")
    )
    expect_identical(result$database$stage, "done")
    expect_true(all(result$database$todo$status == "done"))
    expect_identical(
      result$compiler$compilation_count,
      0L,
      info = paste(result$compiler$invocations, collapse = "\n")
    )
    expect_true(result$compiler$invocation_count > 0L)
    expect_true(result$compiler$probe_count > 0L)
    expect_identical(
      result$private_libraries,
      data.frame(
        target = rep("BuildPkg", 2L),
        which = c("old", "new"),
        package = rep("HitPkg", 2L),
        version = rep("1.0", 2L),
        stringsAsFactors = FALSE
      )
    )
    expect_identical(result$projection_after, initialization$projection_before)
    expect_identical(result$cache_after, initialization$cache_before)
    expect_identical(
      fixture$ready$projection$manifest,
      projection_before$manifest
    )
    expect_true(all(vapply(
      result$logs,
      function(log) {
        grepl("^[a-f0-9]{64}$", log$sha256)
      },
      logical(1L)
    )))
    expect_invisible(
      revdeprunner:::validate_stock_revdepcheck_result(result, context)
    )

    failed_complete <- revdeprunner:::stock_adapter_results(
      initialization,
      incomplete_process,
      result$database
    )
    expect_identical(
      failed_complete$outcome,
      c("incomplete", "not_checked", "incomplete")
    )
    for (status in c("i+", "i-", "t+", "t-")) {
      failed_database <- result$database
      failed_database$stock$stock_status[] <- status
      failed_status <- revdeprunner:::stock_adapter_results(
        initialization,
        result$process,
        failed_database
      )
      expect_true(all(
        failed_status$outcome[failed_status$package != "FilePkg"] ==
          "incomplete"
      ))
    }

    cat(
      "\n",
      file = file.path(
        fixture$ready$projection$repository_path,
        "src",
        "contrib",
        "PACKAGES"
      ),
      append = TRUE
    )
    expect_error(
      revdeprunner:::validate_stock_revdepcheck_result(result, context),
      "projection evidence changed",
      fixed = TRUE
    )
  })

  test_that("stock preconditions fail before worker state is accepted", {
    fixture <- make_stock_repository_fixture()
    on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
    context <- source_preparation_context(fixture)
    wrong_repository <- file.path(fixture$root, "wrong-baseline")
    dir.create(wrong_repository)
    wrong <- make_installable_source_archive(
      wrong_repository,
      package = "SubjectPkg",
      version = "9.9",
      needs_compilation = "no"
    )

    expect_error(
      revdeprunner:::initialize_stock_revdepcheck(
        fixture$ready,
        context,
        wrong
      ),
      "does not match the frozen package version",
      fixed = TRUE
    )
    expect_false(dir.exists(file.path(
      fixture$paths[[3L]],
      fixture$path_plan$run_id,
      "stock-revdepcheck"
    )))

    altered_baseline <- file.path(fixture$root, "SubjectPkg_0.1.tar.gz")
    expect_true(file.copy(fixture$baseline, altered_baseline))
    cat("tampered", file = altered_baseline, append = TRUE)
    expect_error(
      revdeprunner:::validate_stock_baseline_source(
        altered_baseline,
        context$cohort,
        context$snapshot
      ),
      "differs from its frozen checksum",
      fixed = TRUE
    )

    not_ready <- fixture$ready$report
    not_ready$results$outcome[
      not_ready$results$package == "BuildPkg"
    ] <- "prepared"
    expect_error(
      revdeprunner:::require_ready_stock_targets(
        not_ready,
        context$universe$targets
      ),
      "must have one exact ready result",
      fixed = TRUE
    )

    initialization <- revdeprunner:::initialize_stock_revdepcheck(
      fixture$ready,
      context,
      fixture$baseline
    )

    candidate_source <- file.path(
      initialization$paths$checkout,
      "R",
      "subject.R"
    )
    candidate_lines <- readLines(candidate_source, warn = FALSE)
    writeLines(c(candidate_lines, "tampered <- TRUE"), candidate_source)
    expect_error(
      revdeprunner:::validate_stock_revdepcheck_initialization(
        initialization,
        context
      ),
      "candidate checkout identity changed",
      fixed = TRUE
    )
    writeLines(candidate_lines, candidate_source)

    wrapper <- initialization$compiler_tools$wrapper[[1L]]
    wrapper_lines <- readLines(wrapper, warn = FALSE)
    writeLines(c(wrapper_lines, "# tampered"), wrapper)
    expect_error(
      revdeprunner:::validate_stock_revdepcheck_initialization(
        initialization,
        context
      ),
      "compiler wrapper is unavailable",
      fixed = TRUE
    )
    writeLines(wrapper_lines, wrapper)

    secondary_cache_file <- file.path(
      initialization$paths$cache,
      "bioc",
      "src",
      "contrib",
      "unexpected.tar.gz"
    )
    file.create(secondary_cache_file)
    expect_error(
      revdeprunner:::validate_stock_revdepcheck_initialization(
        initialization,
        context
      ),
      "cache artifact evidence changed",
      fixed = TRUE
    )
    unlink(secondary_cache_file)

    fallback_file <- file.path(
      sub("^file://", "", initialization$repository_settings[["CRAN"]]),
      "src",
      "contrib",
      "unexpected.tar.gz"
    )
    file.create(fallback_file)
    expect_error(
      revdeprunner:::validate_stock_repository_settings(
        initialization$repository_settings,
        initialization$paths
      ),
      "fallback is not empty",
      fixed = TRUE
    )
    unlink(fallback_file)

    altered_todo <- initialization
    altered_todo$discovery$todo$status[[1L]] <- "done"
    expect_error(
      revdeprunner:::validate_stock_revdepcheck_initialization(
        altered_todo,
        context
      ),
      "Stock database changed before worker launch",
      fixed = TRUE
    )

    revdeprunner:::stock_namespace_function("db_metadata_set")(
      initialization$paths$checkout,
      "todo",
      "run"
    )
    revdeprunner:::stock_namespace_function("db_disconnect")(
      initialization$paths$checkout
    )
    expect_error(
      revdeprunner:::validate_stock_revdepcheck_initialization(
        initialization,
        context
      ),
      "Stock database changed before worker launch",
      fixed = TRUE
    )
    revdeprunner:::stock_namespace_function("db_metadata_set")(
      initialization$paths$checkout,
      "todo",
      "install"
    )
    revdeprunner:::stock_namespace_function("db_disconnect")(
      initialization$paths$checkout
    )

    cache_archive <- file.path(
      initialization$paths$binary_contrib,
      initialization$binary_manifest$archive_name[[1L]]
    )
    cat("tampered", file = cache_archive, append = TRUE)
    expect_error(
      revdeprunner:::validate_stock_revdepcheck_initialization(
        initialization,
        context
      ),
      "Stock cache artifact evidence changed before comparison",
      fixed = TRUE
    )
    projection_archive <- file.path(
      fixture$ready$projection$repository_path,
      "src",
      "contrib",
      initialization$binary_manifest$archive_name[[1L]]
    )
    expect_true(file.copy(
      projection_archive,
      cache_archive,
      overwrite = TRUE,
      copy.date = FALSE
    ))

    wrong_dependencies <- stats::setNames(
      rep(list("WrongPkg"), nrow(initialization$requested_targets)),
      initialization$requested_targets$package
    )
    expect_error(
      revdeprunner:::stock_dependencies_from_observation(
        wrong_dependencies,
        initialization$requested_targets$package,
        context$universe
      ),
      "Stock dependency requests differ from the frozen universe",
      fixed = TRUE
    )
    expect_invisible(
      revdeprunner:::validate_stock_revdepcheck_initialization(
        initialization,
        context
      )
    )
  })

  test_that("stock compiler wrappers count and propagate real tool exits", {
    root <- tempfile("stock-compiler-wrapper-")
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    wrapper_root <- file.path(root, "bin")
    dir.create(wrapper_root)
    log <- file.path(root, "compiler.log")
    file.create(log)
    tools <- revdeprunner:::create_stock_compiler_wrappers(
      file.path(R.home("bin"), "R"),
      wrapper_root,
      log
    )
    wrapper <- tools$wrapper[tools$configuration == "CC"][[1L]]

    expect_identical(
      system2(wrapper, "--version", stdout = FALSE, stderr = FALSE),
      0L
    )
    expect_true(
      system2(
        wrapper,
        "--revdeprunner-invalid-option",
        stdout = FALSE,
        stderr = FALSE
      ) !=
        0L
    )
    expect_true(
      system2(
        wrapper,
        c("-c", "revdeprunner-missing-source.c"),
        stdout = FALSE,
        stderr = FALSE
      ) !=
        0L
    )
    evidence <- revdeprunner:::stock_adapter_compiler_evidence(log)
    expect_identical(evidence$invocation_count, 3L)
    expect_identical(evidence$probe_count, 0L)
    expect_identical(evidence$compilation_count, 1L)
    expect_true(all(startsWith(
      evidence$invocations,
      tools$command[tools$configuration == "CC"]
    )))
  })
}
# nolint end
