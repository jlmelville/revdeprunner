# These private tests protect the pinned stock-runner bridge, its resumable
# pre-worker boundary, and its decision-relevant run evidence.

stock_fixture_database <- function() {
  database <- source_acquisition_fixture_database()
  primary <- source_acquisition_fixture_repositories()[["CRAN"]]
  database$Version[
    database$Package == "BuildPkg" & database$Repository == primary
  ] <- "2.0-1"
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
    build_imports = "SubjectPkg"
  )
  source_cache <- file.path(fixture$root, "source-cache")
  dir.create(source_cache)
  fixture$path_plan <- revdeprunner:::new_runtime_root_plan(
    fixture$paths[[1L]],
    fixture$paths[[2L]],
    fixture$paths[[3L]],
    "run-20260829-wp3f",
    c(fixture$paths[[5L]], source_cache)
  )
  requests <- revdeprunner:::preparation_required_packages(
    revdeprunner:::derive_preparation_requirements(
      fixture$download_contracts$universe
    )
  )
  requests <- requests[!is.na(requests$version), , drop = FALSE]
  initial_inventory <- revdeprunner:::write_cache_inventory(
    source_cache,
    fixture$paths[[4L]],
    fixture$paths[[1L]],
    requests
  )$inventory_path
  fixture$binary_reuse <- revdeprunner:::reuse_inventory_binaries(
    requests,
    data.frame(
      inventory_path = initial_inventory,
      lane_id = fixture$lane$lane_id,
      priority = 1L,
      stringsAsFactors = FALSE
    ),
    fixture$lane,
    fixture$path_plan
  )
  fixture$source_plan <- revdeprunner:::new_source_acquisition_plan(
    fixture$download_contracts$universe,
    fixture$download_contracts$cohort,
    fixture$download_contracts$snapshot,
    fixture$binary_reuse,
    fixture$lane,
    fixture$path_plan
  )
  context <- source_preparation_context(fixture)
  bootstrap <- do.call(
    revdeprunner:::prepare_dependency_universe,
    c(
      context,
      list(
        baseline_source = fixture$baseline_source,
        timeout_seconds = 60L
      )
    )
  )
  cache_binary <- file.path(
    fixture$paths[[5L]],
    "cran-bin",
    "src",
    "contrib",
    basename(bootstrap$source_preparations$HitPkg$binary_path)
  )
  dir.create(dirname(cache_binary), recursive = TRUE)
  if (
    !file.copy(
      bootstrap$source_preparations$HitPkg$binary_path,
      cache_binary,
      overwrite = TRUE
    )
  ) {
    stop("Unable to seed the stock fixture binary cache.", call. = FALSE)
  }
  cache_file_binary <- file.path(
    source_cache,
    "cran-bin",
    "src",
    "contrib",
    basename(bootstrap$source_preparations$FilePkg$binary_path)
  )
  dir.create(dirname(cache_file_binary), recursive = TRUE)
  if (
    !file.copy(
      bootstrap$source_preparations$FilePkg$binary_path,
      cache_file_binary,
      overwrite = TRUE
    )
  ) {
    stop("Unable to seed the second stock fixture binary cache.", call. = FALSE)
  }
  cache_source <- file.path(
    source_cache,
    "cran",
    "src",
    "contrib",
    basename(fixture$source_archives$HitPkg)
  )
  dir.create(dirname(cache_source), recursive = TRUE)
  if (!file.copy(fixture$source_archives$HitPkg, cache_source)) {
    stop("Unable to seed the stock fixture source cache.", call. = FALSE)
  }
  inventory_paths <- vapply(
    c(fixture$paths[[5L]], source_cache),
    function(cache_root) {
      revdeprunner:::write_cache_inventory(
        cache_root,
        fixture$paths[[4L]],
        fixture$paths[[1L]],
        requests
      )$inventory_path
    },
    character(1L)
  )
  fixture$binary_reuse <- revdeprunner:::reuse_inventory_binaries(
    requests,
    data.frame(
      inventory_path = inventory_paths,
      lane_id = rep(fixture$lane$lane_id, 2L),
      priority = c(1L, 2L),
      stringsAsFactors = FALSE
    ),
    fixture$lane,
    fixture$path_plan
  )
  fixture$source_plan <- revdeprunner:::new_source_acquisition_plan(
    context$universe,
    context$cohort,
    context$snapshot,
    fixture$binary_reuse,
    fixture$lane,
    fixture$path_plan
  )
  context <- source_preparation_context(fixture)
  fixture$gate <- do.call(
    revdeprunner:::prepare_dependency_universe,
    c(
      context,
      list(
        baseline_source = fixture$baseline_source,
        timeout_seconds = 60L
      )
    )
  )
  write_stock_candidate(context$path_plan$package_root)
  fixture$baseline <- fixture$source_archives$SubjectPkg
  fixture$stock_cached_hit_source <- cache_source
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
      "Imports: HitPkg",
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

test_that("stock worker timeouts use visible preparation evidence", {
  expect_null(
    formals(revdeprunner:::run_stock_revdepcheck)$worker_timeout_seconds
  )
  attempts <- data.frame(
    package = c("FastPkg", "ctsem", "OtherPkg", "ctsem"),
    stage = c("build", "build", "build", "install"),
    outcome = c("success", "success", "success", "success"),
    duration_ms = c("2212", "844422", "1800000", "9000000"),
    stringsAsFactors = FALSE
  )
  initialization <- list(
    preparation_report = list(attempts = attempts),
    requested_targets = data.frame(
      package = c("FastPkg", "ctsem"),
      stringsAsFactors = FALSE
    )
  )

  recommendation <-
    revdeprunner:::stock_adapter_worker_timeout_recommendation(initialization)
  expect_identical(recommendation$seconds, 1800L)
  expect_identical(recommendation$package, "ctsem")
  expect_equal(recommendation$build_seconds, 844.422)
  expect_message(
    automatic <- revdeprunner:::stock_adapter_worker_timeout(
      initialization,
      NULL
    ),
    "1800 seconds.*ctsem preparation build took 844.4 seconds"
  )
  expect_identical(automatic, 1800L)
  expect_message(
    explicit <- revdeprunner:::stock_adapter_worker_timeout(
      initialization,
      600L
    ),
    "600 seconds.*explicit.*automatic recommendation: 1800 seconds"
  )
  expect_identical(explicit, 600L)

  initialization$requested_targets$package <- "FastPkg"
  rounded <-
    revdeprunner:::stock_adapter_worker_timeout_recommendation(initialization)
  expect_identical(rounded$seconds, 600L)
  expect_identical(rounded$package, "FastPkg")

  initialization$preparation_report$attempts <-
    attempts[FALSE, , drop = FALSE]
  fallback <-
    revdeprunner:::stock_adapter_worker_timeout_recommendation(initialization)
  expect_identical(
    fallback,
    list(
      seconds = 600L,
      package = NA_character_,
      build_seconds = NA_real_
    )
  )
  expect_message(
    revdeprunner:::stock_adapter_worker_timeout(initialization, NULL),
    "600 seconds.*automatic fallback"
  )
})

if (!identical(unname(Sys.info()[["sysname"]]), "Linux")) {
  test_that("the stock adapter reports its Linux boundary", {
    expect_error(
      revdeprunner:::require_linux_revdep_runner(),
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
    expect_identical(
      formals(revdeprunner:::initialize_stock_revdepcheck)$workspace,
      "stock-revdepcheck"
    )
    fixture <- make_stock_repository_fixture()
    on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
    context <- source_preparation_context(fixture)
    source_download_file <- revdeprunner:::source_download_file
    validate_initialization <-
      revdeprunner:::validate_stock_revdepcheck_initialization
    source_downloads <- character()
    initialization_validation_modes <- logical()
    local_mocked_bindings(
      validate_stock_revdepcheck_initialization = function(
        initialization,
        context,
        require_pre_worker = TRUE
      ) {
        initialization_validation_modes <<-
          c(initialization_validation_modes, require_pre_worker)
        validate_initialization(initialization, context, require_pre_worker)
      },
      .package = "revdeprunner"
    )

    binary_manifest <- revdeprunner:::stock_binary_manifest(
      fixture$gate,
      context
    )
    wrong_version <- binary_manifest[
      c("package", "version", "archive_name", "sha256")
    ]
    wrong_version$version[wrong_version$package == "BuildPkg"] <- "9.9"
    expect_error(
      revdeprunner:::seed_stock_cache_repository(
        binary_manifest$cache_path,
        wrong_version,
        file.path(fixture$root, "wrong-version-binary-contrib")
      ),
      "indexes differ from frozen artifacts",
      fixed = TRUE
    )
    corrupt_path <- binary_manifest$cache_path[
      binary_manifest$package == "FilePkg"
    ]
    original <- readBin(
      corrupt_path,
      what = "raw",
      n = file.info(corrupt_path, extra_cols = FALSE)$size
    )
    cat("tampered", file = corrupt_path, append = TRUE)
    expect_error(
      revdeprunner:::seed_stock_binary_cache(
        fixture$gate,
        context,
        file.path(fixture$root, "corrupt-binary-contrib")
      ),
      "payload does not match its SHA-256 identity",
      fixed = TRUE
    )
    writeBin(original, corrupt_path)

    initialization <- testthat::with_mocked_bindings(
      revdeprunner:::initialize_stock_revdepcheck(
        fixture$gate,
        context,
        fixture$baseline,
        exclude_targets = "FilePkg",
        workspace = "stock-revdepcheck-fixture"
      ),
      source_download_file = function(url, destination) {
        source_downloads <<- c(source_downloads, basename(url))
        source_download_file(url, destination)
      },
      .package = "revdeprunner"
    )
    expect_identical(initialization_validation_modes, logical())
    expect_identical(source_downloads, "FilePkg_3.0.tar.gz")
    checkpoint <- file.path(fixture$root, "stock-initialization.rds")
    saveRDS(initialization, checkpoint)
    initialization <- readRDS(checkpoint)

    expect_identical(
      basename(initialization$paths$root),
      "stock-revdepcheck-fixture"
    )
    primary <- revdeprunner:::create_stock_adapter_paths(context$path_plan)
    expect_identical(basename(primary$root), "stock-revdepcheck")
    expect_true(dir.exists(initialization$paths$root))
    expect_true(dir.exists(primary$root))
    expect_error(
      revdeprunner:::create_stock_adapter_paths(
        context$path_plan,
        "stock-revdepcheck-fixture"
      ),
      "already exists",
      fixed = TRUE
    )
    before <- sort(list.files(dirname(primary$root)), method = "radix")
    expect_error(
      revdeprunner:::create_stock_adapter_paths(
        context$path_plan,
        "../escape"
      ),
      "portable path component",
      fixed = TRUE
    )
    expect_identical(
      sort(list.files(dirname(primary$root)), method = "radix"),
      before
    )
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
      revdeprunner:::stock_subject_hard_dependencies(
        initialization$preparation_report,
        context,
        initialization$paths$checkout
      ),
      data.frame(
        package = "HitPkg",
        version = "1.0",
        stringsAsFactors = FALSE
      )
    )
    subject_libraries <- revdeprunner:::stock_subject_library_paths(
      initialization$paths,
      initialization$package
    )
    expect_true(all(vapply(
      subject_libraries,
      revdeprunner:::source_preparation_library_has_package,
      logical(1L),
      package = "HitPkg",
      version = "1.0"
    )))
    expect_identical(
      initialization$source_manifest$package,
      c("BuildPkg", "FilePkg", "HitPkg", "SubjectPkg")
    )
    expect_identical(
      initialization$binary_manifest$package,
      c("BuildPkg", "FilePkg", "HitPkg")
    )
    expect_identical(
      initialization$binary_manifest$version[[1L]],
      "2.0-1"
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
    expect_false(any(
      c("projection_before", "cache_before") %in% names(initialization)
    ))
    runtime <- revdeprunner:::observe_stock_runtime(
      initialization$r_executable,
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
    expect_invisible(testthat::with_mocked_bindings(
      revdeprunner:::validate_stock_revdepcheck_initialization(
        initialization,
        context
      ),
      validate_preparation_gate = function(...) {
        stop("deep gate validation was repeated", call. = FALSE)
      },
      validate_preparation_report = function(...) {
        stop("deep report validation was repeated", call. = FALSE)
      },
      validate_stock_baseline_source = function(...) {
        stop("deep baseline validation was repeated", call. = FALSE)
      },
      observe_stock_runtime = function(...) {
        stop("stock runtime probe was repeated", call. = FALSE)
      },
      .package = "revdeprunner"
    ))

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

    initialization_validation_modes <- logical()
    result <- revdeprunner:::run_stock_revdepcheck(
      initialization,
      context,
      worker_timeout_seconds = 60L,
      process_timeout_seconds = 300L
    )
    expect_identical(initialization_validation_modes, c(TRUE, FALSE))

    expect_identical(result$state, "success")
    expect_identical(
      result$results$outcome,
      c("unchanged", "not_checked", "unchanged")
    )
    expect_identical(
      result$diagnostics,
      revdeprunner:::empty_stock_diagnostics()
    )
    expect_identical(result$database$stage, "done")
    expect_true(all(result$database$todo$status == "done"))
    expect_false(any(
      c("projection_after", "cache_after") %in% names(result)
    ))
    expect_false(dir.exists(file.path(fixture$paths[[2L]], "repositories")))
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
    expect_invisible(revdeprunner:::validate_stock_private_libraries(
      initialization,
      result$process,
      result$database
    ))

    library_path <- file.path(
      initialization$paths$checkout,
      "revdep",
      "checks",
      "BuildPkg",
      "old",
      "libraries.txt"
    )
    library_lines <- readLines(library_path, warn = FALSE)
    changed_lines <- sub(
      "^HitPkg [(]1[.]0[)]$",
      "HitPkg (9.9)",
      library_lines
    )
    expect_false(identical(changed_lines, library_lines))
    writeLines(changed_lines, library_path, useBytes = TRUE)
    expect_error(
      revdeprunner:::validate_stock_private_libraries(
        initialization,
        result$process,
        result$database
      ),
      "private-library versions differ from the frozen universe",
      fixed = TRUE
    )
    writeLines(library_lines, library_path, useBytes = TRUE)

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

    cached_hit_source <- fixture$stock_cached_hit_source
    hit_source <- context$source_plan$sources[
      context$source_plan$sources$package == "HitPkg",
      ,
      drop = FALSE
    ]
    inventories <- revdeprunner:::stock_source_inventories(
      context$binary_reuse
    )
    cat("tampered", file = cached_hit_source, append = TRUE)
    expect_null(revdeprunner:::stock_cached_source_for_binary(
      hit_source,
      inventories,
      context$path_plan
    ))
    expect_true(file.copy(
      fixture$source_archives$HitPkg,
      cached_hit_source,
      overwrite = TRUE
    ))
    expect_error(
      testthat::with_mocked_bindings(
        revdeprunner:::stock_acquire_source_for_binary(
          "HitPkg",
          context$source_plan,
          context$path_plan
        ),
        source_download_file = function(...) {
          stop("fixture download failure", call. = FALSE)
        },
        .package = "revdeprunner"
      ),
      "Unable to resolve stock source for HitPkg 1.0: fixture download failure",
      fixed = TRUE
    )
    expect_identical(unlink(cached_hit_source), 0L)
    override_manifest <- revdeprunner:::seed_stock_source_cache(
      fixture$gate,
      initialization$baseline,
      file.path(fixture$root, "override-source-contrib"),
      context,
      source_archives = c(HitPkg = fixture$source_archives$HitPkg)
    )
    expect_identical(
      override_manifest$package,
      c("BuildPkg", "FilePkg", "HitPkg", "SubjectPkg")
    )
    expect_error(
      revdeprunner:::seed_stock_source_cache(
        fixture$gate,
        initialization$baseline,
        file.path(fixture$root, "wrong-override-source-contrib"),
        context,
        source_archives = c(HitPkg = fixture$source_archives$BuildPkg)
      ),
      "Stock source override differs from the frozen source",
      fixed = TRUE
    )
  })

  test_that("stock subject install failures retain their underlying output", {
    fixture <- make_stock_repository_fixture()
    on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
    configure <- file.path(fixture$path_plan$package_root, "configure")
    marker <- "revdeprunner retained subject install failure"
    writeLines(
      c(
        "#!/bin/sh",
        sprintf("printf '%s\\n' %s >&2", "%s", shQuote(marker)),
        "exit 1"
      ),
      configure
    )
    Sys.chmod(configure, mode = "0755")
    context <- source_preparation_context(fixture)
    initialization <- revdeprunner:::initialize_stock_revdepcheck(
      fixture$gate,
      context,
      fixture$baseline,
      exclude_targets = "FilePkg",
      workspace = "stock-install-failure-fixture"
    )

    process <- revdeprunner:::run_stock_revdepcheck_process(
      initialization$r_executable,
      initialization$paths,
      initialization$environment,
      initialization$repository_settings,
      worker_timeout_seconds = 60L,
      process_timeout_seconds = 300L
    )

    expect_false(process$timed_out)
    expect_true(process$status != 0L)
    expect_match(
      paste(
        readLines(initialization$paths$stderr, warn = FALSE),
        collapse = "\n"
      ),
      marker,
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
        fixture$gate,
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

    not_prepared <- fixture$gate$report
    not_prepared$results$outcome[
      not_prepared$results$package == "BuildPkg"
    ] <- "not_checked"
    expect_error(
      revdeprunner:::require_prepared_stock_targets(
        not_prepared,
        context$universe$targets
      ),
      "must have one exact prepared result",
      fixed = TRUE
    )

    unprepared_subject <- fixture$gate$report
    unprepared_subject$results$outcome[
      unprepared_subject$results$package == "HitPkg"
    ] <- "not_checked"
    expect_error(
      revdeprunner:::stock_subject_hard_dependencies(
        unprepared_subject,
        context,
        context$path_plan$package_root
      ),
      "subject hard dependency must be exactly prepared",
      fixed = TRUE
    )

    initialization <- revdeprunner:::initialize_stock_revdepcheck(
      fixture$gate,
      context,
      fixture$baseline
    )

    altered_executable <- initialization
    altered_executable$r_executable <- normalizePath(
      file.path(R.home("bin"), "Rscript"),
      winslash = "/"
    )
    expect_error(
      revdeprunner:::validate_stock_revdepcheck_initialization(
        altered_executable,
        context
      ),
      "contract bindings are inconsistent",
      fixed = TRUE
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

    altered_provenance <- initialization
    altered_provenance$provenance$remote_sha[[1L]] <- paste0(
      "0",
      substring(altered_provenance$provenance$remote_sha[[1L]], 2L)
    )
    expect_error(
      revdeprunner:::validate_stock_revdepcheck_initialization(
        altered_provenance,
        context
      ),
      "Stock tool provenance changed",
      fixed = TRUE
    )

    altered_targets <- initialization
    altered_targets$requested_targets <- altered_targets$requested_targets[
      -1L,
      ,
      drop = FALSE
    ]
    expect_error(
      revdeprunner:::validate_stock_revdepcheck_initialization(
        altered_targets,
        context
      ),
      "requested and excluded targets are inconsistent",
      fixed = TRUE
    )

    binary_index <- file.path(
      initialization$paths$binary_contrib,
      "PACKAGES.rds"
    )
    binary_index_value <- readRDS(binary_index)
    unlink(binary_index)
    expect_error(
      revdeprunner:::validate_stock_revdepcheck_initialization(
        initialization,
        context
      ),
      "indexes are incomplete",
      fixed = TRUE
    )
    saveRDS(binary_index_value, binary_index)

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

  test_that("stock diagnostics summarize bounded incomplete-check evidence", {
    run_root <- tempfile("stock-diagnostics-")
    dir.create(run_root)
    on.exit(unlink(run_root, recursive = TRUE), add = TRUE)
    check_root <- file.path(run_root, "stock-revdepcheck", "checkout")

    make_detail <- function(which, source) {
      checkdir <- file.path(
        check_root,
        "revdep",
        "checks",
        "ctsem",
        which,
        "ctsem.Rcheck"
      )
      dir.create(checkdir, recursive = TRUE)
      check_lines <- c(
        "* checking package dependencies ... OK",
        "* checking whether package 'ctsem' can be installed ..."
      )
      install_lines <- c(
        "* installing *source* package 'ctsem' ...",
        paste("g++ -std=gnu++17 -c", source, "-o output.o"),
        "template warning output"
      )
      writeLines(check_lines, file.path(checkdir, "00check.log"))
      if (which == "old") {
        writeLines(install_lines, file.path(checkdir, "00install.out"))
      }
      structure(
        list(
          stdout = paste(check_lines, collapse = "\n"),
          status = -9L,
          duration = if (which == "old") 600.01 else 600.76,
          timeout = TRUE,
          errors = "R CMD check timed out",
          checkdir = checkdir,
          install_out = paste(install_lines, collapse = "\n")
        ),
        class = "rcmdcheck"
      )
    }

    details <- list(
      old = list(make_detail("old", "stanExports_ctsm.cc")),
      new = make_detail("new", "stanExports_ctsmgen.cc")
    )
    diagnostics <- revdeprunner:::stock_adapter_detail_diagnostics(
      "ctsem",
      details,
      run_root
    )

    expect_identical(diagnostics$package, rep("ctsem", 2L))
    expect_identical(diagnostics$which, c("old", "new"))
    expect_identical(diagnostics$reason, rep("timeout", 2L))
    expect_equal(diagnostics$duration_seconds, c(600.01, 600.76))
    expect_identical(diagnostics$status, rep(-9L, 2L))
    expect_identical(
      diagnostics$last_check,
      rep("checking whether package 'ctsem' can be installed", 2L)
    )
    expect_identical(
      diagnostics$last_compilation,
      c("stanExports_ctsm.cc", "stanExports_ctsmgen.cc")
    )
    expect_identical(
      diagnostics$error_excerpt,
      rep("R CMD check timed out", 2L)
    )
    expect_match(diagnostics$check_log, "^stock-revdepcheck/")
    expect_match(diagnostics$install_log[[1L]], "^stock-revdepcheck/")
    expect_true(is.na(diagnostics$install_log[[2L]]))
  })
}
