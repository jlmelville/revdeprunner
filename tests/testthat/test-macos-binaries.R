# These contracts protect platform attribution and the crancache projection on
# every CI host; native integration tests also execute on both supported OSes.
test_that("native pure-R metadata stays unassigned until a local build binds it", {
  root <- tempfile("native-binary-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  lane <- source_preparation_runner_lane()
  built <- paste("R", as.character(getRversion()), "; ; 2026-10-11; unix")
  path <- make_test_archive(
    root,
    "",
    "NativePure",
    "1.0",
    "no",
    built,
    "NativePure_1.0.tgz"
  )
  observations <- revdeprunner:::observe_cache_artifacts(root)
  expect_identical(observations$status, "ok")
  expect_identical(observations$archive_type, "binary")
  expect_true(is.na(observations$platform))
  expect_equal(
    nrow(revdeprunner:::collect_cached_binary_candidates(
      observations,
      "NativePure",
      "1.0",
      lane
    )),
    0L
  )
  before <- unname(tools::md5sum(path))
  named <- revdeprunner:::name_source_preparation_binary(
    path,
    "NativePure",
    "1.0",
    lane
  )
  expect_identical(unname(tools::md5sum(named)), before)
  expect_false(file.exists(path))
  expect_identical(
    basename(named),
    paste0("NativePure_1.0_R_", lane$r_platform, ".tgz")
  )
  observations <- revdeprunner:::observe_cache_artifacts(root)
  expect_identical(observations$platform, lane$r_platform)
  expect_equal(
    nrow(revdeprunner:::collect_cached_binary_candidates(
      observations,
      "NativePure",
      "1.0",
      lane
    )),
    1L
  )
  wrong_r <- lane
  wrong_r$r_major_minor <- "0.0"
  wrong_platform <- lane
  wrong_platform$r_platform <- "other-platform"
  for (wrong in list(wrong_r, wrong_platform)) {
    expect_equal(
      nrow(revdeprunner:::collect_cached_binary_candidates(
        observations,
        "NativePure",
        "1.0",
        wrong
      )),
      0L
    )
  }
})

test_that("compiled and uncertain native binaries need matching platform evidence", {
  root <- tempfile("native-compiled-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  lane <- source_preparation_runner_lane()
  built <- paste("R", as.character(getRversion()), "; ; 2026-10-11; unix")
  for (compilation in c("yes", NA_character_)) {
    path <- make_test_archive(
      root,
      "",
      "NativeCompiled",
      "1.0",
      compilation,
      built,
      "NativeCompiled_1.0.tgz"
    )
    metadata <- revdeprunner:::read_archive_metadata(
      path,
      revdeprunner:::archive_filename_fields(basename(path))
    )
    expect_identical(metadata$status, "incomplete_metadata")
    expect_error(
      revdeprunner:::name_source_preparation_binary(
        path,
        "NativeCompiled",
        "1.0",
        lane
      ),
      "archive validation failed"
    )
  }
  for (platform in c(lane$r_platform, "other-platform")) {
    path <- make_test_archive(
      root,
      "",
      "NativeCompiled",
      "1.0",
      "yes",
      paste(
        paste("R", as.character(getRversion())),
        platform,
        "2026-10-11",
        "unix",
        sep = "; "
      ),
      "NativeCompiled_1.0.tgz"
    )
    observations <- revdeprunner:::observe_cache_artifacts(root)
    native <- observations[
      observations$filename == "NativeCompiled_1.0.tgz",
      ,
      drop = FALSE
    ]
    expect_equal(
      nrow(revdeprunner:::collect_cached_binary_candidates(
        native,
        "NativeCompiled",
        "1.0",
        lane
      )),
      if (platform == lane$r_platform) 1L else 0L
    )
    if (platform == lane$r_platform) {
      named <- revdeprunner:::name_source_preparation_binary(
        path,
        "NativeCompiled",
        "1.0",
        lane
      )
      metadata <- revdeprunner:::read_archive_metadata(
        named,
        revdeprunner:::archive_filename_fields(basename(named))
      )
      expect_identical(metadata$status, "ok")
      expect_identical(metadata$platform, platform)
    } else {
      expect_error(
        revdeprunner:::name_source_preparation_binary(
          path,
          "NativeCompiled",
          "1.0",
          lane
        ),
        "does not match its compatibility lane"
      )
    }
  }
  path <- make_test_archive(
    root,
    "",
    "Conflict",
    "1.0",
    "yes",
    paste(
      paste("R", as.character(getRversion())),
      "other-platform",
      "2026-10-11",
      "unix",
      sep = "; "
    ),
    paste0("Conflict_1.0_R_", lane$r_platform, ".tgz")
  )
  metadata <- revdeprunner:::read_archive_metadata(
    path,
    revdeprunner:::archive_filename_fields(basename(path))
  )
  expect_identical(metadata$status, "incomplete_metadata")
  expect_match(
    metadata$error,
    "Built platform does not match the filename",
    fixed = TRUE
  )
})

test_that("stock binary projection follows R package type rather than host OS", {
  root <- tempfile("native-cache-")
  expect_identical(
    revdeprunner:::stock_binary_contrib_path(root, "source"),
    file.path(root, "cran-bin", "src", "contrib")
  )
  for (type in c(
    "mac.binary.big-sur-arm64",
    "mac.binary.big-sur-x86_64",
    "mac.binary.sonoma-arm64"
  )) {
    expect_identical(
      revdeprunner:::stock_binary_contrib_path(root, type),
      utils::contrib.url(file.path(root, "cran"), type = type)
    )
    expect_match(
      revdeprunner:::stock_binary_contrib_path(root, type),
      "/cran/bin/macosx/",
      fixed = TRUE
    )
    expect_identical(
      revdeprunner:::stock_binary_archive_name(
        "NativePure",
        "/cache/NativePure_1.0_R_aarch64-apple-darwin20.tgz",
        type
      ),
      "NativePure_1.0.tgz"
    )
    expect_identical(
      revdeprunner:::stock_binary_archive_name(
        "NativeCompiled",
        "/cache/NativeCompiled_1.0.tgz",
        type
      ),
      "NativeCompiled_1.0.tgz"
    )
  }
  expect_identical(
    revdeprunner:::stock_binary_archive_name(
      "LinuxBinary",
      "/cache/LinuxBinary_1.0_R_x86_64-pc-linux-gnu.tar.gz",
      "source"
    ),
    "LinuxBinary_1.0_R_x86_64-pc-linux-gnu.tar.gz"
  )
})

test_that("native macOS cache indexes install the exact prepared tgz", {
  skip_if_not(identical(unname(Sys.info()[["sysname"]]), "Darwin"))
  skip_if_stock_tools_unavailable()
  fixture <- make_source_preparation_fixture(
    missing_binary_packages = "BuildPkg"
  )
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  context <- source_preparation_context(fixture)
  acquisition <- revdeprunner:::acquire_source_artifact_in_context(
    "BuildPkg",
    context$source_plan,
    context$path_plan
  )
  prepared <- revdeprunner:::prepare_source_binary_in_context(
    "BuildPkg",
    context,
    acquisition,
    timeout_seconds = 60L
  )
  expect_identical(prepared$result$outcome, "prepared")
  root <- tempfile("native-cache-")
  dir.create(root)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  type <- if (startsWith(.Platform$pkgType, "mac.binary"))
    .Platform$pkgType else "mac.binary.big-sur-arm64"
  # On Homebrew R exercise the native installer and pinned crancache routing
  # with an isolated package-type observation. Hosted CRAN R needs no override.
  local_mocked_bindings(
    .Platform = modifyList(.Platform, list(pkgType = type)),
    .package = "base"
  )
  cache <- file.path(root, "cache")
  dir.create(cache)
  withr::local_envvar(c(
    CRANCACHE_DIR = cache,
    CRANCACHE_REPOS = "cran",
    CRANCACHE_DISABLE = "",
    CRANCACHE_DISABLE_UPDATES = "yes",
    CRANCACHE_QUIET = "yes"
  ))
  withr::local_options(pkgType = type)
  contrib <- revdeprunner:::stock_binary_contrib_path(cache)
  expected <- data.frame(
    package = "BuildPkg",
    version = prepared$version,
    archive_name = revdeprunner:::stock_binary_archive_name(
      "BuildPkg",
      prepared$binary_path
    ),
    sha256 = prepared$binary_artifact$sha256,
    stringsAsFactors = FALSE
  )
  revdeprunner:::seed_stock_cache_repository(
    prepared$binary_path,
    expected,
    contrib
  )
  empty <- file.path(root, "empty", "cran")
  revdeprunner:::initialize_stock_empty_contribs(empty)
  expect_invisible(revdeprunner:::validate_stock_empty_repositories(dirname(
    empty
  )))
  repo <- crancache:::get_crancache_repos(type = "binary")[["cran"]]
  expect_identical(
    normalizePath(
      sub("^file://", "", utils::contrib.url(repo, type = type)),
      winslash = "/"
    ),
    contrib
  )
  available <- utils::available.packages(
    contriburl = utils::contrib.url(repo, type = type),
    type = type,
    filters = list()
  )
  expect_identical(unname(available["BuildPkg", "Version"]), prepared$version)
  library <- file.path(root, "library")
  dir.create(library)
  expect_no_error(crancache::install_packages(
    "BuildPkg",
    lib = library,
    repos = paste0("file://", empty),
    type = type,
    dependencies = FALSE,
    quiet = TRUE
  ))
  expect_identical(
    as.character(utils::packageVersion("BuildPkg", lib.loc = library)),
    prepared$version
  )
  expect_true(callr::r(
    function(lib) {
      loadNamespace("BuildPkg", lib.loc = lib)
      .C("buildpkg_noop", PACKAGE = "BuildPkg")
      "BuildPkg" %in% names(getLoadedDLLs())
    },
    args = list(lib = library)
  ))
  # Admission must verify native empty indexes too, not just src/contrib.
  native_index <- file.path(
    utils::contrib.url(empty, type = type),
    "PACKAGES.rds"
  )
  saveRDS(
    matrix(
      c("Unexpected", "1.0"),
      nrow = 1L,
      dimnames = list(NULL, c("Package", "Version"))
    ),
    native_index
  )
  expect_error(
    revdeprunner:::validate_stock_empty_repositories(dirname(empty)),
    "not empty"
  )
})
