make_test_cache <- function() {
  cache_root <- tempfile("artifact-cache-")
  dir.create(cache_root)

  make_test_archive(
    cache_root,
    repository = "cran/src/contrib",
    package = "pureR",
    version = "1.0.0",
    needs_compilation = "no"
  )
  make_test_archive(
    cache_root,
    repository = "cran-bin/src/contrib",
    package = "compiled",
    version = "2.0.0",
    needs_compilation = "yes",
    built = "R 4.5.2; x86_64-pc-linux-gnu; 2026-08-29; unix",
    filename = "compiled_2.0.0_R_x86_64-pc-linux-gnu.tar.gz"
  )
  make_test_archive(
    cache_root,
    repository = "cran/src/contrib",
    package = NULL,
    version = "4.0.0",
    needs_compilation = "sometimes",
    filename = "incomplete_4.0.0.tar.gz"
  )

  broken <- file.path(cache_root, "cran/src/contrib/broken_3.0.0.tar.gz")
  writeBin(charToRaw("not an archive"), broken)

  metadata_root <- file.path(cache_root, "_meta/example")
  dir.create(metadata_root, recursive = TRUE)
  writeLines("Package: example", file.path(metadata_root, "PACKAGES.gz"))
  saveRDS(list(package = "example"), file.path(metadata_root, "PACKAGES.rds"))

  cache_root
}

make_test_archive <- function(
  cache_root,
  repository,
  package,
  version,
  needs_compilation,
  built = NULL,
  filename = NULL
) {
  staging_root <- tempfile("package-staging-")
  dir.create(staging_root)
  old_working_directory <- getwd()
  on.exit(
    {
      setwd(old_working_directory)
      unlink(staging_root, recursive = TRUE)
    },
    add = TRUE
  )

  package_directory <- if (is.null(package)) "incomplete" else package
  package_root <- file.path(staging_root, package_directory)
  dir.create(package_root)

  description <- c(
    if (!is.null(package)) paste("Package:", package),
    paste("Version:", version),
    paste("NeedsCompilation:", needs_compilation),
    if (!is.null(built)) paste("Built:", built)
  )
  writeLines(description, file.path(package_root, "DESCRIPTION"))

  repository_root <- file.path(cache_root, repository)
  dir.create(repository_root, recursive = TRUE, showWarnings = FALSE)
  if (is.null(filename)) {
    filename <- paste0(package, "_", version, ".tar.gz")
  }
  archive <- file.path(repository_root, filename)

  setwd(staging_root)
  if (grepl("\\.zip$", archive, ignore.case = TRUE)) {
    invisible(capture.output(
      utils::zip(archive, files = package_directory, flags = "-rq9")
    ))
  } else {
    utils::tar(
      archive,
      files = package_directory,
      compression = "gzip",
      tar = "internal"
    )
  }

  invisible(archive)
}

make_linked_test_archive <- function(cache_root, description_is_link = FALSE) {
  staging_root <- tempfile("linked-package-staging-")
  dir.create(staging_root)
  old_working_directory <- getwd()
  on.exit(
    {
      setwd(old_working_directory)
      unlink(staging_root, recursive = TRUE)
    },
    add = TRUE
  )

  package <- if (description_is_link) "linkeddesc" else "linked"
  package_root <- file.path(staging_root, package)
  dir.create(package_root)
  target <- tempfile("external-link-target-")
  writeLines(
    c(
      paste("Package:", package),
      "Version: 1.0.0",
      "NeedsCompilation: no"
    ),
    target
  )

  if (description_is_link) {
    supported <- file.symlink(target, file.path(package_root, "DESCRIPTION"))
  } else {
    writeLines(
      c("Package: linked", "Version: 1.0.0", "NeedsCompilation: no"),
      file.path(package_root, "DESCRIPTION")
    )
    supported <- all(
      file.symlink(target, file.path(package_root, "symbolic-link")),
      file.link(target, file.path(package_root, "hard-link"))
    )
  }

  repository_root <- file.path(cache_root, "other/src/contrib")
  dir.create(repository_root, recursive = TRUE, showWarnings = FALSE)
  archive <- file.path(repository_root, paste0(package, "_1.0.0.tar.gz"))
  setwd(staging_root)
  utils::tar(
    archive,
    files = package,
    compression = "gzip",
    tar = Sys.which("tar")
  )

  list(archive = archive, target = target, supported = supported)
}

snapshot_test_cache <- function(cache_root) {
  paths <- list.files(
    cache_root,
    all.files = TRUE,
    full.names = TRUE,
    recursive = TRUE,
    include.dirs = TRUE,
    no.. = TRUE
  )
  info <- file.info(paths, extra_cols = FALSE)
  files <- !info$isdir
  hashes <- rep(NA_character_, length(paths))
  hashes[files] <- vapply(
    paths[files],
    digest::digest,
    character(1L),
    algo = "sha256",
    file = TRUE,
    serialize = FALSE
  )

  data.frame(
    path = substring(paths, nchar(cache_root) + 2L),
    size = info$size,
    modified = as.numeric(info$mtime),
    mode = info$mode,
    sha256 = hashes,
    stringsAsFactors = FALSE
  )
}

make_test_package_root <- function() {
  package_root <- tempfile("package-root-")
  dir.create(package_root)
  writeLines(
    "Package: inventoryfixture",
    file.path(package_root, "DESCRIPTION")
  )
  package_root
}

make_report_artifact <- function(
  cache_root,
  package,
  version,
  sha256,
  archive_type = "source",
  built = NA_character_,
  platform = NA_character_,
  status = "ok",
  error = NA_character_,
  suffix = "tar.gz"
) {
  filename <- paste0(package, "_", version, ".", suffix)
  data.frame(
    cache_root = cache_root,
    relative_path = file.path("cran", "src", "contrib", filename),
    filename = filename,
    size_bytes = 1,
    modified_at = "2026-08-29T00:00:00.000000Z",
    sha256 = sha256,
    archive_type = archive_type,
    package = package,
    version = version,
    built = built,
    platform = platform,
    needs_compilation = FALSE,
    status = status,
    error = error,
    stringsAsFactors = FALSE
  )
}

make_report_observation <- function(cache_root, artifacts) {
  structure(
    list(
      cache_root = cache_root,
      artifacts = artifacts,
      repository_metadata = revdeprunner:::empty_repository_metadata_observations()
    ),
    class = "revdeprunner_cache_observation"
  )
}

write_report_inventory <- function(observation, directory) {
  payload <- serialize(observation, connection = NULL, version = 3L)
  sha256 <- digest::digest(payload, algo = "sha256", serialize = FALSE)
  path <- file.path(directory, paste0(sha256, ".rds"))
  writeBin(payload, path)
  path
}

make_report_inventory_fixture <- function() {
  fixture_root <- tempfile("inventory-report-")
  inventory_root <- file.path(fixture_root, "inventories")
  dir.create(inventory_root, recursive = TRUE)
  cache_roots <- file.path(fixture_root, paste0("cache-", c("a", "b", "c")))
  invisible(lapply(cache_roots, dir.create))
  invisible(lapply(cache_roots, function(path) {
    writeLines("source remains unchanged", file.path(path, "marker"))
  }))

  hash <- function(character) paste(rep(character, 64L), collapse = "")
  root_a <- do.call(
    rbind,
    list(
      make_report_artifact(cache_roots[[1L]], "duplicate", "1.0", hash("a")),
      make_report_artifact(cache_roots[[1L]], "collision", "1.0", hash("b")),
      make_report_artifact(
        cache_roots[[1L]],
        "broken",
        "1.0",
        hash("c"),
        archive_type = "unknown",
        status = "unreadable_archive",
        error = "Unable to read archive."
      ),
      make_report_artifact(
        cache_roots[[1L]],
        "incomplete",
        "1.0",
        hash("d"),
        status = "incomplete_metadata",
        error = "DESCRIPTION metadata is incomplete."
      ),
      make_report_artifact(
        cache_roots[[1L]],
        "rconflict",
        "1.0",
        hash("e"),
        archive_type = "binary",
        built = "R 4.4.3; x86_64-pc-linux-gnu; date; unix",
        platform = "x86_64-pc-linux-gnu"
      ),
      make_report_artifact(
        cache_roots[[1L]],
        "platformconflict",
        "1.0",
        hash("f"),
        archive_type = "binary",
        built = "R 4.5.2; x86_64-pc-linux-gnu; date; unix",
        platform = "x86_64-pc-linux-gnu"
      ),
      make_report_artifact(
        cache_roots[[1L]],
        "missingdimension",
        "1.0",
        hash("1"),
        archive_type = "binary",
        built = "development; x86_64-pc-linux-gnu; date; unix",
        platform = "x86_64-pc-linux-gnu"
      ),
      make_report_artifact(cache_roots[[1L]], "mixed", "1.0", hash("2"))
    )
  )
  root_b <- do.call(
    rbind,
    list(
      make_report_artifact(cache_roots[[2L]], "duplicate", "1.0", hash("a")),
      make_report_artifact(cache_roots[[2L]], "collision", "1.0", hash("9")),
      make_report_artifact(
        cache_roots[[2L]],
        "rconflict",
        "1.0",
        hash("8"),
        archive_type = "binary",
        built = "R 4.5.2; x86_64-pc-linux-gnu; date; unix",
        platform = "x86_64-pc-linux-gnu"
      ),
      make_report_artifact(
        cache_roots[[2L]],
        "platformconflict",
        "1.0",
        hash("7"),
        archive_type = "binary",
        built = "R 4.5.2; x86_64-w64-mingw32; date; windows",
        platform = "x86_64-w64-mingw32",
        suffix = "zip"
      ),
      make_report_artifact(
        cache_roots[[2L]],
        "missingdimension",
        "1.0",
        hash("6"),
        archive_type = "binary",
        built = "R 4.5.2; x86_64-pc-linux-gnu; date; unix",
        platform = "x86_64-pc-linux-gnu"
      ),
      make_report_artifact(
        cache_roots[[2L]],
        "mixed",
        "1.0",
        hash("5"),
        archive_type = "binary",
        built = "R 4.5.2; x86_64-pc-linux-gnu; date; unix",
        platform = "x86_64-pc-linux-gnu"
      )
    )
  )
  root_c <- make_report_artifact(
    cache_roots[[3L]],
    "duplicate",
    "1.0",
    hash("a")
  )

  observations <- list(
    make_report_observation(cache_roots[[1L]], root_a),
    make_report_observation(cache_roots[[2L]], root_b),
    make_report_observation(cache_roots[[3L]], root_c)
  )
  inventory_paths <- vapply(
    observations,
    write_report_inventory,
    character(1L),
    directory = inventory_root
  )

  list(
    root = fixture_root,
    cache_roots = cache_roots,
    inventory_paths = inventory_paths,
    hashes = vapply(c("a", "b", "c", "d"), hash, character(1L))
  )
}

snapshot_report_inputs <- function(fixture) {
  paths <- c(
    fixture$inventory_paths,
    file.path(fixture$cache_roots, "marker")
  )
  info <- file.info(paths, extra_cols = FALSE)
  data.frame(
    path = paths,
    size_bytes = info$size,
    modified_at = as.numeric(info$mtime),
    sha256 = vapply(
      paths,
      digest::digest,
      character(1L),
      algo = "sha256",
      file = TRUE,
      serialize = FALSE
    ),
    stringsAsFactors = FALSE
  )
}
