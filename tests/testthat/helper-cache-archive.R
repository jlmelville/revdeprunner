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
    "Package: cachefixture",
    file.path(package_root, "DESCRIPTION")
  )
  package_root
}
