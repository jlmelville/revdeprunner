checkout_identity <- function(path, package) {
  description <- file.path(path, "DESCRIPTION")
  if (!utils::file_test("-f", description)) {
    stop("Copied package checkout has no DESCRIPTION.", call. = FALSE)
  }
  record <- read.dcf(description)
  if (
    nrow(record) != 1L ||
      !all(c("Package", "Version") %in% colnames(record)) ||
      !identical(unname(record[1L, "Package"]), package)
  ) {
    stop("Copied package checkout identity is inconsistent.", call. = FALSE)
  }
  manifest <- snapshot_directory(
    path,
    exclude = c(".git", ".Rproj.user", "revdep")
  )
  list(
    package = package,
    version = validate_package_version(unname(record[1L, "Version"])),
    description_sha256 = digest::digest(
      description,
      algo = "sha256",
      file = TRUE,
      serialize = FALSE
    ),
    manifest = manifest
  )
}

snapshot_directory <- function(root, exclude = character()) {
  root <- normalize_runtime_anchor(root, "snapshot root")
  entries <- list.files(
    root,
    all.files = TRUE,
    full.names = TRUE,
    no.. = TRUE
  )
  entries <- entries[!basename(entries) %in% exclude]
  if (any(vapply(entries, path_is_link, logical(1L)))) {
    stop("Snapshot roots must not contain symbolic links.", call. = FALSE)
  }
  paths <- unlist(
    lapply(entries, function(entry) {
      if (!dir.exists(entry)) {
        return(entry)
      }
      list.files(
        entry,
        all.files = TRUE,
        full.names = TRUE,
        recursive = TRUE,
        include.dirs = FALSE,
        no.. = TRUE
      )
    }),
    use.names = FALSE
  )
  if (length(paths) == 0L) {
    return(data.frame(
      relative_path = character(),
      size_bytes = numeric(),
      sha256 = character(),
      stringsAsFactors = FALSE
    ))
  }
  if (any(vapply(paths, path_is_link, logical(1L)))) {
    stop("Snapshot roots must not contain symbolic links.", call. = FALSE)
  }
  info <- file.info(paths, extra_cols = FALSE)
  if (anyNA(info$isdir) || any(info$isdir)) {
    stop("Snapshot roots contain an unreadable entry.", call. = FALSE)
  }
  relative <- substring(paths, nchar(sub("/$", "", root)) + 2L)
  snapshot <- data.frame(
    relative_path = relative,
    size_bytes = unname(info$size),
    sha256 = vapply(
      paths,
      function(path) {
        digest::digest(path, algo = "sha256", file = TRUE, serialize = FALSE)
      },
      character(1L)
    ),
    stringsAsFactors = FALSE
  )
  snapshot <- snapshot[
    order(snapshot$relative_path, method = "radix"),
    ,
    drop = FALSE
  ]
  rownames(snapshot) <- NULL
  snapshot
}
