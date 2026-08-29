observe_cache <- function(cache_root) {
  cache_root <- normalize_cache_root(cache_root)
  paths <- walk_cache_files(cache_root)

  relative_paths <- cache_relative_path(cache_root, paths)
  ordering <- order(relative_paths, method = "radix")
  paths <- paths[ordering]
  relative_paths <- relative_paths[ordering]

  in_meta <- startsWith(relative_paths, "_meta/")
  artifact <- !in_meta & is_package_archive(relative_paths)
  repository_metadata <- in_meta & is_packages_file(relative_paths)

  structure(
    list(
      cache_root = cache_root,
      artifacts = observe_artifacts(
        cache_root,
        paths[artifact],
        relative_paths[artifact]
      ),
      repository_metadata = observe_repository_metadata(
        cache_root,
        paths[repository_metadata],
        relative_paths[repository_metadata]
      )
    ),
    class = "revdeprunner_cache_observation"
  )
}

write_cache_inventory <- function(cache_root, staging_root, package_root) {
  cache_root <- normalize_cache_root(cache_root)
  staging_root <- normalize_existing_directory(staging_root, "staging_root")
  package_root <- normalize_existing_directory(package_root, "package_root")
  if (!file.exists(file.path(package_root, "DESCRIPTION"))) {
    stop("`package_root` must identify an R package checkout.", call. = FALSE)
  }
  validate_inventory_paths(cache_root, staging_root, package_root)

  source_before <- observe_cache_files(cache_root)
  observation <- observe_cache(cache_root)
  payload <- serialize(observation, connection = NULL, version = 3L)
  inventory_sha256 <- digest::digest(
    payload,
    algo = "sha256",
    serialize = FALSE
  )
  source_after <- observe_cache_files(cache_root)

  if (!identical(source_after, source_before)) {
    stop(
      "The cache root changed during inventory serialization.",
      call. = FALSE
    )
  }

  source_sha256 <- digest::digest(
    serialize(source_before, connection = NULL, version = 3L),
    algo = "sha256",
    serialize = FALSE
  )
  cache_id <- digest::digest(
    charToRaw(enc2utf8(cache_root)),
    algo = "sha256",
    serialize = FALSE
  )
  inventory_root <- validated_staging_directory(
    file.path(staging_root, "cache-inventories"),
    staging_root
  )
  inventory_directory <- validated_staging_directory(
    file.path(inventory_root, cache_id),
    staging_root
  )
  inventory_path <- file.path(
    inventory_directory,
    paste0(inventory_sha256, ".rds")
  )

  reused <- publish_inventory_payload(
    inventory_path,
    payload,
    staging_root
  )

  structure(
    list(
      cache_root = cache_root,
      inventory_path = inventory_path,
      inventory_sha256 = inventory_sha256,
      source_sha256 = source_sha256,
      reused = reused
    ),
    class = "revdeprunner_inventory_write"
  )
}

normalize_existing_directory <- function(path, argument) {
  if (
    length(path) != 1L ||
      is.na(path) ||
      !is.character(path) ||
      !nzchar(path)
  ) {
    stop(sprintf("`%s` must be one non-empty path.", argument), call. = FALSE)
  }

  path <- path.expand(path)
  if (!file.exists(path)) {
    stop(
      sprintf("`%s` must identify an existing directory.", argument),
      call. = FALSE
    )
  }
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (!dir.exists(path)) {
    stop(sprintf("`%s` must identify a directory.", argument), call. = FALSE)
  }

  path
}

validate_inventory_paths <- function(cache_root, staging_root, package_root) {
  if (path_trees_overlap(cache_root, package_root)) {
    stop("`cache_root` must not overlap `package_root`.", call. = FALSE)
  }
  if (path_trees_overlap(staging_root, cache_root)) {
    stop("`staging_root` must not overlap `cache_root`.", call. = FALSE)
  }
  if (path_trees_overlap(staging_root, package_root)) {
    stop("`staging_root` must not overlap `package_root`.", call. = FALSE)
  }

  invisible(NULL)
}

path_trees_overlap <- function(first, second) {
  path_is_within(first, second) || path_is_within(second, first)
}

observe_cache_files <- function(cache_root) {
  paths <- walk_cache_files(cache_root)
  relative_paths <- cache_relative_path(cache_root, paths)
  if (length(paths) == 0L) {
    return(empty_cache_file_observations())
  }

  observations <- Map(
    observe_file,
    path = paths,
    relative_path = relative_paths,
    MoreArgs = list(cache_root = cache_root)
  )
  observations <- lapply(observations, as.data.frame, stringsAsFactors = FALSE)
  observations <- do.call(rbind, observations)
  observations[order(observations$relative_path, method = "radix"), ]
}

validated_staging_directory <- function(path, staging_root) {
  link_target <- Sys.readlink(path)
  if (!is.na(link_target) && nzchar(link_target)) {
    stop(
      "Inventory staging directories must not be symbolic links.",
      call. = FALSE
    )
  }
  if (file.exists(path) && !dir.exists(path)) {
    stop("Inventory staging path must identify a directory.", call. = FALSE)
  }
  if (!dir.exists(path) && !dir.create(path, recursive = FALSE)) {
    stop("Unable to create the inventory staging directory.", call. = FALSE)
  }

  resolved_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (!path_is_within(staging_root, resolved_path)) {
    stop("Inventory staging directory escapes `staging_root`.", call. = FALSE)
  }

  resolved_path
}

publish_inventory_payload <- function(inventory_path, payload, staging_root) {
  link_target <- Sys.readlink(inventory_path)
  if (!is.na(link_target) && nzchar(link_target)) {
    stop("Existing inventory must not be a symbolic link.", call. = FALSE)
  }
  if (file.exists(inventory_path)) {
    validate_existing_inventory(inventory_path, payload, staging_root)
    return(TRUE)
  }

  inventory_directory <- dirname(inventory_path)
  temporary_path <- tempfile(
    pattern = ".inventory-",
    tmpdir = inventory_directory,
    fileext = ".tmp"
  )
  on.exit(unlink(temporary_path), add = TRUE)

  connection <- file(temporary_path, open = "wxb")
  on.exit(if (!is.null(connection)) close(connection), add = TRUE)
  writeBin(payload, connection)
  close(connection)
  connection <- NULL

  if (!identical(read_inventory_payload(temporary_path), payload)) {
    stop("Staged inventory verification failed.", call. = FALSE)
  }
  published <- suppressWarnings(file.link(temporary_path, inventory_path))
  if (!published) {
    link_target <- Sys.readlink(inventory_path)
    if (
      file.exists(inventory_path) ||
        (!is.na(link_target) && nzchar(link_target))
    ) {
      validate_existing_inventory(inventory_path, payload, staging_root)
      return(TRUE)
    }
    stop("Unable to publish the staged inventory atomically.", call. = FALSE)
  }

  remove_published <- TRUE
  on.exit(if (remove_published) unlink(inventory_path), add = TRUE)
  validate_existing_inventory(inventory_path, payload, staging_root)
  remove_published <- FALSE
  FALSE
}

validate_existing_inventory <- function(inventory_path, payload, staging_root) {
  link_target <- Sys.readlink(inventory_path)
  if (!is.na(link_target) && nzchar(link_target)) {
    stop("Existing inventory must not be a symbolic link.", call. = FALSE)
  }
  resolved_path <- normalizePath(
    inventory_path,
    winslash = "/",
    mustWork = TRUE
  )
  if (!path_is_within(staging_root, resolved_path)) {
    stop("Existing inventory escapes `staging_root`.", call. = FALSE)
  }
  if (!identical(read_inventory_payload(resolved_path), payload)) {
    stop(
      "Existing content-addressed inventory does not match its identity.",
      call. = FALSE
    )
  }

  invisible(NULL)
}

read_inventory_payload <- function(path) {
  info <- file.info(path, extra_cols = FALSE)
  if (is.na(info$isdir) || info$isdir || is.na(info$size)) {
    stop("Inventory path does not identify a readable file.", call. = FALSE)
  }

  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  payload <- readBin(connection, what = "raw", n = info$size)
  if (length(payload) != info$size) {
    stop("Unable to read the complete inventory payload.", call. = FALSE)
  }

  payload
}

walk_cache_files <- function(cache_root) {
  pending <- cache_root
  files <- character()

  while (length(pending) > 0L) {
    directory <- pending[[1L]]
    pending <- pending[-1L]

    if (file.access(directory, mode = 4L) != 0L) {
      stop(
        sprintf(
          "Cache traversal cannot read directory: %s",
          cache_relative_path(cache_root, directory)
        ),
        call. = FALSE
      )
    }

    entries <- tryCatch(
      list.files(
        directory,
        all.files = TRUE,
        full.names = TRUE,
        recursive = FALSE,
        include.dirs = TRUE,
        no.. = TRUE
      ),
      error = function(error) {
        stop("Cache directory traversal failed.", call. = FALSE)
      },
      warning = function(warning) {
        stop("Cache directory traversal failed.", call. = FALSE)
      }
    )
    if (length(entries) == 0L) {
      next
    }

    info <- file.info(entries, extra_cols = FALSE)
    links <- nzchar(Sys.readlink(entries))
    if (any(is.na(info$isdir))) {
      stop("Cache traversal encountered an unreadable entry.", call. = FALSE)
    }
    if (any(links & info$isdir)) {
      stop(
        "Cache traversal refuses symbolic-link directories.",
        call. = FALSE
      )
    }

    pending <- c(pending, entries[info$isdir])
    files <- c(files, entries[!info$isdir])
  }

  sort(files, method = "radix")
}

normalize_cache_root <- function(cache_root) {
  if (
    length(cache_root) != 1L ||
      is.na(cache_root) ||
      !is.character(cache_root) ||
      !nzchar(cache_root)
  ) {
    stop("`cache_root` must be one non-empty path.", call. = FALSE)
  }

  cache_root <- path.expand(cache_root)
  if (!file.exists(cache_root)) {
    stop("`cache_root` must identify an existing directory.", call. = FALSE)
  }

  cache_root <- normalizePath(
    cache_root,
    winslash = "/",
    mustWork = TRUE
  )

  if (!dir.exists(cache_root)) {
    stop("`cache_root` must identify a directory.", call. = FALSE)
  }

  cache_root
}

cache_relative_path <- function(cache_root, paths) {
  if (length(paths) == 0L) {
    return(character())
  }

  substring(paths, nchar(cache_root) + 2L)
}

is_package_archive <- function(paths) {
  grepl("\\.(?:tar\\.(?:gz|bz2|xz)|tgz|zip)$", paths, ignore.case = TRUE)
}

is_packages_file <- function(paths) {
  grepl("(?:^|/)PACKAGES(?:\\.(?:gz|rds|db))?$", paths, ignore.case = TRUE)
}

observe_artifacts <- function(cache_root, paths, relative_paths) {
  if (length(paths) == 0L) {
    return(empty_artifact_observations())
  }

  observations <- Map(
    observe_artifact,
    path = paths,
    relative_path = relative_paths,
    MoreArgs = list(cache_root = cache_root)
  )
  do.call(rbind, observations)
}

observe_artifact <- function(path, relative_path, cache_root) {
  facts <- observe_file(cache_root, path, relative_path)
  filename_fields <- archive_filename_fields(facts$filename)

  metadata <- list(
    package = filename_fields$package,
    version = filename_fields$version,
    built = NA_character_,
    platform = filename_fields$platform,
    needs_compilation = NA,
    archive_type = "unknown",
    status = facts$status,
    error = facts$error
  )

  if (identical(facts$status, "ok")) {
    metadata <- read_archive_metadata(path, filename_fields)
  }

  data.frame(
    cache_root = facts$cache_root,
    relative_path = facts$relative_path,
    filename = facts$filename,
    size_bytes = facts$size_bytes,
    modified_at = facts$modified_at,
    sha256 = facts$sha256,
    archive_type = metadata$archive_type,
    package = metadata$package,
    version = metadata$version,
    built = metadata$built,
    platform = metadata$platform,
    needs_compilation = metadata$needs_compilation,
    status = metadata$status,
    error = metadata$error,
    stringsAsFactors = FALSE
  )
}

observe_repository_metadata <- function(cache_root, paths, relative_paths) {
  if (length(paths) == 0L) {
    return(empty_repository_metadata_observations())
  }

  observations <- Map(
    observe_file,
    path = paths,
    relative_path = relative_paths,
    MoreArgs = list(cache_root = cache_root)
  )
  observations <- lapply(observations, as.data.frame, stringsAsFactors = FALSE)
  do.call(rbind, observations)
}

observe_file <- function(cache_root, path, relative_path) {
  record <- list(
    cache_root = cache_root,
    relative_path = relative_path,
    filename = basename(path),
    size_bytes = NA_real_,
    modified_at = NA_character_,
    sha256 = NA_character_,
    status = "ok",
    error = NA_character_
  )

  resolved_path <- tryCatch(
    normalizePath(path, winslash = "/", mustWork = TRUE),
    error = function(error) NA_character_,
    warning = function(warning) NA_character_
  )
  if (is.na(resolved_path) || !path_is_within(cache_root, resolved_path)) {
    record$status <- "unreadable_file"
    record$error <- "File does not resolve within the cache root."
    return(record)
  }

  info <- file.info(path, extra_cols = FALSE)
  if (is.na(info$isdir) || info$isdir) {
    record$status <- "unreadable_file"
    record$error <- "File metadata is unavailable or does not describe a file."
    return(record)
  }

  record$size_bytes <- unname(info$size)
  record$modified_at <- format(
    info$mtime,
    format = "%Y-%m-%dT%H:%M:%OS6Z",
    tz = "UTC"
  )
  record$sha256 <- tryCatch(
    digest::digest(path, algo = "sha256", file = TRUE, serialize = FALSE),
    error = function(error) NA_character_,
    warning = function(warning) NA_character_
  )

  if (is.na(record$sha256)) {
    record$status <- "unreadable_file"
    record$error <- "Unable to calculate SHA-256."
  }

  record
}

archive_filename_fields <- function(filename) {
  stem <- sub(
    "\\.(?:tar\\.(?:gz|bz2|xz)|tgz|zip)$",
    "",
    filename,
    ignore.case = TRUE
  )
  binary_match <- regexec("^([^_]+)_([^_]+)_R_(.+)$", stem)
  binary_fields <- regmatches(stem, binary_match)[[1L]]

  if (length(binary_fields) == 4L) {
    return(list(
      package = binary_fields[[2L]],
      version = binary_fields[[3L]],
      platform = binary_fields[[4L]]
    ))
  }

  source_match <- regexec("^([^_]+)_([^_]+)$", stem)
  source_fields <- regmatches(stem, source_match)[[1L]]
  if (length(source_fields) == 3L) {
    return(list(
      package = source_fields[[2L]],
      version = source_fields[[3L]],
      platform = NA_character_
    ))
  }

  list(
    package = NA_character_,
    version = NA_character_,
    platform = NA_character_
  )
}

read_archive_metadata <- function(path, filename_fields) {
  tryCatch(
    {
      description <- read_archive_description(path)
      package <- dcf_field(description, "Package")
      version <- dcf_field(description, "Version")
      built <- dcf_field(description, "Built")
      needs_compilation_field <- dcf_field(description, "NeedsCompilation")
      needs_compilation <- parse_needs_compilation(needs_compilation_field)
      platform <- built_platform(built)

      errors <- character()
      if (is.na(package) || !nzchar(package)) {
        errors <- c(errors, "DESCRIPTION has no Package field.")
      }
      if (is.na(version) || !nzchar(version)) {
        errors <- c(errors, "DESCRIPTION has no Version field.")
      }
      if (!is.na(needs_compilation_field) && is.na(needs_compilation)) {
        errors <- c(
          errors,
          "DESCRIPTION has an invalid NeedsCompilation field."
        )
      }
      if (!is.na(built) && is.na(platform)) {
        errors <- c(errors, "DESCRIPTION has a Built field without a platform.")
      }
      if (
        !is.na(package) &&
          !is.na(filename_fields$package) &&
          !identical(package, filename_fields$package)
      ) {
        errors <- c(errors, "Package field does not match the filename.")
      }
      if (
        !is.na(version) &&
          !is.na(filename_fields$version) &&
          !identical(version, filename_fields$version)
      ) {
        errors <- c(errors, "Version field does not match the filename.")
      }

      list(
        package = value_or_fallback(package, filename_fields$package),
        version = value_or_fallback(version, filename_fields$version),
        built = built,
        platform = value_or_fallback(platform, filename_fields$platform),
        needs_compilation = needs_compilation,
        archive_type = if (is.na(built)) "source" else "binary",
        status = if (length(errors) == 0L) "ok" else "incomplete_metadata",
        error = if (length(errors) == 0L) NA_character_ else
          paste(errors, collapse = " ")
      )
    },
    error = function(error) {
      list(
        package = filename_fields$package,
        version = filename_fields$version,
        built = NA_character_,
        platform = filename_fields$platform,
        needs_compilation = NA,
        archive_type = "unknown",
        status = "unreadable_archive",
        error = conditionMessage(error)
      )
    }
  )
}

read_archive_description <- function(path) {
  description_payload <- if (grepl("\\.zip$", path, ignore.case = TRUE)) {
    read_zip_description(path)
  } else {
    read_tar_description(path)
  }
  description_connection <- rawConnection(description_payload, open = "r")
  on.exit(close(description_connection), add = TRUE)

  description <- tryCatch(
    read.dcf(description_connection),
    error = function(error) {
      stop("Unable to parse the package DESCRIPTION.", call. = FALSE)
    },
    warning = function(warning) {
      stop("Unable to parse the package DESCRIPTION.", call. = FALSE)
    }
  )
  if (nrow(description) != 1L) {
    stop("Package DESCRIPTION must contain exactly one record.", call. = FALSE)
  }

  description
}

read_zip_description <- function(path) {
  listing <- tryCatch(
    utils::unzip(path, list = TRUE),
    error = function(error) {
      stop("Unable to list ZIP archive contents.", call. = FALSE)
    },
    warning = function(warning) {
      stop("Unable to list ZIP archive contents.", call. = FALSE)
    }
  )
  member <- description_archive_member(listing$Name)
  member_size <- listing$Length[match(member, listing$Name)]
  validate_description_size(member_size)

  connection <- tryCatch(
    unz(path, member, open = "rb"),
    error = function(error) {
      stop("Unable to open ZIP package DESCRIPTION.", call. = FALSE)
    }
  )
  on.exit(close(connection), add = TRUE)
  read_exact_raw(connection, member_size, "ZIP package DESCRIPTION")
}

read_tar_description <- function(path) {
  connection <- open_tar_connection(path)
  on.exit(close(connection), add = TRUE)
  description <- NULL

  repeat {
    header <- readBin(connection, what = "raw", n = 512L)
    if (length(header) == 0L || all(header == as.raw(0L))) {
      break
    }
    if (length(header) != 512L) {
      stop("Tar archive has a truncated header.", call. = FALSE)
    }
    validate_tar_checksum(header)

    member <- tar_member_name(header)
    member_size <- tar_octal_field(header, 125L, 12L, "member size")
    type_flag <- tar_text_field(header, 157L, 1L)
    is_description <- is_description_archive_member(member)
    padded_size <- ceiling(member_size / 512) * 512

    if (is_description) {
      if (!type_flag %in% c("", "0")) {
        stop("Package DESCRIPTION must be a regular tar member.", call. = FALSE)
      }
      if (!is.null(description)) {
        stop(
          "Archive must contain exactly one top-level package DESCRIPTION.",
          call. = FALSE
        )
      }
      validate_description_size(member_size)
      description <- read_exact_raw(
        connection,
        member_size,
        "tar package DESCRIPTION"
      )
      skip_exact_raw(connection, padded_size - member_size)
    } else {
      skip_exact_raw(connection, padded_size)
    }
  }

  if (is.null(description)) {
    stop(
      "Archive must contain exactly one top-level package DESCRIPTION.",
      call. = FALSE
    )
  }

  description
}

open_tar_connection <- function(path) {
  if (grepl("\\.(?:tar\\.gz|tgz)$", path, ignore.case = TRUE)) {
    return(gzfile(path, open = "rb"))
  }
  if (grepl("\\.tar\\.bz2$", path, ignore.case = TRUE)) {
    return(bzfile(path, open = "rb"))
  }
  if (grepl("\\.tar\\.xz$", path, ignore.case = TRUE)) {
    return(xzfile(path, open = "rb"))
  }

  stop("Unsupported tar compression format.", call. = FALSE)
}

tar_member_name <- function(header) {
  name <- tar_text_field(header, 1L, 100L)
  prefix <- tar_text_field(header, 346L, 155L)
  if (nzchar(prefix)) paste(prefix, name, sep = "/") else name
}

tar_text_field <- function(header, start, length) {
  field <- header[seq.int(start, length.out = length)]
  terminator <- match(as.raw(0L), field, nomatch = length(field) + 1L)
  field <- field[seq_len(terminator - 1L)]
  if (length(field) == 0L) "" else rawToChar(field)
}

tar_octal_field <- function(header, start, length, label) {
  field <- header[seq.int(start, length.out = length)]
  if (bitwAnd(as.integer(field[[1L]]), 128L) != 0L) {
    stop(
      sprintf("Tar %s uses an unsupported binary encoding.", label),
      call. = FALSE
    )
  }

  value <- trimws(tar_text_field(header, start, length))
  if (!nzchar(value)) {
    return(0)
  }
  if (!grepl("^[0-7]+$", value)) {
    stop(sprintf("Tar %s is not a valid octal value.", label), call. = FALSE)
  }

  digits <- as.integer(strsplit(value, "", fixed = TRUE)[[1L]])
  Reduce(function(total, digit) total * 8 + digit, digits, init = 0)
}

validate_tar_checksum <- function(header) {
  expected <- tar_octal_field(header, 149L, 8L, "header checksum")
  checksum_header <- header
  checksum_header[149L:156L] <- as.raw(32L)
  observed <- sum(as.integer(checksum_header))
  if (observed != expected) {
    stop("Tar archive has an invalid header checksum.", call. = FALSE)
  }

  invisible(NULL)
}

validate_description_size <- function(size) {
  if (length(size) != 1L || is.na(size) || size < 1 || size > 1024^2) {
    stop("Package DESCRIPTION has an invalid size.", call. = FALSE)
  }

  invisible(NULL)
}

read_exact_raw <- function(connection, size, label) {
  bytes <- readBin(connection, what = "raw", n = as.integer(size))
  if (length(bytes) != size) {
    stop(sprintf("Unable to read complete %s.", label), call. = FALSE)
  }

  bytes
}

skip_exact_raw <- function(connection, size) {
  remaining <- size
  while (remaining > 0) {
    chunk_size <- as.integer(min(remaining, 1024^2))
    chunk <- readBin(connection, what = "raw", n = chunk_size)
    if (length(chunk) != chunk_size) {
      stop("Tar archive is truncated.", call. = FALSE)
    }
    remaining <- remaining - chunk_size
  }

  invisible(NULL)
}

description_archive_member <- function(members) {
  safe_description <- is_description_archive_member(members)

  if (sum(safe_description) != 1L) {
    stop(
      "Archive must contain exactly one top-level package DESCRIPTION.",
      call. = FALSE
    )
  }

  members[[which(safe_description)]]
}

is_description_archive_member <- function(members) {
  normalized_members <- gsub("\\\\", "/", members)
  normalized_members <- sub("^(?:\\./)+", "", normalized_members)
  grepl("^[A-Za-z][A-Za-z0-9.]*/DESCRIPTION$", normalized_members)
}

dcf_field <- function(description, field) {
  if (!field %in% colnames(description)) {
    return(NA_character_)
  }

  value <- unname(trimws(description[1L, field]))
  if (is.na(value) || !nzchar(value)) NA_character_ else value
}

parse_needs_compilation <- function(value) {
  if (is.na(value)) {
    return(NA)
  }

  switch(
    tolower(value),
    yes = TRUE,
    no = FALSE,
    NA
  )
}

built_platform <- function(built) {
  if (is.na(built)) {
    return(NA_character_)
  }

  fields <- trimws(strsplit(built, ";", fixed = TRUE)[[1L]])
  if (length(fields) < 2L || !nzchar(fields[[2L]])) {
    return(NA_character_)
  }

  fields[[2L]]
}

value_or_fallback <- function(value, fallback) {
  if (is.na(value) || !nzchar(value)) fallback else value
}

path_is_within <- function(root, path) {
  identical(path, root) || startsWith(path, paste0(sub("/$", "", root), "/"))
}

empty_artifact_observations <- function() {
  data.frame(
    cache_root = character(),
    relative_path = character(),
    filename = character(),
    size_bytes = numeric(),
    modified_at = character(),
    sha256 = character(),
    archive_type = character(),
    package = character(),
    version = character(),
    built = character(),
    platform = character(),
    needs_compilation = logical(),
    status = character(),
    error = character(),
    stringsAsFactors = FALSE
  )
}

empty_repository_metadata_observations <- function() {
  data.frame(
    cache_root = character(),
    relative_path = character(),
    filename = character(),
    size_bytes = numeric(),
    modified_at = character(),
    sha256 = character(),
    status = character(),
    error = character(),
    stringsAsFactors = FALSE
  )
}

empty_cache_file_observations <- function() {
  data.frame(
    cache_root = character(),
    relative_path = character(),
    filename = character(),
    size_bytes = numeric(),
    modified_at = character(),
    sha256 = character(),
    status = character(),
    error = character(),
    stringsAsFactors = FALSE
  )
}
