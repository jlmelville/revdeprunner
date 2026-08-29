compatibility_lane_schema_version <- function() {
  "revdeprunner-compatibility-lane/v1"
}

artifact_identity_schema_version <- function() {
  "revdeprunner-artifact-identity/v1"
}

new_compatibility_lane <- function(
  r_major_minor,
  r_platform,
  architecture,
  os_abi,
  toolchain_tag
) {
  r_major_minor <- validate_r_major_minor(r_major_minor)
  r_platform <- validate_contract_token(r_platform, "r_platform")
  architecture <- validate_contract_token(architecture, "architecture")
  os_abi <- validate_compatibility_tag(os_abi, "os_abi")
  toolchain_tag <- validate_compatibility_tag(toolchain_tag, "toolchain_tag")
  schema_version <- compatibility_lane_schema_version()
  fields <- c(
    r_major_minor = r_major_minor,
    r_platform = r_platform,
    architecture = architecture,
    os_abi = os_abi,
    toolchain_tag = toolchain_tag
  )

  lane <- structure(
    list(
      schema_version = schema_version,
      lane_id = record_identity(schema_version, fields),
      r_major_minor = r_major_minor,
      r_platform = r_platform,
      architecture = architecture,
      os_abi = os_abi,
      toolchain_tag = toolchain_tag
    ),
    class = "revdeprunner_compatibility_lane"
  )
  validate_compatibility_lane(lane)
  lane
}

validate_compatibility_lane <- function(lane) {
  fields <- c(
    "schema_version",
    "lane_id",
    "r_major_minor",
    "r_platform",
    "architecture",
    "os_abi",
    "toolchain_tag"
  )
  validate_contract_record(
    lane,
    fields,
    "revdeprunner_compatibility_lane",
    "compatibility lane"
  )
  if (!identical(lane$schema_version, compatibility_lane_schema_version())) {
    stop("Compatibility lane schema version is unsupported.", call. = FALSE)
  }

  validate_sha256_identity(lane$lane_id, "lane_id")
  validate_r_major_minor(lane$r_major_minor)
  validate_contract_token(lane$r_platform, "r_platform")
  validate_contract_token(lane$architecture, "architecture")
  validate_compatibility_tag(lane$os_abi, "os_abi")
  validate_compatibility_tag(lane$toolchain_tag, "toolchain_tag")

  identity_fields <- unlist(
    lane[c(
      "r_major_minor",
      "r_platform",
      "architecture",
      "os_abi",
      "toolchain_tag"
    )],
    use.names = TRUE
  )
  expected <- record_identity(lane$schema_version, identity_fields)
  if (!identical(lane$lane_id, expected)) {
    stop(
      "Compatibility lane identity does not match its fields.",
      call. = FALSE
    )
  }

  invisible(lane)
}

new_artifact_identity <- function(
  package,
  version,
  sha256,
  archive_type,
  lane = NULL
) {
  package <- validate_package_name(package)
  version <- validate_package_version(version)
  sha256 <- validate_sha256(sha256, "sha256")
  archive_type <- validate_archive_type(archive_type)

  if (identical(archive_type, "binary")) {
    if (is.null(lane)) {
      stop("Binary artifacts require a compatibility lane.", call. = FALSE)
    }
    validate_compatibility_lane(lane)
    lane_id <- lane$lane_id
  } else {
    if (!is.null(lane)) {
      stop(
        "Source artifacts must not have a compatibility lane.",
        call. = FALSE
      )
    }
    lane_id <- NA_character_
  }

  schema_version <- artifact_identity_schema_version()
  fields <- c(
    package = package,
    version = version,
    archive_type = archive_type,
    sha256 = sha256,
    lane_id = lane_id
  )
  artifact <- structure(
    list(
      schema_version = schema_version,
      artifact_id = record_identity(schema_version, fields),
      package = package,
      version = version,
      archive_type = archive_type,
      sha256 = sha256,
      lane_id = lane_id
    ),
    class = "revdeprunner_artifact_identity"
  )
  validate_artifact_identity(artifact)
  artifact
}

validate_artifact_identity <- function(artifact) {
  fields <- c(
    "schema_version",
    "artifact_id",
    "package",
    "version",
    "archive_type",
    "sha256",
    "lane_id"
  )
  validate_contract_record(
    artifact,
    fields,
    "revdeprunner_artifact_identity",
    "artifact identity",
    allow_na = "lane_id"
  )
  if (!identical(artifact$schema_version, artifact_identity_schema_version())) {
    stop("Artifact identity schema version is unsupported.", call. = FALSE)
  }

  validate_sha256_identity(artifact$artifact_id, "artifact_id")
  validate_package_name(artifact$package)
  validate_package_version(artifact$version)
  validate_archive_type(artifact$archive_type)
  validate_sha256(artifact$sha256, "sha256")

  if (identical(artifact$archive_type, "binary")) {
    validate_sha256_identity(artifact$lane_id, "lane_id")
  } else if (!is.na(artifact$lane_id)) {
    stop("Source artifact identity must not contain a lane.", call. = FALSE)
  }

  identity_fields <- unlist(
    artifact[c("package", "version", "archive_type", "sha256", "lane_id")],
    use.names = TRUE
  )
  expected <- record_identity(artifact$schema_version, identity_fields)
  if (!identical(artifact$artifact_id, expected)) {
    stop("Artifact identity does not match its fields.", call. = FALSE)
  }

  invisible(artifact)
}

validate_contract_record <- function(
  record,
  fields,
  class_name,
  label,
  allow_na = character()
) {
  if (
    !inherits(record, class_name) ||
      !is.list(record) ||
      !identical(names(record), fields)
  ) {
    stop(
      sprintf("The %s record has an invalid structure.", label),
      call. = FALSE
    )
  }

  valid <- vapply(
    fields,
    function(field) {
      value <- record[[field]]
      is.character(value) &&
        length(value) == 1L &&
        (!is.na(value) || field %in% allow_na)
    },
    logical(1L)
  )
  if (!all(valid)) {
    stop(
      sprintf("The %s record has invalid field types.", label),
      call. = FALSE
    )
  }

  invisible(record)
}

validate_package_name <- function(package) {
  package <- validate_contract_text(package, "package")
  if (!grepl("^[A-Za-z][A-Za-z0-9.]*[A-Za-z0-9]$", package)) {
    stop("`package` is not a valid package name.", call. = FALSE)
  }

  package
}

validate_package_version <- function(version) {
  version <- validate_contract_text(version, "version")
  valid <- tryCatch(
    {
      package_version(version)
      TRUE
    },
    error = function(error) FALSE,
    warning = function(warning) FALSE
  )
  if (!valid) {
    stop("`version` is not a valid package version.", call. = FALSE)
  }

  version
}

validate_archive_type <- function(archive_type) {
  archive_type <- validate_contract_text(archive_type, "archive_type")
  if (!archive_type %in% c("source", "binary")) {
    stop("`archive_type` must be `source` or `binary`.", call. = FALSE)
  }

  archive_type
}

validate_r_major_minor <- function(r_major_minor) {
  r_major_minor <- validate_contract_text(r_major_minor, "r_major_minor")
  if (!grepl("^(0|[1-9][0-9]*)\\.(0|[1-9][0-9]*)$", r_major_minor)) {
    stop("`r_major_minor` must have `major.minor` form.", call. = FALSE)
  }

  r_major_minor
}

validate_contract_token <- function(value, argument) {
  value <- validate_contract_text(value, argument)
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9._:+-]*$", value)) {
    stop(
      sprintf("`%s` must be a portable non-empty token.", argument),
      call. = FALSE
    )
  }

  value
}

validate_compatibility_tag <- function(value, argument) {
  value <- validate_contract_token(value, argument)
  if (tolower(value) %in% c("unknown", "unspecified", "default")) {
    stop(
      sprintf(
        "`%s` must identify a specific compatibility boundary.",
        argument
      ),
      call. = FALSE
    )
  }

  value
}

validate_contract_text <- function(value, argument) {
  if (
    !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(value) ||
      !identical(value, trimws(value)) ||
      grepl("[[:cntrl:]]", value)
  ) {
    stop(sprintf("`%s` must be one non-empty string.", argument), call. = FALSE)
  }

  enc2utf8(value)
}

validate_sha256 <- function(value, argument) {
  value <- validate_contract_text(value, argument)
  if (!grepl("^[a-f0-9]{64}$", value)) {
    stop(sprintf("`%s` must be a lowercase SHA-256.", argument), call. = FALSE)
  }

  value
}

validate_sha256_identity <- function(value, argument) {
  value <- validate_contract_text(value, argument)
  if (!grepl("^sha256:[a-f0-9]{64}$", value)) {
    stop(
      sprintf("`%s` must be a SHA-256 record identity.", argument),
      call. = FALSE
    )
  }

  value
}

record_identity <- function(schema_version, fields) {
  paste0(
    "sha256:",
    digest::digest(
      charToRaw(enc2utf8(canonical_record_key(schema_version, fields))),
      algo = "sha256",
      serialize = FALSE
    )
  )
}

canonical_record_key <- function(schema_version, fields) {
  schema_version <- validate_contract_text(schema_version, "schema_version")
  if (
    !is.character(fields) ||
      is.null(names(fields)) ||
      any(!nzchar(names(fields))) ||
      anyDuplicated(names(fields))
  ) {
    stop(
      "Canonical record fields must be a uniquely named character vector.",
      call. = FALSE
    )
  }

  fields <- c(schema_version = schema_version, fields)
  encoded <- vapply(
    seq_along(fields),
    function(index)
      canonical_record_field(names(fields)[[index]], fields[[index]]),
    character(1L)
  )
  paste0(encoded, collapse = "\n")
}

canonical_record_field <- function(name, value) {
  name_length <- length(charToRaw(enc2utf8(name)))
  if (is.na(value)) {
    return(paste0(name_length, ":", name, "=-1:"))
  }

  value <- validate_contract_text(value, name)
  value_length <- length(charToRaw(enc2utf8(value)))
  paste0(name_length, ":", name, "=", value_length, ":", value)
}
