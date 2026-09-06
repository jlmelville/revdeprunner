# These private tests protect artifact identity and malformed machine-facing records.

test_that("source artifact identities are deterministic and lane-free", {
  sha256 <- paste(rep("a", 64L), collapse = "")

  artifact <- revdeprunner:::new_artifact_identity(
    package = "example.pkg",
    version = "1.2-3",
    sha256 = sha256,
    archive_type = "source"
  )
  repeated <- revdeprunner:::new_artifact_identity(
    package = "example.pkg",
    version = "1.2-3",
    sha256 = sha256,
    archive_type = "source"
  )

  expect_s3_class(artifact, "revdeprunner_artifact_identity")
  expect_identical(artifact, repeated)
  expect_identical(
    names(artifact),
    c(
      "schema_version",
      "artifact_id",
      "package",
      "version",
      "archive_type",
      "sha256",
      "lane_id"
    )
  )
  expect_identical(
    artifact$schema_version,
    "revdeprunner-artifact-identity/v1"
  )
  expect_match(artifact$artifact_id, "^sha256:[a-f0-9]{64}$")
  expect_identical(artifact$version, "1.2-3")
  expect_true(is.na(artifact$lane_id))
  expect_invisible(revdeprunner:::validate_artifact_identity(artifact))
})

test_that("binary identities require complete explicit compatibility lanes", {
  lane <- revdeprunner:::new_compatibility_lane(
    r_major_minor = "4.5",
    r_platform = "x86_64-pc-linux-gnu",
    architecture = "x86_64",
    os_abi = "linux-glibc-2.39",
    toolchain_tag = "gcc-15.2.1-gfortran-15.2.1"
  )
  repeated <- revdeprunner:::new_compatibility_lane(
    r_major_minor = "4.5",
    r_platform = "x86_64-pc-linux-gnu",
    architecture = "x86_64",
    os_abi = "linux-glibc-2.39",
    toolchain_tag = "gcc-15.2.1-gfortran-15.2.1"
  )
  artifact <- revdeprunner:::new_artifact_identity(
    package = "compiled",
    version = "2.0.0",
    sha256 = paste(rep("b", 64L), collapse = ""),
    archive_type = "binary",
    lane = lane
  )

  expect_s3_class(lane, "revdeprunner_compatibility_lane")
  expect_identical(lane, repeated)
  expect_identical(
    names(lane),
    c(
      "schema_version",
      "lane_id",
      "r_major_minor",
      "r_platform",
      "architecture",
      "os_abi",
      "toolchain_tag"
    )
  )
  expect_identical(
    lane$schema_version,
    "revdeprunner-compatibility-lane/v1"
  )
  expect_match(lane$lane_id, "^sha256:[a-f0-9]{64}$")
  expect_invisible(revdeprunner:::validate_compatibility_lane(lane))
  expect_identical(artifact$lane_id, lane$lane_id)
  expect_invisible(revdeprunner:::validate_artifact_identity(artifact))
})

test_that("each compatibility dimension changes the lane identity", {
  baseline_fields <- list(
    r_major_minor = "4.5",
    r_platform = "x86_64-pc-linux-gnu",
    architecture = "x86_64",
    os_abi = "linux-glibc-2.39",
    toolchain_tag = "gcc-15.2.1"
  )
  alternatives <- list(
    r_major_minor = "4.4",
    r_platform = "aarch64-unknown-linux-gnu",
    architecture = "aarch64",
    os_abi = "linux-musl-1.2.5",
    toolchain_tag = "clang-21.1.0"
  )
  baseline <- do.call(
    revdeprunner:::new_compatibility_lane,
    baseline_fields
  )

  changed_ids <- vapply(
    names(alternatives),
    function(field) {
      changed <- baseline_fields
      changed[[field]] <- alternatives[[field]]
      do.call(revdeprunner:::new_compatibility_lane, changed)$lane_id
    },
    character(1L)
  )

  expect_false(any(changed_ids == baseline$lane_id))
  expect_length(unique(changed_ids), length(alternatives))
})

test_that("artifact identities separate every identity dimension", {
  hash_a <- paste(rep("c", 64L), collapse = "")
  hash_b <- paste(rep("d", 64L), collapse = "")
  lane <- revdeprunner:::new_compatibility_lane(
    "4.5",
    "x86_64-pc-linux-gnu",
    "x86_64",
    "linux-glibc-2.39",
    "gcc-15.2.1"
  )
  other_lane <- revdeprunner:::new_compatibility_lane(
    "4.5",
    "x86_64-pc-linux-gnu",
    "x86_64",
    "linux-glibc-2.39",
    "clang-21.1.0"
  )
  binary <- function(
    package = "compiled",
    version = "1.0.0",
    sha256 = hash_a,
    selected_lane = lane
  ) {
    revdeprunner:::new_artifact_identity(
      package,
      version,
      sha256,
      "binary",
      selected_lane
    )
  }

  baseline <- binary()
  alternatives <- list(
    binary(package = "compiled.other"),
    binary(version = "1.0.1"),
    binary(sha256 = hash_b),
    binary(selected_lane = other_lane)
  )

  expect_false(any(vapply(
    alternatives,
    function(artifact) identical(artifact$artifact_id, baseline$artifact_id),
    logical(1L)
  )))
  expect_false(identical(lane$lane_id, other_lane$lane_id))
})

test_that("constructors reject missing or malformed identity fields", {
  hash <- paste(rep("e", 64L), collapse = "")
  lane_fields <- list(
    r_major_minor = "4.5",
    r_platform = "x86_64-pc-linux-gnu",
    architecture = "x86_64",
    os_abi = "linux-glibc-2.39",
    toolchain_tag = "gcc-15.2.1"
  )
  lane <- revdeprunner:::new_compatibility_lane(
    lane_fields$r_major_minor,
    lane_fields$r_platform,
    lane_fields$architecture,
    lane_fields$os_abi,
    lane_fields$toolchain_tag
  )

  expect_error(
    revdeprunner:::new_compatibility_lane(
      "4.5.2",
      "x86_64-pc-linux-gnu",
      "x86_64",
      "linux-glibc-2.39",
      "gcc-15.2.1"
    ),
    "major.minor",
    fixed = TRUE
  )
  expect_error(
    revdeprunner:::new_compatibility_lane(
      "04.5",
      "x86_64-pc-linux-gnu",
      "x86_64",
      "linux-glibc-2.39",
      "gcc-15.2.1"
    ),
    "major.minor",
    fixed = TRUE
  )
  expect_error(
    revdeprunner:::new_compatibility_lane(
      "4.5",
      "x86_64 pc linux gnu",
      "x86_64",
      "linux-glibc-2.39",
      "gcc-15.2.1"
    ),
    "portable non-empty token",
    fixed = TRUE
  )
  expect_error(
    revdeprunner:::new_compatibility_lane(
      "4.5",
      "x86_64-pc-linux-gnu",
      "",
      "linux-glibc-2.39",
      "gcc-15.2.1"
    ),
    "one non-empty string",
    fixed = TRUE
  )
  for (field in c("r_platform", "architecture")) {
    for (fallback in c("unknown", "unspecified", "default")) {
      invalid <- lane_fields
      invalid[[field]] <- fallback
      expect_error(
        do.call(revdeprunner:::new_compatibility_lane, invalid),
        "specific compatibility boundary",
        fixed = TRUE,
        info = paste(field, "must reject", fallback)
      )
    }
  }
  expect_error(
    revdeprunner:::new_compatibility_lane(
      "4.5",
      "x86_64-pc-linux-gnu",
      "x86_64",
      "unknown",
      "gcc-15.2.1"
    ),
    "specific compatibility boundary",
    fixed = TRUE
  )
  expect_error(
    revdeprunner:::new_compatibility_lane(
      "4.5",
      "x86_64-pc-linux-gnu",
      "x86_64",
      "linux-glibc-2.39",
      "default"
    ),
    "specific compatibility boundary",
    fixed = TRUE
  )
  expect_error(
    revdeprunner:::new_artifact_identity(
      "invalid_name",
      "1.0.0",
      hash,
      "source"
    ),
    "valid package name",
    fixed = TRUE
  )
  expect_error(
    revdeprunner:::new_artifact_identity("valid", "bad", hash, "source"),
    "valid package version",
    fixed = TRUE
  )
  expect_error(
    revdeprunner:::new_artifact_identity(
      "valid",
      "1.0.0",
      toupper(hash),
      "source"
    ),
    "lowercase SHA-256",
    fixed = TRUE
  )
  expect_error(
    revdeprunner:::new_artifact_identity("valid", "1.0.0", hash, "unknown"),
    "must be `source` or `binary`",
    fixed = TRUE
  )
  expect_error(
    revdeprunner:::new_artifact_identity("valid", "1.0.0", hash, "binary"),
    "require a compatibility lane",
    fixed = TRUE
  )
  expect_error(
    revdeprunner:::new_artifact_identity(
      "valid",
      "1.0.0",
      hash,
      "source",
      lane
    ),
    "must not have a compatibility lane",
    fixed = TRUE
  )
})

test_that("record validation rejects structural and identity mutation", {
  lane <- revdeprunner:::new_compatibility_lane(
    "4.5",
    "x86_64-pc-linux-gnu",
    "x86_64",
    "linux-glibc-2.39",
    "gcc-15.2.1"
  )
  artifact <- revdeprunner:::new_artifact_identity(
    "compiled",
    "1.0.0",
    paste(rep("f", 64L), collapse = ""),
    "binary",
    lane
  )

  changed_lane <- lane
  changed_lane$architecture <- "aarch64"
  expect_error(
    revdeprunner:::validate_compatibility_lane(changed_lane),
    "identity does not match",
    fixed = TRUE
  )

  extra_lane <- lane
  extra_lane$unexpected <- "field"
  expect_error(
    revdeprunner:::validate_compatibility_lane(extra_lane),
    "invalid structure",
    fixed = TRUE
  )

  changed_artifact <- artifact
  changed_artifact$version <- "1.0.1"
  expect_error(
    revdeprunner:::validate_artifact_identity(changed_artifact),
    "identity does not match",
    fixed = TRUE
  )

  missing_artifact <- artifact[-length(artifact)]
  expect_error(
    revdeprunner:::validate_artifact_identity(missing_artifact),
    "invalid structure",
    fixed = TRUE
  )

  source <- revdeprunner:::new_artifact_identity(
    "sourcepkg",
    "1.0.0",
    paste(rep("1", 64L), collapse = ""),
    "source"
  )
  source$lane_id <- lane$lane_id
  expect_error(
    revdeprunner:::validate_artifact_identity(source),
    "must not contain a lane",
    fixed = TRUE
  )
})

test_that("canonical record keys are byte-stable and unambiguous", {
  fields <- c(first = "alpha:beta", second = "gamma")
  key <- revdeprunner:::canonical_record_key("fixture/v1", fields)
  repeated <- revdeprunner:::canonical_record_key("fixture/v1", fields)
  different <- revdeprunner:::canonical_record_key(
    "fixture/v1",
    c(first = "alpha", second = "beta:gamma")
  )

  expect_identical(charToRaw(key), charToRaw(repeated))
  expect_false(identical(key, different))
})
