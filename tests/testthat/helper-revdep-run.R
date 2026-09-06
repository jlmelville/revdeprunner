# These exported-API fixtures compose the accepted private preparation helpers.

revdep_run_fixture_database <- function() {
  database <- source_acquisition_fixture_database()
  primary <- source_acquisition_fixture_repositories()[["CRAN"]]
  database$Imports[database$Package == "HitPkg"] <- "SubjectPkg"
  database$Suggests[
    database$Package == "BuildPkg" & database$Repository == primary
  ] <- "HitPkg, OptionalPkg"
  database$NeedsCompilation[
    database$Package == "BuildPkg" & database$Repository == primary
  ] <- "no"
  database
}

write_revdep_run_candidate <- function(path) {
  dir.create(file.path(path, "R"), showWarnings = FALSE)
  writeLines(
    c(
      "Package: SubjectPkg",
      "Type: Package",
      "Title: Public Runner Subject Fixture",
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

make_revdep_run_fixture <- function() {
  fixture <- make_source_preparation_fixture(
    missing_binary_packages = c("BuildPkg", "FilePkg", "HitPkg"),
    database = revdep_run_fixture_database(),
    build_imports = "SubjectPkg"
  )
  write_revdep_run_candidate(fixture$paths[[1L]])
  repositories <- fixture$download_contracts$snapshot$repositories
  database <- fixture$download_contracts$snapshot$packages
  database <- database[
    database$Repository == repositories[["CRAN"]],
    ,
    drop = FALSE
  ]
  bases <- c(CRAN = sub("/src/contrib$", "", repositories[["CRAN"]]))
  list(
    fixture = fixture,
    database = database,
    bases = bases
  )
}

revdep_run_stock_tools_supported <- function() {
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

revdep_run_distinct_snapshot_database <- function(database) {
  unrelated <- database[database$Package == "SubjectPkg", , drop = FALSE]
  unrelated$Package <- "SnapshotOnlyPkg"
  unrelated$Version <- "1.0"
  unrelated$Depends <- NA_character_
  unrelated$Imports <- NA_character_
  unrelated$LinkingTo <- NA_character_
  unrelated$Suggests <- NA_character_
  unrelated$File <- NA_character_
  unrelated$MD5sum <- "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"
  rbind(database, unrelated)
}
