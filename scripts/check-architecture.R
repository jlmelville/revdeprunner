# Run from the repository root, either directly or through scripts/lint.R.
architecture <- readLines("ARCHITECTURE.md", warn = FALSE)
matches <- gregexpr("\\]\\(([^)]+)\\)", architecture, perl = TRUE)
links <- unlist(regmatches(architecture, matches), use.names = FALSE)
targets <- unique(sub("^\\]\\(([^)]+)\\)$", "\\1", links))
# Source links also work on pkgdown, which renders root Markdown without R/ or tests/.
repository_prefix <- "https://github.com/jlmelville/revdeprunner/blob/main/"
repository_links <- startsWith(targets, repository_prefix)
targets[repository_links] <- substring(
  targets[repository_links],
  nchar(repository_prefix) + 1L
)
local_targets <- targets[!grepl("^[A-Za-z][A-Za-z0-9+.-]*:|^#", targets)]
local_paths <- sub("#.*$", "", local_targets)
missing_paths <- local_paths[!file.exists(local_paths)]
if (length(missing_paths)) {
  stop(
    "Architecture links do not exist: ",
    paste(missing_paths, collapse = ", ")
  )
}

source_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
documented <- local_paths[grepl("^R/.*\\.R$", local_paths)]
unmapped <- setdiff(source_files, documented)
if (length(unmapped)) {
  stop("Architecture map omits R files: ", paste(unmapped, collapse = ", "))
}
message("Architecture file map and local links are current.")
