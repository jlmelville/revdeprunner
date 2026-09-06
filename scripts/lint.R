required_version <- "3.4.0"
if (
  !requireNamespace("lintr", quietly = TRUE) ||
    as.character(utils::packageVersion("lintr")) != required_version
) {
  stop(
    "Lint requires lintr ",
    required_version,
    ". Install with pak::pak('lintr@",
    required_version,
    "')."
  )
}
message("Linting with lintr ", required_version)
source("scripts/check-architecture.R")
pkgload::load_all(".", attach = TRUE, helpers = TRUE, quiet = TRUE)
lints <- lintr::lint_package()
print(lints)
quit(status = if (length(lints) > 0L) 1L else 0L)
