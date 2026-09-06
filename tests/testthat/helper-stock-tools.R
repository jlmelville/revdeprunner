# Only absent optional packages justify skipping stock integration tests.
# With dependencies installed, an incompatible toolchain must fail visibly.
skip_if_stock_tools_unavailable <- function() {
  for (package in c("revdepcheck", "crancache", "cranlike")) {
    testthat::skip_if_not_installed(package)
  }
  revdeprunner:::require_stock_adapter_tools()
  invisible(NULL)
}
