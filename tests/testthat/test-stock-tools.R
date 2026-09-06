# Test the toolchain boundary with controlled observations; integration tests
# independently require installed dependencies and exercise the real guard.
test_that("the stock adapter rejects incompatible installed tool provenance", {
  skip_if_stock_tools_unavailable()
  observed <- revdeprunner:::stock_adapter_provenance()
  for (field in c("version", "remote_sha")) {
    changed <- observed
    changed[[field]][[1L]] <- if (field == "version") "0.0.0" else
      strrep("0", 40L)
    expect_error(
      with_mocked_bindings(
        revdeprunner:::require_stock_adapter_tools(),
        stock_adapter_provenance = function() changed,
        .package = "revdeprunner"
      ),
      "requires the pinned revdepcheck",
      fixed = TRUE
    )
  }
})

if (!identical(unname(Sys.info()[["sysname"]]), "Linux")) {
  test_that("the stock adapter reports its Linux boundary", {
    expect_error(
      revdeprunner:::require_linux_revdep_runner(),
      "supported only on Linux",
      fixed = TRUE
    )
  })
}
