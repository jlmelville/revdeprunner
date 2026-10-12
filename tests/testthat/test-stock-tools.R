# Test the toolchain boundary with controlled observations; integration tests
# independently require installed dependencies and exercise the real guard.
test_that("the stock adapter rejects incompatible installed tool provenance", {
  skip_if_stock_tools_unavailable()
  observed <- revdeprunner:::stock_adapter_provenance()
  for (field in c("version", "remote_sha")) {
    changed <- observed
    changed[[field]][[1L]] <- if (field == "version") {
      "0.0.0"
    } else {
      strrep("0", 40L)
    }
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

test_that("the stock adapter supports Linux and macOS only", {
  for (platform in c("Linux", "Darwin", "Windows")) {
    if (platform == "Windows") {
      expect_error(
        revdeprunner:::require_supported_revdep_runner(platform),
        "supported only on Linux and macOS",
        fixed = TRUE
      )
    } else {
      expect_invisible(revdeprunner:::require_supported_revdep_runner(platform))
    }
  }
})
