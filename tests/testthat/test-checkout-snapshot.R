# Directly test the identity/copy boundary without running package installation.
test_that("checkout snapshots prune excluded roots before inspecting their contents", {
  root <- tempfile("checkout-snapshot-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  source <- file.path(root, "source")
  copied <- file.path(root, "copied")
  dir.create(source)
  dir.create(copied)
  dir.create(file.path(source, "R"))
  writeLines(
    c("Package: SubjectPkg", "Version: 0.2"),
    file.path(source, "DESCRIPTION")
  )
  writeLines(
    "subject_value <- function() 42L",
    file.path(source, "R", "subject.R")
  )
  before <- revdeprunner:::checkout_identity(source, "SubjectPkg")
  for (excluded in c(".git", ".Rproj.user", "revdep")) {
    dir.create(file.path(source, excluded))
    expect_true(file.symlink(
      file.path(source, "DESCRIPTION"),
      file.path(source, excluded, "linked-file")
    ))
  }
  expect_identical(
    revdeprunner:::checkout_identity(source, "SubjectPkg"),
    before
  )
  revdeprunner:::stock_adapter_copy_checkout(source, copied)
  expect_identical(
    revdeprunner:::checkout_identity(copied, "SubjectPkg"),
    before
  )
  writeLines(
    "subject_value <- function() 43L",
    file.path(source, "R", "subject.R")
  )
  expect_false(identical(
    revdeprunner:::checkout_identity(source, "SubjectPkg"),
    before
  ))
})
