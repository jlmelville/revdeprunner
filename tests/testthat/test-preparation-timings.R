# Synthetic persisted evidence distinguishes historical totals from current-call time.
test_that("preparation timing summaries retain failures and select successful references by time", {
  report <- list(
    results = data.frame(
      package = c("TargetPkg", "CachedPkg", "DependencyPkg")
    ),
    attempts = data.frame(
      attempt_id = c(
        "z-old",
        "a-new",
        "failed",
        "install",
        "load",
        "dependency",
        "z-old"
      ),
      package = c(rep("TargetPkg", 5L), "DependencyPkg", "TargetPkg"),
      stage = c("build", "build", "build", "install", "load", "build", "build"),
      outcome = c(
        "success",
        "success",
        "failure",
        "success",
        "success",
        "success",
        "success"
      ),
      started_at = c(
        "2026-01-01",
        "2026-01-03",
        "2026-01-04",
        "2026-01-05",
        "2026-01-06",
        "2026-01-02",
        "2026-01-01"
      ),
      duration_ms = c("1000", "2500", "500", "300", "200", "8000", "1000")
    )
  )
  plan <- list(
    targets = data.frame(
      package = c("TargetPkg", "CachedPkg", "DependencyPkg"),
      selected = c(TRUE, TRUE, FALSE)
    )
  )
  # Use the public print method on a persisted-object shape; no process is replayed.
  prepared <- structure(
    list(
      summary = data.frame(
        package = "SubjectPkg",
        baseline_version = "1.0",
        state = "ready",
        selected_targets = 2L,
        requirements = 3L,
        problems = 0L
      ),
      plan = plan,
      evidence = list(report = report)
    ),
    class = "revdep_prepared"
  )
  output <- capture.output(print(prepared))
  expect_true(any(grepl(
    "build 12.0 s; install 0.3 s; load 0.2 s",
    output,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "TargetPkg (target): 4.0 s; build attempts: 3",
    output,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "DependencyPkg (dependency): 8.0 s",
    output,
    fixed = TRUE
  )))
  expect_true(any(grepl("TargetPkg: 2.5 s.", output, fixed = TRUE)))
  expect_true(any(grepl(
    "CachedPkg: no successful build timing recorded",
    output,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "not this call's wall time or a check ETA",
    output,
    fixed = TRUE
  )))
  expect_true(any(grepl("both old/new sides", output, fixed = TRUE)))
  prepared$evidence$report <- NULL
  expect_true(
    "No preparation subprocess timings recorded." %in%
      capture.output(print(prepared))
  )
})
