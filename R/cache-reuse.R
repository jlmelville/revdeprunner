reuse_cached_binaries <- function(requests, observations, lane, path_plan) {
  requests <- normalize_binary_reuse_requests(requests)
  observations <- normalize_cache_observations(observations, path_plan)
  selections <- select_cached_binaries(
    requests,
    observations,
    lane,
    path_plan
  )

  cache_paths <- vapply(
    seq_along(selections),
    function(index) {
      selection <- selections[[index]]
      if (identical(selection$status, "missing")) {
        return(NA_character_)
      }
      publish_binary_cache_artifact(
        selection$source_path,
        selection$artifact,
        path_plan
      )
    },
    character(1L)
  )
  names(cache_paths) <- requests$package

  new_binary_reuse(
    requests,
    observations,
    selections,
    cache_paths,
    lane,
    path_plan
  )
}

new_binary_reuse <- function(
  requests,
  observations,
  selections,
  cache_paths,
  lane,
  path_plan
) {
  reuse <- structure(
    list(
      lane_id = lane$lane_id,
      path_plan_id = path_plan$path_plan_id,
      requests = requests,
      observations = observations,
      selections = selections,
      cache_paths = cache_paths
    ),
    class = "revdeprunner_binary_reuse"
  )
  validate_binary_reuse(reuse, lane, path_plan)
  reuse
}

validate_binary_reuse <- function(reuse, lane, path_plan) {
  fields <- c(
    "lane_id",
    "path_plan_id",
    "requests",
    "observations",
    "selections",
    "cache_paths"
  )
  if (
    !inherits(reuse, "revdeprunner_binary_reuse") ||
      !is.list(reuse) ||
      !identical(names(reuse), fields)
  ) {
    stop("Binary reuse has an invalid structure.", call. = FALSE)
  }
  validate_sha256_identity(reuse$lane_id, "lane_id")
  validate_sha256_identity(reuse$path_plan_id, "path_plan_id")
  validate_compatibility_lane(lane)
  validate_runtime_root_plan(path_plan)
  if (
    !identical(reuse$lane_id, lane$lane_id) ||
      !identical(reuse$path_plan_id, path_plan$path_plan_id)
  ) {
    stop("Binary reuse does not match its runtime context.", call. = FALSE)
  }

  requests <- normalize_binary_reuse_requests(reuse$requests)
  observations <- normalize_cache_observations(reuse$observations, path_plan)
  if (
    !identical(reuse$requests, requests) ||
      !identical(reuse$observations, observations)
  ) {
    stop("Binary reuse inputs are not normalized.", call. = FALSE)
  }
  expected_names <- requests$package
  if (
    !is.list(reuse$selections) ||
      !is.character(reuse$cache_paths) ||
      !identical(names(reuse$selections), expected_names) ||
      !identical(names(reuse$cache_paths), expected_names)
  ) {
    stop("Binary reuse entries do not match their requests.", call. = FALSE)
  }

  for (row in seq_len(nrow(requests))) {
    selection <- reuse$selections[[row]]
    cache_path <- reuse$cache_paths[[row]]
    validate_cached_binary_selection(selection)
    if (
      !identical(selection$package, requests$package[[row]]) ||
        !identical(selection$version, requests$version[[row]]) ||
        !identical(selection$lane_id, reuse$lane_id)
    ) {
      stop("Binary reuse selection does not match its request.", call. = FALSE)
    }

    if (identical(selection$status, "missing")) {
      if (!is.na(cache_path)) {
        stop(
          "Missing binary reuse selection must not have a cache path.",
          call. = FALSE
        )
      }
      next
    }
    matches <- !is.na(observations$sha256) &
      observations$cache_root == selection$cache_root &
      observations$relative_path == selection$relative_path &
      observations$sha256 == selection$artifact$sha256 &
      observations$priority == selection$priority
    if (sum(matches) != 1L) {
      stop(
        "Binary reuse selection lacks matching cache evidence.",
        call. = FALSE
      )
    }
    validate_binary_cache_artifact(
      cache_path,
      selection$artifact,
      path_plan
    )
  }

  invisible(reuse)
}
