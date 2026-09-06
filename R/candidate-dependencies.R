empty_candidate_dependencies <- function() {
  data.frame(
    dependency = character(),
    relationship = character(),
    operator = character(),
    version = character(),
    stringsAsFactors = FALSE
  )
}

read_candidate_dependencies <- function(package_root) {
  fields <- stock_runner_recursive_fields()
  description <- read.dcf(
    file.path(package_root, "DESCRIPTION"),
    fields = fields
  )
  rows <- list()
  for (field in fields) {
    value <- description[[1L, field]]
    dependencies <- parse_stock_dependency_field(value, field)
    if (!length(dependencies)) next
    entries <- trimws(strsplit(
      sub(",[[:space:]]*$", "", value),
      ",",
      fixed = TRUE
    )[[1L]])
    for (entry in entries) {
      package <- parse_stock_dependency_entry(entry, field)
      constraint <- regmatches(
        entry,
        regexec("\\((>=|<=|==|!=|>|<)[[:space:]]+([^() ]+)\\)$", entry)
      )[[1L]]
      rows[[length(rows) + 1L]] <- data.frame(
        dependency = package,
        relationship = field,
        operator = if (length(constraint)) constraint[[2L]] else "",
        version = if (length(constraint)) constraint[[3L]] else "",
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(rows)) return(empty_candidate_dependencies())
  result <- unique(do.call(rbind, rows))
  result <- result[
    do.call(order, c(result, list(method = "radix"))),
    ,
    drop = FALSE
  ]
  rownames(result) <- NULL
  result
}

validate_candidate_dependencies <- function(dependencies) {
  if (
    !is.data.frame(dependencies) ||
      !identical(names(dependencies), names(empty_candidate_dependencies())) ||
      !all(vapply(dependencies, is.character, logical(1L))) ||
      anyNA(dependencies) ||
      anyDuplicated(dependencies) ||
      any(!dependencies$relationship %in% stock_runner_recursive_fields())
  ) {
    stop("Candidate dependency requirements are invalid.", call. = FALSE)
  }
  for (row in seq_len(nrow(dependencies))) {
    entry <- dependencies$dependency[[row]]
    if (nzchar(dependencies$operator[[row]])) {
      entry <- paste0(
        entry,
        " (",
        dependencies$operator[[row]],
        " ",
        dependencies$version[[row]],
        ")"
      )
    } else if (nzchar(dependencies$version[[row]])) {
      stop("Candidate dependency constraint is invalid.", call. = FALSE)
    }
    parse_stock_dependency_entry(entry, dependencies$relationship[[row]])
  }
  invisible(dependencies)
}

validate_candidate_dependency_versions <- function(dependencies, snapshot) {
  validate_candidate_dependencies(dependencies)
  selected <- snapshot$packages[
    !duplicated(snapshot$packages$Package),
    ,
    drop = FALSE
  ]
  for (row in which(nzchar(dependencies$operator))) {
    package <- dependencies$dependency[[row]]
    required <- dependencies$version[[row]]
    version <- if (package == "R") {
      if (startsWith(required, "r")) paste0("r", R.version[["svn rev"]]) else
        as.character(getRversion())
    } else if (package %in% revdep_base_packages()) {
      as.character(utils::packageVersion(package, lib.loc = .Library))
    } else {
      selected$Version[match(package, selected$Package)]
    }
    # Unavailable packages remain visible as unmet preparation requirements.
    if (is.na(version)) next
    comparison <- if (package == "R" && startsWith(required, "r")) {
      sign(
        as.numeric(sub("^r", "", version)) - as.numeric(sub("^r", "", required))
      )
    } else {
      utils::compareVersion(version, required)
    }
    satisfied <- switch(
      dependencies$operator[[row]],
      ">=" = comparison >= 0,
      "<=" = comparison <= 0,
      "==" = comparison == 0,
      "!=" = comparison != 0,
      ">" = comparison > 0,
      "<" = comparison < 0
    )
    if (!isTRUE(satisfied)) {
      stop(
        sprintf(
          "Candidate dependency %s requires %s %s; the frozen selected version is %s.",
          package,
          dependencies$operator[[row]],
          required,
          version
        ),
        call. = FALSE
      )
    }
  }
  invisible(NULL)
}

validate_revdep_candidate_requirements <- function(state) {
  expected <- attr(state$plan, "candidate_dependencies", exact = TRUE)
  current <- read_candidate_dependencies(state$context$path_plan$package_root)
  if (!identical(expected, current)) {
    stop(
      "The candidate's dependency requirements have changed since preparation. Prepare again before checking.",
      call. = FALSE
    )
  }
  validate_candidate_dependency_versions(current, state$context$snapshot)
  invisible(NULL)
}
