
#' Performs a list of checks according to specified rules
#'
#' @description
#' Takes a named list of function (the rules). Each function should take
#' the arguments it requires plus ... Each function should return a list
#' with the following contents:
#'  - msg (character): a message indicating the check failure, or "" for a pass
#'  - check (any object): an object representing the check performed.
#'
#' This function will produce warning(s) for each of the rules that fail.
#'
#' @param rules a named list of rules
#' @param override a vector of names of rules to ignore
#' @param ... arguments to pass to the rules
#'
#' @return
#' a named vector of lists. Each list has the following contents:
#'   - msg: The message generated by the check. "" if it passed.
#'   - obj: The object representing the check
#'   - pfo: "P" if the test passed, "F" if the test failed,
#'          "O" if the test was overridden, and NA if the test was not run
#'          due to a missing argument.
#'
#' @noRd
#'
#' @importFrom rlang list2
#' @importFrom rlang fn_fmls_names
#' @importFrom rlang warn
#' @importFrom rlang inform
perform_checks <- function(rules, override = c(), ...) {
  args <- list2(...)

  lapply(names(rules), function(cur_rule_name) {
    if (!(cur_rule_name %in% override)) {
      cur_rule <- rules[[cur_rule_name]]
      all_formal_names <- fn_fmls_names(cur_rule)
      missing_formals <- vapply(all_formal_names, function(cur_formal_name) {
        is.null(args[[cur_formal_name]]) & cur_formal_name != "..."
      },
      FUN.VALUE = logical(1L)
      )
      if (!any(missing_formals)) {
        check_res <- tryCatch(
          do.call(cur_rule, args),
          error = function(e) {
            stop(paste0("During evaluation of `", cur_rule_name, "`: ", e))
          })
        if (check_res$msg != "") {
          warn(paste0("`", cur_rule_name, "` failed: ", check_res$msg))
          check_res[["pfo"]] <- "F"
          check_res[["rule_name"]] <- cur_rule_name
          return(check_res)
        }
        check_res[["pfo"]] <- "P"
        check_res[["rule_name"]] <- cur_rule_name
        check_res
      } else {
        inform(
          paste0("`", cur_rule_name, "` not run because parameter",
                 ifelse(sum(missing_formals) > 1, "s", ""),
                 " `",
                 paste(all_formal_names[missing_formals], collapse = "`, `"),
                 "` not specified")
        )
        return(list(
          msg = "Not run",
          obj = NULL,
          rule_name = cur_rule_name,
          pfo = NA_character_))
      }
    } else {
      # in override list
      return(list(
        msg = "Overridden",
        obj = NULL,
        rule_name = cur_rule_name,
        pfo = "O"))
    }
  })
}

#' Gets the pass/fail/overridden results for a set of checks
#'
#' @return
#' a named vector of characters. "P" if the test passed,
#' "F" if the test failed, "O" if the test was overridden,
#' and NA if the test was not run
#' due to a missing argument.
#'
#' @noRd
get_check_pfo <- function(check_results) {
  res <- vapply(
    check_results,
    function(cr) {
      cr[["pfo"]]
    },
    FUN.VALUE = character(1L)
  )
  names(res) <- sapply(
    check_results,
    function(cr) {
      cr[["rule_name"]]
    }
  )
  res
}

#' Gets the object representing results for a set of checks
#'
#' @return
#' a named vector of objects
#'
#' @noRd
get_check_obj <- function(check_results) {
  res <- sapply(
    check_results,
    function(cr) {
      cr[["obj"]]
    }
  )
  names(res) <- sapply(
    check_results,
    function(cr) {
      cr[["rule_name"]]
    }
  )
  res
}

#' Gets the names of the diagnostic tests that failed
#'
#' @param x a named character vector created by perform_checks
#'
#' @return
#' A character vector of the tests that failed (if any)
#'
#' @noRd
get_check_failure_names <- function(x) {
  names(x[x == "F" & !is.na(x)])
}

process_overrides <- function(override, rules) {
  if ("all" %in% override) {
    # Remove the "all" value
    override <- override[!override %in% "all"]
    # Add all of the rules to the override vector
    override <- c(override, names(rules))
  }
  # Keep only the unique values of override
  override <- unique(override)
  # Warn if there are invalid overrides
  for (ov in override) {
    if (!ov %in% names(rules)) {
      warn(paste0("`", ov, "` is not a valid diagnostic test to override"))
    }
  }

  override
}
