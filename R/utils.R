get_var <- function(dat, var) {
  if (!inherits(dat, c("data.frame", "tbl_srvy"))) {
    stop("Input must be a data frame", call. = FALSE)
  }
  tidyselect::vars_select(colnames(dat), !!rlang::enquo(var))
}

to_character <- function(x) {
  if (!is.character(x) && !is.factor(x)) as.character(x) else x
}



#' Remove or replace missing data
#'
#' @param dat a data frame
#' @param split_by column to split by
#' @param stack_by column to stack by
#'
#' @return the data frame, but maybe shorter
#' @noRd
#' @keywords internal
treat_nas <- function(dat, age_group, split_by, stack_by, na.rm) {

  if (anyNA(dat[[split_by]]) || anyNA(dat[[stack_by]])) {
    nas <- is.na(dat[[split_by]]) | is.na(dat[[stack_by]])
    warning(sprintf(
      "removing %d observations with missing values between the %s and %s columns.",
      sum(nas), split_by, stack_by
    ))
    dat <- dat[!nas, , drop = FALSE]
  }
  if (na.rm) {
    nas <- is.na(dat[[age_group]])
    warning(sprintf(
      "removing %d observations with missing values from the %s column.",
      sum(nas), age_group
    ))
    dat <- dat[!nas, , drop = FALSE]
  } else {
    dat[[age_group]] <- forcats::fct_explicit_na(dat[[age_group]])
  }

  return(dat)

}

force_factors <- function(plt, dat, split_by, stack_by) {

  if (is.factor(dat[[split_by]])) {
    plt[[split_by]] <- factor(plt[[split_by]], levels(dat[[split_by]]))
  }
  if (is.factor(dat[[stack_by]])) {
    plt[[stack_by]] <- factor(plt[[stack_by]], levels(dat[[stack_by]]))
  }
  return(plt)

}
