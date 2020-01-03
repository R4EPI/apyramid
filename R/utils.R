stop_if_not_df_or_svy <- function(data, name = deparse(substitute(data))) {
  is_df <- is.data.frame(data)
  is_svy <- inherits(data, "tbl_svy")
  if (!is_df && !is_svy) {
    msg <- sprintf("%s must be a data frame or tbl_svy object", name)
    stop(msg, call. = FALSE)
  }
  invisible(NULL)
}

get_var <- function(dat, var) {
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
treat_nas <- function(data, age_group, split_by, stack_by, na.rm) {

  da_vars <- c(age_group, split_by, stack_by)
  data    <- dplyr::select(data, da_vars)

  if (na.rm) {
    missing   <- dplyr::mutate_at(data, .vars = da_vars, .funs = is.na)
    sumissing <- colSums(missing)
    if (any(sumissing > 0)) {
      last  <- if (length(sumissing) > 2) ", and " else " and "
      nmiss <- glue::glue("{sumissing} values from `{names(sumissing)}`")
      nmiss <- glue::glue_collapse(nmiss, sep = ", ", last = last)
      missing <- missing[[age_group]] | missing[[split_by]] | missing[[stack_by]]
      msg     <- glue::glue("{sum(missing)} missing rows were removed ({nmiss}).")
      warning(msg, call. = FALSE)
      data    <- data[!missing, , drop = FALSE]
    } else {
      # :shrug:
    }
    # Force any data that's not a factor to be a factor
    data <- dplyr::mutate_if(data,
                             .pred = Negate(is.factor),
                             .funs = forcats::fct_inorder)
  } else {
    # Force missing values to be "Missing"
    data <- dplyr::mutate_at(data,
                             .vars = da_vars,
                             .funs = forcats::fct_explicit_na, "Missing")
  }

  return(data)
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
