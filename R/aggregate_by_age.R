#' Aggregate linelist data into counts by age group
#'
#'
#' @param data a data frame
#'
#' @param age_group the name of a column in the data frame that defines the age
#'   group categories. Defaults to "age_group"
#'
#' @param split_by the name of a column in the data frame that defines the
#'   the bivariate column. Defaults to "sex". See NOTE
#'
#' @param stack_by the name of the column in the data frame to use for shading
#'   the bars
#'
#' @param proportional If `TRUE`, bars will represent proportions of cases out
#'   of the entire population. Otherwise (`FALSE`, default), bars represent
#'   case counts
#'
#' @param na.rm  If `TRUE`, this removes NA counts from the age groups. Defaults
#'   to `TRUE`.
#'
#' @return a data frame with counts aggregated by age
#' @keywords internal
#' @noRd
#'
#' @examples
#' set.seed(2018 - 01 - 15)
#' ages <- cut(sample(80, 150, replace = TRUE),
#'   breaks = c(0, 5, 10, 30, 90), right = FALSE
#' )
#' sex <- sample(c("Female", "Male"), 150, replace = TRUE)
#' gender <- sex
#' gender[sample(5)] <- "NB"
#' ill <- sample(c("case", "non-case"), 150, replace = TRUE)
#' dat <- data.frame(
#'   AGE = ages,
#'   sex = factor(sex, c("Male", "Female")),
#'   gender = factor(gender, c("Male", "NB", "Female")),
#'   ill = ill,
#'   stringsAsFactors = FALSE
#' )
#' aggregate_by_age(dat, 
#'                  age_group = AGE,
#'                  split_by = gender, 
#'                  stack_by = ill,
#'                  proportional = TRUE,
#'                  na.rm = FALSE)
aggregate_by_age <- function(data, age_group = "age_group", split_by = "sex",
                             stack_by = NULL, proportional = FALSE, 
                             na.rm = TRUE) {

  stop_if_not_df_or_svy(data, deparse(substitute(data)))

  # age_group <- get_var(data, !!rlang::enquo(age_group))
  # split_by  <- get_var(data, !!rlang::enquo(split_by))
  # stack_by  <- get_var(data, !!rlang::enquo(stack_by))

  if (length(stack_by) == 0) {
    stack_by <- split_by
  }

  ag        <- rlang::sym(age_group)
  sb        <- rlang::sym(split_by)
  st        <- rlang::sym(stack_by)

  if (is.data.frame(data)) {
    data[[split_by]] <- to_character(data[[split_by]])
    data[[stack_by]] <- to_character(data[[stack_by]])
    data             <- treat_nas(data, age_group, split_by, stack_by, na.rm)

    plot_data <- dplyr::count(data, !!ag, !!sb, !!st, .drop = FALSE)
    plot_data <- force_factors(plot_data, data, split_by, stack_by)
  } else if (inherits(data, "tbl_svy")) {
    if (!requireNamespace("srvyr")) {
      stop("Please install the srvyr package to proceed\n\ninstall.packages('srvyr')")
    } else {
      plot_data <- srvyr::group_by(data, !!ag, !!sb, !!st, .drop = FALSE)
      plot_data <- srvyr::summarise(plot_data,
        n = srvyr::survey_total(vartype = "ci", level = 0.95)
      )
    }
  } else {
    stop("Input must be a data frame")
  }

  # Remove any missing values
  to_delete <- is.na(plot_data[[split_by]]) & 
               is.na(plot_data[[stack_by]]) & 
               plot_data[["n"]] == 0

  plot_data <- plot_data[!to_delete, , drop = FALSE]

  if (proportional) {
    plot_data[["n"]] <- plot_data[["n"]] / sum(plot_data[["n"]], na.rm = TRUE)
  }

  plot_data
}
