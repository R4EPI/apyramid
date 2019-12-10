#' Aggregate linelist data into counts by age group
#'
#' @param data a data frame
#' @param age_group the name of a column in the data frame that defines the age
#'   group categories. Defaults to "age_group"
#' @param split_by the name of a column in the data frame that defines the
#'   the bivariate column. Defaults to "sex". See NOTE
#' @param stack_by the name of the column in the data frame to use for shading
#'   the bars
#' @param proportional If `TRUE`, bars will represent proportions of cases out
#'   of the entire population. Otherwise (`FALSE`, default), bars represent
#'   case counts
#' @param na.rm  If `TRUE`, this removes NA counts from the age groups. Defaults
#'   to `TRUE`.
#'
#' @return a data frame with counts aggregated by age
#' @export
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
#'                  age_group = "AGE",
#'                  split_by = "gender", 
#'                  stack_by = "ill",
#'                  proportional = TRUE,
#'                  na.rm = FALSE)
aggregate_by_age <- function(data, age_group = "age_group", split_by = "sex",
                             stack_by = split_by, proportional = FALSE, 
                             na.rm = TRUE) {
  ag <- rlang::sym(age_group)
  sb <- rlang::sym(split_by)
  st <- rlang::sym(stack_by)

  if (is.data.frame(data)) {
    sbv <- data[[split_by]]
    stv <- data[[stack_by]]
    if (!is.character(sbv) || !is.factor(sbv)) {
      data[[split_by]] <- as.character(sbv)
    }
    if (!is.character(stv) || !is.factor(stv)) {
      data[[stack_by]] <- as.character(stv)
    }
    if (anyNA(sbv) || anyNA(stv)) {
      nas <- is.na(sbv) | is.na(stv)
      warning(sprintf(
        "removing %d observations with missing values between the %s and %s columns.",
        sum(nas), split_by, stack_by
      ))
      data <- data[!nas, , drop = FALSE]
    }
    if (na.rm) {
      nas <- is.na(data[[age_group]])
      warning(sprintf(
        "removing %d observations with missing values from the %s column.",
        sum(nas), age_group
      ))
      data <- data[!nas, , drop = FALSE]
    } else {
      data[[age_group]] <- forcats::fct_explicit_na(data[[age_group]])
    }
    plot_data <- dplyr::count(data, !!ag, !!sb, !!st, .drop = FALSE)
    plot_data <- dplyr::ungroup(plot_data)
    if (is.factor(sbv)) {
      plot_data[[split_by]] <- factor(plot_data[[split_by]], levels(sbv))
    }
    if (is.factor(stv)) {
      plot_data[[stack_by]] <- factor(plot_data[[stack_by]], levels(stv))
    }
  } else {
    plot_data <- srvyr::group_by(data, !!ag, !!sb, !!st, .drop = FALSE)
    plot_data <- srvyr::summarise(plot_data,
      n = srvyr::survey_total(vartype = "ci", level = 0.95)
    )
  }
  # Remove any missing values
  to_delete <- is.na(plot_data[[split_by]]) & is.na(plot_data[[stack_by]]) & plot_data[["n"]] == 0
  plot_data <- plot_data[!to_delete, , drop = FALSE]

  if (proportional) {
    plot_data$n <- plot_data$n / sum(plot_data$n, na.rm = TRUE)
  }

  plot_data
}
