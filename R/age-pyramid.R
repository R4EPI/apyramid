#' Plot a population pyramid (age-sex) from a dataframe.
#'
#' @param data Your dataframe (e.g. linelist)
#'
#' @param age_group the name of a column in the data frame that defines the age
#'   group categories. Defaults to "age_group"
#'
#' @param split_by the name of a column in the data frame that defines the
#'   the bivariate column. Defaults to "sex". See NOTE
#'
#' @param stack_by the name of the column in the data frame to use for shading
#'   the bars. Defaults to `NULL` which will shade the bars by the `split_by`
#'   variable.
#' 
#' @param count **for pre-computed data** the name of the column in the data
#'   frame for the values of the bars. If this represents proportions, the
#'   values should be within \[0, 1\].
#'
#' @param proportional If `TRUE`, bars will represent proportions of cases out
#'   of the entire population. Otherwise (`FALSE`, default), bars represent
#'   case counts
#'
#' @param na.rm  If `TRUE`, this removes NA counts from the age groups. Defaults
#'   to `TRUE`.
#'
#' @param show_midpoint When `TRUE` (default), a dashed vertical line will be
#'   added to each of the age bars showing the halfway point for the
#'   un-stratified age group. When `FALSE`, no halfway point is marked.
#'
#' @param vertical_lines If you would like to add dashed vertical lines to help
#' visual interpretation of numbers. Default is to not show (`FALSE`),
#' to turn on write `TRUE`.
#'
#' @param horizontal_lines If `TRUE` (default), horizontal dashed lines will
#'   appear behind the bars of the pyramid
#'
#' @param pyramid if `TRUE`, then binary `split_by` variables will result in
#'   a population pyramid (non-binary variables cannot form a pyramid). If
#'   `FALSE`, a pyramid will not form.
#'
#' @param pal a color palette function or vector of colors to be passed to
#'   [ggplot2::scale_fill_manual()] defaults to the first "qual" palette from
#'   [ggplot2::scale_fill_brewer()].
#'
#' @note If the `split_by` variable is bivariate (e.g. an indicator for a
#' specific symptom), then the result will show up as a pyramid, otherwise, it
#' will be presented as a facetted barplot with with empty bars in the
#' background indicating the range of the un-facetted data set. Values of
#' `split_by` will show up as labels at top of each facet.
#'
#' @import ggplot2
#' @importFrom scales percent
#' @importFrom rlang !!
#' @export
#' @examples
#' 
#' library(ggplot2)
#' old <- theme_set(theme_classic(base_size = 18))
#' 
#' # with pre-computed data ----------------------------------------------------
#' # 2018/2008 US census data by age and gender
#' data(us_2018)
#' data(us_2008)
#' age_pyramid(us_2018, age_group = age, split_by = gender, count = count)
#' age_pyramid(us_2008, age_group = age, split_by = gender, count = count)
#'
#' # 2018 US census data by age, gender, and insurance status
#' data(us_ins_2018)
#' age_pyramid(us_ins_2018, 
#'   age_group = age,
#'   split_by = gender,
#'   stack_by = insured,
#'   count = count
#' )
#' us_ins_2018$prop <- us_ins_2018$percent/100
#' age_pyramid(us_ins_2018,
#'   age_group = age,
#'   split_by = gender,
#'   stack_by = insured,
#'   count = prop,
#'   proportion = TRUE
#' )
#'
#' # from linelist data --------------------------------------------------------
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
#'
#' # Create the age pyramid, stratifying by sex
#' print(ap <- age_pyramid(dat, age_group = AGE))
#'
#' # Create the age pyramid, stratifying by gender, which can include non-binary
#' print(apg <- age_pyramid(dat, age_group = AGE, split_by = gender))
#'
#' # Remove NA categories with na.rm = TRUE
#' dat2 <- dat
#' dat2[1, 1] <- NA
#' dat2[2, 2] <- NA
#' dat2[3, 3] <- NA
#' print(ap <- age_pyramid(dat2, age_group = AGE))
#' print(ap <- age_pyramid(dat2, age_group = AGE, na.rm = TRUE))
#'
#' # Stratify by case definition and customize with ggplot2
#' ap <- age_pyramid(dat, age_group = AGE, split_by = ill) +
#'   theme_bw(base_size = 16) +
#'   labs(title = "Age groups by case definition")
#' print(ap)
#'
#' # Stratify by multiple factors
#' ap <- age_pyramid(dat,
#'   age_group = AGE,
#'   split_by = sex,
#'   stack_by = ill,
#'   vertical_lines = TRUE
#' ) +
#'   labs(title = "Age groups by case definition and sex")
#' print(ap)
#'
#' # Display proportions
#' ap <- age_pyramid(dat,
#'   age_group = AGE,
#'   split_by = sex,
#'   stack_by = ill,
#'   proportional = TRUE,
#'   vertical_lines = TRUE
#' ) +
#'   labs(title = "Age groups by case definition and sex")
#' print(ap)
#'
#' # empty group levels will still be displayed
#' dat3 <- dat2
#' dat3[dat$AGE == "[0,5)", "sex"] <- NA
#' age_pyramid(dat3, age_group = AGE)
#' theme_set(old)
age_pyramid <- function(data, age_group = "age_group", split_by = "sex",
                        stack_by = NULL, count = NULL,  
                        proportional = FALSE, na.rm = TRUE,
                        show_midpoint = TRUE, vertical_lines = FALSE,
                        horizontal_lines = TRUE, pyramid = TRUE,
                        pal = NULL) {


  stop_if_not_df_or_svy(data, deparse(substitute(data)))

  age_group <- get_var(data, !!rlang::enquo(age_group))
  split_by  <- get_var(data, !!rlang::enquo(split_by))
  stack_by  <- get_var(data, !!rlang::enquo(stack_by))
  count     <- get_var(data, !!rlang::enquo(count))


  if (!is.factor(as.data.frame(data)[[age_group]])) {
    stop("age group must be a factor")
  }

  if (length(stack_by) == 0) {
    stack_by <- split_by
  }

  ag <- rlang::sym(age_group)
  sb <- rlang::sym(split_by)
  st <- rlang::sym(stack_by)

  # Count the plot data --------------------------------------------------------
  if (length(count) == 0) {
    plot_data <- aggregate_by_age(
      data,
      age_group    = age_group,
      stack_by     = stack_by,
      split_by     = split_by,
      proportional = proportional,
      na.rm        = na.rm
    )
  } else {
    plot_data <- dplyr::rename(data, n = !!rlang::sym(count))
  }
  # gathering the levels for each of the elements ------------------------------
  age_levels <- levels(plot_data[[age_group]])
  max_age_group <- age_levels[length(age_levels)]

  # Splitting levels without missing data
  split_levels <- plot_data[[split_by]]
  split_levels <- if (is.factor(split_levels)) levels(split_levels) else unique(split_levels)
  split_levels <- split_levels[!is.na(split_levels)]

  # Stacking levels assuming there is no missing data
  stk_levels <- plot_data[[stack_by]]
  stk_levels <- if (is.factor(stk_levels)) levels(stk_levels) else unique(stk_levels)

  stopifnot(length(split_levels) >= 1L)

  # Switch between pyramid and non-pyramid shape -------------------------------
  # This will only result in a pyramid if the user specifies so AND the split
  # levels is binary.
  split_measured_binary <- pyramid && length(split_levels) == 2L

  if (split_measured_binary) {
    maxdata <- dplyr::group_by(plot_data, !!ag, !!sb, .drop = FALSE)
  } else {
    maxdata <- dplyr::group_by(plot_data, !!ag, .drop = FALSE)
  }

  # find the maximum x axis position
  maxdata <- dplyr::tally(maxdata, wt = !!quote(n))
  max_n <- max(abs(maxdata[["n"]]), na.rm = TRUE)

  if (proportional) {
    lab_fun <- function(i) scales::percent(abs(i))
    y_lab <- "proportion"
  } else {
    lab_fun <- function(i) format(abs(i), big.mark = ",", trim = TRUE)
    y_lab <- "counts"
  }

  stopifnot(is.finite(max_n), max_n > 0)

  # Make sure the breaks are correct for the plot size
  the_breaks <- pretty(c(0, max_n), min.n = 5)
  the_breaks <- if (split_measured_binary) c(-rev(the_breaks[-1]), the_breaks) else the_breaks


  if (split_measured_binary) {
    # If the user has a binary level and wants to plot the data in a pyramid,
    # then we need to make the counts for the primary level negative so that
    # they appear to go to the left on the plot.
    plot_data[["n"]] <- ifelse(plot_data[[split_by]] == split_levels[[1L]], -1L, 1L) * plot_data[["n"]]
    maxdata[["d"]] <- ifelse(maxdata[[split_by]] == split_levels[[1L]], -1L, 0L) * maxdata[["n"]]
    # If we are labelling the center, then we can get it by summing the values over the age groups
    maxdata <- dplyr::summarise(maxdata, center = sum(!!quote(d)) + sum(!!quote(n)) / 2)
  } else {
    maxdata[["center"]] <- maxdata[["n"]] / 2
  }

  # Create base plot -----------------------------------------------------------
  pyramid <- ggplot(plot_data, aes(x = !!ag, y = !!quote(n))) +
    theme(axis.line.y = element_blank()) +
    labs(y = y_lab)
  pal <- if (is.function(pal)) pal(length(stk_levels)) else pal

  if (!split_measured_binary) {
    # add the background layer if the split is not binary
    maxdata[["zzzzz_alpha"]] <- "Total"
    pyramid <- pyramid +
      geom_col(aes(alpha = !!quote(zzzzz_alpha)), fill = "grey80", color = "grey20", data = maxdata)
  }

  # Add bars, scales, and themes -----------------------------------------------
  pyramid <- pyramid +
    geom_col(aes(group = !!sb, fill = !!st), color = "grey20") +
    coord_flip()
  if (is.null(pal)) {
    pyramid <- pyramid + scale_fill_brewer(type = "qual", guide = guide_legend(order = 1))
  } else {
    pyramid <- pyramid + scale_fill_manual(values = pal, guide = guide_legend(order = 1))
  }
  EXPANSION <- if (utils::packageVersion("ggplot2") < "3.3.0") ggplot2::expand_scale else ggplot2::expansion
  pyramid <- pyramid +
    scale_y_continuous(
      limits = if (split_measured_binary) range(the_breaks) else c(0, max_n),
      breaks = the_breaks,
      labels = lab_fun,
      expand = EXPANSION(mult = 0.02, add = 0)
    ) +
    scale_x_discrete(drop = FALSE) # note: drop = FALSE important to avoid missing age groups

  if (!split_measured_binary) {
    # Wrap the categories if the split is not binary
    pyramid <- pyramid +
      facet_wrap(split_by) +
      scale_alpha_manual(values = 0.5, guide = guide_legend(title = NULL, order = 3))
  }
  if (vertical_lines == TRUE) {
    pyramid <- pyramid +
      geom_hline(yintercept = the_breaks, linetype = "dotted", colour = "grey50")
  }


  if (show_midpoint) {
    maxdata <- dplyr::arrange(maxdata, !!ag)
    maxdata[["x"]] <- seq_along(maxdata[[age_group]]) - 0.25
    maxdata[["xend"]] <- maxdata[["x"]] + 0.5
    maxdata[["halfway"]] <- "midpoint"
    pyramid <- pyramid +
      geom_segment(aes(
        x        = !!quote(x),
        xend     = !!quote(xend),
        y        = !!quote(center),
        yend     = !!quote(center),
        linetype = !!quote(halfway)
      ),
      color = "grey20",
      key_glyph = "vpath", # NOTE: key_glyph is only part of ggplot2 >= 2.3.0; this will warn otherwise
      data = maxdata
      ) +
      scale_linetype_manual(values = "dashed", guide = guide_legend(title = NULL, order = 2))
  }

  if (split_measured_binary && stack_by != split_by) {
    # If the split is binary and we have both stacked and split data, then we
    # need to label the groups. We do so by adding a label annotation
    pyramid <- pyramid +
      annotate(
        geom = "label",
        x = max_age_group,
        y = -diff(the_breaks)[1],
        vjust = 0.5,
        hjust = 1,
        label = split_levels[[1]]
      ) +
      annotate(
        geom = "label",
        x = max_age_group,
        y = diff(the_breaks)[1],
        vjust = 0.5,
        hjust = 0,
        label = split_levels[[2]]
      )
  }

  if (horizontal_lines == TRUE) {
    pyramid <- pyramid + theme(panel.grid.major.y = element_line(linetype = 2))
  }

  pyramid <- pyramid +
    geom_hline(yintercept = 0) # add vertical line

  pyramid
}

