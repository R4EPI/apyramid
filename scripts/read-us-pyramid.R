# This set of functions will read in an excel data set from the US Census and
# produce a table for use for testing. No guarantees that this works
# The data I got was from here:
# https://census.gov/data/tables/2018/demo/age-and-sex/2018-age-sex-composition.html
#
`%>%` <- magrittr::`%>%`
#' Process pyramid data from the US census
#'
#' @param age_table a single-column data frame that has ages in order with the
#'   patterns "Under X years", "X to X years", and "X years and over".
#' @param genders table of counts and percentages by gender with the column name
#'   pattern of `[$gender]_[np]` where `$gender` is a stand-in variable and 
#'   [np] refers to the regex of n (indicating counts) or p (indicating percentages)
#'
#' @return a long table of US census figures by age and sex
process_pyramids <- function(age_table, counts, what = "gender") {
  WHAT <- rlang::sym(what)
  # Bind together the ages and the genders
  dplyr::bind_cols(age_table, counts) %>%
    # Process the ages for better labels
    dplyr::mutate(age = dplyr::case_when(
      stringr::str_detect(age, "Under") ~ stringr::str_replace_all(age, ".?Under (\\d+?) years", "<\\1"),
      stringr::str_detect(age, "to")    ~ stringr::str_replace_all(age, ".?(\\d+?) to (\\d+?) years", "\\1-\\2"),
      stringr::str_detect(age, "over")  ~ stringr::str_replace_all(age, ".?(\\d+?) years and over", "\\1+"),
      TRUE ~ "total"
    )) %>%
    # arrange the factors in order
    dplyr::mutate(age = forcats::fct_inorder(age)) %>%
    # make the table long
    tidyr::pivot_longer(-age, names_to = c(what, "type"), names_sep = '_') %>%
    # convert n/p to count/percent
    dplyr::mutate(type = dplyr::if_else(type == "n", "count", "percent")) %>%
    # widen to include counts and percents in different columns
    tidyr::pivot_wider(names_from = "type", values_from = "value") %>%
    # clean up types
    dplyr::mutate(count = as.integer(count)) %>%
    dplyr::mutate(!!WHAT := forcats::fct_inorder(!!WHAT))
}

download_maybe <- function(path) {
  if (file.exists(path)) {
    return(path)
  } else if (grepl("^https?\\://", path)) {
    tmp <- file.path(tempdir(), basename(path))
    res <- if (file.exists(tmp)) TRUE else download.file(path, tmp) == 0
    if (res) {
      path <- tmp
    } else {
      stop("file could not be downloaded")
    }
    return(path)
  } else {
    stop("file or URL not known")
  }
}

#' get a simple US census pyramid
#'
#' @param path path to a file
#' @param full boolean to get full five-year incremental dat or summarized
#'
#' @return a long census table`
#'
#' @examples
#' s <- 'https://www2.census.gov/programs-surveys/demo/tables/age-and-sex/2018/age-sex-composition/2018gender_table1.xls'
#' get_simple_pyramid(s, full = TRUE)
#' get_simple_pyramid(s, full = FALSE)
get_simple_pyramid <- function(path, full = TRUE) {
  path     <- download_maybe(path)
  the_cols <- c('male_n', 'male_p', 'female_n', 'female_p')

  np <- readxl::read_excel(path, 
                           range = if (full) "D7:G25" else "D27:G32", 
                           col_names = the_cols)

  age_table <- readxl::read_excel(path, 
                                  range = if (full) "A7:A25" else "A27:A32", 
                                  col_names = "age")
  process_pyramids(age_table, np)

}

#' get a stratified US census pyramid
#'
#' @param path path to a file
#' @param what the type of stratification
#'
#' @return a long census table
#'
#' @examples
#' x <- 'https://www2.census.gov/programs-surveys/demo/tables/age-and-sex/2018/age-sex-composition/2018gender_table12.xls'
#' generation <- get_stratified_pyramid(x, what = "Generation")
get_stratified_pyramid <- function(path, what = "Generation") {
  path    <- download_maybe(path)
  males   <- cellranger::cell_limits(c(28, 4), c(46, NA))
  females <- cellranger::cell_limits(c(48, 4), c(66, NA))

  categories <- readxl::read_excel(path,
                                   range = cellranger::cell_limits(c(6, 4), c(6, NA)),
                                   col_names = FALSE)
  categories <- as.character(categories)[!is.na(as.vector(categories))]
  categories <- sort(apply(expand.grid(categories, c("n", "p")), 1, paste, collapse = "_"))

  male_np        <- readxl::read_excel(path, range = males, col_names = categories)
  male_age_table <- readxl::read_excel(path, range = "A28:A46", col_names = "age")
  male_table     <- process_pyramids(male_age_table, male_np, what = what)

  female_np        <- readxl::read_excel(path, range = females, col_names = categories)
  female_age_table <- readxl::read_excel(path, range = "A48:A66", col_names = "age")
  female_table     <- process_pyramids(female_age_table, female_np, what = what)

  dplyr::bind_rows(male = male_table, female = female_table, .id = "gender") %>%
    dplyr::arrange(age, !!rlang::sym(what), desc(gender)) %>%
    dplyr::mutate(gender = forcats::fct_inorder(gender))
}
