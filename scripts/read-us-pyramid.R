process_pyramids <- function(age_table, genders) {
  # Bind together the ages and the genders
  dplyr::bind_cols(age_table, genders) %>%
    # Process the ages for better labels
    dplyr::mutate(age = dplyr::case_when(
      stringr::str_detect(age, "Under") ~ stringr::str_replace_all(age, ".?Under (\\d+?) years", "<\\1"),
      stringr::str_detect(age, "to")    ~ stringr::str_replace_all(age, ".?(\\d+?) to (\\d+?) years", "\\1-\\2"),
      stringr::str_detect(age, "over")  ~ stringr::str_replace_all(age, ".?(\\d+?) years and over", "\\1+"),
      stringr::str_detect(age, "All ages") ~ "total"
    )) %>%
    # arrange the factors in order
    dplyr::mutate(age = forcats::fct_inorder(age)) %>%
    # make the table long
    tidyr::pivot_longer(-age, names_to = c("gender", "type"), names_sep = '_') %>%
    # convert n/p to count/percent
    dplyr::mutate(type = dplyr::if_else(type == "n", "count", "percent")) %>%
    # widen to include counts and percents in different columns
    tidyr::pivot_wider(names_from = "type", values_from = "value") %>%
    # clean up types
    dplyr::mutate(count = as.integer(count)) %>%
    dplyr::mutate(gender = forcats::fct_inorder(gender))
}

get_simple_pyramid <- function(path, full = TRUE) {
  the_cols <- c('male_n', 'male_p', 'female_n', 'female_p')
  np <- readxl::read_excel(path, 
                           range = if (full) "D7:G25" else "D27:G32", 
                           col_names = the_cols)

  age_table <- readxl::read_excel(path, 
                                  range = if (full) "A7:A25" else "A27:A32", 
                                  col_names = "age")
  process_pyramids(age_table, np)

}

get_stratified_pyramid <- function(path) {

}
