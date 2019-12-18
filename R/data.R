#' US Census data for population, age, and gender
#'
#' All of these tables were read directly from the excel sources via custom
#' script located at \url{https://github.com/R4EPI/apyramid/blob/master/scripts/read-us-pyramid.R}.
#' 
#' @format 
#' 
#' All tables are in long [tibble][dplyr::tibble] format. There are three
#' columns common to all of the tables:
#'
#'  - **age** \[factor\] 18 ordered age groups in increments of five years from
#'    "<5" to  "85+"
#'  - **gender** \[factor\] 2 reported genders (male, female).
#'  - **count** \[integer\] Numbers in thousands. Civilian noninstitutionalized
#'    and military population.
#'
#' Below are specifics of each table beyond the stated three columns with names
#' as reported on the US census website
#'
#' \subsection{Population by Age and Sex (`us_2018`, `us_2008`)}{
#'   A tibble with 36 rows and 4 columns. 
#'   (`us_2018` source: \url{https://www2.census.gov/programs-surveys/demo/tables/age-and-sex/2018/age-sex-composition/2018gender_table1.xls})
#'   (`us_2008` source: \url{https://www2.census.gov/programs-surveys/demo/tables/age-and-sex/2008/age-sex-composition/2008gender_table1.xls})
#'  
#'   Additional columns: 
#'
#'    - **percent** \[numeric\] percent of the total US population rounded to the nearest 0.1%
#' }
#'
#' \subsection{Health Insurance by Sex and Age (`us_ins_2018`, `us_ins_2008`)}{
#'   A tibble with 72 rows and 5 columns. 
#'   (`us_ins_2018` source: \url{https://www2.census.gov/programs-surveys/demo/tables/age-and-sex/2018/age-sex-composition/2018gender_table14.xls})
#'   (`us_ins_2008` source: \url{https://www2.census.gov/programs-surveys/demo/tables/age-and-sex/2008/age-sex-composition/2008gender_table29.xls})
#'  
#'   Additional columns: 
#'
#'    - **insured** \[factor\] Either "Insured" or "Not insured" indicating insured status
#'    - **percent** \[numeric\] percent of each age and gender category insured rounded to the nearest 0.1%
#' }
#' 
#' \subsection{Generational Distribution of the Population by Sex and Age (`us_gen_2018`, `us_gen_2008`)}{
#'   A tibble with 108 rows and 5 columns. 
#'   (`us_gen_2018` source: \url{https://www2.census.gov/programs-surveys/demo/tables/age-and-sex/2018/age-sex-composition/2018gender_table13.xls})
#'   (`us_gen_2008` source: \url{https://www2.census.gov/programs-surveys/demo/tables/age-and-sex/2008/age-sex-composition/2008gender_table29.xls})
#'  
#'   Additional columns: 
#'
#'    - **generation** \[factor\] Three categories of generations in the US:
#'    First, Second, Third and higher (see note)
#'    - **percent** \[numeric\] percent of the total US population rounded to the nearest 0.1%
#'
#'  Note: from the US Census Bureau: The foreign born are considered first
#'  generation.  Natives with at least one foreign-born parent are considered
#'  second generation.  Natives with two native parents are considered
#'  third-and-higher generation.  
#' 
#' }
#' @source \url{https://census.gov/data/tables/2018/demo/age-and-sex/2018-age-sex-composition.html}
#' \url{https://census.gov/data/tables/2008/demo/age-and-sex/2008-age-sex-composition.html}
#' @rdname us_2018
"us_2018"

#' @rdname us_2018
#' @aliases us_2008
"us_2008"

#' @rdname us_2018
#' @aliases us_ins_2018
"us_ins_2018"

#' @rdname us_2018
#' @aliases us_ins_2008
"us_ins_2008"

#' @rdname us_2018
#' @aliases us_gen_2018
"us_gen_2018"

#' @rdname us_2018
#' @aliases us_gen_2008
"us_gen_2008"
