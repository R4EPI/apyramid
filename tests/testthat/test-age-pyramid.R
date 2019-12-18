context("age pyramid tests")

# set.seed(2018-01-15)
# ages <- cut(sample(80, 150, replace = TRUE),
#            breaks = c(0, 5, 10, 30, 50, 80, 100), right = FALSE)
# sex  <- sample(c("Female", "Male"), 150, replace = TRUE)
# gender <- sex
# gender[sample(5)] <- "NB"
# ill  <- sample(0:1, 150, replace = TRUE)
ages <- structure(c(
  4L, 4L, 5L, 3L, 3L, 1L, 5L, 5L, 4L, 3L, 5L, 2L, 5L, 5L,
  2L, 3L, 5L, 4L, 3L, 5L, 1L, 3L, 5L, 3L, 1L, 5L, 2L, 5L,
  4L, 3L, 4L, 2L, 5L, 1L, 4L, 3L, 4L, 3L, 3L, 4L, 4L, 2L,
  4L, 3L, 3L, 4L, 3L, 4L, 2L, 5L, 5L, 4L, 4L, 2L, 5L, 3L,
  5L, 4L, 4L, 5L, 3L, 5L, 2L, 4L, 4L, 5L, 5L, 5L, 5L, 5L,
  5L, 3L, 1L, 4L, 3L, 3L, 3L, 3L, 4L, 4L, 5L, 5L, 5L, 3L,
  5L, 5L, 5L, 3L, 4L, 1L, 3L, 5L, 5L, 3L, 5L, 5L, 4L, 5L,
  5L, 2L, 4L, 3L, 2L, 3L, 5L, 4L, 3L, 4L, 6L, 3L, 4L, 3L,
  5L, 3L, 5L, 5L, 4L, 5L, 4L, 4L, 4L, 5L, 5L, 5L, 3L, 2L,
  5L, 1L, 3L, 5L, 5L, 4L, 2L, 2L, 5L, 5L, 1L, 1L, 2L, 4L,
  5L, 4L, 3L, 5L, 3L, 5L, 3L, 4L, 4L, 4L
),
.Label = c("[0,5)", "[5,10)", "[10,30)", "[30,50)", "[50,80)", "[80,100)"),
class = "factor"
)
sex <- structure(c(
  1L, 1L, 2L, 1L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 2L, 1L,
  1L, 2L, 2L, 1L, 2L, 2L, 2L, 1L, 2L, 1L, 2L, 2L, 2L, 1L, 1L,
  2L, 1L, 2L, 2L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 2L, 1L, 1L,
  2L, 1L, 1L, 1L, 2L, 2L, 1L, 1L, 1L, 2L, 1L, 2L, 1L, 1L, 2L,
  2L, 1L, 2L, 2L, 2L, 1L, 2L, 2L, 1L, 1L, 1L, 2L, 2L, 2L, 2L,
  1L, 2L, 1L, 1L, 1L, 1L, 2L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 1L,
  1L, 1L, 1L, 1L, 2L, 2L, 1L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 2L,
  2L, 2L, 1L, 2L, 2L, 1L, 1L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L,
  1L, 2L, 2L, 1L, 2L, 1L, 2L, 1L, 1L, 1L, 2L, 1L, 2L, 1L, 2L,
  2L, 1L, 2L, 1L, 2L, 1L, 2L, 2L, 1L, 2L, 1L, 1L, 1L, 2L, 1L
),
.Label = c("Male", "Female"),
class = "factor"
)
gender <- structure(c(
  2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 3L, 3L, 3L, 1L, 3L,
  1L, 1L, 3L, 3L, 1L, 3L, 3L, 3L, 1L, 3L, 1L, 3L, 3L, 3L,
  1L, 1L, 3L, 1L, 3L, 3L, 1L, 1L, 1L, 3L, 1L, 1L, 1L, 1L,
  3L, 1L, 1L, 3L, 1L, 1L, 1L, 3L, 3L, 1L, 1L, 1L, 3L, 1L,
  3L, 1L, 1L, 3L, 3L, 1L, 3L, 3L, 3L, 1L, 3L, 3L, 1L, 1L,
  1L, 3L, 3L, 3L, 3L, 1L, 3L, 1L, 1L, 1L, 1L, 3L, 1L, 3L,
  3L, 3L, 3L, 3L, 3L, 1L, 1L, 1L, 1L, 1L, 3L, 3L, 1L, 3L,
  3L, 3L, 3L, 3L, 1L, 1L, 3L, 3L, 3L, 1L, 3L, 3L, 1L, 1L,
  3L, 1L, 1L, 3L, 3L, 1L, 1L, 3L, 1L, 3L, 3L, 1L, 3L, 1L,
  3L, 1L, 1L, 1L, 3L, 1L, 3L, 1L, 3L, 3L, 1L, 3L, 1L, 3L,
  1L, 3L, 3L, 1L, 3L, 1L, 1L, 1L, 3L, 1L
),
.Label = c("Male", "NB", "Female"),
class = "factor"
)
ill <- c(
  0L, 1L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 1L, 0L, 1L, 1L, 1L, 0L,
  0L, 0L, 0L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 0L, 1L, 0L, 1L, 0L,
  0L, 1L, 1L, 0L, 0L, 1L, 0L, 1L, 1L, 1L, 1L, 1L, 0L, 0L, 1L, 1L, 1L,
  1L, 0L, 1L, 1L, 1L, 0L, 1L, 0L, 1L, 1L, 1L, 0L, 0L, 1L, 1L, 1L, 0L,
  1L, 0L, 0L, 1L, 0L, 0L, 1L, 1L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 1L, 1L,
  0L, 0L, 1L, 1L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 0L, 1L, 1L, 1L,
  1L, 0L, 0L, 1L, 1L, 1L, 0L, 0L, 1L, 1L, 0L, 0L, 1L, 1L, 1L, 0L, 1L,
  0L, 0L, 1L, 1L, 1L, 0L, 1L, 1L, 1L, 0L, 0L, 1L, 0L, 1L, 0L, 0L, 0L,
  0L, 1L, 0L, 0L, 0L, 1L, 1L, 0L, 0L, 0L, 0L, 1L, 1L, 1L
)

dat <- data.frame(
  AGE = ages,
  sex = factor(sex, c("Male", "Female")),
  gender = factor(gender, c("Male", "NB", "Female")),
  ill = ill,
  stringsAsFactors = FALSE
)

ap1 <- age_pyramid(dat, age_group = "AGE")
ap2 <- age_pyramid(dat, age_group = "AGE", split_by = "ill")
apg <- age_pyramid(dat, age_group = "AGE", split_by = gender)
apnp <- age_pyramid(dat, age_group = AGE, pyramid = FALSE)
# missing data
datd <- dat[dat$AGE != levels(dat$AGE)[2], , drop = FALSE]
ap3 <- age_pyramid(datd, age_group = "AGE")
data(us_2018, package = "apyramid")
data(us_ins_2018, package = "apyramid")
us_2018$prop <- us_2018$percent / 100
us_ins_2018$prop <- us_ins_2018$percent / 100
us2018c <- age_pyramid(us_2018, age, gender, count = count)
us2018p <- age_pyramid(us_2018, age, gender, count = prop, proportion = TRUE)
usi2018c <- age_pyramid(us_ins_2018, age, gender, insured, count = count)
usi2018p <- age_pyramid(us_ins_2018, age, gender, insured, count = prop, proportion = TRUE)
us2018pal <- age_pyramid(us_2018, age, gender, count = count, pal = c("#FFFF55", "#55FFFF"), vertical_lines = TRUE)

test_that("plots appear the same", {
  skip_if_not_installed("vdiffr")
  old <- ggplot2::theme_set(ggplot2::theme_classic(base_size = 18))
  vdiffr::expect_doppelganger("default age pyramid", ap1)
  vdiffr::expect_doppelganger("ill age pyramid", ap2)
  vdiffr::expect_doppelganger("missing age pyramid", ap3)
  vdiffr::expect_doppelganger("gender age pyramid", apg)
  vdiffr::expect_doppelganger("default no pyramid", apnp)
  vdiffr::expect_doppelganger("us 2018 counts", us2018c)
  vdiffr::expect_doppelganger("us 2018 proportions", us2018p)
  vdiffr::expect_doppelganger("us insured 2018 counts", usi2018c)
  vdiffr::expect_doppelganger("us insured 2018 proportions", usi2018p)
  vdiffr::expect_doppelganger("with custom palette and vertical lines", us2018pal)
  ggplot2::theme_set(old)
})


test_that("age pyramid returns a ggplot2 object", {
  expect_is(ap1, "ggplot")
  expect_is(ap2, "ggplot")
})

test_that("errors are thrown with invalid inputs", {

  expect_error(age_pyramid(as.matrix(us_2018), age, gender, count = count),
               "as.matrix(us_2018) must be a data frame or tbl_svy object",
               fixed = TRUE)

  expect_error(age_pyramid(us_2018, count, age, count = gender),
               "age group must be a factor")

})

test_that("toggling pyramid will turn off the pyramid", {
  expect_identical(names(ap1$data), names(apnp$data))
  # the absolute values of pyramid data are equal to the values of non-pyramid data
  expect_identical(abs(ap1$data$n), apnp$data$n)
  expect_failure(expect_equal(ap1$data$n, apnp$data$n))
})

test_that("choosing a column that doesn't exist results in an error", {
  expect_error(age_pyramid(dat, age_group = "pourg_ega"))
})

test_that("plot by gender works", {
  expect_true("gender" %in% colnames(apg$data))
  expect_equal(levels(apg$data$gender), c("Male", "NB", "Female"))
})

test_that("plot by sex default works", {
  expect_true("sex" %in% colnames(ap1$data))
  expect_equal(levels(ap1$data$sex), c("Male", "Female"))
  expect_true("AGE" %in% colnames(ap1$data))
  expect_false("ill" %in% colnames(ap1$data))
})

test_that("plot by ill works", {
  expect_true("ill" %in% colnames(ap2$data))
  expect_true("AGE" %in% colnames(ap2$data))
  expect_false("sex" %in% colnames(ap2$data))
  expect_equal(levels(ap2$data$ill), as.character(0:1))
})

test_that("missing levels are still plotted", {
  # Complete data has both groups
  expect_equal(sum(ap1$data$AGE == levels(dat$AGE)[2]), 2)
  # Incomplete data has groups replaced with NA
  expect_equal(sum(ap3$data$n[ap3$data$AGE == levels(dat$AGE)[2]]), 0)
})

test_that("missing split data are removed before plotting", {
  dat$sex[69] <- NA
  expect_warning(
    age_pyramid(dat, age_group = "AGE", na.rm = TRUE),
    "1 missing rows were removed (0 values from `AGE` and 1 values from `sex`)",
    fixed = TRUE
  )
  expect_silent(age_pyramid(dat, age_group = "AGE", na.rm = FALSE))
})

test_that("survey data is accounted for", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("srvyr")
  old <- ggplot2::theme_set(ggplot2::theme_classic(base_size = 18))
    
  data("api", package = "survey")

  dstrata <- srvyr::mutate(apistrat, 
     apicat = cut(api00, pretty(api00), include.lowest = TRUE, right = TRUE))
  dstrata <- srvyr::as_survey_design(dstrata, strata = stype, weights = pw)
   
  srv <- age_pyramid(dstrata, apicat, split_by = stype)
  vdiffr::expect_doppelganger("Survey data", srv)
  ggplot2::theme_set(old)

})
