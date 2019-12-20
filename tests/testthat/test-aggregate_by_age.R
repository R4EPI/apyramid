test_that('errors are thrown properly', {
  expect_error(aggregate_by_age(iris$Species), 
               "iris$Species must be a data frame or tbl_svy object",
               fixed = TRUE)
})

get_flu <- function() {
  f <- outbreaks::fluH7N9_china_2013
  f$age <- cut(as.numeric(f$age), 
               breaks = pretty(as.numeric(f$age)), 
               include.lowest = TRUE,
               right = FALSE)
  return(f)
               
}

test_that('aggregation will do sums without missing', {

  skip_if_not_installed("outbreaks")
  f <- get_flu()

  # Without stratification
  expect_warning(agg <- aggregate_by_age(f, "age", "gender", na.rm = TRUE),
                 "2 missing rows were removed (0 values from `age` and 2 values from `gender`).",
                 fixed = TRUE)

  expect_named(agg, c("age", "gender", "n"))
  expect_equal(sum(agg$n), nrow(f) - 2)

  expect_setequal(agg$gender, c("f", "m"))
  expect_equal(levels(agg$gender), levels(f$gender))

  
  # With stratification
  expect_warning(agg <- aggregate_by_age(f, "age", "gender", "outcome", na.rm = TRUE),
                 "59 missing rows were removed (0 values from `age`, 2 values from `gender`, and 57 values from `outcome`).",
                 fixed = TRUE)
  expect_named(agg, c("age", "gender", "outcome", "n"))
  expect_equal(sum(agg$n), nrow(f) - 59)

  expect_setequal(agg$gender, c("f", "m"))
  expect_equal(levels(agg$gender), levels(f$gender))

  expect_setequal(agg$outcome, c("Death", "Recover"))
  expect_equal(levels(agg$outcome), levels(f$outcome))
})

test_that('aggregation will do proportions without missing', {

  skip_if_not_installed("outbreaks")
  f <- get_flu()

  # Without stratification
  expect_warning(agg <- aggregate_by_age(f, "age", "gender", na.rm = TRUE, proportion = TRUE),
                 "2 missing rows were removed (0 values from `age` and 2 values from `gender`).",
                 fixed = TRUE)

  expect_named(agg, c("age", "gender", "n"))
  expect_equal(sum(agg$n), 1)
  expect_equal(nrow(agg), nlevels(agg$age) * nlevels(agg$gender))

  expect_setequal(agg$gender, c("f", "m"))
  expect_equal(levels(agg$gender), levels(f$gender))

  
  # With stratification
  expect_warning(agg <- aggregate_by_age(f, "age", "gender", "outcome", na.rm = TRUE, proportion = TRUE),
                 "59 missing rows were removed (0 values from `age`, 2 values from `gender`, and 57 values from `outcome`).",
                 fixed = TRUE)
  expect_named(agg, c("age", "gender", "outcome", "n"))
  expect_equal(sum(agg$n), 1)
  expect_equal(nrow(agg), nlevels(agg$age) * nlevels(agg$gender) * nlevels(agg$outcome))

  expect_setequal(agg$gender, c("f", "m"))
  expect_equal(levels(agg$gender), levels(f$gender))

  expect_setequal(agg$outcome, c("Death", "Recover"))
  expect_equal(levels(agg$outcome), levels(f$outcome))
})


test_that('aggregation will do sums with missing', {

  skip_if_not_installed("outbreaks")
  f <- get_flu()
  expect_silent(agg <- aggregate_by_age(f, "age", "gender", na.rm = FALSE))
  expect_named(agg, c("age", "gender", "n"))
  expect_equal(sum(agg$n), nrow(f)) # counts should equal number of rows
  expect_setequal(agg$gender, c("f", "m", "Missing")) # missing will be a new factor

  expect_silent(agg <- aggregate_by_age(f, "age", "gender", "outcome", na.rm = FALSE))
  expect_named(agg, c("age", "gender", "outcome", "n"))
  expect_equal(sum(agg$n), nrow(f)) # counts should equal number of rows

  expect_setequal(agg$gender, c("f", "m", "Missing")) # missing will be a new factor
  expect_equal(levels(agg$gender), c(levels(f$gender), "Missing"))

  expect_setequal(agg$outcome, c("Death", "Recover", "Missing"))
  expect_equal(levels(agg$outcome), c(levels(f$outcome), "Missing"))

})

test_that('aggregation will do proportions with missing', {

  skip_if_not_installed("outbreaks")
  f <- get_flu()
  expect_silent(agg <- aggregate_by_age(f, "age", "gender", na.rm = FALSE, proportion = TRUE))
  expect_named(agg, c("age", "gender", "n"))
  expect_equal(sum(agg$n), 1) # counts should equal number of rows
  expect_setequal(agg$gender, c("f", "m", "Missing")) # missing will be a new factor
  expect_equal(nrow(agg), nlevels(agg$age) * nlevels(agg$gender))

  expect_silent(agg <- aggregate_by_age(f, "age", "gender", "outcome", na.rm = FALSE, proportion = TRUE))
  expect_named(agg, c("age", "gender", "outcome", "n"))
  expect_equal(sum(agg$n), 1) # counts should equal number of rows
  expect_equal(nrow(agg), nlevels(agg$age) * nlevels(agg$gender) * nlevels(agg$outcome))

  expect_setequal(agg$gender, c("f", "m", "Missing")) # missing will be a new factor
  expect_equal(levels(agg$gender), c(levels(f$gender), "Missing"))

  expect_setequal(agg$outcome, c("Death", "Recover", "Missing"))
  expect_equal(levels(agg$outcome), c(levels(f$outcome), "Missing"))

})
