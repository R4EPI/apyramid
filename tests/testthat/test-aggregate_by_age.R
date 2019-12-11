test_that('errors are thrown properly', {
  expect_error(aggregate_by_age(iris$Species), "Input must be a data frame")
})
