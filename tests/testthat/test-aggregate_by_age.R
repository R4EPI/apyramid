test_that('errors are thrown properly', {
  expect_error(aggregate_by_age(iris$Species), "Input must be a data frame")
})

test_that('aggregation works as expected', {

  skip_if_not_installed("outbreaks")
  

})
