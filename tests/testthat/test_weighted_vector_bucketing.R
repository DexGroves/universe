context("Vector bucketer behaving for weights")


test_that("Cuts appropriately on type = 'even'.", {
  by_vector <- seq(100)
  weights <- c(rep(1, 50), rep(0, 50))
  vb <- WeightedVectorBucketer$new("even")

  chopped_vector <- vb$cut_vector(by_vector, weights, 10)
  expect_equal(nlevels(chopped_vector), 10)
  expect_equal(length(chopped_vector), 50)

  chopped_map <- as.numeric(as.factor(as.numeric(chopped_vector)))
  expect_equal(chopped_map, sort(rep(seq(10), 5)))
})


test_that("Cuts appropriately on type = 'quantile'.", {
  by_vector <- log(seq(100))
  weights <- c(rep(1, 50), rep(0, 50))
  vb <- WeightedVectorBucketer$new("quantile")

  chopped_vector <- vb$cut_vector(by_vector, weights, 10)
  expect_equal(nlevels(chopped_vector), 10)
  expect_equal(length(chopped_vector), 50)

  chopped_map <- as.numeric(as.factor(as.numeric(chopped_vector)))
  expect_equal(chopped_map, sort(rep(seq(10), 5)))
})
