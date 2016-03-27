context("Vector bucketer behaving")


test_that("Cuts appropriately on type = 'even'.", {
  by_vector <- seq(50)
  vb <- VectorBucketer$new("even")

  chopped_vector <- vb$cut_vector(by_vector, 10)
  expect_equal(nlevels(chopped_vector), 10)
  expect_equal(length(chopped_vector), 50)

  chopped_map <- as.numeric(as.factor(as.numeric(chopped_vector)))
  expect_equal(chopped_map, sort(rep(seq(10), 5)))
})


test_that("Cuts appropriately on type = 'quantile'.", {
  by_vector <- log(seq(50))
  vb <- VectorBucketer$new("quantile")

  chopped_vector <- vb$cut_vector(by_vector, 10)
  expect_equal(nlevels(chopped_vector), 10)
  expect_equal(length(chopped_vector), 50)

  chopped_map <- as.numeric(as.factor(as.numeric(chopped_vector)))
  expect_equal(chopped_map, sort(rep(seq(10), 5)))
})
