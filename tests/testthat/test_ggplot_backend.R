context("ggplot backend doing something")

# ggplot backend can only draw due to gtable tricks.
# Can at least test it's producing output by drawing a png and checking
# its size. Open to suggestions on handling this better.

test_that("ggplot2 backend png works", {
  tmp <- tempfile()
  pdf(tmp)
  universe(diamonds,
           plot_cols = c("x", "y", "z"),
           by_col = "price",
           buckets = 20,
           cut_type = "quantile",
           scale = "uniform",
           backend = "ggplot2")
  dev.off()

  plot_size <- file.info(tmp)$size

  pdf(tmp)
  dev.off()

  empty_size <- file.info(tmp)$size
  # Expect at least 2kb of additional information
  expect_gt(plot_size, empty_size + 2 * 1024)
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
