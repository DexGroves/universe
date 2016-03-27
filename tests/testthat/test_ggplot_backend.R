context("ggplot backend doing something")

# ggplot backend can only draw due to gtable tricks.
# Can at least test it's producing output by drawing a pdf and checking
# its size. Open to suggestions on handling this better.

test_that("ggplot2 backend pdf works", {
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
