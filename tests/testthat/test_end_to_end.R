context("End-to-end tests")

make_fake_data <- function(N) {
  data.frame(response1 = rnorm(N),
             response2 = rnorm(N),
             response3 = rnorm(N),
             numeric1  = rnorm(N),
             numeric2  = rnorm(N),
             numeric3  = rnorm(N))
}

set.seed(1234)
df <- make_fake_data(1e4)
df$factor1 <- sample(letters, 1e4, TRUE)
df$factor2 <- c(sample(letters, 9e3, TRUE), rep(NA, 1e3))
df$factor3 <- sample(letters[1:5], 1e4, TRUE)
plot_cols <- c("response1", "response2", "response3")

even_unif <- universe(df, plot_cols, "numeric1", 20)
quant_unif <- universe(df, plot_cols, "numeric1", 20, cut_type = "quantile")
even_cart <- universe(df, plot_cols, "numeric1", 20, scale = "cartesian")
quant_cart <- universe(df, plot_cols, "numeric1", 20, cut_type = "quantile",
                       scale = "cartesian")
factor_unif <- universe(df, plot_cols, "factor1", 20)
factor_unif_na <- universe(df, plot_cols, "factor2", 20)
factor_unif_no_other <- universe(df, plot_cols, "factor3", 20)


test_that("even and uniform working end-to-end", {
  expect_is(even_unif, "plotly")
  expect_is(even_unif, "data.table")
  expect_is(even_unif$grp_by_col, "factor")

  expected_line <- c(2, 7, 23, 58, 158, 330, 609, 881, 1195, 1386, 1463,
                     1336, 1067, 703, 422, 203, 90, 52, 8, 7)

  expect_equal(even_unif$value, expected_line)
})

test_that("quantile and uniform working end-to-end", {
  expect_is(quant_unif, "plotly")
  expect_is(quant_unif, "data.table")
  expect_is(quant_unif$grp_by_col, "factor")

  expected_line <- c(500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500,
                     500, 500, 500, 500, 500, 500, 500)

  expect_equal(quant_unif$value, expected_line)
})

test_that("even and cartesian working end-to-end", {
  expect_is(even_cart, "plotly")
  expect_is(even_cart, "data.table")
  expect_is(even_cart$grp_by_col, "numeric")

  expected_x <- c(-3.7, -3.17, -2.84, -2.49, -2.12, -1.73, -1.36, -1, -0.62, -0.26,
                  0.12, 0.48, 0.85, 1.23, 1.6, 1.96, 2.35, 2.71, 3.02, 3.47)

  expected_val1 <- c(-1.288, -0.282, -0.2, -0.091, 0.094, 0.041, -0.036, 0.08,
                     -0.003, 0.015, -0.01, -0.039, -0.004, -0.02, 0.079, 0.096,
                     0.081, 0.224, -0.655, 0.067)

  expected_val2 <- c(-0.376, -0.454, -0.103, -0.128, -0.095, -0.039, 0.017, -0.005,
                     -0.037, 0.005, -0.026, 0.024, -0.001, -0.011, 0.009, 0.043,
                     0.051, -0.102, -0.033, 0.008)

  expected_val3 <- c(0.741, 0.273, -0.16, 0.045, -0.02, -0.002, 0.03, -0.018, 0.013,
                     0.011, -0.023, -0.025, 0.076, -0.004, -0.04, 0.043, 0.057, 0.025,
                     0.276, -0.35)

  expect_equal(round(even_cart[variable == "response1", grp_by_col], 2),
               expected_x)
  expect_equal(round(even_cart[variable == "response2", grp_by_col], 2),
               expected_x)
  expect_equal(round(even_cart[variable == "response3", grp_by_col], 2),
               expected_x)

  expect_equal(round(even_cart[variable == "response1", value], 3),
               expected_val1)
  expect_equal(round(even_cart[variable == "response2", value], 3),
               expected_val2)
  expect_equal(round(even_cart[variable == "response3", value], 3),
               expected_val3)
})

test_that("quantile and cartesian working end-to-end", {
  expect_is(quant_cart, "plotly")
  expect_is(quant_cart, "data.table")
  expect_is(quant_cart$grp_by_col, "numeric")

  expected_x <- c(-2.04, -1.45, -1.16, -0.94, -0.76, -0.59, -0.45, -0.31, -0.18,
                  -0.05, 0.08, 0.21, 0.34, 0.48, 0.62, 0.77, 0.95, 1.17, 1.46, 2.08)

  expected_val1 <- c(0.038, -0.054, 0.07, 0.072, -0.016, -0.033, 0.067, -0.042, 0.055,
                     -0.018, 0.031, -0.063, -0.016, 0.01, -0.062, -0.014, -0.002, -0.059,
                     0.086, 0.075)

  expected_val2 <- c(-0.078, 0.037, -0.023, 0.004, -0.036, -0.009, -0.027, -0.032, 0.002,
                     0.017, -0.06, -0.031, 0.053, 0.046, -0.011, 0.017, -0.031, -0.038,
                     0.021, 0.025)

  expected_val3 <- c(-0.001, 0.01, 0.003, -0.013, 0.051, 0.02, -0.037, 0.026, 0.025, -0.03,
                     -0.009, -0.066, -0.071, 0.038, 0.037, 0.016, 0.088, -0.002, -0.023, 0.021)

  expect_equal(round(quant_cart[variable == "response1", grp_by_col], 2),
               expected_x)
  expect_equal(round(quant_cart[variable == "response2", grp_by_col], 2),
               expected_x)
  expect_equal(round(quant_cart[variable == "response3", grp_by_col], 2),
               expected_x)

  expect_equal(round(quant_cart[variable == "response1", value], 3),
               expected_val1)
  expect_equal(round(quant_cart[variable == "response2", value], 3),
               expected_val2)
  expect_equal(round(quant_cart[variable == "response3", value], 3),
               expected_val3)
})

test_that("factor plots working end-to-end", {
  expect_is(factor_unif, "plotly")
  expect_is(factor_unif, "data.table")
  expect_is(factor_unif$grp_by_col, "factor")
  expect_equal(nlevels(factor_unif$grp_by_col), 20)

  expected_val <- c(422, 408, 406, 405, 403, 402, 400, 399, 398, 398, 395,
                    393, 392, 387, 381, 379, 378, 378, 375, 2501)
  expect_equal(factor_unif$value, expected_val)

  expected_val_na <- c(414, 404, 384, 377, 362, 360, 358, 353, 352, 349, 349,
                       348, 348, 344, 340, 338, 336, 334, 330, 2220, 1000)
  expect_equal(factor_unif_na$value, expected_val_na)

  expected_val_no_other <- c(2061, 2034, 2002, 1991, 1912)
  expect_equal(factor_unif_no_other$value, expected_val_no_other)
})
