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
plot_cols <- c("response1", "response2", "response3")

even_unif <- universe(df, plot_cols, "numeric1", 20)
quant_unif <- universe(df, plot_cols, "numeric1", 20, cut_type = "quantile")
even_cart <- universe(df, plot_cols, "numeric1", 20, scale = "cartesian")
quant_cart <- universe(df, plot_cols, "numeric1", 20, cut_type = "quantile",
                       scale = "cartesian")
factor_unif <- universe(df, plot_cols, "factor1", 20)


test_that("even and uniform working end-to-end", {
  expect_is(even_unif, "plotly")
  expect_is(even_unif, "data.table")
  expect_is(even_unif$grp_by_col, "factor")

  expected_line <- c(3, 7, 26, 85, 197, 410, 755, 1038, 1365, 1511, 1505, 1262,
                     860, 528, 268, 102, 60, 11, 7)

  expect_equal(even_unif$value, expected_line)
})

test_that("quantile and uniform working end-to-end", {
  expect_is(quant_unif, "plotly")
  expect_is(quant_unif, "data.table")
  expect_is(quant_unif$grp_by_col, "factor")

  expected_line <- c(527, 526, 526, 527, 526, 526, 526, 527, 526, 526, 526, 527,
                     526, 526, 526, 527, 526, 526, 527)

  expect_equal(quant_unif$value, expected_line)

})

test_that("even and cartesian working end-to-end", {
  expect_is(even_cart, "plotly")
  expect_is(even_cart, "data.table")
  expect_is(even_cart$numeric1, "numeric")

  expected_x <- c(-3.61, -3.12, -2.81, -2.40, -2.01, -1.63, -1.24, -0.85, -0.46,
                  -0.08, 0.32, 0.71, 1.10, 1.49, 1.88, 2.28, 2.66, 2.99, 3.47)

  expected_val1 <- c(-0.875, -0.254, -0.211, -0.033, 0.036, 0.040, 0.013, 0.031,
                     -0.006, 0.018, -0.009, -0.040, -0.021, 0.040, 0.123, 0.142,
                      0.092, -0.286, 0.067)

  expected_val2 <- c(-0.145, -0.617, -0.087, 0.041, -0.059, -0.037, -0.004,
                     -0.015, -0.018, -0.013, 0.026, 0.009, -0.056, -0.001,
                     0.036, 0.085, -0.086, 0.138, 0.008)

  expected_val3 <- c(0.673, 0.143, -0.196, -0.028, -0.004, 0.025, 0.014, 0.01,
                     -0.003, 0.005, -0.029, 0.031, 0.022, -0.026, 0.019, 0.101,
                     -0.055, 0.18, -0.35)

  expect_equal(round(even_cart[variable == "response1", numeric1], 2),
               expected_x)
  expect_equal(round(even_cart[variable == "response2", numeric1], 2),
               expected_x)
  expect_equal(round(even_cart[variable == "response3", numeric1], 2),
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
  expect_is(quant_cart$numeric1, "numeric")

  expected_x <- c(-2.02, -1.42, -1.13, -0.91, -0.72, -0.55, -0.4, -0.26, -0.12,
                  0.02, 0.15, 0.29, 0.43, 0.58, 0.73, 0.91, 1.14, 1.43, 2.06)

  expected_val1 <- c(0.042, -0.065, 0.068, 0.075, -0.028, -0.002, 0.045, -0.004,
                     0.016, -0.003, 0.005, -0.028, -0.03, -0.067, 0.022, -0.04,
                     -0.05, 0.09, 0.072)

  expected_val2 <- c(-0.07, 0.011, 0.003, -0.016, -0.031, -0.031, -0.011, 0.001,
                     -0.013, 0.025, -0.111, 0.057, 0.087, -0.043, 0.021, -0.007,
                     -0.041, 0.012, 0.01)

  expected_val3 <- c(-0.003, 0.015, 0.013, -0.013, 0.037, -0.015, -0.013, 0.037,
                     0.026, -0.007, -0.091, -0.052, 0.015, -0.012, 0.059, 0.092,
                     -0.014, -0.017, 0.02)

  expect_equal(round(quant_cart[variable == "response1", numeric1], 2),
               expected_x)
  expect_equal(round(quant_cart[variable == "response2", numeric1], 2),
               expected_x)
  expect_equal(round(quant_cart[variable == "response3", numeric1], 2),
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

  expected_val <- c(405, 402, 398, 375, 381, 406, 378, 399, 398, 379, 400, 2501,
                    403, 393, 395, 408, 387, 392, 422, 378)
  expect_equal(factor_unif$value, expected_val)
})
