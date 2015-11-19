context("Erroring appropriately")

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

test_that("Errors for inappropriate cut_type and scale.", {
  expect_error(
    universe(df, c("response1", "response2", "response3"), "numeric1", 20,
             scale = "shlum shlummmm schlippiddy dop!"), "scale")
  expect_error(
    universe(df, c("response1", "response2", "response3"), "numeric1", 20,
             cut_type = "uh ohh! somersault jump!"), "cut_type")
})
