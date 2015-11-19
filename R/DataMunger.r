initialize <- function(input_df, plot_cols, by_col) {
  self$plot_cols <- plot_cols
  self$by_col <- by_col
  self$vb <- VectorBucketer$new()
  self$df <- self$generate_core_df(input_df)
}

generate_core_df <- function(input_df) {
  data.table(input_df[, c(self$by_col, self$plot_cols)])
}

bucket_data <- function(buckets) {
  self$df[, grp_by_col := self$vb$cut_evenly(get(self$by_col), buckets)]
  self$bucketed_df <- self$df[, lapply(.SD, mean),
                              .SDcols = self$plot_cols, by = grp_by_col]
  invisible()
}

melt_df <- function() {
  self$melted_df <- melt(self$bucketed_df, id.vars = "grp_by_col")
  setkey(self$melted_df, grp_by_col)
}

#' @import R6
#' @import data.table
#' @import reshape2
DataMunger <- R6Class("DataMunger",
  public = list(
    plot_cols = NA,
    by_col = NA,
    df = NA,
    vb = NA,
    bucketed_df = NA,
    melted_df = NA,
    initialize = initialize,
    generate_core_df = generate_core_df,
    bucket_data = bucket_data,
    melt_df = melt_df))


cut_evenly <- function(cut_vector, buckets) {
  lower <- min(cut_vector)
  upper <- max(cut_vector)
  cut_points <- seq(lower, upper, length.out = buckets)
  cut(cut_vector, breaks = cut_points, include.lowest = TRUE)
}

VectorBucketer <- R6Class("VectorBucketer",
  public = list(
    cut_evenly = cut_evenly))