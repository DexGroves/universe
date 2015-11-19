#' @import plotly
#' @import magrittr
#' @import R6
#' @import data.table
#' @import reshape2
#' @export
universe <- function(input_df,
                     plot_cols,
                     by_col,
                     buckets = 10,
                     cut_type = "even",
                     scale = "uniform") {
  dm <- DataMunger$new(input_df, plot_cols, by_col, cut_type)
  pp <- PlotlyPlotter$new(scale)

  dm$bucket_data(buckets)
  dm$melt_df()

  pp$plot_df(dm)
}
