#' @import plotly
#' @import magrittr
#' @import R6
#' @import data.table
#' @import reshape2

plot_df <- function(dm) {
  ay <- list(
    tickfont = list(color = "red"),
    overlaying = "y",
    side = "right"
  )

  plot_ly(dm$melted_df, x = grp_by_col, y = value, group = variable) %>%
    add_trace(x = dm$df$grp_by_col, type = "histogram",
              opacity = 0.3, orientation = "v", yaxis = "y2") %>%
    layout(yaxis2 = ay)
}

#' @export
universe <- function(input_df,
                     plot_cols,
                     by_col,
                     buckets = 10,
                     cut_type = "even") {
  dm <- DataMunger$new(input_df, plot_cols, by_col, cut_type)
  dm$bucket_data(buckets)
  dm$melt_df()
  plot_df(dm)
}
