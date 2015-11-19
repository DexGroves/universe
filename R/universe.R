#' @import plotly
#' @import magrittr
plot_df <- function(dm, by_col) {
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
                     buckets = 10) {
  dm <- DataMunger$new(input_df, plot_cols, by_col)
  dm$bucket_data(buckets)
  dm$melt_df()
  plot_df(dm, by_col)
}
