PlotlyPlotter <- R6Class("PlotlyPlotter",
  public = list(
    scale = NA,

    initialize = function(scale) {
      self$scale <- scale
    },

    secondary_axis_options = list(
      tickfont = list(color = "red"),
      overlaying = "y",
      side = "right"
    ),

    plot_df = function(dm) {
      if (self$scale == "uniform") {
        return(self$plot_df_bar(dm))
      } else if (self$scale == "cartesian") {
        return(self$plot_df_hist(dm))
      } else {
        stop("scale not recognised!", call. = FALSE)
      }
      invisible()
    },

    plot_df_hist = function(dm) {
      lines_df <- dm$melted_df[variable != "weight"]

      plot_ly(lines_df,
              x = get(dm$by_col),
              y = value,
              group = variable) %>%
        add_trace(x = dm$df[[dm$by_col]],
                  type = "histogram",
                  opacity = 0.3,
                  orientation = "v",
                  yaxis = "y2") %>%
        layout(yaxis2 = self$secondary_axis_options)
    },

    plot_df_bar = function(dm) {
      lines_df <- dm$melted_df[variable != "weight" & variable != dm$by_col]
      bars_df  <- dm$melted_df[variable == "weight"]

      plot_ly(lines_df,
              x = grp_by_col,
              y = value,
              group = variable) %>%
        add_trace(data = bars_df,
                  type = "bar",
                  x = grp_by_col,
                  y = value,
                  group = variable,
                  opacity = 0.3,
                  orientation = "v",
                  yaxis = "y2") %>%
        layout(yaxis2 = self$secondary_axis_options)
    }
  )
)
