#' Create plotly objects from a melted dataframe.
#' @import plotly
PlotlyPlotter <- R6Class("PlotlyPlotter",
  public = list(
    plot_fn = NA,
    xaxis = NA,
    yaxis = NA,
    yaxis2 = NA,

    initialize = function(scale, xtitle, ytitle) {
      if (scale == "uniform" | scale == "factor") {
        self$plot_fn <- self$plot_fn_bar
      }
      else if (scale == "cartesian") {
        self$plot_fn <- self$plot_fn_hist
      }
      else {
        stop("scale not recognised!", call. = FALSE)
      }

      self$xaxis = list(title = xtitle)
      self$yaxis = list(title = ytitle)

      self$yaxis2 = list(
        title = "Exposure",
        overlaying = "y",
        side = "right",
        showgrid = FALSE
      )
    },

    plot_fn_hist = function(data_munger) {
      self$plot_lines(data_munger) %>%
        {self$add_histogram_trace(., data_munger)} %>%
        {self$set_layout(.)}
    },

    plot_fn_bar = function(data_munger) {
      self$plot_lines(data_munger) %>%
        {self$add_bar_trace(., data_munger)} %>%
        {self$set_layout(.)}
    },

    plot_lines = function(data_munger) {
      lines_df <- data_munger$melted_df[variable != "weight" &
                                        variable != data_munger$by_col]
      plot_ly(lines_df,
              x = grp_by_col,
              y = value,
              group = variable,
              mode = "markers+lines")
    },

    add_histogram_trace = function(plot, data_munger) {
      add_trace(plot,
                x = data_munger$df[[data_munger$by_col]],
                type = "histogram",
                name = "weight",
                opacity = 0.3,
                orientation = "v",
                yaxis = "y2")
    },

    add_bar_trace = function(plot, data_munger) {
      bars_df  <- data_munger$melted_df[variable == "weight"]
      add_trace(plot,
                data = bars_df,
                type = "bar",
                x = grp_by_col,
                y = value,
                group = variable,
                name = "weight",
                opacity = 0.3,
                orientation = "v",
                yaxis = "y2")
    },

    set_layout = function(p) {
      layout(p,
             xaxis = self$xaxis,
             yaxis = self$yaxis,
             yaxis2 = self$yaxis2)
    }
  )
)
