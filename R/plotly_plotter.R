#' Create plotly objects from a melted dataframe.
PlotlyPlotter <- R6Class("PlotlyPlotter",
  public = list(
    scale = NA,
    xaxis = NA,
    yaxis = NA,
    yaxis2 = NA,

    initialize = function(scale, xtitle, ytitle) {
      self$scale <- scale


      self$xaxis = list(title = xtitle)
      self$yaxis = list(title = ytitle)

      self$yaxis2 = list(
        title = "Exposure",
        overlaying = "y",
        side = "right",
        showgrid = FALSE
     )
    },

    plot_df = function(data_munger) {
      if (self$scale == "uniform" | self$scale == "factor") {
        return(self$plot_df_bar(data_munger))
      }
      if (self$scale == "cartesian") {
        return(self$plot_df_hist(data_munger))
      } else {
        stop("scale not recognised!", call. = FALSE)
      }
      invisible()
    },

    plot_df_hist = function(data_munger) {
      lines_df <- data_munger$melted_df[variable != "weight"]

      plot_ly(lines_df,
              x = get(data_munger$by_col),
              y = value,
              group = variable) %>%
        add_trace(x = data_munger$df[[data_munger$by_col]],
                  type = "histogram",
                  opacity = 0.3,
                  orientation = "v",
                   yaxis = "y2") %>%
        {self$set_layout(.)}
    },

    plot_df_bar = function(data_munger) {
      lines_df <- data_munger$melted_df[variable != "weight" &
                                        variable != data_munger$by_col]
      bars_df  <- data_munger$melted_df[variable == "weight"]

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
        {self$set_layout(.)}
    },

    set_layout = function(p) {
      layout(p,
             xaxis = self$xaxis,
             yaxis = self$yaxis,
             yaxis2 = self$yaxis2)
    }
  )
)
