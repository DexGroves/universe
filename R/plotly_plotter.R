PlotlyPlotter <- R6Class("PlotlyPlotter",
  # Create plotly objects from a melted dataframe.
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

    plot_df = function(dm) {
      if (self$scale == "uniform" | self$scale == "factor") {
        return(self$plot_df_bar(dm))
      }
      if (self$scale == "cartesian") {
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
        {self$set_layout(.)}
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
