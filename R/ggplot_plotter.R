#' ggplot2 plotting backend.
#' Uses some gtable hacks to get exposure bars on a scaled secondary axis.
#' Unfortunately, this approach means the plot method can't return a ggplot
#' object, it just draws the plot to whatever graphics device is available.
#' Can potentially handle extra ggplot arguments another way? With an
#' additional_arguments argument? Not sure there's a clean solution.
#'
#' From: http://rpubs.com/kohske/dual_axis_in_ggplot2
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 ggplot_gtable
#' @importFrom grid grid.newpage
#' @importFrom grid grid.draw
#' @importFrom gtable gtable_add_grob
#' @importFrom gtable gtable_add_cols
GgplotPlotter <- R6Class("GgplotPlotter",
  public = list(
    plot_fn = NA,

    initialize = function(scale, xtitle, ytitle) {
      if (scale == "uniform" | scale == "factor") {
        self$plot_fn <- self$plot_fn_uniform
      }
      else if (scale == "cartesian") {
        self$plot_fn <- self$plot_fn_cartesian
      }
      else {
        stop("scale not recognised!", call. = FALSE)
      }
    },

    plot_fn_cartesian = function(data_munger) {
      stop("ggplot backend not implemented for cartesian", call. = FALSE)
    },

    plot_fn_uniform = function(data_munger) {
      self$ggplot_double_axis(
        self$gg_base_lines(data_munger),
        self$gg_exposure_bars(data_munger)
      )
    },

    gg_base_lines = function(data_munger) {
      lines_df <- data_munger$melted_df[variable != "weight" &
                                        variable != data_munger$by_col]

      ggplot(lines_df, aes(x = grp_by_col, y = value,
                           group = variable, color = variable)) +
        geom_point() +
        geom_line() +
        theme_bw()
    },

    gg_exposure_bars = function(data_munger) {
      bars_df  <- data_munger$melted_df[variable == "weight"]

      ggplot(bars_df, aes(x = grp_by_col, y = value)) +
        geom_bar(stat = "identity", alpha = 0.3) +
        theme_bw() %+replace%
        theme(panel.background = element_rect(fill = NA))
    },

    ggplot_double_axis = function(plot_left, plot_right) {
      grid.newpage()

      # extract gtable
      g1 <- ggplot_gtable(ggplot_build(plot_left))
      g2 <- ggplot_gtable(ggplot_build(plot_right))

      # overlap the panel of 2nd plot on that of 1st plot
      pp <- c(subset(g1$layout, name == "panel", se = t:r))
      g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]],
                           pp$t, pp$l, pp$b, pp$l)

      # axis tweaks
      ia <- which(g2$layout$name == "axis-l")
      ga <- g2$grobs[[ia]]
      ax <- ga$children[[2]]
      ax$widths <- rev(ax$widths)
      ax$grobs <- rev(ax$grobs)
      ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
      g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l],
                           length(g$widths) - 1)
      g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)

      # draw it
      grid.draw(g)
    }
  )
)
