#' Simple univariate plots.
#'
#' Bucket data and draw univariate plots in plotly. Univariate plots show the
#' variation of one or more variable versus one covariate. universe will also
#' draw a neat exposure histogram.
#'
#' Operates in two parts; bucketing the data into discrete bins (either evenly
#' in by_col or evenly by quantile in by_col), then drawing the variation of
#' the plot_cols vs by_col with a histogram of by_col's exposure.
#'
#' @import R6
#' @import data.table
#' @export
#'
#' @param input_df input data.frame or data.table
#' @param plot_cols string vector naming one or more columns in input_df to
#' plot against by_col
#' @param by_col string naming the x-variable against which the plot_cols are
#' measured
#' @param buckets number of buckets into which to attempt to reduce the data.
#' Defaults to 10.
#' @param cut_type either "even" or "quantile". Defaults to "even".
#' \itemize{
#'   \item{"even"}{ Cut data into evenly spaced buckets from the min to the max
#'     of by_col}
#'   \item{"quantile"}{ Cut data by quantile, in which each bucket will have
#'     approximately the same weight.}
#' }
#' @param scale either "uniform" or "cartesian". Defaults to "uniform".
#' \itemize{
#'   \item{"uniform"}{ Distribute data buckets evenly. Represent weight with a
#'     bar chart.}
#'   \item{"cartesian"}{ Distribute data buckets according to the mean of their
#'     by_var. Represent weight with a histogram.}
#' }
#' @return \pkg{plotly} object.
universe <- function(input_df,
                     plot_cols,
                     by_col,
                     buckets = 10,
                     cut_type = "even",
                     scale = "uniform",
                     xtitle = by_col,
                     ytitle = "",
                     backend = "plotly") {

  if (is_char_or_factor(input_df[[by_col]])) {
    scale <- "factor"
  }

  plotting_backend <- get_plotting_backend(backend)
  plotter <- plotting_backend$new(scale, xtitle, ytitle)

  munger <- DataMunger$new(input_df, plot_cols, by_col, cut_type, scale)
  munger$prepare_data(buckets)

  plotter$plot_fn(munger)
}

#' Simple univariate plots, without the plot. Just the data.table.
#'
#' Performs all the preprocessing of `universe`, but stops short of calling
#' plotly and returns its data structure. Use with your own plotly methods for
#' greater control.
#' @inheritParams universe
universe_df <- function(input_df,
                        plot_cols,
                        by_col,
                        buckets = 10,
                        cut_type = "even",
                        scale = "uniform") {
  if (is.factor(input_df[[by_col]]) | is.character(input_df[[by_col]])) {
    scale <- "factor"
  }

  munger <- DataMunger$new(input_df, plot_cols, by_col, cut_type, scale)
  munger$prepare_data(buckets)

  munger$melted_df
}

get_plotting_backend <- function(backend) {
  if (backend == "plotly") {
    return(PlotlyPlotter)
  }
  else if (backend == "ggplot2") {
    return(GgplotPlotter)
  }
  else {
    stop("Backend not recognised!", call. = FALSE)
  }
}

is_char_or_factor <- function(var) {
  is.factor(var) | is.character(var)
}
