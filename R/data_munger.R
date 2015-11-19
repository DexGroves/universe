DataMunger <- R6Class("DataMunger",
  public = list(
    plot_cols = NA,
    by_col = NA,
    df = NA,
    vb = NA,
    bucketed_df = NA,
    melted_df = NA,

    initialize = function(input_df, plot_cols, by_col, cut_type) {
      self$plot_cols <- plot_cols
      self$by_col <- by_col
      self$vb <- VectorBucketer$new(cut_type)
      self$df <- self$generate_core_df(input_df)
    },

    generate_core_df = function(input_df) {
      data.table(input_df[, c(self$by_col, self$plot_cols)])
    },


    bucket_data = function(buckets) {
      self$df[, grp_by_col := self$vb$cut_vector(get(self$by_col), buckets)]
      self$bucketed_df <- self$df[, self$reduce_df(.SD),
                                  .SDcols = c(self$plot_cols, self$by_col),
                                  by = grp_by_col]
    },

    reduce_df = function(sd) {
      c(weight = as.double(nrow(sd)),
        lapply(sd, mean))
    },

    melt_df = function() {
      self$melted_df <- melt(self$bucketed_df,
                             id.vars = "grp_by_col")
      setkey(self$melted_df, grp_by_col)
    }
  )
)

VectorBucketer <- R6Class("VectorBucketer",
  public = list(
    cut_type = NA,

    initialize = function(cut_type) {
      self$cut_type <- cut_type
    },

    cut_vector = function(cut_vector, buckets) {
      if (self$cut_type == "even") {
        return(self$cut_evenly(cut_vector, buckets))
      }
      if (self$cut_type == "quantile") {
        return(self$cut_by_quantile(cut_vector, buckets))
      }
      else {
        stop("cut_type not recognised!", call. = FALSE)
      }
    },

    cut_evenly = function(cut_vector, buckets) {
      lower <- min(cut_vector)
      upper <- max(cut_vector)
      cut_points <- seq(lower, upper, length.out = buckets)
      cut(cut_vector, breaks = cut_points, include.lowest = TRUE)
    },

    cut_by_quantile = function(cut_vector, buckets) {
      quantiles <- seq(0, 1, length.out = buckets)
      cut_points <- quantile(cut_vector, probs = quantiles)
      cut(cut_vector, breaks = cut_points, include.lowest = TRUE)
    }
  )
)
