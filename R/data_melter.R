DataMelter <- R6Class("DataMelter",
  public = list(
    melt_data = NA,

    initialize = function(scale) {
      if (scale == "cartesian") {
        self$melt_data <- self$melt_df_cartesian
      }
      else if (scale == "uniform") {
        self$melt_data <- self$melt_df_uniform
      }
      else if (scale == "factor") {
        self$melt_data <- self$melt_df_factor
      }
      else {
        stop("scale not recognised!", call. = FALSE)
      }
    },

    melt_df_cartesian = function(bucketed_df, by_col) {
      melted_df <- melt(bucketed_df[, !("grp_by_col"), with = FALSE],
                        id.vars = by_col)
      setnames(melted_df, by_col, "grp_by_col")
      setkey(melted_df, grp_by_col)
    },

    melt_df_uniform = function(bucketed_df, by_col) {
      melted_df <- melt(bucketed_df, id.vars = "grp_by_col")
      setkey(melted_df, grp_by_col)
    },

    melt_df_factor = function(bucketed_df, by_col) {
      melted_df <- melt(bucketed_df[, !(by_col), with = FALSE],
                        id.vars = "grp_by_col")
      setkey(melted_df, grp_by_col)
    }
  )
)
