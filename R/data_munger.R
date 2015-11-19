DataMunger <- R6Class("DataMunger",
  # Handle everything to do with producing a melted dataframe to feed into plot
  # methods later.
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

    melt_df = function(scale) {
      if (scale == "cartesian") {
        self$melt_df_cartesian()
      } else if (scale == "uniform") {
        self$melt_df_uniform()
      }
    },

    melt_df_cartesian = function() {
      self$melted_df <- melt(self$bucketed_df[, !"grp_by_col", with = FALSE],
                             id.vars = self$by_col)
      setkeyv(self$melted_df, self$by_col)
    },

    melt_df_uniform = function() {
      self$melted_df <- melt(self$bucketed_df,
                             id.vars = "grp_by_col")
      setkey(self$melted_df, grp_by_col)
    }
  )
)
