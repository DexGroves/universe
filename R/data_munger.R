#' Handle everything to do with producing a melted dataframe to feed into plot
#' methods later.
DataMunger <- R6Class("DataMunger",
  public = list(
    plot_cols = NA,
    by_col = NA,
    df = NA,
    vb = NA,
    bucketed_df = NA,
    melted_df = NA,
    reduce_fn = NA,

    initialize = function(input_df, plot_cols, by_col, cut_type) {
      self$plot_cols <- plot_cols
      self$by_col <- by_col
      self$vb <- VectorBucketer$new(cut_type)
      self$df <- self$generate_core_df(input_df)

      if (is.character(self$df[[by_col]]) | is.factor(self$df[[by_col]])) {
        self$reduce_fn <- self$mean_unless_char
      } else {
        self$reduce_fn <- mean
      }
    },

    generate_core_df = function(input_df) {
      if (!is.data.table(input_df)) {
        return(data.table(input_df[, c(self$by_col, self$plot_cols)]))
      }
      input_df[, c(self$by_col, self$plot_cols), with = FALSE]
    },


    bucket_data = function(buckets) {
      self$df[, grp_by_col := self$vb$cut_vector(get(self$by_col), buckets)]
      self$bucketed_df <- self$df[, self$reduce_df(.SD),
                                  .SDcols = c(self$plot_cols, self$by_col),
                                  by = grp_by_col]
    },

    reduce_df = function(sd) {
        c(weight = as.double(nrow(sd)), lapply(sd, self$reduce_fn))
    },

    mean_unless_char =  function(var) {
      if (is.character(var) | is.factor(var)) {
        return(var[1])
      }
      mean(var)
    },

    melt_df = function(scale) {
      if (scale == "cartesian")     self$melt_df_cartesian()
      else if (scale == "uniform")  self$melt_df_uniform()
      else if (scale == "factor")   self$melt_df_factor()
      else {
        stop("scale not recognised!", call. = FALSE)
      }
    },

    melt_df_cartesian = function() {
      self$melted_df <- melt(self$bucketed_df[, !"grp_by_col", with = FALSE],
                             id.vars = self$by_col)
      setnames(self$melted_df, self$by_col, "grp_by_col")
      setkey(self$melted_df, grp_by_col)
    },

    melt_df_uniform = function() {
      self$melted_df <- melt(self$bucketed_df, id.vars = "grp_by_col")
      setkey(self$melted_df, grp_by_col)
    },

    melt_df_factor = function() {
      self$melted_df <- melt(self$bucketed_df[, !self$by_col, with = FALSE],
                             id.vars = "grp_by_col")
      setkey(self$melted_df, grp_by_col)
    }
  )
)
