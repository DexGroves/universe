#' Handle everything to do with producing a melted dataframe to feed into plot
#' methods later.
DataMunger <- R6Class("DataMunger",
  public = list(
    plot_cols = NA,
    by_col = NA,
    df = NA,
    bucketer = NA,
    melter = NA,
    bucketed_df = NA,
    melted_df = NA,
    summarise_fn = NA,

    initialize = function(input_df, plot_cols, by_col, cut_type, scale) {
      self$plot_cols <- plot_cols
      self$by_col <- by_col

      self$bucketer <- VectorBucketer$new(cut_type)
      self$melter <- DataMelter$new(scale)
      self$df <- self$generate_core_df(input_df)

      self$summarise_fn <- self$mean_unless_char
    },

    prepare_data = function(buckets) {
      self$df[, grp_by_col :=
        self$bucketer$cut_vector(get(self$by_col), buckets)]

      self$bucketed_df <- self$summarise_data_by_buckets()

      self$melted_df <- self$melter$melt_data(
        self$bucketed_df, self$by_col)
    },

    generate_core_df = function(input_df) {
      if (!is.data.table(input_df)) {
        return(data.table(input_df[, c(self$by_col, self$plot_cols)]))
      }

      input_df[, c(self$by_col, self$plot_cols), with = FALSE]
    },

    summarise_data_by_buckets = function() {
      self$df[, self$summarise_with_weight(.SD),
              .SDcols = c(self$plot_cols, self$by_col),
              by = grp_by_col]
    },

    summarise_with_weight = function(sd) {
      c(weight = as.double(nrow(sd)), lapply(sd, self$summarise_fn))
    },

    mean_unless_char =  function(var) {
      if (is.character(var) | is.factor(var)) {
        return(var[1])
      }
      mean(var)
    }
  )
)
