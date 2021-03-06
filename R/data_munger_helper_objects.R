#' Handle bucketing of the by variable.
VectorBucketer <- R6Class("VectorBucketer",
  public = list(
    cut_fn = NA,

    initialize = function(cut_type) {
      if (cut_type == "even") {
        self$cut_fn <- self$cut_evenly
      }
      else if (cut_type == "quantile") {
        self$cut_fn <- self$cut_by_quantile
      }
      else {
        stop("cut_type not recognised!", call. = FALSE)
      }
    },

    cut_vector = function(cut_vector, buckets) {
      if (is.factor(cut_vector) | is.character(cut_vector)) {
        return(self$cut_factor_by_exposure(cut_vector, buckets))
      }
      self$cut_fn(cut_vector, buckets)
    },

    cut_evenly = function(cut_vector, buckets) {
      lower <- min(cut_vector)
      upper <- max(cut_vector)
      cut_points <- seq(lower, upper, length.out = (buckets+1))
      cut(cut_vector, breaks = unique(cut_points), include.lowest = TRUE)
    },

    cut_by_quantile = function(cut_vector, buckets) {
      quantiles <- seq(0, 1, length.out = (buckets+1))
      cut_points <- quantile(cut_vector, probs = quantiles)
      cut(cut_vector, breaks = unique(cut_points), include.lowest = TRUE)
    },

    cut_factor_by_exposure = function(cut_vector, buckets) {
      tab <- table(cut_vector)
      tab <- tab[order(tab, decreasing=TRUE)][1:(buckets - 1)]
      cut_vector <- as.character(cut_vector)
      cut_vector[!cut_vector %in% names(tab) & !is.na(cut_vector)] <- "OTHER"
      cut_vector[is.na(cut_vector)] <- "NA"
      cut_vector <- factor(cut_vector, levels=c(names(tab), "OTHER", "NA"))
      cut_vector[drop=TRUE]
    }
  )
)

#' Handle melting of data according to the taxonomy of the input.
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
