VectorBucketer <- R6Class("VectorBucketer",
  # Handle bucketing of the by variable.
  public = list(
    cut_type = NA,

    initialize = function(cut_type) {
      self$cut_type <- cut_type
    },

    cut_vector = function(cut_vector, buckets) {
      if (is.factor(cut_vector) | is.character(cut_vector)) {
        return(self$cut_factor_by_exposure(cut_vector, buckets))
      }
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
      cut_vector[!cut_vector %in% names(tab)] <- "OTHER"
      as.factor(cut_vector)
    }
  )
)
