% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/data_munger.R
\docType{data}
\name{DataMunger}
\alias{DataMunger}
\title{Handle everything to do with producing a melted dataframe to feed into plot
methods later.}
\format{\preformatted{Class 'R6ClassGenerator' <DataMunger> object generator
  Public:
    plot_cols: NA
    by_col: NA
    df: NA
    bucketer: NA
    melter: NA
    bucketed_df: NA
    melted_df: NA
    summarise_fn: NA
    initialize: function (input_df, plot_cols, by_col, cut_type, scale) 
    prepare_data: function (buckets) 
    generate_core_df: function (input_df) 
    summarise_data_by_buckets: function () 
    summarise_with_weight: function (sd) 
    mean_unless_char: function (var) 
    clone: function (deep = FALSE) 
  Parent env: <environment: namespace:universe>
  Locked objects: TRUE
  Locked class: FALSE
  Portable: TRUE 
 - attr(*, "name")= chr "DataMunger_generator"
}}
\usage{
DataMunger
}
\description{
Handle everything to do with producing a melted dataframe to feed into plot
methods later.
}
\keyword{datasets}

