# universe
Plotly-powered univariates. Runs on `R6` and `data.table`.
Very much a work in progress.

Bucket data and draw univariate plots in plotly. Univariate plots show the
variation of one or more variable versus one covariate. Also draws a neat
exposure histogram.

## Things that don't work (non exhaustive)
1. Factors
2. Weights

## How to plot
```R
diamonds %>%
  universe(plot_cols = c("x", "y", "z"),
           by_col = "price",
           buckets = 20,
           cut_type = "quantile",
           scale = "cartesian")
```
