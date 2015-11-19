# universe
Plotly-powered univariates. Runs on `R6` and `data.table`.
Very much a work in progress.

Bucket data and draw univariate plots in plotly. Univariate plots show the
variation of one or more variable versus one covariate. Also draws a neat
exposure histogram.

## Things that don't work (non exhaustive)
1. Factors
2. Weights
3. Axis labels

## How to plot
```R
# devtools::install_github("DexGroves/universe")

library("ggplot2")
library("universe")

data(diamonds)

diamonds %>%
  universe(plot_cols = c("x", "y", "z"),
           by_col = "price",
           buckets = 20,
           cut_type = "quantile",
           scale = "cartesian")
```
[Produces this chart](https://rawgit.com/dexgroves/universe/html/example/index.html)
