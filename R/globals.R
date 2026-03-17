# Global variables used in ggplot2 aes() calls
# Declared here to avoid R CMD check NOTE: "no visible binding for global variable"

utils::globalVariables(c(
  # plot_compare
  "variable",
  "mean_conn",
  "group_var",
  "error",
  # plot_heatmap
  "col",
  "row",
  "value",
  "x",
  "xend",
  "y",
  "yend"
))
