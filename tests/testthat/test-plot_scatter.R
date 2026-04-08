# ==============================================================================
# Basic Functionality Tests
# ==============================================================================

test_that("plot_scatter exists and is callable", {
  expect_true(is.function(plot_scatter))
})

test_that("plot_scatter has required parameters", {
  args <- formals(plot_scatter)

  expect_true("data" %in% names(args))
  expect_true("x" %in% names(args))
  expect_true("y" %in% names(args))
  expect_true("group" %in% names(args))
  expect_true("colors" %in% names(args))
  expect_true("show_points" %in% names(args))
  expect_true("show_line" %in% names(args))
  expect_true("point_args" %in% names(args))
  expect_true("line_args" %in% names(args))
  expect_true("band_args" %in% names(args))
  expect_true("clean_labels" %in% names(args))
  expect_true("title" %in% names(args))
})

test_that("plot_scatter has correct defaults", {
  args <- formals(plot_scatter)

  expect_null(args$group)
  expect_null(args$colors)
  expect_true(args$show_points)
  expect_true(args$show_line)
  expect_true(args$clean_labels)
  expect_null(args$title)
})

# ==============================================================================
# Test Data Setup
# ==============================================================================

# Create reusable test data frame
make_scatter_df <- function() {
  set.seed(42)
  indices <- get_indices(ex_conn_array)
  df <- calc_conn(ex_conn_array, indices, from = "ahip", to = "default")
  df$behavior <- rnorm(10)
  df$group <- rep(c("YA", "OA"), times = c(5, 5))
  df
}

# ==============================================================================
# Input Validation Tests
# ==============================================================================

test_that("plot_scatter errors with non-data-frame input", {
  expect_error(
    plot_scatter(matrix(1:9, 3, 3), "x", "y"),
    "data must be a data frame"
  )

  expect_error(
    plot_scatter(list(a = 1), "a", "b"),
    "data must be a data frame"
  )
})

test_that("plot_scatter errors with invalid x parameter", {
  df <- make_scatter_df()

  # Non-character
  expect_error(
    plot_scatter(df, 1, "behavior"),
    "single character string"
  )

  # Vector of length > 1
  expect_error(
    plot_scatter(df, c("ahip_default", "other"), "behavior"),
    "single character string"
  )
})

test_that("plot_scatter errors with invalid y parameter", {
  df <- make_scatter_df()

  # Non-character
  expect_error(
    plot_scatter(df, "ahip_default", 1),
    "single character string"
  )

  # Vector of length > 1
  expect_error(
    plot_scatter(df, "ahip_default", c("behavior", "other")),
    "single character string"
  )
})

test_that("plot_scatter errors with missing x column", {
  df <- make_scatter_df()

  expect_error(
    plot_scatter(df, "nonexistent", "behavior"),
    "not found in data"
  )
})

test_that("plot_scatter errors with missing y column", {
  df <- make_scatter_df()

  expect_error(
    plot_scatter(df, "ahip_default", "nonexistent"),
    "not found in data"
  )
})

test_that("plot_scatter errors with non-numeric x column", {
  df <- make_scatter_df()
  df$char_col <- rep("a", nrow(df))

  expect_error(
    plot_scatter(df, "char_col", "behavior"),
    "must be numeric"
  )
})

test_that("plot_scatter errors with non-numeric y column", {
  df <- make_scatter_df()
  df$char_col <- rep("a", nrow(df))

  expect_error(
    plot_scatter(df, "ahip_default", "char_col"),
    "must be numeric"
  )
})

test_that("plot_scatter errors with invalid group parameter", {
  df <- make_scatter_df()

  # Non-character
  expect_error(
    plot_scatter(df, "ahip_default", "behavior", group = 1),
    "single character string"
  )

  # Vector of length > 1
  expect_error(
    plot_scatter(df, "ahip_default", "behavior", group = c("group", "other")),
    "single character string"
  )
})

test_that("plot_scatter errors with missing group column", {
  df <- make_scatter_df()

  expect_error(
    plot_scatter(df, "ahip_default", "behavior", group = "nonexistent"),
    "not found in data"
  )
})

test_that("plot_scatter errors with single-level group", {
  df <- make_scatter_df()
  df$group <- "A"

  expect_error(
    plot_scatter(df, "ahip_default", "behavior", group = "group"),
    "at least 2 levels"
  )
})

test_that("plot_scatter errors with invalid colors (non-character)", {
  df <- make_scatter_df()

  expect_error(
    plot_scatter(df, "ahip_default", "behavior", colors = 123),
    "must be a character"
  )
})

test_that("plot_scatter errors with unnamed colors when group is provided", {
  df <- make_scatter_df()

  expect_error(
    plot_scatter(df, "ahip_default", "behavior",
      group = "group", colors = c("red", "blue")
    ),
    "named vector"
  )
})

test_that("plot_scatter errors with multi-color vector when group is NULL", {
  df <- make_scatter_df()

  expect_error(
    plot_scatter(df, "ahip_default", "behavior",
      colors = c("red", "blue")
    ),
    "single color string"
  )
})

test_that("plot_scatter errors when both show_points and show_line are FALSE", {
  df <- make_scatter_df()

  expect_error(
    plot_scatter(df, "ahip_default", "behavior",
      show_points = FALSE, show_line = FALSE
    ),
    "At least one"
  )
})

test_that("plot_scatter errors with non-list args parameters", {
  df <- make_scatter_df()

  expect_error(
    plot_scatter(df, "ahip_default", "behavior", point_args = "invalid"),
    "point_args must be a list"
  )

  expect_error(
    plot_scatter(df, "ahip_default", "behavior", line_args = "invalid"),
    "line_args must be a list"
  )

  expect_error(
    plot_scatter(df, "ahip_default", "behavior", band_args = "invalid"),
    "band_args must be a list"
  )
})

# ==============================================================================
# Return Value Tests
# ==============================================================================

test_that("plot_scatter returns a ggplot object (ungrouped)", {
  df <- make_scatter_df()

  p <- plot_scatter(df, "ahip_default", "behavior")
  expect_s3_class(p, "ggplot")
})

test_that("plot_scatter returns a ggplot object (grouped)", {
  df <- make_scatter_df()

  p <- plot_scatter(df, "ahip_default", "behavior", group = "group")
  expect_s3_class(p, "ggplot")
})

test_that("plot_scatter has correct axis labels", {
  df <- make_scatter_df()

  p <- plot_scatter(df, "ahip_default", "behavior")
  expect_equal(p$labels$x, "Ahip-Default")
  expect_equal(p$labels$y, "Behavior")
})

test_that("plot_scatter legend labels are Group", {
  df <- make_scatter_df()

  p <- plot_scatter(df, "ahip_default", "behavior", group = "group")
  expect_equal(p$labels$colour, "Group")
})

# ==============================================================================
# Title Tests
# ==============================================================================

test_that("plot_scatter adds title when provided", {
  df <- make_scatter_df()

  p <- plot_scatter(df, "ahip_default", "behavior", title = "Test Title")
  expect_equal(p$labels$title, "Test Title")
})

test_that("plot_scatter has no title when title is NULL", {
  df <- make_scatter_df()

  p <- plot_scatter(df, "ahip_default", "behavior")
  expect_null(p$labels$title)
})

# ==============================================================================
# Clean Labels Tests
# ==============================================================================

test_that("plot_scatter cleans labels when clean_labels = TRUE", {
  df <- make_scatter_df()

  p <- plot_scatter(df, "ahip_default", "behavior", clean_labels = TRUE)
  expect_equal(p$labels$x, "Ahip-Default")
  expect_equal(p$labels$y, "Behavior")
})

test_that("plot_scatter preserves raw labels when clean_labels = FALSE", {
  df <- make_scatter_df()

  p <- plot_scatter(df, "ahip_default", "behavior", clean_labels = FALSE)
  expect_equal(p$labels$x, "ahip_default")
  expect_equal(p$labels$y, "behavior")
})

test_that("plot_scatter title-cases each segment after hyphens", {
  df <- data.frame(
    default_cont = rnorm(10),
    memory_score = rnorm(10)
  )

  p <- plot_scatter(df, "default_cont", "memory_score", clean_labels = TRUE)
  expect_equal(p$labels$x, "Default-Cont")
  expect_equal(p$labels$y, "Memory-Score")
})

# ==============================================================================
# Group Handling Tests
# ==============================================================================

test_that("plot_scatter works without group (NULL)", {
  df <- make_scatter_df()

  p <- plot_scatter(df, "ahip_default", "behavior")
  expect_s3_class(p, "ggplot")
})

test_that("plot_scatter works with character group column", {
  df <- make_scatter_df()
  df$group <- as.character(df$group)

  p <- plot_scatter(df, "ahip_default", "behavior", group = "group")
  expect_s3_class(p, "ggplot")
})

test_that("plot_scatter works with factor group column", {
  df <- make_scatter_df()
  df$group <- factor(df$group, levels = c("YA", "OA"))

  p <- plot_scatter(df, "ahip_default", "behavior", group = "group")
  expect_s3_class(p, "ggplot")
})

test_that("plot_scatter works with more than 2 groups", {
  df <- make_scatter_df()
  df$group <- rep(c("A", "B", "C"), length.out = nrow(df))

  p <- plot_scatter(df, "ahip_default", "behavior", group = "group")
  expect_s3_class(p, "ggplot")
})

test_that("plot_scatter hides legend when group is NULL", {
  df <- make_scatter_df()

  p <- plot_scatter(df, "ahip_default", "behavior")

  legend_pos <- p$theme$legend.position
  expect_equal(legend_pos, "none")
})

test_that("plot_scatter shows legend when group is provided", {
  df <- make_scatter_df()

  p <- plot_scatter(df, "ahip_default", "behavior", group = "group")

  legend_pos <- p$theme$legend.position
  expect_equal(legend_pos, "bottom")
})

# ==============================================================================
# Show/Hide Layer Tests
# ==============================================================================

test_that("plot_scatter respects show_points = FALSE", {
  df <- make_scatter_df()

  p <- plot_scatter(df, "ahip_default", "behavior", show_points = FALSE)
  layer_types <- vapply(p$layers, function(l) class(l$geom)[1], character(1))

  expect_false("GeomPoint" %in% layer_types)
  expect_true("GeomSmooth" %in% layer_types)
})

test_that("plot_scatter respects show_line = FALSE", {
  df <- make_scatter_df()

  p <- plot_scatter(df, "ahip_default", "behavior", show_line = FALSE)
  layer_types <- vapply(p$layers, function(l) class(l$geom)[1], character(1))

  expect_true("GeomPoint" %in% layer_types)
  expect_false("GeomSmooth" %in% layer_types)
})

test_that("plot_scatter includes both layers by default", {
  df <- make_scatter_df()

  p <- plot_scatter(df, "ahip_default", "behavior")
  layer_types <- vapply(p$layers, function(l) class(l$geom)[1], character(1))

  expect_true("GeomPoint" %in% layer_types)
  expect_true("GeomSmooth" %in% layer_types)
})

# ==============================================================================
# Colors Tests
# ==============================================================================

test_that("plot_scatter applies named colors with group", {
  df <- make_scatter_df()

  p <- plot_scatter(df, "ahip_default", "behavior",
    group = "group",
    colors = c("YA" = "steelblue", "OA" = "coral")
  )
  expect_s3_class(p, "ggplot")

  # Check that scale_color_manual was applied
  color_scale <- p$scales$get_scales("colour")
  expect_false(is.null(color_scale))
})

test_that("plot_scatter applies single color without group", {
  df <- make_scatter_df()

  p <- plot_scatter(df, "ahip_default", "behavior",
    colors = "steelblue"
  )
  expect_s3_class(p, "ggplot")
})

test_that("plot_scatter ignores colors = NULL silently", {
  df <- make_scatter_df()

  p <- plot_scatter(df, "ahip_default", "behavior",
    group = "group", colors = NULL
  )
  expect_s3_class(p, "ggplot")
})

# ==============================================================================
# Point Args Tests
# ==============================================================================

test_that("plot_scatter passes point_args to geom_point", {
  df <- make_scatter_df()

  p <- plot_scatter(df, "ahip_default", "behavior",
    point_args = list(size = 5, shape = 17)
  )
  expect_s3_class(p, "ggplot")

  # Check that geom_point has the custom params
  point_layer <- p$layers[[which(
    vapply(p$layers, function(l) class(l$geom)[1], character(1)) == "GeomPoint"
  )]]
  expect_equal(point_layer$aes_params$size, 5)
  expect_equal(point_layer$aes_params$shape, 17)
})

test_that("plot_scatter point_args overrides default alpha", {
  df <- make_scatter_df()

  p <- plot_scatter(df, "ahip_default", "behavior",
    point_args = list(alpha = 0.9)
  )

  point_layer <- p$layers[[which(
    vapply(p$layers, function(l) class(l$geom)[1], character(1)) == "GeomPoint"
  )]]
  expect_equal(point_layer$aes_params$alpha, 0.9)
})

# ==============================================================================
# Line Args Tests
# ==============================================================================

test_that("plot_scatter passes line_args to geom_smooth", {
  df <- make_scatter_df()

  p <- plot_scatter(df, "ahip_default", "behavior",
    line_args = list(linetype = "dashed", linewidth = 2)
  )
  expect_s3_class(p, "ggplot")

  smooth_layer <- p$layers[[which(
    vapply(p$layers, function(l) class(l$geom)[1], character(1)) == "GeomSmooth"
  )]]
  expect_equal(smooth_layer$aes_params$linetype, "dashed")
  expect_equal(smooth_layer$aes_params$linewidth, 2)
})

# ==============================================================================
# Band Args Tests
# ==============================================================================

test_that("plot_scatter disables confidence band with se = FALSE", {
  df <- make_scatter_df()

  p <- plot_scatter(df, "ahip_default", "behavior",
    band_args = list(se = FALSE)
  )
  expect_s3_class(p, "ggplot")

  smooth_layer <- p$layers[[which(
    vapply(p$layers, function(l) class(l$geom)[1], character(1)) == "GeomSmooth"
  )]]
  expect_false(smooth_layer$stat_params$se)
})

test_that("plot_scatter applies custom confidence level", {
  df <- make_scatter_df()

  p <- plot_scatter(df, "ahip_default", "behavior",
    band_args = list(level = 0.99)
  )

  smooth_layer <- p$layers[[which(
    vapply(p$layers, function(l) class(l$geom)[1], character(1)) == "GeomSmooth"
  )]]
  expect_equal(smooth_layer$stat_params$level, 0.99)
})

# ==============================================================================
# Color Precedence Tests
# ==============================================================================

test_that("point_args color overrides colors parameter (ungrouped)", {
  df <- make_scatter_df()

  p <- plot_scatter(df, "ahip_default", "behavior",
    colors = "steelblue",
    point_args = list(color = "red")
  )
  expect_s3_class(p, "ggplot")

  point_layer <- p$layers[[which(
    vapply(p$layers, function(l) class(l$geom)[1], character(1)) == "GeomPoint"
  )]]
  expect_equal(point_layer$aes_params$colour, "red")
})

test_that("line_args fill overrides colors for band (ungrouped)", {
  df <- make_scatter_df()

  p <- plot_scatter(df, "ahip_default", "behavior",
    colors = "steelblue",
    line_args = list(fill = "grey80")
  )
  expect_s3_class(p, "ggplot")

  smooth_layer <- p$layers[[which(
    vapply(p$layers, function(l) class(l$geom)[1], character(1)) == "GeomSmooth"
  )]]
  expect_equal(smooth_layer$aes_params$fill, "grey80")
})

# ==============================================================================
# Edge Case Tests
# ==============================================================================

test_that("plot_scatter works with non-connectivity data frame", {
  set.seed(42)
  df <- data.frame(
    predictor = rnorm(20),
    outcome = rnorm(20),
    condition = rep(c("A", "B"), each = 10)
  )

  p <- plot_scatter(df, "predictor", "outcome", group = "condition")
  expect_s3_class(p, "ggplot")
})

test_that("plot_scatter works with calc_within output", {
  indices <- get_indices(ex_conn_array, roi_include = "schaefer")
  within_df <- calc_within(ex_conn_array, indices)
  within_df$behavior <- rnorm(10)

  p <- plot_scatter(within_df, "within_default", "behavior")
  expect_s3_class(p, "ggplot")
})

test_that("plot_scatter works with all options combined", {
  df <- make_scatter_df()

  p <- plot_scatter(
    df,
    "ahip_default",
    "behavior",
    group = "group",
    colors = c("YA" = "steelblue", "OA" = "coral"),
    show_points = TRUE,
    show_line = TRUE,
    point_args = list(size = 3, alpha = 0.8),
    line_args = list(linetype = "dashed"),
    band_args = list(alpha = 0.3, level = 0.99),
    clean_labels = TRUE,
    title = "Full Options Test"
  )
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Full Options Test")
})

test_that("plot_scatter works with show_points = FALSE and group", {
  df <- make_scatter_df()

  p <- plot_scatter(df, "ahip_default", "behavior",
    group = "group",
    show_points = FALSE,
    colors = c("YA" = "steelblue", "OA" = "coral")
  )
  expect_s3_class(p, "ggplot")

  layer_types <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_false("GeomPoint" %in% layer_types)
})

test_that("plot_scatter works with per-layer colors (no colors param)", {
  df <- make_scatter_df()

  p <- plot_scatter(df, "ahip_default", "behavior",
    point_args = list(color = "steelblue"),
    line_args = list(color = "coral", fill = "grey80")
  )
  expect_s3_class(p, "ggplot")
})
