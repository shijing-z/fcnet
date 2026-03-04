# ==============================================================================
# Basic Functionality Tests
# ==============================================================================

test_that("plot_compare exists and is callable", {
  expect_true(is.function(plot_compare))
})

test_that("plot_compare has required parameters", {
  args <- formals(plot_compare)

  expect_true("data" %in% names(args))
  expect_true("conn_vars" %in% names(args))
  expect_true("group" %in% names(args))
  expect_true("error_bar" %in% names(args))
  expect_true("show_zero" %in% names(args))
  expect_true("clean_labels" %in% names(args))
  expect_true("title" %in% names(args))
})

test_that("plot_compare has correct defaults", {
  args <- formals(plot_compare)

  expect_true(args$show_zero)
  expect_true(args$clean_labels)
  expect_null(args$title)
})

# ==============================================================================
# Test Data Setup
# ==============================================================================

# Create reusable test data frame
make_test_df <- function() {
  indices <- get_indices(ex_conn_array, roi_include = "schaefer")
  df <- calc_within(ex_conn_array, indices)
  df$group <- rep(c("YA", "OA"), times = c(5, 5))
  df
}

# ==============================================================================
# Input Validation Tests
# ==============================================================================

test_that("plot_compare errors with non-data-frame input", {
  expect_error(
    plot_compare(matrix(1:9, 3, 3), "V1", "group"),
    "data must be a data frame"
  )

  expect_error(
    plot_compare(list(a = 1), "a", "group"),
    "data must be a data frame"
  )
})

test_that("plot_compare errors with invalid conn_vars", {
  df <- make_test_df()

  # Non-character
  expect_error(
    plot_compare(df, 1:3, "group"),
    "character vector"
  )

  # Empty vector
  expect_error(
    plot_compare(df, character(0), "group"),
    "at least one column name"
  )
})

test_that("plot_compare errors with missing conn_vars columns", {
  df <- make_test_df()

  expect_error(
    plot_compare(df, c("within_default", "nonexistent"), "group"),
    "not found in data"
  )
})

test_that("plot_compare errors with non-numeric conn_vars", {
  df <- make_test_df()
  df$char_col <- rep("a", nrow(df))

  expect_error(
    plot_compare(df, "char_col", "group"),
    "Non-numeric"
  )
})

test_that("plot_compare errors with invalid group parameter", {
  df <- make_test_df()

  # Non-character
  expect_error(
    plot_compare(df, "within_default", 1),
    "single character string"
  )

  # Vector of length > 1
  expect_error(
    plot_compare(df, "within_default", c("group", "other")),
    "single character string"
  )
})

test_that("plot_compare errors with missing group column", {
  df <- make_test_df()

  expect_error(
    plot_compare(df, "within_default", "nonexistent"),
    "not found in data"
  )
})

test_that("plot_compare errors with single-level group", {
  df <- make_test_df()
  df$group <- "A"

  expect_error(
    plot_compare(df, "within_default", "group"),
    "at least 2 levels"
  )
})

test_that("plot_compare validates error_bar parameter", {
  df <- make_test_df()

  expect_error(
    plot_compare(df, "within_default", "group", error_bar = "invalid"),
    "should be one of"
  )
})

# ==============================================================================
# Return Value Tests
# ==============================================================================

test_that("plot_compare returns a ggplot object", {
  df <- make_test_df()

  p <- plot_compare(df, "within_default", "group")
  expect_s3_class(p, "ggplot")
})

test_that("plot_compare returns ggplot with multiple conn_vars", {
  df <- make_test_df()

  p <- plot_compare(
    df,
    c("within_default", "within_vis", "within_cont"),
    "group"
  )
  expect_s3_class(p, "ggplot")
})

test_that("plot_compare returns ggplot for each error_bar option", {
  df <- make_test_df()

  expect_s3_class(
    plot_compare(df, "within_default", "group", error_bar = "se"),
    "ggplot"
  )
  expect_s3_class(
    plot_compare(df, "within_default", "group", error_bar = "sd"),
    "ggplot"
  )
  expect_s3_class(
    plot_compare(df, "within_default", "group", error_bar = "ci"),
    "ggplot"
  )
  expect_s3_class(
    plot_compare(df, "within_default", "group", error_bar = "none"),
    "ggplot"
  )
})

test_that("plot_compare y-axis is labeled Connectivity", {
  df <- make_test_df()

  p <- plot_compare(df, "within_default", "group")
  expect_equal(p$labels$y, "Connectivity")
})

test_that("plot_compare fill legend is labeled Group", {
  df <- make_test_df()

  p <- plot_compare(df, "within_default", "group")
  expect_equal(p$labels$fill, "Group")
})

# ==============================================================================
# Title Tests
# ==============================================================================

test_that("plot_compare adds title when provided", {
  df <- make_test_df()

  p <- plot_compare(df, "within_default", "group", title = "Test Title")
  expect_equal(p$labels$title, "Test Title")
})

test_that("plot_compare has no title when title is NULL", {
  df <- make_test_df()

  p <- plot_compare(df, "within_default", "group")
  expect_null(p$labels$title)
})

# ==============================================================================
# Show Zero Tests
# ==============================================================================

test_that("plot_compare with show_zero = FALSE has fewer layers", {
  df <- make_test_df()

  p_zero <- plot_compare(df, "within_default", "group", show_zero = TRUE)
  p_no_zero <- plot_compare(df, "within_default", "group", show_zero = FALSE)

  expect_true(length(p_zero$layers) > length(p_no_zero$layers))
})

# ==============================================================================
# Error Bar Tests
# ==============================================================================

test_that("plot_compare with error_bar = none has fewer layers", {
  df <- make_test_df()

  p_se <- plot_compare(df, "within_default", "group", error_bar = "se")
  p_none <- plot_compare(df, "within_default", "group", error_bar = "none")

  expect_true(length(p_se$layers) > length(p_none$layers))
})

# ==============================================================================
# Clean Labels Tests
# ==============================================================================

test_that("plot_compare cleans labels when clean_labels = TRUE", {
  df <- make_test_df()

  p <- plot_compare(
    df,
    c("within_default", "within_vis"),
    "group",
    clean_labels = TRUE
  )

  x_scale <- p$scales$get_scales("x")
  expect_equal(x_scale$labels, c("Within-Default", "Within-Vis"))
})

test_that("plot_compare preserves raw labels when clean_labels = FALSE", {
  df <- make_test_df()

  p <- plot_compare(
    df,
    c("within_default", "within_vis"),
    "group",
    clean_labels = FALSE
  )

  x_scale <- p$scales$get_scales("x")
  expect_equal(x_scale$labels, c("within_default", "within_vis"))
})

test_that("plot_compare title-cases each segment after hyphens", {
  # Simulate a pairwise column name with multiple underscores
  df <- data.frame(
    default_cont = rnorm(10),
    group = rep(c("A", "B"), each = 5)
  )

  p <- plot_compare(df, "default_cont", "group", clean_labels = TRUE)
  x_scale <- p$scales$get_scales("x")
  expect_equal(x_scale$labels, "Default-Cont")
})

# ==============================================================================
# Group Handling Tests
# ==============================================================================

test_that("plot_compare works with character group column", {
  df <- make_test_df()
  df$group <- as.character(df$group)

  p <- plot_compare(df, "within_default", "group")
  expect_s3_class(p, "ggplot")
})

test_that("plot_compare works with factor group column", {
  df <- make_test_df()
  df$group <- factor(df$group, levels = c("YA", "OA"))

  p <- plot_compare(df, "within_default", "group")
  expect_s3_class(p, "ggplot")
})

test_that("plot_compare works with more than 2 groups", {
  df <- make_test_df()
  df$group <- rep(c("A", "B", "C"), length.out = nrow(df))

  p <- plot_compare(df, "within_default", "group")
  expect_s3_class(p, "ggplot")
})

# ==============================================================================
# Edge Case Tests
# ==============================================================================

test_that("plot_compare works with single conn_var", {
  df <- make_test_df()

  p <- plot_compare(df, "within_default", "group")
  expect_s3_class(p, "ggplot")
})

test_that("plot_compare works with non-calc output data frame", {
  df <- data.frame(
    metric_a = rnorm(20),
    metric_b = rnorm(20),
    condition = rep(c("control", "treatment"), each = 10)
  )

  p <- plot_compare(df, c("metric_a", "metric_b"), "condition")
  expect_s3_class(p, "ggplot")
})

test_that("plot_compare works with calc_conn output", {
  indices <- get_indices(ex_conn_array)
  ahip_df <- calc_conn(
    ex_conn_array, indices,
    from = "ahip", to = c("default", "cont", "vis")
  )
  ahip_df$group <- rep(c("YA", "OA"), times = c(5, 5))

  p <- plot_compare(
    ahip_df,
    c("ahip_default", "ahip_cont", "ahip_vis"),
    "group"
  )
  expect_s3_class(p, "ggplot")
})

test_that("plot_compare works with calc_between pairwise output", {
  indices <- get_indices(ex_conn_array, roi_include = "schaefer")
  pairwise_df <- calc_between(ex_conn_array, indices, pairwise = TRUE)
  pairwise_df$group <- rep(c("YA", "OA"), times = c(5, 5))

  pair_cols <- names(pairwise_df)[1:3]
  p <- plot_compare(pairwise_df, pair_cols, "group")
  expect_s3_class(p, "ggplot")
})

test_that("plot_compare preserves conn_vars order on x-axis", {
  df <- make_test_df()
  vars_reversed <- c("within_vis", "within_default")

  p <- plot_compare(df, vars_reversed, "group")
  x_scale <- p$scales$get_scales("x")

  expect_equal(x_scale$labels, c("Within-Vis", "Within-Default"))
})
