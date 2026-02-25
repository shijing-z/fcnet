# ==============================================================================
# Basic Functionality Tests
# ==============================================================================

test_that("calc_within exists and is callable", {
  expect_true(is.function(calc_within))
})

test_that("calc_within has required parameters", {
  args <- formals(calc_within)

  expect_true("conn_array" %in% names(args))
  expect_true("indices" %in% names(args))
})

# ==============================================================================
# Input Validation Tests
# ==============================================================================

test_that("calc_within errors with non-3D array", {
  indices <- get_indices(ex_conn_array)

  # 2D matrix
  expect_error(
    calc_within(matrix(1:9, 3, 3), indices),
    "3D array"
  )

  # Vector
  expect_error(
    calc_within(1:10, indices),
    "3D array"
  )
})

test_that("calc_within errors with invalid indices", {
  # Unnamed list
  expect_error(
    calc_within(ex_conn_array, list(1:5, 6:10)),
    "named list"
  )

  # Not a list
  expect_error(
    calc_within(ex_conn_array, c(1, 2, 3)),
    "named list"
  )
})

# ==============================================================================
# Safety Net Tests
# ==============================================================================

test_that("calc_within warns and drops entries with fewer than 2 ROIs", {
  indices <- get_indices(ex_conn_array)

  # Full indices include ahip (1 ROI) and phip (1 ROI)
  expect_warning(
    result <- calc_within(ex_conn_array, indices),
    "Dropped entries with fewer than 2 ROIs"
  )

  # ahip and phip should NOT be in output columns
  expect_false("within_ahip" %in% colnames(result))
  expect_false("within_phip" %in% colnames(result))
})

test_that("calc_within warning mentions dropped entry names", {
  indices <- get_indices(ex_conn_array)

  expect_warning(
    calc_within(ex_conn_array, indices),
    "ahip"
  )
})

test_that("calc_within warning suggests calc_conn", {
  indices <- get_indices(ex_conn_array)

  expect_warning(
    calc_within(ex_conn_array, indices),
    "calc_conn"
  )
})

test_that("calc_within runs without warning when all entries have 2+ ROIs", {
  indices <- get_indices(ex_conn_array, roi_include = "schaefer")

  expect_no_warning(
    calc_within(ex_conn_array, indices)
  )
})

test_that("calc_within errors when no entries have 2+ ROIs", {
  single_roi_indices <- list(ahip = 29L, phip = 30L)

  expect_error(
    suppressWarnings(calc_within(ex_conn_array, single_roi_indices)),
    "No entries with 2 or more ROIs"
  )
})

# ==============================================================================
# Return Value Tests
# ==============================================================================

test_that("calc_within returns data frame with correct structure", {
  indices <- get_indices(ex_conn_array, roi_include = "schaefer")
  result <- calc_within(ex_conn_array, indices)

  expect_true(is.data.frame(result))

  # One row per subject
  expect_equal(nrow(result), dim(ex_conn_array)[3])

  # One column per network + within_network average
  network_names <- names(indices)
  expected_cols <- c(paste0("within_", network_names), "within_network")
  expect_equal(colnames(result), expected_cols)
})

test_that("calc_within column names have within_ prefix", {
  indices <- get_indices(ex_conn_array, roi_include = "schaefer")
  result <- calc_within(ex_conn_array, indices)

  # All columns except within_network should have within_ prefix
  network_cols <- setdiff(colnames(result), "within_network")
  expect_true(all(grepl("^within_", network_cols)))
})

test_that("calc_within returns numeric values", {
  indices <- get_indices(ex_conn_array, roi_include = "schaefer")
  result <- calc_within(ex_conn_array, indices)

  expect_true(all(sapply(result, is.numeric)))
})

# ==============================================================================
# Computation Correctness Tests
# ==============================================================================

test_that("calc_within computes upper triangle mean correctly", {
  indices <- get_indices(ex_conn_array, roi_include = "schaefer")
  result <- calc_within(ex_conn_array, indices)

  # Manual calculation for visual network, subject 1
  vis_idx <- indices$vis
  vis_matrix <- ex_conn_array[vis_idx, vis_idx, 1]
  expected_vis <- mean(vis_matrix[upper.tri(vis_matrix)])

  expect_equal(result$within_vis[1], expected_vis)
})

test_that("calc_within computes correctly for multiple networks", {
  indices <- get_indices(ex_conn_array, roi_include = "schaefer")
  result <- calc_within(ex_conn_array, indices)

  # Manual calculation for default network, last subject
  n_subj <- dim(ex_conn_array)[3]
  def_idx <- indices$default
  def_matrix <- ex_conn_array[def_idx, def_idx, n_subj]
  expected_default <- mean(def_matrix[upper.tri(def_matrix)])

  expect_equal(result$within_default[n_subj], expected_default)
})

test_that("calc_within within_network is mean of per-network columns", {
  indices <- get_indices(ex_conn_array, roi_include = "schaefer")
  result <- calc_within(ex_conn_array, indices)

  network_cols <- paste0("within_", names(indices))
  expected_avg <- rowMeans(result[, network_cols])

  expect_equal(result$within_network, expected_avg)
})

# ==============================================================================
# Edge Case Tests
# ==============================================================================

test_that("calc_within works with single subject", {
  single_subj <- ex_conn_array[, , 1, drop = FALSE]
  indices <- get_indices(single_subj, roi_include = "schaefer")

  result <- calc_within(single_subj, indices)

  expect_equal(nrow(result), 1)
  expect_true(is.data.frame(result))
})

test_that("calc_within works with two networks", {
  two_net_indices <- list(
    vis = get_indices(ex_conn_array)$vis,
    default = get_indices(ex_conn_array)$default
  )

  result <- calc_within(ex_conn_array, two_net_indices)

  expect_equal(ncol(result), 3) # within_vis, within_default, within_network
})
