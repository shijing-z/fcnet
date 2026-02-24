# ==============================================================================
# example_roi_names Tests
# ==============================================================================

test_that("example_roi_names exists and is accessible", {
  expect_true(exists("example_roi_names"))
})

test_that("example_roi_names has correct type", {
  expect_type(example_roi_names, "character")
})

test_that("example_roi_names has correct length", {
  expect_length(example_roi_names, 30)
})

test_that("example_roi_names contains no empty strings", {
  expect_true(all(nchar(example_roi_names) > 0))
  expect_false(any(is.na(example_roi_names)))
})

test_that("example_roi_names contains Schaefer ROIs", {
  # Should have some Schaefer-labeled ROIs
  schaefer_rois <- grepl("^Schaefer100\\.", example_roi_names)
  expect_true(sum(schaefer_rois) > 0)
})

test_that("example_roi_names contains hippocampal ROIs", {
  expect_true("AHIP" %in% example_roi_names)
  expect_true("PHIP" %in% example_roi_names)
})

test_that("example_roi_names represents all 7 networks", {
  # Check for presence of each network
  expect_true(any(grepl("vis", example_roi_names, ignore.case = TRUE)))
  expect_true(any(grepl("sommot", example_roi_names, ignore.case = TRUE)))
  expect_true(any(grepl("dorsatt", example_roi_names, ignore.case = TRUE)))
  expect_true(any(grepl("salventattn", example_roi_names, ignore.case = TRUE)))
  expect_true(any(grepl("limbic", example_roi_names, ignore.case = TRUE)))
  expect_true(any(grepl("cont", example_roi_names, ignore.case = TRUE)))
  expect_true(any(grepl("default", example_roi_names, ignore.case = TRUE)))
})

# ==============================================================================
# example_conn_array Tests
# ==============================================================================

test_that("example_conn_array exists and is accessible", {
  expect_true(exists("example_conn_array"))
})

test_that("example_conn_array is an array", {
  expect_true(is.array(example_conn_array))
})

test_that("example_conn_array has correct dimensions", {
  dims <- dim(example_conn_array)

  expect_equal(length(dims), 3)
  expect_equal(dims[1], 30) # ROIs
  expect_equal(dims[2], 30) # ROIs
  expect_equal(dims[3], 10) # Subjects
})

test_that("example_conn_array is symmetric", {
  # Connectivity matrices should be symmetric (i <-> j same as j <-> i)
  for (subj in 1:dim(example_conn_array)[3]) {
    conn_matrix <- example_conn_array[,, subj]
    expect_true(all(abs(conn_matrix - t(conn_matrix)) < 1e-10))
  }
})

test_that("example_conn_array has valid correlation values", {
  # Values should be between -1 and 1 (correlation range)
  expect_true(all(example_conn_array >= -1))
  expect_true(all(example_conn_array <= 1))
})

test_that("example_conn_array has perfect self-correlation", {
  # Diagonal should be 1 (perfect correlation with self)
  for (subj in 1:dim(example_conn_array)[3]) {
    diag_values <- diag(example_conn_array[,, subj])
    expect_true(all(abs(diag_values - 1) < 1e-10))
  }
})

test_that("example_conn_array has dimnames", {
  expect_false(is.null(dimnames(example_conn_array)))

  # Check all three dimensions have names
  expect_equal(length(dimnames(example_conn_array)), 3)
  expect_false(is.null(dimnames(example_conn_array)[[1]]))
  expect_false(is.null(dimnames(example_conn_array)[[2]]))
  expect_false(is.null(dimnames(example_conn_array)[[3]]))
})

test_that("example_conn_array dimnames match example_roi_names", {
  array_roi_names <- dimnames(example_conn_array)[[1]]

  expect_equal(length(array_roi_names), 30)
  expect_equal(array_roi_names, example_roi_names)
})

test_that("example_conn_array has subject IDs", {
  subject_names <- dimnames(example_conn_array)[[3]]

  expect_length(subject_names, 10)
  expect_true(all(grepl("^subj", subject_names)))
})

test_that("example_conn_array is numeric", {
  expect_true(is.numeric(example_conn_array))
})

test_that("example_conn_array has no missing values", {
  expect_false(any(is.na(example_conn_array)))
})

# ==============================================================================
# example_network_indices Tests
# ==============================================================================

test_that("example_network_indices exists and is accessible", {
  expect_true(exists("example_network_indices"))
})

test_that("example_network_indices is a list", {
  expect_type(example_network_indices, "list")
})

test_that("example_network_indices has correct length", {
  expect_equal(length(example_network_indices), 9)
})

test_that("example_network_indices has correct names", {
  expected_names <- c(
    "default",
    "cont",
    "limbic",
    "salventattn",
    "dorsattn",
    "sommot",
    "vis",
    "ahip",
    "phip"
  )
  expect_equal(names(example_network_indices), expected_names)
})

test_that("example_network_indices contains integer vectors", {
  expect_true(all(sapply(example_network_indices, is.integer)))
})

test_that("example_network_indices has non-empty elements", {
  lengths <- sapply(example_network_indices, length)
  expect_true(all(lengths > 0))
})

test_that("example_network_indices indices are in valid range", {
  all_indices <- unlist(example_network_indices)

  expect_true(all(all_indices >= 1))
  expect_true(all(all_indices <= 30))
})

test_that("example_network_indices has no duplicate indices", {
  all_indices <- unlist(example_network_indices)

  expect_equal(length(all_indices), length(unique(all_indices)))
})

test_that("example_network_indices covers all ROIs", {
  all_indices <- unlist(example_network_indices)

  # Should have exactly 30 indices total (one for each ROI)
  expect_equal(length(all_indices), 30)

  # Should cover positions 1 through 30
  expect_setequal(all_indices, 1:30)
})

test_that("example_network_indices matches example_roi_names", {
  # Visual network indices should point to visual ROIs
  vis_indices <- example_network_indices$vis
  vis_roi_names <- example_roi_names[vis_indices]
  expect_true(all(grepl("vis", vis_roi_names, ignore.case = TRUE)))

  # Hippocampal indices should point to hippocampal ROIs
  expect_equal(example_roi_names[example_network_indices$ahip], "AHIP")
  expect_equal(example_roi_names[example_network_indices$phip], "PHIP")
})

# ==============================================================================
# Consistency Tests Across Example Data
# ==============================================================================

test_that("example data objects are mutually consistent", {
  # example_conn_array dimensions match example_roi_names
  expect_equal(dim(example_conn_array)[1], length(example_roi_names))
  expect_equal(dim(example_conn_array)[2], length(example_roi_names))

  # example_network_indices covers all ROIs in example_roi_names
  all_indices <- unlist(example_network_indices)
  expect_equal(length(all_indices), length(example_roi_names))
})

test_that("example data can be used together", {
  indices <- get_indices(example_roi_names)

  # Should be able to extract network connectivity
  vis_connectivity <- example_conn_array[
    indices$vis,
    indices$vis,
    1
  ]

  expect_true(is.matrix(vis_connectivity))
  expect_equal(nrow(vis_connectivity), length(indices$vis))
})

test_that("example_network_indices matches get_indices output", {
  # Running get_indices on example_roi_names should give same result
  computed_indices <- get_indices(example_roi_names)

  expect_equal(names(computed_indices), names(example_network_indices))

  # Indices should match for each network
  for (net in names(example_network_indices)) {
    expect_equal(
      computed_indices[[net]],
      example_network_indices[[net]],
      info = paste("Mismatch in network:", net)
    )
  }
})

# ==============================================================================
# Example Data Quality Tests
# ==============================================================================

test_that("example_conn_array has realistic connectivity values", {
  # Most correlations should be relatively small
  off_diag <- example_conn_array[1:30, 1:30, 1]
  off_diag[diag(1:30)] <- NA # Remove diagonal

  # Check that most values are between -0.5 and 0.5
  moderate_values <- sum(abs(off_diag) <= 0.5, na.rm = TRUE)
  total_values <- sum(!is.na(off_diag))

  expect_true(moderate_values / total_values > 0.7) # At least 70% moderate
})

test_that("example data is suitable for documentation", {
  # Small enough for quick examples
  expect_lte(length(example_roi_names), 50)
  expect_lte(dim(example_conn_array)[3], 20) # Not too many subjects

  # But large enough to be meaningful
  expect_gte(length(example_roi_names), 20)
  expect_gte(dim(example_conn_array)[3], 5)
})
