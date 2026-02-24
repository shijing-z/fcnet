# Tests for get_indices()
# Save as: tests/testthat/test-get_indices.R

# ==============================================================================
# Basic Functionality Tests
# ==============================================================================

test_that("get_indices works with character vector", {
  result <- get_indices(example_roi_names)

  expect_type(result, "list")
  expect_true(all(sapply(result, is.integer)))
  expect_equal(length(result), 9) # 7 networks + 2 hippocampal ROIs
})

test_that("get_indices works with connectivity array", {
  result <- get_indices(example_conn_array)

  expect_type(result, "list")
  expect_equal(length(result), 9)

  # Should have same networks as character vector version
  result_char <- get_indices(example_roi_names)
  expect_equal(names(result), names(result_char))
})

test_that("get_indices returns correct network names", {
  result <- get_indices(example_roi_names)

  expected_networks <- c(
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
  expect_equal(names(result), expected_networks)
})

# ==============================================================================
# Parameter Tests
# ==============================================================================

test_that("roi_include = 'schaefer' excludes non-Schaefer ROIs", {
  result <- get_indices(example_roi_names, roi_include = "schaefer")

  # Should NOT include ahip or phip
  expect_false("ahip" %in% names(result))
  expect_false("phip" %in% names(result))

  # Should include Schaefer networks
  expect_true("vis" %in% names(result))
  expect_true("default" %in% names(result))
})

test_that("roi_include = 'all' includes all ROIs", {
  result <- get_indices(example_roi_names, roi_include = "all")

  # Should include both Schaefer and non-Schaefer
  expect_true("vis" %in% names(result))
  expect_true("ahip" %in% names(result))
  expect_true("phip" %in% names(result))
})

test_that("reorder_networks = TRUE orders networks correctly", {
  result <- get_indices(example_roi_names, reorder_networks = TRUE)

  # First network should be default
  expect_equal(names(result)[1], "default")

  # Non-Schaefer ROIs should be at the end
  network_names <- names(result)
  n <- length(network_names)
  expect_true(network_names[n - 1] %in% c("ahip", "phip"))
  expect_true(network_names[n] %in% c("ahip", "phip"))
})

test_that("reorder_networks = FALSE preserves matrix order", {
  # Create test data with known order
  roi_names_custom <- c(
    "Schaefer100.l_vis_1",
    "AHIP",
    "Schaefer100.l_default_1",
    "PHIP"
  )

  result <- get_indices(roi_names_custom, reorder_networks = FALSE)

  # Networks should appear in order they first appear in matrix
  # vis (1), ahip (2), default (3), phip (4)
  # But grouping means: vis, ahip, default, phip
  expect_true("vis" %in% names(result))
  expect_true("ahip" %in% names(result))
  expect_true("default" %in% names(result))
  expect_true("phip" %in% names(result))
})

test_that("manual_assignments groups ROIs correctly", {
  result <- get_indices(
    example_roi_names,
    manual_assignments = list(ahip = "hippocampus", phip = "hippocampus")
  )

  # Should have hippocampus group
  expect_true("hippocampus" %in% names(result))

  # Should NOT have separate ahip/phip
  expect_false("ahip" %in% names(result))
  expect_false("phip" %in% names(result))

  # Hippocampus group should have 2 indices
  expect_length(result$hippocampus, 2)
})

test_that("manual_assignments is case-insensitive", {
  # Test with different case
  result <- get_indices(
    example_roi_names,
    manual_assignments = list(AHIP = "hippocampus", Phip = "hippocampus")
  )

  expect_true("hippocampus" %in% names(result))
  expect_false("ahip" %in% names(result))
  expect_false("phip" %in% names(result))
})

# ==============================================================================
# Edge Cases and Error Handling
# ==============================================================================

test_that("get_indices handles empty input", {
  expect_error(get_indices(character(0)))
})

test_that("get_indices handles NULL input", {
  expect_error(get_indices(NULL))
})

test_that("get_indices handles invalid input types", {
  # Numeric vector
  expect_error(get_indices(1:10))

  # Logical vector
  expect_error(get_indices(c(TRUE, FALSE)))

  # List
  expect_error(get_indices(list(a = 1, b = 2)))
})

test_that("get_indices handles array without dimnames", {
  # Create array without dimnames
  arr <- array(1:27, dim = c(3, 3, 3))

  expect_error(get_indices(arr), "dimnames")
})

test_that("get_indices handles non-existent ROI in manual_assignments", {
  expect_warning(
    get_indices(
      example_roi_names,
      manual_assignments = list(fake_roi = "test_group")
    ),
    "not found"
  )
})

# ==============================================================================
# Network Identification Tests
# ==============================================================================

test_that("get_indices correctly identifies visual network", {
  result <- get_indices(example_roi_names)

  expect_true("vis" %in% names(result))
  expect_true(length(result$vis) > 0)

  # Visual ROIs should be from visual network
  vis_roi_names <- example_roi_names[result$vis]
  expect_true(all(grepl("vis", vis_roi_names, ignore.case = TRUE)))
})

test_that("get_indices correctly identifies somatomotor network", {
  result <- get_indices(example_roi_names)

  expect_true("sommot" %in% names(result))
  expect_true(length(result$sommot) > 0)

  sommot_roi_names <- example_roi_names[result$sommot]
  expect_true(all(grepl("sommot", sommot_roi_names, ignore.case = TRUE)))
})

test_that("get_indices correctly identifies attention networks", {
  result <- get_indices(example_roi_names)

  # Should have both ventral and dorsal attention
  expect_true("salventattn" %in% names(result))
  expect_true("dorsattn" %in% names(result))
})

test_that("get_indices handles different attention network naming", {
  # Test with different naming conventions
  roi_names_variant <- c(
    "Schaefer100.l_ventatt_1", # ventatt instead of salventattn
    "Schaefer100.l_dorsatt_1" # dorsatt instead of dorsattn
  )

  result <- get_indices(roi_names_variant)

  # Should normalize to standard names
  expect_true("salventattn" %in% names(result))
  expect_true("dorsattn" %in% names(result))
})

# ==============================================================================
# Index Validity Tests
# ==============================================================================

test_that("get_indices returns valid indices", {
  result <- get_indices(example_roi_names)

  # All indices should be within valid range
  all_indices <- unlist(result)
  expect_true(all(all_indices >= 1))
  expect_true(all(all_indices <= length(example_roi_names)))

  # No duplicate indices across networks
  expect_equal(length(all_indices), length(unique(all_indices)))
})

test_that("get_indices indices sum to total ROIs", {
  result <- get_indices(example_roi_names)

  # Total number of indices should equal total ROIs
  total_indices <- sum(sapply(result, length))
  expect_equal(total_indices, length(example_roi_names))
})

test_that("get_indices with schaefer only returns correct count", {
  result_all <- get_indices(example_roi_names, roi_include = "all")
  result_schaefer <- get_indices(example_roi_names, roi_include = "schaefer")

  # Schaefer-only should have fewer indices
  total_all <- sum(sapply(result_all, length))
  total_schaefer <- sum(sapply(result_schaefer, length))

  expect_true(total_schaefer < total_all)
})

# 17-Network Tests ----

test_that("prettify_network_names distinguishes limbic from limbic+suffix", {
  # 7-network: "limbic" should stay lowercase
  expect_equal(fconn:::prettify_network_names("limbic"), "limbic")

  # 17-network: subnetworks should get uppercase letters
  expect_equal(fconn:::prettify_network_names("limbica"), "limbicA")
  expect_equal(fconn:::prettify_network_names("limbicb"), "limbicB")
})

test_that("prettify_network_names handles 17-network subnetworks correctly", {
  # Test various 17-network subnetworks
  expect_equal(fconn:::prettify_network_names("defaulta"), "defaultA")
  expect_equal(fconn:::prettify_network_names("defaultb"), "defaultB")
  expect_equal(fconn:::prettify_network_names("contb"), "contB")
  expect_equal(fconn:::prettify_network_names("contc"), "contC")
  expect_equal(fconn:::prettify_network_names("sommota"), "sommotA")

  # 7-network names without suffixes should stay lowercase
  expect_equal(fconn:::prettify_network_names("default"), "default")
  expect_equal(fconn:::prettify_network_names("cont"), "cont")
  expect_equal(fconn:::prettify_network_names("sommot"), "sommot")
})

test_that("get_indices works with 17-network style ROI names", {
  # Create mock 17-network ROI names
  roi_names_17net <- c(
    "Schaefer100.l_defaulta_pfc_1",
    "Schaefer100.l_defaultb_temp_1",
    "Schaefer100.l_limbica_ofc_1",
    "Schaefer100.l_limbicb_temp_1",
    "Schaefer100.l_contc_par_1",
    "Schaefer100.l_sommota_mot_1",
    "Schaefer100.l_vis_1" # Regular vis without suffix
  )

  result <- get_indices(roi_names_17net)

  # Check that subnetwork names are formatted with uppercase letters
  expect_true("defaultA" %in% names(result))
  expect_true("defaultB" %in% names(result))
  expect_true("limbicA" %in% names(result))
  expect_true("limbicB" %in% names(result))
  expect_true("contC" %in% names(result))
  expect_true("sommotA" %in% names(result))

  # Regular network without suffix should stay lowercase
  expect_true("vis" %in% names(result))

  # Verify indices are correct
  expect_equal(result$defaultA, 1L)
  expect_equal(result$defaultB, 2L)
  expect_equal(result$limbicA, 3L)
  expect_equal(result$limbicB, 4L)
  expect_equal(result$contC, 5L)
})
