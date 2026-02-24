#' Get ROI indices organized by network
#'
#' Extracts ROI index positions and organizes them by network or region name.
#' Schaefer ROIs are automatically grouped by functional network. Other ROIs
#' are preserved individually unless grouped via manual_assignments.
#'
#' @param x Input source for ROI names. Can be:
#'   \itemize{
#'     \item A file path to a CONN toolbox .mat file (requires NetworkToolbox)
#'     \item A 3D connectivity array with ROI names in dimnames
#'     \item A character vector of ROI names
#'   }
#' @param roi_include Character. "all" (default) processes all ROIs; "schaefer"
#'   processes only Schaefer-labeled ROIs.
#' @param reorder_networks Logical. If TRUE (default), reorder Schaefer networks
#'   as: default, cont, limbic, salventattn, dorsattn, sommot, vis.
#'   Non-Schaefer ROIs always appear at the end.
#' @param manual_assignments Named list to override groupings. Format:
#'   list(roi_name = "group_name"). Matching is case-insensitive.
#'
#' @return A named list where names are network/ROI labels and values are
#'   integer vectors of ROI indices.
#'
#' @details
#' Schaefer ROIs are grouped by network (e.g., all visual ROIs → "vis").
#' Non-Schaefer ROIs are preserved individually with lowercase names
#' (e.g., "AHIP" → "ahip"). Attention network naming variations across
#' atlas versions (ventatt, salventattn, dorsatt, etc.) are automatically
#' normalized for consistency.
#'
#' Works with any Schaefer version (100-1000 parcels, 7 or 17 networks).
#'
#' @examples
#' # From character vector of ROI names
#' indices <- get_indices(example_roi_names)
#' names(indices)
#'
#' # Access specific network indices
#' indices$vis
#' indices$default
#' indices$ahip
#'
#' # From connectivity array with dimnames
#' indices <- get_indices(example_conn_array)
#' names(indices)
#'
#' # Extract Schaefer ROIs only
#' indices_schaefer <- get_indices(example_roi_names, roi_include = "schaefer")
#' names(indices_schaefer)
#'
#' # Manual grouping of ROIs
#' indices_grouped <- get_indices(
#'   example_roi_names,
#'   manual_assignments = list(ahip = "hippocampus", phip = "hippocampus")
#' )
#' indices_grouped$hippocampus
#'
#' # Preserve original matrix order
#' indices_original <- get_indices(example_roi_names, reorder_networks = FALSE)
#'
#' \dontrun{
#' # From CONN .mat file
#' indices <- get_indices("path/to/conn.mat")
#'
#' # From connectivity array loaded with NetworkToolbox
#' conn_mats <- NetworkToolbox::convertConnBrainMat("path/to/conn.mat")
#' indices <- get_indices(conn_mats$rmat)
#' }
#'
#' @export

get_indices <- function(
  x,
  roi_include = c("all", "schaefer"),
  reorder_networks = TRUE,
  manual_assignments = NULL
) {
  roi_include <- match.arg(roi_include)

  if (is.character(x) && length(x) == 0) {
    stop("Input must contain at least one ROI name", call. = FALSE)
  }

  # Get ROI names from various input types
  if (is.character(x) && length(x) == 1 && file.exists(x)) {
    # Input is a file path - process with NetworkToolbox
    if (!requireNamespace("NetworkToolbox", quietly = TRUE)) {
      stop(
        "Package 'NetworkToolbox' is required to read .mat files. ",
        "Install with: install.packages('NetworkToolbox')"
      )
    }

    conn_mats <- NetworkToolbox::convertConnBrainMat(x)

    # Validate CONN structure
    if (!is.list(conn_mats) || !"rmat" %in% names(conn_mats)) {
      stop(
        "File does not appear to be a valid CONN toolbox output. ",
        "Expected list with 'rmat' element."
      )
    }

    # Extract ROI names (only rmat has them in CONN output)
    roi_names <- colnames(conn_mats$rmat)

    if (is.null(roi_names)) {
      stop(
        "Could not extract ROI names from .mat file. ",
        "Ensure file is a valid CONN toolbox output."
      )
    }
  } else if (is.character(x)) {
    # Input is already a character vector of ROI names
    roi_names <- x
  } else if (is.array(x)) {
    # Input is a connectivity array
    roi_names <- dimnames(x)[[1]]
    if (is.null(roi_names)) {
      stop("Connectivity array must have ROI names in dimnames")
    }
  } else {
    stop(
      "Input must be: (1) path to CONN .mat file, ",
      "(2) connectivity array with ROI names in dimnames, or ",
      "(3) character vector of ROI names"
    )
  }

  # Clean any whitespace from ROI names
  roi_names <- trimws(roi_names)

  # Identify Schaefer ROIs
  is_schaefer <- grepl("^Schaefer[0-9]+\\.", roi_names, ignore.case = TRUE)

  # Initialize network labels
  network_labels <- character(length(roi_names))

  if (roi_include == "all") {
    # Extract Schaefer networks
    if (any(is_schaefer)) {
      network_labels[is_schaefer] <- extract_schaefer_networks(roi_names[
        is_schaefer
      ])
    }

    # Preserve other ROIs with their original names (lowercase)
    network_labels[!is_schaefer] <- tolower(roi_names[!is_schaefer])
  } else if (roi_include == "schaefer") {
    # Only process Schaefer ROIs
    if (any(is_schaefer)) {
      network_labels[is_schaefer] <- extract_schaefer_networks(roi_names[
        is_schaefer
      ])
    }

    # Other ROIs excluded (set to NA)
    network_labels[!is_schaefer] <- NA_character_
  }

  # Apply manual assignments (overrides automatic detection, case-insensitive)
  if (!is.null(manual_assignments)) {
    for (roi_name in names(manual_assignments)) {
      # Case-insensitive matching
      roi_idx <- which(tolower(roi_names) == tolower(roi_name))
      if (length(roi_idx) > 0) {
        network_labels[roi_idx] <- manual_assignments[[roi_name]]
      } else {
        warning("ROI '", roi_name, "' not found in matrix. Skipping.")
      }
    }
  }

  # Create list of ROI indices by network (preserves matrix order)
  indices <- split(seq_along(roi_names), network_labels)

  # Handle empty strings from failed parsing
  if ("" %in% names(indices)) {
    warning("Some ROIs could not be parsed. Check ROI naming format.")
    indices <- indices[names(indices) != ""]
  }

  # Remove NA group (excluded ROIs)
  indices <- indices[!is.na(names(indices))]

  # Remove empty groups
  indices <- indices[sapply(indices, length) > 0]

  # Reorder if requested
  if (reorder_networks) {
    indices <- reorder_network_list(indices)
  }

  return(indices)
}

#' Extract network names from Schaefer ROI labels
#'
#' @param roi_names Character vector of Schaefer ROI names
#' @return Character vector of network names (formatted for readability)
#' @keywords internal
extract_schaefer_networks <- function(roi_names) {
  # Remove "Schaefer###." prefix (case-insensitive)
  clean_names <- sub("^Schaefer[0-9]+\\.", "", roi_names, ignore.case = TRUE)

  # Split by underscores
  parts_list <- strsplit(clean_names, "_")

  # Extract network name (position 2)
  network_labels <- sapply(parts_list, function(parts) {
    if (length(parts) >= 2) {
      return(parts[2])
    } else {
      return(NA_character_)
    }
  })

  # First normalize to lowercase for consistent processing
  network_labels <- tolower(network_labels)

  # Normalize variations in attention network names
  network_labels <- normalize_attention_networks(network_labels)

  # Apply pretty formatting for 17-network (uppercase A/B/C, camelCase for special networks)
  network_labels <- prettify_network_names(network_labels)

  return(network_labels)
}

#' Normalize variations in attention network names
#'
#' @param network_names Character vector of lowercase network names
#' @return Character vector with standardized attention network names
#' @keywords internal
#' @details
#' Handles variations across different Schaefer atlas versions and preprocessing pipelines:
#' - Ventral attention: ventatt, ventattn, salventatt, salvatt, salvattn,
#'   ventralatt, ventralattn, salience, van → salventattn
#' - Dorsal attention: dorsatt, dorsalatt, dorsalattn, dan → dorsattn
normalize_attention_networks <- function(network_names) {
  sapply(
    network_names,
    function(name) {
      if (is.na(name) || name == "") {
        return(NA_character_)
      }

      # Extract base name and suffix (a/b/c) if present
      suffix <- ""
      base_name <- name

      # Remove hyphens (handles variants like "sal-vent-attn" or "dorsal-attention")
      base_name <- gsub("-", "", base_name)

      if (grepl("[abc]$", name)) {
        suffix <- substr(name, nchar(name), nchar(name))
        base_name <- substr(name, 1, nchar(name) - 1)
      }

      # Normalize ventral attention variations (order matters - check longer patterns first)
      ventral_patterns <- c(
        "salventattn",
        "salventatt", # Standard forms
        "ventralattn",
        "ventralatt", # Full "ventral" spelling
        "salvattn",
        "salvatt", # Typo/abbreviation variants
        "ventattn",
        "ventatt", # Without "sal" prefix
        "salience", # Alternative naming convention
        "van" # Abbreviation (Ventral Attention Network)
      )
      if (base_name %in% ventral_patterns) {
        base_name <- "salventattn"
      }

      # Normalize dorsal attention variations
      dorsal_patterns <- c(
        "dorsattn",
        "dorsatt", # Standard forms
        "dorsalattn",
        "dorsalatt", # Full "dorsal" spelling
        "dan" # Abbreviation (Dorsal Attention Network)
      )
      if (base_name %in% dorsal_patterns) {
        base_name <- "dorsattn"
      }

      return(paste0(base_name, suffix))
    },
    USE.NAMES = FALSE
  )
}

#' Prettify network names for better readability
#'
#' @param network_names Character vector of lowercase network names
#' @return Character vector with prettified names
#' @keywords internal
#' @details
#' Applies formatting rules:
#' - 17-network subnetworks: defaultA, contB, etc. (uppercase A/B/C)
#' - Visual networks: visCent, visPeri (camelCase)
#' - Temporal parietal: tempPar (camelCase)
#' - 7-network: unchanged (vis, default, cont, etc.)
#' Note: Attention networks are already normalized to salventattn/dorsattn
prettify_network_names <- function(network_names) {
  sapply(
    network_names,
    function(name) {
      if (is.na(name)) {
        return(NA_character_)
      }

      # Handle 17-network subnetworks (ending in a/b/c)
      known_bases <- c(
        "default",
        "cont",
        "limbic",
        "salventattn",
        "dorsattn",
        "sommot",
        "vis",
        "temppar"
      )
      potential_base <- sub("([abc])$", "", name)
      if (grepl("[abc]$", name) && potential_base %in% known_bases) {
        # Extract letter
        letter <- toupper(sub(".*([abc])$", "\\1", name))

        # Apply special formatting to base if needed
        if (potential_base == "salventattn") {
          return(paste0("salventatt", letter)) # salventattA, salventattB
        } else if (potential_base == "dorsattn") {
          return(paste0("dorsatt", letter)) # dorsattA, dorsattB
        }

        return(paste0(potential_base, letter))
      }

      # Handle special camelCase networks (17-network specific)
      if (name == "viscent") {
        return("visCent")
      }
      if (name == "visperi") {
        return("visPeri")
      }
      if (name == "temppar") {
        return("tempPar")
      }

      # Everything else stays lowercase (7-network or non-subnetwork)
      # This includes salventattn and dorsattn for 7-network
      return(name)
    },
    USE.NAMES = FALSE
  )
}

#' Reorder network list with Schaefer networks first, others at end
#'
#' @param indices Named list of ROI indices
#' @return Reordered list
#' @keywords internal
reorder_network_list <- function(indices) {
  # Desired network order for 7-network Schaefer (lowercase)
  network_order_7 <- c(
    "default",
    "cont",
    "limbic",
    "salventattn",
    "dorsattn",
    "sommot",
    "vis"
  )

  # For 17-network, group by base network following same order (lowercase base)
  # temppar positioned after default (functionally related to default mode)
  network_order_17_base <- c(
    "default", # defaultA, defaultB, defaultC
    "temppar", # tempPar
    "cont", # contA, contB, contC
    "limbic", # limbicA, limbicB
    "salventatt", # salventattA, salventattB
    "dorsatt", # dorsattA, dorsattB
    "sommot", # sommotA, sommotB
    "vis" # visCent, visPeri
  )

  network_names <- names(indices)

  # Convert to lowercase for matching (preserves original prettified names)
  network_names_lower <- tolower(network_names)

  # Separate Schaefer networks from other ROIs (case-insensitive)
  # Check if this is 7-network or 17-network
  is_7net <- any(network_names_lower %in% network_order_7)
  # Check for 17-network by looking for subnetwork suffixes ON Schaefer-like base names
  schaefer_bases <- c(
    "default",
    "cont",
    "limbic",
    "salventatt",
    "dorsatt",
    "sommot",
    "vis",
    "temppar"
  )
  is_17net <- any(sapply(schaefer_bases, function(base) {
    any(grepl(paste0("^", base, "[abc]$"), network_names_lower))
  })) ||
    any(network_names_lower %in% c("viscent", "visperi", "temppar"))

  if (is_17net) {
    # For 17-network, order by base network name (case-insensitive)
    is_schaefer <- grepl(
      "^(default|temppar|cont|limbic|salventatt|dorsatt|sommot|vis)",
      network_names_lower
    )
    schaefer_nets <- network_names[is_schaefer]
    other_rois <- network_names[!is_schaefer]

    # Order Schaefer networks by base, preserving sub-network order from matrix
    ordered_schaefer <- character(0)
    for (base_net in network_order_17_base) {
      # Match against lowercase version but keep original prettified names
      matching_idx <- grepl(paste0("^", base_net), network_names_lower) &
        is_schaefer
      matching <- network_names[matching_idx]

      # Sort subnetworks (A, B, C or Cent, Peri) alphabetically
      if (length(matching) > 1) {
        matching <- sort(matching)
      }

      ordered_schaefer <- c(ordered_schaefer, matching)
    }
  } else if (is_7net) {
    # For 7-network (case-insensitive matching)
    schaefer_nets <- network_names[network_names_lower %in% network_order_7]
    other_rois <- network_names[!network_names_lower %in% network_order_7]

    # Order according to specified order
    ordered_schaefer <- character(0)
    for (net in network_order_7) {
      if (net %in% network_names_lower) {
        # Find the actual name (preserves any formatting)
        actual_name <- network_names[network_names_lower == net]
        ordered_schaefer <- c(ordered_schaefer, actual_name)
      }
    }
  } else {
    # No Schaefer networks - all are other ROIs
    ordered_schaefer <- character(0)
    other_rois <- network_names
  }

  # Combine: Schaefer networks first (reordered), then other ROIs (original order)
  # Other ROIs maintain their original appearance order in the matrix
  final_order <- c(ordered_schaefer, other_rois)

  return(indices[final_order])
}
