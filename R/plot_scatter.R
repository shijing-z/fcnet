#' Plot connectivity-behavior scatter
#'
#' Create scatter plots with optional linear regression lines for exploring
#' connectivity-behavior relationships. Points, regression lines, and
#' confidence bands can be toggled on or off, and their appearance can be
#' customized through list arguments.
#'
#' @param data Data frame with one row per subject containing connectivity
#'   and behavioral/outcome columns
#' @param x Character string naming the x-axis column (typically a
#'   connectivity variable)
#' @param y Character string naming the y-axis column (typically a
#'   behavioral or outcome variable)
#' @param group Optional character string naming a grouping column. When
#'   provided, maps groups to color. When NULL (default), plots a single
#'   set of geoms
#' @param colors Optional color specification. When \code{group} is
#'   provided, a named character vector mapping group levels to colors
#'   (e.g., \code{c("YA" = "steelblue", "OA" = "coral")}). When
#'   \code{group} is NULL, a single color string applied to all geoms
#'   (e.g., \code{"steelblue"})
#' @param show_points Logical. If TRUE (default), display scatter points
#' @param show_line Logical. If TRUE (default), display linear regression
#'   lines with confidence bands
#' @param point_args Named list of additional arguments passed to
#'   \code{\link[ggplot2]{geom_point}}. Overrides defaults
#'   (\code{alpha = 0.6}). See Details
#' @param line_args Named list of additional arguments passed to
#'   \code{\link[ggplot2]{geom_smooth}} controlling the regression line.
#'   Overrides defaults (\code{linewidth = 1}). See Details
#' @param band_args Named list of additional arguments controlling the
#'   confidence band. Supports \code{se} (logical, default TRUE),
#'   \code{level} (numeric, default 0.95), and \code{alpha} (numeric,
#'   default 0.2). See Details
#' @param clean_labels Logical. If TRUE (default), clean column names for
#'   axis display by replacing underscores with hyphens and applying title
#'   case. If FALSE, use raw column names
#' @param title Optional character string for the plot title
#'
#' @return A \code{ggplot} object that can be further customized with
#'   standard ggplot2 syntax.
#'
#' @details
#' Regression lines are fitted with \code{geom_smooth(method = "lm")}.
#' When \code{group} is provided, separate lines are fitted for each
#' group.
#'
#' At least one of \code{show_points} or \code{show_line} must be TRUE.
#'
#' The \code{point_args}, \code{line_args}, and \code{band_args} lists
#' are merged with internal defaults using \code{utils::modifyList()}.
#' Defaults:
#' \itemize{
#'   \item \code{point_args}: \code{list(alpha = 0.6)}
#'   \item \code{line_args}: \code{list(linewidth = 1)}
#'   \item \code{band_args}: \code{list(se = TRUE, alpha = 0.2,
#'     level = 0.95)}
#' }
#'
#' Common \code{point_args} values include \code{size}, \code{shape},
#' \code{alpha}, and \code{color}. See
#' \code{\link[ggplot2]{geom_point}} for all options.
#'
#' Common \code{line_args} values include \code{linewidth},
#' \code{linetype}, \code{color}, and \code{method} (e.g.,
#' \code{"loess"}). See \code{\link[ggplot2]{geom_smooth}} for all
#' options.
#'
#' Band parameters \code{se}, \code{level}, and \code{alpha} must be
#' passed through \code{band_args}, not \code{line_args}. If found in
#' \code{line_args}, they are ignored with a warning.
#'
#' \code{colors} is a convenience shortcut that applies a uniform color
#' to all layers. For per-layer control, use \code{color} or \code{fill}
#' inside the \code{*_args} lists instead. When both are specified,
#' \code{*_args} takes precedence over \code{colors}.
#'
#' For grouped plots where points and bands need different shades per
#' group, override the scales directly with standard ggplot2 syntax:
#' \preformatted{plot_scatter(df, x = "x", y = "y", group = "group") +
#'   scale_color_manual(values = c("YA" = "navy", "OA" = "darkred")) +
#'   scale_fill_manual(values = c("YA" = "lightblue", "OA" = "lightsalmon"))}
#'
#' Axis labels can always be overridden with \code{+ labs(x = ..., y = ...)}
#' regardless of the \code{clean_labels} setting.
#'
#' @examples
#' # Ungrouped scatter
#' indices <- get_indices(ex_conn_array)
#' ahip_df <- calc_conn(ex_conn_array, indices,
#'   from = "ahip", to = "default"
#' )
#' ahip_df$behavior <- rnorm(10)
#'
#' plot_scatter(ahip_df, x = "ahip_default", y = "behavior")
#'
#' # Ungrouped with a single color
#' plot_scatter(ahip_df, x = "ahip_default", y = "behavior",
#'   colors = "steelblue"
#' )
#'
#' # Grouped scatter
#' ahip_df$group <- rep(c("YA", "OA"), times = c(5, 5))
#' plot_scatter(ahip_df, x = "ahip_default", y = "behavior",
#'   group = "group"
#' )
#'
#' # Custom group colors
#' plot_scatter(ahip_df, x = "ahip_default", y = "behavior",
#'   group = "group",
#'   colors = c("YA" = "steelblue", "OA" = "coral")
#' )
#'
#' # Lines and bands only (no points)
#' plot_scatter(ahip_df, x = "ahip_default", y = "behavior",
#'   group = "group",
#'   show_points = FALSE,
#'   colors = c("YA" = "steelblue", "OA" = "coral")
#' )
#'
#' # Points only (no line or band)
#' plot_scatter(ahip_df, x = "ahip_default", y = "behavior",
#'   group = "group",
#'   show_line = FALSE,
#'   point_args = list(size = 3, alpha = 0.8)
#' )
#'
#' # Custom line style and wider confidence band
#' plot_scatter(ahip_df, x = "ahip_default", y = "behavior",
#'   group = "group",
#'   line_args = list(linetype = "dashed"),
#'   band_args = list(alpha = 0.3, level = 0.99)
#' )
#'
#' # Disable confidence band
#' plot_scatter(ahip_df, x = "ahip_default", y = "behavior",
#'   group = "group",
#'   band_args = list(se = FALSE)
#' )
#'
#' # Different colors for points, line, and band
#' plot_scatter(ahip_df, x = "ahip_default", y = "behavior",
#'   point_args = list(color = "steelblue"),
#'   line_args = list(color = "coral", fill = "grey80")
#' )
#'
#' \dontrun{
#' # Full workflow with real data
#' z_mat <- load_matrices("data/conn.mat", type = "zmat", exclude = c(3, 5))
#' indices <- get_indices(z_mat)
#' ahip_nets <- calc_conn(z_mat, indices,
#'   from = "ahip", to = c("default", "cont"))
#' df <- cbind(ahip_nets, demo[c("group", "memory")])
#'
#' plot_scatter(df, x = "ahip_default", y = "memory", group = "group",
#'   colors = c("YA" = "steelblue", "OA" = "coral"),
#'   point_args = list(size = 2, shape = 17),
#'   title = "AHIP-Default Connectivity on Memory"
#' )
#' }
#'
#' @seealso
#' \code{\link{plot_heatmap}} for connectivity matrix heatmaps.
#' \code{\link{plot_compare}} for group comparison bar plots.
#'
#' @importFrom rlang .data
#' @export

plot_scatter <- function(
  data,
  x,
  y,
  group = NULL,
  colors = NULL,
  show_points = TRUE,
  show_line = TRUE,
  point_args = list(),
  line_args = list(),
  band_args = list(),
  clean_labels = TRUE,
  title = NULL
) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data frame.", call. = FALSE)
  }

  if (!is.character(x) || length(x) != 1) {
    stop(
      "x must be a single character string naming a column in data.",
      call. = FALSE
    )
  }

  if (!is.character(y) || length(y) != 1) {
    stop(
      "y must be a single character string naming a column in data.",
      call. = FALSE
    )
  }

  if (!x %in% names(data)) {
    stop("Column '", x, "' not found in data.", call. = FALSE)
  }

  if (!y %in% names(data)) {
    stop("Column '", y, "' not found in data.", call. = FALSE)
  }

  if (!is.numeric(data[[x]])) {
    stop("x column '", x, "' must be numeric.", call. = FALSE)
  }

  if (!is.numeric(data[[y]])) {
    stop("y column '", y, "' must be numeric.", call. = FALSE)
  }

  if (!is.null(group)) {
    if (!is.character(group) || length(group) != 1) {
      stop(
        "group must be a single character string naming a column in data.",
        call. = FALSE
      )
    }

    if (!group %in% names(data)) {
      stop("Group column '", group, "' not found in data.", call. = FALSE)
    }

    data[[group]] <- as.factor(data[[group]])

    if (length(levels(data[[group]])) < 2) {
      stop(
        "group column must have at least 2 levels. Found: ",
        length(levels(data[[group]])),
        call. = FALSE
      )
    }
  }

  if (!is.null(colors)) {
    if (!is.character(colors)) {
      stop("colors must be a character vector.", call. = FALSE)
    }
    if (!is.null(group) && is.null(names(colors))) {
      stop(
        "colors must be a named vector when group is provided ",
        "(e.g., c(\"YA\" = \"steelblue\", \"OA\" = \"coral\")).",
        call. = FALSE
      )
    }
    if (is.null(group) && length(colors) != 1) {
      stop(
        "colors must be a single color string when group is NULL.",
        call. = FALSE
      )
    }
  }

  if (!isTRUE(show_points) && !isTRUE(show_line)) {
    stop(
      "At least one of show_points or show_line must be TRUE.",
      call. = FALSE
    )
  }

  if (!is.list(point_args)) {
    stop("point_args must be a list.", call. = FALSE)
  }

  if (!is.list(line_args)) {
    stop("line_args must be a list.", call. = FALSE)
  }

  if (!is.list(band_args)) {
    stop("band_args must be a list.", call. = FALSE)
  }

  # Guard: redirect band parameters found in line_args
  band_keys <- c("se", "level", "alpha")
  misplaced <- intersect(names(line_args), band_keys)
  if (length(misplaced) > 0) {
    warning(
      "Band parameter(s) ", paste(misplaced, collapse = ", "),
      " found in line_args. Use band_args instead. Ignoring.",
      call. = FALSE
    )
    line_args[misplaced] <- NULL
  }

  # Merge user args with defaults
  point_defaults <- list(alpha = 0.6)
  line_defaults <- list(linewidth = 1)

  # Apply single color for ungrouped plots
  if (is.null(group) && !is.null(colors)) {
    point_defaults$color <- colors
    line_defaults$color <- colors
  }

  point_params <- utils::modifyList(point_defaults, point_args)
  line_params <- utils::modifyList(line_defaults, line_args)

  band_defaults <- list(se = TRUE, alpha = 0.2, level = 0.95)
  band_params <- utils::modifyList(band_defaults, band_args)

  # Clean labels
  if (clean_labels) {
    clean_one <- function(label) {
      label <- gsub("_", "-", label)
      parts <- strsplit(label, "-")[[1]]
      parts <- tools::toTitleCase(parts)
      paste(parts, collapse = "-")
    }
    x_label <- clean_one(x)
    y_label <- clean_one(y)
  } else {
    x_label <- x
    y_label <- y
  }

  # Build base plot
  if (is.null(group)) {
    p <- ggplot2::ggplot(
      data,
      ggplot2::aes(x = .data[[x]], y = .data[[y]])
    )
  } else {
    p <- ggplot2::ggplot(
      data,
      ggplot2::aes(
        x = .data[[x]],
        y = .data[[y]],
        color = .data[[group]]
      )
    )
  }

  # Add points
  if (show_points) {
    p <- p + do.call(ggplot2::geom_point, point_params)
  }

  # Add regression line and band
  if (show_line) {
    # Separate se and level from visual args for geom_smooth
    smooth_defaults <- list(
      method = "lm", se = band_params$se, level = band_params$level
    )
    smooth_params <- utils::modifyList(smooth_defaults, line_params)

    if (isTRUE(band_params$se)) {
      smooth_params$alpha <- band_params$alpha
      if (is.null(group) && !is.null(colors) && is.null(line_args$fill)) {
        smooth_params$fill <- colors
      }
    }

    if (!is.null(group)) {
      smooth_params$mapping <- ggplot2::aes(fill = .data[[group]])
    }

    p <- p + do.call(ggplot2::geom_smooth, smooth_params)
  }

  # Add color and fill scales
  if (!is.null(group) && !is.null(colors)) {
    p <- p + ggplot2::scale_color_manual(values = colors)
    if (show_line) {
      p <- p + ggplot2::scale_fill_manual(values = colors)
    }
  }

  # Add theme and labels
  plot_labs <- list(x = x_label, y = y_label)
  if (!is.null(group)) plot_labs$color <- "Group"
  if (!is.null(group) && show_line) plot_labs$fill <- "Group"

  p <- p +
    do.call(ggplot2::labs, plot_labs) +
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 5)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 5)),
      legend.position = if (is.null(group)) "none" else "bottom",
      legend.margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 5),
      panel.grid = ggplot2::element_blank()
    )

  # Suppress fill legend (redundant with color legend)
  if (!is.null(group) && show_line) {
    p <- p +
      ggplot2::guides(fill = "none")
  }

  # Add title
  if (!is.null(title)) {
    p <- p + ggplot2::ggtitle(title)
  }

  return(p)
}
