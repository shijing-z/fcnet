#' Plot connectivity-behavior scatter
#'
#' Create scatter plots with linear regression lines for exploring
#' connectivity-behavior relationships.
#'
#' @param data Data frame with one row per subject containing connectivity
#'   and behavioral/outcome columns
#' @param x Character string naming the x-axis column (typically a
#'   connectivity variable)
#' @param y Character string naming the y-axis column (typically a
#'   behavioral or outcome variable)
#' @param group Optional character string naming a grouping column. When
#'   provided, plots per-group regression lines. When NULL (default),
#'   plots a single regression line
#' @param clean_labels Logical. If TRUE (default), clean column names for
#'   axis display by replacing underscores with hyphens and applying title
#'   case. If FALSE, use raw column names
#' @param title Optional character string for the plot title
#'
#' @return A \code{ggplot} object that can be further customized with
#'   standard ggplot2 syntax.
#'
#' @details
#' Regression lines are fitted with \code{geom_smooth(method = "lm")} and
#' include a shaded 95 percent confidence band. When \code{group} is
#' provided, separate lines are fitted for each group.
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
#' # Grouped scatter
#' ahip_df$group <- rep(c("YA", "OA"), times = c(5, 5))
#' plot_scatter(ahip_df, x = "ahip_default", y = "behavior",
#'   group = "group"
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

  # Build plot
  if (is.null(group)) {
    # Ungrouped scatter
    p <- ggplot2::ggplot(
      data,
      ggplot2::aes(x = .data[[x]], y = .data[[y]])
    ) +
      ggplot2::geom_point(alpha = 0.6) +
      ggplot2::geom_smooth(
        method = "lm",
        linewidth = 1,
        se = TRUE,
        alpha = 0.2
      )
  } else {
    # Grouped scatter
    p <- ggplot2::ggplot(
      data,
      ggplot2::aes(
        x = .data[[x]],
        y = .data[[y]],
        color = .data[[group]]
      )
    ) +
      ggplot2::geom_point(alpha = 0.6) +
      ggplot2::geom_smooth(
        ggplot2::aes(fill = .data[[group]]),
        method = "lm",
        linewidth = 1,
        se = TRUE,
        alpha = 0.2
      )
  }

  # Add theme and labels
  p <- p +
    ggplot2::labs(
      x = x_label,
      y = y_label,
      color = "Group",
      fill = "Group"
    ) +
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 5)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 5)),
      legend.position = if (is.null(group)) "none" else "bottom",
      legend.margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 5),
      panel.grid = ggplot2::element_blank()
    )

  # Suppress fill legend (redundant with color legend)
  if (!is.null(group)) {
    p <- p +
      ggplot2::guides(fill = "none")
  }

  # Add title
  if (!is.null(title)) {
    p <- p + ggplot2::ggtitle(title)
  }

  return(p)
}
