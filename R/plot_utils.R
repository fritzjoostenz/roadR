

' Gets optimal subsections on a set of Treatment Lengths
#'
#' \code{rr_get_subsecs_on_sections} Generates a set of optimal sub-sections
#' for each section within a larger set. For example, you can use this function
#' to generate sub-sections for an entire network when the set of sections
#' provided as the first parameter represent all sections on your network.
#'
#' Note: This method DOES provide a full set of sub-segments representing the
#' entire network defined by the set of treatment lengths you provide.
#'
#' Also note that this method, unlike \code{\link{rr_get_subsecs_on_sections}}
#' IS lane specific. If your data and/or treatment lengths are not
#' lane-specific, then use lane code 'all' in your deficit data or treatment
#' length definitions. If your data and treatment length definitions ARE
#' lane-specific, then note that the comparison is case-sensitive!
#'
#' @param data data frame containing a the data to plot
#' @param x_col name of the column holding the X-axis data. Should be factor or
#' character
#' @param y_col name of the column holding the Y-axis data.
#' @param y_min minimum for y-axis
#' @param y_max maximum for y-axis
#' @param y_inc increment for y-axis
#' @param x_label label for X-axis, or pass empty string to omit
#' @param y_label label for Y-axis, or pass empty string to omit
#' @param show_jitter Should points be shown using jitter?
#' @param obs_size Size of labels showing number of observations (values should
#' be in range of 3-6, these sizes are not font sizes). Enter zero to omit the
#' number of observations
#' @param obs_offset Percentage of Y-axis maximum at which observations should
#' be shown. To show labels for number of observations right at the top, pass
#' in 1.0
#' @param font_name Name of the font. Default is 'serif'. You can try other
#' fonts handled by ggplot such as 'sans'
#' @param x_font_size Size of font for X-axis label and ticks
#' @param y_font_size Size of font for Y-axis label and ticks
#' @param x_lab_rot_angle Rotation angle for X-axis labels. Default is zero (no
#' rotation). You can experiment with negative values, such as -35 for longer
#' labels
#' @export
#'
#' @importFrom stats complete.cases
#' @importFrom data.table as.data.table
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 stat_summary
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 geom_jitter
rr_plot_boxplot <- function(data, x_col, y_col, y_min, y_max, y_inc,
                        x_label, y_label,
                        show_jitter = TRUE,
                        obs_size = 5,  obs_offset = 1,
                        font_name = "serif",
                        x_font_size = 12, y_font_size = 12,
                        x_lab_rot_angle = 0) {

  y_limits <- c(y_min, y_max)
  increms <- seq(y_min, y_max, y_inc)

  counts <- function(x){
    adjust <- obs_offset
    return(c(y = adjust * y_max, label = length(x)))
  }

  # Copy data and add generic columns to make it easier to refer to these in
  # ggplot
  df <- data
  df$xx <- data[ , x_col]
  df$yy <- data[ , y_col]

  y_label <- paste0(y_label, "\n")

  p <- ggplot2::ggplot(df, aes(x = .data$xx, y=.data$yy)) +
    geom_boxplot(outlier.alpha = 0.5, outlier.size = 0.5,
                 fill = "lightsteelblue") +
    stat_summary(fun.data = counts, geom = "text",
                 family = font_name, size = obs_size) +
    scale_y_continuous(limits = y_limits, breaks = increms) +
    xlab(x_label) +
    ylab(y_label) +
    theme_minimal() +
    theme(
      text = element_text(family = font_name),
      axis.title.x = element_text(size = x_font_size),
      axis.title.y = element_text(size = y_font_size),
      axis.text.x=element_text(size = x_font_size,
                               angle = x_lab_rot_angle, hjust = 0),
      axis.text.y=element_text(size = y_font_size),
      panel.grid.major.y = element_line(color = "lightgray",
                                        size = 0.15,linetype = 2),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank())

  if (show_jitter) {
    p <- p + geom_jitter(alpha = 0.05, width = 0.15, colour = "black")
  }
  return(p)
}
