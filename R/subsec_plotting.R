
#' Plots the data and sub-section locations for a section
#'
#' \code{rr_plot_segment_data} Plots the data and sub-section locations (with
#' their scores) for a section
#'
#' @param data data frame with deficit data for the section
#' @param sub_segs data frame with identified sub-sections and scores. This
#' should be the output from method \code{rr_get_subsections_v1}
#' @param x_min minimum for x-axis scale
#' @param x_max maximum for x-axis scale
#' @param x_inc increment for x-axis scale. If NULL is passed, a suitable increment
#' will be automatically picked
#' @param y_min minimim for y-axis scale. Default is zero, assuming a scale from
#' 1 to 12
#' @param y_max maximim for y-axis scale. Default is 12, assuming a scale from
#' 1 to 10 for deficits and then allows a bit at the top
#' @param best_seg_y y-location for the top-scoring sub-section. Default is 10.5
#' for a scale from 0 to 12
#' @return ggplot object that can be further customised
#' @export
#'
#' @importFrom stats runif
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom plyr round_any
#' @importFrom dplyr .data
rr_plot_segment_data <- function(data, sub_segs, x_min = NULL, x_max = NULL,
                                 x_inc = NULL, y_min = 0, y_max = 12,
                                 best_seg_y = 10.5) {

  if (is.null(x_min)) {
    x_min <- 0
  }
  if (is.null(x_max)) {
    x_max <- max(10000, max(data$loc_to))
  }

  data$y_plot <- data$deficit + stats::runif(nrow(data), -0.2,+0.2)
  max_fitness <- max(sub_segs$score)
  sub_segs$fitness_score <- best_seg_y * sub_segs$score/max_fitness

  if (is.null(x_inc)) {
    inc <- round_any((x_max - x_min)/15,10)
  }
  else {
    inc <- x_inc
  }

  gg <- ggplot(data) +
    ggplot2::geom_segment(aes(x = loc_from, xend = loc_to, y = .data$y_plot,
                              yend = .data$y_plot,
                              colour = .data$data_code), size = 3) +
    ggplot2::scale_x_continuous(limits = c(x_min, x_max),
                                breaks = seq(from = x_min, to = x_max, by = inc)) +
    ggplot2::scale_y_continuous(limits = c(y_min, y_max)) +
    ggplot2::xlab("Location") + ggplot2::ylab("Deficit Score") +
    viridis::scale_color_viridis(discrete=TRUE, option="D") +
    ggplot2::theme_classic()


  if (nrow(sub_segs) > 0) {

    for(i in 1:nrow(sub_segs)) {

      loc_from <- sub_segs[i, ]$loc_from
      loc_to <- sub_segs[i, ]$loc_to
      fitness_score <- sub_segs[i, ]$fitness_score
      fitness <- sub_segs[i, ]$score

      gg <- gg + ggplot2::geom_segment(x = loc_from, xend = loc_to,
                                       y = fitness_score, yend = fitness_score,
                                       size = 2, colour = "black")+
        ggplot2::geom_vline(xintercept = loc_from, colour = "darkgrey",
                            linetype = "dashed") +
        ggplot2::geom_vline(xintercept = loc_to, colour = "darkgrey",
                            linetype = "dashed") +
        ggplot2::annotate(geom="text", x=(loc_from + loc_to)/2,
                          y= (fitness_score + 0.35),
                          label= round(fitness,0),
                          color="black", hjust = 0.5, family = "mono", size = 3.5)
    }

  }
  return(gg)
}
