
#' Gets the overlap between two linear segments
#'
#' \code{rr_overlap_length} takes the start and end locations for two linear
#' segments and returns the length of the overlap between them in the same
#' units as the start and end locations
#' @param seg1_start 'start' or 'from' location for the first segment
#' @param seg1_end 'end' or 'to' location for the first segment
#' @param seg2_start 'start' or 'from' location for the second segment
#' @param seg2_end 'end' or 'to' location for the second segment
#' @return length of the overlap
#' @export
#'
rr_overlap_length <- function(seg1_start, seg1_end, seg2_start, seg2_end) {
  # Find the maximum of the start locations

  start <- max(seg1_start, seg2_start)
  # Find the minimum of the end locations
  end <- min(seg1_end, seg2_end)
  # If the start is greater than the end, there is no overlap
  if (start > end) {
    return(0)
  } else {
    # Calculate the length of overlap
    return(end - start)
  }
}

#' Gets the overlaps between one linear segment and a set of others
#'
#' \code{rr_overlap_lengths} is a vectorised version for getting the overlaps
#' between one segment and a set of others. Takes the start and end locations
#' for a linear segment plus arrays of the starts and ends of a set of other
#' segments and returns the length of the overlap between the first segment
#' and each of the other segments in the same units as the start and end
#' locations
#' @param seg1_start 'start' or 'from' location for the first segment
#' @param seg1_end 'end' or 'to' location for the first segment
#' @param seg_starts 'start' or 'from' location for the segments to check
#' against the first segment
#' @param seg_ends 'end' or 'to' location for the segments to check against the
#' first segment
#' @return vector containing lengths of the overlap with first section
#' @export
#'
rr_overlap_lengths <- function(seg1_start, seg1_end, seg_starts, seg_ends) {

  vec_func <- Vectorize(rr_overlap_length,
                        vectorize.args = c("seg2_start", "seg2_end"))

  vals <- vec_func(seg1_start, seg1_end, seg_starts, seg_ends)
  return(vals)

}


#' Assigns a percentile based 0 to 4 score based on defect values (relative)
#'
#' \code{rr_classify_defects_relative} can be used to get a classification of
#' 0 to 4 on a set of defect values that are assumed to be zero when there is no
#' defect, and a high number (can be any number) when there is severe/extensive
#' defects.
#'
#' The assignment of classes is made such that the assigned classes are:
#' \enumerate{
#'   \item 0: Assigned when defect values are zero
#'   \item 1: Assigned to values lower then the 25th percentile of values that
#'   are NOT zero
#'   \item 2: Assigned to defect values that lie between the 25th to 50th
#'   percentiles of values that are NOT zero
#'   \item 3: Assigned to defect values that lie between the 50th to 75th
#'   percentiles of values that are NOT zero
#'   \item 4: Assigned to defect values that are above the 75th percentile of
#'   values that are NOT zero
#' }
#'
#' This function can be used to assign a relative severity scale that ensures
#' that defect values that are zero are assigned a zero class, while the values
#' that are above a certain percentile get a rating of severe = 4.
#'
#' This function is useful when the distress values are not reliably mapped to
#' absolute values such as percent of segment or square metre. It can handle
#' zero inflated situations that are highly skewed.
#'
#' Note that the percentiles noted above (25th, 50th, 75th) are defaults and
#' can be overwritten in the parameter specifications.
#' @param df data frame holding the data
#' @param col_name name of the column that holds the defect data values
#' @param perc_c1 percentile that is the upper cut-off for class 1
#' @param perc_c2 percentile that is the upper cut-off for class 2
#' @param perc_c3 percentile that is the upper cut-off for class 3. Values
#' greater than this percentile are assigned a class 4 automatically
#' @param as_numeric If TRUE (default) then the values returned are numeric
#' (0,1,2,3,4) instead of Factor (be careful to convert Factor to numeric)
#' @return vector containing class values for each defect value
#' @export
#'
rr_classify_defects_relative <- function(df, col_name,
                                  perc_c1 = 0.25, perc_c2 = 0.5,
                                  perc_c3 = 0.75,
                                  as_numeric = TRUE) {

  gt_zeros <- df[df[, col_name] > 0, ]

  p1 <- stats::quantile(gt_zeros[ , col_name], perc_c1)
  p2 <- stats::quantile(gt_zeros[ , col_name], perc_c2)
  p3 <- stats::quantile(gt_zeros[ , col_name], perc_c3)

  df$class <- cut(df[ , col_name],
                    breaks = c(-Inf, 0, p1, p2, p3, Inf),
                    labels = c("0", "1", "2", "3", "4"),
                    include.lowest = TRUE)

  if (as_numeric) {
    result <- as.numeric(levels(df$class))[df$class]
  }
  else {
    result <- df$class
  }

  return(result)

}

#' Assigns a percentile based 0 to 4 score based on defect values (absolute)
#'
#' \code{rr_classify_defects_absol} can be used to get a classification of
#' 0 to 4 on a set of defect values that are assumed to be zero when there is no
#' defect, and a high number (can be any number) when there is severe/extensive
#' defects.
#'
#' The assignment of classes is made such that the assigned classes are:
#' \enumerate{
#'   \item 0: Assigned when defect values are zero
#'   \item 1: Assigned to values lower parameter c1 but NOT zero
#'   \item 2: Assigned to defect values that lie between the c1 and c2
#'   \item 3: Assigned to defect values that lie between the c2 and c3
#'   \item 4: Assigned to defect values that are above c3
#' }
#'
#' This function can be used to assign a severity scale based on absolute
#' threshold values. Defect values that are zero are assigned a zero class, and
#' classes 1 to 4 are assigned based on the thresholds provided as parameters
#' c1, c2 and c3
#'
#' Use this function when the distress values are reliably mapped to
#' absolute values such as percent of segment or square metre. Otherwise, you
#' can use a relative assignement based on percentiles:
#' @seealso \code{link{rr_classify_defects_relative}}
#'
#' @param df data frame holding the data
#' @param col_name name of the column that holds the defect data values
#' @param c1 upper threshold for class 1 (zeros excluded)
#' @param c2 upper threshold for class 2
#' @param c3 upper threshold for class 3. Values greater than this threshold
#' are assigned a class 4 automatically
#' @param as_numeric If TRUE (default) then the values returned are numeric
#' (0,1,2,3,4) instead of Factor (be careful to convert Factor to numeric!)
#' @return vector containing class values for each defect value
#' @export
#'
rr_classify_defects_absol <- function(df, col_name, c1, c2, c3,
                                      as_numeric = TRUE) {

  df$class <- cut(df[ , col_name],
                  breaks = c(-Inf, 0, c1, c2, c3, Inf),
                  labels = c("0", "1", "2", "3", "4"),
                  include.lowest = TRUE)

  if (as_numeric) {
    result <- as.numeric(levels(df$class))[df$class]
  }
  else {
    result <- df$class
  }

  return(result)

}

