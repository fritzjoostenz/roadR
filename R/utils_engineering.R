
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
#' @return voctor containing lengths of the overlap with first section
#' @export
#'
rr_overlap_lengths <- function(seg1_start, seg1_end, seg_starts, seg_ends) {

  vec_func <- Vectorize(rr_overlap_length,
                        vectorize.args = c("seg2_start", "seg2_end"))

  vals <- vec_func(seg1_start, seg1_end, seg_starts, seg_ends)
  return(vals)

}
