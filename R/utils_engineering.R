
#' Gets the overlap between two linear segments
#'
#' \code{tt_overlap_length} takes the start and end locations for two linear 
#' segments and returns the length of the overlap between them in the same
#' units as the start and end locations
#' @param seg1_start 'start' or 'from' location for the first segment
#' @param seg1_end 'end' or 'to' location for the first segment
#' @param seg2_start 'start' or 'from' location for the second segment
#' @param seg2_end 'end' or 'to' location for the second segment
#' @return length of the overlap
#' @export
#'
tt_overlap_length <- function(seg1_start, seg1_end, seg2_start, seg2_end) {
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
