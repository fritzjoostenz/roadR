
#-------------------------------------------------------------------------------
#
#     Utility Tools for filtering and selecting linear element rows that
#     match criteria, e.g. handling lanes etc.
#
#-------------------------------------------------------------------------------

#' Gets all data for a specific treatment length or segment
#'
#' \code{rr_get_tl_data} Filters data and returns only those rows where
#' (a) the section Id matches the section ID of the specified treatment length or
#' segment;
#' (b) the data locations has some overlap with treatment length locations (note
#' that there is no minimum overlap specified)
#' (c) the data lane code is 'all' or else matches the lane code of the
#' specified treatment length (if the treatment length has lane code 'all', then
#' lane filtering is omitted)
#' @param treat_len data frame containing information for the specified
#' treatment length. Required columns are:
#' \enumerate{
#'   \item section_id: ID for the section on which the treatment length is
#'   \item loc_from: Start/From location for the treatment length
#'   \item loc_to: End/To location for the treatment length
#'   \item lane: Lane code for the treatment length
#' }
#' @param df data frame with data to filter for the treatment length. This
#' data frame should contain the following columns:
#' \enumerate{
#'   \item section_id: containing the unique ID for sections on which deficit
#'   data is found
#'   \item loc_from: start location for each deficit observation
#'   \item loc_to: end location for each deficit observation
#'   \item lane: data lane code, or 'all' if the data applies to all lanes
#' }
#' @export
#'
rr_get_tl_data <- function(treat_len, df) {

  req_cols <- c("section_id", "loc_from", "loc_to", "lane")
  .check_required_cols(req_cols, treat_len, "Treatment Length")

  req_cols <- c("section_id", "loc_from", "loc_to", "lane")
  .check_required_cols(req_cols, df, "Data")

  if (!is.data.frame(df)) stop("data for matching should be a data frame")

  section_id <- as.numeric(treat_len[["section_id"]])
  loc_from <- as.numeric(treat_len[["loc_from"]])
  loc_to <- as.numeric(treat_len[["loc_to"]])
  lane_code <- as.character(treat_len[["lane"]])

  if (lane_code == "all") {

    rows <- df[df$section_id == section_id &
                 df$loc_from <= loc_to &
                 df$loc_to >= loc_from, ]
  }
  else {

    rows <- df[df$section_id == section_id &
                 df$loc_from <= loc_to &
                 df$loc_to >= loc_from &
                 (df$lane == "all" | df$lane == lane_code), ]
  }

  return(rows)

}


#' Gets all data for a specific treatment length that has a minimum overlap
#'
#' \code{rr_get_seg_data_ovlp} Filters data and returns only those rows where
#' (a) the section Id matches the section ID of the specified treatment length;
#' (b) the data locations has at least a certain overlap with treatment length
#' locations
#' (c) the data lane code is 'all' or else matches the lane code of the
#' specified treatment length (if the treatment length has lane code 'all', then
#' lane filtering is omitted)
#' @param seg data frame containing information for the specified
#' segment. Required columns are:
#' \enumerate{
#'   \item section_id: ID for the section on which the treatment length is
#'   \item loc_from: Start/From location for the treatment length
#'   \item loc_to: End/To location for the treatment length
#'   \item lane: Lane code for the treatment length
#' }
#' @param df data frame with data to filter for the treatment length. This
#' data frame should contain the following columns:
#' \enumerate{
#'   \item section_id: containing the unique ID for sections on which deficit
#'   data is found
#'   \item loc_from: start location for each deficit observation
#'   \item loc_to: end location for each deficit observation
#'   \item lane: data lane code, or 'all' if the data applies to all lanes
#' }
#' @param min_overlap minimum overlap percent using treatment length as base
#' @returns The row with the maximum overlap, IF the overlap is more than the
#' specified minimum fraction of the length of the treatment. If there are no
#' overlapping rows, or if the maximum overlap is less than the specified
#' minimum overlap, then NULL is returned
#' @export
#'
rr_get_seg_data_ovlp <- function(seg, df, min_overlap = 0.5) {

  rows <- rr_get_tl_data(seg, df)
  n_rows <- nrow(rows)
  if (n_rows > 0) {

    seg_from <- as.numeric(seg[["loc_from"]])
    seg_to <- as.numeric(seg[["loc_to"]])
    length <- seg_to - seg_from

    overlaps <- rr_overlap_lengths(seg_from, seg_to, rows[ ,"loc_from"],
                                   rows[ ,"loc_to"])
    max_overlap_row <- which.max(overlaps)
    max_overlap <- overlaps[max_overlap_row]

    max_overlap_perc <- max_overlap / length
    if (max_overlap_perc > min_overlap) {

      return(rows[max_overlap_row, ])

    }
  }

  return(NULL)

}
