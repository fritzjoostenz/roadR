
#-------------------------------------------------------------------------------
#
#     Utility Tools for filtering and selecting linear element rows that
#     match criteria, e.g. handling lanes etc.
#
#-------------------------------------------------------------------------------

#' Gets all data for a specific treatment length
#'
#' \code{rr_get_tl_data} Filters data and returns only those rows where
#' (a) the section Id matches the section ID of the specified treatment length;
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
#' @param dt data table with data to filter for the treatment length. This
#' data table should contain the following columns:
#' \enumerate{
#'   \item section_id: containing the unique ID for sections on which deficit
#'   data is found
#'   \item loc_from: start location for each deficit observation
#'   \item loc_to: end location for each deficit observation
#'   \item lane: data lane code, or 'all' if the data applies to all lanes
#' }
#' @export
#'
rr_get_tl_data <- function(treat_len, dt) {

  req_cols <- c("section_id", "loc_from", "loc_to", "lane")
  .check_required_cols(req_cols, treat_len, "Treatment Length")

  req_cols <- c("section_id", "loc_from", "loc_to", "lane")
  .check_required_cols(req_cols, dt, "Data")

  tl_lane_code <- tolower(treat_len$lane)
  if (tl_lane_code == "all") {

    rows <- dt[dt$section_id == treat_len$section_id &
                 dt$loc_from <= treat_len$loc_to &
                 dt$loc_to >= treat_len$loc_from, ]

  }
  else {

    rows <- dt[dt$section_id == treat_len$section_id &
                 dt$loc_from <= treat_len$loc_to &
                 dt$loc_to >= treat_len$loc_from &
                 (dt$lane == "all" | dt$lane == treat_len$lane), ]
  }

  return(rows)

}
