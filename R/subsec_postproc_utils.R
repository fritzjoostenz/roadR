
#-------------------------------------------------------------------------------
#
#   Utility Functions for Post Processing on Dynamic Segmenting
#
#
#-------------------------------------------------------------------------------

#' Gets the sum of deficit scores in a segment for a set of data_codes
#'
#' \code{rr_get_subseg_defect_sums} gets the sum of the deficit scores within a
#' segment for all data codes within a set of values passed in parameter
#' \code{data_codes_to_include}.
#'
#' Use this function to get the sum of deficit scores for a specific class of
#' defect, e.g. those data_codes mapping to deformation or shear etc. Here,
#' deficit score means the deficit multiplied by the length.
#'
#' @param sub_seg row from sub-sectioning output data frame. This data
#' frame should contain all distresses or data relating to capacity deficits,
#' and should contain the following columns:
#' \enumerate{
#'   \item section_id: containing the unique ID for sections on which deficit
#'   data is found
#'   \item loc_from: start location for each deficit observation
#'   \item loc_to: end location for each deficit observation
#'   \item lane: lane code
#'   \item deficit: normalised deficit score for each observation. It is
#'   recommended that a scale of 0 to 10 be used, with higher values indicating
#'   greater deficit (distress or capacity deficit)
#'   \item data_code: short code identifying each data type in the set
#' }
#' @param deficit_data data frame with deficit data for the network. This data
#' frame should contain all distresses or data relating to capacity deficits,
#' and should contain the following columns:
#' \enumerate{
#'   \item section_id: containing the unique ID for sections on which deficit
#'   data is found
#'   \item loc_from: start location for each deficit observation
#'   \item loc_to: end location for each deficit observation
#'   \item lane: lane code
#'   \item deficit: normalised deficit score for each observation. It is
#'   recommended that a scale of 0 to 10 be used, with higher values indicating
#'   greater deficit (distress or capacity deficit)
#'   \item data_code: short code identifying each data type in the set
#' }
#' @param data_codes_to_include vector containing the data codes to include in
#' the sum.
#'
#' @export
#'
#' @importFrom stats complete.cases
#' @importFrom data.table as.data.table
#' @importFrom dplyr group_by
#' @importFrom dplyr arrange
rr_get_subseg_defect_sums <- function(sub_seg, deficit_data,
                                      data_codes_to_include) {

  req_cols <- c("section_id", "loc_from", "loc_to", "lane",
                "data_code", "deficit")
  ok <- .check_required_cols(req_cols, deficit_data, "Deficit Data")
  if (ok == FALSE) stop("Some required columns are missing")

  points_in_seg <- rr_get_tl_data(sub_seg, deficit_data)
  if (nrow(points_in_seg) == 0) {
    return(0)
  }
  valid_data <- points_in_seg %>%
    dplyr::filter(.data$data_code %in% data_codes_to_include)
  if (nrow(valid_data) == 0) {
    return(0)
  }

  valid_data$deficit_length <- valid_data$loc_to - valid_data$loc_from

  valid_data$score <- valid_data$deficit * valid_data$deficit_length

  result <- sum(valid_data$score, na.rm = TRUE)
  return(result)

}

#' Gets the sum of deficit scores in a segment for a set of data_codes
#'
#' \code{rr_add_subseg_failure_class_sums} gets the deficit sums for specified
#' failure classes associated with each deficit data type, and adds a column for
#' each failure class to the sub_segment data set.
#'
#' This function allows you to get an added column that has, for example, the
#' sum of deficit scores for all data codes associated with a certain failure
#' type such as 'deformation' or 'shear failure'
#'
#' @param sub_segs sub-sectioning output data frame. This data
#' frame should contain all distresses or data relating to capacity deficits,
#' and should contain the following columns:
#' \enumerate{
#'   \item section_id: containing the unique ID for sections on which deficit
#'   data is found
#'   \item loc_from: start location for each deficit observation
#'   \item loc_to: end location for each deficit observation
#'   \item lane: lane code
#'   \item deficit: normalised deficit score for each observation. It is
#'   recommended that a scale of 0 to 10 be used, with higher values indicating
#'   greater deficit (distress or capacity deficit)
#'   \item data_code: short code identifying each data type in the set
#' }
#' @param deficit_data data frame with deficit data for the network. This data
#' frame should contain all distresses or data relating to capacity deficits,
#' and should contain the following columns:
#' \enumerate{
#'   \item section_id: containing the unique ID for sections on which deficit
#'   data is found
#'   \item loc_from: start location for each deficit observation
#'   \item loc_to: end location for each deficit observation
#'   \item lane: lane code
#'   \item deficit: normalised deficit score for each observation. It is
#'   recommended that a scale of 0 to 10 be used, with higher values indicating
#'   greater deficit (distress or capacity deficit)
#'   \item data_code: short code identifying each data type in the set
#' }
#' @param failure_type_lkps data frame containing a lookup set in which each
#' data_code found in the deficit data is associated with a more general
#' failure type. If there are any data codes in the deficit data that are not
#' handled in the failure type lookups, then an error is thrown. This data
#' frame should have columns:
#' \enumerate{#'
#'   \item data_code: short code identifying each data type in the set
#'   \item failure_type: short code representing the more general failure type
#'   associated with each data code
#' }
#'
#' @export
#'
rr_add_subseg_failure_class_sums <- function(sub_segs, deficit_data,
                                      failure_type_lkps) {

  failure_types <- unique(failure_type_lkps$failure_type)

  # Ensre all data codes have a valid failure type lookup
  all_data_codes <- unique(deficit_data$data_code)
  all_lkp_codes <- unique(failure_type_lkps$data_code)
  for (data_code in all_data_codes) {
    if (! data_code %in% all_lkp_codes) {
      stop(paste0("Data code '", data_code, "' is not handled in the ",
                  "failure class lookup data"))
    }
  }

  # now get the deficit summs for all failure types and add columns for each
  i_type <- 1
  n_types <- length(failure_types)

  for (fail_type in failure_types) {

    print(paste0("Getting sums for failure class '", fail_type, "' (",
                 "class ", i_type, " of ", n_types, ")"))
    # Get the data codes associated with this failure type
    tmp <- failure_type_lkps %>%
      dplyr::filter(.data$failure_type == fail_type)

    data_codes_for_type <- unique(tmp$data_code)

    sub_segs[ , fail_type] <- apply(sub_segs, 1, rr_get_subseg_defect_sums,
                                   deficit_data, data_codes_for_type)

    i_type <- i_type + 1
  }

  print("Finished adding failure classification columns with deficit sums")
  return(sub_segs)

}






#' Gets a Breakdown of Defects contributing to Deficit Score
#'
#' \code{rr_get_subseg_defects_breakdown} analyses the deficits within a
#' particular segment based on the 'data_code' for each deficit, and returns
#' a code that that represents the top contributing defects
#'
#' @param sub_seg row from sub-sectioning output data frame. This data
#' frame should contain all distresses or data relating to capacity deficits,
#' and should contain the following columns:
#' \enumerate{
#'   \item section_id: containing the unique ID for sections on which deficit
#'   data is found
#'   \item loc_from: start location for each deficit observation
#'   \item loc_to: end location for each deficit observation
#'   \item lane: lane code
#'   \item deficit: normalised deficit score for each observation. It is
#'   recommended that a scale of 0 to 10 be used, with higher values indicating
#'   greater deficit (distress or capacity deficit)
#'   \item data_code: short code identifying each data type in the set
#' }
#' @param deficit_data data frame with deficit data for the network. This data
#' frame should contain all distresses or data relating to capacity deficits,
#' and should contain the following columns:
#' \enumerate{
#'   \item section_id: containing the unique ID for sections on which deficit
#'   data is found
#'   \item loc_from: start location for each deficit observation
#'   \item loc_to: end location for each deficit observation
#'   \item deficit: normalised deficit score for each observation. It is
#'   recommended that a scale of 0 to 10 be used, with higher values indicating
#'   greater deficit (distress or capacity deficit)
#'   \item data_code: short code identifying each data type in the set
#' }
#' @param benefit_scaler scaling factor for deficits. This factor should be
#' manually calibrated using the plotting function
#' @export
#'
#' @importFrom stats complete.cases
#' @importFrom data.table as.data.table
#' @importFrom dplyr group_by
#' @importFrom dplyr arrange
rr_get_subseg_defects_breakdown <- function(sub_seg, deficit_data,
                                            benefit_scaler) {

  req_cols <- c("section_id", "loc_from", "loc_to", "lane",
                "deficit", "data_code")
  ok <- .check_required_cols(req_cols, deficit_data, "Deficit Data")
  if (ok == FALSE) stop("Some required columns are missing")

  section_id <- as.numeric(sub_seg[["section_id"]])
  loc_from <- as.numeric(sub_seg[["loc_from"]])
  loc_to <- as.numeric(sub_seg[["loc_to"]])
  lane_code <- as.character(sub_seg[["lane"]])

  points_in_seg <- rr_get_tl_data(sub_seg, deficit_data)

  # points_in_seg <- deficit_data[deficit_data$section_id == section_id &
  #                                 deficit_data$loc_from >= loc_from &
  #                                 deficit_data$loc_to <= loc_to, ]

  if (nrow(points_in_seg) == 0) {
    return("none")
  }

  points_in_seg$deficit_length <- points_in_seg$loc_to - points_in_seg$loc_from

  points_in_seg$score <- points_in_seg$deficit * points_in_seg$deficit_length
  points_in_seg$score <- points_in_seg$score * benefit_scaler

  breakdown <- points_in_seg %>% group_by(.data$data_code) %>% dplyr::summarise(
    sum_score = sum(.data$score, na.rm = TRUE)
  )

  tot_score <- sum(breakdown$sum_score)

  breakdown$sum_score_pct <- round(100 * breakdown$sum_score/tot_score, 1)

  breakdown <- breakdown %>% arrange(-.data$sum_score_pct)

  breakdown$cum_pct <- cumsum(breakdown$sum_score_pct)

  n <- nrow(breakdown)
  code <- ""
  for (i in 1:n) {

    code <- paste0(code, breakdown[i, ]$data_code, " = ",
                   breakdown[i, ]$sum_score_pct, " | ")

    if (n > 3 & breakdown[i, ]$cum_pct > 80) {
      break
    }
  }

  return(code)

}
