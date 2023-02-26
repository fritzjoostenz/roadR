#-------------------------------------------------------------------------------
#
#     Methods for refining and cleaning a deficit set that will serve as input
#     for the sub-sectioning programe
#
#-------------------------------------------------------------------------------

#' Cleans a deficit input set for sub-sectioning and checks for problems
#'
#' \code{rr_Clean_deficit_set} Checks a prepared input set for sub-sectioning
#' and makes corrections to the start and end locations for each observation.
#'
#' Currently, the primary objective of this function is to ensure that there are
#' no very long observations that will dominate sub-section identification. For
#' example, a flushing area may have a low deficit but if it is 70m long the
#' deficit score (which multiplies the deficit by the length) will push this
#' observation to the top because of its length. If you want to prevent these
#' types of situations, then use a shorter maximum length such as 30m.
#'
#' @param deficit_set data frame with deficit data for the segment. This data frame
#' should contain all distresses or data relating to capacity deficits, and
#' should contain the following columns:
#' \enumerate{
#'   \item section_id: containing the unique ID for sections
#'   \item loc_from: start location for each deficit observation
#'   \item loc_to: end location for each deficit observation
#'   \item deficit: normalised deficit score for each observation. It is
#'   recommended that a scale of 0 to 10 be used, with higher values indicating
#'   greater deficit (distress or capacity deficit)
#'   \item data_code: short code identifying each data type in the set
#' }
#' @param min_obs_length minimum length for an observation. It is strongly
#' recommended that this value be set at 1m. Values of less than one will be
#' corrected in any event by the sub-sectioning algorithm
#' @param max_obs_length maximum length for a deficit observation. This value will
#' ensure that a single observation that has a low deficit but a very long value
#' does not dominate the selection of a sub-section. If a value is longer than this
#' specified maximum length, the from and to locations will be modified so that the
#' observation has the same middle but length equal to the allowed maximum
#' @export
rr_Clean_deficit_set <- function(deficit_set, min_obs_length = 1,
                              max_obs_length = 30) {

  df <- deficit_set
  req_cols <- c("section_id", "loc_from", "loc_to", "deficit", "data_code")
  ok <- .check_required_cols(req_cols, df, "Deficit Data")
  if (ok == FALSE) {
    stop("Some required columns are missing. Check messages in console.")
  }

  #Calculate the middle and length of each observation
  df$tt_middle <- df$loc_from + df$loc_to
  df$tt_length <- df$loc_to - df$loc_from

  # Adjust end location for observations that are too short by adding the minimum
  # length to the start location for cases that are too sort. Leave the others as is.
  df$loc_to <- ifelse(df$tt_length < min_obs_length,
                      df$loc_from + min_obs_length,
                      df$loc_to)


  # Adjust observations that are too long
  # Updated start location: subtract half of max_obs_length from mid-point
  df$loc_from <- ifelse(df$tt_length > max_obs_length,
                        df$tt_middle - max_obs_length/2, df$loc_from)

  # Updated end location: add half of max_obs_length from mid-point
  df$loc_to <- ifelse(df$tt_length > max_obs_length,
                      df$tt_middle + max_obs_length/2, df$loc_to)

  # Re-calculate lengths using updated locations, then check
  df$tt_length <- df$loc_to - df$loc_from
  print(paste0("Minimum length after adjustment is: ", min(df$tt_length)))
  print(paste0("Maximum length after adjustment is: ", max(df$tt_length)))

  df <- df %>% dplyr::select(-c("tt_length", "tt_middle"))

  return(df)
}
