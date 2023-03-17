
#' Converts a distress quantity to a percentage
#'
#' \code{rr_get_distress_pct} converts a quantity (either length or area) of
#' observed distress to a percentage value based on the observed quantity and
#' the inspection length.
#'
#' For example, if a 25 m length of cracking was observed in an inspection
#' length ranging from location 50 to 150, then that would equate to 25m distress
#' for an inspection length of 100 m, thus the percentage is 25%.
#'
#' This function does the conversion for all values in a data frame and returns
#' a vector with the percentage values. It only does one distress at a time.
#'
#' @param df data frame with the distress values and inspection start and end
#' locations
#' @param  distress_column name of the column containing the distress to convert
#' @param  insp_from_col name of the column containing the inspection START
#' locations
#' @param  insp_to_col name of the column containing the inspection END
#' locations
#' @param  width_col name of the column containing the width of the inspection
#' area. Only used if parameter \code{area_based} is TRUE.
#' @param  area_based TRUE/FALSE value indicating whether the percentage
#' conversion is area or length based. If area based, then the inspection length
#' is converted to a area using the specified width column
#' @return vector of percentage values, with percentages as 0 to 100+ values.
#' @export
#'
rr_get_distress_pct <- function(df, distress_column,
                             insp_from_col, insp_to_col, width_col = "none",
                             area_based = FALSE) {

  df <- as.data.frame(df) #do this to ensure we can extract cols as vectors

  # Do some checks
  col_names <- names(df)
  cols_to_check <- c(distress_column, insp_from_col, insp_to_col)
  if (area_based == TRUE) cols_to_check <- c(cols_to_check, width_col)

  ok <- .check_required_cols(cols_to_check, df, "visual condition data")
  if (ok == FALSE) stop("Errors in specified parameters, check messages")

  distress_qty <- as.numeric(df[ ,distress_column])

  surv_from <- as.numeric(df[ , insp_from_col])
  surv_to <- as.numeric(df[ , insp_to_col])
  surv_len <- surv_to - surv_from

  if (area_based == FALSE) {
    distress_pct <- distress_qty/surv_len
  }
  else {
    width <- as.numeric(df[ , width_col])
    surv_area <- surv_len * width
    distress_pct <- distress_qty/surv_area
  }

  return(distress_pct * 100)
}


#' Converts a column with pothole counts to a percentage of inspection area
#'
#' \code{rr_get_pothole_pct} converts a pothole count to a percentage value based on the observed quantity and
#' the inspection area. The area each pothole is assumed to represent can be
#' specified
#'
#'
#' @param df data frame with the pothole counts and inspection start and end
#' locations
#' @param  pothole_column name of the column containing the pothole counts
#' @param  pothole_area square metre area assumed for each pothole
#' @param  insp_from_col name of the column containing the inspection START
#' locations
#' @param  insp_to_col name of the column containing the inspection END
#' locations
#' @param  width_col name of the column containing the width of the inspection
#' area
#' @return vector of percentage area values, with percentages as 0 to 100+ values.
#' @export
#'
rr_get_pothole_pct <- function(df, pothole_column, pothole_area,
                                insp_from_col, insp_to_col, width_col) {

  df <- as.data.frame(df) #do this to ensure we can extract cols as vectors

  # Do some checks
  col_names <- names(df)
  cols_to_check <- c(pothole_column, insp_from_col, insp_to_col, width_col)

  ok <- .check_required_cols(cols_to_check, df, "visual condition data")
  if (ok == FALSE) stop("Errors in specified parameters, check messages")

  pothole_count <- as.numeric(df[ ,pothole_column])
  pothole_area <- pothole_count * pothole_area

  surv_from <- as.numeric(df[ , insp_from_col])
  surv_to <- as.numeric(df[ , insp_to_col])
  surv_len <- surv_to - surv_from
  width <- as.numeric(df[ , width_col])
  surv_area <- surv_len * width
  distress_pct <- pothole_area/surv_area

  return(distress_pct * 100)
}
