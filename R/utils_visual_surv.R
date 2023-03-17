
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

#' Converts a visual distress percentage to approximated local distress areas
#'
#' \code{rr_spread_visual_distress} takes a visual distress over a treatment
#' length and then splits it into approximated individual distress areas. This
#' is needed to incorporate traditional visual distress percentages to an
#' approximate set of local distress areas. This function is needed to utilise
#' the traditional visual assessments in the sub-sectioning tool which uses
#' individual distress locations. It is an approximate solution but better than
#' disregarding visual survey results completely.
#'
#' For example, if a treatment length is from zero to 200m and has 20% of a
#' specific distress, then this function will create \code{n_splits} sub-segments
#' each with a distress that covers 20% of its length. So if \code{n_splits} is
#' 10 (the default), then this function will split this treatment length-wide
#' distress into 10 "distresses", each one covering 20% of the length. Thus,
#' first segment is 0 to 20 m (10th of total length), of which 20% has distress,
#' thus distress assumed to be from 0 to 4. Second is from 20 to 40, thus distress
#' is assumed to be from 20 to 24, etc.
#'
#' The output is a 'long' set containing simulated distress areas within each
#' treatment length, such that the total distress areas matches the original
#' percentage distress. The result set will not contain any observations for
#' treatment lengths where the distress percentage was zero.
#'
#' Note that because of rounding of lengths and sub-lengths, and also because
#' any distress length less than 1 is droppped, the set of simulated distress
#' areas will typically be shorter than the total length of areas based on the
#' percentage of distress on total treatment lengths. Again, this is an
#' approximate method to simulate individual distresses based on estimated total
#' areas of distress.
#'
#' @param segments data frame with the treatment lengths and distress percentages
#' @param  defect_pct_column name of the column containing the defect percentages
#' @param  defect_label label to assign to observations, e.g. "viz_croc_crax"
#' @param  tl_id_col name of the column containing the treatment length ID
#' @param  section_id_column name of the column containing the section IDs
#' @param  area_name_column name of the column containing the treatment length
#' area names
#' @param  lane_column name of the column containing the lane codes
#' @param  loc_from_col name of the column containing the start locations for each
#' treatment length
#' @param  loc_to_col name of the column containing the end locations for each
#' treatment length
#' @param  n_splits number of splits to do on each treatment length
#' @param  min_split_length minimum length below which treatment length will not
#' be split (instead, length will be shortened to match distress area)
#' @return vector of percentage area values, with percentages as 0 to 100+ values.
#' @export
#'
rr_spread_visual_distress <- function(segments, defect_pct_column, defect_label,
                           tl_id_col, section_id_column, area_name_column,
                           lane_column, loc_from_col, loc_to_col,
                           n_splits = 10, min_split_length = 20) {

  n_segs <- nrow(segments)
  if (n_segs == 0) return(segments)

  n <- 100000
  tl_ids <- rep(NA, n)
  section_ids <- rep(NA, n)
  area_names <- rep(NA, n)
  lanes <- rep(NA, n)
  defect_froms <- rep(NA, n)
  defect_tos <- rep(NA, n)
  defects <- rep(NA, n)

  i_defect <- 1
  for (i_seg in 1:n_segs) {


    tl_id <- segments[[i_seg, tl_id_col]]
    section_id <- segments[[i_seg, section_id_column]]
    area_name <- segments[[i_seg, area_name_column]]
    lane_code <- segments[[i_seg, lane_column]]

    defect_pct <- as.numeric(segments[[i_seg, defect_pct_column]])

    if (defect_pct > 0) {

      loc_from <- as.numeric(segments[[i_seg, loc_from_col]])
      loc_to <- as.numeric(segments[[i_seg, loc_to_col]])

      length <- loc_to - loc_from


      if (length <= min_split_length) {

        sub_len <- length
        defect_len <- floor(sub_len * (defect_pct)/100)

        if (defect_len > 1) {

          defect_from <- loc_from
          defect_to <- loc_from + defect_len

          tl_ids[i_defect] <- tl_id
          section_ids[i_defect] <- section_id
          area_names[i_defect] <- area_name
          lanes[i_defect] <- lane_code

          defect_froms[i_defect] <- defect_from
          defect_tos[i_defect] <- defect_to
          defects[i_defect] <- defect_label
          i_defect <- i_defect + 1

        }

      }
      else {

        sub_from <- loc_from

        sub_len <- floor(length/n_splits)
        defect_len <- floor(sub_len * (defect_pct)/100)

        if (defect_len > 1) {

          while (sub_from < loc_to) {

            defect_from <- sub_from
            defect_to <- min(loc_to, sub_from + defect_len)

            tl_ids[i_defect] <- tl_id
            section_ids[i_defect] <- section_id
            area_names[i_defect] <- area_name
            lanes[i_defect] <- lane_code

            defect_froms[i_defect] <- defect_from
            defect_tos[i_defect] <- defect_to
            defects[i_defect] <- defect_label

            i_defect <- i_defect + 1

            sub_from <- sub_from + sub_len
          }

        }

      }

      if (i_defect > n) stop("max number of rows allowed is exceeded")

    }
  }

  result <- data.frame(tl_id = tl_ids, section_id = section_ids,
                       area_name = area_names,
                       lane = lanes,
                       loc_from = defect_froms,
                       loc_to = defect_tos,
                       defect = defects)

  result <- result[complete.cases(result), ]

  return(result)
}
