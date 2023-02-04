
#-------------------------------------------------------------------------------
#
#     Utility Tools that implement Sub-Sectioning at the FWP or Network
#     (Section Set) Level. Also methods for applying Heuristics for joining
#     sub-sections or altering its limits
#
#-------------------------------------------------------------------------------

#' Gets optimal subsections on a set of Treatment Lengths
#'
#' \code{rr_get_subsecs_on_sections} Generates a set of optimal sub-sections
#' for each section within a larger set. For example, you can use this function
#' to generate sub-sections for an entire network when the set of sections
#' provided as the first parameter represent all sections on your network.
#'
#' Note: This method DOES provide a full set of sub-segments representing the
#' entire network defined by the set of treatment lengths you provide.
#'
#' Also note that this method, unlike \code{\link{rr_get_subsecs_on_sections}}
#' IS lane specific. If your data and/or treatment lengths are not
#' lane-specific, then use lane code 'all' in your deficit data or treatment
#' length definitions. If your data and treatment length definitions ARE
#' lane-specific, then note that the comparison is case-sensitive!
#'
#' @param treat_lengths data frame containing a list of all treatment lengths
#' on which sub-sectioning should be done. This dataframe should contain the
#' identification columns typical for a JunoViewer FWP export. Required
#' columns are:
#' \enumerate{
#'   \item tl_id: unique ID for each treatment length
#'   \item area_name: unique area name for the treatment length
#'   \item section_id: ID for the section on which the treatment length is
#'   \item loc_from: Start/From location for the treatment length
#'   \item loc_to: End/To location for the treatment length
#'   \item lane: Lane code for the treatment length
#' }
#' @param deficit_data data frame with deficit data for network. This data
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
#' @param min_length minimum sub-section length allowed
#' @param max_length maximum sub-section length allowed
#' @param score_threshold lower threshold below which sub-sections should be
#' ignored
#' @param benefit_scaler scaling factor for deficits. This factor should be
#' manually calibrated using the plotting function
#' @param method method to use for calculating optimal sub-sectioning. Valid
#' values are 'npv' or 'bcr'. The 'npv' method is recommended. 'bcr' tends to
#' give very short sub-sections
#' @param max_subsecs maximum number of sub-sections to allow within each
#' section (default is 25)
#' @param ends_join_limit length within which any sub-sections close to the
#' start or end of a section should be joined to the start or end. Default
#' value is 50m. Pass in a value of -1 if you do NOT want to join sub-sections
#' to the starts of ends.
#' @export
#'
#' @importFrom stats complete.cases
#' @importFrom data.table as.data.table
rr_get_fwp_subsecs <- function(treat_lengths, deficit_data,
                               min_length, max_length,
                               score_threshold, benefit_scaler,
                               method = "npv", max_subsecs = 25,
                               ends_join_limit = 50) {

  req_cols <- c("tl_id", "area_name", "section_id", "loc_from", "loc_to",
                "lane")
  .check_required_cols(req_cols, treat_lengths, "Treatment Lengths")

  req_cols <- c("section_id", "loc_from", "loc_to", "lane", "deficit",
                "data_code")
  .check_required_cols(req_cols, deficit_data, "Deficit Data")

  treat_lengths$length <- treat_lengths$loc_to - treat_lengths$loc_from

  n <- 10000
  # result <- data.table::as.data.table(matrix(NA_real_, nrow = n, ncol = 11))
  # names(result) <- c("loc_from", "loc_to", "length", "score", "index",
  #                    "tl_id", "section_id", "tl_areaname",
  #                    "tl_from", "tl_to", "tl_length")

  result <- data.table::data.table(
    loc_from = rep(NA_real_, n),
    loc_to = rep(NA_real_, n),
    length = rep(NA_real_, n),
    score = rep(NA_real_, n),
    index = rep(NA_real_, n),
    tl_id = rep(NA_real_, n),
    section_id = rep(NA_real_, n),
    tl_areaname = rep(NA_character_, n),
    tl_from = rep(NA_real_, n),
    tl_to = rep(NA_real_, n),
    tl_length = rep(NA_real_, n)
  )

  #Convert to data table for faster filtering
  deficit_data <- as.data.table(deficit_data)

  tl_ids <- treat_lengths$tl_id
  i_from <- 1
  i_to <- 1
  n_sections <- length(tl_ids)
  index <- 1
  prog_index <- 1
  for (id in tl_ids) {

    #if (id == "204570") browser()

    # Get treatment length info so we can add it to the sub-sections generated
    tl_info <- treat_lengths %>% dplyr::filter(.data$tl_id == id)

    # get the deficit data for this treatment length
    tl_data <- rr_get_tl_data(tl_info, deficit_data)

    tl_from <- treat_lengths[treat_lengths$tl_id == id, ]$loc_from
    tl_to <- treat_lengths[treat_lengths$tl_id == id, ]$loc_to
    tl_length <- tl_to - tl_from

    if (tl_length <= min_length) {

      score <- rr_get_deficit_score(tl_data, tl_from, tl_to,
                                    benefit_scaler, method)

      sub_segs <- data.frame(loc_from = c(tl_from), loc_to = c(tl_to),
                             length = c(tl_length), score = c(score),
                             index = c(1))

    }
    else {

      sub_segs <- rr_get_subsections_v1(tl_data, tl_from, tl_to, min_length,
                                        max_length, score_threshold,
                                        benefit_scaler, show_progress = FALSE)

      if (is.null(sub_segs)) {

        score <- rr_get_deficit_score(tl_data, tl_from, tl_to,
                                      benefit_scaler, method)

        sub_segs <- data.frame(loc_from = c(tl_from), loc_to = c(tl_to),
                               length = c(tl_length), score = c(score),
                               index = c(1))

      }
      else {

        if (ends_join_limit > 0) {
          sub_segs <- rr_subsecs_join_ends(sub_segs, tl_from, tl_to,
                                           max_gap = ends_join_limit)
        }

      }
    }

    sub_segs$tl_id <- tl_info$tl_id
    sub_segs$section_id <- tl_info$section_id
    sub_segs$tl_areaname <- tl_info$area_name
    sub_segs$tl_from <- tl_info$loc_from
    sub_segs$tl_to <- tl_info$loc_to
    sub_segs$tl_length <- tl_info$length

    # Fill any gaps between identified optimal sub-segments
    sub_segs <- rr_subsecs_fill_gaps(sub_segs, tl_from, tl_to)

    # Correct for situations where there are overlaps between the optimal
    # identified sub-segments (happens sometimes, rare edge case)
    sub_segs <- .correct_for_overlaps(sub_segs)

    if (id == "203075") browser()
    sub_segs <- rr_subsecs_trim_shorts(sub_segs, min_length, "score")
    sub_segs$length <- sub_segs$loc_to - sub_segs$loc_from  #update lengths

    tl_length <- tl_info$length
    subsecs_length <- sum(sub_segs$length)
    if (tl_length != subsecs_length) {
      stop(paste0("Total length for sub-sections does not match length of ",
      "treatment length for tl_id = ", id))
    }

    n_segs <- nrow(sub_segs)
    i_to <- (i_from + n_segs - 1)

    result[i_from:i_to, ] <- sub_segs
    i_from <- i_to + 1

    if (prog_index == 50) {
      print(sprintf("Working...(now at TL %i of %i)...", index, n_sections))
      prog_index <- 1
    }
    else {
      prog_index <- prog_index + 1
    }

    index <- index + 1
  }

  result <- result[complete.cases(result), ]

  # Check that the sub-sections cover the entire length of the set of
  # treatment lengths on which sub-sectioning is based. This is to double-check
  # that there are no gaps left out
  total_tl_length <- sum(treat_lengths$length)
  total_subsec_length <- sum(result$length)
  if (total_tl_length == total_subsec_length) {
    .logmessage(c("Treatment length set total length matches length of ",
                  "identified sub-sections. All good!"))
  }
  else {
    print(paste0("Warning! Total lengths for sub-sections (",
                 total_subsec_length, ")",
    " does not match total length of treatment lengths (", total_tl_length,")"))
  }

  return(result)

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

  section_id <- as.numeric(sub_seg[["section_id"]])
  loc_from <- as.numeric(sub_seg[["loc_from"]])
  loc_to <- as.numeric(sub_seg[["loc_to"]])
  points_in_seg <- deficit_data[deficit_data$section_id == section_id &
                                  deficit_data$loc_from >= loc_from &
                                  deficit_data$loc_to <= loc_to, ]

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

#' Gets optimal subsections on a set of Sections
#'
#' \code{rr_get_subsecs_on_sections} Generates a set of optimal sub-sections
#' for each section within a larger set. For example, you can use this function
#' to generate sub-sections for an entire network when the set of sections
#' provided as the first parameter represent all sections on your network.
#'
#' Note: This method does NOT provide a full set of segments representing the
#' entire network as defined by the list of Sections you proivide. It only
#' provides a list of optimal sub-sections for the network based on the
#' parameters \code{score_threshold} and \code{benefit_scaler}.
#'
#' Also note that this method, unlike \code{\link{rr_get_fwp_subsecs}} is NOT
#' lane specific.
#'
#' @param sections data frame containing a list of all sections on which
#' sub-sectioning should be done. This dataframe should contain the following
#' columns:
#' \enumerate{
#'   \item section_id: containing the unique ID for sections
#'   \item section_name: name of the section
#'   \item loc_from: start location for each section
#'   \item loc_to: end location for each section
#' }
#' @param deficit_data data frame with deficit data for the network This data
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
#' @param min_length minimum sub-section length allowed
#' @param max_length maximum sub-section length allowed
#' @param score_threshold lower threshold below which sub-sections should be
#' ignored
#' @param benefit_scaler scaling factor for deficits. This factor should be
#' manually calibrated using the plotting function
#' @param method method to use for calculating optimal sub-sectioning. Valid
#' values are 'npv' or 'bcr'. The 'npv' method is recommended. 'bcr' tends to
#' give very short sub-sections
#' @param max_subsecs maximum number of sub-sections to allow within each
#' section (default is 25)
#' @param ends_join_limit length within which any sub-sections close to the
#' start or end of a section should be joined to the start or end. Default
#' value is 50m. Pass in a value of -1 if you do NOT want to join sub-sections
#' to the starts of ends.
#' @export
#'
#' @importFrom stats complete.cases
rr_get_subsecs_on_sections <- function(sections, deficit_data,
                                       min_length, max_length,
                                       score_threshold, benefit_scaler,
                                       method = "npv", max_subsecs = 25,
                                       ends_join_limit = 50) {

  req_cols <- c("section_id", "loc_from", "loc_to", "section_name")
  .check_required_cols(req_cols, sections, "Sections Data")

  req_cols <- c("section_id", "loc_from", "loc_to", "deficit",
                "data_code")
  .check_required_cols(req_cols, deficit_data, "Deficit Data")

  sections$length <- sections$loc_to - sections$loc_from

  n <- 10000
  result <- data.table::as.data.table(matrix(NA_real_, nrow = n, ncol = 10))
  names(result) <- c("loc_from", "loc_to", "length", "score", "index",
                     "section_id", "sec_name", "sec_from", "sec_to",
                     "sec_length")

  section_ids <- sections$section_id
  i_from <- 1
  i_to <- 1
  n_sections <- length(section_ids)
  index <- 1
  for (id in section_ids) {

    # Get section info so we can add it to the sub-sections being generated
    sec_info <- sections %>% dplyr::filter(.data$section_id == id)

    # get the deficit data for this section
    sec_data <- deficit_data %>% dplyr::filter(.data$section_id == id)

    sec_from <- sections[sections$section_id == id, ]$loc_from
    sec_to <- sections[sections$section_id == id, ]$loc_to
    sec_length <- sec_to - sec_from

    if (sec_length <= min_length) {

      score <- rr_get_deficit_score(sec_data, sec_from, sec_to,
                                    benefit_scaler, method)

      sub_segs <- data.frame(loc_from = c(sec_from), loc_to = c(sec_to),
                             length = c(sec_length), score = c(score),
                             index = c(1))

    }
    else {

      sub_segs <- rr_get_subsections_v1(sec_data, sec_from, sec_to, min_length,
                                        max_length, score_threshold,
                                        benefit_scaler, show_progress = FALSE)

      if (is.null(sub_segs)) {

        score <- rr_get_deficit_score(sec_data, sec_from, sec_to,
                                      benefit_scaler, method)

        sub_segs <- data.frame(loc_from = c(sec_from), loc_to = c(sec_to),
                               length = c(sec_length), score = c(score),
                               index = c(1))

      }
      else {

        if (ends_join_limit > 0) {
          sub_segs <- rr_subsecs_join_ends(sub_segs, sec_from, sec_to,
                                           max_gap = ends_join_limit)
        }

      }

    }

    sub_segs$section_id <- id
    sub_segs$sec_name <- sec_info$section_name
    sub_segs$sec_from <- sec_info$loc_from
    sub_segs$sec_to <- sec_info$loc_to
    sub_segs$sec_length <- sec_info$length

    n_segs <- nrow(sub_segs)
    i_to <- (i_from + n_segs - 1)

    result[i_from:i_to, ] <- sub_segs
    i_from <- i_to + 1

    print(sprintf("Finished with section %i of %i; %i sub-sections identified",
                  index, n_sections, nrow(sub_segs)))

    index <- index + 1
  }

  result <- result[complete.cases(result), ]
  return(result)

}


#' Checks sub-sections and joins ends to start and end of section if close
#'
#' \code{rr_subsecs_join_ends} Checks the set of sub-sections and joins the
#' first and last sub-section limits to the start and/or end of the section if
#' these are within range.
#'
#' This function can be used to modify sub-sections by ensuring there are not
#' short sections between a sub-section to be treated and the start or end of
#' a section
#'
#' @param sub_segs data frame with identified sub-sections and scores. This
#' should be the output from method \code{tt_get_subsections_v1}
#' @param sec_start start location for the section
#' @param sec_end end location for the section
#' @param max_gap maximum gap between the start or end of a sub-section and the
#' start/end of the section. Default is 150 m.
#' @return data frame containing the original sub-sections but with adjusted
#' start and ends for the first and last sub-sections if these are within
#' \code{max_gap} of the section start or end
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr .data
rr_subsecs_join_ends <- function(sub_segs, sec_start, sec_end, max_gap = 150) {

  if (nrow(sub_segs) > 0) {
    req_cols <- c("loc_from", "loc_to")
    .check_required_cols(req_cols, sub_segs, "Sub-Segments")

    sub_segs <- sub_segs %>% dplyr::arrange(.data$loc_from)

    n <- nrow(sub_segs)
    sub_segs_min <- sub_segs[1, ]$loc_from
    sub_segs_max <- sub_segs[n, ]$loc_to

    diff <- abs(sub_segs_min - sec_start)
    if (diff > 0 && diff < max_gap) {
      sub_segs[[1, "loc_from"]] <- sec_start
    }

    diff <- abs(sec_end - sub_segs_max)
    if (diff > 0 && diff < max_gap) {
      sub_segs[[n, "loc_to"]] <- sec_end
    }

    sub_segs$length <- sub_segs$loc_to - sub_segs$loc_from

    return(sub_segs)
  }
  else {
    return(sub_segs)
  }



}


#' Fills any gaps between sub-sections to cover entire length
#'
#' \code{rr_subsecs_fill_gaps} fills any gaps between sub-sections, or between
#' the start of a section or treatment length and start of the first
#' sub-section, or between the end of the section or treatment length and the
#' end of the last sub-section.
#'
#' Rows are added to fill the gaps and the score and index values for these
#' 'filler' sub-segments are set to -1.
#'
#' @param sub_segs data frame with identified sub-sections and scores. This
#' should be the output from method \code{tt_get_subsections_v1}
#' @param sec_start start location for the section or treatment length
#' @param sec_end end location for the section or treatment length
#' @return data frame containing the original sub-sections but with added rows
#' to ensure all gaps between sub-sections and start and end for the treatment
#' length or section are covered
#' @export
#'
rr_subsecs_fill_gaps <- function(sub_segs, sec_start, sec_end) {

  req_cols <- c("loc_from", "loc_to")
  .check_required_cols(req_cols, sub_segs, "Sub-Segments")

  sub_segs <- sub_segs %>% dplyr::arrange(.data$loc_from)

  n <- nrow(sub_segs)
  sub_segs_min <- sub_segs[1, ]$loc_from
  sub_segs_max <- sub_segs[n, ]$loc_to

  # handle case where first sub-segment is not at start of section or tl
  # in this case we add a segment that covers the start of the section or tl
  # up to the start of the first sub-segment
  if (sub_segs_min > sec_start) {
    row <- sub_segs[1, ]  #get first sub-segment
    row$loc_from <- sec_start
    row$loc_to <- sub_segs_min
    row$length <- row$loc_to - row$loc_from
    row$score <- -1
    row$index <- -1
    sub_segs <- rbind(sub_segs, row)

  }

  # handle case where last sub-segment is not at end of section or tl
  # in this case we add a segment that covers the end of the last sub-segment
  # up to the end of the section or tl
  if (sub_segs_max < sec_end) {
    row <- sub_segs[n, ] # get last sub-segment
    row$loc_from <- sub_segs_max
    row$loc_to <- sec_end
    row$length <- row$loc_to - row$loc_from
    row$score <- -1
    row$index <- -1
    sub_segs <- rbind(sub_segs, row)

  }

  # sort the sub-segment set again now that we covered the start and end
  sub_segs <- sub_segs %>% dplyr::arrange(.data$loc_from)

  if (nrow(sub_segs) == 1) return(sub_segs)


  # Now loop over all rows and add rows where there are gaps between
  # sub-segments

  n <- 1000
  new_rows <- data.table::as.data.table(matrix(NA_real_, nrow = n,
                                               ncol = ncol(sub_segs)))
  names(new_rows) <- names(sub_segs)

  index <- 0
  # Iterate through the rows of the input data frame
  for (i in 2:nrow(sub_segs)) {

    # Check if there is a gap between the end of the previous row and the start
    # of the current row
    prev_end <- sub_segs[i-1, "loc_to"]
    this_from <- sub_segs[i, "loc_from"]
    if (prev_end < this_from) {

      # If there is a gap, add a new row to the new_rows data frame to cover
      # the gap
      seg_len <- (this_from - prev_end)
      new_row <- sub_segs[i, ] #initially, set to current row
      #now update start and end locations
      new_row$loc_from <- prev_end
      new_row$loc_to <- this_from
      new_row$length <- seg_len
      new_row$score <- -1
      new_row$index <- -1

      index <- index + 1
      new_rows[index, ] <- new_row

    }
  }

  # Discard all NAs
  new_rows <- new_rows[complete.cases(new_rows), ]

  # Bind the new rows to the original data frame
  if (nrow(new_rows) > 0) {
    sub_segs <- rbind(sub_segs, new_rows)
    # Sort the filled data frame by ascending 'start'
    sub_segs <- sub_segs[order(sub_segs$loc_from),]
  }

  return(sub_segs)

}


.correct_for_overlaps <- function(sub_segs) {

  n <- nrow(sub_segs)
  if (n <= 1) {
    return(sub_segs)
  }
  else {

    for (i in 2:n) {

      prev_to <- sub_segs[i-1, "loc_to"]
      this_from <- sub_segs[i, "loc_from"]
      if (prev_to > this_from) {
        ovlp <- prev_to - this_from
        .logmessage(c("Correcting overlap of ", ovlp, "m on identified",
                      " sub-sections"))
        sub_segs[[i-1, "loc_to"]] <- this_from
      }
    }

    sub_segs$length <- sub_segs$loc_to - sub_segs$loc_from

  }

  return(sub_segs)

}
