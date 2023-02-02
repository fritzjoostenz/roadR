
#-------------------------------------------------------------------------------
#
#     Methods for finding optimal sub-sections using the Deficit-Score concept
#     and a graded grid search (looping) to find optimal sub-sections
#
#-------------------------------------------------------------------------------

#' Gets optimal subsections using Looping method
#'
#' \code{rr_get_subsections_v1} Searches for the optimal subsections within a
#' section This version uses a sequential looping method starting with a large
#' search grid and then going sequentially smaller. Gives roughly the same
#' results as Genetic Algorithm but more consistent and potentially faster
#'
#' @param df data frame with deficit data for the segment. This data frame
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
#' @param section_min minimum location for the segment
#' @param section_max maximum location for the segment
#' @param min_length minimum sub-section length allowed
#' @param max_length maximum sub-section length allowed
#' @param score_threshold lower threshold below which sub-sections should be
#' ignored
#' @param benefit_scaler maximum location for the segment
#' @param method method to use for calculating optimal sub-sectioning. Valid
#' values are 'npv' or 'bcr'. The 'npv' method is recommended. 'bcr' tends to
#' give very short sub-sections
#' @param max_subsecs maximum number of sub-sections to allow within the
#' section (default is 25)
#' @param show_progress TRUE/FALSE flag determining if messages should show the
#' search progress. Set this to FALSE if you are calling this method in a loop
#' @return TRUE or FALSE
#' @export
#'
#' @importFrom stats complete.cases
rr_get_subsections_v1 <- function(df, section_min, section_max,
                               min_length, max_length, score_threshold,
                               benefit_scaler, method = "npv",
                               max_subsecs = 25, show_progress = TRUE) {

  req_cols <- c("section_id", "loc_from", "loc_to", "deficit", "data_code")
  .check_required_cols(req_cols, df, "Deficit Data")

  sec_length <- (section_max - section_min)

  # We need to handle cases where the deficit data starts before or after the
  # section min and max. If we do not do this, then defects that start well
  # before the section start and overlaps only a little, will add undue weight
  # to the total deficit score:
  df$loc_from <- ifelse(df$loc_from < section_min, section_min, df$loc_from)
  df$loc_to <- ifelse(df$loc_to > section_max, section_max, df$loc_to)

  #Set the deficit length, and assign 1 if the length is zero (e.g. MSD data)
  df$deficit_length <- df$loc_to - df$loc_from
  df$deficit_length <- ifelse(df$deficit_length == 0, 1, df$deficit_length)

  loc_froms <- rep(NA, 100)
  loc_tos <- rep(NA, 100)
  lengths <- rep(NA, 100)
  scores <- rep(NA, 100)

  for (i in 1:max_subsecs) {

    result <- .get_sub_section(df, min_length = min_length,
                               max_length = max_length,
                              benefit_scaler = benefit_scaler,
                              method = method)

    this_score <- result$score

    # Because of the way looper works, we may end up with a sub-section that
    # starts before the start, or ends after the end, of the section. Correct
    # for that while extracting the data
    loc_froms[i] <- max(section_min, result$loc_from)
    loc_tos[i] <- min(section_max, result$loc_to)
    lengths[i] <- result$loc_to - result$loc_from
    scores[i] <- this_score

    if (i == 1) {
      best_score <- this_score
      score_threshold <- best_score - (best_score * 0.7)
    }

    if (this_score <= score_threshold) {
        loc_froms[i] <- NA
        loc_tos[i] <- NA
        lengths[i] <- NA
        scores[i] <- NA
        break
    }
    else {
      if (show_progress) {
        print(sprintf("Subsec %i; from %i to %i; score = %.2f;", i,
                      result$loc_from, result$loc_to, result$score))
      }
    }

    df <- result$data

  }

  segs <- data.frame(loc_from = loc_froms, loc_to = loc_tos, length = lengths,
                     score = scores)
  segs <- segs[complete.cases(segs),]
  if (nrow(segs) > 0) {
    segs$index <- 1:nrow(segs)
    return(segs)
  }
  else {
    return(NULL)
  }


}


#' Gets the Deficit Score for a specific sub-section
#'
#' \code{rr_get_deficit_score} Allows you to manually calculate the Deficit
#' Score for a specific sub-section.
#'
#' @param df data frame with deficit data for the section
#' @param loc_from start location for the sub-section
#' @param loc_to end location for the sub-section
#' @param benefit_scaler maximum location for the segment
#' @param method method to use for calculating optimal sub-sectioning. Valid
#' values are 'npv' or 'bcr'. The 'npv' method is recommended. 'bcr' tends to
#' give very short sub-sections
#' @return score for the sub-section limits
#' @export
rr_get_deficit_score <- function(df, loc_from, loc_to,
                                 benefit_scaler, method = "npv") {

  #Set the deficit length, and assign 1 if the length is zero (e.g. MSD data)
  df$deficit_length <- df$loc_to - df$loc_from
  df$deficit_length <- ifelse(df$deficit_length == 0, 1, df$deficit_length)

  length <- loc_to - loc_from
  x <- c(loc_from, length)
  score <- .get_score(df, x, method, benefit_scaler)
  return(score)

}


#-------------------- Local Methods not Exported               -----------------

.get_score <- function(df, x, method, benefit_scaler) {

  ch_start <- floor(x[1])
  length <- floor(x[2])
  ch_end <-ch_start + length

  points_in_seg <- df[df$loc_from >= ch_start & df$loc_to <= ch_end, ]

  if (nrow(points_in_seg) == 0) return(0)

  tot_deficit <- (points_in_seg$deficit * points_in_seg$deficit_length)
  tot_deficit <- sum(tot_deficit)

  benefit <- tot_deficit * benefit_scaler

  cost <- length

  if (method == "bcr") {
    bcr <- benefit/cost
    return(bcr/benefit_scaler)
  }
  else {
    npv <- (benefit - cost)/benefit_scaler
    return(npv)
  }
}


.solve_looper <- function(df, min_length, max_length,
                          method = "npv", benefit_scaler = 1) {

  ch_min <- min(df$loc_from)
  ch_max <- max(df$loc_to)

  if (ch_min == ch_max) {
    result <- list(x = c(ch_min, 0), score = -999)
    return(result)
  }

  solve <- function(ch_min, ch_max, min_length, max_length, increm) {

    lengths <- seq(from = min_length, to = max_length, by = increm)
    starts <- seq(from = ch_min, to = ch_max, by = increm)

    best_x <- c(-Inf, -Inf)
    best_score <- -Inf

    for (length in lengths) {
      for (start in starts) {

        x <- c(start, length)
        score <- .get_score(df, x, method, benefit_scaler)
        if (is.na(score)) score <- -Inf
        if (score > best_score) {

          best_score <- score
          best_x <- x

        }
      }
    }

    result <- list(x = best_x, score = best_score)
    return(result)
  }

  increms <- c(min_length, min_length/5, max(min_length/10,5))
  for (increm in increms) {

    result <- solve(ch_min, ch_max, min_length, max_length, increm)

    ch_min <- result$x[1] - increm/2
    ch_max <- ch_min + result$x[2] + increm/2

  }

  return(result)

}


.get_sub_section <- function(df, min_length, max_length,
                             benefit_scaler, method = "npv") {
  if (nrow(df) == 0)  {
    result <- list(loc_from = 0, loc_to = 0,
                   score = -999, data = df)
    return(result)
  }

  ch_max <- max(df$loc_to)

  looper <- .solve_looper(df, min_length, max_length, method, benefit_scaler)
  x1 = looper$x[1]
  x2 = looper$x[2]
  score <- looper$score

  loc_from <- max(0, floor(x1))
  length <- floor(x2)
  loc_to <- min(ch_max, loc_from + length)

  rows_in_seg <- df[df$loc_from >= loc_from & df$loc_to <= loc_to, ]
  loc_froms <- unique(rows_in_seg$loc_from)
  df$deficit <- ifelse(df$loc_from %in% loc_froms, 0, df$deficit)

  result <- list(loc_from = loc_from, loc_to = loc_to,
                 score = score, data = df)
  return(result)
}
