

#' Trims any short segments by joining it to adjacent segments
#'
#' \code{rr_subsecs_trim_shorts} trims out any segments shorter than a
#' specified length by joining it to one of it's two neighbours. To determine
#' which of the two neighbours are most appropriate to join to, it compares the
#' value in the column '\code{value_column}' and picks the neighbour with the
#' value closest to the current value. A
#'
#' After joining the segment to the most appropriate neighbour, the value in the
#' value column is updating using a length based weighted sum. For example, if
#' the short segment being joined has a length of 50 and a value of 10, and
#' the selected neighbour has a length of 200 and a value of 12, the joined
#' segment will get a value of \code{(50/250)*10 + (200/250)*12 = 11.6}
#'
#' Note that if the set of segments being scanned has only one segment, then no
#' change will be made to the segments, even if the one segment has a length
#' shorter than the minimum allowed (this is because there are no longer
#' neighbours to join the short segment to).
#'
#' @param segments segments from which to remove/join too short segments
#' @param min_length minimum allowed length
#' @param value_column name of the column that contains the score or value that
#' will determine which neighbour is 'most-alike'. The value in this column will
#' be updated based on a weighted sum of the lengths before joining.
#' @return data frame containing the segments will locations and values
#' adjusted so that there are no segments with length below the minimum allowed
#' length
#' @export
#'
rr_subsecs_trim_shorts <- function(segments, min_length, value_column) {

  req_cols <- c("loc_from", "loc_to", value_column)
  .check_required_cols(req_cols, segments, "segments join")

  # if there is not at least 2 rows, then there is nothing to join or trim
  if (nrow(segments) < 2) return(segments)

  new_segments <- .do_join(segments, min_length, value_column)
  new_segments <- new_segments %>% dplyr::select(-c("tmp_length"))

  return(new_segments)

}


.do_join <- function(segs, min_length, value_column) {


  segs$tmp_length <- segs$loc_to - segs$loc_from

  for (i in 1:nrow(segs)) {

    length <- segs[i, "tmp_length"]
    if (!is.na(length)) {
      if (length < min_length) {

        if (i == 1) {
          segs <- .join_to_neighbour(segs, i, "next", value_column)
        }
        else if (i == nrow(segs)) {
          segs <- .join_to_neighbour(segs, i, "prev", value_column)
        }
        else {

          value_prev <- segs[[i-1, value_column]]
          value_this <- segs[[i, value_column]]
          value_next <- segs[[i+1, value_column]]

          diff_prev <- abs(value_prev - value_this)
          diff_next <- abs(value_next - value_this)

          if (diff_prev < diff_next) {
            segs <- .join_to_neighbour(segs, i, "prev", value_column)
          }
          else {
            segs <- .join_to_neighbour(segs, i, "next", value_column)
          }

        }


        segs <- .do_join(segs, min_length, value_column)
      }
    }
  }

  return(segs)
}

.join_to_neighbour <- function(segs, i, which_neighbour, value_column) {

  length_this <-  segs[i, ]$tmp_length
  value_this <-  segs[[i, value_column]]
  if (which_neighbour == "next") {
    seg_from <- segs[i, ]$loc_from
    segs[i + 1, "loc_from"] <- seg_from

    length_next <-  segs[i + 1, ]$tmp_length
    value_next <-  segs[[i + 1, value_column]]
    tot_len <- length_this + length_next
    value_new <- (length_this/tot_len) * value_this +
      (length_next/tot_len)*value_next
    segs[[i + 1, value_column]] <- value_new

  }
  else if (which_neighbour == "prev") {

    seg_to <- segs[i, ]$loc_to
    segs[i - 1, "loc_to"] <- seg_to

    length_prev <-  segs[i - 1, ]$tmp_length
    value_prev <-  segs[[i - 1, value_column]]
    tot_len <- length_prev + length_this
    value_new <-  (length_prev/tot_len)*value_prev +
      (length_this/tot_len) * value_this

    segs[[i - 1, value_column]] <- value_new

  }
  else {
    stop(paste0("which_neighbour value '", which_neighbour, "' is not handled"))
  }

  segs <- segs[-i, ]
  segs$tmp_length <- segs$loc_to - segs$loc_from
  return(segs)

}
