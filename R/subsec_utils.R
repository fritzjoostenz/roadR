
#' Checks sub-sections and joins ends to start and end of section if close
#'
#' \code{tt_subsecs_join_ends} Checks the set of sub-sections and joins the 
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
tt_subsecs_join_ends <- function(sub_segs, sec_start, sec_end, max_gap = 150) {
  
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
    return(sub_segs)
  }
  else {
    return(sub_segs)
  }
  
  
  
}