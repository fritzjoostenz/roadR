
#' Gets a boolean value from a general value
#'
#' \code{tt_get_boolean} returns TRUE or FALSE based on the following
#' conditions:
#' \enumerate{
#'   \item if value is NULL or NA returns FALSE
#'   \item if value as numeric is 1 returns TRUE
#'   \item if value as numeric is 0 returns FALSE
#'   \item if lowercase of value is any of 'true', 't', 'yes' or 'y':
#'   return TRUE
#'   \item if lowercase of value is any of 'false', 'f', 'no' or 'n':
#'   return FALSE
#'   \item if length of value as character after trimming is 0, return FALSE
#'   \item if none of the above, return TRUE
#' }
#' @param value value (character or number) to evaluate
#' @return TRUE or FALSE
#' @export
#'
tt_get_boolean <- function(value) {
  suppressWarnings({

    if (is.null(value)) return(FALSE)
    if (is.na(value)) return(FALSE)

    txt_value <- tolower(stringr::str_trim(as.character(value), side="both"))
    if (nchar(value) == 0) return(FALSE)

    #put most likely incoming values at top
    if (txt_value == "1") return(TRUE)
    if (txt_value == "true") return(TRUE)

    if (txt_value == "0") return(FALSE)
    if (txt_value == "false") return(FALSE)

    if (txt_value == "no") return(FALSE)
    if (txt_value == "yes") return(TRUE)

    if (txt_value == "t") return(TRUE)
    if (txt_value == "f") return(FALSE)

    if (txt_value == "n") return(FALSE)
    if (txt_value == "y") return(TRUE)


    #If we get here, check for numeric
    num_val <- as.numeric(value)
    if (is.na(num_val)) return(FALSE)
    if (num_val > 0) return(TRUE)

  })
  return(FALSE)
}

#' Does secure lookup on a lookup set
#'
#' \code{tt_get_lookup_value} Finds the value in a required column of a
#' specified lookup table. If either the lookup table key or the lookup column
#' does not exist an informative error is thrown.
#'
#' @param lookups List object containing lookup sets read with method
#' \code{tt_get_lookups}
#' @param lookup_set_key key that identifies the required lookup set. If this
#' key is not found in the names of parameter lookups, an error is thrown
#' @param key_value key that identifies the row on which the value should be
#' retrieved. If the key does not exist, an error is thrown
#' @param key_column column that contains the key on which to look up on. If
#' the column does not exist, an informative error is thrown
#' @param lookup_column column on which to look up they value on. If the column
#' does not exist, an informative error is thrown
#' @export
#'
tt_get_lookup_value <- function(lookups, lookup_set_key, key_value,
                                key_column = "key", lookup_column = "value") {

  if(!lookup_set_key %in% names(lookups)) {
    stop(paste0("Cannot find lookup set '", lookup_set_key, "'"))
  }


  lkpSet <- lookups[[lookup_set_key]]
  if (!key_column %in% names(lkpSet)) {
    stop(paste0("Cannot find key column '", key_column, "' in lookup set '",
                lookup_set_key, "'"))
  }

  if (!lookup_column %in% names(lkpSet)) {
    stop(paste0("Cannot find required column '", lookup_column,
    "' in lookup set '", lookup_set_key, "'"))
  }

  values <- lkpSet[lkpSet[, key_column] == key_value, lookup_column]
  if (is.na(values) || length(values) == 0) {
    stop(paste0("Cannot find value matching key '", key_value,
    "in lookup set '", lookup_set_key, "'"))
  }

  if (length(values) > 1) {
    warning(paste0("More than one value matching key '", key_value,
                   "in lookup set '", lookup_set_key, "'"))
  }

  return(values[[1]])

}

.logmessage <- function(parts, dividers = 0, divider_before = TRUE,
                        log_mode = "detail", log_type = "detail") {

  if (log_mode != log_type) return()

  if (divider_before) {
    if (dividers > 0) {

      for (i in 1:dividers) {
        message(rep("-",80))
      }
    }
    msg <- paste0(parts)
    message(msg)
  }
  else {
    msg <- paste0(parts)
    message(msg)
    if (dividers > 0) {

      for (i in 1:dividers) {
        message(rep("-",80))
      }
    }
  }
}


.check_required_cols <- function(required_cols, df, df_label) {
  cols_on_df <- names(df)
  ierrors <- 0
  for (col in required_cols) {
    if (! col %in% cols_on_df) {
      .logmessage(c("Required column '", col, "' not found in ", df_label,
                  " data;"))
      ierrors <- ierrors + 1
    }
  }
  if (ierrors == 0) return(TRUE)
  return(FALSE)
}

.get_scale_factor = function(scale_factor) {
  result <- switch(
  #Note: for ggplot scales::label_xxx scale factor is inverse
  scale_factor,
  "1"= 1,
  "10k"= 1/10000,
  "100k"= 1/10000,
  "million"= 1/1e6,
  "mil"= 1/1e6,
  "mill"= 1/1e6
 )
  return(result)
}

.get_scale_label = function(scale_factor) {
  result <- switch(
  scale_factor,
  "1"= "",
  "10k"= "(10 thou)",
  "100k"= "(100 thou)",
  "million"= "(million)",
  "mil"= "(million)",
  "mill"= "(million)"
  )
  return(result)
}

.tt_get_string <- function(string_value, to_lower = FALSE) {

  string_value <- stringr::str_trim(string_value, side = "both")
  if (is.na(string_value)) return(NA)
  if (string_value == "") return(NA)
  if (to_lower) string_value = tolower(string_value)
  return(as.character(string_value))
}

.tt_check_column_name_ok <- function(name, context_label, stop_if_false = TRUE) {

  #TODO: Check if name starts with a number

  browser()
  #First check for spaces
  if (grepl(" ", name, fixed = TRUE)) {
    if (stop_if_false) {
      stop("Column name '", name, "' is not valid. Name cannot contain spaces.",
           "Check: ", context_label)
    }
    else {
      return(FALSE)
    }
  }

  #TODO: Freaking not working! Fix
  #Check for special characters
  pattern <- "/|:|?|<|>|$|#|@|!|%|^|&|*|(|)|-|~|`|+|\\*"
  if (grepl(pattern, name)) {
    if (stop_if_false) {
      stop("Column name '", name, "' is not valid. Name cannot special ",
           "characters. Check: ", context_label)
    }
    else {
      return(FALSE)
    }
  }

  return(TRUE)


}

