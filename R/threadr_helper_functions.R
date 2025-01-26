str_to_underscore <- function(x) {
  
  x <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", x)
  x <- gsub(".", "_", x, fixed = TRUE)
  x <- gsub(":", "_", x, fixed = TRUE)
  x <- gsub("\\$", "_", x)
  x <- gsub(" |-", "_", x)
  x <- gsub("__", "_", x)
  x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
  x <- stringr::str_to_lower(x)
  x <- stringr::str_trim(x)
  return(x)
  
}


detect_date_interval <- function(date, skip = 1L, n = 100L, text_return = FALSE) {
  
  # Check
  if (!lubridate::is.POSIXt(date)) {
    cli::cli_abort("`date` must be a POSIXt date.")
  }
  
  # A catch for vectors with fewer elements than skip
  if (length(date) <= skip) skip <- 0L
  
  # Get vectors
  # Skip if needed
  date <- date[skip:length(date)]
  
  # Filter
  date <- utils::head(date, n)
  date_lag <- dplyr::lag(date)
  
  # Calculate difference
  seconds <- difftime(date, date_lag, units = "secs")
  seconds <- as.integer(seconds)
  seconds <- seconds[!is.na(seconds)]
  
  # Most common occurrence
  seconds <- mode_average(seconds, na.rm = TRUE)
  
  if (!text_return) {
    return(seconds)
  } else {
    
    # Default
    period <- "unknown"
    
    # Missing-ness test, when length is one
    if (length(seconds) == 1L && is.na(seconds)) {
      return(period)
    }
    
    # Known periods
    period <- dplyr::case_when(
      all(seconds == 1L) ~ "second",
      all(seconds == 60L) ~ "minute",
      all(seconds == 300L) ~ "five_minute",
      all(seconds == 600L) ~ "ten_minute",
      all(seconds == 900L) ~ "fifteen_minute",
      all(seconds == 1800L) ~ "half_hour",
      all(seconds == 3600L) ~ "hour",
      all(seconds == 86400L) ~ "day",
      all(seconds %in% c(2419200L, 2678400L, 2592000L, 2505600L)) ~ "month",
      .default = "unknown"
    )
    
    # Return text
    return(period)
    
  }
  
}


mode_average <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x <- stats::na.omit(x)
  }
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


str_date_formatted <- function(date = NA, time_zone = TRUE, 
                               fractional_seconds = TRUE) {
  
  # Get date if not supplied
  if (is.na(date)[1]) date <- lubridate::now(tz = Sys.timezone())
  
  # Format string
  format_date <- ifelse(
    fractional_seconds, 
    "%Y-%m-%d %H:%M:%OS3", 
    "%Y-%m-%d %H:%M:%S"
  )
  
  # Format
  x <- format(date, format = format_date, usetz = time_zone)
  
  return(x)
  
}


cli_date <- function() stringr::str_c(str_date_formatted(), ":")
