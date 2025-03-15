#' Function to catch and parse user arguments for the ICOS Cities programmes. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Named list. 
#' 
#' @export
catch_and_return_user_arguments <- function() {
  
  # Catch user arguments
  option_list <- list(
    optparse::make_option(
      c("-c", "--path_config"), 
      action = "store",
      type = "character",
      default = NA_character_, 
      help = "Path of YAML configuration file."
    ),
    optparse::make_option(
      c("-n", "--n_past_days"), 
      action = "store",
      type = "integer",
      default = 30L, 
      help = "How many days in the past should the programme process?"
    ),
    optparse::make_option(
      c("-e", "--extended"), 
      action = "store",
      type = "logical",
      default = FALSE, 
      help = "Should the programme be run with extended operations?"
    )
  )
  
  # To list
  arguments <- optparse::parse_args(
    optparse::OptionParser(option_list = option_list)
  )
  
  return(arguments)
  
}
