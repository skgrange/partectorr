#' Function to squash R check's global variable notes. 
#' 
#' @name zzz
#' 
if (getRversion() >= "2.15.1") {
  
  # What variables are causing issues?
  variables <- c(
    ".", "variable", "value", "dateTime", "date_end", "date_start", 
    "hardware_version", "key", "priority", "serial_number", "start", "time",
    "variable_clean", "Time"
  )
  
  # Squash the note
  utils::globalVariables(variables)
  
}
