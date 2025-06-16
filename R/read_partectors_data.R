#' Function to read the Naneos Partector 2 ultrafine particle (UFP) monitor 
#' data files. 
#' 
#' @author Stuart K. Grange
#' 
#' @param file A vector of Naneos Partector 2 ultrafine particle monitor data
#' files. The files can be the monitor's instrument logs, an export from 
#' Nanoes's cloud database, or the Java tool export. 
#' 
#' @param tz_in_file What time zone are the dates represented in the data files? 
#' 
#' @param as_long Should the data files be returned in a "long" format? 
#' 
#' @param variable_switch Should the data files' names be switched to more 
#' universal versions? 
#' 
#' @param priority_variables If \code{as_long} and \code{variable_switch} are
#' \code{TRUE}, should only "priority" variables be returned? 
#' 
#' @param serial_number_as_character Should the `serial_number` variable be
#' returned as a character vector?  
#' 
#' @param date_calibration_as_numeric should the `date_calibration` variable 
#' be returned as a numeric vector?  
#' 
#' @param names_to An optional character vector to add the file name of the data
#' file to the tibble return. 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @param progress Should a progress bar be displayed? 
#' 
#' @return Tibble. 
#' 
#' @export
read_partectors_data <- function(file, tz_in_file = "UTC", as_long = TRUE, 
                                 variable_switch = FALSE, 
                                 priority_variables = FALSE, 
                                 serial_number_as_character = FALSE,
                                 date_calibration_as_numeric = FALSE,
                                 names_to = rlang::zap(), verbose = FALSE, 
                                 progress = FALSE) {
  
  # Read each file in turn
  df <- file %>% 
    purrr::set_names(.) %>% 
    purrr::map(
      ~read_partectors_data_worker(
        ., 
        tz_in_file = tz_in_file,
        as_long = as_long, 
        variable_switch = variable_switch, 
        priority_variables = priority_variables,
        serial_number_as_character = serial_number_as_character,
        date_calibration_as_numeric = date_calibration_as_numeric,
        verbose = verbose
      ), 
      .progress = progress
    ) %>% 
    purrr::list_rbind(names_to = names_to)
  
  # Arrange the observations, different variables to arrange on depending on the 
  # format choice
  if (!as_long) {
    df <- df %>% 
      arrange(serial_number,
              date)
  } else {
    df <- df %>% 
      arrange(serial_number,
              variable,
              date)
  }
  
  return(df)
  
}


read_partectors_data_worker <- function(file, tz_in_file, as_long, 
                                        variable_switch, priority_variables, 
                                        serial_number_as_character, 
                                        date_calibration_as_numeric, verbose) {
  
  # Print file name if desired
  if (verbose) {
    cli::cli_alert_info("{cli_date()} `{file}`...")
  }
  
  # Read file as text
  text <- readr::read_lines(file, progress = FALSE)
  
  # If the file is empty, this has been seen
  if (length(text) == 0L) {
    return(tibble())
  }
  
  # Where does the file's preamble end?
  index_end_preamble <- stringr::str_which(text, "^time")
  
  # If the preamble is incomplete, return nothing
  if (length(index_end_preamble) == 0L) {
    return(tibble())
  }
  
  # File format tests
  file_type <- dplyr::case_when(
    length(index_end_preamble) != 0L ~ "instrument_log",
    stringr::str_detect(text[3], "dateTime") ~ "nanos_cloud_export", 
    stringr::str_detect(text[1], "java tool") ~ "java_tool_export", 
    .default = NA_character_
  )
  
  # Handle the different file formats
  if (file_type == "instrument_log") {
    df <- format_partectors_instrument_log(
      text,
      index_end_preamble = index_end_preamble,
      tz_in_file = tz_in_file,
      date_calibration_as_numeric = date_calibration_as_numeric
    )
  } else if (file_type == "nanos_cloud_export") {
    df <- format_partectors_cloud_export(text)
  } else if (file_type == "java_tool_export") {
    df <- format_partectors_java_tool_export(text, tz_in_file = tz_in_file)
  }
  
  # An identifier to character if desired
  if (serial_number_as_character) {
    df <- mutate(df, serial_number = as.character(serial_number))
  }

  # Calculate date_end, might need to be enhanced further, but the instrument 
  # waits for two seconds between measurement cycles
  measurement_duration <- df %>% 
    select(date) %>% 
    pull() %>% 
    detect_date_interval()
  
  # Test
  if (!is.na(measurement_duration) && 
      !measurement_duration %in% c(1L, 2L, 4L, 6L, 10L, 18L, 34L)) {
    cli::cli_alert_warning(
      "`{file}` contains a non-standard measurement duration..."
    )
  }
  
  # Test for sub second dates
  if (has_sub_seconds(df$date)) {
    cli::cli_alert_warning("`{file}` contains sub-second date accuracy...")
  }
  
  # Calculate date end
  df <- df %>% 
    mutate(date_end = date + (!!measurement_duration - 2)) %>% 
    relocate(date_end,
             .after = date)
  
  if (as_long) {
    
    # Make longer data
    df <- tidyr::pivot_longer(
      df, -monitor_type:-date_end, names_to = "variable", values_drop_na = TRUE
    )
    
    # Make better names for variables
    if (variable_switch) {
      # Join better names and drop old names
      df <- df %>% 
        left_join(partectors_variables_look_up(), by = join_by(variable)) %>% 
        select(-variable) %>% 
        relocate(variable = variable_clean, 
                 .after = date_end)
      
      # Filter and/or drop non-priority variables
      if (priority_variables) {
        df <- df %>% 
          filter(priority) %>% 
          select(-priority)
      } else {
        df <- select(df, -priority)
      }
    }
    
  }
  
  return(df)
  
}


format_partectors_instrument_log <- function(text, index_end_preamble, 
                                             tz_in_file,
                                             date_calibration_as_numeric) {
  
  # Filter to preamble text
  text_preamble <- text[1:(index_end_preamble - 2L)]
  
  # Format preamble
  df_preamble <- format_partectors_data_preamble(
    text_preamble, 
    tz_in_file = tz_in_file,
    date_calibration_as_numeric = date_calibration_as_numeric
  )
  
  # Get start date
  date_start <- df_preamble$date_start
  
  # Drop a variable
  df_preamble <- select(df_preamble, -date_start)
  
  # Parse tabular data, parse date, and add identifiers, warning suppression
  # is for bad new line characters seen in some files
  suppressWarnings(
    df <- text[index_end_preamble:length(text)] %>% 
      readr::read_table(progress = FALSE, guess_max = 10000L) %>% 
      mutate(date = time + !!date_start) %>% 
      select(-time) %>% 
      dplyr::bind_cols(df_preamble) %>% 
      relocate(!!names(df_preamble),
               date)
  )
  
  return(df)
  
}


format_partectors_data_preamble <- function(text, tz_in_file, 
                                            date_calibration_as_numeric) {
  
  # Get monitor type and serial number
  monitor_type <- stringr::str_subset(text, "(?i)sn") %>% 
    stringr::str_split_1(" ") %>% 
    .[-2]
  
  # Format the first element
  monitor_type[1] <- str_to_underscore(monitor_type[1])
  
  # Get serial number
  serial_number <- as.integer(monitor_type[2])
  
  # Get calibration date, time zone does not matter too much here
  date_calibration <- text %>% 
    stringr::str_subset("calibrated on") %>% 
    stringr::str_split_1("on ") %>% 
    .[2] %>% 
    lubridate::dmy(tz = "UTC")
  
  # To numeric if desired
  if (date_calibration_as_numeric) {
    date_calibration <- as.numeric(date_calibration)
  }
  
  # Get the key identifiers, the start date does not encode time zone
  matrix_identifiers <- text %>% 
    stringr::str_subset("(?i)firmware|hardware|start:") %>% 
    stringr::str_split_fixed(": ", 2) %>% 
    apply(2, stringr::str_squish)
  
  # Clean names and values in matrix
  matrix_identifiers[, 1] <- str_to_underscore(matrix_identifiers[, 1])
  
  matrix_identifiers[1:3, 1] <- stringr::str_c(
    matrix_identifiers[1:3, 1], "_version"
  )
  
  matrix_identifiers[, 2] <- stringr::str_remove(matrix_identifiers[, 2], "^rev ")
  
  # Make a single row tibble
  df <- matrix_identifiers %>% 
    tibble::as_tibble(.name_repair = "minimal") %>% 
    purrr::set_names(c("key", "value")) %>% 
    tidyr::pivot_wider(names_from = key) %>% 
    rename(date_start = start) %>% 
    mutate(monitor_type = !!monitor_type[1],
           serial_number = !!serial_number,
           date_calibration = !!date_calibration) %>% 
    mutate(date_start = lubridate::dmy_hms(date_start, tz = tz_in_file),
           date_start = lubridate::with_tz(date_start, tz = "UTC")) %>% 
    relocate(monitor_type,
             serial_number,
             hardware_version) %>% 
    relocate(date_calibration,
             .before = date_start)
  
  return(df)
  
}


format_partectors_cloud_export <- function(text) {
  
  # Get identifiers from the first line of the data file
  identifiers <- stringr::str_split_1(text[1], " SN ")
  
  # Format identifiers
  monitor_type <- stringr::str_to_lower(identifiers[1])
  serial_number <- as.integer(identifiers[2])
  
  # Parse data table, dates will be represented in UTC and assuming the dates
  # are the start date
  df <- text[-1] %>% 
    I() %>% 
    readr::read_csv(show_col_types = FALSE, progress = FALSE) %>% 
    rename(date = dateTime) %>% 
    mutate(monitor_type = !!monitor_type,
           serial_number = !!serial_number) %>% 
    relocate(monitor_type,
             serial_number)
  
  return(df)
  
}


format_partectors_java_tool_export <- function(text, tz_in_file) {
  
  # Get identifier line
  identifiers <- stringr::str_subset(text, "partector")[2]
  
  # Format serial number
  serial_number <- identifiers %>% 
    stringr::str_split_1("partector|running") %>% 
    .[2] %>% 
    as.integer()
  
  # Get firmware version
  firmware_version <- identifiers %>% 
    stringr::str_split_1("firmware|\\]") %>% 
    .[2] %>% 
    stringr::str_squish()
  
  # Get start date
  date_start_identifiers <- text %>% 
    .[1:10] %>% 
    stringr::str_subset("File start") %>% 
    stringr::str_split_fixed(":", n = 2) %>% 
    .[, 2] %>% 
    stringr::str_remove("]") %>% 
    stringr::str_squish()
  
  # Combine date and times
  date_start <- stringr::str_c(
    date_start_identifiers[1], date_start_identifiers[2], sep = " "
  )
  
  # Parse date
  date_start <- lubridate::dmy_hms(date_start, tz = tz_in_file)
  
  # Parse tabular data and add a few variables, warning suppression is for 
  # too many columns in the file
  suppressWarnings(
    df <- text[-1:-5] %>% 
      stringr::str_remove("\t$") %>% 
      I() %>% 
      readr::read_delim(
        delim = "\t",
        locale = readr::locale(decimal_mark = ","), 
        show_col_types = FALSE, 
        progress = FALSE
      ) %>% 
      select(-dplyr::matches("^X20")) %>% 
      rename(date = Time) %>% 
      mutate(date = date + !!date_start,
             monitor_type = "partector2",
             serial_number = !!serial_number,
             firmware_version = !!firmware_version) %>% 
      relocate(monitor_type,
               serial_number,
               firmware_version)
  )
  
  return(df)
  
}


partectors_variables_look_up <- function(select = TRUE) {
  
  # The look-up table
  df <- tibble::tribble(                                                                                                                                                                                    
    ~file_type,                     ~variable,               ~variable_clean,                    ~unit,         ~priority, ~decimal_digits,
    "instrument_log; cloud_export", "number",                "particle_number",                  "#.cm-3",      1,         0,              
    "instrument_log",               "diam",                  "particle_diameter",                "nm",          1,         0,              
    "instrument_log",               "LDSA",                  "lung_deposited_surface_area",      "um2.cm-3",    1,         2,              
    "instrument_log",               "surface",               "total_surface_area",               "um2.cm-3",    1,         1,              
    "instrument_log; cloud_export", "mass",                  "pm0.3",                            "ug.m-3",      1,         2,              
    "instrument_log",               "sigma",                 "particle_size_distribution_sigma", NA_character_, 0,         NA_real_,       
    "instrument_log",               "idiff",                 "diffusion_current",                "na",          0,         NA_real_,       
    "instrument_log",               "ucor",                  "ucor",                             NA_character_, 0,         NA_real_,       
    "instrument_log",               "DV",                    "deposition_voltage",               "volts",       1,         0,              
    "instrument_log; cloud_export", "T",                     "temperature_device",               "degrees.c",   1,         0,              
    "instrument_log; cloud_export", "RH",                    "relative_humidity_device",         "percent",     1,         1,              
    "instrument_log",               "P",                     "absolute_pressure",                "mbar",        1,         1,              
    "instrument_log",               "flow",                  "flow_rate",                        "l.min-1",     1,         3,              
    "instrument_log",               "bat",                   "battery",                          "volts",       1,         2,              
    "instrument_log",               "Ipump",                 "pump_current",                     "ma",          1,         2,              
    "instrument_log",               "error",                 "error_status",                     NA_character_, 1,         NA_real_,       
    "instrument_log",               "PWMpump",               "pump_control_signal",              "percent",     0,         NA_real_,       
    "instrument_log",               "steps",                 "algorithm_iteration_count",        NA_character_, 0,         NA_real_,       
    "instrument_log",               "n10.00",                "pnc_10_nm",                        "dN/dlogD",    1,         0,              
    "instrument_log",               "n16.26",                "pnc_16.26_nm",                     "dN/dlogD",    1,         0,              
    "instrument_log",               "n26.43",                "pnc_26.43_nm",                     "dN/dlogD",    1,         0,              
    "instrument_log",               "n42.96",                "pnc_42.96_nm",                     "dN/dlogD",    1,         0,              
    "instrument_log",               "n69.83",                "pnc_69.83_nm",                     "dN/dlogD",    1,         0,              
    "instrument_log",               "n113.52",               "pnc_113.52_nm",                    "dN/dlogD",    1,         0,              
    "instrument_log",               "n184.55",               "pnc_184.55_nm",                    "dN/dlogD",    1,         0,              
    "instrument_log",               "n300.00",               "pnc_300_nm",                       "dN/dlogD",    1,         0,              
    "instrument_log",               "A1",                    "a1_electrometer_amplitude",        "mv",          0,         NA_real_,       
    "instrument_log",               "A2",                    "a2_electrometer_amplitude",        "mv",          0,         NA_real_,       
    "instrument_log",               "A3",                    "a3_electrometer_amplitude",        "mv",          0,         NA_real_,       
    "instrument_log",               "A4",                    "a4_electrometer_amplitude",        "mv",          0,         NA_real_,       
    "instrument_log",               "A5",                    "a5_electrometer_amplitude",        "mv",          0,         NA_real_,       
    "instrument_log",               "calcflo",               "calcflo",                          NA_character_, 0,         NA_real_,       
    "nanos_cloud_export",           "diameter",              "particle_diameter",                "nm",          1,         0,              
    "nanos_cloud_export",           "ldsa",                  "lung_deposited_surface_area",      "um2.cm-3",    1,         2,              
    "nanos_cloud_export",           "particle_number_10nm",  "pnc_10_nm",                        "dN/dlogD",    1,         0,              
    "nanos_cloud_export",           "particle_number_114nm", "pnc_113.52_nm",                    "dN/dlogD",    1,         0,              
    "nanos_cloud_export",           "particle_number_16nm",  "pnc_16.26_nm",                     "dN/dlogD",    1,         0,              
    "nanos_cloud_export",           "particle_number_185nm", "pnc_184.55_nm",                    "dN/dlogD",    1,         0,              
    "nanos_cloud_export",           "particle_number_26nm",  "pnc_26.43_nm",                     "dN/dlogD",    1,         0,              
    "nanos_cloud_export",           "particle_number_300nm", "pnc_300_nm",                       "dN/dlogD",    1,         0,              
    "nanos_cloud_export",           "particle_number_43nm",  "pnc_42.96_nm",                     "dN/dlogD",    1,         0,              
    "nanos_cloud_export",           "particle_number_70nm",  "pnc_69.83_nm",                     "dN/dlogD",    1,         0,              
    "nanos_cloud_export",           "status",                "error_status",                     NA_character_, 1,         NA_real_,       
    "java_tool_export",             "p",                     "absolute_pressure",                "mbar",        1,         1,              
    "java_tool_export",             "altitude",              "elevation",                        "m",           0,         1
  ) %>% 
    mutate(priority = as.logical(priority))
  
  # Only keep the variables used for joining
  if (select) {
    df <- select(df, variable, variable_clean, priority)
  }
  
  return(df)
  
}
