# read non-Freestyle Libre files


#' @title Transform Raw Nutrisense into Canonical Glucose Format
#' @param raw_data dataframe pulled from the Nutrisense file
#' @param tz character string for time zone
#' @return dataframe canonical Glucose format
xform_nutrisense <- function(raw_data, tz) {
  result <- raw_data %>% # col_types = "cccdddddcddddcddddd") %>%
    filter(class %in% c("GlucoseMeasurement", "Meal")) %>% arrange(`occurred_at`) %>%
    transmute(value = if_else(!is.na(`value`), `value`,lag(`value`)),
              time = lubridate::mdy_hm(`occurred_at`, tz = `tz`),
              food = ifelse(!is.na(`description`), paste0("Notes=",`description`), NA),
              scan = `value`,
              hist = `value`)

  return(result)
}

#' @title Raw Dataframe from Nutrisense Filepath
#' @param filepath path to a Nutrisense CSV file
#' @param skip_lines integer lines to skip while reading
#' @return dataframe raw nutrisense data format
nutrisense_raw <- function(filepath = system.file("extdata", package = "cgmr", "Firstname3Lastname3_nutrisense.csv"),
                           skip_lines = 1) {
  readr::read_csv(filepath, skip = skip_lines,
                  col_types = cols(
                    class = col_character(),
                    value = col_double(),
                    time = col_character(),
                    length = col_double(),
                    photo_url = col_character(),
                    description = col_character(),
                    occurred_at = col_character(),
                    body = col_character(),
                    updated_at = col_character(),
                    started_at = col_character(),
                    ended_at = col_character(),
                    created_by = col_character()
                  ))
}



#' @title read a Nutrisense export file and convert to the PSI CGM format
#' @description Nutrisense export files contain timestamped information about
#' glucose values, meals, exercise, sleep, and more.
#' Important: This function expects an extra line at the top of the Nutrisense raw file.
#' The first line must say "Nutrisense" and include the full name of the person.
#' @param filepath path to a valid Nutrisense data file
#' @param user_id new user ID to be appended to the dataframe
#' @param tz time zone
#' @return a canonical glucose value dataframe
#' @export
glucose_df_from_nutrisense <- function(filepath = system.file("extdata", package = "cgmr", "Firstname3Lastname3_nutrisense.csv"),
                                       user_id = 2000, # placeholder
                                       tz = "UTC") {


  firstline <- readLines(con = filepath, 1) %>%
    str_split(pattern = ",", simplify = TRUE)

  if(firstline[1] != "Nutrisense") return(NULL)  # Not a Nutrisense file

  skip_lines <- 1

  glucose_raw_ <- nutrisense_raw(filepath = filepath, skip_lines = skip_lines)
  # col_types = "cccdddddcddddcddddd")

  glucose_raw <- xform_nutrisense(glucose_raw_, tz = tz) %>% bind_cols(`user_id` = user_id)

  return(glucose_raw)
}

#' @title read a Nutrisense export file and return its username and PSI glucose format
#' @description Nutrisense export files contain timestamped information about
#' glucose values, meals, exercise, sleep, and more.
#' This function is like `glucose_df_from_nutrisense()` except it also returns the name of the user.
#' Important: This function expects an extra line at the top of the Nutrisense raw file.
#' The first line must say "Nutrisense" and include the full name of the person.
#' @param filepath path to a valid Nutrisense data file
#' @param user_id new user ID to be appended to the dataframe
#' @param tz time zone
#' @import dplyr magrittr
#' @return a list (username, glucose_df)
#' @export
nutrisense_results <- function(filepath = system.file("extdata", package = "cgmr", "Firstname3Lastname3_nutrisense.csv"),
                                       user_id = 2000, # placeholder
                                       tz = "UTC") {


  firstline <- readLines(con = filepath, 1) %>%
    str_split(pattern = ",", simplify = TRUE)

  if(firstline[1] != "Nutrisense") return(NULL)  # Not a Nutrisense file

  username <- firstline[2]

  skip_lines <- 1

  glucose_raw_ <- nutrisense_raw(filepath = filepath, skip_lines = skip_lines)
  glucose_raw <- xform_nutrisense(glucose_raw_, tz)

  return(list(username=username, glucose_raw=arrange(glucose_raw, `time`)))
}

#' @title read a Levels export file and convert to the PSI CGM format
#' @description Levels export files contain timestamped information about
#' glucose values, meals, exercise, sleep, and more.
#' Important: This function expects an extra line at the top of the Nutrisense raw file.
#' The first line must say "Levels" and include the full name of the person.
#' @param filepath path to a valid Levels CSV data file
#' @param user_id new user ID to be appended to the dataframe
#' @param tz time zone
#' @return a canonical glucose value dataframe
#' @export
glucose_df_from_levels <- function(filepath = system.file("extdata", package = "cgmr", "Firstname4Lastname4_levels.csv"),
                                       user_id = 3000, # placeholder
                                       tz = "UTC") {


  firstline <- readLines(con = filepath, 1) %>%
    str_split(pattern = ",", simplify = TRUE)

  if(firstline[1] != "Levels") return(NULL)  # Not a Levels file

  skip_lines <- 1

  glucose_raw <-
    readr::read_csv(filepath, skip=skip_lines, show_col_types = FALSE) %>%
    transmute(`time` = `Time (UTC)`,
              value = ifelse(`Type of Reading` == "timeseries",
                             `Glucose Reading (mg/dL)`,
                             `Glucose Reading (mg/dL)`),
              scan = ifelse(`Type of Reading` == "directread",
                            `Glucose Reading (mg/dL)`,
                            NA),
              hist = ifelse(`Type of Reading` == "timeseries",
                            `Glucose Reading (mg/dL)`,
                            `Glucose Reading (mg/dL)`),
              user_id = user_id)


  return(glucose_raw)
}

