# read non-Freestyle Libre files

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

  glucose_raw <-
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
                    )) %>% # col_types = "cccdddddcddddcddddd") %>%
    filter(class %in% c("GlucoseMeasurement", "Meal")) %>% arrange(occurred_at) %>%
    transmute(value = if_else(!is.na(value), value,lag(value)),
              time = mdy_hm(occurred_at, tz = tz),
              food = description,
              scan = value,
              hist = value,
              user_id = user_id)

  return(glucose_raw)
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
    transmute(time = `Time (UTC)`,
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

