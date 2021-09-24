# Initial constants ----


#' @title Possible values for `Activity` column in Notes.
NOTES_COLUMNS <- c("Start", "End", "Activity","Comment", "Z","user_id", "TZ")
ACTIVITY_TYPES <- c("Sleep", "Event", "Food","Exercise")


# Read CSV ----

#' Read a valid libreview CSV file and return a dataframe and new user id
#' Since Libreview files don't already include a user ID, append one to the dataframe that is returned.
#' Importantly, datetimes are assumed to be `Sys.timezone()`.
#' @title Read a standard format Libreview CSV file
#' @return a canonical glucose value dataframe
#' @param file path to a valid Libreview CSV file
#' @param user_id new user ID to be appended to the dataframe
#' @export
#' @import readr magrittr tibble
#' @import lubridate
glucose_df_from_libreview_csv <- function(file=system.file("extdata",
                                                           package = "psiCGM",
                                                           "Firstname2Lastname2_glucose.csv"),
                                          user_id = 1234) {


  firstline <- readLines(con = file, 1) %>%
    str_split(pattern = ",", simplify = TRUE)

  skip_lines <- if_else(firstline[1] == "Glucose Data", 1, 2)

  glucose_raw <-
    readr::read_csv(file, skip = skip_lines, col_types = "cccdddddcddddcddddd") %>%
    transmute(
      timestamp = lubridate::mdy_hm(`Device Timestamp`),
      record_type = `Record Type`,
      glucose_historic = `Historic Glucose mg/dL`,
      glucose_scan = `Scan Glucose mg/dL`,
      strip_glucose = `Strip Glucose mg/dL`,
      notes = if_else(!is.na(Notes), paste0("Notes=",Notes),
                      Notes)
    )

  glucose_df <- glucose_raw  %>%
    #dplyr::filter(record_type != 6) %>% # Record type 6 does nothing
    transmute(time = `timestamp`,
              scan = glucose_scan,
              hist = glucose_historic,
              strip = strip_glucose,
              value = hist,
              food = notes) # dplyr::if_else(is.na(notes),notes, paste0("Notes=",notes))) #dplyr::if_else(is.na(notes),"no", "yes"))#paste0("Notes=",notes)))


  glucose_df %>% add_column(user_id = user_id)

}

#' @title Notes dataframe from a CSV
#' @description Return a canonical notes dataframe from a properly-constructed Notes CSV file.
#' @param file path to a valid notes CSV file
#' @param user_id user ID to associate with this dataframe
#' @return dataframe for a valid notes CSV file
#' @export
notes_df_from_csv <- function(file=system.file("extdata", package="psiCGM", "FirstName1Lastname1_notes.csv"),
                              user_id = 1235) {


  notes <-   read_csv(file, show_col_types = FALSE)
  transmute("{NOTES_COLUMNS[1]}" := mdy_hm(Start),
            "{NOTES_COLUMNS[2]}" := mdy_hm(End),
            "{NOTES_COLUMNS[3]}" := factor(Activity, levels = ACTIVITY_TYPES),
            "{NOTES_COLUMNS[4]}" :=  Comment,
            "{NOTES_COLUMNS[5]}" := Z,
            "{NOTES_COLUMNS[6]}" := user_id,
            "{NOTES_COLUMNS[7]}" := as.integer(NA))


  return(notes)
}


#' @title Return all `glucose_records` that have something in the notes field
#' @description Notes are sometimes located in the "Notes" field of a CSV file,
#' which in turn will be saved in `glucose_records`.
#' Harvest any such notes and place them in `notes_records`.
#' @param user_id User ID
#' @return dataframe of
#' @export
#'
notes_df_from_glucose_table <- function(glucose_records,
                                        user_id = 1234) {


  ID = user_id

  food_records <- glucose_records %>%
    filter(user_id == ID) %>%
    filter(!is.na(food)) %>% collect() %>%
    transmute(Start = time,
              End = lubridate::as_datetime(NA),
              Activity = factor("Food", levels = ACTIVITY_TYPES),
              Comment = as.character(stringr::str_replace(food,"Notes=","")),
              Z = as.numeric(NA),
              user_id = user_id)

  return(food_records)

}

