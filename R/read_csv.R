

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
      timestamp = lubridate::mdy_hm(`Device Timestamp`, tz = tz),
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


  notes <-   read_csv(file) %>%
    transmute(Start = mdy_hm(Start, tz = Sys.timezone()),
              End = mdy_hm(End, tz = Sys.timezone()),
              Activity = factor(Activity, levels = NOTES_COLUMNS),
              Comment = Comment,
              Z = Z    )


  notes$Start <- lubridate::force_tz(notes$Start, tzone=Sys.timezone())
  notes$End <- lubridate::force_tz(notes$End, tzone=Sys.timezone())
  return(bind_cols(notes,user_id = user_id))
}
