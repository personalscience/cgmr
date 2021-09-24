# read_data_utils.R
# generalized functions to read data, either from disk or from databases
#

# Note: only works with p4mi database (or any db that includes a user_id)
# Sys.setenv(R_CONFIG_ACTIVE = "p4mi")

# library(tidyverse)
# library(lubridate)

# Initial constants ----


#' @title Possible values for `Activity` column in Notes.
NOTES_COLUMNS <- c("Sleep", "Event", "Food","Exercise")

# Read CSV ----

#' Read a valid libreview CSV file and return a dataframe and new user id
#' Since Libreview files don't already include a user ID, append one to the dataframe that is returned.
#' Importantly, datetimes are assumed to be `Sys.timezone()`.
#' @title Read a standard format Libreview CSV file
#' @return a canonical glucose value dataframe
#' @param file path to a valid Libreview CSV file
#' @param user_id new user ID to be appended to the dataframe
#' @param tz time zone
#' @export
#' @import readr magrittr tibble
#' @import lubridate
glucose_df_from_libreview_csv <- function(file=system.file("extdata",
                                                           package = "psiCGM",
                                                           "Firstname2Lastname2_glucose.csv"),
                                          user_id = 1234,
                                          tz = Sys.timezone()) {


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

#' @title Return all `glucose_records` that have something in the notes field
#' @param conn_args valid database connection with a `glucose_records` table
#' @param user_id User ID
#' @return dataframe of
#' @export
#'
notes_df_from_glucose_table <- function(conn_args = config::get("dataconnection"),
                             user_id = 1234) {
  con <- DBI::dbConnect(
    drv = conn_args$driver,
    user = conn_args$user,
    host = conn_args$host,
    port = conn_args$port,
    dbname = conn_args$dbname,
    password = conn_args$password
  )

  ID = user_id

  food_records <- tbl(con, conn_args$glucose_table) %>%
    filter(user_id == ID) %>%
    filter(!is.na(food)) %>% collect() %>%
    transmute(Start = time,
              End = lubridate::as_datetime(NA),
              Activity = factor("Food", levels = NOTES_COLUMNS),
              Comment = as.character(stringr::str_replace(food,"Notes=","")),
              Z = as.numeric(NA),
              user_id = user_id)


  DBI::dbDisconnect(con)
  return(food_records)

}


# Read DB ----

#' @title Load all rows from a database table and return as a dataframe
#' @description Connects to the default database and looks up a table.
#' Slightly more convenient than setting up the database connection first.
#' @param conn_args valid database connection
#' @param table_name character string
#' @return dataframe representation of the table
#' @export
table_df_from_db <- function(conn_args = config::get("dataconnection"),
                             table_name = "glucose_records") {
  con <- DBI::dbConnect(
    drv = conn_args$driver,
    user = conn_args$user,
    host = conn_args$host,
    port = conn_args$port,
    dbname = conn_args$dbname,
    password = conn_args$password
  )

  df <- tbl(con, table_name) %>% collect()

  DBI::dbDisconnect(con)
  return(df)

}


#' @title Read from database a dataframe of glucose values for user_id ID
#' @param conn_args valid database connection
#' @param from_date a string representing the date from which you want to read the glucose values
#' @param user_id ID for a specific user in the database.
#' @param db_filter A function for filtering the database. Use instead of `from_date` or `user_id`
#' @return a dataframe (tibble) with the full Libreview results since fromDate for user_id
#' @description Reads from the current default database the glucose values for user_id ID.
#' @export
glucose_df_from_db <- function(conn_args=config::get("dataconnection"),
                         user_id = 1234,
                         from_date= as_datetime("2000-01-01",
                                                tz = Sys.timezone()),
                         db_filter = NULL){

  con <- DBI::dbConnect(drv = conn_args$driver,
                        user = conn_args$user,
                        host = conn_args$host,
                        port = conn_args$port,
                        dbname = conn_args$dbname,
                        password = conn_args$password)


  ID <- user_id # needed for SQL conversion.

  ## TODO this section can be optimized with a direct SQL call instead of
  ## dealing with dataframes.

  if(!is.null(db_filter)){
    glucose_df <- tbl(con, conn_args$glucose_table)  %>%
      db_filter() %>% collect()
  } else {
    glucose_df <-tbl(con, conn_args$glucose_table) %>%
    dplyr::filter(user_id %in% ID & time >= from_date) %>% collect() # & top_n(record_date,2))# %>%
  }

  glucose_raw <- glucose_df %>% transmute(time = force_tz(as_datetime(time), Sys.timezone()),
                                          scan = value, hist = value, strip = NA, value = value,
                                          food = food,
                                          user_id = user_id)

  DBI::dbDisconnect(con)
  return(glucose_raw)
}


#' @title Read notes dataframe from database
#' @description If notes exist for ID, return all notes in a dataframe
#' @param conn_args valid database connection
#' @param user_id user id
#' @param db_filter A function for filtering the database. Use instead of `from_date` or `user_id`
#' @param fromDate (optional) earliest date from which to return notes
#' @return dataframe representation of all notes for that user
#' @export
notes_df_from_notes_table <- function(conn_args=config::get("dataconnection"),
                    user_id=1235,
                    fromDate="2019-11-01",
                    db_filter = NULL){

  ID = user_id

  con <- DBI::dbConnect(drv = conn_args$driver,
                        user = conn_args$user,
                        host = conn_args$host,
                        port = conn_args$port,
                        dbname = conn_args$dbname,
                        password = conn_args$password)

  if(!is.null(db_filter)){
    notes_df <- tbl(con, "notes_records")  %>%
      db_filter() %>% collect()
  } else {
  notes_df <- tbl(con, "notes_records") %>%   filter(user_id %in% ID ) %>%
    collect()
  }

  DBI::dbDisconnect(con)
  return(notes_df)


}



#' @title Glucose values after eating a specific food
#' @description
#' return a dataframe of the Glucose values for a `timeLength`
#' following `foodname` appearing in `notes_records`.
#' This function calls the database directly
#' and is intended to work standalone, without other functions.
#' @param conn_args database connection
#' @param user_id user ID
#' @param foodname character string representing the food item of interest
#' @param timeLength number of minutes for the glucose record to show after the food was eaten.
#' @param prefixLength number of additional minutes to add before the starttime.
#' @return dataframe
#' @export
food_times_df <-
  function(conn_args = config::get("dataconnection"),
           user_id = NULL,
           timeLength = 120,
           prefixLength = 0,
           foodname = "watermelon",
           db_filter = function(x) {
             x
           }) {
    con <- DBI::dbConnect(
      drv = conn_args$driver,
      user = conn_args$user,
      host = conn_args$host,
      port = conn_args$port,
      dbname = conn_args$dbname,
      password = conn_args$password
    )

    if (is.null(user_id)){
    notes_df <-
      tbl(con, "notes_records") %>% collect() %>%  db_filter() %>%
      filter(stringr::str_detect(
        stringr::str_to_lower(Comment),
        stringr::str_to_lower(foodname)
      ))}
    else {
      ID = user_id
      notes_df <-
        tbl(con, "notes_records") %>% filter(user_id %in% ID) %>%  collect() %>%
        filter(stringr::str_detect(
          stringr::str_to_lower(Comment),
          stringr::str_to_lower(foodname)
        ))
    }

    df <- NULL

    users <- unique(notes_df$user_id)

    for (user in users) {
      f <- tbl(con, "glucose_records") %>% filter(user_id == user) %>% filter(!is.na(value))
      times <- notes_df %>% filter(user_id == user)  %>% pull(Start)
      for (atime in times) {

        t0 <- as_datetime(atime) - minutes(prefixLength)
        tl <- as_datetime(t0 + minutes(timeLength + prefixLength))

        filtered_df <- f %>%
          filter(time >= t0 & time <= tl) %>% collect()
        if (nrow(filtered_df)==0) new_df <- NULL
        else new_df <- filtered_df %>%
          transmute(t = as.numeric(time - min(time))/60 - prefixLength,
                    value = value,
                    username = username_for_id(user),
                    date_ch = sprintf("%i/%i",
                                      month(as_datetime(atime)),
                                      day(as_datetime(atime))),
                    timestamp = as_datetime(atime),
                    meal=sprintf("%s%s-%i/%i-%s",
                                 substring(username_for_id(user),1,1),
                                 str_split(username_for_id(user),
                                           " ")[[1]][2],
                                 month(as_datetime(atime)),
                                 day(as_datetime(atime)),
                                 foodname),
                    foodname = foodname,
                    user_id = factor(user_id)) #user_id=factor(user_id, levels = original_levels))
        df <- bind_rows(df,new_df)

      }
    }

    if(is.null(df)) return(NULL)
    else if(nrow(df)==0) return(NULL)
    else return(df)

  }


#' @title List all products consumed by `user_id`
#' @param user_id vector of user IDs or NULL to show all users
#' @return character vector of product names sorted alphabetically
#' @export
food_list_db <- function(user_id = 1234  ) {
  conn_args <- config::get("dataconnection")
  con <- DBI::dbConnect(
    drv = conn_args$driver,
    user = conn_args$user,
    host = conn_args$host,
    port = conn_args$port,
    dbname = conn_args$dbname,
    password = conn_args$password
  )

  ID <- user_id

  if (is.null(ID)) {
    prods <- tbl(con, "notes_records") %>% filter(Activity == "Food") %>%
      filter(Start > "2021-06-01") %>%
      group_by(Comment) %>% add_count() %>% filter(n > 2) %>% distinct(Comment) %>%
      transmute(productName = Comment, user_id = user_id) %>%
      collect() %>% arrange(productName)
  } else
    prods <-
    tbl(con, "notes_records") %>% filter(Activity == "Food") %>%
    filter(Start > "2021-06-01") %>% filter(user_id %in% ID) %>% distinct(Comment) %>%
    collect() %>% pull(Comment)

  if(length(prods) > 0)
    return(sort(prods))
  else return(NULL)

  DBI::dbDisconnect(con)
  return(prods)
}


# DB queries ----

#' @description
#' Search the default database for the most recent (aka latest) timestamp
#' Note: doesn't currently work for tables other than `glucose_records`
#' @title Most recent date in the database for a given user
#' @param conn_args valid database connection
#' @param user_id user ID
#' @param fromDate data object representing the first date to return
#' @param table_name the table in which to find the latest record (currently fixed at `glucose-records`)
#' @return a date object representing the most recent record in the database for this user. NA if there are no records.
#' @import DBI
#' @export
max_date_for_user <-
  function(conn_args = config::get("dataconnection"),
           user_id = 1234,
           fromDate = "2019-11-01",
           table_name = conn_args$glucose_table) {
    con <- DBI::dbConnect(
      drv = conn_args$driver,
      user = conn_args$user,
      host = conn_args$host,
      port = conn_args$port,
      dbname = conn_args$dbname,
      password = conn_args$password
    )

    ID = user_id

    # maxDate <-
    #   DBI::dbGetQuery(con, "select max(\"time\") from glucose_records;")$max

    maxDate <-
      tbl(con, table_name) %>%
      filter(user_id == ID) %>%
      filter(time == max(time, na.rm = TRUE)) %>%
      pull(time)


    DBI::dbDisconnect(con)

    return(if (length(maxDate > 0))
      maxDate
      else
        NA)


  }

