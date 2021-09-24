# Extra functions, most of which are doomed to deprecation and/or to be factored into other packages


#' @title Glucose values for ID after startDate
#' @description
#' For example `read_glucose_for_user_at_time(ID=22,startTime = as_datetime("2020-02-16 00:50:00", tz=Sys.timezone()))`
#' @param conn_args valid database connection
#' @param user_id user ID
#' @param startTime datetime object for start time
#' @param timelength minutes of glucose values to return
#' @return A valid glucose dataframe
#' @export
glucose_df_for_users_at_time <- function(conn_args=config::get("dataconnection"),
                                         user_id=1234,
                                         startTime=now()-hours(36),
                                         timelength=120){


  con <- DBI::dbConnect(drv = conn_args$driver,
                        user = conn_args$user,
                        host = conn_args$host,
                        port = conn_args$port,
                        dbname = conn_args$dbname,
                        password = conn_args$password)

  ID = user_id

  cutoff_1 <- as_datetime(startTime)
  cutoff_2 <- as_datetime(startTime + minutes(timelength))

  glucose_df <- tbl(con, conn_args$glucose_table)  %>%
    filter(user_id %in% ID & time >= cutoff_1 &
             time <= cutoff_2) %>% collect()

  #  filter(user_id == ID & (record_date+record_time) >= startTime & (record_date+record_time) <= (startTime + timelength)) %>% collect()# & top_n(record_date,2))# %>%

  glucose_raw <- glucose_df %>% transmute(  time = force_tz(as_datetime(time), Sys.timezone()),
                                            scan = value, hist = value, strip = NA, value = value,
                                            food = as.character(stringr::str_match(food,"Notes=.*")),
                                            user_id = user_id)



  DBI::dbDisconnect(con)
  glucose_raw
}

#' @title return a dataframe of rows in the database where food matches food
#' @description
#' Search the notes database for records indicating `foodname` and
#' return just those rows that contain that food.
#' @param conn_args database connection
#' @param user_id user ID
#' @param foodname a string indicating a specific food
#' @import DBI stringr
#' @return a valid glucose dataframe containing records matching `food`
#' @export
glucose_for_food_df <- function(conn_args=config::get("dataconnection"),
                                user_id = 1235,
                                foodname = "blueberries"){


  con <- DBI::dbConnect(drv = conn_args$driver,
                        user = conn_args$user,
                        host = conn_args$host,
                        port = conn_args$port,
                        dbname = conn_args$dbname,
                        password = conn_args$password)


  ID <-  user_id

  nf <- notes_df_from_notes_table(conn_args,
                                  user_id = ID,
                                  db_filter = function(x) {x}) %>%
    filter(stringr::str_detect(stringr::str_to_lower(Comment), stringr::str_to_lower(foodname)))

  return(nf)
}


#' @title Convert the timestamp into time objects
#' @param times_vector vector of start times.
#' @return a vector normalized to the beginning of the sequence
#' @export
zero_time <- function(times_vector){
  start <- min(times_vector)

  return(as.numeric((times_vector-start)/60))

}

#' @title Normalize time dataframe to zero
#' @param df valid glucose dataframe
#' @return dataframe
#' @export
make_zero_time_df <- function(df){
  return(arrange(df,time) %>% transmute(t=zero_time(time),
                                        value=value,
                                        meal=meal,
                                        user_id=user_id))
}


#' @title normalize with prefixlength
#' @description
#' Assumes `df includes columns `t` and `value`
#' @param df a dataframe
#' @return dataframe
#' @export
normalize_value <- function(df) {

  after_start <- df %>% group_by(meal) %>%
    filter(t>=0) %>%
    arrange(meal,t) %>%
    mutate(value = value-first(value)) %>% ungroup()

  before_start <- df %>% group_by(meal) %>%
    filter(t<0) %>%
    arrange(meal,t) %>%
    mutate(value = value - last(value)) %>% ungroup()

  bind_rows(before_start, after_start) %>% group_by(meal,t)
}

#' @title normalize all `df$value` to a range from 0 to 1
#' @description
#' Assumes `df includes columns `t` and `value`
#' @param df a dataframe
#' @return dataframe
#' @export
normalize_to_zero <- function(df) {

  after_start <- df %>% group_by(meal) %>%
    filter(t>=0) %>%
    arrange(meal,t) %>%
    mutate(value = (value - min(value)) / (max(value) - min(value))) %>%
    #mutate(value = value-first(value)) %>%
    ungroup()

  before_start <- df %>% group_by(meal) %>%
    filter(t<0) %>%
    arrange(meal,t) %>%
    mutate(value = (value - min(value)) / (max(value) - min(value))) %>%
    #mutate(value = value - last(value)) %>%
    ungroup()

  bind_rows(before_start, after_start) %>% group_by(meal,t)
}


#ft_df0 %>% normalize_value() %>% select(t, value, meal,nvalue,pvalue) %>% arrange(meal,t)
# ft_df1 %>% normalize_value() %>% arrange(meal,t) %>% select(t,value,nvalue) %>%  View()
#
#
# ft_df0 %>% normalize_value() %>% select(t, value, meal, nvalue) %>% arrange(meal,t)


#' @title return a new df where value are normalized to start from zero.
#' @description Useful when transforming `food_times_df`, this will reset all time values
#' back to offsets from the earliest time in the dataframe.
#' Assumes a dataframe with column `value`.
#' @param df dataframe
#' @return dataframe
#' @export
old_normalize_value <- function(df){


  return(df %>% mutate(value=value-first(value)))


}


#' @title Glucose values after eating a specific food
#' @description
#' return a dataframe of the Glucose values for a `timeLength`
#' following `foodname` appearing in `notes_records`
#' @param user_id user ID
#' @param foodname character string representing the food item of interest
#' @param timeLength number of minutes for the glucose record to show after the food was eaten.
#' @return dataframe
#' @export
old_food_times_df <- function(user_id = 1235, timeLength=120, foodname="watermelon"){


  f <- glucose_for_food_df(user_id = user_id, foodname=foodname)


  # original_levels <- factor(f$user_id) # to prevent a conversion of id_user to char later

  ID = user_id

  df <- NULL
  for(user in ID){
    g <- f %>% filter(user_id==user)
    for(t in g$Start){
      new_segment_df <- glucose_df_for_users_at_time(user_id =user, startTime = lubridate::as_datetime(t,tz=Sys.timezone())) %>%
        mutate(meal=sprintf("%s%s-%i/%i",
                            substring(username_for_id(user),1,1),
                            str_split(username_for_id(user),
                                      " ")[[1]][2],
                            month(as_datetime(t)),
                            day(as_datetime(t))),
               foodname = sprintf("%s-%i/%i",
                                  foodname,
                                  month(as_datetime(t)),
                                  day(as_datetime(t))),
               user_id = factor(user_id)) #user_id=factor(user_id, levels = original_levels))

      df <- bind_rows(df, transmute(new_segment_df,
                                    t=zero_time(time),
                                    value=value,
                                    meal=meal,
                                    foodname = foodname,
                                    user_id=user_id)
                      #make_zero_time_df(new_segment_df)
      )
    }
  }
  return(df)
}

#' @title Glucose values after eating a specific food (local memory version)
#' @description
#' return a dataframe of the Glucose values for a `timeLength`
#' following `foodname` appearing in `notes_records`.
#' This function calls the database directly
#' and is intended to work standalone, without other functions.
#' @param user_id user ID.  If NULL then assume all users
#' @param glucose_df a valid glucose dataframe
#' @param notes_df a valid notes dataframe
#' @param foodname character string representing the food item of interest
#' @param timeLength number of minutes for the glucose record to show after the food was eaten.
#' @param prefixLength number of additional minutes to add before the starttime.
#' @return dataframe
#' @export
food_times_df_fast <-
  function(glucose_df,
           notes_df,
           user_id = NULL,
           timeLength = 120,
           prefixLength = 0,
           foodname = "watermelon") {


    if (is.null(user_id)){  # find all foodnames regardless of user_id
      notes_df <-
        notes_df  %>%
        filter(stringr::str_detect(
          stringr::str_to_lower(Comment),
          stringr::str_to_lower(foodname)
        ))}
    else { # find only those foodname associated with user_id
      ID = user_id
      notes_df <-
        notes_df %>% filter(user_id %in% ID) %>%
        filter(stringr::str_detect(
          stringr::str_to_lower(Comment),
          stringr::str_to_lower(foodname)
        ))
    }

    df <- NULL

    users <- unique(notes_df$user_id)

    for (user in users) {
      username <- username_for_id(user)
      f <- glucose_df %>% filter(user_id == user) %>% filter(!is.na(value))
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
                    username = username,
                    date_ch = sprintf("%i/%i",
                                      month(as_datetime(atime)),
                                      day(as_datetime(atime))),
                    timestamp = as_datetime(atime),
                    meal=sprintf("%s%s-%i/%i-%s",
                                 substring(username,1,1),
                                 str_split(username,
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
