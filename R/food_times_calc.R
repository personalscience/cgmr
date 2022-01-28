
#' @title (UNFINISHED) Interpolate values between two vectors
#' @description Given two vectors `v1` and `v2`,
#' return a new vector that has the same length as base
#' @param v1 first vector
#' @param v2 second vector
#' @param operator any function
#' @return vector
interpolated <- function(v1, v2, operator = function(x) {x}){

  result = do.call(operator, list(v1))
  return(result)

}

#' @title Combine multiple glucose values
#' @description Given a dataframe of the form returned by `food_times_df()`,
#' returns that df augmented with an additional row `ave`, which
#' represents the average across all glucose values at that time slot
#' @param food_times_df a dataframe of similar meals offset by a constant time
#' @return dataframe `food_times_df` augmented with a new `ave` row
#' @export
combined_food_times_df <- function(food_times_df) {

  cut_items <- food_times_df %>% select(`t`,`value`,`date_ch`,`meal`,`foodname`, `user_id`, `timestamp`) %>%
    mutate(nn=n()) %>%
    group_by(`meal`) %>%
    add_count() %>%
    filter(n>5) %>%  # must have enough data points to be worth comparing
    ungroup() %>%
    mutate(max_len = max(n)) %>%
    group_by(`meal`) %>%
    mutate(id = cur_group_id()) %>%
    mutate(time_slice = ggplot2::cut_interval(`t`, length(`t`), labels=FALSE)) %>%
    ungroup() %>%
    group_by(time_slice) %>%
    mutate(ave = mean(`value`, na.rm = TRUE)) %>%  # consolidate any values within a given time_slice
    ungroup() %>%
    arrange(`meal`)

  return(cut_items %>% select(-(nn:time_slice)))
}



#' @title Glucose values after eating a specific food (deprecated)
#' @description
#' return a dataframe of the Glucose values for a `timeLength`
#' following `foodname` appearing in `notes_records`.
#' This function calls the database directly
#' and is intended to work standalone, without other functions.
#' @param user_id user ID.  If NULL then assume all users
#' @param cgm_data valid cgm_data object
#' @param foodname character string representing the food item of interest
#' @param timeLength number of minutes for the glucose record to show after the food was eaten.
#' @param prefixLength number of additional minutes to add before the starttime.
#' @return dataframe
#' @export
food_times_df_fast <-
  function(cgm_data,
           user_id = NULL,
           timeLength = 120,
           prefixLength = 0,
           foodname = "watermelon") {

    notes_df <- cgm_data[["notes_records"]]
    glucose_df <- cgm_data[["glucose_records"]]


    if (is.null(user_id)){  # find all foodnames regardless of user_id
      notes_df <-
        notes_df  %>%
        filter(stringr::str_detect(
          stringr::str_to_lower(`Comment`),
          stringr::str_to_lower(`foodname`)
        ))}
    else { # find only those foodname associated with user_id
      ID = user_id
      notes_df <-
        notes_df %>% filter(user_id %in% ID) %>%
        filter(stringr::str_detect(
          stringr::str_to_lower(`Comment`),
          stringr::str_to_lower(`foodname`)
        ))
    }

    df <- NULL

    users <- unique(notes_df$user_id)

    for (user in users) {
      username <- as.character(user)
      f <- glucose_df %>% filter(user_id == user) %>% filter(!is.na(`value`))
      times <- notes_df %>% filter(user_id == user)  %>% pull(`Start`)
      for (atime in times) {

        t0 <- as_datetime(atime) - minutes(prefixLength)
        tl <- as_datetime(t0 + minutes(timeLength + prefixLength))

        filtered_df <- f %>%
          filter(`time` >= t0 & `time` <= tl) %>% collect()
        if (nrow(filtered_df)==0) new_df <- NULL
        else new_df <- filtered_df %>%
          transmute(t = as.numeric(`time` - min(`time`))/60 - prefixLength,
                    value = value,
                    username = username,
                    date_ch = sprintf("%i/%i-%i",
                                      month(as_datetime(atime)),
                                      day(as_datetime(atime)),
                                      hour(as_datetime(atime))),
                    timestamp = as_datetime(atime),
                    meal=sprintf("%s-%i/%i-%s",
                                 username,
                                 month(as_datetime(atime)),
                                 day(as_datetime(atime)),
                                 `foodname`),
                    foodname = `foodname`,
                    user_id = factor(user_id)) #user_id=factor(user_id, levels = original_levels))
        df <- bind_rows(df,new_df)

      }
    }

    if(is.null(df)) return(NULL)
    else if(nrow(df)==0) return(NULL)
    else return(df)

  }


#' @title Glucose values after eating a specific food (local memory version)
#' @description
#' Same results as `food_times_df()`. Return a dataframe of the Glucose values for a `timeLength`
#' following `foodname` appearing in `notes_records`. Returned in the format
#' `t`, `value`, `username`, `date-ch`, `timestamp`, `meal`, `foodname`, `user_id`
#' @param user_id user ID.  If NULL then assume all users
#' @param cgm_data CGM data object
#' @param foodname character string representing the food item of interest
#' @param timeLength number of minutes for the glucose record to show after the food was eaten.
#' @param prefixLength number of additional minutes to add before the starttime.
#' @return dataframe
#' @export
food_times_df <-
  function(cgm_data,
           user_id = NULL,
           timeLength = 120,
           prefixLength = 0,
           foodname = "watermelon") {



    return(food_times_df_fast(cgm_data, user_id, timeLength, prefixLength, foodname))
  }
