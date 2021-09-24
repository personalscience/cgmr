


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
