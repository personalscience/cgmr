
utils::globalVariables(c("value", "meal", "foodname", "control", "legend", "ymin", "ymax"))


#' Process Food Results by Meal
#'
#' This function takes a food results dataframe and processes it by grouping
#' it by meal, arranging it by time ('t'), and adjusting the 'value' column
#' to start from zero for each meal. The resulting dataframe is arranged by 'meal'.
#'
#' @param foodresults A dataframe of food results. It should contain at least the
#' following three columns: 't' (numeric, representing time), 'value' (numeric, representing
#' the value associated with each time point), and 'meal' (character, representing the meal
#' associated with each time point). Typically, `foodresults` is a dataframe created after
#' calling \code{\link{food_times_df}} with a valid object created with
#' \code{\link{cgm_data}} and a 'foodname' string.
#'
#' @return A dataframe of the processed food results, arranged by 'meal'.
#'
#' @export
food_by_meal <- function(foodresults = NULL) {
  food_results <- foodresults %>%
    group_by(meal) %>%
    arrange(t) %>%
    mutate(value = value - first(value)) %>%
    ungroup() %>%
    arrange(meal)
}

#' @title Plot Glucose Level Changes Over Time
#' @description This function creates a line plot of glucose level changes over time. The changes are colored by meals. The function includes a grey rectangle behind the line plot for contrast.
#' @param foodresults A dataframe, usually the result of a call to \link{food_times_df}.
#' The dataframe should have the following columns:
#' * t: a numeric vector representing time.
#' * value: a numeric vector representing glucose level changes.
#' * meal: a character vector representing meal names.
#' If NULL, the function will return NULL. Default is NULL.
#' @param foodname a string representing the name of the food to plot. Note: this is for display only; it does not filter or otherwise generate different results.
#' @return A ggplot object representing the line plot of glucose level changes over time.
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_rect
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 labs
#' @export
plot_food <- function(foodresults = NULL, foodname = "Food") {

  if(is.null(foodresults)) {return(NULL)}
  food_results <- food_by_meal(foodresults)

  ggplot2::ggplot(food_results, aes(x = t, y = value, color = meal)) +
    ggplot2::geom_rect(
      aes(
        xmin = -Inf,
        xmax = Inf,
        ymin = -Inf,
        ymax = Inf
      ),
      color = "lightgrey",
      fill = "lightgrey",  # Add fill color to make the background visible
      alpha = 0.1  # Adjust the transparency level as per your preference
    ) +
    ggplot2::geom_line(linewidth = 3) +
    labs(title = "Change in Glucose Levels", subtitle = sprintf("%s", foodname), y = "Change in mg/dL", x = "Minutes")
}

#' Plot Intervention Over Time
#'
#' This function generates a plot of glucose responses over time, showing
#' vertical bars representing the range of glucose values for each time point.
#' The bars are color-coded based on whether the meal includes the specified intervention.
#'
#' @param foodresults A data frame containing the glucose measurements. This is typically generated
#'   by calling \code{\link{food_times_df}} with a valid object created by \code{\link{cgm_data}}
#'   and a 'foodname' string. The dataframe should contain columns named 't', 'value', 'meal', and 'control'.
#'   't' should be the time measurements.
#'   'value' should be the glucose measurements.
#'   'meal' identifies unique meals.
#'   'control' is a logical indicating whether an intervention was applied.
#'   If this parameter is NULL, the function will return NULL.
#' @param intervention A string representing the intervention (e.g., "glucotrojan").
#'   The function will check whether each meal includes this intervention.
#'
#' @return A ggplot2 object representing the glucose response plot. The plot can be further customized
#'   using additional ggplot2 functions.
#'
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_linerange labs scale_color_manual
#' @importFrom dplyr mutate group_by summarise arrange
#' @importFrom stringr str_detect str_to_lower

plot_intervention <- function(foodresults = NULL, intervention = "glucotrojan") {

  if(is.null(foodresults)) {return(NULL)}
  food_results <- food_with_intervention(foodresults, intervention)

  # calculate counts of unique meals for each group
  counts <- food_results %>%
    group_by(control) %>%
    summarise(n = n_distinct(meal), .groups = 'drop')

  # create a new variable for legend labels with counts
  food_results <- food_results %>%
    left_join(counts, by = "control") %>%
    mutate(legend = ifelse(control, paste0("YES (", n, ")"), paste0("NO (", n, ")")))

  # calculate ymin and ymax
  food_results_summary <- food_results %>%
    group_by(t, legend) %>%
    summarise(ymin = min(value), ymax = max(value), .groups = 'drop')

  g <- ggplot2::ggplot() +
    ggplot2::geom_linerange(data = food_results_summary, aes(x = t, ymin = ymin, ymax = ymax, color = legend), linetype = "solid", size = 1) +
    ggplot2::geom_point(data = food_results, aes(x = t, y = value, color = legend)) +
    labs(title = "Change in Glucose Levels", subtitle = sprintf("%s", food_results$foodname[1]), y = "Change in mg/dL", x = "Minutes") +
    scale_color_manual(values = c("blue", "red"), name = intervention)

  return(g)
}


#' Calculate and Summarize Metabolic Response Metrics
#'
#' This function computes several metrics related to glucose metabolism response
#' for each unique meal from a dataset of glucose measurements.
#' The metrics include incremental area under the curve (iAUC), minimum glucose level,
#' maximum glucose level, and the rise in glucose from beginning to end.
#'
#' @param foodresults A data frame containing the glucose measurements. This is typically generated
#'   by calling \code{\link{food_times_df}} with a valid object created by \code{\link{cgm_data}}
#'   and a 'foodname' string. The dataframe should contain columns named 't', 'value', 'meal', and 'control'.
#'   't' should be the time measurements.
#'   'value' should be the glucose measurements.
#'   'meal' identifies unique meals.
#'   'control' is a logical indicating whether an intervention was applied.
#'   If this parameter is NULL, the function will return NULL.
#'
#' @return A tibble with the following columns:
#'   'Experiment' - name of the meal.
#'   'IAUC' - incremental area under the curve for the glucose response.
#'   'Min' - minimum glucose level.
#'   'Max' - maximum glucose level.
#'   'Rise' - change in glucose from the first to the last measurement.
#'   The tibble is sorted in ascending order by IAUC.
#'
#' @export
#'
#' @importFrom dplyr mutate group_by summarise select arrange distinct
#' @importFrom tibble tibble
food_table <- function(foodresults = NULL) {
  if(is.null(foodresults)) {return(NULL)}
  food_by_meal(foodresults) %>%
    distinct() %>%
    group_by(meal, foodname) %>%
    summarize(
      iAUC = cgmr::auc_calc(tibble(time = t, value = value)),
      # auc = DescTools::AUC(t,value-first(value)),
      min = min(value),
      max = max(value),
      rise = last(value) - first(value),
      .groups = "drop"
    ) %>%
    select(-foodname) %>%
    {
      colnames(.) <- c("Experiment", "IAUC", "Min", "Max", "Rise")
      .
    } %>%
    # summarize(auc = sum((lag(value)-value)*(t-lag(t)), na.rm = TRUE)) %>%
    arrange(IAUC)
}


#' Add intervention column to data frame
#'
#' This function adds a new logical column to a tibble, named "control". The value in this column will be TRUE if
#' the specified intervention string is found within the 'meal' column of the data frame, and FALSE otherwise.
#' The string detection is case-insensitive.
#'
#' @param df A data frame (or tibble) to which the 'control' column will be added.
#' @param intervention A string to be searched for within the 'meal' column of the input data frame. Defaults to "glycotrojan".
#'
#' @return A tibble that is the same as the input tibble, but with an added 'control' column.
#'
#' @importFrom dplyr mutate
#' @importFrom stringr str_detect str_to_lower
#' @export
food_with_intervention <- function(df, intervention = "glucotrojan") {
  df <- df %>%
    mutate(control = str_detect(str_to_lower(meal), str_to_lower(intervention)))

  return(df)
}
