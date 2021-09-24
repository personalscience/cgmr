# psi_calc_util.r
#


#' @title Calculate Area Under the Curve of glucose values for a restricted timeframe
#' @description Returns AUC for the first timelength minutes after the start of the glucose_df
#' @param glucose_df dataframe of glucose values
#' @param timelength number of minutes to look ahead for the AUC calculation
#' @import DescTools magrittr dplyr lubridate
#' @export
auc_calc_old <- function(glucose_df, timelength = 120) {
  x <- glucose_df %>%
    filter(!is.na(value)) %>%
    select("time","value") %>%
    dplyr::filter(.[["time"]] < first(.[["time"]]) + lubridate::minutes(timelength) ) %>%
    mutate(sec = as.numeric(.[["time"]])-as.numeric(first(.[["time"]]))) %>%
    summarise(auc = DescTools::AUC(as.numeric(sec),value)/60/60)
  x$auc

}


#' @title Helper to calculate Area Under the Curve of glucose values for a restricted timeframe
#' @description Returns each segment of the AUC for the first timelength minutes after the start of the glucose_df.
#' See Brouns, F., Bjorck, I., Frayn, K., Gibbs, A., Lang, V., Slama, G., & Wolever, T. (2005).
#'  Glycaemic index methodology. Nutrition Research Reviews, 18(1), 145-171. doi:10.1079/NRR2005100
#' @param glucose_df dataframe of glucose values
#' @param timelength number of minutes to look ahead for the AUC calculation
#' @return a vector of the AUC values for each segment of the curve.
#' @import DescTools magrittr dplyr lubridate
auc_calc_components <- function(glucose_df, timelength = 120) {

  G <- glucose_df %>% pull("value")
  t <- glucose_df %>% pull("time")
  A <- rep(0,length(t)-1)

  G_ <- function(x) {
    return(G[x+1])
  }
  t_ <- function(x) {
    return(t[x+1])
  }


  A[1] <-  ifelse(G[2]>G[1],
                  (G[2]- G[1]) * (t[2] - t[1])/2,
                  0)

  for(x in 2:length(A)){
    A[x] <- ifelse(G_(x) >= G_(0) & G_(x-1) >= G_(0),
                   ((G_(x) - G_(0)) / 2 + (G_(x-1) - G_(0))/2 )  * (t_(x) - t_(x-1)),
                   ifelse((G_(x) >= G_(0)) & G_(x-1) < G_(0),
                          ((G_(x) - G_(0))^2)/(G_(x) - G_(x-1)) * (t_(x) - t_(x-1))/2,
                          ifelse((G_(x) < G_(0)) & G_(x-1) >= G_(0),
                                 ((G_(x-1) - G_(0))^2) / (G_(x-1) - G_(x))*(t_(x) - t_(x-1))/2,
                                 ifelse((G_(x) < G_(0)) & (G_(x-1) < G_(0)),
                                        0,
                                        NULL))))

  }
  return(A)
}

#' @title Calculate Area Under the Curve of glucose values for a restricted timeframe
#' @description Returns AUC for the first timelength minutes after the start of the glucose_df.
#' See Brouns, F., Bjorck, I., Frayn, K., Gibbs, A., Lang, V., Slama, G., & Wolever, T. (2005).
#'  Glycaemic index methodology. Nutrition Research Reviews, 18(1), 145-171. doi:10.1079/NRR2005100
#' @param glucose_df dataframe of glucose values
#' @param timelength number of minutes to look ahead for the AUC calculation
#' @import DescTools magrittr dplyr lubridate
#' @export
auc_calc <- function(glucose_df, timelength = 120) {

  return(sum(auc_calc_components(glucose_df, timelength = timelength)))


}
