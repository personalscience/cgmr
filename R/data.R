
#' Sample Libreview CSV Download
#'
#' A typical CSV file from a two-week user of the Freestyle Libre
#' @docType data
#'
#' @usage data(sample_libreview_df)
#' @format A data frame with 2713 rows and 7 variables:
#' \describe{
#'   \item{time}{datetime}
#'   \item{scan}{scanned glucose value mg/dL}
#'   \item{hist}{computed glucose value mg/dL}
#'   \item{strip}{glucose value (mg/dL) from strip (optional)}
#'   \item{value}{glucose value, in mg/dL}
#'   \item{food}{Food the user ate at that moment, if applicable}
#'   \item{user_id}{Unique user ID}
#' }
#' @source \url{http://libreview.com/}
#'
"sample_libreview_df"


#' @title Make a Standard CGM Data Object
#' @param glucose_records canonical glucose dataframe
#' @param notes_records metadata associated with the dataframe
cgm_data <- function(glucose_records, notes_records){

  cgm_d <- list(glucose_records = glucose_records,
                notes_records = notes_records)

  return(cgm_d)

}
