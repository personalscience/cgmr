% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nutrisense_read.R
\name{nutrisense_results}
\alias{nutrisense_results}
\title{read a Nutrisense export file and return its username and PSI glucose format}
\usage{
nutrisense_results(
  filepath = system.file("extdata", package = "cgmr",
    "Firstname3Lastname3_nutrisense.csv"),
  user_id = 2000,
  tz = "UTC"
)
}
\arguments{
\item{filepath}{path to a valid Nutrisense data file}

\item{user_id}{new user ID to be appended to the dataframe}

\item{tz}{time zone}
}
\value{
a list (username, glucose_df)
}
\description{
Nutrisense export files contain timestamped information about
glucose values, meals, exercise, sleep, and more.
This function is like \code{glucose_df_from_nutrisense()} except it also returns the name of the user.
Important: This function expects an extra line at the top of the Nutrisense raw file.
The first line must say "Nutrisense" and include the full name of the person.
}
