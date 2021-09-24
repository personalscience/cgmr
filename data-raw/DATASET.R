## code to prepare `sample_libreview_df` dataset goes here


#USER_LIST_FILEPATH <- file.path("~/dev/psi/psiCGM","inst","extdata","Tastermonial_allPatients_dashboard.csv")


sample_libreview_df <- glucose_df_from_libreview_csv(system.file("extdata", package = "psiCGM", "Firstname1Lastname1_glucose.csv"))
usethis::use_data(sample_libreview_df, overwrite = TRUE)


