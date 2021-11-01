## code to prepare `sample_libreview_df` dataset goes here

sample_libreview_df <- glucose_df_from_libreview_csv(system.file("extdata", package = "cgmr", "Firstname1Lastname1_glucose.csv"))
usethis::use_data(sample_libreview_df, overwrite = TRUE)


