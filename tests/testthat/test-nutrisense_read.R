
sample_levels_path <- system.file("extdata", package = "cgmr", "Firstname4Lastname4_levels.csv")

sample_nutrisense_path <- system.file("extdata", package = "cgmr", "Firstname3Lastname3_nutrisense.csv")
#
# levels_results <- glucose_df_from_levels(sample_levels_path)
# nutrisense_just_df <- glucose_df_from_nutrisense(sample_nutrisense_path)
# nutrisense_results <- nutrisense_results(sample_nutrisense_path)
#
# nutrisense_results
#
# test_that("Levels conversion seems reasonable",{
#   expect_equal(levels_results %>% head() %>% pull(`value`), c(106,106,102,102,100,106))
# })
#
# test_that("Nutrisense conversion seems reasonable",{
#   expect_equal(nrow(nutrisense_just_df),16052)
# })
#
# test_that("Nutrisense conversion captures username",{
#   expect_equal(nutrisense_results$username, "Firstname3 Lastname3")
#   message(sprintf("username = %s", nutrisense_results$username))
#   expect_equal(nrow(nutrisense_results$glucose_raw),16052)
#   message(sprintf("Nutrisense head = %s\n", paste(nutrisense_results$glucose_raw$value[1:5])))
#   expect_equal(nutrisense_results$glucose_raw$value[5], 69)
# })
