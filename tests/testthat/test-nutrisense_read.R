
sample_levels_path <- system.file("extdata", package = "cgmr", "Firstname4Lastname4_levels.csv")

sample_nutrisense_path <- system.file("extdata", package = "cgmr", "Firstname3Lastname3_nutrisense.csv")

levels_results <- glucose_df_from_levels(sample_levels_path)
nutrisense_results <- glucose_df_from_nutrisense(sample_nutrisense_path)

test_that("Levels conversion seems reasonable",{
  expect_equal(levels_results %>% head() %>% pull(`value`), c(106,106,102,102,100,106))
})

test_that("Nutrisense conversion seems reasonable",{
  expect_equal(nrow(nutrisense_results),16052)
})
