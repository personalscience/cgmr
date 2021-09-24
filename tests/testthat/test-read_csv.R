
SAMPLE_NEW_CSV <- file.path("SamplePerson_glucose.csv")
FAKE_NEW_USER_ID <- -1234

sample_csv <- glucose_df_from_libreview_csv(file=SAMPLE_NEW_CSV,
                                            user_id = FAKE_NEW_USER_ID)

test_that("can find all new records in a CSV file not in the databaes", {
  expect_equal(nrow(sample_csv),
               14) # Although the CSV has 16 rows, 2 are headers and 4 are record-type = 6 (which is ignored)
})

test_that("hist and value are the same", {
  expect_equal(sum(sample_csv %>% filter(!is.na(hist)) %>% pull(hist) == sample_csv %>% filter(!is.na(hist)) %>% pull(value)),
               6) # Although the CSV has 16 rows, 2 are headers and 4 are record-type = 6 (which is ignored)
})


test_that("Notes entries found in CSV file", {
  expect_equal(filter(sample_csv,!is.na(food))[1,6]$food,
               "Notes=typical note") # Although the CSV has 16 rows, 2 are headers and 4 are record-type = 6 (which is ignored)
})
