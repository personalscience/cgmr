
SAMPLE_NEW_CSV <- file.path("SamplePerson_glucose.csv")
FAKE_NEW_USER_ID <- -1234

sample_csv <- glucose_df_from_libreview_csv(file=SAMPLE_NEW_CSV,
                                            user_id = FAKE_NEW_USER_ID)

richard_glucose <- glucose_df_from_libreview_csv(
  user_id = 1234,
  system.file("extdata", package = "psiCGM", "Firstname2Lastname2_glucose.csv")
) %>% filter(time>as_date("2021-06-01"))

richard_notes_glucose <-   notes_df_from_glucose_table(richard_glucose, user_id=1234)
richard_notes_notes <- notes_df_from_csv(
  user_id = 1234,
  file = system.file("extdata", package = "psiCGM", "Firstname2Lastname2_notes.csv")
)


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

test_that("Notes from glucose file are correct",{
  expect_equal(richard_notes_glucose$Comment[6],"Blueberry coconut")
})
