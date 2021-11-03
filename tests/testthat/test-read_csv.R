
SAMPLE_NEW_CSV <- file.path("SamplePerson_glucose.csv")
FAKE_NEW_USER_ID <- -1234

sample_csv <- glucose_df_from_libreview_csv(file=SAMPLE_NEW_CSV,
                                            user_id = FAKE_NEW_USER_ID)

richard_glucose <- glucose_df_from_libreview_csv(
  user_id = 1234,
  system.file("extdata", package = "cgmr", "Firstname2Lastname2_glucose.csv")
) %>% filter(`time`>as_date("2021-06-01"))

richard_notes_glucose <-   notes_df_from_glucose_table(richard_glucose, user_id=1234)
richard_notes_notes <- notes_df_from_csv(
  user_id = 1234,
  file = system.file("extdata", package = "cgmr", "Firstname2Lastname2_notes.csv")
)


test_that("can find all new records in a CSV file not in the databaes", {
  expect_equal(nrow(sample_csv),
               14) # Although the CSV has 16 rows, 2 are headers and 4 are record-type = 6 (which is ignored)
})

test_that("Can read correct values from glucose csv",{
  expect_equal(richard_glucose[100,3]$hist, 68)
})

test_that("Can read raw glucose files",{
  expect_equal(libreview_csv_df()[["glucose_raw"]][10,3]$glucose_historic, 66)
  expect_equal(libreview_csv_df(file=SAMPLE_NEW_CSV)[["name"]], "Sample Person")
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

test_that("Name pulled from CSV file is correct",{
  expect_equal(name_from_libreview_file(file = system.file("extdata", package = "cgmr", "Firstname2Lastname2_glucose.csv")),
               "Richard Sprague")
  expect_equal(name_from_libreview_file(file = SAMPLE_NEW_CSV),
               "Sample Person")
})
