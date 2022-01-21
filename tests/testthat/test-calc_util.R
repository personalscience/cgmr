


martha_glucose <- glucose_df_from_libreview_csv(
  user_id = 1235,
  system.file("extdata", package = "cgmr", "Firstname1Lastname1_glucose.csv")
)
richard_glucose <- glucose_df_from_libreview_csv(
  user_id = 1234,
  system.file("extdata", package = "cgmr", "Firstname2Lastname2_glucose.csv")
) %>% filter(time>as_date("2021-06-01"))

richard_notes_glucose <-   notes_df_from_glucose_table(richard_glucose, user_id=1234)
richard_notes_notes <- notes_df_from_csv(
  user_id = 1234,
  file = system.file("extdata", package = "cgmr", "Firstname2Lastname2_notes.csv")
)

martha_notes <-
  notes_df_from_csv(
    user_id = 1235,
    file = system.file("extdata", package = "cgmr", "Firstname1Lastname1_notes.csv")
  )

glucose_records <- bind_rows(martha_glucose,richard_glucose)
notes_records <- bind_rows(martha_notes, richard_notes_glucose, richard_notes_notes)

# from Table 5
auc_example <- tibble(
  time = c(0	 ,
           15,
           30,
           45,
           60,
           90,
           120),

  value = c( 3.67,
             6.11,
             6.06,
             4.44,
             3.17,
             3.61,
             4)
)



test_that("AUC for typical meals", {
  expect_equal(food_times_df_fast(glucose_records, notes_records, user_id = 1235) %>% group_by(meal) %>%
                 summarize(auc=DescTools::AUC(t,value-first(value))) %>%
                 pull(auc) %>% as.integer(),
               c(-484,228))

})

test_that("Components of AUC values are correct", {
  expect_equal(auc_calc_components(auc_example, timelength = 120),c(18.3, 36.23,23.70,3.50, 0, 4.19),
               tolerance = .1)
})

test_that("Full incremental AUC values are correct", {
  expect_equal(auc_calc(auc_example, timelength = 120),85.914,
               tolerance = .1)
})

test_that("AUC for all specific foods are correct", {
  expect_equal(auc_for_food(foodname = "Blueberries",
                            notes_records = notes_records,
                            glucose_records = glucose_records) %>% pull(iAUC),
               c(82.2, 7.92, 33.8),
               tolerance = 0.001)
  expect_equal(auc_for_food(foodname = "garbagevalue",
                            notes_records = notes_records,
                            glucose_records = glucose_records),
               NA)

  expect_equal(df_for_all_auc(food_list = c("Blueberries", "watermelon"),
                              glucose_records = glucose_records,
                              notes_records = notes_records) %>% pull(iAUC),
               c(238.5, 506, 82.2, 7.92, 33.8),
               tolerance = 0.001)
  expect_equal(df_for_all_auc(food_list = c("garbagevalue", "watermelon"),
                              glucose_records = glucose_records,
                              notes_records = notes_records) %>% pull(iAUC),
               c(238.5, 506),
               tolerance = 0.001)
})

