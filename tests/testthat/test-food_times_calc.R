
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

glucose_records_p <- bind_rows(martha_glucose,richard_glucose)
notes_records_p <- bind_rows(martha_notes, richard_notes_glucose, richard_notes_notes)
cgm_d <- cgm_data(glucose_records_p, notes_records_p)

ftf_df0 <- food_times_df_fast(cgm_d, prefixLength = 0)
ftf_df1 <- food_times_df_fast(cgm_d, prefixLength = 20, foodname = "blueberries")

mealnames_blu <-
  as_tibble_col(c("1235-6/4-blueberries",
                  "1234-7/30-blueberries",
                  "1234-7/31-blueberries",
                  "1234-8/2-blueberries"
  ), column_name = "meal")

v1 <- ftf_df1 %>% group_by(date_ch) %>% filter(date_ch == "7/31")
v2 <- ftf_df1 %>% group_by(date_ch) %>% filter(date_ch == "6/4")

test_that("interpolation works", {
  expect_equal(interpolated(v1$t,v2, operator = function(x) {sum(x)}), sum(v1$t))
})

test_that("combined_food_times_df() gives reasonable answers",{
  expect_equal(combined_food_times_df(ftf_df0) %>% group_by(meal) %>% slice(2) %>% pull(ave),
               rep(101.66667,3),
               tolerance = 1e-3)

})

test_that("food_times_df can handle non-existent users", {
  expect_equal(food_times_df(cgm_d, user_id = -1), NULL)
})


test_that("food_times_df_fast holds correct mealnames",{
  expect_equal(ftf_df1 %>% distinct(meal), mealnames_blu)
  expect_equal(ftf_df0 %>% distinct(date_ch) %>% as.character() , "c(\"6/26-9\", \"6/27-10\", \"8/8-12\")")
})


test_that("food_times_df holds correct start time: prefixLength = 20",{
  expect_equal(ftf_df1 %>%
                 group_by(meal) %>%
                 slice(1) %>%
                 pull(value),
               c(91,56,79,64))
})

test_that("food_times_df holds correct start time: prefixLength = 0 ",{
  expect_equal(ftf_df0 %>%
                 group_by(meal) %>%
                 slice(2) %>% pull(value), c(136,81,88))
})


test_that("normalize_value() works for prefixLength = 0 ",{
  expect_equal(ftf_df0 %>% normalize_value() %>% group_by(meal) %>% slice(5) %>% pull(value),
               c(-38,-4,2))
})

test_that("normalize_value() works for prefixLength = 20 ",{
  expect_equal(ftf_df1 %>% normalize_value() %>% group_by(meal) %>% slice(5) %>% pull(value),
               c(-27, -3, -17))
})
