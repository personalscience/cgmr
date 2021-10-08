

library(cgmr)

# Glucose Data sample ----
martha_glucose <- glucose_df_from_libreview_csv(
  user_id = 1235,
  system.file("extdata", package = "psiCGM", "Firstname1Lastname1_glucose.csv")
)
richard_glucose <- glucose_df_from_libreview_csv(
  user_id = 1234,
  system.file("extdata", package = "psiCGM", "Firstname2Lastname2_glucose.csv")
) %>% filter(time>as_date("2021-06-01"))

richard_notes_glucose <-   notes_df_from_glucose_table(richard_glucose, user_id=1234)
richard_notes_notes <- notes_df_from_csv(
  user_id = 1234,
  file = system.file("extdata", package = "psiCGM", "Firstname2Lastname2_notes.csv")
)

martha_notes <-
  notes_df_from_csv(
    user_id = 1235,
    file = system.file("extdata", package = "psiCGM", "Firstname1Lastname1_notes.csv")
  )

glucose_records <- bind_rows(martha_glucose,richard_glucose)
notes_records <- bind_rows(martha_notes, richard_notes_glucose, richard_notes_notes)

ftf_df0 <- food_times_df_fast(glucose_records, notes_records, prefixLength = 0)
ftf_df1 <- food_times_df_fast(glucose_records, notes_records, prefixLength = 20, foodname = "blueberries")



# By hand ----
v1 <- ftf_df1 %>% filter(date_ch == "7/31")
v2 <- ftf_df1  %>% filter(date_ch == "6/4")

full_join(with(v1, approx(t,value)) %>% as_tibble() %>% transmute(t=x, value1 = y, date_ch = v1$date_ch[1]),
          with(v2, approx(t,value)) %>% as_tibble() %>% transmute(t=x, value2 = y, date_ch = v2$date_ch[1])) %>%
  mutate(ave = (value1 + value2)/2) %>%
  pivot_longer(cols = c(value1,value2)) %>%
  ggplot(aes(x=t,y=value,color=name)) +
  geom_point() +
    geom_line() + geom_point(aes(x=t,y=ave), color = "green" )


#bind_rows(v1,v2) %>% select(t,value, date_ch) %>% approx()
 # complete(date_ch, nesting(t))


#%>% arrange(date_ch,t) %>%
  ggplot(aes(x=t,y=value,color=date_ch)) +
  geom_point() + geom_line() + geom_line(aes(x=t,y=ave), color = "green" )


bind_rows(v1,v2) %>% select(t,value, date_ch) %>%  expand(date_ch, t) %>% group_by(date_ch) %>% View() # %>% %>%
  ggplot(aes(x=t,y=value, color=date_ch)) + geom_point()


# Using Zoo ----

library(zoo)

  v1 <- ftf_df1 %>% filter(date_ch == "7/31") %>% select(t,value,meal)
  v2 <- ftf_df1  %>% filter(date_ch == "6/4") %>% select(t,value,meal)

z.v1 <- zoo(v1, order.by = v1$t)
z.v2 <- zoo(v1, order.by = v2$t)
start(z.v1)

z <- merge(z.v1,z.v2)
na.approx(z,na.rm = TRUE)

v1 %>% mutate(new = cut_number(t, length(t)))
v1 %>% mutate(new = cut_interval(t, length = 29)) %>%
  group_by(new) %>% add_count() %>% summarize(ave = mean(value))


cut_items <-
  ftf_df1 %>% select(t,value,date_ch,meal) %>%
  mutate(nn=n()) %>%
  group_by(meal) %>%
    add_count() %>%
    filter(n>5) %>%  # must have enough data points to be worth comparing
  ungroup() %>%
  mutate(max_len = max(n)) %>%
  group_by(meal) %>%
    mutate(id = cur_group_id()) %>%
  mutate(time_slice = cut_interval(t, length(t), labels=FALSE)) %>%
  ungroup() %>%
  group_by(time_slice) %>%
  mutate(ave = mean(value, na.rm = TRUE)) %>%  # consolidate any values within a given time_slice
  ungroup() %>%
  arrange(meal)

cut_items %>% select(t,value=ave, meal) %>% ggplot(aes(x=t,y=value)) + geom_line()

ftf_df1 %>% ggplot() +
  geom_line(aes(x=t,y=value,color = meal))  +
  # geom_line(inherit.aes = FALSE, data = cut_items %>% select(t,value=ave, meal),
  #           aes(x=t,y=value), color = "red", size = 3) +
  geom_smooth(inherit.aes = FALSE, data = cut_items %>% select(t,value=ave, meal),
                                                                     aes(x=t,y=value))

for( group in (cut_items %>% pull(id) %>% unique())) {
  cat(sprintf("group=%s\n",group))
}
# for each 'new' in the first group,
# ave =



# ---

set.seed(1)
n <- 1e3
dat <- data.frame(
  x = 1:n,
  y = sin(seq(0, 5*pi, length.out = n)) + rnorm(n=n, mean = 0, sd=0.1)
)

approxData <- data.frame(
  with(dat,
       approx(x, y, xout = seq(1, n, by = 10), method = "linear")
  ),
  method = "approx()"
)

splineData <- data.frame(
  with(dat,
       spline(x, y, xout = seq(1, n, by = 10))
  ),
  method = "spline()"
)

smoothData <- data.frame(
  x = 1:n,
  y = as.vector(smooth(dat$y)),
  method = "smooth()"
)

loessData <- data.frame(
  x = 1:n,
  y = predict(loess(y~x, dat, span = 0.1)),
  method = "loess()"
)

library(ggplot2)
ggplot(rbind(approxData, splineData, smoothData, loessData), aes(x, y)) +
  geom_point(dat = dat, aes(x, y), alpha = 0.2, col = "red") +
  geom_line(col = "blue") +
  facet_wrap(~method) +
  ggtitle("Interpolation and smoothing functions in R") +
  theme_bw(16)
