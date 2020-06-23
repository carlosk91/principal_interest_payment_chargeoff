#### Creating boosting trees for principal paid ####

monthly_payments_for_bt <- principalpaid_df_bt()

#Boosting
Ntrees <- 1000
Idepth <- 7
Shrink <- 0.001
pred.boost <- c()

## Boosting tree

bt_model_principal_paid <-
  boost_tree(
    learn_rate = Shrink,
    trees = Ntrees,
    tree_depth = Idepth,
    min_n = 1,
    sample_size = 1,
    mode = "regression"
  ) %>%
  set_engine("xgboost") %>%
  fit(principal_paid_share ~ .,
      data = monthly_payments_for_bt %>%
        filter(!is.na(principal_paid_share)))

vintage <- seq_months_to_forecast(end_date = '2020-12-01')
months <- seq_months_to_forecast()
original_term_to_maturity <- c(3, 6, 11)
credit_segment <- c('Prime', 'NearPrime', 'SubPrime')
vertical <- c('Air', 'Cruise', 'Other', 'Package')

vintages_mb <-
  as_tibble(expand_grid(
    vintage,
    months,
    original_term_to_maturity,
    credit_segment,
    vertical
  )) %>%
  transmute(
    vintage,
    months,
    vintage_year = year(vintage),
    year_calendar = year(months),
    vintage_month = months(months),
    month_calendar = months(months),
    original_term_to_maturity,
    months_in_books = month_diff(vintage, months),
    credit_segment,
    vertical
  )  %>%
  filter(months_in_books >= 0,
         original_term_to_maturity + 2 >= months_in_books)


pred.boost <-
  unlist(predict(bt_model_principal_paid,
                 vintages_mb %>% select(-c(vintage,
                                           months)))[['.pred']])

monthly_payments_for_terms_simulation_18months <-
  expand_grid(vintage,
              term_simulation_payments(base_term = 11,
                                       term_to_simulate = 18)) %>%
  mutate(months = vintage %m+%
           months(months_in_books))

monthly_payments_for_terms_simulation_24months <-
  expand_grid(vintage,
              term_simulation_payments(base_term = 11,
                                       term_to_simulate = 24)) %>%
  mutate(months = vintage %m+%
           months(months_in_books))

df_pronostico_principalpaid_share <-
  as_tibble(cbind(vintages_mb, pred.boost)) %>%
  mutate(principal_paid_mod =
           case_when(pred.boost < 0 ~ 0,
                     T ~ pred.boost)) %>%
  arrange(vintage,
          original_term_to_maturity,
          credit_segment,
          vertical,
          months_in_books) %>%
  group_by(vintage,
           original_term_to_maturity,
           credit_segment,
           vertical) %>%
  mutate(
    principal_paid_share = principal_paid_mod /
      sum(principal_paid_mod),
    principal_paid_cumulative_share =
      cumsum(principal_paid_share) - principal_paid_share
  ) %>%
  ungroup() %>%
  select(
    -c(
      vintage_year,
      year_calendar,
      vintage_month,
      month_calendar,
      pred.boost,
      principal_paid_mod
    )
  ) %>%
  bind_rows(
    monthly_payments_for_terms_simulation_18months,
    monthly_payments_for_terms_simulation_24months
  ) %>%
  filter(vintage < as_date('2023-01-01'))
