#### Creating boosting trees for charge off ####

monthly_chargeoff_fcst <- chargeoff_df_bt()

#Boosting
Ntrees <- 1000
Idepth <- 7
Shrink <- 0.001
pred.boost <- c()

## Boosting tree

bt_model_charge_off <-
  boost_tree(
    learn_rate = Shrink,
    trees = Ntrees,
    tree_depth = Idepth,
    min_n = 1,
    sample_size = 1,
    mode = "regression"
  ) %>%
  set_engine("xgboost") %>%
  fit(charge_off_principal_share ~ .,
      data = monthly_chargeoff_fcst %>%
        filter(!is.na(charge_off_principal_share)))

vintage <- seq_months_to_forecast(end_date = '2022-12-01')
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
    months_on_books = month_diff(vintage, months),
    credit_segment,
    vertical
  )  %>%
  filter(months_on_books > 2,
         original_term_to_maturity + 2 >= months_on_books,)


pred.boost <-
  unlist(predict(bt_model_charge_off,
                 vintages_mb %>% select(-c(vintage,
                                           months)))[['.pred']])

monthly_charge_off_for_terms_simulation_18months <-
  expand_grid(vintage,
              term_simulation_charge_off(base_term = 11,
                                         term_to_simulate = 18)) %>%
  mutate(months = vintage %m+%
           months(months_on_books))

monthly_charge_off_for_terms_simulation_24months <-
  expand_grid(vintage,
              term_simulation_charge_off(base_term = 11,
                                         term_to_simulate = 24)) %>%
  mutate(months = vintage %m+%
           months(months_on_books))

df_pronostico_chargeoff_share <-
  as_tibble(cbind(vintages_mb, pred.boost)) %>%
  mutate(charge_off_mod =
           case_when(pred.boost < 0 ~ 0,
                     T ~ pred.boost)) %>%
  group_by(vintage,
           original_term_to_maturity,
           credit_segment,
           vertical) %>%
  mutate(charge_off_principal_share = charge_off_mod /
           sum(charge_off_mod)) %>%
  ungroup() %>%
  select(
    -c(
      vintage_year,
      year_calendar,
      vintage_month,
      month_calendar,
      charge_off_mod,
      pred.boost
    )
  ) %>%
  rbind(
    monthly_charge_off_for_terms_simulation_18months,
    monthly_charge_off_for_terms_simulation_24months
  ) %>%
  filter(vintage < as_date('2023-01-01'))
