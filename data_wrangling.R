


#### Training set ####

monthly_originations <-
  monthly_originations %>%
  rowid_to_column('index')

with_progress({
  training_set_amortization_table <-
    amortization_mapping(monthly_originations, monthly_originations$index)
})
plan(sequential)

training_set_amortization_table_unit <-
  training_set_amortization_table %>%
  group_by(index) %>%
  mutate(principal_amortization_share =
           principal_amortization / sum(principal_amortization)) %>%
  ungroup()

monthly_principal_paid_matrix <-
  monthly_cashflows %>%
  mutate(month_on_book = month_dif(vintage, transaction_month)) %>%
  filter(
    !(vintage %in% dates_not_to_consider()) &
      principal_paid != 0 &
      month_on_book <=
      original_term_to_maturity + 2 &
      month_dif(vintage, closing_month()) > original_term_to_maturity + 2
  ) %>%
  group_by(vintage,
           original_term_to_maturity,
           credit_segment,
           vertical) %>%
  transmute(
    month_on_book,
    vintage,
    transaction_month,
    original_term_to_maturity,
    credit_segment,
    vertical,
    principal_paid_share = principal_paid / sum(principal_paid)
  ) %>%
  ungroup()

monthly_charge_off_matrix <-
  monthly_chargeoff %>%
  mutate(month_on_book = month_dif(vintage, charge_off_month)) %>%
  filter(
    !(vintage %in% dates_not_to_consider()) &
      chargeoff_amount != 0 &
      month_on_book <=
      original_term_to_maturity + 4 &
      month_on_book >= 3 &
      month_dif(vintage, closing_month()) > original_term_to_maturity + 4
  ) %>%
  group_by(vintage,
           original_term_to_maturity,
           credit_segment,
           vertical) %>%
  transmute(
    month_on_book,
    vintage,
    charge_off_month,
    original_term_to_maturity,
    credit_segment,
    vertical,
    chargeoff_share = chargeoff_amount / sum(chargeoff_amount)
  ) %>%
  ungroup()

monthly_payments_for_bt <-
  monthly_principal_paid_matrix %>%
  left_join(
    monthly_originations %>%
      select(
        index,
        vintage,
        original_term_to_maturity,
        credit_segment,
        vertical,
        wa_interest_rate
      )
  ) %>%
  left_join(training_set_amortization_table_unit) %>%
  transmute(
    completion_percentage = month_on_book / original_term_to_maturity,
    original_term_to_maturity,
    credit_segment,
    vertical,
    wa_interest_rate,
    principal_amortization =
      replace_na(principal_amortization_share, 0),
    principal_paid_share_delta =
      principal_paid_share - principal_amortization
  )

monthly_chargeoff_for_bt <-
  monthly_charge_off_matrix %>%
  left_join(
    monthly_originations %>%
      select(
        index,
        vintage,
        original_term_to_maturity,
        credit_segment,
        vertical,
        wa_interest_rate
      )
  ) %>%
  left_join(training_set_amortization_table_unit) %>%
  transmute(
    transaction_year = lubridate::year(charge_off_month),
    transaction_month = lubridate::month(charge_off_month),
    vintage_year = lubridate::year(vintage),
    vintage_month = lubridate::month(vintage),
    completion_percentage = month_on_book / original_term_to_maturity,
    credit_segment,
    vertical,
    wa_interest_rate,
    principal_amortization = replace_na(principal_amortization_share, 0),
    chargeoff_share
  )


#### Evaluation set ####
originations_forecast <-
  originations_forecast %>%
  arrange(vintage, vertical, original_term_to_maturity) %>%
  rowid_to_column('index') %>%
  mutate(loan_amount = 1)

plan(multisession, workers = 2)
with_progress({
  origination_schedule <- amortization_mapping(originations_forecast,
                                               originations_forecast$index)
})
plan(sequential)

month_on_book <- tibble(join_column = 1, month_on_book = 0:27)

origination_schedule_unit <-
  origination_schedule %>%
  group_by(index) %>%
  mutate(principal_amortization_share =
           principal_amortization / sum(principal_amortization)) %>%
  ungroup()

monthly_originations_for_bt <-
  monthly_originations %>%
  tibble() %>%
  filter(vintage <= closing_month() &
           vintage >= '2019-11-01') %>%
  transmute(
    index,
    vintage,
    vertical,
    original_term_to_maturity,
    credit_segment,
    wa_interest_rate,
    join_column = 1
  ) %>%
  left_join(month_on_book, by = c('join_column' = 'join_column')) %>%
  left_join(training_set_amortization_table_unit) %>%
  bind_rows(
    originations_forecast %>%
      tibble() %>%
      mutate(vintage = as_date(vintage)) %>%
      filter(vintage > closing_month()) %>%
      transmute(
        index,
        vintage,
        vertical,
        original_term_to_maturity,
        credit_segment,
        wa_interest_rate,
        join_column = 1
      ) %>%
      left_join(month_on_book, by = c('join_column' = 'join_column')) %>%
      left_join(origination_schedule_unit)
  ) %>%
  filter(original_term_to_maturity + 4 >= month_on_book) %>%
  mutate(transaction_month = ymd(vintage) %m+% months(month_on_book)) %>%
  transmute(
    vintage,
    vintage_year = lubridate::year(vintage),
    vintage_month = lubridate::month(vintage),
    months = transaction_month,
    original_term_to_maturity,
    credit_segment,
    vertical,
    transaction_year = lubridate::year(transaction_month),
    transaction_month = lubridate::month(transaction_month),
    vintage_year = lubridate::year(vintage),
    vintage_month = lubridate::month(vintage),
    month_on_book,
    completion_percentage = month_on_book / original_term_to_maturity,
    original_term_to_maturity,
    credit_segment,
    vertical,
    wa_interest_rate,
    principal_amortization = replace_na(principal_amortization_share, 0)
  )
