
#### Training set ####

monthly_originations <-
  monthly_originations %>% 
  rowid_to_column('index')

with_progress({
  training_set_amortization_table <- 
    amortization_mapping(monthly_originations, monthly_originations$index)
})
plan(sequential)

monthly_principal_paid_matrix <- 
  monthly_cashflows %>%
  filter(!(vintage %in% dates_not_to_consider()) & 
           principal_paid != 0 & 
           month_dif(vintage, closing_month()) > 
            original_term_to_maturity + 2) %>%
  mutate(month_on_book = month_dif(vintage, transaction_month)) %>%
  group_by(vintage, 
           original_term_to_maturity, 
           credit_segment, 
           vertical) %>%
  transmute(month_on_book,
            vintage, 
            transaction_month,
            original_term_to_maturity, 
            credit_segment, 
            vertical, 
            principal_paid_share = principal_paid / sum(principal_paid)) %>%
  ungroup()

monthly_charge_off_matrix <- 
  monthly_chargeoff %>%
  filter(!(vintage %in% dates_not_to_consider()) & 
           chargeoff_amount != 0 & 
           month_dif(vintage, closing_month()) >= 
           original_term_to_maturity + 3) %>%
  mutate(month_on_book = month_dif(vintage, charge_off_month)) %>%
  group_by(vintage, 
           original_term_to_maturity, 
           credit_segment, 
           vertical) %>%
  transmute(month_on_book,
            vintage, 
            charge_off_month,
            original_term_to_maturity, 
            credit_segment, 
            vertical, 
            chargeoff_share = chargeoff_amount / sum(chargeoff_amount)) %>%
  ungroup()

monthly_payments_for_bt <- 
  monthly_principal_paid_matrix %>%
  left_join(monthly_originations %>%
              select(index,
                     vintage, 
                     original_term_to_maturity, 
                     credit_segment, 
                     vertical,
                     wa_interest_rate)) %>%
  left_join(training_set_amortization_table) %>%
  transmute(
    transaction_year = lubridate::year(transaction_month), 
    transaction_month = lubridate::month(transaction_month),
    vintage_year = lubridate::year(vintage), 
    vintage_month = lubridate::month(vintage),
    completion_percentage = month_on_book/original_term_to_maturity, 
    original_term_to_maturity, 
    credit_segment, 
    vertical,
    wa_interest_rate,
    principal_amortization = replace_na(principal_amortization, 0),
    principal_paid_share_delta = principal_paid_share - principal_amortization)

monthly_chargeoff_for_bt <- 
  monthly_charge_off_matrix %>%
  left_join(monthly_originations %>%
              select(index,
                     vintage, 
                     original_term_to_maturity, 
                     credit_segment, 
                     vertical,
                     wa_interest_rate)) %>%
  left_join(training_set_amortization_table) %>%
  transmute(
    transaction_year = lubridate::year(charge_off_month), 
    transaction_month = lubridate::month(charge_off_month),
    vintage_year = lubridate::year(vintage), 
    vintage_month = lubridate::month(vintage),
    completion_percentage = month_on_book/original_term_to_maturity, 
    original_term_to_maturity, 
    credit_segment, 
    vertical,
    wa_interest_rate,
    principal_amortization = replace_na(principal_amortization, 0),
    chargeoff_share)


#### Evaluation set ####
originations_forecast <- 
  originations_forecast %>% 
  rowid_to_column('index') %>%
  mutate(loan_amount = 1)

originations_to_process <-
  (originations_forecast %>%
     select(index))[[1]]


with_progress({
  origination_schedule <- amortization_mapping(originations_forecast, 
                                               originations_forecast$index)
})
plan(sequential)

month_on_book <- tibble(join_column = 1, month_on_book = 0:27)

monthly_originations_for_bt <- 
  monthly_originations %>% 
  filter(vintage <= closing_month()) %>%
  mutate(join_column = 1) %>%
  bind_rows(originations_forecast %>% 
              filter(vintage > closing_month()) %>%
              mutate(join_column = 1, vintage = as_date(vintage))) %>%
  left_join(month_on_book, by =c('join_column' = 'join_column')) %>%
  filter(original_term_to_maturity + 3 >= month_on_book) %>%
  left_join(origination_schedule) %>%
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
    completion_percentage = month_on_book/original_term_to_maturity,
    original_term_to_maturity, 
    credit_segment, 
    vertical,
    wa_interest_rate,
    principal_amortization = replace_na(principal_amortization, 0))



