full_data <-
  full_join(df_pronostico_principalpaid_share,
            df_pronostico_chargeoff_share) %>%
  left_join(interest_rate) %>%
  arrange(vintage, credit_segment, vertical, months, original_term_to_maturity)

write_csv(full_data,
          'projected_share.csv',
          na = '0')
