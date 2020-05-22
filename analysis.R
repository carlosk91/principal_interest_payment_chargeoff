monthly_records <- read_csvs()

#### Total ####

monthly_payments_summary <-
  monthly_payments_summary_fun(max_months_after_term = 2)


lineplot_principal_paid_share(data = monthly_payments_summary,
                          term = 3)


lineplot_principal_paid_share(data = monthly_payments_summary,
                          term = 6)


lineplot_principal_paid_share(data = monthly_payments_summary,
                          term = 11)


lineplot_principal_paid_share(data = monthly_payments_summary,
                          term = 12)


#### By vintage ####

monthly_payments_summary_vintage <-
  monthly_payments_summary_fun(only_post_term_vintages = T, 
                               max_months_after_term = 2,
                               vintage = vintage)

lineplot_principal_paid_share(data = monthly_payments_summary_vintage,
                          group_var = vintage,
                          term = 3)

lineplot_principal_paid_share(data = monthly_payments_summary_vintage,
                          group_var = vintage,
                          term = 6)

lineplot_principal_paid_share(data = monthly_payments_summary_vintage,
                          group_var = vintage,
                          term = 11)

lineplot_principal_paid_share(data = monthly_payments_summary_vintage,
                          group_var = vintage,
                          term = 12)

#### By credit segment #####

monthly_payments_summary_credit_segment <-
  monthly_payments_summary_fun(max_months_after_term = 2,
                               credit_segment = credit_segment)

lineplot_principal_paid_share(data = monthly_payments_summary_credit_segment,
                          group_var = credit_segment,
                          term = 3)

lineplot_principal_paid_share(data = monthly_payments_summary_credit_segment,
                          group_var = credit_segment,
                          term = 6)

lineplot_principal_paid_share(data = monthly_payments_summary_credit_segment,
                          group_var = credit_segment,
                          term = 11)

lineplot_principal_paid_share(data = monthly_payments_summary_credit_segment,
                          group_var = credit_segment,
                          term = 12)

#### By credit vertical ####

monthly_payments_summary_credit_vertical <-
  monthly_payments_summary_fun(max_months_after_term = 2,
                               vertical = vertical)

lineplot_principal_paid_share(data = monthly_payments_summary_credit_vertical,
                          group_var = vertical,
                          term = 3)

lineplot_principal_paid_share(data = monthly_payments_summary_credit_vertical,
                          group_var = vertical,
                          term = 6)

lineplot_principal_paid_share(data = monthly_payments_summary_credit_vertical,
                          group_var = vertical,
                          term = 11)

lineplot_principal_paid_share(data = monthly_payments_summary_credit_vertical,
                          group_var = vertical,
                          term = 12)

#### By original term, credit segment and vertical ####

monthly_payments_summary_credit_vertical <-
  monthly_payments_summary_fun(max_months_after_term = 2,
                               credit_segment = credit_segment,
                               vertical = vertical) %>%
  select(original_term_to_maturity,
         credit_segment, 
         vertical,
         months_in_books,
         principal_paid_share) %>%
  arrange(original_term_to_maturity,
          credit_segment, 
          vertical,
          months_in_books)
