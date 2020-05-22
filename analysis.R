monthly_records <- read_csvs()

monthly_payments_summary <-
  monthly_payments_summary_fun()

plot_principal_paid_share(data = monthly_payments_summary,
                          term = 11)

monthly_payments_summary_vintage <-
  monthly_payments_summary_fun(vintage = vintage)

plot_principal_paid_share(data = monthly_payments_summary_vintage,
                          group_var = vintage,
                          term = 11)

monthly_payments_summary_credit_segment <-
  monthly_payments_summary_fun(credit_segment = credit_segment)



plot_principal_paid_share(data = monthly_payments_summary_credit_segment,
                          group_var = credit_segment,
                          term = 3)

monthly_payments_summary_vintage_capped <-
  monthly_payments_summary_fun(min_months_of_tenure = 1, vintage = vintage)


plot_principal_paid_share(data = monthly_payments_summary_vintage_capped,
                          group_var = vintage,
                          term = 11)

