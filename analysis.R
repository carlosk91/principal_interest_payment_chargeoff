


#### Principal income ####
{
  ##### Total #####
  
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
  
  
  ##### By vintage #####
  
  monthly_payments_summary_vintage <-
    monthly_payments_summary_fun(
      only_post_term_vintages = T,
      max_months_after_term = 2,
      vintage = vintage
    )
  
  plotly::ggplotly(
    lineplot_principal_paid_share(
      data = monthly_payments_summary_vintage,
      group_var = vintage,
      term = 3
    )
  )
  
  plotly::ggplotly(
    lineplot_principal_paid_share(
      data = monthly_payments_summary_vintage,
      group_var = vintage,
      term = 6
    )
  )
  
  plotly::ggplotly(
    lineplot_principal_paid_share(
      data = monthly_payments_summary_vintage,
      group_var = vintage,
      term = 11
    )
  )
  
  lineplot_principal_paid_share(data = monthly_payments_summary_vintage,
                                group_var = vintage,
                                term = 12)
  
  lineplot_principal_paid_share(data = monthly_payments_summary_vintage,
                                group_var = vintage,
                                term = 12)
  
  ##### By credit segment #####
  
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
  
  ##### By vertical #####
  
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
  
  ##### By original term, credit segment and vertical #####
  
  monthly_payments_summary_credit_vertical <-
    monthly_payments_summary_fun(
      only_post_term_vintages = T,
      max_months_after_term = 2,
      credit_segment = credit_segment,
      vertical = vertical
    ) %>%
    select(
      original_term_to_maturity,
      credit_segment,
      vertical,
      months_in_books,
      principal_paid_share
    ) %>%
    arrange(original_term_to_maturity,
            credit_segment,
            vertical,
            months_in_books)
  
  vintage <- seq(as.Date('2017-01-01'),
                 by = "month",
                 length.out = 67)
  months <- seq(as.Date('2017-01-01'),
                by = "month",
                length.out = 67)
  terms <- c(3, 6, 11, 12, 18, 24)
  
  vintages_mb <-
    as_tibble(expand.grid(vintage, months, terms)) %>%
    rename(
      vintage = Var1,
      months = Var2,
      original_term_to_maturity = Var3
    ) %>%
    mutate(months_in_books = month_diff(vintage, months)) %>%
    filter(vintage <= months,
           original_term_to_maturity + 2 >= months_in_books,
    ) %>%
    arrange(vintage, months, original_term_to_maturity) %>%
    left_join(monthly_payments_summary_credit_vertical) %>%
    filter(!(original_term_to_maturity %in% c(12, 18, 24)))
  
  write_csv(vintages_mb,
            'principal_paid_share.csv')
  
}

#### Principal charge off ####
{
  ##### Total #####
  
  monthly_chargeoff_summary <-
    monthly_records[['monthly_principal_chargeoff']] %>%
    mutate(months_in_books = month_diff(vintage, chargeoffmonth)) %>%
    group_by(original_term_to_maturity,
             months_in_books) %>%
    summarise(chargeoff_principal = sum(chargeoffprincipal, na.rm = T)) %>%
    ungroup() %>%
    group_by(original_term_to_maturity) %>%
    mutate(chargeoff_principal_share = chargeoff_principal /
             sum(chargeoff_principal)) %>%
    ungroup() %>%
    select(-chargeoff_principal) %>%
    as_tibble()
  
  monthly_chargeoff_summary %>%
    filter(original_term_to_maturity %in% c(3,6,11)) %>%
    ggplot(aes(
      x = months_in_books,
      y = chargeoff_principal_share,
      color = as.character(original_term_to_maturity)
    )) +
    geom_line() +
    labs(
      title = 'Principal paid share among the months in books',
      color = 'Term',
      x = 'Months in books',
      y = 'Principal paid share'
    ) +
    scale_y_continuous(labels = scales::percent)
  
  ##### By segment #####
  
  monthly_chargeoff_summary <-
    monthly_records[['monthly_principal_chargeoff']] %>%
    mutate(months_in_books = month_diff(vintage, chargeoffmonth)) %>%
    group_by(original_term_to_maturity,
             months_in_books) %>%
    summarise(chargeoff_principal = sum(chargeoffprincipal, na.rm = T)) %>%
    ungroup() %>%
    group_by(original_term_to_maturity) %>%
    mutate(chargeoff_principal_share = chargeoff_principal /
             sum(chargeoff_principal)) %>%
    ungroup() %>%
    select(-chargeoff_principal)
  
  
}

#### Creating 18 and 24 terms payments ####
{
  
  monthly_payments_for_terms_simulation_18months <- 
    term_simulation_payments(base_term = 11,
                             term_to_simulate = 18)
  
  monthly_payments_for_terms_simulation_24months <- 
    term_simulation_payments(base_term = 11,
                             term_to_simulate = 24)
  
  monthly_payments_for_terms_simulation_18months %>%
    filter(credit_segment == 'Prime') %>%
    ggplot(aes(x = months_in_books,
               y = principal_paid_share,
               color = vertical)) +
    geom_line()  
  
  monthly_payments_for_terms_simulation_24months %>%
    filter(credit_segment == 'Prime') %>%
    ggplot(aes(x = months_in_books,
               y = principal_paid_share,
               color = vertical)) +
    geom_line()
  # 
  # 
  # fitted_monthly_payments_for_terms_simulation <-
  #   monthly_payments_for_terms_simulation %>%
  #   group_by(credit_segment,
  #            vertical) %>%
  #   do(loess_fit = loess(
  #     principal_paid_share ~ months_in_books,
  #     data = .,
  #     span = 0.25
  #   )) %>%
  #   augment(loess_fit)
  # 
  # qplot(
  #   months_in_books,
  #   principal_paid_share,
  #   data =
  #     fitted_monthly_payments_for_terms_simulation %>%
  #     filter(credit_segment == 'NearPrime'),
  #   geom = 'point',
  #   colour = vertical
  # ) +
  #   geom_line(aes(y = .fitted))
  
}

#### Creating 18 and 24 terms charge off ####
{
  
  monthly_chargeoff_for_terms_simulation_18months <- 
    term_simulation_charge_off(base_term = 11,
                             term_to_simulate = 18)
  
  monthly_chargeoff_for_terms_simulation_24months <- 
    term_simulation_charge_off(base_term = 11,
                             term_to_simulate = 24)
  
  monthly_payments_for_terms_simulation_18months %>%
    filter(credit_segment == 'Prime') %>%
    ggplot(aes(x = months_in_books,
               y = ca,
               color = vertical)) +
    geom_line()  
  
  monthly_payments_for_terms_simulation_24months %>%
    filter(credit_segment == 'Prime') %>%
    ggplot(aes(x = months_in_books,
               y = principal_paid_share,
               color = vertical)) +
    geom_line()
  # 
  # 
  # fitted_monthly_payments_for_terms_simulation <-
  #   monthly_payments_for_terms_simulation %>%
  #   group_by(credit_segment,
  #            vertical) %>%
  #   do(loess_fit = loess(
  #     principal_paid_share ~ months_in_books,
  #     data = .,
  #     span = 0.25
  #   )) %>%
  #   augment(loess_fit)
  # 
  # qplot(
  #   months_in_books,
  #   principal_paid_share,
  #   data =
  #     fitted_monthly_payments_for_terms_simulation %>%
  #     filter(credit_segment == 'NearPrime'),
  #   geom = 'point',
  #   colour = vertical
  # ) +
  #   geom_line(aes(y = .fitted))
  
}