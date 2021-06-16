source('~/Documents/git/database_connection/db_connection.R')

last_vintage_to_fcst <- function(limit_year = 2026) {
  as_date(glue('{limit_year}-12-01'))
}

last_month_to_fcst <- function(limit_year = 2026) {
  as_date(glue('{limit_year}-12-01'))
}

closing_month <-
  function(date = floor_date(Sys.Date() -
                               mday(Sys.Date()),
                             unit = 'month')) {
    closing_date <- as_date(date)
    
    return(date)
  }

seq_months_to_forecast <-
  function(start_date = '2017-01-01',
           end_date = last_vintage_to_fcst()) {
    seq(from = as.Date(start_date),
        to = as.Date(end_date),
        by = "month")
  }


dates_not_to_consider <- function(to_charge_off = F) {
  dates <-
    as_date(
      c(
        '2017-01-01',
        '2017-02-01',
        '2017-05-01',
        '2017-07-01',
        '2017-10-01',
        '2017-12-01',
        '2018-03-01',
        '2018-04-01',
        '2018-05-01',
        '2019-03-01',
        '2019-05-01'
      )
    )
  if (to_charge_off == T) {
    dates <- c(dates, as_date(
      c(
        '2017-03-01',
        '2017-04-01',
        '2017-06-01',
        '2017-08-01',
        '2017-09-01',
        '2017-11-01',
        '2018-01-01',
        '2018-11-01',
        '2019-04-01',
        '2019-06-01',
        '2019-07-01',
        '2019-08-01',
        '2019-09-01',
        '2019-10-01',
        '2019-11-01',
        '2019-12-01',
        '2020-01-01',
        '2020-02-01'
      )
    ))
  }
  
  return(dates)
}

month_seq <- function(start_date, term) {
  add_with_rollback(as_date(start_date), months(1:term))
}

month_dif <- function(start_date, end_date) {
  x <- interval(start_date,
                end_date) %/% months(1)
  return(x)
}

replace_nan <- function(x, replace_with) {
  x <- if (is.na(x)) {
    replace_with
  } else{
    x
  }
  return(x)
}

col_names_cleaning <- function(data) {
  data %>%
    rename(setNames(names(.), tolower(gsub(" ", "_", names(
      .
    )))))
}

amortization_table <- function(data, index_set) {
  if (data %>%
      filter(index == index_set) %$%
      wa_interest_rate > 0) {
    data %>%
      filter(index == index_set) %>%
      do(as.data.frame(
        amort.table(
          Loan = .$loan_amount,
          n = .$original_term_to_maturity,
          i = .$wa_interest_rate
        )$Schedule
      )) %>%
      mutate(index_set,
             month_on_book = as.numeric(rownames(.))) %>%
      col_names_cleaning() %>%
      select(index = index_set,
             month_on_book,
             principal_amortization = principal_paid)
  } else if (data %>%
             filter(index == index_set) %$%
             wa_interest_rate == 0) {
    data %>%
      filter(index == index_set) %>%
      mutate(term_temp = original_term_to_maturity) %>%
      uncount(original_term_to_maturity) %>%
      transmute(
        index,
        month_on_book = row_number(),
        principal_amortization = loan_amount / term_temp
      )
  }
}

handlers("progress")
amortization_mapping <- function(data, x) {
  p <- progressor(steps = length(x))
  
  future_map_dfr(x, ~ {
    p()
    amortization_table(data, .x)
  }, options(future.debug = F))
}

monthly_payments_summary_fun <-
  function(data = monthly_cashflows,
           max_months_after_term = NA,
           only_post_term_vintages = F,
           with_subvention = F,
           all_dates = F,
           variable_used = principal_paid,
           ...) {
    quo_group_var <- enquos(...)
    max_months <- case_when(is.na(max_months_after_term) ~ 0,
                            T ~ as.numeric(max_months_after_term))
    
    data %>%
      mutate(months_on_books = month_diff(start_date = vintage,
                                          end_date = transaction_month)) %>%
      filter(
        case_when(all_dates ~ T,
                  T ~ !(vintage %in% dates_not_to_consider())),
        vintage >= as_date('2017-01-01'),
        case_when(with_subvention ~ T,
                  T ~ subvention_flag == 0),
        is.na(!!enquo(variable_used)) == F,
        case_when(
          only_post_term_vintages == T ~
            vintage <=
            max(data$vintage, na.rm = T) %m-%
            months(original_term_to_maturity +
                     max_months),
          only_post_term_vintages == F ~
            T
        ),
        case_when(
          is.na(max_months_after_term) ~ T,
          T ~ months_on_books <=
            original_term_to_maturity + max_months
        )
      ) %>%
      arrange(!!!quo_group_var,
              original_term_to_maturity,
              months_on_books) %>%
      group_by(!!!quo_group_var,
               original_term_to_maturity,
               months_on_books) %>%
      summarise(amount_paid = sum(!!enquo(variable_used), na.rm = T)) %>%
      mutate(
        amount_paid_share = amount_paid /
          sum(amount_paid),
        amount_paid_cumulative_share = cumsum(amount_paid) /
          sum(amount_paid)
      ) %>%
      ungroup()
  }

lineplot_principal_paid_share <-
  function(data,
           group_var = NULL,
           term = 11,
           var = 'Principal') {
    # handling NULL name
    if (missing(group_var)) {
      group_variable <- ""
    } else{
      group_variable <- enquo(group_var)
    }
    
    
    plot <-
      data %>%
      filter(original_term_to_maturity == term) %>%
      ggplot(aes(
        x = months_on_books,
        y = amount_paid_share,
        color = as.character(!!group_variable)
      )) +
      geom_line() +
      labs(
        title = glue('{var} paid share among the months in books'),
        color = glue('{term} term loans'),
        x = 'Months on books',
        y = glue('{var} paid share')
      ) +
      scale_y_continuous(labels = scales::percent)
    return(plot)
  }

term_simulation_payments <-
  function(base_term,
           term_to_simulate,
           starting_mb_to_extend = 1,
           extra_payment_months = 2,
           ending_mb_to_long = NA) {
    if (is.na(ending_mb_to_long)) {
      ending_mb_to_long <- base_term - 1
    }
    
    vertical <- c('Air', 'Cruise', 'Other', 'Package')
    
    monthly_cashflows %>%
      mutate(months_on_books = month_diff(start_date = vintage,
                                          end_date = transaction_month)) %>%
      filter(
        !(vintage %in% dates_not_to_consider()),
        vintage >= as_date('2017-01-01'),
        is.na(principal_paid) == F,
        original_term_to_maturity == base_term,
        vintage <=
          max(monthly_cashflows$vintage, na.rm = T) %m-%
          months(original_term_to_maturity +
                   extra_payment_months),
        months_on_books <=
          original_term_to_maturity + extra_payment_months
      ) %>%
      arrange(original_term_to_maturity,
              months_on_books,
              credit_segment,
              vertical) %>%
      group_by(original_term_to_maturity,
               months_on_books,
               credit_segment,
               vertical) %>%
      summarise(principal_paid = sum(principal_paid, na.rm = T)) %>%
      ungroup() %>%
      group_by(original_term_to_maturity,
               credit_segment,
               vertical) %>%
      mutate(principal_paid_share = principal_paid /
               sum(principal_paid)) %>%
      ungroup() %>%
      select(-principal_paid) %>%
      mutate(
        months_on_books =
          case_when(
            months_on_books > ending_mb_to_long ~
              term_to_simulate +
              months_on_books -
              original_term_to_maturity,
            months_on_books < starting_mb_to_extend ~ months_on_books,
            T ~ months_on_books *
              (
                1 + (term_to_simulate - original_term_to_maturity) /
                  (ending_mb_to_long + 1)
              )
          ),
        original_term_to_maturity = term_to_simulate
      ) %>%
      arrange(credit_segment,
              vertical,
              months_on_books) %>%
      mutate(
        trend = case_when(
          credit_segment == lead(credit_segment) &
            vertical == lead(vertical) &
            between(
              months_on_books,
              starting_mb_to_extend - 1,
              ending_mb_to_long
            ) ~
            (lead(principal_paid_share) -
               principal_paid_share) /
            (lead(months_on_books) - months_on_books)
        ),
        prev_principal_paid_share = principal_paid_share,
        prev_month_on_books = months_on_books
      ) %>%
      bind_rows(
        expand_grid(
          original_term_to_maturity = term_to_simulate,
          months_on_books =
            starting_mb_to_extend:(term_to_simulate -
                                     base_term +
                                     ending_mb_to_long),
          credit_segment =
            c('Prime', 'NearPrime', 'SubPrime'),
          vertical =
            c('Air', 'Cruise', 'Other', 'Package'),
          principal_paid_share = NA
        )
      ) %>%
      arrange(credit_segment,
              vertical,
              months_on_books) %>%
      fill(trend,
           prev_principal_paid_share,
           prev_month_on_books) %>%
      mutate(
        new_principal_paid_share =
          case_when(
            is.na(principal_paid_share) ~
              (months_on_books - prev_month_on_books) * trend +
              prev_principal_paid_share,
            T ~ principal_paid_share
          )
      ) %>%
      transmute(
        original_term_to_maturity,
        months_on_books,
        credit_segment,
        vertical,
        principal_paid_share = new_principal_paid_share
      ) %>%
      filter(months_on_books %% 1 == 0)  %>%
      group_by(original_term_to_maturity,
               credit_segment,
               vertical) %>%
      mutate(
        principal_paid_share = principal_paid_share /
          sum(principal_paid_share),
        principal_paid_cumulative_share =
          cumsum(principal_paid_share) - principal_paid_share
      )
  }

term_simulation_charge_off <-
  function(base_term,
           term_to_simulate,
           starting_mb_to_extend = 6,
           months_to_charge_off = 4,
           ending_mb_to_long = NA) {
    if (is.na(ending_mb_to_long)) {
      ending_mb_to_long <-
        term_to_simulate + months_to_charge_off - 1
    }
    
    vertical <- c('Air', 'Cruise', 'Other', 'Package')
    
    monthly_cashflows %>%
      mutate(months_on_books = month_diff(start_date = vintage,
                                          end_date = chargeoffmonth)) %>%
      filter(
        !(vintage %in% dates_not_to_consider(to_charge_off = T)),
        vintage >= as_date('2017-01-01'),
        is.na(chargeoffprincipal) == F,
        original_term_to_maturity == base_term,
        vintage <=
          max(monthly_cashflows$vintage,
              na.rm = T) %m-%
          months(original_term_to_maturity +
                   months_to_charge_off),
        months_on_books <=
          original_term_to_maturity + months_to_charge_off
      ) %>%
      arrange(original_term_to_maturity,
              months_on_books,
              credit_segment) %>%
      group_by(original_term_to_maturity,
               months_on_books,
               credit_segment) %>%
      summarise(chargeoffprincipal = sum(chargeoffprincipal, na.rm = T)) %>%
      ungroup() %>%
      group_by(original_term_to_maturity,
               credit_segment) %>%
      mutate(chargeoffprincipal_share = chargeoffprincipal /
               sum(chargeoffprincipal)) %>%
      ungroup() %>%
      select(-chargeoffprincipal) %>%
      mutate(
        months_on_books =
          case_when(
            months_on_books > ending_mb_to_long ~
              term_to_simulate +
              months_on_books -
              original_term_to_maturity,
            months_on_books < starting_mb_to_extend ~ months_on_books,
            T ~ months_on_books *
              (
                1 + (term_to_simulate - original_term_to_maturity) /
                  (ending_mb_to_long + 1)
              )
          ),
        original_term_to_maturity = term_to_simulate
      ) %>%
      arrange(credit_segment,
              months_on_books) %>%
      mutate(
        trend = case_when(
          credit_segment == lead(credit_segment) &
            between(
              months_on_books,
              starting_mb_to_extend - 1,
              ending_mb_to_long
            ) ~
            (lead(chargeoffprincipal_share) -
               chargeoffprincipal_share) /
            (lead(months_on_books) - months_on_books)
        ),
        prev_chargeoffprincipal_share = chargeoffprincipal_share,
        prev_month_on_books = months_on_books
      ) %>%
      left_join(as_tibble(
        cbind(
          original_term_to_maturity = term_to_simulate,
          vertical =
            c('Air', 'Cruise', 'Other', 'Package')
        )
      ) %>%
        mutate(original_term_to_maturity =
                 as.double(original_term_to_maturity))) %>%
      bind_rows(
        expand_grid(
          original_term_to_maturity = term_to_simulate,
          months_on_books =
            starting_mb_to_extend:(term_to_simulate -
                                     base_term +
                                     ending_mb_to_long),
          credit_segment =
            c('Prime', 'NearPrime', 'SubPrime'),
          vertical =
            c('Air', 'Cruise', 'Other', 'Package'),
          chargeoffprincipal_share = NA
        )
      ) %>%
      arrange(credit_segment,
              vertical,
              months_on_books) %>%
      fill(trend,
           prev_chargeoffprincipal_share,
           prev_month_on_books) %>%
      mutate(
        new_chargeoffprincipal_share =
          case_when(
            is.na(chargeoffprincipal_share) ~
              (months_on_books - prev_month_on_books) * trend +
              prev_chargeoffprincipal_share,
            T ~ chargeoffprincipal_share
          )
      ) %>%
      transmute(
        original_term_to_maturity,
        months_on_books,
        credit_segment,
        vertical,
        charge_off_principal_share =
          case_when(
            new_chargeoffprincipal_share < 0 ~ 0,
            T ~ new_chargeoffprincipal_share
          )
      ) %>%
      filter(months_on_books %% 1 == 0,
             months_on_books <= term_to_simulate + months_to_charge_off)  %>%
      group_by(original_term_to_maturity,
               credit_segment,
               vertical) %>%
      mutate(charge_off_principal_share = charge_off_principal_share /
               sum(charge_off_principal_share))
  }


create_amort_table <- function(data) {
  amort_table <-
    amort.table(
      Loan = data$loan_amount,
      n = data$original_term_to_maturity,
      i = data$original_interest_rate / 100,
      ic = 12,
      pf = 12
    )$Schedule %>%
    as_tibble() %>%
    transmute(
      loan_id = data$loan_id,
      loanamount = data$loan_amount,
      borrowerinterestrate = data$original_interest_rate,
      term = data$original_term_to_maturity,
      tenure = round(Year * 12),
      amort_payment = Payment,
      amort_principal_paid = `Principal Paid`,
      amort_interest_paid = `Interest Paid`
    )
  
  return(amort_table)
}

create_real_payments <- function(data) {
  real_payments <- data %>%
    transmute(
      loan_id,
      loan_amount,
      original_term_to_maturity,
      original_interest_rate,
      monthlyvintage = vintage,
      current_tenure = month_diff(
        start_date = monthlyvintage,
        end_date = as_date(closing_month())
      ),
      apr
    ) %>%
    inner_join(
      loan_tapes[["loans_payments_by_month"]] %>%
        transmute(
          loan_id,
          transaction_month = as_date(transaction_month),
          principal_paid,
          interest_paid
        )
    )
  
  real_payments %<>%
    mutate(tenure =
             month_diff(start_date = monthlyvintage,
                        end_date = transaction_month)) %>%
    arrange(transaction_month)
  
  return(real_payments)
}

amort_payments_comparisson <- function(data) {
  create_amort_table(data) %>%
    left_join(create_real_payments(data)) %>%
    select(
      loan_id,
      original_term_to_maturity,
      loan_amount,
      original_interest_rate,
      tenure,
      amort_payment,
      amort_principal_paid,
      principal_paid,
      amort_interest_paid,
      interest_paid
    )
}

amount_paid_by_loan <- function() {
  loan_tapes[["loans_payments_by_month"]] %>%
    group_by(loanid) %>%
    summarise(
      principal_paid = sum(principal_paid),
      interest_paid = sum(interest_paid)
    ) %>%
    ungroup()
}

apr_summary <- function(paid_loans = T,
                        at_least_2_months = T) {
  loan_tape %>%
    filter(case_when(paid_loans ~ loanstatus == 'Fully Paid',
                     T ~ T)) %>%
    transmute(
      loanid,
      loan_amount,
      original_term_to_maturity,
      monthlyvintage = vintage,
      current_tenure = month_diff(
        start_date = monthlyvintage,
        end_date = max(monthlyvintage)
      ),
      apr
    ) %>%
    inner_join(amount_paid_by_loan()) %>%
    group_by(monthlyvintage,
             current_tenure,
             term) %>%
    summarise(
      weighted_registered_apr =
        sum(loanamount * apr) /
        sum(loanamount) / 100,
      weighted_charged_apr =
        12 * sum(interest_paid, na.rm = T) /
        sum(loanamount) /
        mean(case_when(
          term > current_tenure ~ current_tenure,
          T ~ term
        )),
      loans = n()
    ) %>%
    pivot_longer(
      -c(monthlyvintage,
         current_tenure,
         term),
      names_to = "variable",
      values_to = "values"
    ) %>%
    filter(case_when(at_least_2_months ~ current_tenure > 1,
                     T ~ T))
}

graph_apr_loans <-
  function(data,
           variables = c('weighted_charged_apr',
                         'weighted_registered_apr')) {
    graph <-
      data %>%
      filter(variable %in% variables) %>%
      mutate(group = paste0(term, ', ', variable)) %>%
      ggplot(aes(x = monthlyvintage,
                 y = values,
                 color = group)) +
      geom_line() +
      labs(
        title = 'Comparisson between real APR and scheduled APR',
        color = 'Term and APR',
        x = 'Vintage',
        y = 'APR'
      ) +
      scale_y_continuous(labels = scales::percent)
  }

apr_data <- function(data) {
  apr_amort <-
    12 * sum(amort_payments_comparisson(data)$amort_interest_paid,
             na.rm = T) /
    data$loanamount /
    data$term
  
  apr_paid <-
    12 * sum(amort_payments_comparisson(data)$interest_paid, na.rm = T) /
    data$loanamount /
    data$term
  
  return(tibble(amort = apr_amort, paid = apr_paid))
}


monthly_chargeoff_vintage_summary <- function(term_filter = 3) {
  monthly_records[['monthly_principal_chargeoff']] %>%
    filter(original_term_to_maturity == term_filter,
           !(vintage %in% dates_not_to_consider())) %>%
    mutate(months_on_books = month_diff(vintage, chargeoffmonth)) %>%
    group_by(vintage,
             months_on_books) %>%
    summarise(chargeoff_principal = sum(chargeoffprincipal, na.rm = T)) %>%
    ungroup() %>%
    group_by(vintage) %>%
    mutate(chargeoff_principal_share = chargeoff_principal /
             sum(chargeoff_principal)) %>%
    ungroup() %>%
    select(-chargeoff_principal) %>%
    as_tibble()
}


graph_charge_off_vintage <- function(term_filter) {
  monthly_chargeoff_vintage_summary(term_filter) %>%
    ggplot(aes(
      x = months_on_books,
      y = chargeoff_principal_share,
      color = as.character(vintage)
    )) +
    geom_line() +
    labs(
      title = 'Charge off share among the months in books of {term_filter} month term loans',
      color = 'Vintages',
      x = 'Months on books',
      y = 'Charge off share'
    ) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(breaks = round(seq(1, 25, by = 1), 1))
}

principalpaid_df_bt <- function(first_vintage = '2017-01-01',
                                additional_months = 2) {
  monthly_cashflows %>%
    mutate(
      months_on_books = month_diff(start_date = vintage,
                                   end_date = transaction_month),
      vintage_year = year(vintage),
      year_calendar = year(transaction_month),
      vintage_month  = months(vintage),
      month_calendar = months(transaction_month)
    ) %>%
    filter(
      !(vintage %in% dates_not_to_consider()),
      vintage >= as_date(first_vintage),
      is.na(principal_paid) == F,
      original_term_to_maturity %in% c(3, 6, 11),
      vintage <=
        max(monthly_cashflows$vintage, na.rm = T) %m-%
        months(original_term_to_maturity +
                 additional_months),
      months_on_books <=
        original_term_to_maturity + additional_months
    ) %>%
    arrange(
      vintage_year,
      year_calendar,
      vintage_month,
      month_calendar,
      original_term_to_maturity,
      months_on_books,
      credit_segment,
      vertical
    ) %>%
    group_by(
      vintage_year,
      year_calendar,
      vintage_month,
      month_calendar,
      original_term_to_maturity,
      months_on_books,
      credit_segment,
      vertical
    ) %>%
    summarise(principal_paid = sum(principal_paid, na.rm = T)) %>%
    ungroup() %>%
    group_by(vintage_year,
             vintage_month,
             original_term_to_maturity,
             credit_segment,
             vertical) %>%
    mutate(principal_paid_share = principal_paid /
             sum(principal_paid)) %>%
    ungroup() %>%
    select(-c(principal_paid, vintage_month))
}

chargeoff_df_bt <- function(first_vintage = as_date('2017-01-01'),
                            additional_months = 4) {
  monthly_records[['monthly_principal_chargeoff']] %>%
    mutate(
      chargeoffmonth =
        case_when(
          vintage == chargeoffmonth ~
            chargeoffmonth %m+% months(1),
          T ~ chargeoffmonth
        ),
      months_on_books =
        month_diff(start_date = vintage,
                   end_date = chargeoffmonth),
      vintage_year =
        year(vintage),
      year_calendar =
        year(chargeoffmonth),
      vintage_month  = months(vintage),
      month_calendar =
        months(chargeoffmonth)
    ) %>%
    filter(
      !(vintage %in% dates_not_to_consider(to_charge_off = T)),
      vintage >= first_vintage,
      is.na(chargeoffprincipal) == F,
      original_term_to_maturity %in% c(3, 6, 11),
      vintage <=
        max(monthly_records[['monthly_principal_chargeoff']]$vintage,
            na.rm = T) %m-%
        months(original_term_to_maturity +
                 additional_months),
      months_on_books <=
        original_term_to_maturity + additional_months
    ) %>%
    arrange(
      vintage_year,
      year_calendar,
      original_term_to_maturity,
      months_on_books,
      credit_segment,
      vertical
    ) %>%
    group_by(
      vintage_year,
      year_calendar,
      original_term_to_maturity,
      months_on_books,
      credit_segment,
      vertical
    ) %>%
    summarise(chargeoffprincipal_sum = sum(chargeoffprincipal, na.rm = T)) %>%
    ungroup() %>%
    group_by(vintage_year,
             original_term_to_maturity,
             credit_segment,
             vertical) %>%
    mutate(charge_off_principal_share = chargeoffprincipal_sum /
             sum(chargeoffprincipal_sum)) %>%
    filter(!(n() <= 1)) %>%
    ungroup() %>%
    select(-c(chargeoffprincipal_sum))
  
}

charge_off_summary_by_segment <- function(term = 11) {
  monthly_records[['monthly_principal_chargeoff']] %>%
    filter(original_term_to_maturity == term) %>%
    mutate(months_in_books = month_diff(vintage, chargeoffmonth)) %>%
    group_by(credit_segment,
             months_in_books) %>%
    summarise(chargeoff_principal = sum(chargeoffprincipal, na.rm = T)) %>%
    ungroup() %>%
    group_by(credit_segment) %>%
    mutate(chargeoff_principal_share = chargeoff_principal /
             sum(chargeoff_principal)) %>%
    ungroup() %>%
    select(-chargeoff_principal) %>%
    as_tibble()
}
