seq_months_to_forecast <-
  function(start_date = '2017-01-01',
           end_date = '2022-12-01') {
    seq(from = as.Date(start_date),
        to = as.Date(end_date),
        by = "month")
  }


dates_not_to_consider <- function(to_charge_off = F) {
  dates <-
    as_date(
      c(
        '2017-01-01',
        '2017-12-01',
        '2018-03-01',
        '2018-04-01',
        '2018-05-01',
        '2019-03-01',
        '2019-05-01'
      )
    )
  if (to_charge_off == T) {
    dates <- c(dates, as_date(c('2017-06-01',
                                '2017-08-01')))
  }
  
  return(dates)
}

read_csvs <- function(files_name = NA) {
  if (is.na(files_name)) {
    files_name <- list.files(pattern = '.csv')
  }
  
  csv_files <- lapply(files_name, read_csv)
  names(csv_files) <- gsub(pattern = '.csv',
                           replacement = '',
                           x = tolower(files_name))
  for (i in seq_along(csv_files)) {
    colnames(csv_files[[i]]) <- gsub(
      pattern = '.csv',
      replacement = '',
      x = tolower(colnames(csv_files[[i]]))
    )
  }
  return(csv_files)
}

month_number <- function(date) {
  lt <- as.POSIXlt(as.Date(date, origin = "1900-01-01"))
  lt$year * 12 + lt$mon
}

month_diff <- function(start_date, end_date) {
  month_number(end_date) - month_number(start_date)
}

monthly_payments_summary_fun <-
  function(data = 'monthly_payments',
           max_months_after_term = NA,
           only_post_term_vintages = F,
           ...) {
    quo_group_var <- enquos(...)
    max_months <- case_when(is.na(max_months_after_term) ~ 0,
                            T ~ as.numeric(max_months_after_term))
    
    monthly_records[[data]] %>%
      mutate(months_in_books = month_diff(start_date = vintage,
                                          end_date = payment_month)) %>%
      filter(
        !(vintage %in% dates_not_to_consider()),
        vintage >= as_date('2017-01-01'),
        is.na(principal_paid) == F,
        case_when(
          only_post_term_vintages == T ~
            vintage <=
            max(monthly_records[[data]]$vintage, na.rm = T) %m-%
            months(original_term_to_maturity +
                     max_months),
          only_post_term_vintages == F ~
            T
        ),
        case_when(
          is.na(max_months_after_term) ~ T,
          T ~ months_in_books <=
            original_term_to_maturity + max_months
        )
      ) %>%
      arrange(!!!quo_group_var,
              original_term_to_maturity,
              months_in_books) %>%
      group_by(!!!quo_group_var,
               original_term_to_maturity,
               months_in_books) %>%
      summarise(principal_paid = sum(principal_paid, na.rm = T)) %>%
      mutate(
        principal_paid_share = principal_paid /
          sum(principal_paid),
        principal_paid_cumulative_share = cumsum(principal_paid) /
          sum(principal_paid)
      ) %>%
      ungroup()
  }

lineplot_principal_paid_share <-
  function(data,
           group_var = NULL,
           term = 11) {
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
        x = months_in_books,
        y = principal_paid_share,
        color = as.character(!!group_variable)
      )) +
      geom_line() +
      labs(
        title = 'Principal paid share among the months in books',
        color = deparse(substitute(!!group_variable)),
        x = 'Months in books',
        y = 'Principal paid share'
      ) +
      scale_y_continuous(labels = scales::percent)
    return(plot)
  }

term_simulation_payments <-
  function(base_term,
           term_to_simulate,
           starting_mb_to_long = 2,
           extra_payment_months = 2,
           ending_mb_to_long = NA) {
    if (is.na(ending_mb_to_long)) {
      ending_mb_to_long <- base_term - 1
    }
    
    vertical <- c('Air', 'Cruise', 'Other', 'Package')
    
    monthly_records[['monthly_payments']] %>%
      mutate(months_in_books = month_diff(start_date = vintage,
                                          end_date = payment_month)) %>%
      filter(
        !(vintage %in% dates_not_to_consider()),
        vintage >= as_date('2017-01-01'),
        is.na(principal_paid) == F,
        original_term_to_maturity == base_term,
        vintage <=
          max(monthly_records[['monthly_payments']]$vintage, na.rm = T) %m-%
          months(original_term_to_maturity +
                   extra_payment_months),
        months_in_books <=
          original_term_to_maturity + extra_payment_months
      ) %>%
      arrange(original_term_to_maturity,
              months_in_books,
              credit_segment,
              vertical) %>%
      group_by(original_term_to_maturity,
               months_in_books,
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
        months_in_books =
          case_when(
            months_in_books > ending_mb_to_long ~
              term_to_simulate +
              months_in_books -
              original_term_to_maturity,
            months_in_books < starting_mb_to_long ~ months_in_books,
            T ~ months_in_books *
              (
                1 + (term_to_simulate - original_term_to_maturity) /
                  (ending_mb_to_long + 1)
              )
          ),
        original_term_to_maturity = term_to_simulate
      ) %>%
      arrange(credit_segment,
              vertical,
              months_in_books) %>%
      mutate(
        trend = case_when(
          credit_segment == lead(credit_segment) &
            vertical == lead(vertical) &
            between(months_in_books,
                    starting_mb_to_long - 1,
                    ending_mb_to_long) ~
            (lead(principal_paid_share) -
               principal_paid_share) /
            (lead(months_in_books) - months_in_books)
        ),
        prev_principal_paid_share = principal_paid_share,
        prev_month_in_books = months_in_books
      ) %>%
      bind_rows(
        expand_grid(
          original_term_to_maturity = term_to_simulate,
          months_in_books =
            starting_mb_to_long:(term_to_simulate -
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
              months_in_books) %>%
      fill(trend,
           prev_principal_paid_share,
           prev_month_in_books) %>%
      mutate(
        new_principal_paid_share =
          case_when(
            is.na(principal_paid_share) ~
              (months_in_books - prev_month_in_books) * trend +
              prev_principal_paid_share,
            T ~ principal_paid_share
          )
      ) %>%
      transmute(
        original_term_to_maturity,
        months_in_books,
        credit_segment,
        vertical,
        principal_paid_share = new_principal_paid_share
      ) %>%
      filter(months_in_books %% 1 == 0)  %>%
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
           starting_mb_to_long = 6,
           months_to_charge_off = 6,
           ending_mb_to_long = NA) {
    if (is.na(ending_mb_to_long)) {
      ending_mb_to_long <-
        term_to_simulate + months_to_charge_off - 1
    }
    
    vertical <- c('Air', 'Cruise', 'Other', 'Package')
    
    monthly_records[['monthly_principal_chargeoff']] %>%
      mutate(months_in_books = month_diff(start_date = vintage,
                                          end_date = chargeoffmonth)) %>%
      filter(
        !(vintage %in% dates_not_to_consider(to_charge_off = T)),
        vintage >= as_date('2017-01-01'),
        is.na(chargeoffprincipal) == F,
        original_term_to_maturity == base_term,
        vintage <=
          max(monthly_records[['monthly_principal_chargeoff']]$vintage,
              na.rm = T) %m-%
          months(original_term_to_maturity +
                   months_to_charge_off),
        months_in_books <=
          original_term_to_maturity + months_to_charge_off
      ) %>%
      arrange(original_term_to_maturity,
              months_in_books,
              credit_segment,
              vertical) %>%
      group_by(original_term_to_maturity,
               months_in_books,
               credit_segment,
               vertical) %>%
      summarise(chargeoffprincipal = sum(chargeoffprincipal, na.rm = T)) %>%
      ungroup() %>%
      group_by(original_term_to_maturity,
               credit_segment,
               vertical) %>%
      mutate(chargeoffprincipal_share = chargeoffprincipal /
               sum(chargeoffprincipal)) %>%
      ungroup() %>%
      select(-chargeoffprincipal) %>%
      mutate(
        months_in_books =
          case_when(
            months_in_books > ending_mb_to_long ~
              term_to_simulate +
              months_in_books -
              original_term_to_maturity,
            months_in_books < starting_mb_to_long ~ months_in_books,
            T ~ months_in_books *
              (
                1 + (term_to_simulate - original_term_to_maturity) /
                  (ending_mb_to_long + 1)
              )
          ),
        original_term_to_maturity = term_to_simulate
      ) %>%
      arrange(credit_segment,
              vertical,
              months_in_books) %>%
      mutate(
        trend = case_when(
          credit_segment == lead(credit_segment) &
            vertical == lead(vertical) &
            between(months_in_books,
                    starting_mb_to_long - 1,
                    ending_mb_to_long) ~
            (lead(chargeoffprincipal_share) -
               chargeoffprincipal_share) /
            (lead(months_in_books) - months_in_books)
        ),
        prev_chargeoffprincipal_share = chargeoffprincipal_share,
        prev_month_in_books = months_in_books
      ) %>%
      bind_rows(
        expand_grid(
          original_term_to_maturity = term_to_simulate,
          months_in_books =
            starting_mb_to_long:(term_to_simulate -
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
              months_in_books) %>%
      fill(trend,
           prev_chargeoffprincipal_share,
           prev_month_in_books) %>%
      mutate(
        new_chargeoffprincipal_share =
          case_when(
            is.na(chargeoffprincipal_share) ~
              (months_in_books - prev_month_in_books) * trend +
              prev_chargeoffprincipal_share,
            T ~ chargeoffprincipal_share
          )
      ) %>%
      transmute(
        original_term_to_maturity,
        months_in_books,
        credit_segment,
        vertical,
        charge_off_principal_share =
          case_when(
            new_chargeoffprincipal_share < 0 ~ 0,
            T ~ new_chargeoffprincipal_share
          )
      ) %>%
      filter(months_in_books %% 1 == 0,
             months_in_books <= term_to_simulate + months_to_charge_off)  %>%
      group_by(original_term_to_maturity,
               credit_segment,
               vertical) %>%
      mutate(charge_off_principal_share = charge_off_principal_share /
               sum(charge_off_principal_share))
  }
