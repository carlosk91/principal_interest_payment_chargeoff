read_csvs <- function() {
  files_name <- list.files(pattern = '.csv')
  
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
      filter(
        original_term_to_maturity == term
      ) %>%
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

# boxplot_principal_paid_share <-
#   function(data,
#            group_var = NULL,
#            term = 11) {
#     # handling NULL name
#     if (missing(group_var)) {
#       group_variable <- ""
#     } else{
#       group_variable <- enquo(group_var)
#     }
#     
#     
#     plot <-
#       data %>%
#       filter(
#         original_term_to_maturity == term
#       ) %>%
#       ggplot(aes(
#         x = months_in_books,
#         y = principal_paid_share,
#         color = as.character(!!group_variable)
#       )) +
#       geom_line() +
#       labs(
#         title = 'Principal paid share among the months in books',
#         color = deparse(substitute(!!group_variable)),
#         x = 'Months in books',
#         y = 'Principal paid share'
#       ) +
#       scale_y_continuous(labels = scales::percent)
#     return(plot)
#   }