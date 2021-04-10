


#### File reading ####
uatp_transactions <-
  get_query(query = 'uatp_transactions.sql',
            dms = 'Snowflake') %>%
  `names<-`(tolower(names(.)))

wex_stripe_transactions <-
  get_query(query = 'wex_stripe_transactions.sql',
            dms = 'Snowflake') %>%
  `names<-`(tolower(names(.)))

loan_data <-
  get_query(query = 'loan_data.sql',
            dms = 'Snowflake') %>%
  `names<-`(tolower(names(.)))

processors_transactions <-
  uatp_transactions %>%
  filter(!is.na(loan_id), !is.na(report_date)) %>%
  bind_rows(wex_stripe_transactions %>%
              filter(!is.na(loan_id), !is.na(report_date))) %>%
  inner_join(
    loan_data %>%
      transmute(
        loan_id,
        vintage,
        original_term_to_maturity,
        credit_segment,
        vertical,
        subvention_flag
      )
  )

#### Functions ####

tbl_join <- function(processed_positive = NA,
                     by_report_month = F,
                     ...) {
  quo_group_var <- enquos(...)
  
  df <-
    processors_transactions %>%
    filter(
      case_when(
        is.na(processed_positive) ~ T,
        processed_positive == T ~ amount > 0,
        processed_positive == F ~ amount < 0
      )
    ) %>%
    group_by(report_month = case_when(
      by_report_month ~
        floor_date(report_date, unit = 'month'),
      T ~ as_date(NA)
    ),!!!quo_group_var) %>%
    summarise(processed_amount = sum(amount)) %>%
    inner_join(
      loan_data %>%
        inner_join(processors_transactions %>%
                     distinct(loan_id, processor)) %>%
        group_by(!!!quo_group_var) %>%
        summarise(loan_amount = sum(loan_amount))
    ) %>%
    mutate(
      disbursement_share = processed_amount / loan_amount,
      month_on_book = case_when(
        by_report_month ~
          month_diff(vintage,
                     report_month),
        T ~ as.numeric(NA)
      )
    )
  
  df[, colSums(is.na(df)) != nrow(df)]
}

#### Data Wrangling ####

disbursement_share <-
  tbl_join(
    processed_positive = T,
    by_report_month = T,
    vintage,
    processor,
    original_term_to_maturity,
    credit_segment,
    vertical,
    subvention_flag
  )

#### Data Analysis ####
{
  plotly::ggplotly(
    tbl_join(
      processed_positive = T,
      by_report_month = F,
      vintage
    ) %>%
      ggplot() +
      geom_line(aes(x = vintage, y = disbursement_share))
  )
  
  plotly::ggplotly(
    tbl_join(
      processed_positive = T,
      by_report_month = F,
      vintage,
      original_term_to_maturity
    ) %>%
      ggplot() +
      geom_line(aes(x = vintage, 
                    y = disbursement_share, 
                    color = as.factor(original_term_to_maturity)))
  )
  
  plotly::ggplotly(
    tbl_join(
      processed_positive = T,
      by_report_month = T,
      vintage
    ) %>%
      filter(vintage >= '2018-01-01') %>%
      ggplot() +
      geom_line(
        aes(
          x = report_month,
          y = disbursement_share,
          color = as.factor(vintage)
        )
      )
  )
  
  plotly::ggplotly(
    tbl_join(
      processed_positive = T,
      by_report_month = F,
      vintage,
      processor
    ) %>%
      ggplot() +
      geom_line(aes(
        x = vintage, y = disbursement_share, color = processor
      ))
  )
  
  plotly::ggplotly(
    tbl_join(
      processed_positive = T,
      by_report_month = F,
      vintage,
      vertical,
      processor
    ) %>%
      ggplot() +
      geom_line(aes(
        x = vintage,
        y = disbursement_share,
        color = glue('{vertical} on {processor}')
      ))
  )
  
  plotly::ggplotly(
    tbl_join(
      processed_positive = T,
      by_report_month = F,
      vintage,
      vertical,
      processor
    ) %>%
      ggplot() +
      geom_line(aes(
        x = vintage,
        y = disbursement_share,
        color = glue('{vertical} on {processor}')
      ))
  )
  
  
  plotly::ggplotly(
    tbl_join(
      processed_positive = T,
      by_report_month = F,
      vintage,
      vertical,
      processor,
      subvention_flag
    ) %>%
      mutate(
        segment =
          glue(
            "{vertical}.{case_when(subvention_flag == 1 ~ 'Sub',
           T ~ 'Int')} on {processor}"
          )
      ) %>%
      ggplot() +
      geom_line(aes(
        x = vintage,
        y = disbursement_share,
        color = segment
      ))
  )
}

#### Model creation ####

#Boosting
Ntrees <- 2000
Idepth <- 7
Shrink <- 0.001
pred.boost <- c()

bt_model_disbursement_share <-
  boost_tree(
    learn_rate = Shrink,
    trees = Ntrees,
    tree_depth = Idepth,
    min_n = 1,
    sample_size = 1,
    mode = "regression"
  ) %>%
  set_engine("xgboost") %>%
  fit(
    disbursement_share ~ .,
    data =
      disbursement_share %>%
      ungroup() %>%
      mutate(
        vintage_year = year(vintage),
        vintage_month = months(vintage),
        year_calendar = year(report_month),
        month_calendar = months(report_month)
      ) %>%
      select(-c(
        report_month,
        loan_amount,
        processed_amount
      ))
  )


vintage <-
  seq_months_to_forecast(start_date = '2019-01-01',
                         end_date = last_vintage_to_fcst())
report_month <-
  seq_months_to_forecast(start_date = '2019-01-01',
                         end_date = last_vintage_to_fcst())
original_term_to_maturity <-
  unique(disbursement_share$original_term_to_maturity)
credit_segment <-
  unique(disbursement_share$credit_segment)
vertical <-
  unique(disbursement_share$vertical)
processor <-
  unique(disbursement_share$processor)
subvention_flag <-
  unique(disbursement_share$subvention_flag)

vintages_mb <-
  as_tibble(
    expand_grid(
      vintage,
      report_month,
      original_term_to_maturity,
      credit_segment,
      vertical,
      processor,
      subvention_flag
    )
  ) %>%
  mutate(
    vintage_year = year(vintage),
    year_calendar = year(report_month),
    vintage_month = months(vintage),
    month_calendar = months(report_month),
    month_on_book = month_diff(vintage, report_month)
  )  %>%
  filter(month_on_book >= 0,
         month_on_book < 3)


pred.boost <-
  unlist(predict(bt_model_disbursement_share,
                 vintages_mb)[['.pred']])

fcst_disb_share <-
  cbind(vintages_mb, disbursement_share_hat = pred.boost) %>%
  transmute(
    report_month,
    vintage,
    processor,
    original_term_to_maturity,
    credit_segment,
    vertical,
    subvention_flag,
    disbursement_share_hat
  )

write_csv(fcst_disb_share, 'disbursement_share_fcst.csv')
