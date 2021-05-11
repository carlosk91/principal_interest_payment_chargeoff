#### Querying database ####
con <- connect_to_snowflake(method = 'quantrix')
# con <- DBI::dbConnect(
#   drv = odbc::odbc() ,
#   driver = keys$uplift_snowflake$driver,
#   server = keys$uplift_snowflake$server,
#   uid = keys$uplift_snowflake_quantrix$uid,
#   pwd = keys$uplift_snowflake_quantrix$pwd
# )

monthly_originations <- 
  get_query(con = con,
            query = 'monthly_originations.sql', 
            dms = 'Snowflake',
            exit_after_fetch = F) %>%
  col_names_cleaning()

monthly_chargeoff <- 
  get_query(con = con,
            query = 'monthly_charge_off.sql', 
            dms = 'Snowflake',
            exit_after_fetch = F) %>%
  col_names_cleaning()

monthly_cashflows <- 
  get_query(con = con,
            query = 'monthly_cashflows.sql', 
            dms = 'Snowflake',
            exit_after_fetch = F) %>%
  col_names_cleaning()

loan_tape <- 
  get_query(con = con,
            query = 'loan_tape.sql', 
            dms = 'Snowflake',
            exit_after_fetch = F) %>%
  col_names_cleaning()

refunds <- 
  get_query(con = con,
            query = 'refunds.sql', 
            dms = 'Snowflake',
            exit_after_fetch = F) %>%
  col_names_cleaning()

days_to_travel_stats <-  
  get_query(con = con,
            query = 'days_to_travel_stats.sql', 
            dms = 'Snowflake',
            exit_after_fetch = F) %>%
  col_names_cleaning()

covid_monthly_cases <- 
  read_csv('case_daily_trends_united_states.csv') %>%
  mutate(date = mdy(date),
         refund_month = floor_date(date, 'month')) %>%
  filter(date < floor_date(Sys.Date(), 'month')) %>%
  group_by(refund_month) %>%
  summarise(covid_monthly_cases = sum(new_cases))

refunds_df <- 
  refunds %>%
  left_join(days_to_travel_stats) %>%
  left_join(covid_monthly_cases) %>%
  mutate(covid_monthly_cases = replace_na(covid_monthly_cases, 0))

originations_forecast <-
  get_query(con = con,
            query = 'originations_forecast.sql', 
            dms = 'Snowflake') %>%
  col_names_cleaning()
  
