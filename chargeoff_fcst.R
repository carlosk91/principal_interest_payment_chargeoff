#### Using Model to forecast ####

evaluation_data <- as.h2o(monthly_originations_for_bt %>%
                            filter(month_on_book >= 4) %>%
                            select(all_of(x)))

predict_charge_off <-
  cbind(monthly_originations_for_bt %>%
          filter(month_on_book >= 4),
        estimated_charge_off_share =
          as.vector(h2o.predict(object = sem_co,
                                newdata = evaluation_data))) %>%
  group_by(vintage,
           product,
           original_term_to_maturity,
           credit_segment,
           vertical) %>%
  transmute(
    vintage,
    months,
    original_term_to_maturity,
    month_on_book,
    credit_segment,
    vertical,
    charge_off_curve = 
      pmax(estimated_charge_off_share, 0) / 
      sum(pmax(0, estimated_charge_off_share)),
    product
  )

update_date <- format(Sys.Date(), '%Y%m%d')
write_csv(predict_charge_off, glue('charge_off_results_{update_date}.csv'))
