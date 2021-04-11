#### Using Model to forecast ####

evaluation_data <- as.h2o(monthly_originations_for_bt %>%
                            filter(original_term_to_maturity + 2 >= 
                                     month_on_book) %>%
                            select(all_of(x)))

predict_principal_paid <-
  cbind(monthly_originations_for_bt %>%
          filter(original_term_to_maturity + 2 >= 
                   month_on_book),
        estimated_principal_paid_delta =
          as.vector(h2o.predict(object = sem,
                                newdata = evaluation_data))) %>%
  group_by(vintage,
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
    principal_paid_curve = 
      case_when(principal_amortization + estimated_principal_paid_delta < 0 ~ 0,
                principal_amortization + estimated_principal_paid_delta > 0 ~ 
                  principal_amortization + estimated_principal_paid_delta
                ),
    principal_paid_curve = 
                principal_paid_curve / sum(principal_paid_curve)
  )

update_date <- format(Sys.Date(), '%Y%m%d')
write_csv(predict_principal_paid, glue('principal_paid_results_{update_date}.csv'))
