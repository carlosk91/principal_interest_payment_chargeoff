
### Loan tapes analysis

vintage_term_summary <- apr_summary()

graph_comparing_monthly_apr <- graph_apr_loans(vintage_term_summary)

set.seed(1234)
sample_paid <-
  loan_tape %>%
  filter(loan_status == 'Fully Paid') %>%
  sample_n(10)

sample_1 <- amort_payments_comparisson(sample_paid[1,])

sample_2 <- amort_payments_comparisson(sample_paid[2,])

sample_3 <- amort_payments_comparisson(sample_paid[3,])

sample_4 <- amort_payments_comparisson(sample_paid[4,])



