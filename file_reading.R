monthly_records <- read_csvs(files_name = c('monthly_originations.csv',
                               'monthly_payments.csv',
                               'monthly_principal_chargeoff.csv')
                             )

files_path <- "D:/Carlos' things/Uplift/Loan tapes/"

loan_tapes <- read_csvs(file_path = files_path)

interest_rate <- read_csv('interest_rate.csv')
