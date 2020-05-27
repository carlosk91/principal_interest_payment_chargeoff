full_data <-
  full_join(df_pronostico_principalpaid_share,
            df_pronostico_chargeoff_share)

write_csv(full_data,
          'projected_share.csv',
          na = '0')
