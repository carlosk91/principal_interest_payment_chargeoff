SELECT 
bi.loan_id, 
case when subvention_id is not null then 1 else 0 end as SUBVENTION_FLAG,
Date_trunc('month', To_date(cs.origination_date)) AS vintage, 
bi.order_amount                                   AS trip_amount, 
original_term_to_maturity,
TRIP_START_DATE::date -
          REPORT_DATE::date as days_to_travel,
cs.loan_amount as cs_loan_amount,
bi.loan_amount,
cs.DAYS_PAST_DUE,
cs.fico,
CASE 
        WHEN cs.fico < 640 THEN 'SubPrime' 
        WHEN cs.fico >= 640 AND cs.fico <= 710 THEN 'NearPrime' 
        WHEN cs.fico > 710 THEN 'Prime' 
END AS credit_segment, 
CASE 
        WHEN pc.vertical_l1 IN ( 'Air', 'Cruise', 'Package' ) THEN 
        pc.vertical_l1 
        ELSE 'Other' 
END AS VERTICAL,
principal_balance,
ORIGINATION_DATE,
date_trunc('month', ORIGINATION_DATE::date) as monthly_vintage
FROM production_db.PUBLIC.bi_loan bi 
LEFT JOIN production_db.PUBLIC.credit_suisse_feed cs 
                                 ON cs.loan_number = bi.loanid 
                                    AND cs.report_date = Dateadd(day, -1, 
                                                         CURRENT_DATE) 
LEFT JOIN PRODUCTION_DB.PUBLIC.PARTNER_CLASSIFICATION pc 
        ON (bi.UPCODE = pc.UPCODE)
LEFT JOIN PRODUCTION_DB.PUBLIC.BI_ORDERS bi_o
        ON bi_o.APPLICATION_ID = bi.APPLICATION_ID
