SELECT 
DATE_TRUNC('month', to_date(cs.ORIGINATION_DATE)) AS VINTAGE,
(case when subvention_id IS NOT NULL then 'Sub' else 'IB' end) as PRODUCT,
DATE_TRUNC('month', to_date(left(TRANSACTION_DATE, 10))) as TRANSACTION_MONTH,
cs.ORIGINAL_TERM_TO_MATURITY,
CASE
    WHEN cs.FICO < 640
        THEN 'SubPrime'
    WHEN cs.FICO >= 640
        AND cs.FICO <= 710
        THEN 'NearPrime'
    WHEN cs.FICO > 710
        THEN 'Prime'
END AS CREDIT_SEGMENT,
CASE
    WHEN pc.VERTICAL_L1 IN ('Air','Cruise','Package')
        THEN pc.VERTICAL_L1
    ELSE 'Other'
END AS VERTICAL,
SUM(CASE 
    WHEN transaction_code in ('503', '524') THEN -transaction_amount 
    WHEN transaction_code in ('502', '525') THEN transaction_amount 
    END) as origination_fee,
SUM(CASE 
    WHEN transaction_code in ('205', '221') AND PAYMENT_METHOD_NO NOT IN (7, 8, 11, 15) THEN -transaction_amount 
    WHEN transaction_code in ('204', '220') AND PAYMENT_METHOD_NO NOT IN (7, 8, 11, 15) THEN transaction_amount 
    END) as principal_paid,
SUM(CASE 
    WHEN transaction_code in ('207', '223') AND PAYMENT_METHOD_NO NOT IN (7, 8, 11, 15) THEN -transaction_amount 
    WHEN transaction_code in ('206', '222') AND PAYMENT_METHOD_NO NOT IN (7, 8, 11, 15) THEN transaction_amount 
    END) as interest_paid ,
SUM(CASE 
    WHEN transaction_code in ('205', '221') AND PAYMENT_METHOD_NO IN (7, 8, 11, 15) THEN -transaction_amount 
    WHEN transaction_code in ('204', '220') AND PAYMENT_METHOD_NO IN (7, 8, 11, 15) THEN transaction_amount 
    END) as refunds,
SUM(CASE 
    WHEN transaction_code in ('441') AND PAYMENT_METHOD_NO NOT IN (7, 8, 11, 15) THEN -transaction_amount 
    WHEN transaction_code in ('440') AND PAYMENT_METHOD_NO NOT IN (7, 8, 11, 15) THEN transaction_amount 
    END) as principal_charge_off,
SUM(CASE 
    WHEN transaction_code in ('445') AND PAYMENT_METHOD_NO NOT IN (7, 8, 11, 15) THEN -transaction_amount 
    WHEN transaction_code in ('444') AND PAYMENT_METHOD_NO NOT IN (7, 8, 11, 15) THEN transaction_amount 
    END) as interest_charge_off,
SUM(cs.ORIGINAL_INTEREST_RATE * cs.LOAN_AMOUNT) / SUM(cs.LOAN_AMOUNT) AS wa_interest_rate
FROM PRODUCTION_DB.PUBLIC.NLS_TRANSACTIONS lt
JOIN PRODUCTION_DB.PUBLIC.BI_LOAN bi ON lt.LOAN_NUMBER = bi.LOAN_ID
JOIN PRODUCTION_DB.PUBLIC.CREDIT_SUISSE_FEED cs ON cs.LOAN_NUMBER = bi.LOAN_ID AND cs.REPORT_DATE = DATEADD(Day ,-2, current_date)
JOIN PRODUCTION_DB.PUBLIC.PARTNER_CLASSIFICATION pc USING(UPCODE)
WHERE transaction_code IN (204, 205, 220, 221,  -- Principal payment and reverse
                           206, 207, 222, 223,  -- Interest payment and reverse
                           441, 440,            -- Principal charge off
                           445, 444,            -- Interest chage off
                           502, 503, 524, 525)  -- Origination fees        
                           AND to_date(cs.ORIGINATION_DATE) < DATE_TRUNC('MONTH', CURRENT_DATE)
                           AND to_date(left(TRANSACTION_DATE, 10)) < DATE_TRUNC('MONTH', CURRENT_DATE)
                           AND loan_status NOT IN ('Not Originated', 'Cancelled')
                           AND (subvention_id is null)
GROUP BY VINTAGE, PRODUCT, TRANSACTION_MONTH, ORIGINAL_TERM_TO_MATURITY, CREDIT_SEGMENT, VERTICAL
ORDER BY VINTAGE, PRODUCT, TRANSACTION_MONTH, ORIGINAL_TERM_TO_MATURITY, CREDIT_SEGMENT, VERTICAL
