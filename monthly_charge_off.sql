SELECT 
date_trunc('Month',to_date(cs.ORIGINATION_DATE)) AS VINTAGE,
(case when subvention_id IS NOT NULL then 'Sub' else 'IB' end) as PRODUCT,
date_trunc('Month',to_date(cs.charge_off_date)) AS CHARGE_OFF_MONTH,
cs.ORIGINAL_TERM_TO_MATURITY,
        CASE
            WHEN cs.FICO < 640
                THEN 'SubPrime'
            WHEN cs.FICO >= 640
                AND cs.FICO <= 710
                THEN 'NearPrime'
            WHEN cs.FICO > 710
                THEN 'Prime'
        END AS Credit_Segment,
        CASE
            WHEN pc.VERTICAL_L1 IN ('Air','Cruise','Package')
                THEN pc.VERTICAL_L1
            ELSE 'Other'
        END AS VERTICAL,
SUM(cs.chargeoff_principal) AS chargeoff_amount
FROM PRODUCTION_DB.PUBLIC.BI_LOAN bi 
JOIN PRODUCTION_DB.PUBLIC.CREDIT_SUISSE_FEED cs 
        ON cs.LOAN_NUMBER = bi.LOAN_ID 
                AND cs.REPORT_DATE = DATEADD(Day ,-2, current_date)
JOIN PRODUCTION_DB.PUBLIC.PARTNER_CLASSIFICATION pc USING(UPCODE)
WHERE loan_status NOT IN ('Not Originated', 'Cancelled') and subvention_id is null
and cs.chargeoff_principal != 0
GROUP BY 
        VINTAGE,
        PRODUCT,
        CHARGE_OFF_MONTH,
        cs.ORIGINAL_TERM_TO_MATURITY,
        Credit_Segment,
        VERTICAL
ORDER BY 
        VINTAGE,
        PRODUCT,
        CHARGE_OFF_MONTH,
        cs.ORIGINAL_TERM_TO_MATURITY,
        Credit_Segment,
        VERTICAL
