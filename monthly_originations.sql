SELECT 
date_trunc('Month',to_date(cs.ORIGINATION_DATE)) AS VINTAGE,
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
        SUM(cs.LOAN_AMOUNT) AS LOAN_AMOUNT,
        SUM(cs.ORIGINAL_INTEREST_RATE * cs.LOAN_AMOUNT) / 
                (1200 * SUM(cs.LOAN_AMOUNT)) AS WA_INTEREST_RATE
FROM PRODUCTION_DB.PUBLIC.BI_LOAN bi 
JOIN PRODUCTION_DB.PUBLIC.CREDIT_SUISSE_FEED cs 
        ON cs.LOAN_NUMBER = bi.LOAN_ID 
                AND cs.REPORT_DATE = DATEADD(Day ,-2, current_date)
JOIN PRODUCTION_DB.PUBLIC.PARTNER_CLASSIFICATION pc USING(UPCODE)
WHERE loan_status NOT IN ('Not Originated', 'Cancelled') and subvention_id is null
GROUP BY VINTAGE,
        cs.ORIGINAL_TERM_TO_MATURITY,
        Credit_Segment,
        VERTICAL
ORDER BY VINTAGE,
        cs.ORIGINAL_TERM_TO_MATURITY,
        Credit_Segment,
        VERTICAL
