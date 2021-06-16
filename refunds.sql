SELECT 
DATE_TRUNC('month', lt.TRANSACTION_DATE)::date AS REFUND_MONTH,
date_trunc('Month',to_date(cs.ORIGINATION_DATE)) AS VINTAGE,
(case when subvention_id IS NOT NULL then 'Sub' else 'IB' end) as PRODUCT,
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
SUM(TRANSACTION_AMOUNT) AS REFUND_AMOUNT
FROM PRODUCTION_DB.PUBLIC.NLS_TRANSACTIONS lt
JOIN PRODUCTION_DB.PUBLIC.BI_LOAN bi ON lt.LOAN_NUMBER = bi.LOAN_ID
JOIN PRODUCTION_DB.PUBLIC.CREDIT_SUISSE_FEED cs ON cs.LOAN_NUMBER = bi.LOAN_ID AND cs.REPORT_DATE = DATEADD(Day ,-2, current_date)
JOIN PRODUCTION_DB.PUBLIC.PARTNER_CLASSIFICATION pc USING(UPCODE)
WHERE (PAYMENT_METHOD_NO IN (7, 8, 11, 15) AND TRANSACTION_CODE IN (204, 220)) OR TRANSACTION_CODE IN (522, 503, 524)
GROUP BY REFUND_MONTH, VINTAGE, PRODUCT, ORIGINAL_TERM_TO_MATURITY, CREDIT_SEGMENT, VERTICAL
ORDER BY REFUND_MONTH, VINTAGE, PRODUCT, ORIGINAL_TERM_TO_MATURITY, CREDIT_SEGMENT, VERTICAL
