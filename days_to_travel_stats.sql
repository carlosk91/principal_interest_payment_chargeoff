SELECT 
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
sum(cs.loan_amount) as loan_amount,
count(
    case 
        when trip_start_date::date < cs.origination_date::date then loan_id 
    end)/count(*) as months_to_travel_share_before_origination_date,
count(
    case 
        when datediff(month, 
                        cs.origination_date::date, 
                        trip_start_date::date) = 1 then loan_id 
    end)/count(*) as months_to_travel_share_one_month_origination_date,
count(
    case 
        when datediff(month, 
                        cs.origination_date::date, 
                        trip_start_date::date) = 2 then loan_id 
    end)/count(*) as months_to_travel_share_two_month_origination_date,
count(
    case 
        when datediff(month, 
                        cs.origination_date::date, 
                        trip_start_date::date) = 3 then loan_id 
    end)/count(*) as months_to_travel_share_three_month_origination_date,
count(
    case 
        when datediff(month, 
                        cs.origination_date::date, 
                        trip_start_date::date) = 4 then loan_id 
    end)/count(*) as months_to_travel_share_four_month_origination_date,
count(
    case 
        when datediff(month, 
                        cs.origination_date::date, 
                        trip_start_date::date) > 4 then loan_id 
    end)/count(*) as months_to_travel_share_five_or_more_month_origination_date
FROM PRODUCTION_DB.PUBLIC.BI_LOAN bi
JOIN PRODUCTION_DB.PUBLIC.BI_ORDERS bio ON bi.application_id = bio.application_id
JOIN PRODUCTION_DB.PUBLIC.CREDIT_SUISSE_FEED cs ON cs.LOAN_NUMBER = bi.LOAN_ID AND cs.REPORT_DATE = DATEADD(Day ,-2, current_date)
JOIN PRODUCTION_DB.PUBLIC.PARTNER_CLASSIFICATION pc USING(UPCODE)
WHERE loan_status NOT IN ('Not Disbursed', 'Cancelled')
GROUP BY VINTAGE, PRODUCT, ORIGINAL_TERM_TO_MATURITY, CREDIT_SEGMENT, VERTICAL
ORDER BY VINTAGE, PRODUCT, ORIGINAL_TERM_TO_MATURITY, CREDIT_SEGMENT, VERTICAL
