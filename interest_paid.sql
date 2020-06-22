select 
loan_number,
date_trunc('MONTH',  TO_DATE(left(n.effective_date, 10))) as payment_month,
SUM(CASE 
    WHEN transaction_code in ('207', '223') THEN -transaction_amount 
    ELSE transaction_amount 
    END) as interest_paid
from "PRODUCTION_DB"."PUBLIC"."NLS_TRANSACTIONS" n 
join "PRODUCTION_DB"."PUBLIC"."BI_LOAN" l on n.loan_number = l.loanid
where  transaction_code in ('206', '207', '222', '223')
group by 1, 2
