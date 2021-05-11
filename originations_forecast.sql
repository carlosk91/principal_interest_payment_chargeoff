select transaction_month as vintage, vertical, 
term as original_term_to_maturity, credit_segment, 
sum(transaction_volume * interest_rate) / 
sum(transaction_volume) as wa_interest_rate
from "FINANCE_DB"."F_DATASETS"."BOTTOMS_UP_PLAN"
where version = '0.19_reforcast_2021-05-01-2256' 
and product_cat = 'Traditional'
and transaction_month >= '2019-11-01' 
group by vintage, vertical, term, credit_segment
