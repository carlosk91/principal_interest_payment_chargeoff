select 
transaction_month as vintage, 
vertical, 
product,
term as original_term_to_maturity, 
credit_segment, 
sum(volume * interest_rate) / 
sum(volume) as wa_interest_rate
from "FINANCE_DB"."F_DATASETS"."BOTTOMS_UP"
where version = '0.27_reforecast_2021-05-21' 
and transaction_month >= '2019-11-01' 
group by vintage, vertical, product, term, credit_segment
