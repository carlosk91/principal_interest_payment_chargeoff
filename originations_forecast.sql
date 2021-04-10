select transaction_month as vintage, vertical, 
term as original_term_to_maturity, credit_segment, 
sum(volume * interest_rate) / (12*sum(volume)) as wa_interest_rate
from "FINANCE_DB"."F_DATASETS"."BOTTOMS_UP"
where version = '0.12_reforcast_2021-04-09-1627' and product != 'Subv'
group by vintage, vertical, term, credit_segment
