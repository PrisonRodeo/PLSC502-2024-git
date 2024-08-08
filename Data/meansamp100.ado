program define meansamp100, rclass
version 10
use WarrenBurger, clearsample 100, count
tempvar z 
gen `z' = fedpetsummarize `z'return scalar mean=r(mean)end
