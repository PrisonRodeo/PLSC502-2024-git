program define CI20, rclass 
version 10 
use WarrenBurger, clear 
sample 20, count 
tempvar z 
gen `z' = constit
summarize `z' 
return scalar mean=r(mean) 
return scalar ub=r(mean) + 1.96*sqrt((r(mean) * (1-r(mean)))/20)
return scalar lb=r(mean) - 1.96*sqrt((r(mean) * (1-r(mean)))/20)
end 
