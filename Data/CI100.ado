program define CI100, rclass 
version 10 
use WarrenBurger, clear 
sample 100, count 
tempvar z 
gen `z' = constit
summarize `z' 
return scalar mean=r(mean) 
return scalar ub=r(mean) + 1.96*sqrt((r(mean) * (1-r(mean)))/100)
return scalar lb=r(mean) - 1.96*sqrt((r(mean) * (1-r(mean)))/100)
end 
