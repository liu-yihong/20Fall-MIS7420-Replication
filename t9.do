use t9.dta, clear

* build variables
gen DID =  CCStorePresent *  AfterStoreClosing
gen THREEINTER = DID *  BBStorePresent
egen Code_Time = group(Zip_Code MonthYear)

* Amazon - ReferringDomainIsSearchEngine & NoReferringDomain
eststo: logit ReferringDomainIsSearchEngine DID THREEINTER if domain_name == "amazon.com", vce(cluster Code_Time) noconstant

logistic ReferringDomainIsSearchEngine DID THREEINTER if domain_name == "amazon.com", noconstant vce(cluster Code_Time)

eststo: logit NoReferringDomain DID THREEINTER if domain_name == "amazon.com", vce(cluster Code_Time) noconstant

logistic NoReferringDomain DID THREEINTER if domain_name == "amazon.com", noconstant vce(cluster Code_Time)

* BestBuy - ReferringDomainIsSearchEngine & NoReferringDomain
eststo: logit ReferringDomainIsSearchEngine DID THREEINTER if domain_name == "bestbuy.com", vce(cluster MonthYear) noconstant

logistic ReferringDomainIsSearchEngine DID THREEINTER if domain_name == "bestbuy.com", noconstant vce(cluster Code_Time)

eststo: logit NoReferringDomain DID THREEINTER if domain_name == "bestbuy.com", vce(cluster MonthYear) noconstant

logistic NoReferringDomain DID THREEINTER if domain_name == "bestbuy.com", noconstant vce(cluster Code_Time)

* check http://scorreia.com/software/reghdfe/

* output table
esttab se

esttab using example.tex, se label replace booktabs title(Results of Logistic Regression for Referring Domain\label{tab:tab9})