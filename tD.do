use tD.dta, clear

* build variables
egen Code_Time = group(Zip_Code MonthYear)
gen LogSales = log( prod_totprice + 1)
gen tD1INTER = MinsPerPage * ExperienceGood
gen LogPagesViewed = log( pages_viewed )
gen LogMinsSpent = log( duration + 1 )

* Table D1
eststo: reg LogSales MinsPerPage ExperienceGood if domain_name == "amazon.com", vce(cluster Code_Time) noconstant

esttab using tableD1.tex, se label replace booktabs title(Search Intensity Effects on Sales for Amazon\label{tab:tabD1})

eststo clear

* Table D2
eststo: reg LogPagesViewed prod_totprice ExperienceGood if domain_name == "amazon.com", vce(cluster Code_Time) noconstant

eststo: reg LogMinsSpent prod_totprice ExperienceGood if domain_name == "amazon.com", vce(cluster Code_Time) noconstant

esttab using tableD2.tex, se label replace booktabs title(Product Characteristics Effects on Search Intensity for Amazon\label{tab:tabD2})

eststo clear

* Table D3 
eststo: logit RefDomainIsAmazon ExperienceGood if domain_name == "amazon.com", vce(cluster MonthYear) noconstant

eststo: logit ReferringDomainIsSearchEngine ExperienceGood if domain_name == "amazon.com", vce(cluster MonthYear) noconstant

esttab using tableD3.tex, se label replace booktabs title(Product Characteristics Effects on Search Intensity for Amazon\label{tab:tabD3})

eststo clear
