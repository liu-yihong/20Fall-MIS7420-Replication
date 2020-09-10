# load library
library('dplyr')
library('haven')
library('sqldf')
library('zoo')
library('plm')
library('stargazer')
# see (https://cran.r-project.org/web/packages/stargazer/vignettes/stargazer.pdf)
# for how to make table for latex

# all data path
bb_zipcode_path <- 'data/bestbuyzipcodes_sample.sas7bdat'
sales_allother_zipcode_path <- 'data/sales_allotherzipcode_sample.sas7bdat'
sales_cc_0mile_path <- 'data/sales_ccity0milezipcode_sample.sas7bdat'
sales_cc_5miles_path <- 'data/sales_ccity5milezipcode_sample.sas7bdat'

# load data
bb_zipcode <- read_sas(bb_zipcode_path)
sales_allother_zipcode <- read_sas(sales_allother_zipcode_path)
sales_cc_0mile <- read_sas(sales_cc_0mile_path)
sales_cc_5miles <- read_sas(sales_cc_5miles_path)

# Data Mapping
sales_allother_zipcode$Store_Close_Status <- 0 # NaN means no CC in 5-miles radius, we change NaN to 0

# Exclude Data without purchase
# All data should be with purchase -> tran_flg == 1
sales_allother_zipcode <- sales_allother_zipcode[sales_allother_zipcode$tran_flg == 1,]
sales_cc_0mile <- sales_cc_0mile[sales_cc_0mile$tran_flg == 1,]
sales_cc_5miles <- sales_cc_5miles[sales_cc_5miles$tran_flg == 1,]

# Filter Referring Domain

# groupby ref_domain and count
groupby_ref_domain_result <- aggregate(machine_id ~ ref_domain_name, rbind(sales_allother_zipcode, sales_cc_0mile, sales_cc_5miles), FUN = "length")
groupby_ref_domain_result <- groupby_ref_domain_result[order(-groupby_ref_domain_result$machine_id), ]
# we identify some search engines
search_engine_to_consider1 <- c("GOOGLE.COM", "YAHOO.COM", "google.com", "yahoo.com",
                             "MSN.COM", "msn.com", "aol.com", "AOL.COM", "LIVE.COM", "live.com",
                             "MYWEBSEARCH.COM", "ASK.COM", "MYWAY.COM", "mywebsearch.com",
                             "ask.com", "YAHOO.NET", "BIZRATE.COM", "bizrate.com")

ref_domain_to_consider1 <- c("", "GOOGLE.COM", "YAHOO.COM", "google.com", "yahoo.com",
                             "MSN.COM", "msn.com", "aol.com", "AOL.COM", "LIVE.COM", "live.com",
                             "MYWEBSEARCH.COM", "ASK.COM", "MYWAY.COM", "mywebsearch.com",
                             "ask.com", "YAHOO.NET", "BIZRATE.COM", "bizrate.com")

ref_domain_to_consider2 <- c("", "GOOGLE.COM", "YAHOO.COM", "google.com", "yahoo.com", "bing.com")

# Then we filter data by refer domain name
sales_allother_zipcode <- sales_allother_zipcode[(sales_allother_zipcode$ref_domain_name %in% ref_domain_to_consider1),]
sales_cc_0mile <- sales_cc_0mile[(sales_cc_0mile$ref_domain_name %in% ref_domain_to_consider1),]
sales_cc_5miles <- sales_cc_5miles[(sales_cc_5miles$ref_domain_name %in% ref_domain_to_consider1),]

# Filter Target Domain Name
groupby_target_domain_result <- aggregate(machine_id ~ domain_name, rbind(sales_allother_zipcode, sales_cc_5miles), FUN = "length")
groupby_target_domain_result <- groupby_target_domain_result[order(-groupby_target_domain_result$machine_id), ]

# Here's the top 5 online retail vendors
# 1                   dell.com             1473  428412.50
# 2                 amazon.com             9973  316678.40
# 3                staples.com             5663  225993.15
# 4                walmart.com             1838  147445.34
# 5                bestbuy.com             1159  142993.72
five_target_domain_to_consider <- c("amazon.com", "staples.com", "dell.com", "walmart.com", "bestbuy.com")
two_target_domain_to_consider <- c("amazon.com","bestbuy.com")

# we can choose what filter to apply
sales_allother_zipcode <- sales_allother_zipcode[sales_allother_zipcode$domain_name %in% five_target_domain_to_consider,]
sales_cc_0mile <- sales_cc_0mile[sales_cc_0mile$domain_name %in% five_target_domain_to_consider,]
sales_cc_5miles <- sales_cc_5miles[sales_cc_5miles$domain_name %in% five_target_domain_to_consider,]

# Product Categories
# 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
# remove 28, 30, 39, 40
sort(unique(rbind(sales_allother_zipcode, sales_cc_0mile, sales_cc_5miles)$prod_category_id))
category_to_consider <- c(22, 23, 24, 25, 26, 27, 29, 31, 32, 33, 34, 35, 36, 37, 38)
experience_product <- c(24, 25, 26, 27, 31, 32, 33, 34, 36, 37)
search_product <- c(22, 23, 24, 29, 30, 35)

sales_allother_zipcode <- sales_allother_zipcode[sales_allother_zipcode$prod_category_id %in% category_to_consider,]
sales_cc_0mile <- sales_cc_0mile[sales_cc_0mile$prod_category_id %in% category_to_consider,]
sales_cc_5miles <- sales_cc_5miles[sales_cc_5miles$prod_category_id %in% category_to_consider,]

# Date Transform
sales_allother_zipcode$event_date <- as.Date(sales_allother_zipcode$event_date)
sales_cc_0mile$event_date <- as.Date(sales_cc_0mile$event_date)
sales_cc_5miles$event_date <- as.Date(sales_cc_5miles$event_date)

# construct MonthYear - month of year
sales_allother_zipcode$MonthYear <- format(sales_allother_zipcode$event_date, "%Y-%m")
sales_cc_0mile$MonthYear <- format(sales_cc_0mile$event_date, "%Y-%m")
sales_cc_5miles$MonthYear <- format(sales_cc_5miles$event_date, "%Y-%m")

# Mark CC Closure

# CCStorePresent
# it is the same as Store_Close_Status
sales_allother_zipcode$CCStorePresent <- sales_allother_zipcode$Store_Close_Status
sales_cc_0mile$CCStorePresent <- sales_cc_0mile$Store_Close_Status
sales_cc_5miles$CCStorePresent <- sales_cc_5miles$Store_Close_Status

# AfterStoreClosing
sales_allother_zipcode$AfterStoreClosing <- ifelse(sales_allother_zipcode$MonthYear < "2008-11", 0, 1)
sales_cc_0mile$AfterStoreClosing <- ifelse(sales_cc_0mile$MonthYear < "2008-11", 0, 1)
sales_cc_5miles$AfterStoreClosing <- ifelse(sales_cc_5miles$MonthYear < "2008-11", 0, 1)

# BBStorePresent
sales_allother_zipcode <- merge(sales_allother_zipcode, bb_zipcode, by.x ="Zip_Code", by.y = "Zip_Code", all.x = TRUE)
sales_cc_0mile <- merge(sales_cc_0mile, bb_zipcode, by.x ="Zip_Code", by.y = "Zip_Code", all.x = TRUE)
sales_cc_5miles <- merge(sales_cc_5miles, bb_zipcode, by.x ="Zip_Code", by.y = "Zip_Code", all.x = TRUE)

sales_allother_zipcode$BBStorePresent <- na.fill(sales_allother_zipcode$BB_Store_Status, 0)
sales_cc_0mile$BBStorePresent <- na.fill(sales_cc_0mile$BB_Store_Status, 0)
sales_cc_5miles$BBStorePresent <- na.fill(sales_cc_5miles$BB_Store_Status, 0)

# Mark Referring Domain
# QUestion: How to group data?
sales_allother_zipcode$NoReferringDomain <- ifelse(sales_allother_zipcode$ref_domain_name == '', 1, 0)
sales_cc_0mile$NoReferringDomain <- ifelse(sales_cc_0mile$ref_domain_name == '', 1, 0)
sales_cc_5miles$NoReferringDomain <- ifelse(sales_cc_5miles$ref_domain_name == '', 1, 0)

sales_allother_zipcode$ReferringDomainIsSearchEngine <- ifelse(sales_allother_zipcode$ref_domain_name %in% search_engine_to_consider1, 1, 0)
sales_cc_0mile$ReferringDomainIsSearchEngine <- ifelse(sales_cc_0mile$ref_domain_name %in% search_engine_to_consider1, 1, 0)
sales_cc_5miles$ReferringDomainIsSearchEngine <- ifelse(sales_cc_5miles$ref_domain_name %in% search_engine_to_consider1, 1, 0)

# Aggregate Data

# try aggregate

concat_data1 <- rbind(sales_allother_zipcode, sales_cc_0mile)
concat_data2 <- rbind(sales_allother_zipcode, sales_cc_5miles)

data_0m_t4 <- sqldf("SELECT Zip_Code, MonthYear, domain_name, SUM(prod_totprice) AS TotalMonthlySales, AVG(CCStorePresent) AS CCStorePresent, AVG(BBStorePresent) AS BBStorePresent, AVG(AfterStoreClosing) AS AfterStoreClosing FROM concat_data1 GROUP BY Zip_Code, MonthYear, domain_name")
data_5m_t4 <- sqldf("SELECT Zip_Code, MonthYear, domain_name, SUM(prod_totprice) AS TotalMonthlySales, AVG(CCStorePresent) AS CCStorePresent, AVG(BBStorePresent) AS BBStorePresent, AVG(AfterStoreClosing) AS AfterStoreClosing FROM concat_data2 GROUP BY Zip_Code, MonthYear, domain_name")

data_0m_t5 <- sqldf("SELECT Zip_Code, MonthYear, domain_name, SUM(pages_viewed) / SUM(prod_totprice) AS PagesPerDollar, SUM(duration) / SUM(prod_totprice) AS MinsPerDollar, AVG(CCStorePresent) AS CCStorePresent, AVG(BBStorePresent) AS BBStorePresent, AVG(AfterStoreClosing) AS AfterStoreClosing FROM concat_data1 GROUP BY Zip_Code, MonthYear, domain_name")
data_5m_t5 <- sqldf("SELECT Zip_Code, MonthYear, domain_name, SUM(pages_viewed) / SUM(prod_totprice) AS PagesPerDollar, SUM(duration) / SUM(prod_totprice) AS MinsPerDollar, AVG(CCStorePresent) AS CCStorePresent, AVG(BBStorePresent) AS BBStorePresent, AVG(AfterStoreClosing) AS AfterStoreClosing FROM concat_data2 GROUP BY Zip_Code, MonthYear, domain_name")


data_0m_t9_NoReferringDomain <- sqldf("SELECT Zip_Code, MonthYear, domain_name, AVG(NoReferringDomain) AS NoReferringDomain, AVG(CCStorePresent) AS CCStorePresent, AVG(BBStorePresent) AS BBStorePresent, AVG(AfterStoreClosing) AS AfterStoreClosing FROM concat_data1 GROUP BY Zip_Code, MonthYear, domain_name, NoReferringDomain")
data_5m_t9_NoReferringDomain <- sqldf("SELECT Zip_Code, MonthYear, domain_name, AVG(NoReferringDomain) AS NoReferringDomain, AVG(CCStorePresent) AS CCStorePresent, AVG(BBStorePresent) AS BBStorePresent, AVG(AfterStoreClosing) AS AfterStoreClosing FROM concat_data2 GROUP BY Zip_Code, MonthYear, domain_name, NoReferringDomain")
data_0m_t9_ReferringDomainIsSearchEngine <- sqldf("SELECT Zip_Code, MonthYear, domain_name, AVG(ReferringDomainIsSearchEngine) AS ReferringDomainIsSearchEngine, AVG(CCStorePresent) AS CCStorePresent, AVG(BBStorePresent) AS BBStorePresent, AVG(AfterStoreClosing) AS AfterStoreClosing FROM concat_data1 GROUP BY Zip_Code, MonthYear, domain_name, NoReferringDomain")
data_5m_t9_ReferringDomainIsSearchEngine <- sqldf("SELECT Zip_Code, MonthYear, domain_name, AVG(ReferringDomainIsSearchEngine) AS ReferringDomainIsSearchEngine, AVG(CCStorePresent) AS CCStorePresent, AVG(BBStorePresent) AS BBStorePresent, AVG(AfterStoreClosing) AS AfterStoreClosing FROM concat_data2 GROUP BY Zip_Code, MonthYear, domain_name, NoReferringDomain")

# sqldf("SELECT Zip_Code, MonthYear, domain_name, count(*) as TotalTransaction, SUM(prod_totprice) AS TotalMonthlySales, SUM(pages_viewed) AS TotalPagesViewed, SUM(duration) AS TotalDuration, AVG(CCStorePresent) AS CCStorePresent, AVG(BBStorePresent) AS BBStorePresent, AVG(AfterStoreClosing) AS AfterStoreClosing FROM concat_data1 GROUP BY Zip_Code, MonthYear, domain_name")

# Table 1
table1_raw <-  rbind(read_sas(sales_allother_zipcode_path), read_sas(sales_cc_0mile_path))
table1 <- sqldf("SELECT domain_name as DomainName, count(*) as TotalTransaction, SUM(prod_totprice) AS TotalSales, SUM(pages_viewed) AS TotalPagesViewed, SUM(pages_viewed)/SUM(prod_totprice) AS PagesPerDollar, SUM(duration) AS TotalDuration, SUM(duration)/SUM(prod_totprice) AS MinsPerDollar FROM table1_raw GROUP BY domain_name ORDER BY TotalSales DESC")
stargazer(table1[1:5,], align=TRUE, summary = FALSE, rownames = FALSE, title="Summary Statistics of Top Five Vendors by Sales Volume")

# Table 2

table2_raw <-  rbind(read_sas(sales_allother_zipcode_path), read_sas(sales_cc_0mile_path))
table2_raw$direct_to_website <- ifelse(table2_raw$ref_domain_name == '', 1, 0)
table2_raw$referred_by_search <- ifelse(table2_raw$ref_domain_name %in% search_engine_to_consider1, 1, 0)
table2_raw$referred_by_other <- ifelse(!(table2_raw$ref_domain_name %in% ref_domain_to_consider1), 1, 0)
table2_raw$domain_name[!(table2_raw$domain_name %in% c('amazon.com', 'bestbuy.com'))] <- "All Others"

table2 <- sqldf("SELECT domain_name as DomainName, count(*) as TotalTransaction, SUM(referred_by_search) AS ReferredbySearchEngine, SUM(direct_to_website) AS DirecttoWebsite, SUM(referred_by_other) AS ReferredbyOthers FROM table2_raw GROUP BY domain_name")
stargazer(table2, align=TRUE, summary = FALSE, rownames = FALSE, title="Summary Statistics of Referring Domain Categories")
rm("table2_raw")

# Table 3
# see (https://www.stats4stem.org/r-difference-of-means)
# see (https://stats.stackexchange.com/questions/302445/standard-error-of-difference)
# see (https://stats.stackexchange.com/questions/43586/how-to-test-whether-the-difference-in-difference-between-means-is-significantly)
# see (https://stats.stackexchange.com/questions/160359/difference-in-difference-method-how-to-test-for-assumption-of-common-trend-betw)
# for tutorial
table3_raw <- rbind(sales_allother_zipcode, sales_cc_0mile)

amazonsales_control_before <- table3_raw[(table3_raw$CCStorePresent == 0) & (table3_raw$domain_name == "amazon.com") & (table3_raw$AfterStoreClosing == 0),]$prod_totprice
amazonsales_control_after <- table3_raw[(table3_raw$CCStorePresent == 0) & (table3_raw$domain_name == "amazon.com") & (table3_raw$AfterStoreClosing == 1),]$prod_totprice

amazonsales_control_before_mean <- mean(amazonsales_control_before)
amazonsales_control_after_mean <- mean(amazonsales_control_after)

amazonsales_control_before_length <- length(amazonsales_control_before)
amazonsales_control_after_length <- length(amazonsales_control_after)

amazonsales_control_before_var <- var(amazonsales_control_before)
amazonsales_control_after_var <- var(amazonsales_control_after)
#
amazonsales_control_mean_diff <- amazonsales_control_after_mean - amazonsales_control_before_mean
amazonsales_control_mean_diff_se <- sqrt( (amazonsales_control_before_var / amazonsales_control_before_length) + (amazonsales_control_after_var/amazonsales_control_after_length) )
amazonsales_control_mean_diff_test <- amazonsales_control_mean_diff / amazonsales_control_mean_diff_se
# formal test
# t.test(amazonsales_control_before, amazonsales_control_after, alternative = "less")

summary(table3_raw[(table3_raw$CCStorePresent == 1) & (table3_raw$domain_name == "amazon.com") & (table3_raw$AfterStoreClosing == 0),]$prod_totprice)
summary(table3_raw[(table3_raw$CCStorePresent == 1) & (table3_raw$domain_name == "amazon.com") & (table3_raw$AfterStoreClosing == 1),]$prod_totprice)

# Table 4

ama.t4.0mile <- plm(log(TotalMonthlySales) ~ CCStorePresent:AfterStoreClosing + CCStorePresent:AfterStoreClosing:BBStorePresent, data = data_0m_t4[data_0m_t4$domain_name == "amazon.com",], index = c("Zip_Code", "MonthYear"), model = "within", effect = "twoways")
summary(ama.t4.0mile)
summary(fixef(ama.t4.0mile, effect = "time"))
summary(fixef(ama.t4.0mile, effect = "individual"))

ama.t4.5mile <- plm(log(TotalMonthlySales) ~ CCStorePresent:AfterStoreClosing + CCStorePresent:AfterStoreClosing:BBStorePresent, data = data_5m_t4[data_5m_t4$domain_name == "amazon.com",], index = c("Zip_Code", "MonthYear"), model = "within", effect = "twoways")
summary(ama.t4.5mile)

bb.t4.0mile <- plm(log(TotalMonthlySales) ~ CCStorePresent:AfterStoreClosing + CCStorePresent:AfterStoreClosing:BBStorePresent, data = data_0m_t4[data_0m_t4$domain_name == "bestbuy.com",], index = c("Zip_Code", "MonthYear"), model = "within", effect = "twoways")
summary(bb.t4.0mile)

bb.t4.5mile <- plm(log(TotalMonthlySales) ~ CCStorePresent:AfterStoreClosing + CCStorePresent:AfterStoreClosing:BBStorePresent, data = data_5m_t4[data_5m_t4$domain_name == "bestbuy.com",], index = c("Zip_Code", "MonthYear"), model = "within", effect = "twoways")
summary(bb.t4.5mile)

stargazer(ama.t4.0mile, ama.t4.5mile, bb.t4.0mile, bb.t4.5mile, title="Results", align=TRUE, covariate.labels=c("$beta_1$", "$beta_2$"), no.space=TRUE)

# check multi-collinearity
library(corrplot)
X <- temp2[temp2$domain_name == "bestbuy.com",c('TotalMonthlySales', 'CCStorePresent','AfterStoreClosing','BBStorePresent')]
cor1 = cor(X)
corrplot.mixed(cor1, lower.col = "black", number.cex = .7)

# Table 5
# For PagesPerDollar
ama.t5.pagesperdollar.0mile <- plm(log(PagesPerDollar) ~ CCStorePresent:AfterStoreClosing + CCStorePresent:AfterStoreClosing:BBStorePresent, data = data_0m_t5[data_0m_t5$domain_name == "amazon.com",], index = c("Zip_Code", "MonthYear"), model = "within", effect = "twoways")
summary(ama.t5.pagesperdollar.0mile)

ama.t5.pagesperdollar.5mile <- plm(log(PagesPerDollar) ~ CCStorePresent:AfterStoreClosing + CCStorePresent:AfterStoreClosing:BBStorePresent, data = data_5m_t5[data_5m_t5$domain_name == "amazon.com",], index = c("Zip_Code", "MonthYear"), model = "within", effect = "twoways")
summary(ama.t5.pagesperdollar.5mile)

bb.t5.pagesperdollar.0mile <- plm(log(PagesPerDollar) ~ CCStorePresent:AfterStoreClosing + CCStorePresent:AfterStoreClosing:BBStorePresent, data = data_0m_t5[data_0m_t5$domain_name == "bestbuy.com",], index = c("Zip_Code", "MonthYear"), model = "within", effect = "twoways")
summary(bb.t5.pagesperdollar.0mile)

bb.t5.pagesperdollar.5mile <- plm(log(PagesPerDollar) ~ CCStorePresent:AfterStoreClosing + CCStorePresent:AfterStoreClosing:BBStorePresent, data = data_5m_t5[data_5m_t5$domain_name == "bestbuy.com",], index = c("Zip_Code", "MonthYear"), model = "within", effect = "twoways")
summary(bb.t5.pagesperdollar.5mile)

# For MinsPerDollar
ama.t5.minsperdollar.0mile <- plm(log(MinsPerDollar) ~ CCStorePresent:AfterStoreClosing + CCStorePresent:AfterStoreClosing:BBStorePresent, data = data_0m_t5[data_0m_t5$domain_name == "amazon.com",], index = c("Zip_Code", "MonthYear"), model = "within", effect = "twoways")
summary(ama.t5.minsperdollar.0mile)

ama.t5.minsperdollar.5mile <- plm(log(MinsPerDollar) ~ CCStorePresent:AfterStoreClosing + CCStorePresent:AfterStoreClosing:BBStorePresent, data = data_5m_t5[data_5m_t5$domain_name == "amazon.com",], index = c("Zip_Code", "MonthYear"), model = "within", effect = "twoways")
summary(ama.t5.minsperdollar.5mile)

bb.t5.minsperdollar.0mile <- plm(log(MinsPerDollar) ~ CCStorePresent:AfterStoreClosing + CCStorePresent:AfterStoreClosing:BBStorePresent, data = data_0m_t5[data_0m_t5$domain_name == "bestbuy.com",], index = c("Zip_Code", "MonthYear"), model = "within", effect = "twoways")
summary(bb.t5.minsperdollar.0mile)

bb.t5.minsperdollar.5mile <- plm(log(MinsPerDollar) ~ CCStorePresent:AfterStoreClosing + CCStorePresent:AfterStoreClosing:BBStorePresent, data = data_5m_t5[data_5m_t5$domain_name == "bestbuy.com",], index = c("Zip_Code", "MonthYear"), model = "within", effect = "twoways")
summary(bb.t5.minsperdollar.5mile)

# Table 6
# Question: find corresponding product categories for experience and search product
# AmazonTotalMonthlySales & BBTotalMonthlySale vs Experience and Search Product

# Table 7 & 8
# AMazonPagesPerDollar, AmazonMinsPerDollar, BBPagesPerDollar, BBMinsPerDollar vs Experience and Search Product

# Table 9
library('pglm')

ama.t9.ReferringDomainIsSearchEngine.0mile <- plm(ReferringDomainIsSearchEngine ~ CCStorePresent:AfterStoreClosing + CCStorePresent:AfterStoreClosing:BBStorePresent, data = data_0m_t9_ReferringDomainIsSearchEngine[data_0m_t9_ReferringDomainIsSearchEngine$domain_name == "amazon.com",], index = c("Zip_Code", "MonthYear"), model = "within", effect = "twoways", family = binomial('logit'))
summary(ama.t5.minsperdollar.0mile)

# Table 10
# SalesPerTransaction; PagesPerTransaction; MinsPerTransaction; for Ama & BB


# Table 11
# CEM on zip code level
library('cem')
# see vignette("cem") how to use CEM
# match locations in the treatment group with locations in the control
# group which have similar demographics such as 
# the average household age, average income and average household size.
# TotalSales, PagesPerDollar, and MinsPerDollar for Ama & BB

# Table 12
# add location specific demographics
# in the regression equations as interaction terms with
# our DID term (corresponding to \beta_1). Specifically, we used the
# zip code specific average household age, average income and
# average household size for each location where online sales
# originated.

# Table 13
# averaged the outcome variable
# for each zip code before and after the Circuit City store
# closure date for the 110 matched zip codes.

# performed the same DID analysis
# as before but without time fixed effects.

# Table 14
# see (https://stat.ethz.ch/R-manual/R-patched/library/stats/html/cor.html)
# see (https://www.rdocumentation.org/packages/pbdDMAT/versions/0.2-3/topics/Variance%2FCovariance)
# for matrix

# see (https://web.stanford.edu/~mrosenfe/soc_meth_proj3/matrix_OLS_NYU_notes.pdf)
# see (https://stats.stackexchange.com/questions/62470/variance-covariance-matrix-of-the-errors-in-linear-regression)
# for variance-covariance matrix of error term

# Table C1

# Table D1-D4

# Table E1-E2

# Figure F1
# see (https://dhicks.github.io/2018-10-10-did/)

# Table G1-G3

# Advanced Method
# IV for endogeneity control
# see (https://cran.r-project.org/web/packages/ivtools/ivtools.pdf)
# for library
# see (https://rpubs.com/wsundstrom/t_ivreg)
# see (https://www.econometrics-with-r.org/12-ivr.html)
# for tutorial

# Synthetic Control for endogeneity control
# see (https://cran.r-project.org/web/packages/Synth/Synth.pdf)
# see (https://cran.r-project.org/web/packages/microsynth/index.html)
# for library
# see (https://towardsdatascience.com/causal-inference-using-synthetic-control-the-ultimate-guide-a622ad5cf827) 
# for tutorial

# Generalized Synthetic Control Method  for endogeneity control
# see (https://yiqingxu.org/software/gsynth/gsynth_examples.html)
# for library and tutorial


# Heckit Approach for endogeneity control
# see (https://cran.r-project.org/web/packages/sampleSelection/sampleSelection.pdf)
# see (https://www.rdocumentation.org/packages/micEcon/versions/0.1-3/topics/heckit)
# for library

# PSM & Look Ahead PSM for endogeneity control
# see (https://cran.r-project.org/web/packages/Matching/index.html)
# for PSM library
# see (LA-PSM) paper for LA-PSM method

# Measurement Error Bias Correction

# see (https://cran.r-project.org/web/packages/errors/index.html)
# for library
# see (https://cran.r-project.org/web/packages/errors/vignettes/rjournal.pdf)
# for paper and tuutorial

# see (https://cran.r-project.org/web/packages/simex/index.html)
# for SIMEX- And MCSIMEX-Algorithm for Measurement Error Models
