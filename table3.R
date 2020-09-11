# all data path
bb_zipcode_path <- 'data/bestbuyzipcodes_sample.sas7bdat'
sales_allother_zipcode_path <- 'data/sales_allotherzipcode_sample.sas7bdat'
sales_cc_0mile_path <- 'data/sales_ccity0milezipcode_sample.sas7bdat'
sales_cc_5miles_path <- 'data/sales_ccity5milezipcode_sample.sas7bdat'

# load data
bb_zipcode <- read_sas(bb_zipcode_path)

# Table 3
# see (https://www.stats4stem.org/r-difference-of-means)
# see (https://stats.stackexchange.com/questions/302445/standard-error-of-difference)
# see (https://stats.stackexchange.com/questions/43586/how-to-test-whether-the-difference-in-difference-between-means-is-significantly)
# see (https://stats.stackexchange.com/questions/160359/difference-in-difference-method-how-to-test-for-assumption-of-common-trend-betw)
# for tutorial
#table3_0m_raw <- rbind(sales_allother_zipcode, sales_cc_0mile)
#table3_5m_raw <- rbind(sales_allother_zipcode, sales_cc_5miles)
temp <- read_sas(sales_allother_zipcode_path)
temp$Store_Close_Status <- 0
table3_0m_raw <-  rbind(temp, read_sas(sales_cc_0mile_path))
table3_5m_raw <-  rbind(temp, read_sas(sales_cc_5miles_path))

# Date Transform
table3_0m_raw$event_date <- as.Date(table3_0m_raw$event_date)
table3_5m_raw$event_date <- as.Date(table3_5m_raw$event_date)

# construct MonthYear - month of year
table3_0m_raw$MonthYear <- format(table3_0m_raw$event_date, "%Y-%m")
table3_5m_raw$MonthYear <- format(table3_5m_raw$event_date, "%Y-%m")

# Mark CC Closure

# CCStorePresent
# it is the same as Store_Close_Status
table3_0m_raw$CCStorePresent <- table3_0m_raw$Store_Close_Status
table3_5m_raw$CCStorePresent <- table3_5m_raw$Store_Close_Status

# AfterStoreClosing
table3_0m_raw$AfterStoreClosing <- ifelse(table3_0m_raw$MonthYear < "2008-11", 0, 1)
table3_5m_raw$AfterStoreClosing <- ifelse(table3_5m_raw$MonthYear < "2008-11", 0, 1)

# BBStorePresent
table3_0m_raw <- merge(table3_0m_raw, bb_zipcode, by.x ="Zip_Code", by.y = "Zip_Code", all.x = TRUE)
table3_5m_raw <- merge(table3_5m_raw, bb_zipcode, by.x ="Zip_Code", by.y = "Zip_Code", all.x = TRUE)

table3_0m_raw$BBStorePresent <- na.fill(table3_0m_raw$BB_Store_Status, 0)
table3_5m_raw$BBStorePresent <- na.fill(table3_5m_raw$BB_Store_Status, 0)

# aggregate data

table3_0m_aggregate <- sqldf("SELECT Zip_Code, MonthYear, domain_name, count(*) AS TotalTransactions, SUM(pages_viewed) as TotalPages, SUM(prod_totprice) as TotalMonthlySales, SUM(duration) as TotalMins, SUM(pages_viewed) / SUM(prod_totprice) AS PagesPerDollar, SUM(duration) / SUM(prod_totprice) AS MinsPerDollar, AVG(CCStorePresent) AS CCStorePresent, AVG(BBStorePresent) AS BBStorePresent, AVG(AfterStoreClosing) AS AfterStoreClosing FROM table3_0m_raw GROUP BY Zip_Code, MonthYear, domain_name")
table3_5m_aggregate <- sqldf("SELECT Zip_Code, MonthYear, domain_name, count(*) AS TotalTransactions, SUM(pages_viewed) as TotalPages, SUM(prod_totprice) as TotalMonthlySales, SUM(duration) as TotalMins, SUM(pages_viewed) / SUM(prod_totprice) AS PagesPerDollar, SUM(duration) / SUM(prod_totprice) AS MinsPerDollar, AVG(CCStorePresent) AS CCStorePresent, AVG(BBStorePresent) AS BBStorePresent, AVG(AfterStoreClosing) AS AfterStoreClosing FROM table3_5m_raw GROUP BY Zip_Code, MonthYear, domain_name")

# Amazon Sales
# for control
amazonsales_control_before <- table3_raw[(table3_raw$CCStorePresent == 0) & (table3_raw$domain_name == "amazon.com") & (table3_raw$AfterStoreClosing == 0),]$prod_totprice
amazonsales_control_after <- table3_raw[(table3_raw$CCStorePresent == 0) & (table3_raw$domain_name == "amazon.com") & (table3_raw$AfterStoreClosing == 1),]$prod_totprice

amazonsales_control_before_mean <- mean(amazonsales_control_before)
amazonsales_control_after_mean <- mean(amazonsales_control_after)

amazonsales_control_before_length <- length(amazonsales_control_before)
amazonsales_control_after_length <- length(amazonsales_control_after)

amazonsales_control_before_var <- var(amazonsales_control_before)
amazonsales_control_after_var <- var(amazonsales_control_after)
# se
amazonsales_control_mean_diff <- amazonsales_control_after_mean - amazonsales_control_before_mean
amazonsales_control_mean_diff_se <- sqrt( (amazonsales_control_before_var / amazonsales_control_before_length) + (amazonsales_control_after_var/amazonsales_control_after_length) )
amazonsales_control_mean_diff_test <- amazonsales_control_mean_diff / amazonsales_control_mean_diff_se
# formal test
# t.test(amazonsales_control_before, amazonsales_control_after, alternative = "less")

# Amazon Sales
# for treatment
amazonsales_treatment_before <- table3_raw[(table3_raw$CCStorePresent == 1) & (table3_raw$domain_name == "amazon.com") & (table3_raw$AfterStoreClosing == 0),]$prod_totprice
amazonsales_treatment_after <- table3_raw[(table3_raw$CCStorePresent == 1) & (table3_raw$domain_name == "amazon.com") & (table3_raw$AfterStoreClosing == 1),]$prod_totprice

amazonsales_treatment_before_mean <- mean(amazonsales_treatment_before)
amazonsales_treatment_after_mean <- mean(amazonsales_treatment_after)

amazonsales_treatment_before_length <- length(amazonsales_treatment_before)
amazonsales_treatment_after_length <- length(amazonsales_treatment_after)

amazonsales_treatment_before_var <- var(amazonsales_treatment_before)
amazonsales_treatment_after_var <- var(amazonsales_treatment_after)
# se
amazonsales_treatment_mean_diff <- amazonsales_treatment_after_mean - amazonsales_treatment_before_mean
amazonsales_treatment_mean_diff_se <- sqrt( (amazonsales_treatment_before_var / amazonsales_treatment_before_length) + (amazonsales_treatment_after_var/amazonsales_treatment_after_length) )

# Amazon Sales DID
amazonsales_did <- amazonsales_treatment_mean_diff - amazonsales_control_mean_diff

# Amazon PagesPerDollar
# for control
amazonppd_control_before <- table3_raw[(table3_raw$CCStorePresent == 0) & (table3_raw$domain_name == "amazon.com") & (table3_raw$AfterStoreClosing == 0),]$pages_viewed / table3_raw[(table3_raw$CCStorePresent == 0) & (table3_raw$domain_name == "amazon.com") & (table3_raw$AfterStoreClosing == 0),]$prod_totprice
amazonppd_control_after <- table3_raw[(table3_raw$CCStorePresent == 0) & (table3_raw$domain_name == "amazon.com") & (table3_raw$AfterStoreClosing == 1),]$pages_viewed / table3_raw[(table3_raw$CCStorePresent == 0) & (table3_raw$domain_name == "amazon.com") & (table3_raw$AfterStoreClosing == 1),]$prod_totprice

amazonppd_control_before_mean <- mean(amazonppd_control_before)
amazonppd_control_after_mean <- mean(amazonppd_control_after)

amazonppd_control_before_length <- length(amazonppd_control_before)
amazonppd_control_after_length <- length(amazonppd_control_after)

amazonppd_control_before_var <- var(amazonppd_control_before)
amazonppd_control_after_var <- var(amazonppd_control_after)
# se
amazonppd_control_mean_diff <- amazonppd_control_after_mean - amazonppd_control_before_mean
amazonppd_control_mean_diff_se <- sqrt( (amazonppd_control_before_var / amazonppd_control_before_length) + (amazonppd_control_after_var/amazonppd_control_after_length) )
amazonppd_control_mean_diff_test <- amazonppd_control_mean_diff / amazonppd_control_mean_diff_se

# Amazon PagesPerDollar
# for treatment
amazonppd_treatment_before <- table3_raw[(table3_raw$CCStorePresent == 1) & (table3_raw$domain_name == "amazon.com") & (table3_raw$AfterStoreClosing == 0),]$pages_viewed / table3_raw[(table3_raw$CCStorePresent == 1) & (table3_raw$domain_name == "amazon.com") & (table3_raw$AfterStoreClosing == 0),]$prod_totprice
amazonppd_treatment_after <- table3_raw[(table3_raw$CCStorePresent == 1) & (table3_raw$domain_name == "amazon.com") & (table3_raw$AfterStoreClosing == 1),]$pages_viewed / table3_raw[(table3_raw$CCStorePresent == 1) & (table3_raw$domain_name == "amazon.com") & (table3_raw$AfterStoreClosing == 1),]$prod_totprice

amazonppd_treatment_before_mean <- mean(amazonppd_treatment_before)
amazonppd_treatment_after_mean <- mean(amazonppd_treatment_after)

amazonppd_treatment_before_length <- length(amazonppd_treatment_before)
amazonppd_treatment_after_length <- length(amazonppd_treatment_after)

amazonppd_treatment_before_var <- var(amazonppd_treatment_before)
amazonppd_treatment_after_var <- var(amazonppd_treatment_after)
# se
amazonppd_treatment_mean_diff <- amazonppd_treatment_after_mean - amazonppd_treatment_before_mean
amazonppd_treatment_mean_diff_se <- sqrt( (amazonppd_treatment_before_var / amazonppd_treatment_before_length) + (amazonppd_treatment_after_var/amazonppd_treatment_after_length) )
amazonppd_treatment_mean_diff_test <- amazonppd_treatment_mean_diff / amazonppd_treatment_mean_diff_se

# Amazon PagesPerDollar DID
amazonppd_did <- amazonppd_treatment_mean_diff - amazonppd_control_mean_diff

# Amazon MinsPerDollar
# for control
amazonmpd_control_before <- table3_raw[(table3_raw$CCStorePresent == 0) & (table3_raw$domain_name == "amazon.com") & (table3_raw$AfterStoreClosing == 0),]$duration / table3_raw[(table3_raw$CCStorePresent == 0) & (table3_raw$domain_name == "amazon.com") & (table3_raw$AfterStoreClosing == 0),]$prod_totprice
amazonmpd_control_after <- table3_raw[(table3_raw$CCStorePresent == 0) & (table3_raw$domain_name == "amazon.com") & (table3_raw$AfterStoreClosing == 1),]$duration / table3_raw[(table3_raw$CCStorePresent == 0) & (table3_raw$domain_name == "amazon.com") & (table3_raw$AfterStoreClosing == 1),]$prod_totprice

amazonmpd_control_before_mean <- mean(amazonmpd_control_before)
amazonmpd_control_after_mean <- mean(amazonmpd_control_after)

amazonmpd_control_before_length <- length(amazonmpd_control_before)
amazonmpd_control_after_length <- length(amazonmpd_control_after)

amazonmpd_control_before_var <- var(amazonmpd_control_before)
amazonmpd_control_after_var <- var(amazonmpd_control_after)
# se
amazonmpd_control_mean_diff <- amazonmpd_control_after_mean - amazonmpd_control_before_mean
amazonmpd_control_mean_diff_se <- sqrt( (amazonmpd_control_before_var / amazonmpd_control_before_length) + (amazonmpd_control_after_var/amazonmpd_control_after_length) )
amazonmpd_control_mean_diff_test <- amazonmpd_control_mean_diff / amazonmpd_control_mean_diff_se

# Amazon MinsPerDollar
# for treatment
amazonmpd_treatment_before <- table3_raw[(table3_raw$CCStorePresent == 1) & (table3_raw$domain_name == "amazon.com") & (table3_raw$AfterStoreClosing == 0),]$duration / table3_raw[(table3_raw$CCStorePresent == 1) & (table3_raw$domain_name == "amazon.com") & (table3_raw$AfterStoreClosing == 0),]$prod_totprice
amazonmpd_treatment_after <- table3_raw[(table3_raw$CCStorePresent == 1) & (table3_raw$domain_name == "amazon.com") & (table3_raw$AfterStoreClosing == 1),]$duration / table3_raw[(table3_raw$CCStorePresent == 1) & (table3_raw$domain_name == "amazon.com") & (table3_raw$AfterStoreClosing == 1),]$prod_totprice

amazonmpd_treatment_before_mean <- mean(amazonmpd_treatment_before)
amazonmpd_treatment_after_mean <- mean(amazonmpd_treatment_after)

amazonmpd_treatment_before_length <- length(amazonmpd_treatment_before)
amazonmpd_treatment_after_length <- length(amazonmpd_treatment_after)

amazonmpd_treatment_before_var <- var(amazonmpd_treatment_before)
amazonmpd_treatment_after_var <- var(amazonmpd_treatment_after)
# se
amazonmpd_treatment_mean_diff <- amazonmpd_treatment_after_mean - amazonmpd_treatment_before_mean
amazonmpd_treatment_mean_diff_se <- sqrt( (amazonmpd_treatment_before_var / amazonmpd_treatment_before_length) + (amazonmpd_treatment_after_var/amazonmpd_treatment_after_length) )
amazonmpd_treatment_mean_diff_test <- amazonmpd_treatment_mean_diff / amazonmpd_treatment_mean_diff_se

# Amazon MinsPerDollar DID
amazonmpd_did <- amazonmpd_treatment_mean_diff - amazonmpd_control_mean_diff
