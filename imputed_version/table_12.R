# load library
library('dplyr')
library('haven')
library('sqldf')
library('zoo')
library('plm')
library('stargazer')
library('mice')

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
                                "ask.com", "YAHOO.NET", "BIZRATE.COM", "bizrate.com",
                                "amazon.com", "staples.com", "dell.com", "walmart.com", "bestbuy.com",
                                "AMAZON.COM", "STAPLES.COM", "DELL.COM", "WALMART.COM", "BESTBUY.COM")

search_engine_to_consider2 <- c("GOOGLE.COM", "YAHOO.COM", "BING.COM", "google.com", "yahoo.com", "bing.com")

ref_domain_to_consider1 <- c("", "GOOGLE.COM", "YAHOO.COM", "google.com", "yahoo.com",
                             "MSN.COM", "msn.com", "aol.com", "AOL.COM", "LIVE.COM", "live.com",
                             "MYWEBSEARCH.COM", "ASK.COM", "MYWAY.COM", "mywebsearch.com",
                             "ask.com", "YAHOO.NET", "BIZRATE.COM", "bizrate.com",
                             "amazon.com", "staples.com", "dell.com", "walmart.com", "bestbuy.com",
                             "AMAZON.COM", "STAPLES.COM", "DELL.COM", "WALMART.COM", "BESTBUY.COM")

ref_domain_to_consider2 <- c("", "GOOGLE.COM", "YAHOO.COM", "BING.COM", "google.com", "yahoo.com", "bing.com")

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
# Jay removed 28, 30, 39, 40
# We choose to remove 38 39 40
sort(unique(rbind(sales_allother_zipcode, sales_cc_0mile, sales_cc_5miles)$prod_category_id))
category_to_consider <- c(22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37)
experience_product <- c(24, 25, 26, 27, 28, 31, 32, 33, 34, 36, 37)
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
# Question: How to group data?
sales_allother_zipcode$NoReferringDomain <- ifelse(sales_allother_zipcode$ref_domain_name == "", 1, 0)
sales_cc_0mile$NoReferringDomain <- ifelse(sales_cc_0mile$ref_domain_name == "", 1, 0)
sales_cc_5miles$NoReferringDomain <- ifelse(sales_cc_5miles$ref_domain_name == "", 1, 0)

sales_allother_zipcode$ReferringDomainIsSearchEngine <- ifelse(sales_allother_zipcode$ref_domain_name %in% search_engine_to_consider1, 1, 0)
sales_cc_0mile$ReferringDomainIsSearchEngine <- ifelse(sales_cc_0mile$ref_domain_name %in% search_engine_to_consider1, 1, 0)
sales_cc_5miles$ReferringDomainIsSearchEngine <- ifelse(sales_cc_5miles$ref_domain_name %in% search_engine_to_consider1, 1, 0)

# Aggregate Data
concat_data1 <- rbind(sales_allother_zipcode, sales_cc_0mile)
concat_data2 <- rbind(sales_allother_zipcode, sales_cc_5miles)

# Data for Table
## Table 12
data_0m_t12 <- sqldf("SELECT Zip_Code, MonthYear, domain_name, SUM(prod_totprice) AS TotalMonthlySales, SUM(pages_viewed) / SUM(prod_totprice) AS PagesPerDollar, SUM(duration) / SUM(prod_totprice) AS MinsPerDollar, AVG(household_size) AS HoHSize, AVG(hoh_oldest_age) AS HoHAge, AVG(household_income) AS HoHIncome, AVG(CCStorePresent) AS CCStorePresent, AVG(BBStorePresent) AS BBStorePresent, AVG(AfterStoreClosing) AS AfterStoreClosing FROM concat_data1 GROUP BY Zip_Code, MonthYear, domain_name")

# Data Balancing & Imputation Function
dta_bal_imp_t12 <- function(unbalanced_data){
  balanced_data <- NA
  # For every Domain Name
  for (domain_name in unique(unbalanced_data$domain_name)) {
    # Unbalanced Panel Data Imputation
    unbalanced_data_one <- unbalanced_data[unbalanced_data$domain_name == domain_name, ]
    unbalanced_data_one$Time <- as.integer(as.factor(unbalanced_data_one$MonthYear))
    
    temp <- make.pbalanced(unbalanced_data_one,
                           index = c("Zip_Code", "Time"))
    # Impute Dependent Variables by Group Average 
    temp <- temp %>%
      group_by(Zip_Code) %>%
      mutate(TotalMonthlySales = ifelse(is.na(TotalMonthlySales), mean(TotalMonthlySales, na.rm=TRUE), TotalMonthlySales))
    temp <- temp %>%
      group_by(Zip_Code) %>%
      mutate(PagesPerDollar = ifelse(is.na(PagesPerDollar), mean(PagesPerDollar, na.rm=TRUE), PagesPerDollar))
    temp <- temp %>%
      group_by(Zip_Code) %>%
      mutate(MinsPerDollar = ifelse(is.na(MinsPerDollar), mean(MinsPerDollar, na.rm=TRUE), MinsPerDollar))
    # Impute Demographic Variables by Group Average 
    # AVG(household_size) AS HoHSize, AVG(hoh_oldest_age) AS HoHAge, AVG(household_income) AS HoHIncome
    temp <- temp %>%
      group_by(Zip_Code) %>%
      mutate(HoHSize = ifelse(is.na(HoHSize), mean(HoHSize, na.rm=TRUE), HoHSize))
    temp <- temp %>%
      group_by(Zip_Code) %>%
      mutate(HoHAge = ifelse(is.na(HoHAge), mean(HoHAge, na.rm=TRUE), HoHAge))
    temp <- temp %>%
      group_by(Zip_Code) %>%
      mutate(HoHIncome = ifelse(is.na(HoHIncome), mean(HoHIncome, na.rm=TRUE), HoHIncome))
    # CCStorePresent
    temp <- temp %>%
      group_by(Zip_Code) %>%
      mutate(CCStorePresent = ifelse(is.na(CCStorePresent), mean(CCStorePresent, na.rm=TRUE), CCStorePresent))
    # AfterStoreClosing
    temp$AfterStoreClosing <- ifelse(temp$Time < 11, 0, 1)
    # BBStorePresent
    temp <- merge(temp, bb_zipcode, by.x ="Zip_Code", by.y = "Zip_Code", all.x = TRUE)
    temp$BBStorePresent <- na.fill(temp$BB_Store_Status, 0)
    # Fill Domain Name
    temp$domain_name <- domain_name
    # Add DID & THREE INTERACTION
    temp$DID <- temp$CCStorePresent * temp$AfterStoreClosing * temp$HoHSize * temp$HoHAge * temp$HoHIncome
    temp$THREEINTER <- temp$DID * temp$BBStorePresent
    
    # Merge Data
    balanced_data <- rbind(balanced_data, temp)
  }
  return(balanced_data[-1, ])
}
data_0m_t12 <- dta_bal_imp_t12(data_0m_t12)

# Table 12
ama.t12.0m.PagesPerDollar <- plm(log(PagesPerDollar + 1) ~ DID + THREEINTER, data = data_0m_t12[data_0m_t12$domain_name == "amazon.com",], index = c("Zip_Code", "Time"), model = "within", effect = "twoways")
ama.t12.0m.MinsPerDollar <- plm(log(MinsPerDollar + 1) ~ DID + THREEINTER, data = data_0m_t12[data_0m_t12$domain_name == "amazon.com",], index = c("Zip_Code", "Time"), model = "within", effect = "twoways")
ama.t12.0m.TotalMonthlySales <- plm(log(TotalMonthlySales + 1) ~ DID + THREEINTER, data = data_0m_t12[data_0m_t12$domain_name == "amazon.com",], index = c("Zip_Code", "Time"), model = "within", effect = "twoways")
bb.t12.0m.PagesPerDollar <- plm(log(PagesPerDollar + 1) ~ DID + THREEINTER, data = data_0m_t12[data_0m_t12$domain_name == "bestbuy.com",], index = c("Zip_Code", "Time"), model = "within", effect = "twoways")
bb.t12.0m.MinsPerDollar <- plm(log(MinsPerDollar + 1) ~ DID + THREEINTER, data = data_0m_t12[data_0m_t12$domain_name == "bestbuy.com",], index = c("Zip_Code", "Time"), model = "within", effect = "twoways")
bb.t12.0m.TotalMonthlySales <- plm(log(TotalMonthlySales + 1) ~ DID + THREEINTER, data = data_0m_t12[data_0m_t12$domain_name == "bestbuy.com",], index = c("Zip_Code", "Time"), model = "within", effect = "twoways")

stargazer(ama.t12.0m.TotalMonthlySales, bb.t12.0m.TotalMonthlySales,
          ama.t12.0m.PagesPerDollar, bb.t12.0m.PagesPerDollar,
          ama.t12.0m.MinsPerDollar, bb.t12.0m.MinsPerDollar,
          title="Results of the Online Sales and Search Effect with Zip Code Demographics as Interactions and Time Fixed Effects (All Product Categories)",
          align=TRUE, covariate.labels=c("beta_1", "beta_2"), no.space=TRUE,
          column.sep.width = "1pt",
          float.env = "sidewaystable", label = "tab:table12",
          column.labels=c("Amazon-0 Mile","BestBuy-0 Mile", "Amazon-0 Mile","BestBuy-0 Mile", "Amazon-0 Mile","BestBuy-0 Mile"))

