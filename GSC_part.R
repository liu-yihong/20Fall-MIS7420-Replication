# load libraries
library('dplyr')
library('haven')
library('sqldf')
library('zoo')
library('plm')
library('stargazer')
library('magrittr')
library('gsynth')
library('panelView')


# all data path
bb_zipcode_path <- 'data/bestbuyzipcodes_sample.sas7bdat'
sales_allother_zipcode_path <- 'data/sales_allotherzipcode_sample.sas7bdat'
sales_cc_0mile_path <- 'data/sales_ccity0milezipcode_sample.sas7bdat'
sales_cc_5miles_path <- 'data/sales_ccity5milezipcode_sample.sas7bdat'

# load data
bb_zipcode <- read_sas(bb_zipcode_path)
sales_allother_zipcode <- read_sas(sales_allother_zipcode_path, encoding = "latin1")
sales_cc_0mile <- read_sas(sales_cc_0mile_path)
sales_cc_5miles <- read_sas(sales_cc_5miles_path)

# map NaN(no CC in 5-mile radius) to 0
sales_allother_zipcode$Store_Close_Status <- 0

# Exclude Data without purchase
# All data should be with purchase -> tran_flg == 1
sales_allother_zipcode <- sales_allother_zipcode[sales_allother_zipcode$tran_flg == 1,]
sales_cc_0mile <- sales_cc_0mile[sales_cc_0mile$tran_flg == 1,]
sales_cc_5miles <- sales_cc_5miles[sales_cc_5miles$tran_flg == 1,]

# combine the control group with the treated group
concat_data1 <- rbind(sales_allother_zipcode, sales_cc_0mile)
concat_data2 <- rbind(sales_allother_zipcode, sales_cc_5miles)

head(sales_allother_zipcode)
head(sales_cc_0mile)
head(sales_cc_5miles)

# Filter Referring Domain
# we identify some search engines
ref_domain_to_consider1 <- c("", "GOOGLE.COM", "YAHOO.COM", "google.com", "yahoo.com",
                             "MSN.COM", "msn.com", "aol.com", "AOL.COM", "LIVE.COM", "live.com",
                             "MYWEBSEARCH.COM", "ASK.COM", "MYWAY.COM", "mywebsearch.com",
                             "ask.com", "YAHOO.NET", "BIZRATE.COM", "bizrate.com")

# Then we filter data by refer domain name
concat_data1 %<>% filter(ref_domain_name %in% ref_domain_to_consider1)
concat_data2 %<>% filter(ref_domain_name %in% ref_domain_to_consider1)


# Filter Target Domain Name
five_target_domain_to_consider <- c("amazon.com", "staples.com", "dell.com", "walmart.com", "bestbuy.com")
two_target_domain_to_consider <- c("amazon.com","bestbuy.com")

# we can choose what filter to apply
concat_data1 %<>% filter(domain_name %in% five_target_domain_to_consider)
concat_data2 %<>% filter(domain_name %in% five_target_domain_to_consider)


# Product Categories
# 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
# Jay removed 28, 30, 39, 40
# We choose to remove 38 39 40
category_to_consider <- c(22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37)
experience_product <- c(24, 25, 26, 27, 28, 31, 32, 33, 34, 36, 37)
search_product <- c(22, 23, 24, 29, 30, 35)

concat_data1 %<>% 
  filter(prod_category_id %in% category_to_consider) %>% 
  mutate(prod_category_type = ifelse(prod_category_id %in% experience_product, 1, 0) %>% 
           as.factor()) #experience proudct=1, search product=0
concat_data2 %<>% 
  filter(prod_category_id %in% category_to_consider) %>% 
  mutate(prod_category_type = ifelse(prod_category_id %in% experience_product, 1, 0) %>% 
           as.factor()) #experience proudct=1, search product=0


# construct MonthYear - month of year
concat_data1 %<>% mutate(MonthYear = format(event_date, "%Y-%m") )
concat_data2 %<>% mutate(MonthYear = format(event_date, "%Y-%m") )
                                   

# CCStorePresent
# it is the same as Store_Close_Status
concat_data1 %<>% mutate(CCStorePresent = Store_Close_Status)
concat_data2 %<>% mutate(CCStorePresent = Store_Close_Status)

# AfterStoreClosing
concat_data1 %<>% mutate(AfterStoreClosing = ifelse(MonthYear < "2008-11", 0, 1) %>% 
                           as.factor())
concat_data2 %<>% mutate(AfterStoreClosing = ifelse(MonthYear < "2008-11", 0, 1) %>% 
                           as.factor())  

# BBStorePresent
concat_data1 %<>% 
  merge(.,bb_zipcode, by.x ="Zip_Code", by.y = "Zip_Code", all.x = TRUE) %>% 
  mutate(BBStorePresent = ifelse(is.na(BB_Store_Status), 0, BB_Store_Status),
         PagesPerDollar = pages_viewed/prod_totprice,
         MinsPerDollar = duration/prod_totprice) 
concat_data2 %<>% 
  merge(.,bb_zipcode, by.x ="Zip_Code", by.y = "Zip_Code", all.x = TRUE) %>% 
  mutate(BBStorePresent = ifelse(is.na(BB_Store_Status), 0, BB_Store_Status),
         PagesPerDollar = pages_viewed/prod_totprice,
         MinsPerDollar = duration/prod_totprice)


# Treatment
concat_data1 %<>% 
  mutate(treatment = ifelse(CCStorePresent==1 & AfterStoreClosing == 1, 1, 0))
concat_data2 %<>% 
  mutate(treatment = ifelse(CCStorePresent==1 & AfterStoreClosing == 1, 1, 0))



# Select the useful variables
while(FALSE) {
gsc_0mile <- concat_data1 %>% 
  select(-Store_Close_Status, -domain_id, -ref_domain_name, -MinsPerDollar,
         -event_date,-event_time,-tran_flg,-prod_name, -MonthYear,
         -CCStorePresent,- AfterStoreClosing,-BB_Store_Status, -PagesPerDollar,
         -site_session_id, -prod_category_id, -basket_tot, -machine_id, -Zip_Code,
         -pages_viewed, -duration, -prod_qty, -prod_totprice, -household_size, 
         -hoh_oldest_age, -household_income, -children, -treatment)

gsc_5mile <- concat_data2  %>%
  select(-Store_Close_Status,-domain_id, -ref_domain_name, -MinsPerDollar,
         -event_date,-event_time,-tran_flg,-prod_name, -MonthYear,
         -CCStorePresent, -AfterStoreClosing,-BB_Store_Status, -PagesPerDollar,
         -site_session_id,-prod_category_id, -basket_tot, -machine_id, -Zip_Code,
         -pages_viewed, -duration, -prod_qty, -prod_totprice, -household_size, 
         -hoh_oldest_age, -household_income, -children, -treatment)
}


################################# Amazon Total month sales #####################################
# Select all the observable variables that can be grouped
# Group data by zip code and time
amzon_mothsales_0mile <- sqldf("SELECT SUM(prod_totprice) AS TotalMonthlySales, AVG(treatment) AS Treatment, AVG(BBStorePresent) AS BBStorePresent,
                               AVG(household_size) AS Household_Size, AVG(hoh_oldest_age) AS Hoh_Oldest_Age, AVG(household_income) AS Household_Income, 
                               AVG(children) AS Children, AVG(connection_speed) AS Connection_Speed, MonthYear, Zip_Code 
                               FROM concat_data1 WHERE domain_name='amazon.com' GROUP BY MonthYear, Zip_Code ORDER BY Zip_Code, MonthYear")
# number of control group = 2796
num_control = length(unique(amzon_mothsales_0mile$Zip_Code)) - length(unique(amzon_mothsales_0mile[which(amzon_mothsales_0mile$Treatment == 1), ]$Zip_Code))
# the last pre-treatment period = 10
t0 = length(unique(amzon_mothsales_0mile$MonthYear)) - length(unique(amzon_mothsales_0mile[which(amzon_mothsales_0mile$Treatment == 1), ]$MonthYear))

amzon_mothsales_0mile$logTotalMonthlySales = log(amzon_mothsales_0mile$TotalMonthlySales + 1)
amzon_mothsales_0mile$endo = amzon_mothsales_0mile$Treatment * amzon_mothsales_0mile$BBStorePresent

# Visualize the data structure and spot missing values
panelView(logTotalMonthlySales ~ Treatment + endo + Household_Size + Hoh_Oldest_Age + Household_Income +
            Children + Connection_Speed, data = amzon_mothsales_0mile,  index = c("Zip_Code","MonthYear"), pre.post = TRUE)

# Error: Will remove all treated units then min.T0 >= 4; treated units has too few pre-treatment periods when min.T0 <= 3
out <- gsynth(logTotalMonthlySales ~ Treatment + endo + Household_Size + Hoh_Oldest_Age + Household_Income +
              Children + Connection_Speed, data = amzon_mothsales_0mile, index = c("Zip_Code","MonthYear"), force = "two-way", 
              CV = FALSE, r = c(0, 5), estimator = "mc", se = TRUE, inference = "parametric", min.T0 = 4, nboots = 1000, parallel = TRUE)



amzon_mothsales_5mile <- sqldf("SELECT SUM(prod_totprice) AS TotalMonthlySales, AVG(treatment) AS Treatment, AVG(BBStorePresent) AS BBStorePresent,
                               AVG(household_size) AS Household_Size, AVG(hoh_oldest_age) AS Hoh_Oldest_Age, AVG(household_income) AS Household_Income, 
                               AVG(children) AS Children, AVG(connection_speed) AS Connection_Speed, MonthYear, Zip_Code 
                               FROM concat_data2 WHERE domain_name='amazon.com' GROUP BY MonthYear, Zip_Code ORDER BY Zip_Code, MonthYear")
# number of control group = 2855
num_control = length(unique(amzon_mothsales_5mile$Zip_Code)) - length(unique(amzon_mothsales_5mile[which(amzon_mothsales_5mile$Treatment == 1), ]$Zip_Code))
# the last pre-treatment period = 10
t0 = length(unique(amzon_mothsales_5mile$MonthYear)) - length(unique(amzon_mothsales_5mile[which(amzon_mothsales_5mile$Treatment == 1), ]$MonthYear))

amzon_mothsales_5mile$logTotalMonthlySales = log(amzon_mothsales_5mile$TotalMonthlySales + 1)
amzon_mothsales_5mile$endo = amzon_mothsales_5mile$Treatment * amzon_mothsales_5mile$BBStorePresent

# Visualize the data structure and spot missing values
panelView(logTotalMonthlySales ~ Treatment + endo + Household_Size + Hoh_Oldest_Age + Household_Income +
            Children + Connection_Speed, data = amzon_mothsales_5mile,  index = c("Zip_Code","MonthYear"), pre.post = TRUE)

# Error: treated units has too few pre-treatment periods when min.T0 <= 6
out <- gsynth(logTotalMonthlySales ~ Treatment + endo + Household_Size + Hoh_Oldest_Age + Household_Income +
                Children + Connection_Speed, data = amzon_mothsales_5mile, index = c("Zip_Code","MonthYear"), force = "two-way", 
              CV = FALSE, r = c(0, 5), se = TRUE, inference = "parametric", min.T0 = 7, nboots = 1000, parallel = TRUE)



################################# BestBuy Total month sales #####################################
# Select all the observable variables that can be grouped
# Group data by zip code and time
bestbuy_mothsales_0mile <- sqldf("SELECT SUM(prod_totprice) AS TotalMonthlySales, AVG(treatment) AS Treatment, AVG(BBStorePresent) AS BBStorePresent,
                               AVG(household_size) AS Household_Size, AVG(hoh_oldest_age) AS Hoh_Oldest_Age, AVG(household_income) AS Household_Income, 
                               AVG(children) AS Children, AVG(connection_speed) AS Connection_Speed, MonthYear, Zip_Code 
                               FROM concat_data1 WHERE domain_name='bestbuy.com' GROUP BY MonthYear, Zip_Code ORDER BY Zip_Code, MonthYear")
# number of control group = 597
num_control = length(unique(bestbuy_mothsales_0mile$Zip_Code)) - length(unique(bestbuy_mothsales_0mile[which(bestbuy_mothsales_0mile$Treatment == 1), ]$Zip_Code))
# the last pre-treatment period = 16
t0 = length(unique(bestbuy_mothsales_0mile$MonthYear)) - length(unique(bestbuy_mothsales_0mile[which(bestbuy_mothsales_0mile$Treatment == 1), ]$MonthYear))

bestbuy_mothsales_0mile$logTotalMonthlySales = log(bestbuy_mothsales_0mile$TotalMonthlySales + 1)
bestbuy_mothsales_0mile$endo = bestbuy_mothsales_0mile$Treatment * bestbuy_mothsales_0mile$BBStorePresent

# Visualize the data structure and spot missing values
panelView(logTotalMonthlySales ~ Treatment + endo + Household_Size + Hoh_Oldest_Age + Household_Income +
            Children + Connection_Speed, data = bestbuy_mothsales_0mile,  index = c("Zip_Code","MonthYear"), pre.post = TRUE)

# Error: Will remove all treated units
out <- gsynth(logTotalMonthlySales ~ Treatment + endo + Household_Size + Hoh_Oldest_Age + Household_Income +
                Children + Connection_Speed, data = bestbuy_mothsales_0mile, index = c("Zip_Code","MonthYear"), force = "two-way", 
              CV = FALSE, r = c(0, 5), estimator = "mc", lambda = 1, se = TRUE, inference = "nonparametric", min.T0 = 2, nboots = 1000, parallel = TRUE)
              