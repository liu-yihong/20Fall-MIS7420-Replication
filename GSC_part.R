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
  mutate(treatment = ifelse(CCStorePresent==1 & AfterStoreClosing==1, 1, 0))
concat_data2 %<>% 
  mutate(treatment = ifelse(CCStorePresent==1 & AfterStoreClosing==1, 1, 0))




################################# Amazon Total monthly sales 0 mile #####################################
# Select all the observable variables that can be grouped
# Group data by zip code and time
amazon_monthsales_0mile <- sqldf("SELECT SUM(prod_totprice) AS TotalMonthlySales, AVG(treatment) AS Treatment, AVG(BBStorePresent) AS BBStorePresent,
                               AVG(household_size) AS Household_Size, AVG(hoh_oldest_age) AS Hoh_Oldest_Age, AVG(household_income) AS Household_Income, 
                               AVG(children) AS Children, AVG(connection_speed) AS Connection_Speed, MonthYear, Zip_Code 
                               FROM concat_data1 WHERE domain_name='amazon.com' GROUP BY MonthYear, Zip_Code ORDER BY Zip_Code, MonthYear")
# number of control group = 2796
num_control = length(unique(amazon_monthsales_0mile$Zip_Code)) - length(unique(amazon_monthsales_0mile[which(amazon_monthsales_0mile$Treatment == 1), ]$Zip_Code))
# the last pre-treatment period = 10
t0 = length(unique(amazon_monthsales_0mile$MonthYear)) - length(unique(amazon_monthsales_0mile[which(amazon_monthsales_0mile$Treatment == 1), ]$MonthYear))

amazon_monthsales_0mile$logTotalMonthlySales = log(amazon_monthsales_0mile$TotalMonthlySales + 1)
amazon_monthsales_0mile$endo = amazon_monthsales_0mile$Treatment * amazon_monthsales_0mile$BBStorePresent

# Visualize the data structure for treated units and spot missing values
treated_zip = unique(amazon_monthsales_0mile[which(amazon_monthsales_0mile$Treatment == 1), ]$Zip_Code)
control_zip = unique(amazon_monthsales_0mile[which(amazon_monthsales_0mile$Treatment == 0), ]$Zip_Code)
panelView(logTotalMonthlySales ~ Treatment + endo + Household_Size + Hoh_Oldest_Age + Household_Income + Children + Connection_Speed, 
          data = amazon_monthsales_0mile, show.id = c(970:1030), theme.bw = TRUE, index = c("Zip_Code","MonthYear"), 
          xlab = "Time", axis.adjust = TRUE, pre.post = TRUE, main = "Panel View of a Subset of Amazon Data after Grouping")                                                                                                                                                                                                                                                                                                                                                                                                                                                but increase r (can only run with r={0,1} and min.To = 3)

# run the equation (can only run with r={0,1}.
out_amazon_monthsales_0mile <- gsynth(logTotalMonthlySales ~ Treatment + Household_Size + Hoh_Oldest_Age + Household_Income +
                Children + Connection_Speed, data = amazon_monthsales_0mile, index = c("Zip_Code","MonthYear"), force = "two-way", 
              CV = TRUE, r = c(0,1), se = TRUE, inference = "parametric", min.T0 = 3, nboots = 1000, parallel = TRUE, seed = 1)

# insignificant result
print(out_amazon_monthsales_0mile)

# some figures
plot(out_amazon_monthsales_0mile, type = "raw", theme.bw = TRUE, axis.adjust = TRUE)
plot(out_amazon_monthsales_0mile, type = "counterfactual", raw = "all", theme.bw = TRUE, axis.adjust = TRUE)




################################# Amazon Total monthly sales 5 mile #####################################
# Select all the observable variables that can be grouped
# Group data by zip code and time
amazon_monthsales_5mile <- sqldf("SELECT SUM(prod_totprice) AS TotalMonthlySales, AVG(treatment) AS Treatment, AVG(BBStorePresent) AS BBStorePresent,
                               AVG(household_size) AS Household_Size, AVG(hoh_oldest_age) AS Hoh_Oldest_Age, AVG(household_income) AS Household_Income, 
                               AVG(children) AS Children, AVG(connection_speed) AS Connection_Speed, MonthYear, Zip_Code 
                               FROM concat_data2 WHERE domain_name='amazon.com' GROUP BY MonthYear, Zip_Code ORDER BY Zip_Code, MonthYear")
# number of control group = 2855
num_control = length(unique(amazon_monthsales_5mile$Zip_Code)) - length(unique(amazon_monthsales_5mile[which(amazon_monthsales_5mile$Treatment == 1), ]$Zip_Code))
# the last pre-treatment period = 10
t0 = length(unique(amazon_monthsales_5mile$MonthYear)) - length(unique(amazon_monthsales_5mile[which(amazon_monthsales_5mile$Treatment == 1), ]$MonthYear))

amazon_monthsales_5mile$logTotalMonthlySales = log(amazon_monthsales_5mile$TotalMonthlySales + 1)
amazon_monthsales_5mile$endo = amazon_mothsales_5mile$Treatment * amazon_monthsales_5mile$BBStorePresent

# run the equation (can only run with r={0,1}. MSPE increases as min.T0 increases)
out_amazon_monthsales_5mile <- gsynth(logTotalMonthlySales ~ Treatment + Household_Size + Hoh_Oldest_Age + Household_Income +
                                        Children + Connection_Speed, data = amazon_monthsales_5mile, index = c("Zip_Code","MonthYear"), force = "two-way", 
                                      CV = TRUE, r = c(0,1), se = TRUE, inference = "parametric", min.T0 = 3, nboots = 1000, parallel = TRUE, seed = 1)

# insignificant result
print(out_amazon_monthsales_5mile)




################################# Amazon Pages Per Dollar 0 mile #####################################
# Select all the observable variables that can be grouped
# Group data by zip code and time
amazon_PagesPerDollar_0mile <- sqldf("SELECT PagesPerDollar, AVG(treatment) AS Treatment, AVG(BBStorePresent) AS BBStorePresent,
                               AVG(household_size) AS Household_Size, AVG(hoh_oldest_age) AS Hoh_Oldest_Age, AVG(household_income) AS Household_Income, 
                               AVG(children) AS Children, AVG(connection_speed) AS Connection_Speed, MonthYear, Zip_Code 
                               FROM concat_data1 WHERE domain_name='amazon.com' GROUP BY MonthYear, Zip_Code ORDER BY Zip_Code, MonthYear")

amazon_PagesPerDollar_0mile$logPagesPerDollar = log(amazon_PagesPerDollar_0mile$PagesPerDollar + 1)
amazon_PagesPerDollar_0mile$endo = amazon_PagesPerDollar_0mile$Treatment * amazon_PagesPerDollar_0mile$BBStorePresent

# run the equation (can only run with r={0,1} and min.To = 3)
out_amazon_PagesPerDollar_0mile <- gsynth(logPagesPerDollar ~ Treatment + Household_Size + Hoh_Oldest_Age + Household_Income +
                                      Children + Connection_Speed, data = amazon_PagesPerDollar_0mile, index = c("Zip_Code","MonthYear"), force = "two-way", 
                                      CV = TRUE, r = c(0,1), se = TRUE, inference = "parametric", min.T0 = 3, nboots = 1000, parallel = TRUE, seed = 1)

# insignificant result
print(out_amazon_PagesPerDollar_0mile)

# some figures
plot(out_amazon_PagesPerDollar_0mile, type = "raw", theme.bw = TRUE, axis.adjust = TRUE)
plot(out_amazon_PagesPerDollar_0mile, type = "counterfactual", raw = "all", theme.bw = TRUE, axis.adjust = TRUE)



################################# Amazon Pages Per Dollar 5 mile #####################################

amazon_PagesPerDollar_5mile <- sqldf("SELECT PagesPerDollar, AVG(treatment) AS Treatment, AVG(BBStorePresent) AS BBStorePresent,
                               AVG(household_size) AS Household_Size, AVG(hoh_oldest_age) AS Hoh_Oldest_Age, AVG(household_income) AS Household_Income, 
                               AVG(children) AS Children, AVG(connection_speed) AS Connection_Speed, MonthYear, Zip_Code 
                               FROM concat_data2 WHERE domain_name='amazon.com' GROUP BY MonthYear, Zip_Code ORDER BY Zip_Code, MonthYear")

amazon_PagesPerDollar_5mile$logPagesPerDollar = log(amazon_PagesPerDollar_5mile$PagesPerDollar + 1)
amazon_PagesPerDollar_5mile$endo = amazon_PagesPerDollar_5mile$Treatment * amazon_PagesPerDollar_5mile$BBStorePresent

# Without BBStorePresent, but increase r (can only run with r={0,1}). MSPE increases as min.T0 increases
out_amazon_PagesPerDollar_5mile <- gsynth(logPagesPerDollar ~ Treatment + Household_Size + Hoh_Oldest_Age + Household_Income +
                                        Children + Connection_Speed, data = amazon_PagesPerDollar_5mile, index = c("Zip_Code","MonthYear"), force = "two-way", 
                                      CV = TRUE, r = c(0,1), se = TRUE, inference = "parametric", min.T0 = 3, nboots = 1000, parallel = TRUE, seed = 1)


# insignificant result
print(out_amazon_PagesPerDollar_5mile)




################################# Amazon Mins Per Dollar 0 mile #####################################

amazon_MinsPerDollar_0mile <- sqldf("SELECT MinsPerDollar, AVG(treatment) AS Treatment, AVG(BBStorePresent) AS BBStorePresent,
                               AVG(household_size) AS Household_Size, AVG(hoh_oldest_age) AS Hoh_Oldest_Age, AVG(household_income) AS Household_Income, 
                               AVG(children) AS Children, AVG(connection_speed) AS Connection_Speed, MonthYear, Zip_Code 
                               FROM concat_data1 WHERE domain_name='amazon.com' GROUP BY MonthYear, Zip_Code ORDER BY Zip_Code, MonthYear")

amazon_MinsPerDollar_0mile$logMinsPerDollar = log(amazon_MinsPerDollar_0mile$MinsPerDollar + 1)
amazon_MinsPerDollar_0mile$endo = amazon_MinsPerDollar_0mile$Treatment * amazon_MinsPerDollar_0mile$BBStorePresent

# run the equation (can only run  with r={0,1})
out_amazon_MinsPerDollar_0mile <- gsynth(logMinsPerDollar ~ Treatment + Household_Size + Hoh_Oldest_Age + Household_Income +
                                            Children + Connection_Speed, data = amazon_MinsPerDollar_0mile, index = c("Zip_Code","MonthYear"), force = "two-way", 
                                          CV = TRUE, r = c(0,1), se = TRUE, inference = "parametric", min.T0 = 3, nboots = 1000, parallel = TRUE, seed = 1)

# insignificant result
print(out_amazon_MinsPerDollar_0mile)




################################# Amazon Mins Per Dollar 5 mile #####################################

amazon_MinsPerDollar_5mile <- sqldf("SELECT MinsPerDollar, AVG(treatment) AS Treatment, AVG(BBStorePresent) AS BBStorePresent,
                               AVG(household_size) AS Household_Size, AVG(hoh_oldest_age) AS Hoh_Oldest_Age, AVG(household_income) AS Household_Income, 
                               AVG(children) AS Children, AVG(connection_speed) AS Connection_Speed, MonthYear, Zip_Code 
                               FROM concat_data1 WHERE domain_name='amazon.com' GROUP BY MonthYear, Zip_Code ORDER BY Zip_Code, MonthYear")

amazon_MinsPerDollar_5mile$logMinsPerDollar = log(amazon_MinsPerDollar_0mile$MinsPerDollar + 1)
amazon_MinsPerDollar_5mile$endo = amazon_MinsPerDollar_5mile$Treatment * amazon_MinsPerDollar_5mile$BBStorePresent

# Without BBStorePresent, but increase r (can only run with r={0,1}). MSPE increases as min.T0 increases
out_amazon_MinsPerDollar_5mile <- gsynth(logMinsPerDollar ~ Treatment + Household_Size + Hoh_Oldest_Age + Household_Income +
                                           Children + Connection_Speed, data = amazon_MinsPerDollar_5mile, index = c("Zip_Code","MonthYear"), force = "two-way", 
                                         CV = TRUE, r = c(0,1), se = TRUE, inference = "parametric", min.T0 = 3, nboots = 1000, parallel = TRUE, seed = 1)

# insignificant result
print(out_amazon_MinsPerDollar_5mile)


