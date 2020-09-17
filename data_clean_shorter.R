
rm(list = ls())
library(magrittr)
library(tidyverse)
library(haven)

# data paths
data.dir <- "data"
bb_zipcode_path <- file.path(data.dir,"bestbuyzipcodes_sample.sas7bdat")
sales_cc_0mile_path <- file.path(data.dir,"sales_ccity0milezipcode_sample.sas7bdat")
sales_cc_5miles_path <- file.path(data.dir,"sales_ccity5milezipcode_sample.sas7bdat")
sales_allother_zipcode_path <- file.path(data.dir,"sales_allotherzipcode_sample.sas7bdat")

# load data
bb_zipcode <- read_sas(bb_zipcode_path) 
sales_cc_0mile <- read_sas(sales_cc_0mile_path) %>% 
  mutate(distance = factor("0mile"))
sales_cc_5miles <- read_sas(sales_cc_5miles_path) %>% 
  mutate(distance = factor("5mile"))

# use encoding="latin1" in mac and linux, otherwise you don't need it
sales_allother_zipcode <- read_sas(sales_allother_zipcode_path, encoding = "latin1") %>% 
  mutate(Store_Close_Status = 0, # NaN means no CC in 5-miles radius, we change NaN to 0
         distance=factor("other"))

# aggregate data
all_data <- rbind(sales_allother_zipcode, sales_cc_0mile, sales_cc_5miles) 

# Filter Referring Domain
# we identify some search engines
search_engine_to_consider1 <- c("GOOGLE.COM", "YAHOO.COM", "google.com", "yahoo.com",
                                "MSN.COM", "msn.com", "aol.com", "AOL.COM", "LIVE.COM", "live.com",
                                "MYWEBSEARCH.COM", "ASK.COM", "MYWAY.COM", "mywebsearch.com",
                                "ask.com", "YAHOO.NET", "BIZRATE.COM", "bizrate.com")

ref_domain_to_consider1 <- c("", "GOOGLE.COM", "YAHOO.COM", "google.com", "yahoo.com",
                             "MSN.COM", "msn.com", "aol.com", "AOL.COM", "LIVE.COM", "live.com",
                             "MYWEBSEARCH.COM", "ASK.COM", "MYWAY.COM", "mywebsearch.com",
                             "ask.com", "YAHOO.NET", "BIZRATE.COM", "bizrate.com")

five_target_domain_to_consider <- c("amazon.com", "staples.com", "dell.com", "walmart.com", "bestbuy.com")
# Product Categories
category_to_consider <- c(22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37)


# Then we filter data by refer domain name
# we can choose what filter to apply
all_data %<>% filter(ref_domain_name %in% ref_domain_to_consider1 & 
                       domain_name %in% five_target_domain_to_consider &
                       prod_category_id %in% category_to_consider)

all_data %<>% mutate(MonthYear = format(event_date, "%Y-%m"),# construct MonthYear - month of year
                     CCStorePresent = Store_Close_Status,# Mark CC Closure # CCStorePresent
                     AfterStoreClosing = ifelse(MonthYear < "2008-11", 0, 1)) %>% 
  merge(.,bb_zipcode, by.x ="Zip_Code", by.y = "Zip_Code", all.x = TRUE) %>% 
  mutate(BBStorePresent = na.fill(BB_Store_Status, 0),# BBStorePresent
         NoReferringDomain = ifelse(ref_domain_name == '', 1, 0),
         ReferringDomainIsSearchEngine = ifelse(ref_domain_name %in% search_engine_to_consider1, 1, 0))
        
                  

