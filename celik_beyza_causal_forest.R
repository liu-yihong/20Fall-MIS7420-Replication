rm(list = ls())
library(magrittr)
library(tidyverse)
library(haven)
library(zoo)
library(plm)
library(stargazer)
library(grf)

# data paths
data.dir <- "data"
bb_zipcode_path <- file.path(data.dir,"bestbuyzipcodes_sample.sas7bdat")
sales_cc_0mile_path <- file.path(data.dir,"sales_ccity0milezipcode_sample.sas7bdat")
sales_cc_5miles_path <- file.path(data.dir,"sales_ccity5milezipcode_sample.sas7bdat")
sales_allother_zipcode_path <- file.path(data.dir,"sales_allotherzipcode_sample.sas7bdat")

# load data
bb_zipcode <- read_sas(bb_zipcode_path) %>% 
  mutate_all(~as.factor(.))

sales_cc_0mile <- read_sas(sales_cc_0mile_path) %>% 
  mutate_at(vars(-pages_viewed, -duration,-event_date, -event_time,
                 -prod_name, -prod_qty, -prod_totprice, -basket_tot, 
                 -household_income), as.factor)

sales_cc_5miles <- read_sas(sales_cc_5miles_path) %>% 
  mutate_at(vars(-pages_viewed, -duration,-event_date, -event_time,
                 -prod_name, -prod_qty, -prod_totprice, -basket_tot, 
                 -household_income), as.factor)

# use encoding="latin1" in mac and linux, otherwise you don't need it
sales_allother_zipcode <- read_sas(sales_allother_zipcode_path, encoding = "latin1") %>% 
  mutate(Store_Close_Status = 0) %>% # NaN means no CC in 5-miles radius, we change NaN to 0
  mutate_at(vars(-pages_viewed, -duration,-event_date, -event_time,
                 -prod_name, -prod_qty, -prod_totprice, -basket_tot, 
                 -household_income), as.factor)

# Exclude Data without purchase
# All data should be with purchase -> tran_flg == 1
# tran_flg=1 in all datasets already we do not need to filter 
concat_data1 <- rbind(sales_allother_zipcode, sales_cc_0mile)
concat_data2 <- rbind(sales_allother_zipcode, sales_cc_5miles)

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
# remove 28, 30, 39, 40
# all_data$prod_category_id %>% unique() %>% sort
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

# Mark CC Closure

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
  mutate(BBStorePresent = ifelse(is.na(BB_Store_Status), 0, BB_Store_Status) %>% 
           as.factor,
         PagesPerDollar = pages_viewed/prod_totprice,
         MinsPerDollar = duration/prod_totprice) 

concat_data2 %<>% 
  merge(.,bb_zipcode, by.x ="Zip_Code", by.y = "Zip_Code", all.x = TRUE) %>% 
  mutate(BBStorePresent = ifelse(is.na(BB_Store_Status), 0, BB_Store_Status) %>% 
           as.factor,
         PagesPerDollar = pages_viewed/prod_totprice,
         MinsPerDollar = duration/prod_totprice)

# Sales Effect

cf_d1 <- concat_data1 %>% 
  mutate(treatment = ifelse(CCStorePresent==1 & AfterStoreClosing==1, 1, 0)) %>% 
  select(-Store_Close_Status, -domain_id, -ref_domain_name, -MinsPerDollar,
         -event_date,-event_time,-tran_flg,-prod_name, -MonthYear,
         -CCStorePresent,- AfterStoreClosing,-BB_Store_Status, -PagesPerDollar,
         -site_session_id, -prod_category_id, -basket_tot, -machine_id, -Zip_Code)

cf_d2 <- concat_data2  %>%
  mutate(treatment=ifelse(CCStorePresent==1 & AfterStoreClosing==1, 1, 0)) %>% 
  select(-Store_Close_Status,-domain_id, -ref_domain_name, -MinsPerDollar,
         -event_date,-event_time,-tran_flg,-prod_name, -MonthYear,
         -CCStorePresent, -AfterStoreClosing,-BB_Store_Status, -PagesPerDollar,
         -site_session_id,-prod_category_id, -basket_tot, -machine_id, -Zip_Code)



## Amazon Sales Effect using Zero Mile Data
set.seed(1)

ama_cf_d1 <- cf_d1 %>% 
  filter(domain_name=="amazon.com") %>% 
  select(-domain_name)

W1_ama <- ama_cf_d1$treatment 
Y1_ama <- ama_cf_d1$prod_totprice 

d1_ama <- ama_cf_d1 %>% 
  select(-pages_viewed, -duration, -prod_qty, 
         -prod_totprice, -household_income, -treatment)

d1_ama_exp <-model.matrix(~.+0, d1_ama)

X1_ama <- cbind(ama_cf_d1[,-c(4, 16, which(colnames(ama_cf_d1) %in% colnames(d1_ama)))], d1_ama_exp)

Y1_f_ama <- regression_forest(X1_ama, Y1_ama)
Y1_hat_ama <- predict(Y1_f_ama)$predictions

W1_f_ama <- regression_forest(X1_ama, W1_ama)
W1_hat_ama <- predict(W1_f_ama)$predictions

cf1_raw_ama <- causal_forest(X1_ama, Y1_ama, W1_ama,
                             Y.hat = Y1_hat_ama, W.hat = W1_hat_ama)

varimp1_ama <- variable_importance(cf1_raw_ama)
selected1_idx_ama <- which(varimp1_ama > mean(varimp1_ama))

cf1_ama <- causal_forest(X1_ama[,selected1_idx_ama], Y1_ama, W1_ama,
                         Y.hat = Y1_hat_ama, W.hat = W1_hat_ama,
                         tune.parameters = "all")

tau1_hat_ama <- predict(cf1_ama)$predictions


# Estimate ATE
alpha <- 1-0.95
average_treatment_effect(cf1_ama, target.sample = "all") %>%
  {.["estimate"] + c(-1,0,1)*qnorm(1-alpha/2)*.["std.err"]} %>% 
  as.table() %>% t() %>% 
  set_colnames(c("2.5% ", "Estimate", "97.5%")) %>% 
  xtable(caption = "95% CI for the ATE")

# Online Search Effect
## Search Breadth and Depth
### Amazon Pages Per Dollar using Zero Mile Data

cf_d3 <- concat_data1 %>% 
  mutate(treatment=ifelse(CCStorePresent==1 & AfterStoreClosing==1, 1, 0)) %>% 
  select(-Store_Close_Status, -domain_id, -ref_domain_name, -MinsPerDollar,
         -event_date,-event_time,-tran_flg,-prod_name, -MonthYear, -prod_totprice,
         -CCStorePresent,- AfterStoreClosing,-BB_Store_Status, -pages_viewed,
         -site_session_id, -prod_category_id, -basket_tot, -machine_id, -Zip_Code)

set.seed(1)

ama_cf_d3 <- cf_d3 %>% 
  filter(domain_name=="amazon.com") %>% 
  select(-domain_name)

W3_ama <- ama_cf_d3$treatment 
# Y3_ama <- {ama_cf_d3$PagesPerDollar +1}%>% log(.)
Y3_ama <- ama_cf_d3$PagesPerDollar 

d3_ama <- ama_cf_d3 %>% 
  select(-duration, -prod_qty, 
         -PagesPerDollar,
         -household_income, 
         -treatment)

d3_ama_exp <-model.matrix(~.+0, d3_ama)


# ama_cf_d1[,-"prod_totprice"]
X3_ama <- cbind(ama_cf_d3[,-c(14, 15, which(colnames(ama_cf_d3) %in% colnames(d3_ama)))], d3_ama_exp)

Y3_f_ama <- regression_forest(X3_ama, Y3_ama)
Y3_hat_ama <- predict(Y3_f_ama)$predictions

W3_f_ama <- regression_forest(X3_ama, W3_ama)
W3_hat_ama <- predict(W3_f_ama)$predictions

cf3_raw_ama <- causal_forest(X3_ama, Y3_ama, W3_ama,
                             Y.hat = Y3_hat_ama, W.hat = W3_hat_ama)

varimp3_ama <- variable_importance(cf3_raw_ama)
selected3_idx_ama <- which(varimp3_ama > mean(varimp3_ama))

cf3_ama <- causal_forest(X3_ama[,selected3_idx_ama], Y3_ama, W3_ama,
                         Y.hat = Y3_hat_ama, W.hat = W3_hat_ama,
                         tune.parameters = "all")

tau3_hat_ama <- predict(cf3_ama)$predictions

# Estimate ATE
alpha <- 1-0.95
average_treatment_effect(cf3_ama, target.sample = "all")%>%
  {.["estimate"] + c(-1,0,1)*qnorm(1-alpha/2)*.["std.err"]} %>% 
  as.table() %>% t() %>% 
  set_colnames(c("2.5% ", "Estimate", "97.5%")) %>% 
  xtable(caption = "95% CI for the ATE")


### Amazon Pages Per Dollar using Five Miles Data
cf_d4 <- concat_data2  %>%
  mutate(treatment=ifelse(CCStorePresent==1 & AfterStoreClosing==1, 1, 0)) %>% 
  select(-Store_Close_Status,-domain_id, -ref_domain_name, -MinsPerDollar,
         -event_date,-event_time,-tran_flg,-prod_name, -MonthYear, -prod_totprice,
         -CCStorePresent, -AfterStoreClosing,-BB_Store_Status, -pages_viewed,
         -site_session_id,-prod_category_id, -basket_tot, -machine_id, -Zip_Code)

set.seed(1)

ama_cf_d4 <- cf_d4 %>% 
  filter(domain_name=="amazon.com") %>% 
  select(-domain_name)

W4_ama <- ama_cf_d4$treatment 
Y4_ama <- ama_cf_d4$PagesPerDollar 

d4_ama <- ama_cf_d4 %>% 
  select(-duration, -prod_qty, 
         -PagesPerDollar,
         -household_income, 
         -treatment)

d4_ama_exp <-model.matrix(~.+0, d4_ama)


# ama_cf_d1[,-"prod_totprice"]
X4_ama <- cbind(ama_cf_d4[,-c(14, 15, which(colnames(ama_cf_d4) %in% colnames(d4_ama)))], d4_ama_exp)

Y4_f_ama <- regression_forest(X4_ama, Y4_ama)
Y4_hat_ama <- predict(Y4_f_ama)$predictions

W4_f_ama <- regression_forest(X4_ama, W4_ama)
W4_hat_ama <- predict(W4_f_ama)$predictions

cf4_raw_ama <- causal_forest(X4_ama, Y4_ama, W4_ama,
                             Y.hat = Y4_hat_ama, W.hat = W4_hat_ama)

varimp4_ama <- variable_importance(cf4_raw_ama)
selected4_idx_ama <- which(varimp4_ama > mean(varimp4_ama))

cf4_ama <- causal_forest(X4_ama[,selected4_idx_ama], Y4_ama, W4_ama,
                         Y.hat = Y4_hat_ama, W.hat = W4_hat_ama,
                         tune.parameters = "all")

tau4_hat_ama <- predict(cf4_ama)$predictions

ATE4_ama <- average_treatment_effect(cf4_ama, target.sample = "all")
paste("95% CI for the ATE:", round(ATE4_ama[1], 3),
      "+/-", round(qnorm(0.975) * ATE4_ama[2], 3))

### Best Buy Pages Per Dollar using Zero Mile Data
set.seed(1)

bb_cf_d3 <- cf_d3 %>% 
  filter(domain_name=="bestbuy.com") %>% 
  select(-domain_name)

W3_bb <- bb_cf_d3$treatment 
Y3_bb <- bb_cf_d3$PagesPerDollar 

d3_bb <- bb_cf_d3 %>% 
  select(-duration, -prod_qty, 
         -PagesPerDollar,
        -household_income, 
        -treatment)

d3_bb_exp <-model.matrix(~.+0, d3_bb)


# bb_cf_d1[,-"prod_totprice"]
X3_bb <- cbind(bb_cf_d3[,-c(14, 15, which(colnames(bb_cf_d3) %in% colnames(d3_bb)))], d3_bb_exp)

Y3_f_bb <- regression_forest(X3_bb, Y3_bb)
Y3_hat_bb <- predict(Y3_f_bb)$predictions

W3_f_bb <- regression_forest(X3_bb, W3_bb)
W3_hat_bb <- predict(W3_f_bb)$predictions

cf3_raw_bb <- causal_forest(X3_bb, Y3_bb, W3_bb,
                            Y.hat = Y3_hat_bb, W.hat = W3_hat_bb)

varimp3_bb <- variable_importance(cf3_raw_bb)
selected3_idx_bb <- which(varimp3_bb > mean(varimp3_bb))

cf3_bb <- causal_forest(X3_bb[,selected3_idx_bb], Y3_bb, W3_bb,
                        Y.hat = Y3_hat_bb, W.hat = W3_hat_bb,
                        tune.parameters = "all")

tau3_hat_bb <- predict(cf3_bb)$predictions



ATE3_bb <- average_treatment_effect(cf3_bb, target.sample = "all")
paste("95% CI for the ATE:", round(ATE3_bb[1], 3),
      "+/-", round(qnorm(0.975) * ATE3_bb[2], 3))

### Amazon Minutes Per Dollar using Zero Mile Data

```{r}
cf_d5 <- concat_data1 %>% 
  mutate(treatment=ifelse(CCStorePresent==1 & AfterStoreClosing==1, 1, 0)) %>% 
  select(-Store_Close_Status, -domain_id, -ref_domain_name, -PagesPerDollar,
         -event_date,-event_time,-tran_flg,-prod_name, -MonthYear, -prod_totprice,
         -CCStorePresent,- AfterStoreClosing,-BB_Store_Status, -duration,
         -site_session_id, -prod_category_id, -basket_tot, -machine_id, -Zip_Code)
```

```{r}
set.seed(1)

ama_cf_d5 <- cf_d5 %>% 
  filter(domain_name=="amazon.com") %>% 
  select(-domain_name)

W5_ama <- ama_cf_d5$treatment 
Y5_ama <- ama_cf_d5$MinsPerDollar 

d5_ama <- ama_cf_d5 %>% 
  select(-pages_viewed, -prod_qty, 
         -MinsPerDollar,
         -household_income, 
          -treatment)

d5_ama_exp <-model.matrix(~.+0, d5_ama)


# ama_cf_d1[,-"prod_totprice"]
X5_ama <- cbind(ama_cf_d5[,-c(14, 15, which(colnames(ama_cf_d5) %in% colnames(d5_ama)))], d5_ama_exp)

Y5_f_ama <- regression_forest(X5_ama, Y5_ama)
Y5_hat_ama <- predict(Y5_f_ama)$predictions

W5_f_ama <- regression_forest(X5_ama, W5_ama)
W5_hat_ama <- predict(W5_f_ama)$predictions

cf5_raw_ama <- causal_forest(X5_ama, Y5_ama, W5_ama,
                             Y.hat = Y5_hat_ama, W.hat = W5_hat_ama)

varimp5_ama <- variable_importance(cf5_raw_ama)
selected5_idx_ama <- which(varimp5_ama > mean(varimp5_ama))

cf5_ama <- causal_forest(X5_ama[,selected5_idx_ama], Y5_ama, W5_ama,
                         Y.hat = Y5_hat_ama, W.hat = W5_hat_ama,
                         tune.parameters = "all")

tau5_hat_ama <- predict(cf5_ama)$predictions

# Estimate ATE


ATE5_ama <- average_treatment_effect(cf5_ama, target.sample = "all")
paste("95% CI for the ATE:", round(ATE5_ama[1], 3),
      "+/-", round(qnorm(0.975) * ATE5_ama[2], 3))

### Amazon Minutes Per Dollar using Five Miles Data
cf_d6 <- concat_data2 %>% 
  mutate(treatment=ifelse(CCStorePresent==1 & AfterStoreClosing==1, 1, 0)) %>% 
  select(-Store_Close_Status, -domain_id, -ref_domain_name, -PagesPerDollar,
         -event_date,-event_time,-tran_flg,-prod_name, -MonthYear, -prod_totprice,
         -CCStorePresent,- AfterStoreClosing,-BB_Store_Status, -duration,
         -site_session_id, -prod_category_id, -basket_tot, -machine_id, -Zip_Code)

set.seed(1)

ama_cf_d6 <- cf_d6 %>% 
  filter(domain_name=="amazon.com") %>% 
  select(-domain_name)

W6_ama <- ama_cf_d6$treatment 
Y6_ama <- ama_cf_d6$MinsPerDollar 

d6_ama <- ama_cf_d6 %>% 
  select(-pages_viewed, -prod_qty, 
          -MinsPerDollar,
   -household_income, 
-treatment)

d6_ama_exp <-model.matrix(~.+0, d6_ama)

X6_ama <- cbind(ama_cf_d6[,-c(14, 15, which(colnames(ama_cf_d6) %in% colnames(d6_ama)))], d6_ama_exp)

Y6_f_ama <- regression_forest(X6_ama, Y6_ama)
Y6_hat_ama <- predict(Y6_f_ama)$predictions

W6_f_ama <- regression_forest(X6_ama, W6_ama)
W6_hat_ama <- predict(W6_f_ama)$predictions

cf6_raw_ama <- causal_forest(X6_ama, Y6_ama, W6_ama,
                             Y.hat = Y6_hat_ama, W.hat = W6_hat_ama)

varimp6_ama <- variable_importance(cf6_raw_ama)
selected6_idx_ama <- which(varimp6_ama > mean(varimp6_ama))

cf6_ama <- causal_forest(X6_ama[,selected6_idx_ama], Y6_ama, W6_ama,
                         Y.hat = Y6_hat_ama, W.hat = W6_hat_ama,
                         tune.parameters = "all")

tau6_hat_ama <- predict(cf6_ama)$predictions


# Estimate ATE


ATE6_ama <- average_treatment_effect(cf6_ama, target.sample = "all")
paste("95% CI for the ATE:", round(ATE6_ama[1], 3),
      "+/-", round(qnorm(0.975) * ATE6_ama[2], 3))


