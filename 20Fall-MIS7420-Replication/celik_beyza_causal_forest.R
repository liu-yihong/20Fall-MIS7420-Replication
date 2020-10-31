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
                 -household_income), as.factor) %>% unique

sales_cc_5miles <- read_sas(sales_cc_5miles_path) %>% 
  mutate_at(vars(-pages_viewed, -duration,-event_date, -event_time,
                 -prod_name, -prod_qty, -prod_totprice, -basket_tot, 
                 -household_income), as.factor) %>% unique

# use encoding="latin1" in mac and linux, otherwise you don't need it
sales_allother_zipcode <- read_sas(sales_allother_zipcode_path, encoding = "latin1") %>% 
  mutate(Store_Close_Status = 0) %>% # NaN means no CC in 5-miles radius, we change NaN to 0
  mutate_at(vars(-pages_viewed, -duration,-event_date, -event_time,
                 -prod_name, -prod_qty, -prod_totprice, -basket_tot, 
                 -household_income), as.factor) %>% unique

# Exclude Data without purchase
# All data should be with purchase -> tran_flg == 1
# tran_flg=1 in all datasets already we do not need to filter 
concat_data1 <- rbind(sales_allother_zipcode, sales_cc_0mile)
concat_data2 <- rbind(sales_allother_zipcode, sales_cc_5miles)
concat_all_data <- rbind(concat_data1, sales_cc_5miles) %>% unique 

# Filter Referring Domain

# we identify some search engines

ref_domain_to_consider1 <- c("", "GOOGLE.COM", "YAHOO.COM", "google.com", "yahoo.com",
                             "MSN.COM", "msn.com", "aol.com", "AOL.COM", "LIVE.COM", "live.com",
                             "MYWEBSEARCH.COM", "ASK.COM", "MYWAY.COM", "mywebsearch.com",
                             "ask.com", "YAHOO.NET", "BIZRATE.COM", "bizrate.com")

# Then we filter data by refer domain name
concat_data1 %<>% filter(ref_domain_name %in% ref_domain_to_consider1)
concat_data2 %<>% filter(ref_domain_name %in% ref_domain_to_consider1)
concat_all_data %<>% filter(ref_domain_name %in% ref_domain_to_consider1) 

# Filter Target Domain Name
five_target_domain_to_consider <- c("amazon.com", "staples.com", "dell.com", "walmart.com", "bestbuy.com")
two_target_domain_to_consider <- c("amazon.com","bestbuy.com")

# we can choose what filter to apply
concat_data1 %<>% filter(domain_name %in% five_target_domain_to_consider)
concat_data2 %<>% filter(domain_name %in% five_target_domain_to_consider)
concat_all_data %<>% filter(domain_name %in% five_target_domain_to_consider)

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
concat_all_data %<>% 
  filter(prod_category_id %in% category_to_consider) %>% 
  mutate(prod_category_type = ifelse(prod_category_id %in% experience_product, 1, 0) %>% 
           as.factor()) #experience proudct=1, search product=0

# construct MonthYear - month of year
concat_data1 %<>% mutate(MonthYear = format(event_date, "%Y-%m") )
concat_data2 %<>% mutate(MonthYear = format(event_date, "%Y-%m") )
concat_all_data %<>% mutate(MonthYear = format(event_date, "%Y-%m") )

# Mark CC Closure

# CCStorePresent
# it is the same as Store_Close_Status
concat_data1 %<>% mutate(CCStorePresent = Store_Close_Status)
concat_data2 %<>% mutate(CCStorePresent = Store_Close_Status)
concat_all_data %<>% mutate(CCStorePresent = Store_Close_Status)

# AfterStoreClosing
concat_data1 %<>% mutate(AfterStoreClosing = ifelse(MonthYear < "2008-11", 0, 1) %>% 
                           as.factor())
concat_data2 %<>% mutate(AfterStoreClosing = ifelse(MonthYear < "2008-11", 0, 1) %>% 
                           as.factor()) 
concat_all_data %<>% mutate(AfterStoreClosing = ifelse(MonthYear < "2008-11", 0, 1) %>% 
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

concat_all_data %<>% 
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

cf_all <- concat_all_data  %>%
  mutate(treatment=ifelse(CCStorePresent==1 & AfterStoreClosing==1, 1, 0)) %>% 
  select(-Store_Close_Status,-domain_id, -ref_domain_name, -MinsPerDollar,
         -event_date,-event_time,-tran_flg,-prod_name, -MonthYear,
         -CCStorePresent, -AfterStoreClosing,-BB_Store_Status, -PagesPerDollar,
         -site_session_id,-prod_category_id, -basket_tot, -machine_id, -Zip_Code)

## Amazon Sales Effect using All Data
set.seed(1)

ama_cf_all <- cf_all %>% 
  filter(domain_name=="amazon.com") %>% 
  select(-domain_name)

W_all_ama <- ama_cf_all$treatment 

Y_all_ama <- ama_cf_all$prod_totprice %>% log(.) %>% {.+1}


d_all_ama <- ama_cf_all %>% 
  select(-pages_viewed, -duration, -prod_qty, 
         -prod_totprice, -household_income, -treatment)

d_all_ama_exp <-model.matrix(~.+0, d_all_ama)

X_all_ama <- cbind(ama_cf_all[,-c(4, 16, which(colnames(ama_cf_all) %in% colnames(d_all_ama)))], d_all_ama_exp)

Y_all_f_ama <- regression_forest(X_all_ama, Y_all_ama)
Y_all_hat_ama <- predict(Y_all_f_ama)$predictions

W_all_f_ama <- regression_forest(X_all_ama, W_all_ama)
W_all_hat_ama <- predict(W_all_f_ama)$predictions

cf_all_raw_ama <- causal_forest(X_all_ama, Y_all_ama, W_all_ama,
                                Y.hat = Y_all_hat_ama, W.hat = W_all_hat_ama)

varimp_all_ama <- variable_importance(cf_all_raw_ama)
selected_all_idx_ama <- which(varimp_all_ama > mean(varimp_all_ama))

cf_all_ama <- causal_forest(X_all_ama[,selected_all_idx_ama], Y_all_ama, W_all_ama,
                            Y.hat = Y_all_hat_ama, W.hat = W_all_hat_ama,
                            tune.parameters = "all")

tau_all_hat_ama <- predict(cf_all_ama)$predictions

# Estimate ATE

alpha <- 1-0.90
average_treatment_effect(cf_all_ama, target.sample = "all") %>%
  {.["estimate"] + c(-1,0,1)*qnorm(1-alpha/2)*.["std.err"]} %>% 
  as.table() %>% t() %>% 
  set_colnames(c("5% ", "Estimate", "95%")) %>% 
  xtable(caption = "90% CI for the ATE")

alpha <- 1-0.95
average_treatment_effect(cf_all_ama, target.sample = "all") %>%
  {.["estimate"] + c(-1,0,1)*qnorm(1-alpha/2)*.["std.err"]} %>% 
  as.table() %>% t() %>% 
  set_colnames(c("2.5% ", "Estimate", "97.5%")) %>% 
  xtable(caption = "95% CI for the ATE")

alpha <- 1-0.99
average_treatment_effect(cf_all_ama, target.sample = "all") %>%
  {.["estimate"] + c(-1,0,1)*qnorm(1-alpha/2)*.["std.err"]} %>% 
  as.table() %>% t() %>% 
  set_colnames(c("0.5% ", "Estimate", "99.5%")) %>% 
  xtable(caption = "99% CI for the ATE")

# CATE Histogram
# pdf("tauhat1_ama_hist.pdf")
# pardef = par(mar = c(5, 4, 4, 2) + 0.5, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
hist(tau_all_hat_ama, xlab = "estimated CATE", main = "")
# dev.off()


## Amazon Sales Effect using Zero Mile Data
set.seed(1)

ama_cf_d1 <- cf_d1 %>% 
  filter(domain_name=="amazon.com") %>% 
  select(-domain_name)

W1_ama <- ama_cf_d1$treatment 
Y1_ama <- ama_cf_d1$prod_totprice %>% log(.) %>% {.+1}

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

alpha <- 1-0.90
average_treatment_effect(cf1_ama, target.sample = "all") %>%
  {.["estimate"] + c(-1,0,1)*qnorm(1-alpha/2)*.["std.err"]} %>% 
  as.table() %>% t() %>% 
  set_colnames(c("5% ", "Estimate", "95%")) %>% 
  xtable(caption = "90% CI for the ATE")

alpha <- 1-0.95
average_treatment_effect(cf1_ama, target.sample = "all") %>%
  {.["estimate"] + c(-1,0,1)*qnorm(1-alpha/2)*.["std.err"]} %>% 
  as.table() %>% t() %>% 
  set_colnames(c("2.5% ", "Estimate", "97.5%")) %>% 
  xtable(caption = "95% CI for the ATE")

alpha <- 1-0.99
average_treatment_effect(cf1_ama, target.sample = "all") %>%
  {.["estimate"] + c(-1,0,1)*qnorm(1-alpha/2)*.["std.err"]} %>% 
  as.table() %>% t() %>% 
  set_colnames(c("0.5% ", "Estimate", "99.5%")) %>% 
  xtable(caption = "99% CI for the ATE")

# CATE Histogram
# pdf("tauhat1_ama_hist.pdf")
# pardef = par(mar = c(5, 4, 4, 2) + 0.5, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
hist(tau1_hat_ama, xlab = "estimated CATE", main = "")
# dev.off()

# Omnibus tests for heterogeneity
# Run best linear predictor analysis
stargazer(test_calibration(cf1_ama), dep.var.caption = "CATE")

## Amazon Sales Effect using Five Miles Data
set.seed(1)

ama_cf_d2 <- cf_d2 %>% 
  filter(domain_name=="amazon.com") %>% 
  select(-domain_name)

W2_ama <- ama_cf_d2$treatment 
Y2_ama <- ama_cf_d2$prod_totprice %>% log(.) %>% {.+1}

d2_ama <- ama_cf_d2 %>% 
  select(-pages_viewed, -duration, -prod_qty, 
         -prod_totprice, -household_income, 
         -treatment)

d2_ama_exp <-model.matrix(~.+0, d2_ama)

X2_ama <- cbind(ama_cf_d2[,-c(4, 16, which(colnames(ama_cf_d2) %in% colnames(d2_ama)))], d2_ama_exp)

Y2_f_ama <- regression_forest(X2_ama, Y2_ama)
Y2_hat_ama <- predict(Y2_f_ama)$predictions

W2_f_ama <- regression_forest(X2_ama, W2_ama)
W2_hat_ama <- predict(W2_f_ama)$predictions

cf2_raw_ama <- causal_forest(X2_ama, Y2_ama, W2_ama,
                             Y.hat = Y2_hat_ama, W.hat = W2_hat_ama)

varimp2_ama <- variable_importance(cf2_raw_ama)
selected2_idx_ama <- which(varimp2_ama > mean(varimp2_ama))

cf2_ama <- causal_forest(X2_ama[,selected2_idx_ama], Y2_ama, W2_ama,
                         Y.hat = Y2_hat_ama, W.hat = W2_hat_ama,
                         tune.parameters = "all")

tau2_hat_ama <- predict(cf2_ama)$predictions


# Estimate ATE
alpha <- 1-0.95
average_treatment_effect(cf2_ama, target.sample = "all") %>%
  {.["estimate"] + c(-1,0,1)*qnorm(1-alpha/2)*.["std.err"]} %>% 
  as.table() %>% t() %>% 
  set_colnames(c("2.5% ", "Estimate", "97.5%")) %>% 
  xtable(caption = "95% CI for the ATE")

pdf("tauhat2_ama_hist.pdf")
pardef = par(mar = c(5, 4, 4, 2) + 0.5, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
hist(tau2_hat_ama, xlab = "estimated CATE", main = "")
dev.off()


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
Y3_ama <- ama_cf_d3$PagesPerDollar %>% log(.) %>% {.+1} 

d3_ama <- ama_cf_d3 %>% 
  select(-duration, -prod_qty, 
         -PagesPerDollar,
         -household_income, 
         -treatment)

d3_ama_exp <-model.matrix(~.+0, d3_ama)

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
alpha <- 1-0.99
average_treatment_effect(cf3_ama, target.sample = "all")%>%
  {.["estimate"] + c(-1,0,1)*qnorm(1-alpha/2)*.["std.err"]} %>% 
  as.table() %>% t() %>% 
  set_colnames(c("0.5% ", "Estimate", "99.5%")) %>% 
  xtable(caption = "99% CI for the ATE")

alpha <- 1-0.95
average_treatment_effect(cf3_ama, target.sample = "all")%>%
  {.["estimate"] + c(-1,0,1)*qnorm(1-alpha/2)*.["std.err"]} %>% 
  as.table() %>% t() %>% 
  set_colnames(c("2.5% ", "Estimate", "97.5%")) %>% 
  xtable(caption = "95% CI for the ATE")


# CATE Histogram
pdf("tauhat3_ama_hist.pdf")
pardef = par(mar = c(5, 4, 4, 2) + 0.5, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
hist(tau3_hat_ama, xlab = "estimated CATE", main = "")
dev.off()

# Omnibus tests for heterogeneity
# Run best linear predictor analysis
stargazer(test_calibration(cf3_ama), dep.var.caption = "CATE")


### Best Buy Pages Per Dollar using Zero Mile Data
set.seed(1)

bb_cf_d3 <- cf_d3 %>% 
  filter(domain_name=="bestbuy.com") %>% 
  select(-domain_name)

W3_bb <- bb_cf_d3$treatment 
Y3_bb <- bb_cf_d3$PagesPerDollar %>% log(.) %>% {.+1} 

d3_bb <- bb_cf_d3 %>% 
  select(-duration, -prod_qty, 
         -PagesPerDollar,
        -household_income, 
        -treatment)

d3_bb_exp <-model.matrix(~.+0, d3_bb)

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

# Estimate ATE
alpha <- 1-0.95
average_treatment_effect(cf3_bb, target.sample = "all") %>%
  {.["estimate"] + c(-1,0,1)*qnorm(1-alpha/2)*.["std.err"]} %>% 
  as.table() %>% t() %>% 
  set_colnames(c("2.5% ", "Estimate", "97.5%")) %>% 
  xtable(caption = "95% CI for the ATE")

# CATE Histogram
pdf("tauhat3_bb_hist.pdf")
pardef = par(mar = c(5, 4, 4, 2) + 0.5, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
hist(tau3_hat_bb, xlab = "estimated CATE", main = "")
dev.off()


### Amazon Minutes Per Dollar using Zero Mile Data

cf_d5 <- concat_data1 %>% 
  mutate(treatment=ifelse(CCStorePresent==1 & AfterStoreClosing==1, 1, 0)) %>% 
  select(-Store_Close_Status, -domain_id, -ref_domain_name, -PagesPerDollar,
         -event_date,-event_time,-tran_flg,-prod_name, -MonthYear, -prod_totprice,
         -CCStorePresent,- AfterStoreClosing,-BB_Store_Status, -duration,
         -site_session_id, -prod_category_id, -basket_tot, -machine_id, -Zip_Code)

set.seed(1)

ama_cf_d5 <- cf_d5 %>% 
  filter(domain_name=="amazon.com") %>% 
  select(-domain_name)

W5_ama <- ama_cf_d5$treatment 
Y5_ama <- ama_cf_d5$MinsPerDollar %>% log(.) %>% {.+1} 

d5_ama <- ama_cf_d5 %>% 
  select(-pages_viewed, 
         -prod_qty, 
         -MinsPerDollar,
         -household_income, 
          -treatment)

d5_ama_exp <-model.matrix(~.+0, d5_ama)

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
alpha <- 1-0.90
average_treatment_effect(cf5_ama, target.sample = "all") %>%
  {.["estimate"] + c(-1,0,1)*qnorm(1-alpha/2)*.["std.err"]} %>% 
  as.table() %>% t() %>% 
  set_colnames(c("5% ", "Estimate", "95%")) %>% 
  xtable(caption = "90% CI for the ATE")

alpha <- 1-0.95
average_treatment_effect(cf5_ama, target.sample = "all") %>%
  {.["estimate"] + c(-1,0,1)*qnorm(1-alpha/2)*.["std.err"]} %>% 
  as.table() %>% t() %>% 
  set_colnames(c("2.5% ", "Estimate", "97.5%")) %>% 
  xtable(caption = "95% CI for the ATE")

alpha <- 1-0.99
average_treatment_effect(cf5_ama, target.sample = "all") %>%
  {.["estimate"] + c(-1,0,1)*qnorm(1-alpha/2)*.["std.err"]} %>% 
  as.table() %>% t() %>% 
  set_colnames(c("0.5% ", "Estimate", "99.5%")) %>% 
  xtable(caption = "99% CI for the ATE")

# CATE Histogram
pdf("tauhat5_ama_hist.pdf")
pardef = par(mar = c(5, 4, 4, 2) + 0.5, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
hist(tau5_hat_ama, xlab = "estimated CATE", main = "")
dev.off()

# Omnibus tests for heterogeneity
# Run best linear predictor analysis
stargazer(test_calibration(cf5_ama), dep.var.caption = "CATE")

### Best Buy Minutes Per Dollar using Zero Mile Data

set.seed(1)

bb_cf_d5 <- cf_d5 %>% 
  filter(domain_name=="bestbuy.com") %>% 
  select(-domain_name)

W5_bb <- bb_cf_d5$treatment 
Y5_bb <- bb_cf_d5$MinsPerDollar 

d5_bb <- bb_cf_d5 %>% 
  select(-pages_viewed, -prod_qty, 
         -MinsPerDollar,
         -household_income, 
         -treatment)

d5_bb_exp <-model.matrix(~.+0, d5_bb)


# bb_cf_d1[,-"prod_totprice"]
X5_bb <- cbind(bb_cf_d5[,-c(14, 15, which(colnames(bb_cf_d5) %in% colnames(d5_bb)))], d5_bb_exp)

Y5_f_bb <- regression_forest(X5_bb, Y5_bb)
Y5_hat_bb <- predict(Y5_f_bb)$predictions

W5_f_bb <- regression_forest(X5_bb, W5_bb)
W5_hat_bb <- predict(W5_f_bb)$predictions

cf5_raw_bb <- causal_forest(X5_bb, Y5_bb, W5_bb,
                            Y.hat = Y5_hat_bb, W.hat = W5_hat_bb)

varimp5_bb <- variable_importance(cf5_raw_bb)
selected5_idx_bb <- which(varimp5_bb > mean(varimp5_bb))

cf5_bb <- causal_forest(X5_bb[,selected5_idx_bb], Y5_bb, W5_bb,
                        Y.hat = Y5_hat_bb, W.hat = W5_hat_bb,
                        tune.parameters = "all")

tau5_hat_bb <- predict(cf5_bb)$predictions

# Estimate ATE
alpha <- 1-0.90
average_treatment_effect(cf5_bb, target.sample = "all") %>%
  {.["estimate"] + c(-1,0,1)*qnorm(1-alpha/2)*.["std.err"]} %>% 
  as.table() %>% t() %>% 
  set_colnames(c("5% ", "Estimate", "95%")) %>% 
  xtable(caption = "90% CI for the ATE")

average_treatment_effect(cf5_bb, target.sample = "treated") %>%
  {.["estimate"] + c(-1,0,1)*qnorm(1-alpha/2)*.["std.err"]} %>% 
  as.table() %>% t() %>% 
  set_colnames(c("5% ", "Estimate", "95%")) %>% 
  xtable(caption = "90% CI for the ATT")

alpha <- 1-0.95
average_treatment_effect(cf5_bb, target.sample = "all") %>%
  {.["estimate"] + c(-1,0,1)*qnorm(1-alpha/2)*.["std.err"]} %>% 
  as.table() %>% t() %>% 
  set_colnames(c("2.5% ", "Estimate", "97.5%")) %>% 
  xtable(caption = "95% CI for the ATE")

average_treatment_effect(cf5_bb, target.sample = "treated") %>%
  {.["estimate"] + c(-1,0,1)*qnorm(1-alpha/2)*.["std.err"]} %>% 
  as.table() %>% t() %>% 
  set_colnames(c("2.5% ", "Estimate", "97.5%")) %>% 
  xtable(caption = "95% CI for the ATE")

alpha <- 1-0.99
average_treatment_effect(cf6_ama, target.sample = "all") %>%
  {.["estimate"] + c(-1,0,1)*qnorm(1-alpha/2)*.["std.err"]} %>% 
  as.table() %>% t() %>% 
  set_colnames(c("0.5% ", "Estimate", "99.5%")) %>% 
  xtable(caption = "99% CI for the ATE")

average_treatment_effect(cf6_ama, target.sample = "treated") %>%
  {.["estimate"] + c(-1,0,1)*qnorm(1-alpha/2)*.["std.err"]} %>% 
  as.table() %>% t() %>% 
  set_colnames(c("0.5% ", "Estimate", "99.5%")) %>% 
  xtable(caption = "99% CI for the ATE")

pdf("tauhat5_bb_hist.pdf")
pardef = par(mar = c(5, 4, 4, 2) + 0.5, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
hist(tau5_hat_bb, xlab = "estimated CATE", main = "")
dev.off()


ATE5_bb <- average_treatment_effect(cf5_bb, target.sample = "treated")
paste("95% CI for the ATE:", round(ATE5_bb[1], 3),
      "+/-", round(qnorm(0.975) * ATE5_bb[2], 3))

#
# Omnibus tests for heterogeneity
#

# Run best linear predictor analysis
test_calibration(cf5_bb)


