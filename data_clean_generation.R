# load library
library('haven')
library('sqldf')
library('zoo')
library('plm')

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
# Question: what does Store Close Status mean?
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
ref_domain_to_consider1 <- c("", "GOOGLE.COM", "YAHOO.COM", "google.com", "yahoo.com",
                            "MSN.COM", "msn.com", "aol.com", "AOL.COM", "LIVE.COM", "live.com",
                            "MYWEBSEARCH.COM", "ASK.COM", "MYWAY.COM", "mywebsearch.com",
                            "ask.com", "YAHOO.NET", "BIZRATE.COM", "bizrate.com")

ref_domain_to_consider2 <- c("", "GOOGLE.COM", "YAHOO.COM", "google.com", "yahoo.com", "bing.com")

# Then we filter data by refer domain name
sales_allother_zipcode <- sales_allother_zipcode[(sales_allother_zipcode$ref_domain_name %in% ref_domain_to_consider2),]
sales_cc_0mile <- sales_cc_0mile[(sales_cc_0mile$ref_domain_name %in% ref_domain_to_consider2),]
sales_cc_5miles <- sales_cc_5miles[(sales_cc_5miles$ref_domain_name %in% ref_domain_to_consider2),]

# Filter Target Domain Name
groupby_target_domain_result <- aggregate(machine_id ~ domain_name, rbind(sales_allother_zipcode, sales_cc_0mile, sales_cc_5miles), FUN = "length")
groupby_target_domain_result <- groupby_target_domain_result[order(-groupby_target_domain_result$machine_id), ]

# Here's the top 5 online retail vendors
# Questions: what should we use? Table 1 or here? 
# amazon.com      11644
# staples.com       6649
# officedepot.com       3641
# quillcorp.com       3036
# columbiahouse.com       2237
five_target_domain_to_consider <- c("amazon.com", "staples.com", "dell.com", "walmart.com", "bestbuy.com")
two_target_domain_to_consider <- c("amazon.com","bestbuy.com")

# we can choose what filter to apply
sales_allother_zipcode <- sales_allother_zipcode[sales_allother_zipcode$domain_name %in% five_target_domain_to_consider,]
sales_cc_0mile <- sales_cc_0mile[sales_cc_0mile$domain_name %in% five_target_domain_to_consider,]
sales_cc_5miles <- sales_cc_5miles[sales_cc_5miles$domain_name %in% five_target_domain_to_consider,]

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

sales_allother_zipcode$BB_Store_Status <- na.fill(sales_allother_zipcode$BB_Store_Status, 0)
sales_cc_0mile$BB_Store_Status <- na.fill(sales_cc_0mile$BB_Store_Status, 0)
sales_cc_5miles$BB_Store_Status <- na.fill(sales_cc_5miles$BB_Store_Status, 0)

# Product Categories 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
sort(unique(rbind(sales_allother_zipcode, sales_cc_0mile, sales_cc_5miles)$prod_category_id))
category_to_consider <- c(22, 23, 24, 25, 26, 27, 29, 31, 32, 33, 34, 35, 36, 37, 38)

sales_allother_zipcode <- sales_allother_zipcode[sales_allother_zipcode$prod_category_id %in% category_to_consider,]
sales_cc_0mile <- sales_cc_0mile[sales_cc_0mile$prod_category_id %in% category_to_consider,]
sales_cc_5miles <- sales_cc_5miles[sales_cc_5miles$prod_category_id %in% category_to_consider,]

# Aggregate Data

# try aggregate
# Question: mean, sum?
# QUestion: which sales to use?
aggregate(cbind(prod_totprice, pages_viewed, duration, Store_Close_Status) ~ Zip_Code + t + domain_name, sales_allother_zipcode, FUN = "sum")

concat_data1 <- rbind(sales_allother_zipcode, sales_cc_0mile)
concat_data2 <- rbind(sales_allother_zipcode, sales_cc_5miles)

temp1 <- sqldf("SELECT Zip_Code, MonthYear, domain_name, count(*) as TotalTransaction, SUM(prod_totprice) AS TotalMonthlySales, SUM(pages_viewed) AS TotalPagesViewed, SUM(duration) AS TotalDuration, AVG(CCStorePresent) AS CCStorePresent, AVG(BB_Store_Status) AS BBStorePresent, AVG(AfterStoreClosing) AS AfterStoreClosing FROM concat_data1 GROUP BY Zip_Code, MonthYear, domain_name")
# temp1$CCStorePresent <- factor(temp1$CCStorePresent)
# temp1$AfterStoreClosing <- factor(temp1$AfterStoreClosing)
# temp1$BBStorePresent <- factor(temp1$BBStorePresent)
summary(temp1)

temp2 <- sqldf("SELECT Zip_Code, MonthYear, domain_name, count(*) as TotalTransaction, SUM(prod_totprice) AS TotalMonthlySales, SUM(pages_viewed) AS TotalPagesViewed, SUM(duration) AS TotalDuration, AVG(CCStorePresent) AS CCStorePresent, AVG(BB_Store_Status) AS BBStorePresent, AVG(AfterStoreClosing) AS AfterStoreClosing FROM concat_data2 GROUP BY Zip_Code, MonthYear, domain_name")
# temp2$CCStorePresent <- factor(temp2$CCStorePresent)
# temp2$AfterStoreClosing <- factor(temp2$AfterStoreClosing)
# temp2$BBStorePresent <- factor(temp2$BBStorePresent)
summary(temp2)


# try regression
# CCStorePresent:AfterStoreClosing + CCStorePresent:AfterStoreClosing:BBStorePresent
reg1 <- plm(log(TotalMonthlySales) ~ CCStorePresent:AfterStoreClosing, data = temp1[temp1$domain_name == "amazon.com",], index = c("Zip_Code", "MonthYear"), model = "within", effect = "twoways")

reg1 <- lm(log(TotalMonthlySales) ~ CCStorePresent:AfterStoreClosing + factor(MonthYear) + factor(Zip_Code), data = temp1[temp1$domain_name == "amazon.com",])

summary(reg1)
summary(fixef(reg1))

# see (https://stackoverflow.com/questions/43636724/r-plm-and-lm-fixed-effects)
# see (https://rstudio-pubs-static.s3.amazonaws.com/372492_3e05f38dd3f248e89cdedd317d603b9a.html)
# for fixed effect
# fixed effect theory background (https://dss.princeton.edu/training/Panel101.pdf)

# see (https://rstudio-pubs-static.s3.amazonaws.com/372492_3e05f38dd3f248e89cdedd317d603b9a.html)
# see (https://stats.stackexchange.com/questions/19271/different-ways-to-write-interaction-terms-in-lm)
# see (https://biologyforfun.wordpress.com/2014/04/08/interpreting-interaction-coefficient-in-r-part1-lm/)
# see (https://stackoverflow.com/questions/52044912/panel-data-fixed-effect-issue-with-r-i-dont-see-my-all-dummy-variables-in-the)
# for two way interactions

# see (https://datascienceplus.com/interpreting-three-way-interactions-in-r/)
# see (https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_interactions.html)
# see (http://pages.stat.wisc.edu/~ane/st572/notes/lec08.pdf)
# for three way interactions

# TEST
N <- 10000
df <- data.frame(a = rnorm(N), b = rnorm(N),
                 region = rep(1:100, each = 100), year = rep(1:100, 100), gender=rep(0:1, 100))
df$y <- 2 * df$a - 1.5 * df$b + rnorm(N) + 5 * df$gender + 6 * df$gender * df$a + 7 * df$gender * df$b + 8 * df$gender * df$a * df$b


model.a <- lm(y ~ a + b + factor(year) + factor(region), data = df)
summary(model.a)

model.b <- plm(y ~ a + b + gender + gender:a + gender:b + gender:a:b, data = df, model = "within", effect = "twoways", index = c("region", "year"))
summary(model.b)

model.c <- plm(y ~ a + b + gender + gender*a*b, data = df, model = "within", effect = "twoways", index = c("region", "year"))
summary(model.c)
