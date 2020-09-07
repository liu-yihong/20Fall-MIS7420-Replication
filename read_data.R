# https://cran.r-project.org/web/packages/plm/vignettes/plmPackage.html
# https://www.econometrics-with-r.org/8-3-interactions-between-independent-variables.html
# https://stats.stackexchange.com/questions/47265/how-to-investigate-a-3-way-interaction

# load library
library('haven')
library('Hmisc')

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


# data columns
# for bb_zipcode
colnames(bb_zipcode)
# "BB_Store_Status" -> 1 means bb store exists in that zip code
# "Zip_Code" -> corresponding zip code of bb store

# for sales_allother_zipcode, sales_cc_0mile, sales_cc_5miles
# sales_cc_0mile, sales_cc_5miles have the same columns as sales_allother_zipcode
colnames(sales_allother_zipcode)
# Store_Close_Status: 1 -> CC Closed, 0 -> Never had CC in 5-miles radius
# Zip_Code: zip code of user for this row
# machine_id: identifier for user's machine
# site_session_id: identifier for session
# domain_id: unknown
# ref_domain_name: Domain that refer this user
# pages_viewed: original breadth of searches
# duration: original depth of searches, minutes
# event_date: date of activity
# event_time: time of activity
# tran_flg: transaction flag (purchase or not), all 3 data are 1 on this column
# prod_category_id: product category id  for the product viewed
# prod_name: Text Description for Product Name
# prod_qty: Number of purchased item
# prod_totprice: Total price of purchased item
# basket_tot: Basket total price of this session
# hoh_most_education: Most education for the head of household
# census_region: Census region
# household_size: Household size
# hoh_oldest_age: Oldest Age for the head of household
# household_income: Household income
# children: Has children or not 1-Yes, 0-No
# racial_background: racial background
# connection_speed: Internet connection speed
# country_of_origin: unknown
# domain_name: visited website name

# Data Correction
sales_allother_zipcode$Store_Close_Status <- 0 # NaN means no CC in 5-miles radius, we change NaN to 0

# Data summary
summary(bb_zipcode)
summary(sales_allother_zipcode)
summary(sales_cc_0mile)
summary(sales_cc_5miles)

Hmisc::describe(bb_zipcode)
Hmisc::describe(sales_allother_zipcode)
Hmisc::describe(sales_cc_0mile)
Hmisc::describe(sales_cc_5miles)
