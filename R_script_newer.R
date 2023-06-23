#Aurthor: Douye Victor Orumieyefa 
#Title: Independent Quantium Virtual Internship Project - Retail Strategy and Analytics

#installing Packages
install.packages("tidyverse")
install.packages("data.table")
install.packages("ggplot2")
install.packages("ggmosaic")
install.packages("rear")

#loading packages
library(tidyverse)
library(data.table)
library(ggplot2)
library(ggmosaic)
library(readr)

#Importing Data
Transaction_data <- read.csv("QVI_transaction_data.csv")
Purchase_behaviour <- read.csv("QVI_purchase_behaviour.csv")

#Exploratory Data Analyses

#Transaction_Data
     
#str to look at the format for each of the column 
str(Transaction_data)
#head() for first 6 rows
head(Transaction_data)
#show() for wholistic view of transactional data
show(Transaction_data)
#Converting DATE column to date format
Transaction_data$DATE <- as.Date(Transaction_data$DATE, origin = "1899-12-30")
show(Transaction_data)
#Creating datables from the data frames
Transaction <- data.table(Transaction_data)
Purchase_bhvr <- data.table(Purchase_behaviour)
show(Transaction)
#Check that we're looking the right products
summary(Transaction$PROD_NAME)
#Checking for incorrect entries in PROD_NAME
ProductWords <- data.table(unlist(strsplit(unique(Transaction[, PROD_NAME]), " ")))
setnames(ProductWords, 'words')
#Removing digits and special characters
ProductWords <- ProductWords[!grepl("[^[:alpha:]]", words)]
#Removing salsa products
Transaction[, SALSA := grepl("salsa", tolower(PROD_NAME))]
Transaction <- Transaction[SALSA == FALSE, ][, SALSA := NULL]
summary(Transaction)
#filter the dataset to find the outlier in PROD_QTY
outliers <- Transaction %>% filter(PROD_QTY == 200)
#filter the data frame to exclude the transactions with the specified loyalty number
FilteredTransaction <- Transaction %>% filter(LYLTY_CARD_NBR != 2373)
#Rexamine The Transaction data
head(FilteredTransaction)
#Grouping the transaction by date
transactionCounts <- Transaction %>% group_by(DATE) %>% summarize(count = n())
head(transactionCounts)
summary(transactionCounts)
count(transactionCounts)
#Creating a sequence of dates from 1 Jul 2018 to 30 Jun 2019
dateSeq <- seq(as.Date("2018-07-01"), as.Date("2019-06-30"), by = "day")
#converting the transactionCounts data frame to a tibble and format the DATE column as date object
transactionCounts <- as_tibble(transactionCounts)
transactionCounts$DATE <- as.Date(transactionCounts$DATE)
#Joining the date sequence onto the transactionCounts data frame to fill in the missing days
transactionCounts <- left_join(data.frame(DATE = dateSeq), transactionCounts, by = "DATE")
#Replace any missing transaction counts with 0
transactionCounts$count[is.na(transactionCounts$count)] <- 0
head(transactionCounts)
#Setting plot themes to format graphs
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))

#Plotting transactions over time
ggplot(transactionCounts, aes(x = DATE, y = count)) + 
  geom_line()+
  labs(x= "Day", y = "Number of transactions", title = " Transactions over time") +
  scale_x_date(breaks = "1 month")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
#filter data to include only december
dec_data <- transactionCounts %>%
  filter(month(DATE) == 12)
#plot individual days in december
ggplot(dec_data, aes(x = DATE, y = count)) +
  geom_line() +
  labs(x = "Day", y = " Number of transactions", title = "Transactions in December")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
#Creating Pack size by collecting digits that are in PROD_NAME
Transaction[, PACK_SIZE := parse_number(PROD_NAME)]
#creates a summary of the number of transactions for each pack size in the transactionData data frame and orders the results by pack size
Transaction[, .N, PACK_SIZE][order(PACK_SIZE)]
# creating an histogram of transaction counts by pack size
ggplot(Transaction, aes(x = PACK_SIZE)) +
  geom_histogram(binwidth = 10, fill = "yellow", color = "blue") +
  labs(x = "Pack Size", y = " Number of Transactions",
  title = "Histogram of Transactions by Pack Size")
# Creating a column for brand of the product, by extracting it from the product name
library(stringr)
#Define a function to extract the brand from the product name
get_brand <- function(PROD_NAME) {
  brand <- str_extract(PROD_NAME, "^[a-zA-Z]+")
  return(brand)
}
# applying the function to the product names and creating a new column with the results (brands) 
Transaction$BRAND <- sapply(Transaction$PROD_NAME, get_brand)
show(Transaction)
Transaction <- Transaction[, -10]
show(Transaction)
Transaction$BRAND <- str_extract(Transaction$PROD_NAME, "\\w+")
print(Transaction)
print(Transaction$BRAND)
head(Transaction)
#replacing RED with RRD brand names in the brand column
Transaction[BRAND == "RED", BRAND :="RRD"]

# Examining Customer Data
head(Purchase_bhvr)
summary(Purchase_bhvr)
show(Purchase_bhvr)

#Merging Transaction Data to customer data
Mdata <- merge(Transaction, Purchase_bhvr, all.x = TRUE)
head(Mdata)
show(Mdata)
# checking for missing data
is.na(Mdata$BRAND)
# saving as csv file
write.csv(Mdata, "Quantium Task 1", row.names = FALSE)

#Analysis 
#Computing the summary statistics by LIFESTAGE and PREMIUM_CUSTOMER
Sales_summary <- aggregate(TOT_SALES ~ LIFESTAGE + PREMIUM_CUSTOMER, data = Mdata, FUN = sum)
show(Sales_summary)
# Creating a plot for the sales by life_stage and premium_ customer
ggplot(Sales_summary, aes(x = LIFESTAGE, y = TOT_SALES, fill = PREMIUM_CUSTOMER)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "LIFESTAGE", y = "TOT_SALES", title = "Sales by Life stage and Premium customer", fill = "PREMIUM_CUSTOMER")
#calculating the number of customers by lifestage and premium customers
customers_summary <- aggregate(TXN_ID ~ LIFESTAGE + PREMIUM_CUSTOMER, data = Mdata, FUN = length)
#Rename the TXN_ID column to customers
names(customers_summary)[names(customers_summary) == "TXN_ID"] <- "CUSTOMERS"
view(customers_summary)
# creating a plot
ggplot(customers_summary, aes(x = LIFESTAGE, y = CUSTOMERS, fill = PREMIUM_CUSTOMER)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "LIFESTAGE", y = "CUSTOMERS", title = "Customers by Life Satge and Premium Customer", fill = "PREMIUM_CUSTOMER")
view(Mdata)
# Average number of units per customer by LIFESTAGE and PREMIUM_CUSTOMER
avg_units_by_customer <- Mdata %>% 
  left_join(customers_summary, by = c("LIFESTAGE", "PREMIUM_CUSTOMER")) %>%
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
  summarize(avg_units_sold = mean(PROD_QTY))
view(avg_units_by_customer)
ggplot(avg_units_by_customer, aes(x = LIFESTAGE, y = avg_units_sold, fill = PREMIUM_CUSTOMER))+
  geom_col(position = "dodge")+
  labs (x = "Lifestage", y = "avg_units_sold", fill = "Premium Customers", title = "Average number of units sold by lifestage and premium customer")
# Subset the data for the two groups: Budget vs. Midage
group1 <- subset(Mdata, PREMIUM_CUSTOMER %in% c("Mainstream"))
group2 <- subset(Mdata, PREMIUM_CUSTOMER %in% c("Premium"))

#Perform the Independent t-test between the two groups
ttest_budget_midage <- t.test(group1$PROD_QTY, group2$PROD_QTY)
#Perform an independent t-test between mainstream vs premium and budget midage and
#young singles and couples
priceperunit <- Mdata[, price := TOT_SALES/PROD_QTY]
t.test(Mdata[LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES")
             & PREMIUM_CUSTOMER == "Mainstream", price]
               , Mdata[LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES")
             & PREMIUM_CUSTOMER != "Mainstream", price]
               , alternative = "greater")
#The t-test results in a p-value < 2.2e-16, i.e. the unit price for mainstream, young and mid-age singles and 
#couples are significantly higher than that of budget or premium, young and midage singles and couples

#Deep dive into customer segments for insights
#Deep dive into mainstream, young singles/couples
segment1 <- Mdata[LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER ==
                   "Mainstream",]
other <- Mdata[! (LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER ==
                 "Mainstream"),]

#Brand Affinity Compared to the rest of the population
quantity_segment1 <- segment1[, sum (PROD_QTY)]
quantity_other <- other[, sum(PROD_QTY)]
quantity_segment1_by_brand <- segment1[, .(targetSegment =
                                             sum(PROD_QTY)/quantity_segment1), by = BRAND]
quantity_other_by_brand <- other[, . (other = sum(PROD_QTY)/quantity_other), by
                                 = BRAND]
brand_proportions <- merge(quantity_segment1_by_brand,
                           quantity_other_by_brand)[, affinityToBrand := targetSegment/other]
brand_proportions[order(-affinityToBrand)]

# Mainstream young singles/couples are 23% more likely to purchase Tyrrells chips compared to the
# rest of the population
# Mainstream young singles/couples are 56% less likely to purchase Burger Rings compared to the rest
# of the population



