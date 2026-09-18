
# STEP 1: LOADING THE DATA
#----------------------------------------------

library(dplyr)
library(ggplot2)


#set the working directory
setwd("C:/Users/hp OMEN/Desktop/Bsc Computer Science/B104C_B105A/B105A Applied Statistical Modelling/RFiles")

#read csv. The timestamp is regarded as plaintext
#Data source: https://www.kaggle.com/datasets/greatcool/financial-transactions-and-fraud-detection-dataset
sft <- read.csv("synthetic_financial_transactions.csv")
str(sft)

#convert the text in timestamp column to a date format
sft$timestamp <- as.POSIXct(sft$timestamp, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")

str(sft)
head(sft)




# STEP 2: DATA PREPARATION, SAMPLING AND CLEANING
#---------------------------------------------------
#DATA PREPATATION
#Checking for Missing Values
colSums(is.na(sft))
#Extracting the missing value
bad_row <- which(is.na(sft$timestamp))
bad_row

#Checking for Duplicates
sum(duplicated(sft))        #checks for duplicated rows.
sum(duplicated(sft$transaction_id))   #checks for duplicated transaction IDs.

#Checking for the validity of the data. 
sum(sft$amount <= 0)        # skims for negative values
sum(sft$risk_score < 0 | sft$risk_score > 100)      # checks values outside normal range, 0-100
sum(!(sft$is_fraud %in% c(0,1)))        # checks if values are outside range 0 and 1. Invalid fraud flags.
sum(sft$timestamp > Sys.time())       # checks if there are any future dated time-stamps. 


# SAMPLING
#Random sample of 500 transactions.
set.seed(100)
random_indexes <- sample(nrow(sft), 500)   #Picks 500 random numbers from 1 to 10000, without repetition.
sft_sample <- sft[random_indexes, ]    #Pulls out the 500 rows from sft_idx, keeping every column.
sft_sample        # Outputs the 500 rows.
mean(sft_sample $ amount)     #Does a comparison to the full population mean


#Stratified sample of 500 transactions, preserving the fraud rate. 
set.seed(100)     # reset so that it is independent from the section above.
strat_sample <- sft %>%
  group_by(is_fraud) %>%
  slice_sample(prop = 0.05) %>%
  ungroup()       # splits dataset to a legit and fraud row, taking 5% from each and merging them

nrow(strat_sample)      #counts the rows in the combined table
prop.table(table(strat_sample$is_fraud)) * 100    #counts the 0s and 1s, turns them into fractions and into percentages
prop.table(table(sft$is_fraud)) * 100     #same calc as above but for the full dataset




# STEP 3: DESCRIPTIVE STATISTICS
#----------------------------------
#Overall summary of the full dataset
summary(sft$amount)
summary(sft$risk_score)
sapply(sft[c("amount", "risk_score")], sd, na.rm = TRUE) # standard deviation for col amount and risk_score.
nrow(sft) # total row count
n_distinct (sft$user_id) # counts unique values that appear in a column. 


#Total count, Mean, Median, Std, and Total Amount(USD) for the 8 categories in descending order.
sft %>%
  group_by(category) %>%
  summarise(totalCount=n(), mean=mean(amount), median=median(amount),
            std=sd(amount), totalUSD=sum(amount)) %>%
  arrange(desc(totalUSD)) %>%
  mutate(totalUSD = format(round(totalUSD), big.mark = ","))


#Frequency tables to show what kind of transactions make up this dataset overall. 
table(sft$account_type)
table(sft$payment_method)
table(sft$device_type)


#The Sum and Mean of fraud cases for the 8 categories.
sft %>% group_by(category) %>%
  summarise(totalCount = n(), fraudCases = sum(is_fraud), fraudCases_percent = round(mean(is_fraud) * 100, 3)) %>%
  arrange(desc(fraudCases_percent))




#Visual Representation of the Data
#------------------------------
#Histogram for the transaction amounts
ggplot(sft, aes(x = amount)) +
  geom_histogram(bins = 50, fill = "#61B187", color = "black") +
  scale_x_log10() +      #Data spans across 3 orders of magnitude; most between 0-100, few between 500-1000, 3 above 1000
  labs(title = "Distribution of the Transaction Amounts", x = "Amount (USD)", y = "Count")

#Box-plot for the transaction amount by category
ggplot(sft, aes(x = reorder(category, amount, median), y = amount)) +   #sorts based on median amount for easier manipulation. 
  geom_boxplot(fill = "#77C79D") +
  scale_y_log10() +
  labs(title = "Transaction Amount by Category", x = "Categories", y = "Amount(USD) using Log Scale")




# STEP 4: HYPOTHESIS TESTS
#----------------------------------
#H1- Does Risk_score differ between fraudulent and legitimate transactions ? (t-test)

# Checking Assumptions
RiskScore_fraud <- sft$risk_score[sft$is_fraud == 1]
RiskScore_legit <- sft$risk_score[sft$is_fraud == 0]

# Q-Q plot: Risk score of legitimate(normally distributed) against fraudulent(not normally distributed)
par(mfrow = c(1, 2))
qqnorm(RiskScore_fraud, main = "QQ Plot: Risk Score (Fraudulent)"); qqline(RiskScore_fraud, col = "red")
qqnorm(RiskScore_legit, main = "QQ Plot: Risk Score (Legitimate)"); qqline(RiskScore_legit, col = "red")

# Shapiro-Wilk Test on the fraudulent and legitimate risk scores
set.seed(0)
shapiro.test(RiskScore_fraud)
shapiro.test(sample(RiskScore_legit, 5000)) # specified sample size since there are 9874 data points


# Hypothesis Test
Hypothesis_1 <- t.test(RiskScore_fraud, RiskScore_legit, alternative = "two.sided", var.equal = FALSE)
Hypothesis_1



#H2- Does transaction amount differ across spending categories ? (One-Way ANOVA)
# Check Assumptions
bartlett.test(amount ~ factor(category), data = sft)    #checks whether amount has equal variance across all eight catgories

aov_h2 <- aov(amount ~ category, data = sft)
par(mfrow = c(2, 2))
plot(aov_h2)
par(mfrow = c(1, 1))

set.seed(0)
shapiro.test(sample(residuals(aov_h2), 5000))

# Hypothesis test
summary(aov_h2)

kruskal.test(amount ~ category, data = sft)

