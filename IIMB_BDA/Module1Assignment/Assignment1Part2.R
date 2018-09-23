library(rpivotTable)
library(tidyverse)
library(reshape2)
library(ggplot2)
# Import the xls file
df_hospital<-readxl::read_xls("/Users/gopuprakash/Documents/IIMB_BDA/CaseStudyModule1/TwoDaysOfHospData.xls", sheet = "FinalBill", range = "A1:T105",col_names = TRUE)

#Summary
summary(df_hospital)
str(df_hospital)

#Number formatting
options(scipen = 999)


# Change Column Names for easier processing

colnames(df_hospital) <- c("PatientType", "Category","PayType","PatientID","SponsorID","DoctorID","DepartmentID",
                           "BillID","FinalBillDate","RegistrationDate","DischargeDate","User","Pharma","Consumables",
                           "Investigations","Others","BilledAmount","ReceivedAmt","AmountDue","PackageID")

# Convert to local dataframe

df_hosp = tbl_df(df_hospital)
df_hosp

#Question 2
# In the Indian context, several hospitalizations with private hospitals are self-funded. One way to “reverse engineer” 
# a dataset that has been sufficiently anonymized is to guess obvious categories from frequencies.
# a. Let’s begin by analyzing columns containing categorical variables, such as SponsorID. Constructing a pivot table will 
# help you. Perform this task on a spreadsheet as well as in R, using an appropriate cross-tabulation function.
# b. SPON1 has the highest frequency, with over a third of the data. Which is the second highest? 
#   Discuss among yourselves as to what this source could be.


glimpse(df_hospital)

rpivotTable(data = df_hospital, rows = c("SponsorID"),cols="Category", vals = "Freq",
            aggregatorName = "Count", rendererName = "Table")

(hospital_melt <- melt(df_hosp, id=c("SponsorID"), measure = "Category", value.name = "Category"))
(hospital_pivot <- dcast(hospital_melt, SponsorID~Category,value.var = "Category", margins=TRUE, length))

hospital_pivot %>% arrange(desc(`(all)`))


# Answer: After sorting the data in the pivot table, it is observed that the second highest is SPON5 with with it 
#having source as SPONSOR


# Question 4
# Analyze the Department versus PayType scenario using a pivot table in R.
# a. What’s the overall proportion of Cash versus Credit in the data? 
#   What is this ratio in the specific case of Department 12?
# b. Do you spot any trend in the top 5 departments by count?

(hospital_melt <- melt(df_hosp, id=c("DepartmentID"), measure = "PayType", value.name = "PayType"))
(hospital_pivot <- dcast(hospital_melt, DepartmentID~PayType,value.var = "PayType", margins=TRUE, length) 
  %>% arrange(desc(`(all)`)))

(hospital_pivot <- hospital_pivot %>% mutate(CashVSCredit = CASH/CREDIT))

#Answers:
# a. What’s the overall proportion of Cash versus Credit in the data? 
hospital_pivot %>% filter(DepartmentID == '(all)')
#   What is this ratio in the specific case of Department 12?
hospital_pivot %>% filter(DepartmentID == 'DEP12')
# b. Do you spot any trend in the top 5 departments by count?
(top5 <- hospital_pivot %>% slice(2:6))

ggplot(data = top5, aes(x = CASH, y = CREDIT)) + geom_count()  + scale_fill_distiller(palette = "Blues")

# Observation: Credit is used much more than Cash among the top 5

# 7. Create a new column labelled AmtCategory, based on the billed amount value. 
# If this value < 1000, then AmtCategory = “Silver”, else if the value < 100000, then AmtCategory = “Silver”, 
# else AmtCategory = “Platinum”.
# a. Obtain a pivot table of AmtCategory versus PayType.
# b. Which category has the proportionately highest cash to credit ratio?

# select the necessary variables
df_hosp %>% select (PatientID, BilledAmount)
# Use the mutate function in dplyr
df_hosp <- df_hosp %>% mutate(AmtCategory = if_else(BilledAmount < 1000,"Silver", if_else(BilledAmount < 100000,"Gold","Platinum")))

#Pivot Table
(hosp_melt <- melt(df_hosp, id=c("AmtCategory"), measure = "PayType", value.name = "PayType"))
(hosp_pivot <- dcast(hosp_melt, AmtCategory~PayType,value.var = "PayType", margins=TRUE, length) 
  %>% arrange(desc(`(all)`)))
(hosp_pivot <- hosp_pivot %>% mutate(CashVSCredit = CASH/CREDIT))
# Gold has the highest Cash Vs Credit Ratio

hosp_melt

# Question 8: 
# Let’s visually examine overall differences in Cash vs Credit patients by Category.
# a. Construct a bar chart from the counts in a pivot table. In Excel, you could select a column chart, 
# or just go with a recommended chart.
# b. What can you say about GENERAL category patients?

(hosp_melt <- melt(df_hosp, id=c("Category"), measure = "PayType", value.name = "PayType"))
(hosp_p_catVpaytype <- dcast(hosp_melt, Category~PayType, value.var = "PayType", margins=FALSE, length))

ggplot(data=df_hosp, aes(x=Category, fill=PayType)) +
  geom_bar(stat="count", position=position_dodge2(), colour="black" ) +                       
  scale_fill_hue(name="Payment Type") +      # Set legend title
  xlab("Category") + ylab("Patient Count") + # Set axis labels
  ggtitle("Differences: Cash Vs Credit by Category") +     # Set title
  theme_bw()

# General Category Patients use cash overwhelmingly 

#Question 9
# For our numerical variable, we select the BilledAmount column. As you know, there are two major ways to characterize 
# the data: location and dispersion.
# a. Use spreadsheet functions to calculate location measures such as mean, median and mode. 
# Why is mode meaningless in this case?
#   b. Use spreadsheet functions to calculate dispersion measures such as the range (= max - min) and standard deviation 
# (you may use STDEV.S)
# c. Carry out all of these operations in R

# Function to get mode as R does not have a standard in-built function to calculate mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Summary List Function to  get all Location and Dispersion measures
summary.list = function(x)list(
  Mean = mean(x, na.rm=TRUE),
  Median = median(x, na.rm=TRUE),
  Mode = getmode(x),
  Max.Min = range(x, na.rm=TRUE),
  Range = max(df_hosp$BilledAmount, na.rm=TRUE) - min(df_hosp$BilledAmount, na.rm=TRUE),
  Variance = var(x, na.rm=TRUE),
  Std.Dev = sd(x, na.rm=TRUE)
)

summary.list(df_hosp$BilledAmount)

# Question: 11
# Let’s qualify the distribution of BilledAmount numbers using their mean (mu) and standard deviation (sigma)
# a. What percent of values lie between mu – sigma & mu + sigma? 
# b. According to Chebyshev, at least 75% of BilledAmount data will lie between mu – 2 sigma & mu + 2 sigma. 
# Carry out a count (you could use COUNTIF) to find out how many values are actually contained between those limits.

mu <- mean(df_hosp$BilledAmount, na.rm=TRUE)
sigma <- sd(df_hosp$BilledAmount, na.rm=TRUE)

#total rows
totalcount = nrow(df_hosp)
muminussigma <- mu - sigma
muplussigma <- mu + sigma

muminus2sigma <- mu - 2*sigma
muplus2sigma <- mu + 2*sigma

#Question 11a
#% of values which lie between mu – sigma & mu + sigma: 
sigma1 <- (sum(df_hosp$BilledAmount > muminussigma & df_hosp$BilledAmount < muplussigma))/totalcount*100
sigma1
#Sigma1 is greater than 0%

#Question 11b
#% of values which lie between mu – 2*sigma & mu + 2*sigma: 
countsigma2 <- sum(df_hosp$BilledAmount > muminus2sigma & df_hosp$BilledAmount < muplus2sigma)
# how many values are actually contained between mu – 2*sigma & mu + 2*sigma
countsigma2

sigma2 <- countsigma2/totalcount*100
sigma2
#Sigma1 is greater than 75%
#Both the tests validate Chebichev's theorm

# Question 12
# Examine doctor-wise “revenues” by constructing a pivot table with its rows as DoctorIDs.
# a. For columns, you will need counts of patients attended to by each doctor, the min/max BilledAmount values, average, 
# and standard deviation
# b. Carry out all of these operations in R
# c. Who are the top 5 doctors in terms of billing per patient visit?

detach(package:plyr)
library(dplyr)
# Code to Create the pivot table. with DoctorID as rows and Billed Amount Summaries as columns
pivot <- df_hosp %>% select(DoctorID,BilledAmount) %>% group_by(DoctorID) %>% 
  summarise(Count = n(),
            TotalSales = sum(BilledAmount), 
            Max = max(BilledAmount), 
            Min = min(BilledAmount), 
            Average = mean(BilledAmount),
            StdDev = sd(BilledAmount)) %>% arrange(desc(Average))

pivot

# Top 5 doctors in terms of billing per patient visit are the following
(top_5 <- pivot %>% top_n(5, Average))
top_5

# Question 13
# 
# a. Copy the DischargeDate and BilledAmount columns to a new tab.
# b. Select the columns and insert a time series via a recommended chart.
# c. Examine the peaks. When did they occur?
# d. Carry out all of these operations in R


library(lubridate) # for working with dates
library(ggplot2)  # for creating graphs
library(scales)   # to access breaks/formatting functions
library(gridExtra) # for arranging plots
library(ggthemes)

(timeseries <- df_hosp %>% select(DischargeDate, BilledAmount))
timeseries$DischDate <- as.POSIXct(timeseries$DischargeDate, format='%d/%m/%Y %H:%M')

timeseries

ggplot(timeseries, aes(DischDate, BilledAmount)) +
  geom_point(na.rm=TRUE,  color="blue", size=1) + 
  ggtitle("Bill Amount Vs Discharge Time") +
  xlab("Date") + ylab("Bill Amount") + theme_economist()

# The peaks (Discharges with Highest Bill Amounts) occured on Aug 1st Evening at 16:12 and 17:28


# Question 14
# a. List a set of “bins” in a separate column as shown, and construct a histogram using Analysis Toolpak
# b. Insert a bar chart with the frequency table as shown.
# c. What can you say about the skew of the billed amounts?

# make a histogram out of BilledAmount
histogram <- df_hosp %>% select(BilledAmount)

histogram$BilledAmount

ggplot(data=histogram, aes(histogram$BilledAmount)) + 
  geom_histogram(aes(y = ..count..), binwidth = 50000, 
                 col="black", 
                 fill="goldenrod2", show.legend = TRUE, boundary = 0.5) + 
  labs(title="Billed Amount") +
  labs(x="Amount", y="Count") 

# Observation: The distribution is positively skewed. 

# Question 15
# Question 15a: Figure out the quartiles Q1 = 25th %ile, Q2 = 50th %ile and Q3 = 75th %ile.

set.seed(1)
percentile <- df_hosp %>% select(BilledAmount)

#One way fo getting quartiles
summary(percentile$BilledAmount)

# Another way
quantiles <- quantile(percentile$BilledAmount, c(.25, .50, .75)) 
quantiles
#Question 15c: IQR: 
iqr <- IQR(percentile$BilledAmount)
iqr

#15d At what percentile is a BilledAmount value of Rs.1,00,000? Interpret it in human-understandable terms.

# method to calculate a list of values in Billed Amount with percent rank
 percentrank <- function(x) { 
  var  <- sort(x) 
  p.rank <- 1:length(var)/length(var)*100 
  dd  <- cbind(var,p.rank) 
 } 
#calling percentrabk method and storing the result
pr <- percentrank(percentile$BilledAmount)
pr
# Viewing pr will give the rank of Rs. 100000 to be approximately 74.5%

# 15f Identify an outlier by figuring out billing values that exceed Q3 + 1.5 x IQR.
#How many outliers are there?

q3 <- quantile(percentile$BilledAmount, c(.75)) 
# getting the value
q3 <- unname(q3)
upper_limit = q3 + 1.5*iqr
upper_limit
# values about upper limit

outliers <- percentile  %>% filter(BilledAmount >= upper_limit)
outliers


#Question 16
#  The more the duration of stay, the more the billed amounts. Is this statement true?
#   a. Copy the data in the columns – DurationOfStay followed by BilledAmount – to a new tab.
# b. Assess the validity of the statement using a scatterplot.

# Select just the needed attributes
durationOfStay <- df_hosp %>% select(RegistrationDate,DischargeDate,BilledAmount)

# Convert to Date time
durationOfStay$DischargeDate <- as.POSIXct(durationOfStay$DischargeDate, format='%d/%m/%Y %H:%M')
durationOfStay$RegistrationDate <- as.POSIXct(durationOfStay$RegistrationDate, format='%d/%m/%Y %H:%M')
durationOfStay %>% glimpse()
# Add DurationOfStay
durationOfStay <- 
  durationOfStay %>% mutate(date_diff = as.numeric(round(difftime(DischargeDate, RegistrationDate, units = c("days")))))
durationOfStay %>% glimpse()

options(scipen = 999)
# ScatterPlot between Duration of Stay and Billed Amount
ggplot(durationOfStay, aes(x=date_diff, y=BilledAmount, color = BilledAmount)) +
  geom_point(shape = 16, size = 2, show.legend = TRUE) +
  theme_minimal() + scale_color_gradient(low = "#0091ff", high = "#f0650e")

# Observtion
# The statement "The more the durtation of stay, the more the billed amounts" is false as there is no evidence 
# of a positive correlation										


# Question 21 
# A random variable X is any numerical function that you specify on a sample space. 
# We associate each patient with the average billing amount supplied above. 
# So our variable X takes discrete values 6000, 50000 and 200000 depending on the billing.
# a. Create a new column X in the dataset that maps each patient record to the appropriate value of X.

# Select just the needed attributes
randomVariable <- df_hosp %>% select(PatientID,BilledAmount,Category)
randomVariable <- randomVariable %>% mutate(X = if_else(BilledAmount < 10000, 6000, if_else(BilledAmount < 100000,50000,200000)))
randomVariable

# Question 22


(rv_melt <- melt(randomVariable, id=c("X"), measure = "Category", value.name = "Category"))
(rv_pivot <- dcast(rv_melt, X~Category,value.var = "Category", margins=TRUE, length))

# Calculate Point Probability
rv_pivot <- rv_pivot %>% mutate(Point_prob = `(all)`/104 )
# Calculate Cumulative Probability
rv_pvt_cum <- rv_pivot %>% mutate(cumsum = cumsum(Point_prob) ) %>% slice(1:3)
rv_pvt_cum

#PMF
ggplot(data=rv_pvt_cum, aes(x=X, y=Point_prob)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=round(Point_prob,2)), vjust=-0.3, size=3.5)+
  theme_minimal()
#CDF
ggplot(data=rv_pvt_cum, aes(x=X, y=cumsum, group=1)) +
  geom_line(color="blue") +
  geom_text(aes(label=round(cumsum,2)), vjust=-0.3, size=3.5)+
  theme_minimal()
#Histogram

ggplot(data=randomVariable, aes(randomVariable$X)) + 
  geom_histogram(binwidth = 50000,
                 col = "black", 
                 fill="goldenrod2") + 
  stat_bin(geom = "text", binwidth = 50000, 
           aes(label = ifelse(..count.. > 0, ..count.., "")), vjust = -0.5)

# Question 24c
# Using R, construct a histogram of the billed amount values, and drop lines at the end-points of the standard ranges. 
# Also calculate how much the actual counts are within these ranges, and compare.

mean <-  80480.77 
sd <- 72695.09
(n <- randomVariable %>% summarise(count = n()))
binwidth <- 10000

hist(randomVariable$BilledAmount, breaks = 50,
     xlab = "Billing Amount", ylab = "Probability", 
     main = "Billed Amount Histogram", probability = T, las = 1)
lines(density(randomVariable$BilledAmount, adjust = 2), col="orange", lwd=2)  
abline(v = mean, col = "royalblue", lwd = 2)
abline(v = mean-sd, col = "darkgreen", lwd = 2)
abline(v = mean+sd, col = "darkgreen", lwd = 2)
abline(v = mean-2*sd, col = "red", lwd = 2)
abline(v = mean+2*sd, col = "red", lwd = 2)
legend(x = "topright", 
       c("Density plot", "Mean", "Mean±SD","Mean±2SD"),
       col = c("orange", "royalblue", "darkgreen", "red"),
       lwd = c(2, 2, 2))

# count of values within mean - sd and mean +sd
randomVariable %>% filter(between(BilledAmount,mean-sd,mean+sd)) %>% summarise(count = n())
# count: 85 out of 104 are within mean-sd,mean+sd
randomVariable %>% filter(between(BilledAmount,mean-2*sd,mean+2*sd)) %>% summarise(count = n())
# count: 99 out of 104 are within mean-2*sd,mean+2*sd

#Question 25: Random Sample
df_hosp
# get a random saple of 30 records without replacement

#making the sample set permanent

set.seed(1234)
sample <- sample_n(df_hosp, 30, replace = FALSE)


# Sample30 mean and sd
(sample_mean <- mean(sample$BilledAmount, na.rm=TRUE))
(sample_sd <- sd(sample$BilledAmount, na.rm=TRUE))

# Original Sample mean and sd
(mu <- mean(df_hosp$BilledAmount, na.rm=TRUE))
(sigma <- sd(df_hosp$BilledAmount, na.rm=TRUE))

# Sampling error of mean
(sample_mean - mu)
# Sampling error of sd
(sample_sd - sigma)

#Question 27

#Shapiro test for normality. Null Hypothesis: The distribution is normal
shapiro.test(df_hosp$BilledAmount)
# P value is much lower than 0.05. Hence rejecting the null hypothesis

# b. What is the probability that billed amounts lie between Rs.50,000 & Rs.1L? 
#   Again, compare your estimate with the actual value.

# calculation: Cumulative probability of <=100000 - Cumulative probability of <= 50000	

mean <-  80480.77 
sd <- 72695.09
# probability that billed amounts lie between Rs.50,000 & Rs.1L
p = pnorm(100000, 80480.77 , 72695.09, lower.tail = TRUE) - pnorm(50000, 80480.77 , 72695.09, lower.tail = TRUE) 
p
# Expected Frequency
np <- round(n * p)
np # Expected frequency is 28
#Observed
df_hosp %>% filter(between(BilledAmount,50000,100000)) %>% summarise(count = n())
# Observed is 26

# Question 28
# Construct a 95% confidence interval CI = [xBar – tα/2 s/√n, xBar – t α/2 s/√n], 
# where xBar and s are the sample mean and standard deviation.
#Sample Mean
sample_mean
#Sample sd
sample_sd
alpha <- 0.05
sample_n <- 30
df <- sample_n - 1
# t(0.025,29)
tcrit <- abs(qt(alpha/2, df))
tcrit
#t*S/Sqrt(n)
variation <- tcrit*(sample_sd/sqrt(sample_n))
variation
#Confidence Interval
conf_int <- c(sample_mean-variation, sample_mean+variation)
conf_int

# Polulation Mean
mu <- mean(df_hosp$BilledAmount, na.rm=TRUE)
mu # 77613.72
# The population mean lies between the confidence intervals with alpha = 0.05

#Question 30
# Carry out a two-sample t-test using the Analysis Toolpak, as well as in R, and explain the conclusions.

# take a copy of the data set with just the required variables
bill_date <- df_hosp %>% select(FinalBillDate,BilledAmount)
bill_date %>%  glimpse()

# Convert to Date time
bill_date$FinalBillDate <- format(as.POSIXct(bill_date$FinalBillDate, format='%d/%m/%Y %H:%M'),format='%d/%m/%Y')

bill_date

# Convert to 2 data sets: one for 1st Aug, other for 2nd Aug
aug_1 <- bill_date %>% filter( FinalBillDate == "01/08/2018" )
aug_1
aug_2 <- bill_date %>% filter( FinalBillDate == "02/08/2018" )
aug_2

# Summarising 
group_by(bill_date, FinalBillDate) %>%
  summarise(
    count = n(),
    mean = mean(BilledAmount, na.rm = TRUE),
    sd = sd(BilledAmount, na.rm = TRUE)
  )

# Plot BilledAmount by date and color by date
library("ggpubr")
ggboxplot(bill_date, x = "FinalBillDate", y = "BilledAmount", 
          color = "FinalBillDate", palette = c("#00AFBB", "#E7B800"),
          ylab = "Billed Amount", xlab = "Date")

# Compute t-test
res <- t.test(BilledAmount ~ FinalBillDate, data = bill_date, var.equal = FALSE)
res

# We obtained p-value greater than 0.05, then we can conclude that the averages of two groups are significantly similar. 
# The value of t-stat is less than the Critical t for 81, which  we can calculate:
  
(qt(0.975, 81))


