####################################################################################################################
####################################################################################################################
#------------------------------------- Packages --------------------------------------------------------------------
####################################################################################################################
####################################################################################################################
library(tidyverse) ### Data manipulations
library(dtplyr) ### Data manipulations - merge datasets
library(ggplot2) ### Plots
library(gridExtra) ### Plots
library(Quandl) ### Data
library(dplyr) ### Data manipulations
library(stats) ### ACF plots'
library(matrixcalc) ### Matrix calculations
library("RColorBrewer") ### Colors
library(latex2exp) ### Text for plots
library(matrixStats) ### ColSds
library(dplyr) ### Pipe-function
library(lubridate) ### Date manipulations
library(mice) ### Single imputation
####################################################################################################################
####################################################################################################################
#----------------------------------- Working Directory -------------------------------------------------------------
####################################################################################################################
####################################################################################################################


#set.seed(123)
#setwd("C:/Users/youss/OneDrive - University of Copenhagen/PUK")


####################################################################################################################
####################################################################################################################
#-------------------------- Kenneth R French Data Library Data -----------------------------------------------------
####################################################################################################################
####################################################################################################################
# Function to impute missing values for a dataset
# Function to convert character columns to numeric and impute missing values
impute_data <- function(data) {
  # Convert character columns to numeric
  data[] <- lapply(data, function(x) as.numeric(as.character(x)))
  
  # Check for missing values and impute if necessary
  if (anyNA(data)) {
    imputed_data <- mice(data, m = 1, method = 'pmm', maxit = 5, seed = 123)
    data <- complete(imputed_data)
  }
  
  return(data)
}

### FF data
FFdata <- read.csv("F-F_Research_Data_Factors.CSV", header = TRUE, sep = ",", skip = 2, fill = TRUE, strip.white = TRUE)
colnames(FFdata) <- c("Date", "Mkt_RF", "SMB", "HML", "RF")
FFdata <- FFdata[1:1276,] #remove copyright lines
sum(is.na(FFdata)) # Na's 0



### Portfolio data 6_Portfolios_ME_Prior_12_2.CSV
MOMexp <- read.csv("6_Portfolios_ME_Prior_12_2.CSV", header = FALSE, sep = ",", skip = 12, fill = TRUE, strip.white = TRUE)
colnames(MOMexp) <- c("Date", "SMALL LoPRIOR", "ME1 PRIOR2", "SMALL HiPRIOR", "BIG LoPRIOR", "ME2 PRIOR2", "BIG HiPRIOR")
MOMexp[MOMexp == -99.99 | MOMexp == -9999] <- NA
sum(is.na(MOMexp)) # 0 NA's

## Split into subsets of the CSV file 6_Portfolios_ME_Prior_12_2.CSV
# Average Value Weighted Returns -- Monthly
MOMexp_Average_Value_Weighted_Returns_Monthly <- MOMexp[1:1171,]
# Average Equal Weighted Returns -- Monthly
MOMexp_Average_Equal_Weighted_Returns_Monthly <- MOMexp[1174:2344,]
# Average Value Weighted Returns -- Annual
MOMexp_Average_Value_Weighted_Returns_Annual <- MOMexp[2347:2443,]
# Average Equal Weighted Returns -- Annual
MOMexp_Average_Equal_Weighted_Returns_Annual <- MOMexp[2446:2542,]
# Number of Firms in Portfolios
MOMexp_Number_of_Firms_in_Portfolios <- MOMexp[2545:3715,]
# Average Firm Size
MOMexp_Average_Firm_Size <- MOMexp[3718:4888,]
# Equally-Weighted Average of Prior Returns
MOMexp_Equally_Weighted_Average_of_Prior_Returns <- MOMexp[4891:6061,]
# Value-Weighted Average of Prior Returns
MOMexp_Value_Weighted_Average_of_Prior_Returns <- MOMexp[6064:7234,]

# Impute missing values for each dataset
MOMexp_Average_Value_Weighted_Returns_Monthly <- impute_data(MOMexp_Average_Value_Weighted_Returns_Monthly)
MOMexp_Average_Equal_Weighted_Returns_Monthly <- impute_data(MOMexp_Average_Equal_Weighted_Returns_Monthly)
MOMexp_Average_Value_Weighted_Returns_Annual <- impute_data(MOMexp_Average_Value_Weighted_Returns_Annual)
MOMexp_Average_Equal_Weighted_Returns_Annual <- impute_data(MOMexp_Average_Equal_Weighted_Returns_Annual)
MOMexp_Number_of_Firms_in_Portfolios <- impute_data(MOMexp_Number_of_Firms_in_Portfolios)
MOMexp_Average_Firm_Size <- impute_data(MOMexp_Average_Firm_Size)
MOMexp_Equally_Weighted_Average_of_Prior_Returns <- impute_data(MOMexp_Equally_Weighted_Average_of_Prior_Returns)
MOMexp_Value_Weighted_Average_of_Prior_Returns <- impute_data(MOMexp_Value_Weighted_Average_of_Prior_Returns)


### Portfolio data 25_Portfolios_ME_Prior_12_2.CSV
MOMdep <- read.csv("25_Portfolios_ME_Prior_12_2.CSV", header = FALSE, sep = ",", skip = 12, fill = TRUE, strip.white = TRUE)
colnames(MOMdep) <- c("Date", "SMALL LoPRIOR", "ME1 PRIOR2", "ME1 PRIOR3", "ME1 PRIOR4",
                      "SMALL HiPRIOR", "ME2 PRIOR1", "ME2 PRIOR2", "ME2 PRIOR3", "ME2 PRIOR4", "ME2 PRIOR5",
                      "ME3 PRIOR1", "ME3 PRIOR2", "ME3 PRIOR3", "ME3 PRIOR4", "ME3 PRIOR5", 
                      "ME4 PRIOR1", "ME4 PRIOR2", "ME4 PRIOR3", "ME4 PRIOR4", "ME4 PRIOR5",
                      "BIG LoPRIOR", "ME5 PRIOR2", "ME5 PRIOR3", "ME5 PRIOR4", "BIG HiPRIOR")
MOMdep[MOMdep == -99.99 | MOMdep == -9999] <- NA
sum(is.na(MOMdep)) # 18 NA's under BIG LoPRIOR but in different subsets!
colSums(is.na(MOMdep))
#Date SMALL LoPRIOR    ME1 PRIOR2    ME1 PRIOR3    ME1 PRIOR4 SMALL HiPRIOR    ME2 PRIOR1    ME2 PRIOR2    ME2 PRIOR3 
#0             0             0             0             0             0             0             0             0 
#ME2 PRIOR4    ME2 PRIOR5    ME3 PRIOR1    ME3 PRIOR2    ME3 PRIOR3    ME3 PRIOR4    ME3 PRIOR5    ME4 PRIOR1    ME4 PRIOR2 
#0             0             0             0             0             0             0             0             0 
#ME4 PRIOR3    ME4 PRIOR4    ME4 PRIOR5   BIG LoPRIOR    ME5 PRIOR2    ME5 PRIOR3    ME5 PRIOR4   BIG HiPRIOR 
#0             0             0            18             0             0             0             0 


## Split into subsets of the CSV file 25_Portfolios_ME_Prior_12_2.CSV
#Average Value Weighted Returns -- Monthly
MOMdep_Average_Value_Weighted_Returns_Monthly <- MOMexp[1:1171,]
# Average Equal Weighted Returns -- Monthly
MOMdep_Average_Equal_Weighted_Returns_Monthly <- MOMexp[1174:2344,]
# Average Value Weighted Returns -- Annual
MOMdep_Average_Value_Weighted_Returns_Annual <- MOMexp[2347:2443,]
# Average Equal Weighted Returns -- Annual
MOMdep_Average_Equal_Weighted_Returns_Annual <- MOMexp[2446:2542,]
# Number of Firms in Portfolios
MOMdep_Number_of_Firms_in_Portfolios <- MOMexp[2545:3715,]
# Average Firm Size
MOMdep_Average_Firm_Size <- MOMexp[3718:4888,]
# Equally-Weighted Average of Prior Returns
MOMdep_Equally_Weighted_Average_of_Prior_Returns <- MOMexp[4891:6061,]
# Value-Weighted Average of Prior Returns
MOMdep_Value_Weighted_Average_of_Prior_Returns <- MOMexp[6064:7234,]

# Impute each dataset and overwrite the original for consistency
MOMdep_Average_Value_Weighted_Returns_Monthly <- impute_data(MOMdep_Average_Value_Weighted_Returns_Monthly)
MOMdep_Average_Equal_Weighted_Returns_Monthly <- impute_data(MOMdep_Average_Equal_Weighted_Returns_Monthly)
MOMdep_Average_Value_Weighted_Returns_Annual <- impute_data(MOMdep_Average_Value_Weighted_Returns_Annual)
MOMdep_Average_Equal_Weighted_Returns_Annual <- impute_data(MOMdep_Average_Equal_Weighted_Returns_Annual)
MOMdep_Number_of_Firms_in_Portfolios <- impute_data(MOMdep_Number_of_Firms_in_Portfolios)
MOMdep_Average_Firm_Size <- impute_data(MOMdep_Average_Firm_Size)
MOMdep_Equally_Weighted_Average_of_Prior_Returns <- impute_data(MOMdep_Equally_Weighted_Average_of_Prior_Returns)
MOMdep_Value_Weighted_Average_of_Prior_Returns <- impute_data(MOMdep_Value_Weighted_Average_of_Prior_Returns)



####################################################################################################################
####################################################################################################################
#----------------------------------- US Treasury Data --------------------------------------------------------------
####################################################################################################################
####################################################################################################################


TSYdata <- read.csv("yield-curve-rates-1990-2023.csv", header = TRUE)
sum(is.na(TSYdata)) # 20258 NA's
colSums(is.na(TSYdata))
#  Date  X1.Mo  X2.Mo  X3.Mo  X4.Mo  X6.Mo  X1.Yr  X2.Yr  X3.Yr  X5.Yr  X7.Yr X10.Yr X20.Yr X30.Yr 
#     0   2900   7205      4   8207      1      1      1      1      1      1      1    940    995

# frame without dates
TSYdata_clean <- TSYdata[, c("X1.Mo", "X2.Mo", "X3.Mo", "X4.Mo", "X6.Mo", 
                             "X1.Yr", "X2.Yr", "X3.Yr", "X5.Yr", "X7.Yr", 
                             "X10.Yr", "X20.Yr", "X30.Yr")]

TSYdata_clean <- impute_data(TSYdata_clean)

####################################################################################################################
####################################################################################################################
#------------------------------------ Daily Returns ----------------------------------------------------------------
####################################################################################################################
####################################################################################################################
### Initialize an empty data frame to hold the returns
DailyReturn <- data.frame(Date = TSYdata$Date)  ### Dates for the new returns, minus the first row with return zero



mat <- c(1/12, 2/12, 3/12, 4/12, 6/12, 1, 2, 3, 5, 7, 10, 20, 30) ### Maturities


### Define the present value function
PV <- function(C, Y, T) {
  ### Ensure yields are in decimal form
  ### Payment dates (annual coupon payments, i.e. 1Y steps)
  t_j <- seq(1, T)
  
  ### Calculate the present value for coupon payments
  coupon_PV <- sum(C / (1 + Y) ^ t_j)
  
  ### Calculate the present value for the principal repayment at time T
  principal_PV <- 1 / (1 + Y) ^ T
  
  ### Total present value
  PV_value <- coupon_PV + principal_PV
  
  return(PV_value)
}


### Loop through each row to calculate the monthly returns
for (i in 2:nrow(TSYdata_clean)) {
  for (j in 1:ncol(TSYdata_clean)) {   ### Use TSYdata_clean (without Date and MR) for looping through yields
    Y_prev <- TSYdata_clean[i - 1, j]  ### Previous period yield
    Y_current <- TSYdata_clean[i, j]   ### Current period yield
    
    T <- mat[j]  ### Use correct indexing for maturities (no need to adjust by -1)
    
    ### Calculate present values for Y_prev and Y_current
    PV_Y_current <- PV(Y_prev, Y_current, T - 1/12)  ### PV(Y_{i-1}; Y_i, T - 1M)
    PV_Y_prev <- PV(Y_prev, Y_prev, T)               ### PV(Y_{i-1}; Y_{i-1}, T)
    
    ### Calculate the return using the formula
    r_i <- (PV_Y_current / PV_Y_prev) - 1
    
    ### Store the return in the new data frame
    DailyReturn[i, j + 1] <- r_i  ### j + 1 to account for the Date column
  }
}

### Name the columns of the returns data frame
colnames(DailyReturn)[-1] <- colnames(TSYdata_clean)

### View the results
head(DailyReturn)

### Test the PV function for 100 different values of C > -1
C_values <- runif(100, min = -0.99, max = 5)  # Generate 100 random C values greater than -1

### Integer value of T (e.g., T = 5 years)
T <- 5

### Apply the PV function for C = Y over the 100 different values of C
results <- data.frame(C = C_values, PV = sapply(C_values, function(C) PV(C, C, T)))

### Print the results
print(results)



### ...or manually test it
test_PV <- function(C, T) {
  return(PV(C, C, T))
}
### Run the test with C = 5% and T = 10 years
test_PV(0.05, 10)





####################################################################################################################
####################################################################################################################
#---------------------------------- Monthly Returns ----------------------------------------------------------------
####################################################################################################################
####################################################################################################################

### Ensure the date column is in Date format
TSYdata$Date <- as.Date(TSYdata$Date, format = "%m/%d/%y")


MonthlyYields <- TSYdata %>%
  mutate(YnM = floor_date(Date, "month")) %>%
  group_by(YnM) %>%
  slice_head(n = 1) %>%  ### Get the first row for each month (which is the last date of the month)
  ungroup() %>%
  arrange(YnM)  ### Optionally, sort by month for better readability



### Initialize an empty data frame to hold the returns
MonthlyReturn <- data.frame(Date = MonthlyYields$Date)  ### Dates for the new returns, minus the first row with return zero

### Subset the dataframe
MonthlyYields_clean <- MonthlyYields[, c("X1.Mo", "X2.Mo", "X3.Mo", "X4.Mo", "X6.Mo", 
                             "X1.Yr", "X2.Yr", "X3.Yr", "X5.Yr", "X7.Yr", 
                             "X10.Yr", "X20.Yr", "X30.Yr")]

mat <- c(1/12, 2/12, 3/12, 4/12, 6/12, 1, 2, 3, 5, 7, 10, 20, 30) ### Maturities


### Loop through each row to calculate the monthly returns
for (i in 2:nrow(MonthlyYields_clean)) {
  for (j in 1:ncol(MonthlyYields_clean)) {   ### Use TSYdata_clean (without Date and MR) for looping through yields
    Y_prev <- MonthlyYields_clean[i - 1, j]  ### Previous period yield
    Y_current <- MonthlyYields_clean[i, j]   ### Current period yield
    
    T <- mat[j]  ### Use correct indexing for maturities (no need to adjust by -1)
    
    ### Calculate present values for Y_prev and Y_current
    PV_Y_current <- PV(Y_prev, Y_current, T - 1/12)  ### PV(Y_{i-1}; Y_i, T - 1M)
    PV_Y_prev <- PV(Y_prev, Y_prev, T)               ### PV(Y_{i-1}; Y_{i-1}, T)
    
    ### Calculate the return using the formula
    r_i <- (PV_Y_current / PV_Y_prev) - 1
    
    ### Store the return in the new data frame
    MonthlyReturn[i - 1, j + 1] <- r_i  ### j + 1 to account for the Date column
  }
}

### Name the columns of the returns data frame
colnames(MonthlyReturn)[-1] <- colnames(MonthlyYields_clean)

### View the results
head(MonthlyReturn)
tail(MonthlyReturn)


