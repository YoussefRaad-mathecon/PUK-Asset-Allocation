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


set.seed(123)
setwd("C:/Users/youss/OneDrive - University of Copenhagen/PUK")


####################################################################################################################
####################################################################################################################
#-------------------------- Kenneth R French Data Library Data -----------------------------------------------------
####################################################################################################################
####################################################################################################################
# Functions to convert character columns to numeric and impute missing values

impute_data_monthly <- function(data) {
  
  # Convert all other columns to numeric and reassemble into a dataframe
  data <- as.data.frame(lapply(data, function(x) as.numeric(as.character(x))))
  
  # Check for missing values and impute if necessary
  if (anyNA(data)) {
    imputed_data <- mice(data, m = 1, method = 'pmm', maxit = 50, seed = 123)
    data <- complete(imputed_data)
  }
  
  # Convert the first column (dates in YYYYMM) to Date format
  data[[1]] <- as.Date(paste0(substr(data[[1]], 1, 4), "-", substr(data[[1]], 5, 6), "-01"))
  return(data)
}


impute_data_annual <- function(data) {
  
  # Convert all other columns to numeric and reassemble into a dataframe
  data <- as.data.frame(lapply(data, function(x) as.numeric(as.character(x))))
  
  # Check for missing values and impute if necessary
  if (anyNA(data)) {
    imputed_data <- mice(data, m = 1, method = 'pmm', maxit = 50, seed = 123)
    data <- complete(imputed_data)
  }
  

  data[[1]] <- as.numeric(data[[1]])
  return(data)
}



### FF data
FFdata <- read.csv("F-F_Research_Data_Factors.CSV", header = TRUE, sep = ",", skip = 3, fill = TRUE, strip.white = TRUE)
colnames(FFdata) <- c("Date", "Mkt_RF", "SMB", "HML", "RF")
FFdata_Monthly_Factors <- FFdata[7:1177,] #start from 7 to match MOMexp data
FFdata_Annual_Factors <- FFdata[1180:1276,]

# Convert Monthly/Annual Factors to numeric (except for the Date column)
FFdata_Monthly_Factors[, 2:5] <- lapply(FFdata_Monthly_Factors[, 2:5], as.numeric)
FFdata_Annual_Factors[, 2:5] <- lapply(FFdata_Annual_Factors[, 2:5], as.numeric)


### Portfolio data 6_Portfolios_ME_Prior_12_2.CSV
MOMexp <- read.csv("6_Portfolios_ME_Prior_12_2.CSV", header = FALSE, sep = ",", skip = 12, fill = TRUE, strip.white = TRUE)
colnames(MOMexp) <- c("Date", "SMALL LoPRIOR", "ME1 PRIOR2", "SMALL HiPRIOR", "BIG LoPRIOR", "ME2 PRIOR2", "BIG HiPRIOR")
MOMexp[MOMexp == -99.99 | MOMexp == -9999] <- NA
sum(is.na(MOMexp)) # 0 NA's


# Subsetting with row index reset for each subset
MOMexp_Average_Value_Weighted_Returns_Monthly <- MOMexp[1:1171, ]
row.names(MOMexp_Average_Value_Weighted_Returns_Monthly) <- NULL

MOMexp_Average_Equal_Weighted_Returns_Monthly <- MOMexp[1174:2344, ]
row.names(MOMexp_Average_Equal_Weighted_Returns_Monthly) <- NULL

MOMexp_Average_Value_Weighted_Returns_Annual <- MOMexp[2347:2443, ]
row.names(MOMexp_Average_Value_Weighted_Returns_Annual) <- NULL

MOMexp_Average_Equal_Weighted_Returns_Annual <- MOMexp[2446:2542, ]
row.names(MOMexp_Average_Equal_Weighted_Returns_Annual) <- NULL

MOMexp_Number_of_Firms_in_Portfolios <- MOMexp[2545:3715, ]
row.names(MOMexp_Number_of_Firms_in_Portfolios) <- NULL

MOMexp_Average_Firm_Size <- MOMexp[3718:4888, ]
row.names(MOMexp_Average_Firm_Size) <- NULL

MOMexp_Equally_Weighted_Average_of_Prior_Returns <- MOMexp[4891:6061, ]
row.names(MOMexp_Equally_Weighted_Average_of_Prior_Returns) <- NULL

MOMexp_Value_Weighted_Average_of_Prior_Returns <- MOMexp[6064:7234, ]
row.names(MOMexp_Value_Weighted_Average_of_Prior_Returns) <- NULL


# Impute missing values for each dataset
MOMexp_Average_Value_Weighted_Returns_Monthly <- impute_data_monthly(MOMexp_Average_Value_Weighted_Returns_Monthly)
MOMexp_Average_Equal_Weighted_Returns_Monthly <- impute_data_monthly(MOMexp_Average_Equal_Weighted_Returns_Monthly)
MOMexp_Average_Value_Weighted_Returns_Annual <- impute_data_annual(MOMexp_Average_Value_Weighted_Returns_Annual)
MOMexp_Average_Equal_Weighted_Returns_Annual <- impute_data_annual(MOMexp_Average_Equal_Weighted_Returns_Annual)
MOMexp_Number_of_Firms_in_Portfolios <- impute_data(MOMexp_Number_of_Firms_in_Portfolios)
MOMexp_Average_Firm_Size <- impute_data(MOMexp_Average_Firm_Size)
MOMexp_Equally_Weighted_Average_of_Prior_Returns <- impute_data_monthly(MOMexp_Equally_Weighted_Average_of_Prior_Returns)
MOMexp_Value_Weighted_Average_of_Prior_Returns <- impute_data_monthly(MOMexp_Value_Weighted_Average_of_Prior_Returns)


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


# Average Value Weighted Returns -- Monthly
MOMdep_Average_Value_Weighted_Returns_Monthly <- MOMdep[1:1171,]
row.names(MOMdep_Average_Value_Weighted_Returns_Monthly) <- NULL

# Average Equal Weighted Returns -- Monthly
MOMdep_Average_Equal_Weighted_Returns_Monthly <- MOMdep[1174:2344,]
row.names(MOMdep_Average_Equal_Weighted_Returns_Monthly) <- NULL

# Average Value Weighted Returns -- Annual
MOMdep_Average_Value_Weighted_Returns_Annual <- MOMdep[2347:2443,]
row.names(MOMdep_Average_Value_Weighted_Returns_Annual) <- NULL

# Average Equal Weighted Returns -- Annual
MOMdep_Average_Equal_Weighted_Returns_Annual <- MOMdep[2446:2542,]
row.names(MOMdep_Average_Equal_Weighted_Returns_Annual) <- NULL

# Number of Firms in Portfolios
MOMdep_Number_of_Firms_in_Portfolios <- MOMdep[2545:3715,]
row.names(MOMdep_Number_of_Firms_in_Portfolios) <- NULL

# Average Firm Size
MOMdep_Average_Firm_Size <- MOMdep[3718:4888,]
row.names(MOMdep_Average_Firm_Size) <- NULL

# Equally-Weighted Average of Prior Returns
MOMdep_Equally_Weighted_Average_of_Prior_Returns <- MOMdep[4891:6061,]
row.names(MOMdep_Equally_Weighted_Average_of_Prior_Returns) <- NULL

# Value-Weighted Average of Prior Returns
MOMdep_Value_Weighted_Average_of_Prior_Returns <- MOMdep[6064:7234,]
row.names(MOMdep_Value_Weighted_Average_of_Prior_Returns) <- NULL


# Impute each dataset and overwrite the original for consistency
MOMdep_Average_Value_Weighted_Returns_Monthly <- impute_data_monthly(MOMdep_Average_Value_Weighted_Returns_Monthly)
MOMdep_Average_Equal_Weighted_Returns_Monthly <- impute_data_monthly(MOMdep_Average_Equal_Weighted_Returns_Monthly)
MOMdep_Average_Value_Weighted_Returns_Annual <- impute_data_annual(MOMdep_Average_Value_Weighted_Returns_Annual)
MOMdep_Average_Equal_Weighted_Returns_Annual <- impute_data_annual(MOMdep_Average_Equal_Weighted_Returns_Annual)
MOMdep_Number_of_Firms_in_Portfolios <- impute_data_monthly(MOMdep_Number_of_Firms_in_Portfolios)
MOMdep_Average_Firm_Size <- impute_data_monthly(MOMdep_Average_Firm_Size)
MOMdep_Equally_Weighted_Average_of_Prior_Returns <- impute_data_monthly(MOMdep_Equally_Weighted_Average_of_Prior_Returns)
MOMdep_Value_Weighted_Average_of_Prior_Returns <- impute_data_monthly(MOMdep_Value_Weighted_Average_of_Prior_Returns)



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

### Ensure the date column is in Date format
TSYdata$Date <- as.Date(TSYdata$Date, format = "%m/%d/%y")

# OBS: for some reason x2.Mo won't remove NA's in X2.M0; suspect it is because of high missingness with high colinearity
TSYdata <- impute_data(TSYdata)

# OBS: we can use median instead?! maybe + some bps depending on date?!
TSYdata$X2.Mo[is.na(TSYdata$X2.Mo)] <- median(TSYdata$X2.Mo, na.rm = TRUE)
sum(is.na(TSYdata)) # 20258 NA's


####################################################################################################################
####################################################################################################################
#---------------------------------- Monthly Returns ----------------------------------------------------------------
####################################################################################################################
####################################################################################################################

# Convert yields from percentages to decimal values
TSYdata_dec <- TSYdata %>%
  mutate(across(-Date, ~ .x / 100)) # This converts all columns except Date to decimals

# Resample to monthly data using the last value in each month
monthly_TS <- TSYdata_dec %>%
  group_by(month = floor_date(Date, "month")) %>%
  slice_tail(n = 1) %>%
  ungroup()

# Define the PV and get_r functions
PV <- function(C, Y, T) {
  pv <- 0
  if (T == 10) {
    t_j <- seq(1, 9)
  } else {
    t_j <- seq(1, 9) - 1/12
  }
  for (i in t_j) {
    pv <- pv + C / (1 + Y)^i
  }
  pv <- pv + C / (1 + Y)^T + 1 / (1 + Y)^T
  return(pv)
}

get_r <- function(Yim1, Yi) {
  Ti <- 10  # Fixed value for the 10-year maturity
  Tim1 <- Ti - 1/12  # Previous time step (1 month earlier)
  
  pv1 <- PV(Yim1, Yi, Tim1)  # Present value with shifted yield and current yield
  pv2 <- PV(Yim1, Yim1, Ti)  # Present value with shifted yield twice
  
  # Return the relative change in present values
  r <- pv1 / pv2 - 1
  return(r)
}

# Compute the monthly returns for the 10-Year yield
monthly_TS <- monthly_TS %>%
  mutate(X10.Yr.1M = lag(X10.Yr),
         X10.Yr.Return = mapply(get_r, X10.Yr.1M, X10.Yr))

monthly_TS <- monthly_TS %>%
  mutate(`10 Yr m1` = lag(`X10.Yr`),  # Shift the 10-Year yield by one month
         `10YrReturns` = mapply(get_r, `10 Yr m1`, `X10.Yr`))  # Apply the function element-wise

Bonds <- monthly_TS$X10.Yr.Return


# Filter the data 1990-01-02 to 1990-01-31
TSYdata_1990_Jan <- TSYdata %>%
  filter(Date >= as.Date("1990-01-02") & Date <= as.Date("1990-01-31"))

# Calculate the mean for each bond
avg_1990_Jan <- colMeans(TSYdata_1990_Jan[, c("X1.Mo", "X2.Mo", "X3.Mo", "X4.Mo", "X6.Mo", 
                                              "X1.Yr", "X2.Yr", "X3.Yr", "X5.Yr", "X7.Yr", 
                                              "X10.Yr", "X20.Yr", "X30.Yr")])
avg_1990_Jan <- avg_1990_Jan/100


Bonds[1] <- avg_1990_Jan[11]
RF <- FFdata_Monthly_Factors$RF ### RF rate

