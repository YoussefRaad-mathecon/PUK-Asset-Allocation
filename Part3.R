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
#------------------------------------------- A ---------------------------------------------------------------------
####################################################################################################################
####################################################################################################################
# RF and bonds from Data.R
RF <- FFdata_Monthly_Factors$RF### RF rate
RF <- RF / 100
Bonds <- Bonds$Monthly.Return.10.Yr # Bonds


# Average Value Weighted Returns -- Monthly
MOMdep_Average_Value_Weighted_Returns_Monthly_part3 <- MOMdep_Average_Value_Weighted_Returns_Monthly
# Average Equal Weighted Returns -- Monthly
MOMdep_Average_Equal_Weighted_Returns_Monthly_part3 <- MOMdep_Average_Equal_Weighted_Returns_Monthly
# Average Value Weighted Returns -- Annual
MOMdep_Average_Value_Weighted_Returns_Annual_part3 <- MOMdep_Average_Value_Weighted_Returns_Annual
# Average Equal Weighted Returns -- Annual
MOMdep_Average_Equal_Weighted_Returns_Annual_part3 <- MOMdep_Average_Equal_Weighted_Returns_Annual
# Number of Firms in Portfolios
MOMdep_Number_of_Firms_in_Portfolios_part3 <- MOMdep_Number_of_Firms_in_Portfolios
# Average Firm Size
MOMdep_Average_Firm_Size_part3 <- MOMdep_Average_Firm_Size
# Equally-Weighted Average of Prior Returns
MOMdep_Equally_Weighted_Average_of_Prior_Returns_part3 <- MOMdep_Equally_Weighted_Average_of_Prior_Returns
# Value-Weighted Average of Prior Returns
MOMdep_Value_Weighted_Average_of_Prior_Returns_part3 <- MOMdep_Value_Weighted_Average_of_Prior_Returns



# Remove the first column (assumed to be character) and keep only numeric columns
num_firms <- MOMdep_Number_of_Firms_in_Portfolios_part3[, -1]
avg_firm_size <- MOMdep_Average_Firm_Size_part3[, -1]
avg_value_weighted_returns <- MOMdep_Average_Value_Weighted_Returns_Monthly_part3[, -1]

# Calculate the portfolio weights for all portfolios in one step
portfolio_weights <- num_firms * avg_firm_size

# Calculate total weight by summing all portfolio weights
total_weight <- rowSums(portfolio_weights)

# Calculate the market return using vectorized operations
market_return_dep <- rowSums(avg_value_weighted_returns * portfolio_weights) / total_weight
market_return_dep <- market_return_dep / 100


E_R_S <- mean(market_return_dep)  # Average stock return
E_R_B <- mean(Bonds)  # Average bond return
E_R_C <- mean(RF)  # Expected return for cash


# Use value-weighted monthly returns dataset
low_momentum <- MOMdep_Average_Value_Weighted_Returns_Monthly_part3$`BIG LoPRIOR`
high_momentum <- MOMdep_Average_Value_Weighted_Returns_Monthly_part3$`BIG HiPRIOR`



