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
#setwd("~/Documents/KU/PUKAssetAllocation/Exam/RCode")
setwd("C:/Users/youss/OneDrive - University of Copenhagen/PUK")
####################################################################################################################
####################################################################################################################
#------------------------------------------- A ---------------------------------------------------------------------
####################################################################################################################
####################################################################################################################
# RF and bonds from Data.R
RF <- FFdata_Monthly_Factors$RF[757:1164] ### RF rate
RF <- RF / 100
BondData <- read.csv("Bonds_done.csv", header = TRUE)
Bonds <- BondData$Monthly.Return.10.Yr


# Average Value Weighted Returns -- Monthly
MOMdep_Average_Value_Weighted_Returns_Monthly_part3 <- MOMdep_Average_Value_Weighted_Returns_Monthly[757:1164,]
# Average Equal Weighted Returns -- Monthly
MOMdep_Average_Equal_Weighted_Returns_Monthly_part3 <- MOMdep_Average_Equal_Weighted_Returns_Monthly[757:1164,]
# Number of Firms in Portfolios
MOMdep_Number_of_Firms_in_Portfolios_part3 <- MOMdep_Number_of_Firms_in_Portfolios[757:1164,]
# Average Firm Size
MOMdep_Average_Firm_Size_part3 <- MOMdep_Average_Firm_Size[757:1164,]
# Equally-Weighted Average of Prior Returns
MOMdep_Equally_Weighted_Average_of_Prior_Returns_part3 <- MOMdep_Equally_Weighted_Average_of_Prior_Returns[757:1164,]
# Value-Weighted Average of Prior Returns
MOMdep_Value_Weighted_Average_of_Prior_Returns_part3 <- MOMdep_Value_Weighted_Average_of_Prior_Returns[757:1164,]



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


# Define overlay percentages
short_percentage <- 0.25
long_percentage <- 0.25

MarketReturn <- MOMdep_Average_Value_Weighted_Returns_Monthly_part3

# Calculate new portfolio returns
MarketReturn <- MarketReturn %>%
  mutate(
    AverageReturn = market_return_dep,
    Long_Returns = BIG.HiPRIOR * long_percentage,   # Long on BIG HiPRIOR
    Short_Returns = BIG.LoPRIOR * -short_percentage,  # Short on BIG LoPRIOR
    New_Portfolio_Returns = (Long_Returns + Short_Returns) / 100,  # Combine returns
    MarketAssumptions = AverageReturn + New_Portfolio_Returns
  )




E_R_C <- mean(RF)  # Expected return for cash
E_R_S <- mean(MarketReturn$MarketAssumptions) # Expected return for stocks (with overlay)
E_R_B <- mean(Bonds)  # Expected return for bonds
# Define volatilities
sigma_C <- sd(RF)   # Volatility of cash
sigma_S <- sd(MarketReturn$MarketAssumptions) # Volatility of stocks (with overlay)
sigma_B <- sd(Bonds)  # Volatility of bonds

# Calculate covariances
cov_S_B <- cov(MarketReturn$MarketAssumptions, Bonds, use = "pairwise.complete.obs")
cov_S_C <- cov(MarketReturn$MarketAssumptions, RF , use = "pairwise.complete.obs")
cov_B_C <- cov(Bonds, RF, use = "pairwise.complete.obs")

# Define covariance matrix (3x3)
cov_matrix <- matrix(c(sigma_S^2, cov_S_B, cov_S_C,
                       cov_S_B, sigma_B^2, cov_B_C,
                       cov_S_C, cov_B_C, sigma_C^2), nrow = 3)

# Define expected returns vector
expected_returns <- c(E_R_S, E_R_B, E_R_C)

# Print the results
cat("Expected return of stocks (with overlay): ", expected_returns[1], "\n") # 0.010205607 
cat("Expected return of bonds: ", expected_returns[2], "\n") # 0.003015631 
cat("Expected return of cash: ", expected_returns[3], "\n") # 0.002138480
cat("Volatility of stocks (with overlay): ", sigma_S, "\n") # 0.04214546 
cat("Volatility of bonds: ", sigma_B, "\n") # 0.02114967 
cat("Volatility of cash: ", sigma_C, "\n") # 0.001878971 
cat("Covariance matrix: ", cov_matrix, "\n")
### > cov_matrix
### [,1]          [,2]          [,3]
### [1,]  1.776240e-03 -1.375522e-05 -3.708831e-08
### [2,] -1.375522e-05  4.473086e-04  3.126206e-06
### [3,] -3.708831e-08  3.126206e-06  3.530530e-06



####################################################################################################################
####################################################################################################################
#----------------------------------------- B + C -------------------------------------------------------------------
####################################################################################################################
####################################################################################################################
# Define hurdle rate and fixed fee
hurdle_rate <- 0.01  # 100 bps
fixed_fee <- 0.0015  # 15 bps 

# Calculate the excess return over the hurdle for each period
MarketReturn <- MarketReturn %>%
  mutate(
    Excess_Return = MarketAssumptions - hurdle_rate,
    Performance_Fee = pmax(Excess_Return, 0) * 0.1,  # 10% of the positive part of excess return
    Total_Fee = fixed_fee + Performance_Fee  # Total fee for each period
  )

# Adjust returns by subtracting the fees
MarketReturn <- MarketReturn %>%
  mutate(
    Net_Portfolio_Return = MarketAssumptions - Total_Fee
  )

# Recalculate the expected return and volatility with the adjusted returns
E_R_S_net <- mean(MarketReturn$Net_Portfolio_Return)  # Expected return of stocks after fees
sigma_S_net <- sd(MarketReturn$Net_Portfolio_Return)  # Volatility of stocks after fees

# Print the updated results
cat("Expected return of stocks (with overlay and fees): ", E_R_S_net, "\n")
cat("Volatility of stocks (with overlay and fees): ", sigma_S_net, "\n")
