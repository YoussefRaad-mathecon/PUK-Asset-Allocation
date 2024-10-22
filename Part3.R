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


# Calculate average total fees
average_fixed_fee <- mean(rep(fixed_fee, nrow(MarketReturn)))  # Fixed fee is constant
average_performance_fee <- mean(MarketReturn$Performance_Fee, na.rm = TRUE)  # Average performance fee
average_total_fee <- average_fixed_fee + average_performance_fee  # Total average fee

# Calculate net expected return after fees
E_R_S_net_final <- mean(MarketReturn$Net_Portfolio_Return, na.rm = TRUE)  # Expected return after fees

# Calculate net volatility after fees
sigma_S_net_final <- sd(MarketReturn$Net_Portfolio_Return, na.rm = TRUE)  # Volatility after fees

# Print the updated results for comparison
cat("Average Total Fee (per month): ", average_total_fee, "\n")
cat("Expected return of stocks (with overlay and fees): ", E_R_S_net_final, "\n")
cat("Volatility of stocks (with overlay and fees): ", sigma_S_net_final, "\n")

# Compare with previous expected return and volatility
cat("Expected return of stocks (without fees): ", E_R_S, "\n")  # Expected return without fees
cat("Volatility of stocks (without fees): ", sigma_S, "\n")  # Volatility without fees


####################################################################################################################
####################################################################################################################
#----------------------------------------- D+E -------------------------------------------------------------------
####################################################################################################################
####################################################################################################################

#sequence of overlay sizes (0.1% to 50% by 0.01% increments)
overlay_sizes <- seq(0.001, 0.50, by = 0.001)

# vectors to store
expected_returns_net <- c()
volatilities_net <- c()


# Function to calculate values for different overlay sizes.
for (size in overlay_sizes) {
  # to use different sizes
  long_percentage <- size
  short_percentage <- size
  
  # Recalculate portfolio returns with the new overlay size. We assume it is
  # not a tilt but a over-exposure to 100%+
  MarketReturn <- MarketReturn %>%
    mutate(
      Long_Returns = BIG.HiPRIOR * long_percentage,
      Short_Returns = BIG.LoPRIOR * -short_percentage,
      New_Portfolio_Returns = (Long_Returns + Short_Returns) / 100,
      MarketAssumptions = AverageReturn + New_Portfolio_Returns,
      Excess_Return = MarketAssumptions - hurdle_rate,
      Performance_Fee = pmax(Excess_Return, 0) * 0.1,
      Total_Fee = fixed_fee + Performance_Fee,
      Net_Portfolio_Return = MarketAssumptions - Total_Fee
    )
  
  # expected return and volatility
  E_R_S_net <- mean(MarketReturn$Net_Portfolio_Return)
  sigma_S_net <- sd(MarketReturn$Net_Portfolio_Return)
  
  # store for later
  expected_returns_net <- c(expected_returns_net, E_R_S_net)
  volatilities_net <- c(volatilities_net, sigma_S_net)
}

# combine results 
results <- data.frame(
  Overlay_Size = overlay_sizes,
  Expected_Return = expected_returns_net,
  Volatility = volatilities_net
)



# Find Sharpe Ratios (we still assume RF should be included)
sharpe_ratios <- (expected_returns_net - mean(RF)) / volatilities_net
results <- cbind(results, Sharpe_Ratio = sharpe_ratios)


# Find the overlay size with the maximum Sharpe ratio for the plot
max_sharpe_idx <- which.max(sharpe_ratios)
max_sharpe_ratio <- sharpe_ratios[max_sharpe_idx]
max_overlay_size <- overlay_sizes[max_sharpe_idx]
ggplot(results, aes(x = Overlay_Size, y = Sharpe_Ratio)) +
  geom_line(color = "blue") +
  
  # Max Sharpe line
  geom_vline(xintercept = max_overlay_size, linetype = "dashed", color = "red") +
  
  annotate("text", x = max_overlay_size, y = max_sharpe_ratio + 0.02, 
           label = paste("Max Sharpe:", round(max_sharpe_ratio, 4)), 
           color = "red", size = 4, vjust = 12, hjust = 1.2) +
  
  # Professional annotation for overlay size, expected return, and volatility
  annotate("text", x = 0.41, y = 0.12, 
           label = " Overlay Size: 41%\nReturn: 0.752%\n Volatility: 4.23%", 
           color = "darkgreen", size = 4, hjust = -0.1) +
  
  ggtitle("Sharpe Ratio vs. Overlay Size") +
  xlab("Overlay Size") +
  ylab("Sharpe Ratio") +
  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14))

results[402,]
results[415,]
# The target is hit at 40.2% with Sharpe ratio 0.1271763 and expected return 0.007502576
# but the maximum Sharpe ratio is 0.1271989 at overlay 41.5% with expected return 0.007536794
