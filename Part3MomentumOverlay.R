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

# Calculate the excess return over the hurdle for each period based only on overlay returns
MarketReturn <- MarketReturn %>%
  mutate(
    Excess_Return_Overlay = (New_Portfolio_Returns - hurdle_rate),  # Only overlay return used for excess return
    Performance_Fee_Overlay = pmax(Excess_Return_Overlay, 0) * 0.1,  # Fee on positive part of overlay excess return
    Overlay_Total_Fee = fixed_fee + Performance_Fee_Overlay  # Total overlay fee per period
  )

# Adjust returns by subtracting only the overlay fees from the market + overlay combined return
MarketReturn <- MarketReturn %>%
  mutate(
    Net_Portfolio_Return = MarketAssumptions - Overlay_Total_Fee
  )

# Calculate average total fees
average_fixed_fee <- mean(rep(fixed_fee, nrow(MarketReturn)))  # Fixed fee is constant
average_performance_fee <- mean(MarketReturn$Performance_Fee_Overlay, na.rm = TRUE)  # Average performance fee on overlay only
average_total_fee <- average_fixed_fee + average_performance_fee  # Total average fee for overlay only

# Calculate net expected return after fees
E_R_S_net_final <- mean(MarketReturn$Net_Portfolio_Return, na.rm = TRUE)  # Expected return after fees

# Calculate net volatility after fees
sigma_S_net_final <- sd(MarketReturn$Net_Portfolio_Return, na.rm = TRUE)  # Volatility after fees

# Print the updated results for comparison
cat("Average Total Overlay Fee (per month): ", average_total_fee, "\n") # 0.001760509 
cat("Expected return of stocks (with overlay and fees): ", E_R_S_net_final, "\n") # 0.008445098 
cat("Volatility of stocks (with overlay and fees): ", sigma_S_net_final, "\n") # 0.04212489 

# Calculate covariances
cov_S_B_fee <- cov(MarketReturn$Net_Portfolio_Return, Bonds, use = "pairwise.complete.obs")
cov_S_C_fee <- cov(MarketReturn$Net_Portfolio_Return, RF , use = "pairwise.complete.obs")
cov_B_C_fee <- cov(Bonds, RF, use = "pairwise.complete.obs")

# Define covariance matrix (3x3)
cov_matrix_fee <- matrix(c(sigma_S_net_final^2, cov_S_B_fee, cov_S_C_fee,
                           cov_S_B_fee, sigma_B^2, cov_B_C_fee,
                           cov_S_C_fee, cov_B_C_fee, sigma_C^2), nrow = 3)
print(cov_matrix_fee)
# Compare with previous expected return and volatility
cat("Expected return of stocks (without fees): ", E_R_S, "\n")  # Expected return without fees
cat("Volatility of stocks (without fees): ", sigma_S, "\n")  # Volatility without fees


####################################################################################################################
####################################################################################################################
#----------------------------------------- D+E -------------------------------------------------------------------
####################################################################################################################
####################################################################################################################
# Define sequence of overlay sizes (0.1% to 50% by 0.01% increments)
overlay_sizes <- seq(0.001, 0.50, by = 0.001)

# Vectors to store results
expected_returns_net <- c()
volatilities_net <- c()

# Loop over different overlay sizes
for (size in overlay_sizes) {
  # Define long and short percentages
  long_percentage <- size
  short_percentage <- size
  
  # Calculate overlay returns separately
  MarketReturn <- MarketReturn %>%
    mutate(
      Long_Returns = BIG.HiPRIOR * long_percentage,
      Short_Returns = BIG.LoPRIOR * -short_percentage,
      Overlay_Return = (Long_Returns + Short_Returns) / 100
    )
  
  # Calculate market assumptions (Market return plus overlay)
  MarketReturn <- MarketReturn %>%
    mutate(
      MarketAssumptions = AverageReturn + Overlay_Return
    )
  
  # Calculate fees based only on the overlay return
  MarketReturn <- MarketReturn %>%
    mutate(
      Excess_Overlay_Return = Overlay_Return - hurdle_rate,
      Overlay_Performance_Fee = pmax(Excess_Overlay_Return, 0) * 0.1,  # Fee on overlay return
      Overlay_Total_Fee = fixed_fee + Overlay_Performance_Fee,
      
      # Net Portfolio Return after deducting the overlay fee from MarketAssumptions
      Net_Portfolio_Return = MarketAssumptions - Overlay_Total_Fee
    )
  
  # Expected return and volatility after fees
  E_R_S_net <- mean(MarketReturn$Net_Portfolio_Return)
  sigma_S_net <- sd(MarketReturn$Net_Portfolio_Return)
  
  # Store for later analysis
  expected_returns_net <- c(expected_returns_net, E_R_S_net)
  volatilities_net <- c(volatilities_net, sigma_S_net)
}

# Combine results in a data frame
results <- data.frame(
  Overlay_Size = overlay_sizes,
  Expected_Return = expected_returns_net,
  Volatility = volatilities_net
)

# Annualize the monthly volatility
results$Risk_Annualized <- results$Volatility * sqrt(12)

# Calculate Sharpe Ratios, including RF
sharpe_ratios <- (results$Expected_Return - mean(RF)) / results$Volatility
results <- cbind(results, Sharpe_Ratio = sharpe_ratios)

# Find indices for max Sharpe, min volatility, and max return
max_sharpe_idx <- which.max(sharpe_ratios)
min_volatility_idx <- which.min(results$Volatility)
max_return_idx <- which.max(results$Expected_Return)

# Extract corresponding values
max_sharpe_ratio <- sharpe_ratios[max_sharpe_idx]
max_overlay_size <- overlay_sizes[max_sharpe_idx]
min_volatility <- results[min_volatility_idx, ]
max_return <- results[max_return_idx, ]

# Plotting with additional annotation for index 397
ggplot(results, aes(x = Overlay_Size, y = Sharpe_Ratio)) +
  geom_line(color = "#666666", linewidth = 3) +
  
  # Max Sharpe line
  geom_vline(xintercept = max_sharpe$Overlay_Size, linetype = "dashed", color = "#901a1E", linewidth = 3) +
  
  # Min Volatility line
  geom_vline(xintercept = min_volatility$Overlay_Size, linetype = "dotted", color = "#1A1E90", linewidth = 3) +
  
  # Line where Sharpe falls below minimum volatility's Sharpe
  geom_vline(xintercept = results$Overlay_Size[397], linetype = "dotdash", color = "#1E901A", linewidth = 3) +
  

  # Annotation for Minimum Volatility
  annotate("text", x = min_volatility$Overlay_Size, y = 0.1, 
           label = paste("Min Volatility:", 
                         "Overlay Size:", round(min_volatility$Overlay_Size, 3), 
                         "\nReturn:", round(min_volatility$Expected_Return * 10000, 5), "bps",  
                         "\nRisk:", round(min_volatility$Risk_Annualized * 100, 4), "%"), 
           color = "#1A1E90", size = 13, vjust = -0.5, hjust = 1.1) +
  
  # Annotation for overlay size, expected return, and risk for max Sharpe
  annotate("text", x = max_sharpe$Overlay_Size, y = 0.12, 
           label = paste("Overlay Size:", round(max_sharpe$Overlay_Size * 100, 1), "%",
                         "\nReturn:", round(max_sharpe$Expected_Return * 10000, 5), "bps",  
                         "\nRisk:", round(max_sharpe$Risk_Annualized * 100, 4), "%"), 
           color = "#901a1E", size = 13, hjust = 1.1, vjust = 1.3) +
  
  # Annotation for the Sharpe Ratio falling below min volatility Sharpe at Overlay_Size 0.397
  annotate("text", x = results$Overlay_Size[397], y = results$Sharpe_Ratio[397] - 0.02, 
           label = paste("Sharpe Ratio dips below Min Volatility's Sharpe:\n",
                         "Overlay Size:", round(results$Overlay_Size[397], 3),
                         "\nReturn:", round(results$Expected_Return[397] * 10000, 5), "bps", 
                         "\nRisk:", round(results$Risk_Annualized[397] * 100, 4), "%"), 
           color = "#1E901A", size = 13, vjust = -1, hjust = -0.1) +
  
  ggtitle("Sharpe Ratio vs. Overlay Size") +
  xlab("Overlay Size") +
  ylab("Sharpe Ratio") +
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 27),  
    axis.title.x = element_text(size = 23),  
    axis.title.y = element_text(size = 23),  
    axis.text.x = element_text(size = 17),   
    axis.text.y = element_text(size = 17)
  )



# Print key results
cat("Overlay Size with Least Volatility:\n")
print(min_volatility)

cat("\nOverlay Size with Most Sharpe Ratio:\n")
print(results[max_sharpe_idx, ])

# Sharpe ratio at minimum volatility overlay size
min_volatility_sharpe_ratio <- results$Sharpe_Ratio[min_volatility_idx]

# Find index where Sharpe ratio first falls below this threshold
falling_sharpe_idx <- which(results$Sharpe_Ratio[min_volatility_idx:length(sharpe_ratios)] < min_volatility_sharpe_ratio)[1] + min_volatility_idx - 1

cat("Index where Sharpe ratio first falls below the Sharpe ratio at minimum volatility:", falling_sharpe_idx, "\n")
cat("Overlay Size:", results$Overlay_Size[falling_sharpe_idx], "\n")
cat("Sharpe Ratio at that point:", results$Sharpe_Ratio[falling_sharpe_idx], "\n")


