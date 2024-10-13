####################################################################################################################
####################################################################################################################
#------------------------------- Current Investment Universe -------------------------------------------------------
####################################################################################################################
####################################################################################################################
set.seed(123)
library(quadprog)
# RF and bonds from Data.R

## Split into subsets of subsets and cut timeline of the CSV file 6_Portfolios_ME_Prior_12_2.CSV
# Average Value Weighted Returns -- Monthly
MOMexp_Average_Value_Weighted_Returns_Monthly <- MOMexp_Average_Value_Weighted_Returns_Monthly[757:1164,]
# Average Equal Weighted Returns -- Monthly
MOMexp_Average_Equal_Weighted_Returns_Monthly <- MOMexp_Average_Equal_Weighted_Returns_Monthly[757:1164,]
# Average Value Weighted Returns -- Annual
MOMexp_Average_Value_Weighted_Returns_Annual <- MOMexp_Average_Value_Weighted_Returns_Annual[64:97,]
# Average Equal Weighted Returns -- Annual
MOMexp_Average_Equal_Weighted_Returns_Annual <- MOMexp_Average_Equal_Weighted_Returns_Annual[64:97,]
# Number of Firms in Portfolios
MOMexp_Number_of_Firms_in_Portfolios <- MOMexp_Number_of_Firms_in_Portfolios[757:1164,]
# Average Firm Size
MOMexp_Average_Firm_Size <- MOMexp_Average_Firm_Size[757:1164,]
# Equally-Weighted Average of Prior Returns
MOMexp_Equally_Weighted_Average_of_Prior_Returns <- MOMexp_Equally_Weighted_Average_of_Prior_Returns[757:1164,]
# Value-Weighted Average of Prior Returns
MOMexp_Value_Weighted_Average_of_Prior_Returns <- MOMexp_Value_Weighted_Average_of_Prior_Returns[757:1164,]



# Remove the first column (assumed to be character) and keep only numeric columns
num_firms <- MOMexp_Number_of_Firms_in_Portfolios[, -1]
avg_firm_size <- MOMexp_Average_Firm_Size[, -1]
avg_value_weighted_returns <- MOMexp_Average_Value_Weighted_Returns_Monthly[, -1]

# Calculate the portfolio weights for all portfolios in one step
portfolio_weights <- num_firms * avg_firm_size

# Calculate total weight by summing all portfolio weights
total_weight <- rowSums(portfolio_weights)

# Calculate the market return using vectorized operations
market_return <- rowSums(avg_value_weighted_returns * portfolio_weights) / total_weight
market_return 


# Calculate portfolio expected return
w_S <- 0.60  # Weight in stocks
w_B <- 0.40  # Weight in bonds

# Expected return of portfolio
E_R_S <- mean(market_return)/100  # Average stock return
E_R_B <- mean(Bonds)  # Average bond return

E_R_p <- w_S * E_R_S + w_B * E_R_B

# Calculate volatility of stocks and bonds
sigma_S <- sd(market_return/100)  # Volatility of stocks
sigma_B <- sd(Bonds)  # Volatility of bonds

# Covariance between stocks and bonds
covariance <- cov((market_return/100), Bonds, use = "pairwise.complete.obs")

# Calculate portfolio volatility
sigma_p <- sqrt(w_S^2 * sigma_S^2 + w_B^2 * sigma_B^2 + 2 * w_S * w_B * covariance)

# Check if portfolio meets return target
target_return <- 0.0075  # 75 bps
meets_target <- E_R_p >= target_return

# Print results
cat("Expected Return of Portfolio 60/40:", E_R_p, "\n") #0.007369112
cat("Portfolio 60/40 Volatility :", sigma_p, "\n") #0.02808134
cat("Meets Return Target:", meets_target, "\n") #Does not meet target return


####################################################################################################################
####################################################################################################################
#--------------------------------- Efficient Frontier --------------------------------------------------------------
####################################################################################################################
####################################################################################################################

# Number of portfolios to simulate
n_portfolios <- 100

# Create sequences of weights for stocks and bonds
w_S <- seq(0, 1, length.out = n_portfolios)
w_B <- 1 - w_S  # Complementary weight for bonds


# Initialize vectors to store results
port_returns <- numeric(n_portfolios)
port_volatilities <- numeric(n_portfolios)

# Loop through different portfolio weights
for (i in 1:n_portfolios) {
  # Portfolio expected return
  port_returns[i] <- w_S[i] * E_R_S + w_B[i] * E_R_B
  
  # Portfolio volatility
  port_volatilities[i] <- sqrt(w_S[i]^2 * sigma_S^2 + 
                                 w_B[i]^2 * sigma_B^2 + 
                                 2 * w_S[i] * w_B[i] * covariance)
}

# Find the Global Minimum Variance Portfolio (GMV)
min_variance_idx <- which.min(port_volatilities)

# Extract the return and volatility of the GMV portfolio
gmv_return <- port_returns[min_variance_idx]
gmv_volatility <- port_volatilities[min_variance_idx]

# Split portfolios into efficient (above GMV) and inefficient (below GMV)
efficient_indices <- which(port_returns >= gmv_return)
inefficient_indices <- which(port_returns < gmv_return)

# Plot the inefficient frontier (below GMV) with a dashed line
plot(port_volatilities[inefficient_indices], port_returns[inefficient_indices], type = "l", col = "blue", lwd = 2, lty = 2,
     ylim = c(min(port_returns), max(port_returns)),
     xlim = c(min(port_volatilities), max(port_volatilities)),
     xlab = "Portfolio Volatility", ylab = "Portfolio Expected Return",
     main = "Efficient Frontier with Global Minimum Variance Portfolio")

# Add the efficient frontier (above GMV) with a solid line
lines(port_volatilities[efficient_indices], port_returns[efficient_indices], col = "blue", lwd = 2)

# Highlight the Global Minimum Variance Portfolio (GMV)
points(gmv_volatility, gmv_return, col = "green", pch = 16, cex = 1.5)
text(gmv_volatility, gmv_return, labels = "GMV Portfolio", pos = 4, col = "green")

# Highlight the 60/40 portfolio
points(sigma_p, E_R_p, col = "red", pch = 8, cex = 1.5)
text(sigma_p, E_R_p, labels = "60/40 Strategy", pos = 1, col = "red")

# Add grid for better visualization
grid()

# Print the GMV portfolio details
cat("Global Minimum Variance Portfolio:\n")
cat("Expected Return:", gmv_return, "\n") #0.005440476
cat("Portfolio Volatility:", gmv_volatility, "\n") #0.02005593




####################################################################################################################
####################################################################################################################
#---------------------------------------- C ------------------------------------------------------------------------
####################################################################################################################
####################################################################################################################
# Part 1: with leverage/shorting
# Define expected returns and covariances
E_R_S <- mean(market_return) / 100  # Expected return for stocks
E_R_B <- mean(Bonds, na.rm = TRUE)  # Expected return for bonds
sigma_S <- sd(market_return, na.rm = TRUE) / 100  # Volatility of stocks
sigma_B <- sd(Bonds, na.rm = TRUE)  # Volatility of bonds
covariance <- cov(market_return / 100, Bonds, use = "pairwise.complete.obs")

# Define covar matrix for the assets
cov_matrix <- matrix(c(sigma_S^2, covariance, covariance, sigma_B^2), nrow = 2)

# Define expected returns vector
expected_returns <- c(E_R_S, E_R_B)

# Minimize portfolio variance/risk s.t. target return = target_return
Dmat <- cov_matrix  # Covariance matrix
dvec <- rep(0, 2)  # Zero vector since we're minimizing risk
Amat <- cbind(1, expected_returns)  # Constraints matrix -> 1 for sum of weights, expected returns
bvec <- c(1, target_return)  # Constraints vector: weights sum to 1, return = target_return
meq <- 0  # No equality constraint on sum of weights (allows leverage or shorting)

# Solve quadratic programming problem with no leverage/shorting restrictions
result_leverage <- solve.QP(Dmat, dvec, Amat, bvec, meq)

# Extract portfolio weights
optimal_weights_leverage <- result_leverage$solution

# Calculate optimal portfolio return and risk (volatility)
optimal_return_leverage <- sum(optimal_weights_leverage * expected_returns)
optimal_volatility_leverage <- sqrt(t(optimal_weights_leverage) %*% cov_matrix %*% optimal_weights_leverage)

# Print results
cat("Optimal Weights for Stocks (with leverage):", optimal_weights_leverage[1], "\n")
cat("Optimal Weights for Bonds (with leverage):", optimal_weights_leverage[2], "\n")
cat("Optimal Portfolio Return (fixed at 75 bps):", optimal_return_leverage, "\n")
cat("Optimal Portfolio Volatility (minimized with leverage):", optimal_volatility_leverage, "\n")

#Part 2: without leverage/borrowing
# Define expected returns and covariances
E_R_S <- mean(market_return) / 100  # Expected return for stocks
E_R_B <- mean(Bonds, na.rm = TRUE)  # Expected return for bonds
sigma_S <- sd(market_return, na.rm = TRUE) / 100  # Volatility of stocks
sigma_B <- sd(Bonds, na.rm = TRUE)  # Volatility of bonds
covariance <- cov(market_return / 100, Bonds, use = "pairwise.complete.obs")

# Define covar matrix for the assets
cov_matrix <- matrix(c(sigma_S^2, covariance, covariance, sigma_B^2), nrow = 2)

# Define expected returns vector
expected_returns <- c(E_R_S, E_R_B)


# Minimize portfolio variance/risk s.t. to expected return = target_return
Dmat <- cov_matrix  # Covar Matrix
dvec <- rep(0, 2)  # Zero vector since we're minimizing risk
Amat <- cbind(1, expected_returns)  # Constraints matrix -> 1 for sum of weights, expected returns
bvec <- c(1, target_return)  # Constraints vector: weights sum to 1, return = target_return
meq <- 1  # First constraint is equality (weights sum to 1)

# Solve quadratic programming problem
result <- solve.QP(Dmat, dvec, Amat, bvec, meq)

# Extract portfolio weights
optimal_weights <- result$solution

# Calculate optimal portfolio return and risk (volatility)
optimal_return <- sum(optimal_weights * expected_returns)
optimal_volatility <- sqrt(t(optimal_weights) %*% cov_matrix %*% optimal_weights)

# pritn results
cat("Optimal Weights for Stocks:", optimal_weights[1], "\n") #0.6270092
cat("Optimal Weights for Bonds:", optimal_weights[2], "\n") #0.3729908
cat("Optimal Portfolio Return:", optimal_return, "\n") #0.0075 by construction
cat("Optimal Portfolio Volatility:", optimal_volatility, "\n") #0.02902416


####################################################################################################################
####################################################################################################################
#---------------------------------------- D ------------------------------------------------------------------------
####################################################################################################################
####################################################################################################################
# Load necessary library for optimization
library(quadprog)

# Define expected returns and covariances
E_R_S <- mean(market_return) / 100  # Expected return for stocks
E_R_B <- mean(Bonds, na.rm = TRUE)  # Expected return for bonds
sigma_S <- sd(market_return, na.rm = TRUE) / 100  # Volatility of stocks
sigma_B <- sd(Bonds, na.rm = TRUE)  # Volatility of bonds
covariance <- cov(market_return / 100, Bonds, use = "pairwise.complete.obs")

# Define the covariance matrix for the assets
cov_matrix <- matrix(c(sigma_S^2, covariance, covariance, sigma_B^2), nrow = 2)

# expected return vectors
expected_returns <- c(E_R_S, E_R_B)

# target
target_return <- 0.0075

# 1.5 leverage constraint
Amat <- cbind(1, expected_returns)  # Constraints matrix as 1 for sum of weights, expected returns
bvec_leverage <- c(1.5, target_return)  # total weights sum to 1.5 and return equals 75 bps
meq_leverage <- 2  # Both constraints are equality constraints

# solver to the quadratic programming problem
result_leverage_fixed <- solve.QP(Dmat, dvec, Amat, bvec_leverage, meq_leverage)

# extract portfolio weights with leverage
optimal_weights_leverage_fixed <- result_leverage_fixed$solution

# get optimal portfolio return and risk with leverage
optimal_return_leverage_fixed <- sum(optimal_weights_leverage_fixed * expected_returns)
optimal_volatility_leverage_fixed <- sqrt(t(optimal_weights_leverage_fixed) %*% cov_matrix %*% optimal_weights_leverage_fixed)

# Print the corrected optimal allocation, return, and risk with leverage
cat("Corrected Optimal Weights for Stocks 1.5 leverage:", optimal_weights_leverage_fixed[1], "\n") #0.1666901 
cat("Corrected Optimal Weights for Bonds (with leverage):", optimal_weights_leverage_fixed[2], "\n") #1.33331
cat("Corrected Portfolio Return (fixed at 75 bps):", optimal_return_leverage_fixed, "\n") #0.0075 
cat("Corrected Optimal Portfolio Volatility (minimized with leverage):", optimal_volatility_leverage_fixed, "\n") #0.03077518 

####################################################################################################################
####################################################################################################################
#---------------------------------------- E ------------------------------------------------------------------------
####################################################################################################################
####################################################################################################################
# This is fking bs

# Define the covariance matrix and expected returns
E_R_S <- mean(market_return) / 100  # Expected return for stocks
E_R_B <- mean(Bonds, na.rm = TRUE)  # Expected return for bonds
sigma_S <- sd(market_return, na.rm = TRUE) / 100  # Volatility of stocks
sigma_B <- sd(Bonds, na.rm = TRUE)  # Volatility of bonds
covariance <- cov(market_return / 100, Bonds, use = "pairwise.complete.obs")

# Covariance matrix
Sigma <- matrix(c(sigma_S^2, covariance, covariance, sigma_B^2), nrow = 2)

# Portfolio volatility function
portfolio_volatility <- function(w, Sigma) {
  sqrt(t(w) %*% Sigma %*% w)
}

# Risk contribution function
risk_contributions <- function(w, Sigma) {
  Sigma_w <- Sigma %*% w
  w * Sigma_w / portfolio_volatility(w, Sigma)
}

# Objective function to minimize squared differences in risk contributions
risk_parity_objective <- function(w, Sigma) {
  RC <- risk_contributions(w, Sigma)
  sum((RC - mean(RC))^2)  # Minimize the squared difference in risk contributions
}

# Leverage constraint function: sum of weights should equal 1.5
leverage_constraint <- function(w) {
  sum(w) - 1.5
}

# Set up initial weights (equal weights) but leverage so 1.5
w0 <- c(0.75, 0.75)

# Set optimization bounds
lower_bounds <- c(0, 0)
upper_bounds <- c(1.5, 1.5)

# Solve the optimization problem using nloptr
opt <- nloptr(
  x0 = w0,
  eval_f = function(w) risk_parity_objective(w, Sigma),
  lb = lower_bounds,
  ub = upper_bounds,
  eval_g_eq = leverage_constraint,  # Enforce leverage constraint
  opts = list("algorithm" = "NLOPT_LD_SLSQP", "xtol_rel" = 1e-6)
)

# Optimal weights extraction
optimal_weights <- opt$solution
optimal_weights


####################################################################################################################
####################################################################################################################
#-------------------------- Efficient Frontier with all portfolios -------------------------------------------------
####################################################################################################################
####################################################################################################################

# Plot the inefficient frontier (below GMV) with a dashed line
plot(port_volatilities[inefficient_indices], port_returns[inefficient_indices], type = "l", col = "blue", lwd = 2, lty = 2,
     ylim = c(min(port_returns), max(port_returns)),
     xlim = c(min(port_volatilities), max(port_volatilities)),
     xlab = "Portfolio Volatility", ylab = "Portfolio Expected Return",
     main = "Efficient Frontier with Global Minimum Variance Portfolio")

# Add the efficient frontier (above GMV) with a solid line
lines(port_volatilities[efficient_indices], port_returns[efficient_indices], col = "blue", lwd = 2)

# Highlight the Global Minimum Variance Portfolio (GMV)
points(gmv_volatility, gmv_return, col = "purple", pch = 16, cex = 1.5)
text(gmv_volatility, gmv_return, labels = "GMV Portfolio", pos = 4, col = "purple")

# Highlight the 60/40 portfolio
points(sigma_p, E_R_p, col = "blue", pch = 8, cex = 1.5)
text(sigma_p, E_R_p, labels = "60/40 Strategy", pos = 1, col = "blue")

# Highlight the Optimal portfolio
points(optimal_volatility, optimal_return, col = "red", pch = 8, cex = 1.5)
text(optimal_volatility, optimal_return, labels = "Mean-Variance Efficient", pos = 3, col = "red")

# Highlight the 60/40 portfolio
points(optimal_volatility_leverage, optimal_return_leverage, col = "green", pch = 8, cex = 1.5)
text(optimal_volatility_leverage, optimal_return_leverage, labels = "Mean-Variance Efficient + Leverage", pos = 3, col = "green")

# Add grid for better visualization
grid()









####################################################################################################################
####################################################################################################################
#---------------------------------------- F ------------------------------------------------------------------------
####################################################################################################################
####################################################################################################################

FullPeriod <- data.frame(
  "Date" = FFdata_Monthly_Factors$Date[757:1164],
  "RF" = FFdata_Monthly_Factors$RF[757:1164],
  "Bonds" = Bonds,
  "Equity" = market_return/100)
row.names(FullPeriod) <- NULL
FullPeriod$Date <- as.Date(paste0(FullPeriod$Date, "01"), format = "%Y%m%d")

### 199001 - 200112
Roaring90s <- subset(FullPeriod,
                     Date >= as.Date("1990-01-01") & Date <= as.Date("2001-12-01"))
row.names(Roaring90s) <- NULL
Roaring90s$Date <- format(Roaring90s$Date, "%Y-%m")

### 200201 - 200912
FinancialCrisis <- subset(FullPeriod,
                          Date >= as.Date("2002-01-01") & Date <= as.Date("2009-12-01"))
row.names(FinancialCrisis) <- NULL
FinancialCrisis$Date <- format(FinancialCrisis$Date, "%Y-%m")

### 201001 - 201912
GreatRecessionRecovery <- subset(FullPeriod,
                                 Date >= as.Date("2010-01-01") & Date <= as.Date("2019-12-01"))
row.names(GreatRecessionRecovery) <- NULL
GreatRecessionRecovery$Date <- format(GreatRecessionRecovery$Date, "%Y-%m")

### 202001 - 202312
Covid <- subset(FullPeriod,
                Date >= as.Date("2020-01-01") & Date <= as.Date("2023-12-01"))
row.names(Covid) <- NULL
Covid$Date <- format(Covid$Date, "%Y-%m")

### 199001 - 202312
FullPeriod$Date <- format(FullPeriod$Date, "%Y-%m")


###################################### Full Period Plot ########################################

# Convert Date to factor to keep it as "YYYY-MM"
FullPeriod$Date <- factor(FullPeriod$Date)
# Get labels for every 5th year (e.g., "1990-01", "1995-01", etc.)
FullPeriodBreaks <- FullPeriod$Date[seq(1, length(FullPeriod$Date), by = 12 * 5)]  # every 5 years

# Backtest: 60% in RF and 40% in Bonds
FullPeriod <- FullPeriod %>%
  mutate(Strategy_60_40 = 0.6 * Equity + 0.4 * Bonds,
         Cum_Return_60_40 = cumprod(1 + Strategy_60_40),
         Log_Cum_Return_60_40 = log(Cum_Return_60_40),
         Strategy_Optimal = optimal_weights[1] * Equity + optimal_weights[2] * Bonds,
         Cum_Return_Optimal = cumprod(1 + Strategy_Optimal),
         Log_Cum_Return_Optimal = log(Cum_Return_Optimal),
         Strategy_Optimal_Leverage = optimal_weights_leverage[1] * Equity + optimal_weights_leverage[2] * Bonds,
         Cum_Return_Optimal_Leverage = cumprod(1 + Strategy_Optimal_Leverage),
         Log_Cum_Return_Optimal_Leverage = log(Cum_Return_Optimal_Leverage))


# Plot the cumulative return with manually specified breaks
ggplot(FullPeriod, aes(x = Date)) +
  ### 60/40 Stragey
  geom_line(aes(y = Log_Cum_Return_60_40, group = 1), color = "blue") +
  geom_point(aes(y = Log_Cum_Return_60_40), color = "blue") +
  ### Strategy from C
  geom_line(aes(y = Log_Cum_Return_Optimal, group = 2), color = "red") +
  geom_point(aes(y = Log_Cum_Return_Optimal), color = "red") +
  ### Strategy from D
  geom_line(aes(y = Log_Cum_Return_Optimal_Leverage, group = 2), color = "green") +
  geom_point(aes(y = Log_Cum_Return_Optimal_Leverage), color = "green") +
  ggtitle("Cumulative Return of Strategies (Log Scale)") +
  xlab("Date") + ylab("Log Cumulative Return") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks = FullPeriodBreaks) +  # Use your predefined breaks
  annotate(geom="text", x="2021-07", y=2.3, label=paste("60/40 Strategy"),
           color="blue", size = 7) +
  annotate(geom="text", x="2015-01", y=1.7, label=paste("Mean-Variance Efficient"),
           color="red", size = 7) +
  annotate(geom="text", x="2015-01", y=3, label=paste("Mean-Variance Efficient + Leverage"),
           color="green", size = 7)




###################################### Roaring 90s Plot ########################################

# Convert Date to factor to keep it as "YYYY-MM"
Roaring90s$Date <- factor(Roaring90s$Date)
# Get labels for every 5th year (e.g., "1990-01", "1995-01", etc.)
Roaring90sBreaks <- Roaring90s$Date[seq(1, length(Roaring90s$Date), by = 12 * 1)]  # every 5 years

# Backtest: 60% in RF and 40% in Bonds
Roaring90s <- Roaring90s %>%
  mutate(Strategy_60_40 = 0.6 * Equity + 0.4 * Bonds,
         Cum_Return_60_40 = cumprod(1 + Strategy_60_40),
         Log_Cum_Return_60_40 = log(Cum_Return_60_40),
         Strategy_Optimal = optimal_weights[1] * Equity + optimal_weights[2] * Bonds,
         Cum_Return_Optimal = cumprod(1 + Strategy_Optimal),
         Log_Cum_Return_Optimal = log(Cum_Return_Optimal),
         Strategy_Optimal_Leverage = optimal_weights_leverage[1] * Equity + optimal_weights_leverage[2] * Bonds,
         Cum_Return_Optimal_Leverage = cumprod(1 + Strategy_Optimal_Leverage),
         Log_Cum_Return_Optimal_Leverage = log(Cum_Return_Optimal_Leverage))


# Plot the cumulative return with manually specified breaks
ggplot(Roaring90s, aes(x = Date)) +
  ### 60/40 Stragey
  geom_line(aes(y = Log_Cum_Return_60_40, group = 1), color = "blue") +
  geom_point(aes(y = Log_Cum_Return_60_40), color = "blue") +
  ### Strategy from C
  geom_line(aes(y = Log_Cum_Return_Optimal, group = 2), color = "red") +
  geom_point(aes(y = Log_Cum_Return_Optimal), color = "red") +
  ### Strategy from D
  geom_line(aes(y = Log_Cum_Return_Optimal_Leverage, group = 2), color = "green") +
  geom_point(aes(y = Log_Cum_Return_Optimal_Leverage), color = "green") +
  ggtitle("Cumulative Return of Strategies (Log Scale)") +
  xlab("Date") + ylab("Log Cumulative Return") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks = Roaring90sBreaks) +  # Use your predefined breaks
  annotate(geom="text", x="2000-01", y=1, label=paste("60/40 Strategy"),
           color="blue", size = 7) +
  annotate(geom="text", x="1997-01", y=0.7, label=paste("Mean-Variance Efficient"),
           color="red", size = 7) +
  annotate(geom="text", x="1997-01", y=1.5, label=paste("Mean-Variance Efficient + Leverage"),
           color="green", size = 7)











###################################### Financial Crisis Plot ########################################

# Convert Date to factor to keep it as "YYYY-MM"
FinancialCrisis$Date <- factor(FinancialCrisis$Date)
# Get labels for every 5th year (e.g., "1990-01", "1995-01", etc.)
FinancialCrisisBreaks <- FinancialCrisis$Date[seq(1, length(FinancialCrisis$Date), by = 12 * 1)]  # every 5 years

# Backtest: 60% in RF and 40% in Bonds
FinancialCrisis <- FinancialCrisis %>%
  mutate(Strategy_60_40 = 0.6 * Equity + 0.4 * Bonds,
         Cum_Return_60_40 = cumprod(1 + Strategy_60_40),
         Log_Cum_Return_60_40 = log(Cum_Return_60_40),
         Strategy_Optimal = optimal_weights[1] * Equity + optimal_weights[2] * Bonds,
         Cum_Return_Optimal = cumprod(1 + Strategy_Optimal),
         Log_Cum_Return_Optimal = log(Cum_Return_Optimal),
         Strategy_Optimal_Leverage = optimal_weights_leverage[1] * Equity + optimal_weights_leverage[2] * Bonds,
         Cum_Return_Optimal_Leverage = cumprod(1 + Strategy_Optimal_Leverage),
         Log_Cum_Return_Optimal_Leverage = log(Cum_Return_Optimal_Leverage))


# Plot the cumulative return with manually specified breaks
ggplot(FinancialCrisis, aes(x = Date)) +
  ### 60/40 Stragey
  geom_line(aes(y = Log_Cum_Return_60_40, group = 1), color = "blue") +
  geom_point(aes(y = Log_Cum_Return_60_40), color = "blue") +
  ### Strategy from C
  geom_line(aes(y = Log_Cum_Return_Optimal, group = 2), color = "red") +
  geom_point(aes(y = Log_Cum_Return_Optimal), color = "red") +
  ### Strategy from D
  geom_line(aes(y = Log_Cum_Return_Optimal_Leverage, group = 2), color = "green") +
  geom_point(aes(y = Log_Cum_Return_Optimal_Leverage), color = "green") +
  ggtitle("Cumulative Return of Strategies (Log Scale)") +
  xlab("Date") + ylab("Log Cumulative Return") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks = FinancialCrisisBreaks) +  # Use your predefined breaks
  annotate(geom="text", x="2006-01", y=0.15, label=paste("60/40 Strategy"),
           color="blue", size = 7) +
  annotate(geom="text", x="2007-01", y=0.25, label=paste("Mean-Variance Efficient"),
           color="red", size = 7) +
  annotate(geom="text", x="2008-01", y=0.6, label=paste("Mean-Variance Efficient + Leverage"),
           color="green", size = 7)







###################################### Great Recession Recovery Plot ########################################

# Convert Date to factor to keep it as "YYYY-MM"
GreatRecessionRecovery$Date <- factor(GreatRecessionRecovery$Date)
# Get labels for every 5th year (e.g., "1990-01", "1995-01", etc.)
GreatRecessionRecoveryBreaks <- GreatRecessionRecovery$Date[seq(1, length(GreatRecessionRecovery$Date), by = 12 * 1)]  # every 5 years

# Backtest: 60% in RF and 40% in Bonds
GreatRecessionRecovery <- GreatRecessionRecovery %>%
  mutate(Strategy_60_40 = 0.6 * Equity + 0.4 * Bonds,
         Cum_Return_60_40 = cumprod(1 + Strategy_60_40),
         Log_Cum_Return_60_40 = log(Cum_Return_60_40),
         Strategy_Optimal = optimal_weights[1] * Equity + optimal_weights[2] * Bonds,
         Cum_Return_Optimal = cumprod(1 + Strategy_Optimal),
         Log_Cum_Return_Optimal = log(Cum_Return_Optimal),
         Strategy_Optimal_Leverage = optimal_weights_leverage[1] * Equity + optimal_weights_leverage[2] * Bonds,
         Cum_Return_Optimal_Leverage = cumprod(1 + Strategy_Optimal_Leverage),
         Log_Cum_Return_Optimal_Leverage = log(Cum_Return_Optimal_Leverage))


# Plot the cumulative return with manually specified breaks
ggplot(GreatRecessionRecovery, aes(x = Date)) +
  ### 60/40 Stragey
  geom_line(aes(y = Log_Cum_Return_60_40, group = 1), color = "blue") +
  geom_point(aes(y = Log_Cum_Return_60_40), color = "blue") +
  ### Strategy from C
  geom_line(aes(y = Log_Cum_Return_Optimal, group = 2), color = "red") +
  geom_point(aes(y = Log_Cum_Return_Optimal), color = "red") +
  ### Strategy from D
  geom_line(aes(y = Log_Cum_Return_Optimal_Leverage, group = 2), color = "green") +
  geom_point(aes(y = Log_Cum_Return_Optimal_Leverage), color = "green") +
  ggtitle("Cumulative Return of Strategies (Log Scale)") +
  xlab("Date") + ylab("Log Cumulative Return") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks = GreatRecessionRecoveryBreaks) +  # Use your predefined breaks
  annotate(geom="text", x="2017-01", y=0.7, label=paste("60/40 Strategy"),
           color="blue", size = 7) +
  annotate(geom="text", x="2016-07", y=0.9, label=paste("Mean-Variance Efficient"),
           color="red", size = 7) +
  annotate(geom="text", x="2018-01", y=1.3, label=paste("Mean-Variance Efficient + Leverage"),
           color="green", size = 7)







###################################### Covid Plot ########################################

# Convert Date to factor to keep it as "YYYY-MM"
Covid$Date <- factor(Covid$Date)
# Get labels for every 5th year (e.g., "1990-01", "1995-01", etc.)
CovidBreaks <- Covid$Date[seq(1, length(Covid$Date), by = 12 * 1)]  # every 5 years

# Backtest: 60% in RF and 40% in Bonds
Covid <- Covid %>%
  mutate(Strategy_60_40 = 0.6 * Equity + 0.4 * Bonds,
         Cum_Return_60_40 = cumprod(1 + Strategy_60_40),
         Log_Cum_Return_60_40 = log(Cum_Return_60_40),
         Strategy_Optimal = optimal_weights[1] * Equity + optimal_weights[2] * Bonds,
         Cum_Return_Optimal = cumprod(1 + Strategy_Optimal),
         Log_Cum_Return_Optimal = log(Cum_Return_Optimal),
         Strategy_Optimal_Leverage = optimal_weights_leverage[1] * Equity + optimal_weights_leverage[2] * Bonds,
         Cum_Return_Optimal_Leverage = cumprod(1 + Strategy_Optimal_Leverage),
         Log_Cum_Return_Optimal_Leverage = log(Cum_Return_Optimal_Leverage))


# Plot the cumulative return with manually specified breaks
ggplot(Covid, aes(x = Date)) +
  ### 60/40 Stragey
  geom_line(aes(y = Log_Cum_Return_60_40, group = 1), color = "blue") +
  geom_point(aes(y = Log_Cum_Return_60_40), color = "blue") +
  ### Strategy from C
  geom_line(aes(y = Log_Cum_Return_Optimal, group = 2), color = "red") +
  geom_point(aes(y = Log_Cum_Return_Optimal), color = "red") +
  ### Strategy from D
  geom_line(aes(y = Log_Cum_Return_Optimal_Leverage, group = 2), color = "green") +
  geom_point(aes(y = Log_Cum_Return_Optimal_Leverage), color = "green") +
  ggtitle("Cumulative Return of Strategies (Log Scale)") +
  xlab("Date") + ylab("Log Cumulative Return") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks = CovidBreaks) +  # Use your predefined breaks
  annotate(geom="text", x="2021-10", y=0.3, label=paste("60/40 Strategy"),
           color="blue", size = 7) +
  annotate(geom="text", x="2022-09", y=0.2, label=paste("Mean-Variance Efficient"),
           color="red", size = 7) +
  annotate(geom="text", x="2022-07", y=0.5, label=paste("Mean-Variance Efficient + Leverage"),
           color="green", size = 7)



###################################### Covid Plot ########################################

# Convert Date to factor to keep it as "YYYY-MM"
Covid$Date <- factor(Covid$Date)
# Get labels for every 5th year (e.g., "1990-01", "1995-01", etc.)
CovidBreaks <- Covid$Date[seq(1, length(Covid$Date), by = 12 * 1)]  # every 5 years

# Backtest: 60% in RF and 40% in Bonds
Covid <- Covid %>%
  mutate(Strategy_60_40 = 0.6 * Equity + 0.4 * Bonds,
         Cum_Return_60_40 = cumprod(1 + Strategy_60_40),
         Log_Cum_Return_60_40 = log(Cum_Return_60_40),
         Strategy_Optimal = optimal_weights[1] * Equity + optimal_weights[2] * Bonds,
         Cum_Return_Optimal = cumprod(1 + Strategy_Optimal),
         Log_Cum_Return_Optimal = log(Cum_Return_Optimal),
         Strategy_Optimal_Leverage = optimal_weights_leverage[1] * Equity + optimal_weights_leverage[2] * Bonds,
         Cum_Return_Optimal_Leverage = cumprod(1 + Strategy_Optimal_Leverage),
         Log_Cum_Return_Optimal_Leverage = log(Cum_Return_Optimal_Leverage))


# Plot the cumulative return with manually specified breaks
ggplot(Covid, aes(x = Date)) +
  ### 60/40 Stragey
  geom_line(aes(y = Log_Cum_Return_60_40, group = 1), color = "blue") +
  geom_point(aes(y = Log_Cum_Return_60_40), color = "blue") +
  ### Strategy from C
  geom_line(aes(y = Log_Cum_Return_Optimal, group = 2), color = "red") +
  geom_point(aes(y = Log_Cum_Return_Optimal), color = "red") +
  ### Strategy from D
  geom_line(aes(y = Log_Cum_Return_Optimal_Leverage, group = 2), color = "green") +
  geom_point(aes(y = Log_Cum_Return_Optimal_Leverage), color = "green") +
  ggtitle("Cumulative Return of Strategies (Log Scale)") +
  xlab("Date") + ylab("Log Cumulative Return") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks = CovidBreaks) +  # Use your predefined breaks
  annotate(geom="text", x="2021-10", y=0.3, label=paste("60/40 Strategy"),
           color="blue", size = 7) +
  annotate(geom="text", x="2022-09", y=0.2, label=paste("Mean-Variance Efficient"),
           color="red", size = 7) +
  annotate(geom="text", x="2022-07", y=0.5, label=paste("Mean-Variance Efficient + Leverage"),
           color="green", size = 7)
