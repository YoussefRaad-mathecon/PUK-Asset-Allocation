####################################################################################################################
####################################################################################################################
#------------------------------- Current Investment Universe ------------------------------------------------------#
####################################################################################################################
####################################################################################################################
library(quadprog)
library(nloptr)

### Data
set.seed(123)
RF <- FFdata_Monthly_Factors$RF[757:1164] ### RF rate
RF <- RF / 100
Bonds <- Bonds$Monthly.Return.10.Yr # Bonds

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
market_return <- market_return / 100

# Calculate portfolio expected return
w_S <- 0.60  # Weight in stocks
w_B <- 0.40  # Weight in bonds

# Expected return of portfolio
E_R_S <- mean(market_return)  # Average stock return
E_R_B <- mean(Bonds)  # Average bond return
E_R_C <- mean(RF)  # Expected return for cash
E_R_p_6040 <- w_S * E_R_S + w_B * E_R_B

# Calculate volatility of stocks and bonds
sigma_S <- sd(market_return)  # Volatility of stocks
sigma_B <- sd(Bonds)  # Volatility of bonds
sigma_C <- sd(RF) # Volatility of cash

# Covariance between stocks and bonds
covariance <- cov((market_return/100), Bonds, use = "pairwise.complete.obs")

# Calculate portfolio volatility
sigma_p_6040 <- sqrt(w_S^2 * sigma_S^2 + w_B^2 * sigma_B^2 + 2 * w_S * w_B * covariance)

# Check if portfolio meets return target
target_return <- 0.0075  # 75 bps


# Print results
cat("60/40-portfolio expected return:", E_R_p_6040, "\n") # 0.006790775 < 0.0075 (does not meet target)
cat("60/40-portfolio volatility :", sigma_p_6040, "\n") #0.02777475 

####################################################################################################################
####################################################################################################################
#---------------------------------------- C ------------------------------------------------------------------------
####################################################################################################################
####################################################################################################################
# NO SHORTING/BORROWING!
# Define expected returns for stocks, bonds, and cash
E_R_C <- mean(RF)  # Expected return for cash
E_R_S <- mean(market_return) # Expected return for stocks
E_R_B <- mean(Bonds)  # Expected return for bonds
# Define volatilities
sigma_C <- sd(RF)   # Volatility of cash
sigma_S <- sd(market_return) # Volatility of stocks
sigma_B <- sd(Bonds)  # Volatility of bonds
# Calculate covariances
cov_S_B <- cov(market_return, Bonds, use = "pairwise.complete.obs")
cov_S_C <- cov(market_return, RF , use = "pairwise.complete.obs")
cov_B_C <- cov(Bonds, RF, use = "pairwise.complete.obs")

# Define covariance matrix 
cov_matrix <- matrix(c(sigma_S^2, cov_S_B, cov_S_C,
                       cov_S_B, sigma_B^2, cov_B_C,
                       cov_S_C, cov_B_C, sigma_C^2), nrow = 3)

# Define expected returns vector
expected_returns <- c(E_R_S, E_R_B, E_R_C)

# Target return
target_return <- 0.0075

# Define the number of assets
n_assets <- 3

# Expected returns vector 
expected_returns <- c(E_R_S, E_R_B, E_R_C)

# Set up Dmat and dvec for the quadratic programming solver
Dmat <- cov_matrix  # 2*Covariance matrix as per quadratic programming setup
dvec <- rep(0, n_assets)  # Zeros for the linear part of the quadratic objective

# Set up constraint matrix (Amat), right-hand side (bvec), and sense
Amat <- cbind(
  expected_returns,  # Return constraint
  rep(1, n_assets),  # Sum of weights constraint
  diag(n_assets)     # Identity matrix to enforce non-negative weights
)

# The first element in bvec is the target return, the second is 1 for the sum of weights
# The rest are zeros for non-negative weights
bvec <- c(target_return, 1, rep(0, n_assets))

# Solve the quadratic program (meq = 2 ensures the first two constraints are equality constraints)
result_MVO <- solve.QP(Dmat, dvec, Amat, bvec, meq = 2)

# Get the optimal weights
optimal_weights_MVO <- result_MVO$solution
portfolio_return_MVO <- sum(optimal_weights_MVO * expected_returns)
portfolio_volatility_MVO <- sqrt(t(optimal_weights_MVO) %*% cov_matrix %*% optimal_weights_MVO)
# Print the results
cat("MVO stocks weight: ", optimal_weights_MVO[1], "\n") # 0.7127203 
cat("MVO bonds weight: ", optimal_weights_MVO[2], "\n") # 0.2872797 
cat("MVO cash weight: ", optimal_weights_MVO[3], "\n") # 0
cat("MVO portfolio volatility: ", portfolio_volatility_MVO, "\n") # 0.03159363 
cat("MVO expected return: ", portfolio_return_MVO, "\n") # 0.0075 by construction

####################################################################################################################
####################################################################################################################
#---------------------------------------- D ------------------------------------------------------------------------
####################################################################################################################
####################################################################################################################
# Define expected returns and volatilities
E_R_C <- mean(RF)   # Expected return for cash
E_R_S <- mean(market_return)   # Expected return for stocks
E_R_B <- mean(Bonds)   # Expected return for bonds
sigma_C <- sd(RF)   # Volatility of cash
sigma_S <- sd(market_return)   # Volatility of stocks
sigma_B <- sd(Bonds)   # Volatility of bonds

# Calculate covariances
cov_S_B <- cov(market_return, Bonds, use = "pairwise.complete.obs")
cov_S_C <- cov(market_return, RF, use = "pairwise.complete.obs")
cov_B_C <- cov(Bonds, RF, use = "pairwise.complete.obs")

# Define the updated covariance matrix (3x3) including cash
cov_matrix <- matrix(c(sigma_S^2, cov_S_B, cov_S_C,
                       cov_S_B, sigma_B^2, cov_B_C,
                       cov_S_C, cov_B_C, sigma_C^2), nrow = 3)
print(cov_matrix)
# Define the expected returns vector for stocks, bonds, and cash
expected_returns <- c(E_R_S, E_R_B, E_R_C)

# Target return
target_return <- 0.0075

# Define the number of assets
n_assets <- 3


# Set up Dmat and dvec for the quadratic programming solver
Dmat <- cov_matrix  # 2*Covariance matrix as per quadratic programming setup
dvec <- rep(0, n_assets)  # Zeros for the linear part of the quadratic objective

# Set up constraint matrix (Amat), right-hand side (bvec), and sense
Amat <- cbind(
  expected_returns,  # Return constraint
  rep(1, n_assets),  # Sum of weights constraint (now allows leverage)
  diag(n_assets)     # Identity matrix to enforce non-negative weights
)

# The first element in bvec is the target return, the second is 1.5 to allow for leverage
# The rest are zeros for non-negative weights
bvec <- c(target_return, 1.5, rep(0, n_assets))

# Solve the quadratic program (meq = 2 ensures the first two constraints are equality constraints)
result_LMVO <- solve.QP(Dmat, dvec, Amat, bvec, meq = 2)

# Get the optimal weights
optimal_weights_LMVO <- result_LMVO$solution

# Calculate the portfolio's expected return and vol
portfolio_return_LMVO <- sum(optimal_weights_MVO * expected_returns)
portfolio_volatility_LMVO <- sqrt(t(optimal_weights_LMVO) %*% cov_matrix %*% optimal_weights_LMVO)
# Print the results
cat("LMVO stocks weight: ", optimal_weights_LMVO[1], "\n") # 0.5533951 
cat("LMVO bonds weight: ", optimal_weights_LMVO[2], "\n") # 0.3704708 
cat("LMVO cash weight: ", optimal_weights_LMVO[3], "\n") # 0.5761342
cat("LMVO portfolio volatility: ", portfolio_volatility_LMVO, "\n") # 0.02515146 
cat("LMVO expected return: ", portfolio_return_LMVO, "\n") # 0.0075 by construction

####################################################################################################################
####################################################################################################################
#---------------------------------------- E ------------------------------------------------------------------------
####################################################################################################################
####################################################################################################################
# Define expected returns and volatilities 
E_R_C <- mean(RF)   # Expected return for cash
E_R_S <- mean(market_return)   # Expected return for stocks
E_R_B <- mean(Bonds)   # Expected return for bonds
sigma_C <- sd(RF)   # Volatility of cash
sigma_S <- sd(market_return)   # Volatility of stocks
sigma_B <- sd(Bonds)   # Volatility of bonds

# Calculate covariances
cov_S_B <- cov(market_return, Bonds, use = "pairwise.complete.obs")
cov_S_C <- cov(market_return, RF, use = "pairwise.complete.obs")
cov_B_C <- cov(Bonds, RF, use = "pairwise.complete.obs")

cov_matrix <- matrix(c(sigma_S^2, cov_S_B, cov_S_C,
                       cov_S_B, sigma_B^2, cov_B_C,
                       cov_S_C, cov_B_C, sigma_C^2), nrow = 3)

# Expected returns
expected_returns <- c(E_R_S, E_R_B, E_R_C)

# Define the number of assets
n_assets <- 3

# Portfolio variance calculation
portfolio_variance <- function(x, cov_matrix) {
  return(as.numeric(t(x) %*% cov_matrix %*% x))
}

# Objective function to minimize (residual risk contributions)
objective_function <- function(x, cov_matrix) {
  port_variance <- portfolio_variance(x, cov_matrix)
  marginal_contributions <- cov_matrix %*% x
  total_risk <- sqrt(port_variance)
  risk_contributions <- x * marginal_contributions / total_risk
  
  f_val <- 0
  for (i in 1:n_assets) {
    for (j in 1:n_assets) {
      f_val <- f_val + (risk_contributions[i] - risk_contributions[j])^2
    }
  }
  
  return(f_val)
}

# Constraints: sum of weights equals 1 and portfolio return equals target return
equality_constraint <- function(x) {
  return(c(sum(x) - 1.5,  # Sum of weights constraint
           sum(x * expected_returns) - 0.0075))  # Target return constraint (75 bps)
}

# Optimization using nloptr (COBYLA method)
x0 <- rep(0.5, n_assets)  # Initial guess (equal weights)

# Lower and upper bounds for weights
lb <- rep(0, n_assets)
ub <- rep(1.5, n_assets)

# Optimization problem with added target return constraint
result_LRP <- nloptr(x0 = x0,
                 eval_f = function(x) objective_function(x, cov_matrix),
                 lb = lb,
                 ub = ub,
                 eval_g_eq = equality_constraint,
                 opts = list(algorithm = "NLOPT_LN_COBYLA",  # COBYLA: Gradient-free algorithm
                             xtol_rel = 1.0e-8,  # Tolerance
                             maxeval = 10000))  # Maximum number of iterations
optimal_weights_LRP <- result_LRP$solution

# Calculate the portfolio's expected return and vol
portfolio_return_LRP <- sum(optimal_weights_LRP * expected_returns)
portfolio_volatility_LRP <- sqrt(t(optimal_weights_LRP) %*% cov_matrix %*% optimal_weights_LRP)

# Print the results in the desired format
cat("LRP Stocks weight: ", optimal_weights_LRP[1], "\n") # 0.484716 
cat("LRP bonds weight: ", optimal_weights_LRP[2], "\n") # 0.9317936
cat("LRP cash weight: ", optimal_weights_LRP[3], "\n") # 0.08349038
cat("LRP portfolio volatility: ", portfolio_volatility_LRP, "\n") # 0.0280509
cat("LRP expected return: ", portfolio_return_LRP, "\n") # 0.0075 by construction


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
  mutate(Strategy_60_40 = 0.6 * Equity + 0.4 * Bonds, ### 60/40
         Cum_Return_60_40 = cumprod(1 + Strategy_60_40), ### 60/40
         Log_Cum_Return_60_40 = log(Cum_Return_60_40), ### 60/40
         Strategy_MVO_Unrestricted = optimal_weights_leverage[1] * Equity + optimal_weights_leverage[2] * Bonds, ### MVO unrestricted
         Cum_Return_MVO_Unrestricted = cumprod(1 + Strategy_MVO_Unrestricted), ### MVO unrestricted
         Log_Cum_Return_MVO_Unrestricted = log(Cum_Return_MVO_Unrestricted), ### MVO unrestricted
         Strategy_MVO = optimal_weights[1] * Equity + optimal_weights[2] * Bonds, ### MVO
         Cum_Return_MVO = cumprod(1 + Strategy_MVO), ### MVO
         Log_Cum_Return_MVO = log(Cum_Return_MVO), ### MVO
         Strategy_MVOL = optimal_weights_leverage_fixed[1] * Equity + optimal_weights_leverage_fixed[2] * Bonds, ### MVO+L
         Cum_Return_MVOL = cumprod(1 + Strategy_MVOL), ### MVO+L
         Log_Cum_Return_MVOL = log(Cum_Return_MVOL), ### MVO+L
         Strategy_LRP = optimal_weights_LRP[1] * Equity + optimal_weights_LRP[2] * Bonds, ### LRP
         Cum_Return_LRP = cumprod(1 + Strategy_LRP), ### LRP
         Log_Cum_Return_LRP = log(Cum_Return_LRP)) ### LRP


# Plot the cumulative return with manually specified breaks
ggplot(FullPeriod, aes(x = Date)) +
  ### 60/40
  geom_line(aes(y = Log_Cum_Return_60_40, group = 1), color = "blue") +
  geom_point(aes(y = Log_Cum_Return_60_40), color = "blue") +
  ### MVO (unrestricted)
  geom_line(aes(y = Log_Cum_Return_MVO_Unrestricted, group = 2), color = "pink") +
  geom_point(aes(y = Log_Cum_Return_MVO_Unrestricted), color = "pink") +
  ### MVO
  geom_line(aes(y = Log_Cum_Return_MVO, group = 2), color = "red") +
  geom_point(aes(y = Log_Cum_Return_MVO), color = "red") +
  ### MVO+L
  geom_line(aes(y = Log_Cum_Return_MVOL, group = 2), color = "green") +
  geom_point(aes(y = Log_Cum_Return_MVOL), color = "green") +
  ### LRP
  geom_line(aes(y = Log_Cum_Return_LRP, group = 2), color = "darkgreen") +
  geom_point(aes(y = Log_Cum_Return_LRP), color = "darkgreen") +
  ggtitle("Cumulative Return of Strategies (Log Scale)") +
  xlab("Date") + ylab("Log Cumulative Return") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks = FullPeriodBreaks) +  # Use your predefined breaks
  annotate(geom="text", x="2021-07", y=2.3, label=paste("60/40"),
           color="blue", size = 7) +
  annotate(geom="text", x="2015-01", y=1.7, label=paste("MVO"),
           color="red", size = 7) +
  annotate(geom="text", x="2015-01", y=1.7, label=paste("MVO (unrestricted)"),
           color="pink", size = 7) +
  annotate(geom="text", x="2015-01", y=1.7, label=paste("LRP"),
           color="darkgreen", size = 7) +
  annotate(geom="text", x="2015-01", y=3, label=paste("MVO+L"),
           color="green", size = 7)




###################################### Roaring 90s Plot ########################################

# Convert Date to factor to keep it as "YYYY-MM"
Roaring90s$Date <- factor(Roaring90s$Date)
# Get labels for every 5th year (e.g., "1990-01", "1995-01", etc.)
Roaring90sBreaks <- Roaring90s$Date[seq(1, length(Roaring90s$Date), by = 12 * 1)]  # every 5 years


# Backtest: 60% in RF and 40% in Bonds
Roaring90s <- Roaring90s %>%
  mutate(Strategy_60_40 = 0.6 * Equity + 0.4 * Bonds, ### 60/40
         Cum_Return_60_40 = cumprod(1 + Strategy_60_40), ### 60/40
         Log_Cum_Return_60_40 = log(Cum_Return_60_40), ### 60/40
         Strategy_MVO_Unrestricted = optimal_weights_leverage[1] * Equity + optimal_weights_leverage[2] * Bonds, ### MVO unrestricted
         Cum_Return_MVO_Unrestricted = cumprod(1 + Strategy_MVO_Unrestricted), ### MVO unrestricted
         Log_Cum_Return_MVO_Unrestricted = log(Cum_Return_MVO_Unrestricted), ### MVO unrestricted
         Strategy_MVO = optimal_weights[1] * Equity + optimal_weights[2] * Bonds, ### MVO
         Cum_Return_MVO = cumprod(1 + Strategy_MVO), ### MVO
         Log_Cum_Return_MVO = log(Cum_Return_MVO), ### MVO
         Strategy_MVOL = optimal_weights_leverage_fixed[1] * Equity + optimal_weights_leverage_fixed[2] * Bonds, ### MVO+L
         Cum_Return_MVOL = cumprod(1 + Strategy_MVOL), ### MVO+L
         Log_Cum_Return_MVOL = log(Cum_Return_MVOL), ### MVO+L
         Strategy_LRP = optimal_weights_LRP[1] * Equity + optimal_weights_LRP[2] * Bonds, ### LRP
         Cum_Return_LRP = cumprod(1 + Strategy_LRP), ### LRP
         Log_Cum_Return_LRP = log(Cum_Return_LRP)) ### LRP


# Plot the cumulative return with manually specified breaks
ggplot(Roaring90s, aes(x = Date)) +
  ### 60/40
  geom_line(aes(y = Log_Cum_Return_60_40, group = 1), color = "blue") +
  geom_point(aes(y = Log_Cum_Return_60_40), color = "blue") +
  ### MVO (unrestricted)
  geom_line(aes(y = Log_Cum_Return_MVO_Unrestricted, group = 2), color = "pink") +
  geom_point(aes(y = Log_Cum_Return_MVO_Unrestricted), color = "pink") +
  ### MVO
  geom_line(aes(y = Log_Cum_Return_MVO, group = 2), color = "red") +
  geom_point(aes(y = Log_Cum_Return_MVO), color = "red") +
  ### MVO+L
  geom_line(aes(y = Log_Cum_Return_MVOL, group = 2), color = "green") +
  geom_point(aes(y = Log_Cum_Return_MVOL), color = "green") +
  ### LRP
  geom_line(aes(y = Log_Cum_Return_LRP, group = 2), color = "darkgreen") +
  geom_point(aes(y = Log_Cum_Return_LRP), color = "darkgreen") +
  ggtitle("Cumulative Return of Strategies (Log Scale)") +
  xlab("Date") + ylab("Log Cumulative Return") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks = FullPeriodBreaks) +  # Use your predefined breaks
  annotate(geom="text", x="2000-01", y=1, label=paste("60/40"),
           color="blue", size = 7) +
  annotate(geom="text", x="1997-01", y=0.7, label=paste("MVO"),
           color="red", size = 7) +
  annotate(geom="text", x="1997-01", y=1.5, label=paste("MVO+L"),
           color="green", size = 7) +
  annotate(geom="text", x="1997-01", y=1.7, label=paste("MVO (unrestricted)"),
           color="pink", size = 7) +
  annotate(geom="text", x="1997-01", y=1.7, label=paste("LRP"),
           color="darkgreen", size = 7)










###################################### Financial Crisis Plot ########################################

# Convert Date to factor to keep it as "YYYY-MM"
FinancialCrisis$Date <- factor(FinancialCrisis$Date)
# Get labels for every 5th year (e.g., "1990-01", "1995-01", etc.)
FinancialCrisisBreaks <- FinancialCrisis$Date[seq(1, length(FinancialCrisis$Date), by = 12 * 1)]  # every 5 years

# Backtest: 60% in RF and 40% in Bonds
FinancialCrisis <- FinancialCrisis %>%
  mutate(Strategy_60_40 = 0.6 * Equity + 0.4 * Bonds, ### 60/40
         Cum_Return_60_40 = cumprod(1 + Strategy_60_40), ### 60/40
         Log_Cum_Return_60_40 = log(Cum_Return_60_40), ### 60/40
         Strategy_MVO_Unrestricted = optimal_weights_leverage[1] * Equity + optimal_weights_leverage[2] * Bonds, ### MVO unrestricted
         Cum_Return_MVO_Unrestricted = cumprod(1 + Strategy_MVO_Unrestricted), ### MVO unrestricted
         Log_Cum_Return_MVO_Unrestricted = log(Cum_Return_MVO_Unrestricted), ### MVO unrestricted
         Strategy_MVO = optimal_weights[1] * Equity + optimal_weights[2] * Bonds, ### MVO
         Cum_Return_MVO = cumprod(1 + Strategy_MVO), ### MVO
         Log_Cum_Return_MVO = log(Cum_Return_MVO), ### MVO
         Strategy_MVOL = optimal_weights_leverage_fixed[1] * Equity + optimal_weights_leverage_fixed[2] * Bonds, ### MVO+L
         Cum_Return_MVOL = cumprod(1 + Strategy_MVOL), ### MVO+L
         Log_Cum_Return_MVOL = log(Cum_Return_MVOL), ### MVO+L
         Strategy_LRP = optimal_weights_LRP[1] * Equity + optimal_weights_LRP[2] * Bonds, ### LRP
         Cum_Return_LRP = cumprod(1 + Strategy_LRP), ### LRP
         Log_Cum_Return_LRP = log(Cum_Return_LRP)) ### LRP


# Plot the cumulative return with manually specified breaks
ggplot(FinancialCrisis, aes(x = Date)) +
  ### 60/40
  geom_line(aes(y = Log_Cum_Return_60_40, group = 1), color = "blue") +
  geom_point(aes(y = Log_Cum_Return_60_40), color = "blue") +
  ### MVO (unrestricted)
  geom_line(aes(y = Log_Cum_Return_MVO_Unrestricted, group = 2), color = "pink") +
  geom_point(aes(y = Log_Cum_Return_MVO_Unrestricted), color = "pink") +
  ### MVO
  geom_line(aes(y = Log_Cum_Return_MVO, group = 2), color = "red") +
  geom_point(aes(y = Log_Cum_Return_MVO), color = "red") +
  ### MVO+L
  geom_line(aes(y = Log_Cum_Return_MVOL, group = 2), color = "green") +
  geom_point(aes(y = Log_Cum_Return_MVOL), color = "green") +
  ### LRP
  geom_line(aes(y = Log_Cum_Return_LRP, group = 2), color = "darkgreen") +
  geom_point(aes(y = Log_Cum_Return_LRP), color = "darkgreen") +
  ggtitle("Cumulative Return of Strategies (Log Scale)") +
  xlab("Date") + ylab("Log Cumulative Return") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks = FullPeriodBreaks) +  # Use your predefined breaks
  annotate(geom="text", x="2006-01", y=1, label=paste("60/40"),
           color="blue", size = 7) +
  annotate(geom="text", x="2006-01", y=0.7, label=paste("MVO"),
           color="red", size = 7) +
  annotate(geom="text", x="2006-01", y=1.5, label=paste("MVO+L"),
           color="green", size = 7) +
  annotate(geom="text", x="2006-01", y=1.7, label=paste("MVO (unrestricted)"),
           color="pink", size = 7) +
  annotate(geom="text", x="2006-01", y=1.7, label=paste("LRP"),
           color="darkgreen", size = 7)






###################################### Great Recession Recovery Plot ########################################

# Convert Date to factor to keep it as "YYYY-MM"
GreatRecessionRecovery$Date <- factor(GreatRecessionRecovery$Date)
# Get labels for every 5th year (e.g., "1990-01", "1995-01", etc.)
GreatRecessionRecoveryBreaks <- GreatRecessionRecovery$Date[seq(1, length(GreatRecessionRecovery$Date), by = 12 * 1)]  # every 5 years




# Backtest: 60% in RF and 40% in Bonds
GreatRecessionRecovery <- GreatRecessionRecovery %>%
  mutate(Strategy_60_40 = 0.6 * Equity + 0.4 * Bonds, ### 60/40
         Cum_Return_60_40 = cumprod(1 + Strategy_60_40), ### 60/40
         Log_Cum_Return_60_40 = log(Cum_Return_60_40), ### 60/40
         Strategy_MVO_Unrestricted = optimal_weights_leverage[1] * Equity + optimal_weights_leverage[2] * Bonds, ### MVO unrestricted
         Cum_Return_MVO_Unrestricted = cumprod(1 + Strategy_MVO_Unrestricted), ### MVO unrestricted
         Log_Cum_Return_MVO_Unrestricted = log(Cum_Return_MVO_Unrestricted), ### MVO unrestricted
         Strategy_MVO = optimal_weights[1] * Equity + optimal_weights[2] * Bonds, ### MVO
         Cum_Return_MVO = cumprod(1 + Strategy_MVO), ### MVO
         Log_Cum_Return_MVO = log(Cum_Return_MVO), ### MVO
         Strategy_MVOL = optimal_weights_leverage_fixed[1] * Equity + optimal_weights_leverage_fixed[2] * Bonds, ### MVO+L
         Cum_Return_MVOL = cumprod(1 + Strategy_MVOL), ### MVO+L
         Log_Cum_Return_MVOL = log(Cum_Return_MVOL), ### MVO+L
         Strategy_LRP = optimal_weights_LRP[1] * Equity + optimal_weights_LRP[2] * Bonds, ### LRP
         Cum_Return_LRP = cumprod(1 + Strategy_LRP), ### LRP
         Log_Cum_Return_LRP = log(Cum_Return_LRP)) ### LRP


# Plot the cumulative return with manually specified breaks
ggplot(GreatRecessionRecovery, aes(x = Date)) +
  ### 60/40
  geom_line(aes(y = Log_Cum_Return_60_40, group = 1), color = "blue") +
  geom_point(aes(y = Log_Cum_Return_60_40), color = "blue") +
  ### MVO (unrestricted)
  geom_line(aes(y = Log_Cum_Return_MVO_Unrestricted, group = 2), color = "pink") +
  geom_point(aes(y = Log_Cum_Return_MVO_Unrestricted), color = "pink") +
  ### MVO
  geom_line(aes(y = Log_Cum_Return_MVO, group = 2), color = "red") +
  geom_point(aes(y = Log_Cum_Return_MVO), color = "red") +
  ### MVO+L
  geom_line(aes(y = Log_Cum_Return_MVOL, group = 2), color = "green") +
  geom_point(aes(y = Log_Cum_Return_MVOL), color = "green") +
  ### LRP
  geom_line(aes(y = Log_Cum_Return_LRP, group = 2), color = "darkgreen") +
  geom_point(aes(y = Log_Cum_Return_LRP), color = "darkgreen") +
  ggtitle("Cumulative Return of Strategies (Log Scale)") +
  xlab("Date") + ylab("Log Cumulative Return") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks = FullPeriodBreaks) +  # Use your predefined breaks
  annotate(geom="text", x="2018-01", y=1, label=paste("60/40"),
           color="blue", size = 7) +
  annotate(geom="text", x="2018-01", y=0.7, label=paste("MVO"),
           color="red", size = 7) +
  annotate(geom="text", x="2018-01", y=1.5, label=paste("MVO+L"),
           color="green", size = 7) +
  annotate(geom="text", x="2018-01", y=1.7, label=paste("MVO (unrestricted)"),
           color="pink", size = 7) +
  annotate(geom="text", x="2018-01", y=1.7, label=paste("LRP"),
           color="darkgreen", size = 7)





###################################### Covid Plot ########################################

# Convert Date to factor to keep it as "YYYY-MM"
Covid$Date <- factor(Covid$Date)
# Get labels for every 5th year (e.g., "1990-01", "1995-01", etc.)
CovidBreaks <- Covid$Date[seq(1, length(Covid$Date), by = 12 * 1)]  # every 5 years


# Backtest: 60% in RF and 40% in Bonds
Covid <- Covid %>%
  mutate(Strategy_60_40 = 0.6 * Equity + 0.4 * Bonds, ### 60/40
         Cum_Return_60_40 = cumprod(1 + Strategy_60_40), ### 60/40
         Log_Cum_Return_60_40 = log(Cum_Return_60_40), ### 60/40
         Strategy_MVO_Unrestricted = optimal_weights_leverage[1] * Equity + optimal_weights_leverage[2] * Bonds, ### MVO unrestricted
         Cum_Return_MVO_Unrestricted = cumprod(1 + Strategy_MVO_Unrestricted), ### MVO unrestricted
         Log_Cum_Return_MVO_Unrestricted = log(Cum_Return_MVO_Unrestricted), ### MVO unrestricted
         Strategy_MVO = optimal_weights[1] * Equity + optimal_weights[2] * Bonds, ### MVO
         Cum_Return_MVO = cumprod(1 + Strategy_MVO), ### MVO
         Log_Cum_Return_MVO = log(Cum_Return_MVO), ### MVO
         Strategy_MVOL = optimal_weights_leverage_fixed[1] * Equity + optimal_weights_leverage_fixed[2] * Bonds, ### MVO+L
         Cum_Return_MVOL = cumprod(1 + Strategy_MVOL), ### MVO+L
         Log_Cum_Return_MVOL = log(Cum_Return_MVOL), ### MVO+L
         Strategy_LRP = optimal_weights_LRP[1] * Equity + optimal_weights_LRP[2] * Bonds, ### LRP
         Cum_Return_LRP = cumprod(1 + Strategy_LRP), ### LRP
         Log_Cum_Return_LRP = log(Cum_Return_LRP)) ### LRP


# Plot the cumulative return with manually specified breaks
ggplot(Covid, aes(x = Date)) +
  ### 60/40
  geom_line(aes(y = Log_Cum_Return_60_40, group = 1), color = "blue") +
  geom_point(aes(y = Log_Cum_Return_60_40), color = "blue") +
  ### MVO (unrestricted)
  geom_line(aes(y = Log_Cum_Return_MVO_Unrestricted, group = 2), color = "pink") +
  geom_point(aes(y = Log_Cum_Return_MVO_Unrestricted), color = "pink") +
  ### MVO
  geom_line(aes(y = Log_Cum_Return_MVO, group = 2), color = "red") +
  geom_point(aes(y = Log_Cum_Return_MVO), color = "red") +
  ### MVO+L
  geom_line(aes(y = Log_Cum_Return_MVOL, group = 2), color = "green") +
  geom_point(aes(y = Log_Cum_Return_MVOL), color = "green") +
  ### LRP
  geom_line(aes(y = Log_Cum_Return_LRP, group = 2), color = "darkgreen") +
  geom_point(aes(y = Log_Cum_Return_LRP), color = "darkgreen") +
  ggtitle("Cumulative Return of Strategies (Log Scale)") +
  xlab("Date") + ylab("Log Cumulative Return") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks = FullPeriodBreaks) +  # Use your predefined breaks
  annotate(geom="text", x="2022-01", y=1, label=paste("60/40"),
           color="blue", size = 7) +
  annotate(geom="text", x="2022-01", y=0.7, label=paste("MVO"),
           color="red", size = 7) +
  annotate(geom="text", x="2022-01", y=1.5, label=paste("MVO+L"),
           color="green", size = 7) +
  annotate(geom="text", x="2022-01", y=1.7, label=paste("MVO (unrestricted)"),
           color="pink", size = 7) +
  annotate(geom="text", x="2022-01", y=1.7, label=paste("LRP"),
           color="darkgreen", size = 7)

