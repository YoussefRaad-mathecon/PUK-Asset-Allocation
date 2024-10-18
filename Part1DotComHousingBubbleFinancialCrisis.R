####################################################################################################################
####################################################################################################################
#------------------------------- Current Investment Universe ------------------------------------------------------#
####################################################################################################################
####################################################################################################################
library(quadprog)
library(nloptr)
library(ROI)
library(ROI.plugin.quadprog)
### Data
set.seed(123)
RF <- FFdata_Monthly_Factors$RF[877:996] ### RF rate
RF <- RF / 100
Bonds <- Bonds$Monthly.Return.10.Yr[121:240] # Bonds

## Split into subsets of subsets and cut timeline of the CSV file 6_Portfolios_ME_Prior_12_2.CSV
# Average Value Weighted Returns -- Monthly
MOMexp_Average_Value_Weighted_Returns_Monthly <- MOMexp_Average_Value_Weighted_Returns_Monthly[877:996,]
# Average Equal Weighted Returns -- Monthly
MOMexp_Average_Equal_Weighted_Returns_Monthly <- MOMexp_Average_Equal_Weighted_Returns_Monthly[877:996,]
# Number of Firms in Portfolios
MOMexp_Number_of_Firms_in_Portfolios <- MOMexp_Number_of_Firms_in_Portfolios[877:996,]
# Average Firm Size
MOMexp_Average_Firm_Size <- MOMexp_Average_Firm_Size[877:996,]
# Equally-Weighted Average of Prior Returns
MOMexp_Equally_Weighted_Average_of_Prior_Returns <- MOMexp_Equally_Weighted_Average_of_Prior_Returns[877:996,]
# Value-Weighted Average of Prior Returns
MOMexp_Value_Weighted_Average_of_Prior_Returns <- MOMexp_Value_Weighted_Average_of_Prior_Returns[877:996,]


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
covariance <- cov((market_return), Bonds, use = "pairwise.complete.obs")

# Calculate portfolio volatility
sigma_p_6040 <- sqrt(w_S^2 * sigma_S^2 + w_B^2 * sigma_B^2 + 2 * w_S * w_B * covariance)

# Check if portfolio meets return target
target_return <- 0.0075  # 75 bps

# Calculate Sharpe ratio
Sharpe_ratio <- (E_R_p_6040 - E_R_C) / sigma_p_6040

# Print results
cat("60/40-portfolio expected return:", E_R_p_6040, "\n") # 0.002339922 
cat("60/40-portfolio volatility :", sigma_p_6040, "\n") # 0.02767252 
cat("Sharpe Ratio of the 60/40 portfolio:", Sharpe_ratio, "\n") # 0.002647235


####################################################################################################################
####################################################################################################################
#---------------------------------------- C ------------------------------------------------------------------------
####################################################################################################################
####################################################################################################################
#Proof we cant hit target return
expected_returns <- c(E_R_S, E_R_B, E_R_C)
n_assets <- 3
# Set up linear programming to maximize portfolio return under the no-short-selling constraint
f.obj <- expected_returns  # Objective is to maximize the portfolio return
f.con <- rbind(rep(1, n_assets), diag(n_assets))  # Constraints: sum of weights = 1, no short selling
f.dir <- c("=", rep(">=", n_assets))  # First constraint is equality (sum of weights = 1), others are inequalities (no short selling)
f.rhs <- c(1, rep(0, n_assets))  # Right-hand side: sum of weights = 1, non-negativity
library(lpSolve)
# Solve the linear programming problem to maximize return
lp_result <- lp("max", f.obj, f.con, f.dir, f.rhs)

# Check the optimal portfolio return and weights
max_return <- sum(lp_result$solution * expected_returns)
max_return # 0.004155964
lp_result$solution # 0 1 0 i.e all in bonds


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


# Covariance matrix (Dmat) and expected returns
cov_matrix <- matrix(c(sigma_S^2, cov_S_B, cov_S_C,
                       cov_S_B, sigma_B^2, cov_B_C,
                       cov_S_C, cov_B_C, sigma_C^2), nrow = 3)
expected_returns <- c(E_R_S, E_R_B, E_R_C)

# Number of assets (stocks, bonds, cash)
n_assets <- 3

# Set the covariance matrix and expected returns
Dmat <- cov_matrix  # Covariance matrix of asset returns
dvec <- rep(0, n_assets)  # No linear term in the objective function

# Constraints matrix
Amat <- cbind(rep(1, n_assets), diag(n_assets))  # Combine weight sum to 1 and non-negativity constraints
bvec <- c(1, rep(0, n_assets))  # Right-hand side: sum of weights = 1, non-negativity

# Solve the quadratic programming problem without target return
result <- solve.QP(Dmat, dvec, Amat, bvec, meq = 1)  # meq=1 for the equality constraint (sum of weights)


# Extract optimal weights
optimal_weights_MVO<- result$solution

# Calculate portfolio volatility and expected return
sigma_p_MVO <- sqrt(t(optimal_weights_MVO) %*% cov_matrix %*% optimal_weights_MVO)
E_R_p_MVO <- sum(optimal_weights_MVO * expected_returns)

# Calculate Sharpe ratio
Sharpe_ratio_MVO <- (E_R_p_MVO - E_R_C) / sigma_p_MVO

# Print the results
cat("MVO stocks weight: ", optimal_weights_MVO[1], "\n") # 0.002968373 
cat("MVO bonds weight: ", optimal_weights_MVO[2], "\n") # 0.0008716451 
cat("MVO cash weight: ", optimal_weights_MVO[3], "\n") # 0.99616 
cat("MVO portfolio volatility: ", sigma_p_MVO, "\n") # 0.001576442
cat("MVO expected return: ", E_R_p_MVO, "\n") # 0.002264937 (can not hit target return)
cat("MVO Sharpe ratio: ", Sharpe_ratio_MVO, "\n") # -0.00109712 



####################################################################################################################
####################################################################################################################
#---------------------------------------- D ------------------------------------------------------------------------
####################################################################################################################
####################################################################################################################
#Proof we can't hit target return
# Set up linear programming to maximize portfolio return under the no-short-selling constraint
f.obj <- expected_returns  # Objective is to maximize the portfolio return
f.con <- rbind(rep(1.5, n_assets), diag(n_assets))  # Constraints: sum of weights = 1, no short selling
f.dir <- c("=", rep(">=", n_assets))  # First constraint is equality (sum of weights = 1), others are inequalities (no short selling)
f.rhs <- c(1.5, rep(0, n_assets))  # Right-hand side: sum of weights = 1, non-negativity

# Solve the linear programming problem to maximize return
lp_result <- lp("max", f.obj, f.con, f.dir, f.rhs)

# Check the optimal portfolio return and weights
max_return <- sum(lp_result$solution * expected_returns)
max_return # 0.004155964
lp_result$solution # 0 1 0 i.e all in bonds


# Define expected returns and volatilities (assumed these are already calculated)
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

# Define the expected returns vector for stocks, bonds, and cash
expected_returns <- c(E_R_S, E_R_B, E_R_C)

# Target return
target_return <- 0.0075

# Number of assets (stocks, bonds, cash)
n_assets <- 3

# Define the covariance matrix and expected returns
Dmat <- cov_matrix  # This is the covariance matrix
dvec <- rep(0, n_assets)  # Since we're minimizing variance, the linear term is 0

# Constraints
Amat <- cbind(expected_returns, diag(n_assets))  # Matrix for equality and inequality constraints
bvec <- c(0, rep(0, n_assets))  # The lower bounds for weights (non-negative)
meq <- 0  # Number of equality constraints (we have none for this problem)

# Set up the inequality constraint to allow leverage up to 1.5
Amat <- cbind(Amat, rep(1, n_assets))  # Adding a row of ones for the sum constraint (leverage <= 1.5)
bvec <- c(bvec, 1.5)  # Set the leverage constraint to 1.5

# Solve the quadratic programming problem
result <- solve.QP(Dmat, dvec, Amat, bvec, meq)


# Extract the optimal weights
optimal_weights_LMVO <- result$solution

# Calculate portfolio volatility
sigma_p_LMVO <- sqrt(t(optimal_weights_LMVO) %*% cov_matrix %*% optimal_weights_LMVO)

# Calculate portfolio expected return
E_R_p_LMVO <- sum(optimal_weights_LMVO * expected_returns)

# Calculate Sharpe ratio
Sharpe_ratio_LMVO <- (E_R_p_LMVO - E_R_C) / sigma_p_LMVO

# Print the results
cat("LMVO Stocks weight: ", optimal_weights_LMVO[1], "\n") # 0.004452559 
cat("LMVO Bonds weight: ", optimal_weights_LMVO[2], "\n") # 0.001307468 
cat("LMVO Cash weight: ", optimal_weights_LMVO[3], "\n") # 1.49424
cat("LMVO Portfolio volatility: ", sigma_p_LMVO, "\n") # 0.002364663
cat("LMVO Expected return: ", E_R_p_LMVO, "\n") # 0.003397406 
cat("LMVO Sharpe ratio: ", Sharpe_ratio_LMVO, "\n") # LMVO Sharpe ratio: 0.4781819 

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

# Constraints: sum of weights equals 1 (no return target constraint)
equality_constraint <- function(x) {
  return(sum(x) - 1)  # Sum of weights constraint
}

# Optimization using nloptr (COBYLA method)
x0 <- rep(0.33, n_assets)  # Initial guess (equal weights)

# Lower and upper bounds for weights (no negative weights)
lb <- rep(0, n_assets)
ub <- rep(1.5, n_assets)  # Adjust upper bound if necessary

# Optimization problem without target return constraint
result_LRP <- nloptr(x0 = x0,
                     eval_f = function(x) objective_function(x, cov_matrix),
                     lb = lb,
                     ub = ub,
                     eval_g_eq = function(x) equality_constraint(x),
                     opts = list(algorithm = "NLOPT_LN_COBYLA",  # COBYLA: Gradient-free algorithm
                                 xtol_rel = 1.0e-8,  # Tolerance
                                 maxeval = 10000))  # Maximum number of iterations
optimal_weights_LRP <- result_LRP$solution

# Calculate the portfolio's expected return and volatility
portfolio_return_LRP <- sum(optimal_weights_LRP * expected_returns)
portfolio_volatility_LRP <- sqrt(t(optimal_weights_LRP) %*% cov_matrix %*% optimal_weights_LRP)

# Calculate Sharpe ratio
Sharpe_ratio_LRP <- (portfolio_return_LRP - E_R_C) / portfolio_volatility_LRP

# Print the results in the desired format
cat("LRP Stocks weight: ", optimal_weights_LRP[1], "\n") # 0.03563962 
cat("LRP bonds weight: ", optimal_weights_LRP[2], "\n") # 0.06762812 
cat("LRP cash weight: ", optimal_weights_LRP[3], "\n") # 0.8967323 
cat("LRP portfolio volatility: ", portfolio_volatility_LRP, "\n") # 0.002485279 
cat("LRP expected return: ", portfolio_return_LRP, "\n") # 0.002353898 
cat("LRP Sharpe ratio: ", Sharpe_ratio_LRP, "\n") # 0.03509939







################# test with sharpe ratio
# Portfolio Sharpe ratio calculation
sharpe_ratio <- function(x, expected_returns, cov_matrix, E_R_C) {
  portfolio_return <- sum(x * expected_returns)
  portfolio_volatility <- sqrt(as.numeric(t(x) %*% cov_matrix %*% x))
  
  # Return negative Sharpe ratio (for minimization)
  return(-(portfolio_return - E_R_C) / portfolio_volatility)
}

# Objective function to minimize (negative Sharpe ratio)
objective_function_sharpe <- function(x, expected_returns, cov_matrix, E_R_C) {
  return(sharpe_ratio(x, expected_returns, cov_matrix, E_R_C))
}

# Constraints: sum of weights equals 1
equality_constraint <- function(x) {
  return(sum(x) - 1)  # Sum of weights constraint
}

# Optimization using nloptr (COBYLA method)
x0 <- rep(0.33, n_assets)  # Initial guess (equal weights)

# Lower and upper bounds for weights (no negative weights)
lb <- rep(0, n_assets)
ub <- rep(1.5, n_assets)  # Adjust upper bound if necessary

# Optimization problem to maximize Sharpe ratio
result_Sharpe <- nloptr(x0 = x0,
                        eval_f = function(x) objective_function_sharpe(x, expected_returns, cov_matrix, E_R_C),
                        lb = lb,
                        ub = ub,
                        eval_g_eq = function(x) equality_constraint(x),
                        opts = list(algorithm = "NLOPT_LN_COBYLA",  # COBYLA: Gradient-free algorithm
                                    xtol_rel = 1.0e-8,  # Tolerance
                                    maxeval = 10000))  # Maximum number of iterations
optimal_weights_Sharpe <- result_Sharpe$solution

# Calculate the portfolio's expected return and volatility
portfolio_return_Sharpe <- sum(optimal_weights_Sharpe * expected_returns)
portfolio_volatility_Sharpe <- sqrt(t(optimal_weights_Sharpe) %*% cov_matrix %*% optimal_weights_Sharpe)

# Calculate Sharpe ratio
Sharpe_ratio_Sharpe <- (portfolio_return_Sharpe - E_R_C) / portfolio_volatility_Sharpe

# Print the results in the desired format
cat("Sharpe-maximized Stocks weight: ", optimal_weights_Sharpe[1], "\n") # 0 
cat("Sharpe-maximized Bonds weight: ", optimal_weights_Sharpe[2], "\n") # 1
cat("Sharpe-maximized Cash weight: ", optimal_weights_Sharpe[3], "\n") # 0 
cat("Sharpe-maximized portfolio volatility: ", portfolio_volatility_Sharpe, "\n") # 0.02365899 
cat("Sharpe-maximized expected return: ", portfolio_return_Sharpe, "\n") # 0.004155964
cat("Sharpe-maximized Sharpe ratio: ", Sharpe_ratio_Sharpe, "\n") # 0.07985539 

