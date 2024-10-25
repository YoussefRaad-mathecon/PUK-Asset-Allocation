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
RF <- FFdata_Monthly_Factors$RF[757:876] ### RF rate
RF <- RF / 100
Bonds <- Bonds$Monthly.Return.10.Yr[1:120] # Bonds

## Split into subsets of subsets and cut timeline of the CSV file 6_Portfolios_ME_Prior_12_2.CSV
# Average Value Weighted Returns -- Monthly
MOMexp_Average_Value_Weighted_Returns_Monthly <- MOMexp_Average_Value_Weighted_Returns_Monthly[757:876,]
# Average Equal Weighted Returns -- Monthly
MOMexp_Average_Equal_Weighted_Returns_Monthly <- MOMexp_Average_Equal_Weighted_Returns_Monthly[757:876,]
# Average Value Weighted Returns -- Annual
MOMexp_Average_Value_Weighted_Returns_Annual <- MOMexp_Average_Value_Weighted_Returns_Annual[64:73,]
# Average Equal Weighted Returns -- Annual
MOMexp_Average_Equal_Weighted_Returns_Annual <- MOMexp_Average_Equal_Weighted_Returns_Annual[64:73,]
# Number of Firms in Portfolios
MOMexp_Number_of_Firms_in_Portfolios <- MOMexp_Number_of_Firms_in_Portfolios[757:876,]
# Average Firm Size
MOMexp_Average_Firm_Size <- MOMexp_Average_Firm_Size[757:876,]
# Equally-Weighted Average of Prior Returns
MOMexp_Equally_Weighted_Average_of_Prior_Returns <- MOMexp_Equally_Weighted_Average_of_Prior_Returns[757:876,]
# Value-Weighted Average of Prior Returns
MOMexp_Value_Weighted_Average_of_Prior_Returns <- MOMexp_Value_Weighted_Average_of_Prior_Returns[757:876,]




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
Sharpe_ratio_6040 <- (E_R_p_6040 - E_R_C) / sigma_p_6040

# Print results
cat("60/40-portfolio expected return:", E_R_p_6040, "\n") # 0.01025922
cat("60/40-portfolio volatility :", sigma_p_6040, "\n") # 0.02705023
cat("Sharpe Ratio of the 60/40 portfolio:", Sharpe_ratio_6040, "\n") # 0.2308381


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

# Define covariance matrix (3x3)
cov_matrix <- matrix(c(sigma_S^2, cov_S_B, cov_S_C,
                       cov_S_B, sigma_B^2, cov_B_C,
                       cov_S_C, cov_B_C, sigma_C^2), nrow = 3)

# Define expected returns vector
expected_returns <- c(E_R_S, E_R_B, E_R_C)

# Target return
target_return <- 0.0075

# Define the number of assets
n_assets <- 3

# Covariance matrix 
cov_matrix <- matrix(c(sigma_S^2, cov_S_B, cov_S_C,
                       cov_S_B, sigma_B^2, cov_B_C,
                       cov_S_C, cov_B_C, sigma_C^2), nrow = 3)

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
E_R_p_MVO <- sum(optimal_weights_MVO * expected_returns)
sigma_p_MVO <- sqrt(t(optimal_weights_MVO) %*% cov_matrix %*% optimal_weights_MVO)

# Calculate Sharpe ratio
Sharpe_ratio_MVO <- (E_R_p_MVO - E_R_C) / sigma_p_MVO


# Print the results
cat("MVO stocks weight: ", optimal_weights_MVO[1], "\n") # 0.3312331 
cat("MVO bonds weight: ", optimal_weights_MVO[2], "\n") # 0
cat("MVO cash weight: ", optimal_weights_MVO[3], "\n") # 0.6687669
cat("MVO portfolio volatility: ", sigma_p_MVO, "\n") # 0.01306414
cat("MVO expected return: ", E_R_p_MVO, "\n") # 0.0075 by construction
cat("MVO Sharpe ratio: ", Sharpe_ratio_MVO, "\n") # 0.2667608

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


# Covariance matrix (Dmat) and expected returns
cov_matrix <- matrix(c(sigma_S^2, cov_S_B, cov_S_C,
                       cov_S_B, sigma_B^2, cov_B_C,
                       cov_S_C, cov_B_C, sigma_C^2), nrow = 3)
expected_returns <- c(E_R_S, E_R_B, E_R_C)

# Number of assets (stocks, bonds, cash)
n_assets <- 3

# Define the optimization problem
Dmat <- cov_matrix  # This is the variance-covariance matrix
dvec <- rep(0, n_assets)  # No linear term (minimize variance)

# Define the constraints
Amat <- rbind(
  expected_returns,  # Ensures the target return is met
  rep(1, n_assets)   # Ensures the sum of weights <= 1.5 (leverage constraint)
)
bvec <- c(target_return, 1.5)  # The target return and max leverage constraints

# Formulate the optimization problem
qp_problem <- OP(
  objective = Q_objective(Q = Dmat, L = dvec),  # Quadratic objective
  constraints = L_constraint(
    L = Amat,
    dir = c("==", "<="),  # "==" for the target return, "<=" for leverage
    rhs = bvec
  ),
  bounds = V_bound(lb = rep(0, n_assets), ub = rep(1.5, n_assets))  # Non-negative and no shorting
)

solution <- ROI_solve(qp_problem, solver = "quadprog")

# Extract the optimal weights
optimal_weights_LMVO <- solution$solution

# Calculate portfolio volatility and expected return
sigma_p_LMVO <- sqrt(t(optimal_weights_LMVO) %*% cov_matrix %*% optimal_weights_LMVO)
E_R_p_LMVO <- sum(optimal_weights_LMVO * expected_returns)

# Calculate Sharpe ratio
Sharpe_ratio_LMVO <- (E_R_p_LMVO - E_R_C) / sigma_p_LMVO


# Print the results in the desired format
cat("LMVO Stocks weight: ", optimal_weights_LMVO[1], "\n") #0.1404295 
cat("LMVO Bonds weight: ", optimal_weights_LMVO[2], "\n") # 0
cat("LMVO Cash weight: ", optimal_weights_LMVO[3], "\n") # 1.35957 
cat("LMVO Portfolio volatility: ", sigma_p_LMVO, "\n") # 0.005721307
cat("LMVO Expected return: ", E_R_p_LMVO, "\n") # 0.0075 by construction
cat("LMVO Sharpe ratio: ", Sharpe_ratio_LMVO, "\n") # 0.6091265 


####################################################################################################################
####################################################################################################################
#---------------------------------------- E ------------------------------------------------------------------------
####################################################################################################################
####################################################################################################################
# Define expected returns and volatilities
E_R_C <- mean(RF)  # Expected return for cash
E_R_S <- mean(market_return)  # Expected return for stocks
E_R_B <- mean(Bonds)  # Expected return for bonds
sigma_C <- sd(RF)  # Volatility of cash
sigma_S <- sd(market_return)  # Volatility of stocks
sigma_B <- sd(Bonds)  # Volatility of bonds

# Calculate covariances
cov_S_B <- cov(market_return, Bonds, use = "pairwise.complete.obs")
cov_S_C <- cov(market_return, RF, use = "pairwise.complete.obs")
cov_B_C <- cov(Bonds, RF, use = "pairwise.complete.obs")

# Covariance matrix
cov_matrix <- matrix(c(sigma_S^2, cov_S_B, cov_S_C,
                       cov_S_B, sigma_B^2, cov_B_C,
                       cov_S_C, cov_B_C, sigma_C^2), nrow = 3)

# Expected returns
expected_returns <- c(E_R_S, E_R_B, E_R_C)

# Number of assets
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

# Inequality constraints: leverage between 1 and 1.5
inequality_constraint <- function(x) {
  return(c(sum(x) - 1.5,   # Upper bound on leverage
           1 - sum(x)))    # Lower bound on leverage
}

# Equality constraint: target portfolio return equals 75bps
equality_constraint <- function(x) {
  return(sum(x * expected_returns) - 0.0075)  # Target return of 75 bps
}

# Initial guess for the weights
x0 <- rep(0.5, n_assets)

# Lower and upper bounds for weights
lb <- rep(0, n_assets)  # No shorting (non-negative weights)
ub <- rep(1.5, n_assets)  # Upper limit for weights (due to leverage constraint)

# Optimization using nloptr with COBYLA method
result_LRP <- nloptr(x0 = x0,
                     eval_f = function(x) objective_function(x, cov_matrix),
                     lb = lb,
                     ub = ub,
                     eval_g_ineq = inequality_constraint,  # Inequality constraints for leverage
                     eval_g_eq = equality_constraint,      # Target return constraint
                     opts = list(algorithm = "NLOPT_LN_COBYLA",  # COBYLA method
                                 xtol_rel = 1.0e-8,  # Tolerance level
                                 maxeval = 10000))   # Maximum number of iterations

# Extract optimal weights
optimal_weights_LRP <- result_LRP$solution

# Calculate portfolio's expected return and volatility
E_R_p_LRP <- sum(optimal_weights_LRP * expected_returns)
sigma_p_LRP <- sqrt(t(optimal_weights_LRP) %*% cov_matrix %*% optimal_weights_LRP)

# Calculate Sharpe ratio
Sharpe_ratio_LRP <- (E_R_p_LRP - E_R_C) / sigma_p_LRP


# Print the results
cat("LRP Stocks weight: ", optimal_weights_LRP[1], "\n") # 0.1439084 
cat("LRP bonds weight: ", optimal_weights_LRP[2], "\n") # 0.2135776 
cat("LRP cash weight: ", optimal_weights_LRP[3], "\n") # 1.142514
cat("LRP portfolio volatility: ", sigma_p_LRP, "\n") # 0.008135465
cat("LRP expected return: ", E_R_p_LRP, "\n") # 0.0075 by construction
cat("LRP Sharpe ratio: ", Sharpe_ratio_LRP, "\n") # 0.4283713 

# Function to check f(x_star) = 0 condition
check_f_zero <- function(optimal_weights, cov_matrix) {
  marginal_contributions <- cov_matrix %*% optimal_weights
  total_risk <- sqrt(portfolio_variance(optimal_weights, cov_matrix))
  risk_contributions <- optimal_weights * marginal_contributions / total_risk
  
  f_val_check <- 0
  for (i in 1:n_assets) {
    for (j in 1:n_assets) {
      f_val_check <- f_val_check + (risk_contributions[i] - risk_contributions[j])^2
    }
  }
  
  return(f_val_check)
}


check_f_zero(optimal_weights_LRP, cov_matrix)

#--------------------------------------------extra: VW port--------------------------------------------------------------#
#Value-Weighted portfolio / Market Portfolio
E_R_p_VW <- mean(market_return)
sigma_p_VW <- sd(market_return)

Sharpe_ratio_VW <- (E_R_p_VW - E_R_C) / sigma_p_VW 
cat("VW Portfolio Return: ", mean(market_return), "\n") # 0.01453629
cat("VW Portfolio Volatility: ", sd(market_return), "\n") # 0.03939666
cat("VW Portfolio Sharpe Ratio:", Sharpe_ratio_VW, "\n") # 0.2670605
#--------------------------------------------extra: market assumptions---------------------------------------------------#

expected_returns # stocks, bonds, cash
sigma_S
sigma_B
sigma_C
cov_matrix

#--------------------------------------------extra: efficient frontier test-------------------------------------------------#

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

# Define the covariance matrix (Dmat) and expected returns
cov_matrix <- matrix(c(sigma_S^2, cov_S_B, cov_S_C,
                       cov_S_B, sigma_B^2, cov_B_C,
                       cov_S_C, cov_B_C, sigma_C^2), nrow = 3)
expected_returns <- c(E_R_S, E_R_B, E_R_C)

# Number of assets (stocks, bonds, cash)
n_assets <- length(expected_returns)

# Function to calculate the efficient frontier
efficient_frontier <- function(cov_matrix, expected_returns, n_points = 10000) {
  # Initialize empty lists to store results
  frontier_returns <- c()
  frontier_volatilities <- c()
  
  # Create a sequence of target returns
  target_returns <- seq(min(expected_returns), max(expected_returns), length.out = n_points)
  
  for (target_return in target_returns) {
    # Solve the quadratic program to minimize volatility (quadratic form) subject to target return
    result <- solve.QP(Dmat = cov_matrix, dvec = rep(0, n_assets), 
                       Amat = cbind(1, expected_returns), 
                       bvec = c(1, target_return), meq = 2)
    
    weights <- result$solution
    portfolio_return <- sum(weights * expected_returns)
    portfolio_volatility <- sqrt(t(weights) %*% cov_matrix %*% weights)
    
    # Store the results
    frontier_returns <- c(frontier_returns, portfolio_return)
    frontier_volatilities <- c(frontier_volatilities, portfolio_volatility)
  }
  
  return(data.frame(Return = frontier_returns, Volatility = frontier_volatilities))
}

# Generate random portfolios
n_portfolios <- 10000
weights <- matrix(runif(n_portfolios * n_assets, min = 0, max = 1), nrow = n_portfolios, ncol = n_assets)
weights <- weights / rowSums(weights)  # Normalize so that weights sum to 1

# Portfolio statistics function
portfolio_stats <- function(w, cov_matrix, expected_returns) {
  portfolio_return <- sum(w * expected_returns)
  portfolio_volatility <- sqrt(t(w) %*% cov_matrix %*% w)
  return(c(portfolio_return, portfolio_volatility))
}

# Calculate statistics for random portfolios
portfolio_results <- t(apply(weights, 1, portfolio_stats, cov_matrix = cov_matrix, expected_returns = expected_returns))
portfolio_df <- data.frame(
  Return = portfolio_results[, 1],
  Volatility = portfolio_results[, 2]
)

# Compute the efficient frontier
efficient_frontier_df <- efficient_frontier(cov_matrix, expected_returns)

# Define weights for the specified portfolios
w_60_40 <- c(0.60, 0.40, 0) 
w_MVO <- c(optimal_weights_MVO)  
w_LMVO <- c(optimal_weights_LMVO) 
w_LRP <- c(optimal_weights_LRP)  
w_VW <- c(1, 0, 0)  

# Calculate stats for each specified portfolio
portfolio_60_40 <- portfolio_stats(w_60_40, cov_matrix, expected_returns)
portfolio_MVO <- portfolio_stats(w_MVO, cov_matrix, expected_returns)
portfolio_LMVO <- portfolio_stats(w_LMVO, cov_matrix, expected_returns)
portfolio_LRP <- portfolio_stats(w_LRP, cov_matrix, expected_returns)
portfolio_VW <- portfolio_stats(w_VW, cov_matrix, expected_returns)

# Combine results into a data frame for easy plotting
portfolio_names <- c("60/40", "MVO", "LMVO", "LRP", "VW")
portfolio_points <- data.frame(
  Strategy = portfolio_names,
  Return = c(portfolio_60_40[1], portfolio_MVO[1], portfolio_LMVO[1], portfolio_LRP[1], portfolio_VW[1]),
  Volatility = c(portfolio_60_40[2], portfolio_MVO[2], portfolio_LMVO[2], portfolio_LRP[2], portfolio_VW[2])
)


# Plot both the random portfolios and the specified portfolios (without the efficient frontier line)
ggplot(portfolio_df, aes(x = Volatility, y = Return)) +
  geom_point(color = "#666666", alpha = 0.3) +  
  geom_point(data = portfolio_points, aes(x = Volatility, y = Return, color = Strategy), size = 7) + 
  labs(title = "Efficient Frontier - Roaring 90s",
       x = "Volatility (Standard Deviation of Returns)",
       y = "Expected Return") +
  scale_color_manual(values = c("firebrick", "darkgreen", "purple", "salmon", "orange")) +  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 27),  # Title size
    axis.title.x = element_text(size = 23),  # X-axis label size
    axis.title.y = element_text(size = 23),  # Y-axis label size
    axis.text.x = element_text(size = 17),   # X-axis tick label size
    axis.text.y = element_text(size = 17),
    legend.title = element_text(size = 20),  # Legend title size
    legend.text = element_text(size = 17)    # Legend text size
  )

