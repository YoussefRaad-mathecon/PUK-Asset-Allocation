####################################################################################################################
####################################################################################################################
#------------------------------- Current Investment Universe ------------------------------------------------------#
####################################################################################################################
####################################################################################################################
library(quadprog)
library(nloptr)

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


# Print results
cat("60/40-portfolio expected return:", E_R_p_6040, "\n") # 0.01025922
cat("60/40-portfolio volatility :", sigma_p_6040, "\n") # 0.02705023

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
portfolio_return_MVO <- sum(optimal_weights_MVO * expected_returns)
portfolio_volatility_MVO <- sqrt(t(optimal_weights_MVO) %*% cov_matrix %*% optimal_weights_MVO)
# Print the results
cat("MVO stocks weight: ", optimal_weights_MVO[1], "\n") # 0.3312331 
cat("MVO bonds weight: ", optimal_weights_MVO[2], "\n") # 0
cat("MVO cash weight: ", optimal_weights_MVO[3], "\n") # 0.6687669
cat("MVO portfolio volatility: ", portfolio_volatility_MVO, "\n") # 0.01306414
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

library(ROI)
library(ROI.plugin.quadprog)

# Covariance matrix (Dmat) and expected returns
cov_matrix <- matrix(c(sigma_S^2, cov_S_B, cov_S_C,
                       cov_S_B, sigma_B^2, cov_B_C,
                       cov_S_C, cov_B_C, sigma_C^2), nrow = 3)
expected_returns <- c(E_R_S, E_R_B, E_R_C)
target_return <- 0.0075  # 75 bps target return

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
portfolio_volatility_LMVO <- sqrt(t(optimal_weights_LMVO) %*% cov_matrix %*% optimal_weights_LMVO)
portfolio_return_LMVO <- sum(optimal_weights_LMVO * expected_returns)

# Print the results in the desired format
cat("LMVO Stocks weight: ", optimal_weights_LMVO[1], "\n") #0.1404295 
cat("LMVO Bonds weight: ", optimal_weights_LMVO[2], "\n") # 0
cat("LMVO Cash weight: ", optimal_weights_LMVO[3], "\n") # 1.35957 
cat("LMVO Portfolio volatility: ", portfolio_volatility_LMVO, "\n") # 0.005721307
cat("LMVO Expected return: ", portfolio_return_LMVO, "\n") # 0.0075



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
cat("LRP Stocks weight: ", optimal_weights_LRP[1], "\n") # 0.1439084 
cat("LRP bonds weight: ", optimal_weights_LRP[2], "\n") # 0.2135776 
cat("LRP cash weight: ", optimal_weights_LRP[3], "\n") # 1.142514 
cat("LRP portfolio volatility: ", portfolio_volatility_LRP, "\n") # 0.008135465
cat("LRP expected return: ", portfolio_return_LRP, "\n") # 0.0075 by construction