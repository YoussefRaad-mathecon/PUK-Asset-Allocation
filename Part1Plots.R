####################################################################################################################
####################################################################################################################
#----------------------------------- Working Directory -------------------------------------------------------------
####################################################################################################################
####################################################################################################################


set.seed(123)
setwd("~/Documents/KU/PUKAssetAllocation/Exam/RCode")


### Data
set.seed(123)
RF <- FFdata_Monthly_Factors$RF[757:1164] ### RF rate
RF <- RF / 100
length(RF) ### 408
BondData <- read.csv("Bonds_done.csv", header = TRUE)
Bonds <- BondData$Monthly.Return.10.Yr
length(Bonds) ### 408
length(market_return) ### 408



FullPeriod <- data.frame(
  "Date" = FFdata_Monthly_Factors$Date[757:1164],
  "RF" = RF,
  "Bonds" = Bonds,
  "Equity" = market_return)
row.names(FullPeriod) <- NULL
FullPeriod$Date <- as.Date(paste0(FullPeriod$Date, "01"), format = "%Y%m%d")

### 199001 - 200112
Roaring90s <- subset(FullPeriod,
                     Date >= as.Date("1990-01-01") & Date <= as.Date("1999-12-01"))
row.names(Roaring90s) <- NULL
Roaring90s$Date <- format(Roaring90s$Date, "%Y-%m")

### 200201 - 200912
FinancialCrisis <- subset(FullPeriod,
                          Date >= as.Date("2000-01-01") & Date <= as.Date("2009-12-01"))
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




####################################################################################################################
####################################################################################################################
#--------------------------------- Efficient Frontier --------------------------------------------------------------
####################################################################################################################
####################################################################################################################



###################################### Full Period Plot ########################################



# Define expected returns and volatilities 
E_R_C <- mean(FullPeriod$RF)   # Expected return for cash
E_R_S <- mean(FullPeriod$Equity)   # Expected return for stocks
E_R_B <- mean(FullPeriod$Bonds)   # Expected return for bonds
sigma_C <- sd(FullPeriod$RF)   # Volatility of cash
sigma_S <- sd(FullPeriod$Equity)   # Volatility of stocks
sigma_B <- sd(FullPeriod$Bonds)   # Volatility of bonds

# Calculate covariances
cov_S_B <- cov(FullPeriod$Equity, FullPeriod$Bonds, use = "pairwise.complete.obs")
cov_S_C <- cov(FullPeriod$Equity, FullPeriod$RF, use = "pairwise.complete.obs")
cov_B_C <- cov(FullPeriod$Bonds, FullPeriod$RF, use = "pairwise.complete.obs")

cov_matrix <- matrix(c(sigma_S^2, cov_S_B, cov_S_C,
                       cov_S_B, sigma_B^2, cov_B_C,
                       cov_S_C, cov_B_C, sigma_C^2), nrow = 3)

# Number of portfolios to simulate
n_portfolios <- 100

# Create a sequence of weights for stocks and bonds
w_S <- seq(0, 1, length.out = n_portfolios)

# Initialize vectors to store results
port_returns <- numeric(n_portfolios * n_portfolios)
port_volatilities <- numeric(n_portfolios * n_portfolios)
weights_stock <- numeric(n_portfolios * n_portfolios)
weights_bond <- numeric(n_portfolios * n_portfolios)
weights_cash <- numeric(n_portfolios * n_portfolios)

counter <- 1

# Loop through different combinations of weights for stocks, bonds, and cash
for (i in 1:n_portfolios) {
  for (j in 1:n_portfolios) {
    # Define weights for stocks, bonds, and cash
    w_S_i <- w_S[i]
    w_B_i <- w_S[j]
    w_C_i <- 1 - w_S_i - w_B_i  # Cash is the residual
    
    # Ensure weights sum to 1 and are non-negative
    if (w_C_i >= 0) {
      # Portfolio expected return
      port_returns[counter] <- w_S_i * E_R_S + w_B_i * E_R_B + w_C_i * E_R_C
      
      # Portfolio volatility (assuming zero covariance with cash)
      port_volatilities[counter] <- sqrt(w_S_i^2 * sigma_S^2 +
                                           w_B_i^2 * sigma_B^2 +
                                           2 * w_S_i * w_B_i * cov_S_B)
      
      # Store the weights for plotting later
      weights_stock[counter] <- w_S_i
      weights_bond[counter] <- w_B_i
      weights_cash[counter] <- w_C_i
      
      counter <- counter + 1
    }
  }
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
     main = "Efficient Frontier")

# Add the efficient frontier (above GMV) with a solid line
points(port_volatilities[efficient_indices], port_returns[efficient_indices], col = "blue", lwd = 2)

# Highlight the Global Minimum Variance Portfolio (GMV)
points(gmv_volatility, gmv_return, col = "green", pch = 16, cex = 1.5)
text(gmv_volatility, gmv_return, labels = "GMV Portfolio", pos = 4, col = "green")

### 60/40/0 strategy
w_S_6040 <- 0.6
w_B_6040 <- 0.4
w_C_6040 <- 0.0
E_R_6040 <- w_S_6040 * E_R_S + w_B_6040 * E_R_B + w_C_6040 * E_R_C
sigma_6040 <- sqrt(w_S_6040^2 * sigma_S^2 + w_B_6040^2 * sigma_B^2 + 2 * w_S_6040 * w_B_6040 * cov_S_B)
points(sigma_6040, E_R_6040, col = "red", pch = 8, cex = 1.5)
text(sigma_6040, E_R_6040, labels = "60/40/0 Strategy", pos = 1, col = "red")

### MVO strategy
w_S_MVO <- 0.7127203
w_B_MVO <- 0.2872797
w_C_MVO <- 0
E_R_MVO <- w_S_MVO * E_R_S + w_B_MVO * E_R_B + w_C_MVO * E_R_C
sigma_MVO <- sqrt(w_S_MVO^2 * sigma_S^2 + w_B_MVO^2 * sigma_B^2 + 2 * w_S_MVO * w_B_MVO * cov_S_B)
points(sigma_MVO, E_R_MVO, col = "red", pch = 8, cex = 1.5)
text(sigma_MVO, E_R_MVO, labels = "MVO Strategy", pos = 1, col = "red")

### MVO+L strategy
w_S_LMVO <- 0.5533951
w_B_LMVO <- 0.3704708
w_C_LMVO <- 0.5761342
E_R_LMVO <- w_S_LMVO * E_R_S + w_B_LMVO * E_R_B + w_C_LMVO * E_R_C
sigma_LMVO <- sqrt(w_S_LMVO^2 * sigma_S^2 + w_B_LMVO^2 * sigma_B^2 + 2 * w_S_LMVO * w_B_LMVO * cov_S_B)
points(sigma_LMVO, E_R_LMVO, col = "red", pch = 8, cex = 1.5)
text(sigma_LMVO, E_R_LMVO, labels = "LMVO Strategy", pos = 1, col = "red")

### LRP strategy
w_S_LRP <- 0.484716
w_B_LRP <- 0.9317936
w_C_LRP <- 0.08349038
E_R_LRP <- w_S_LRP * E_R_S + w_B_LRP * E_R_B + w_C_LRP * E_R_C
sigma_LRP <- sqrt(w_S_LRP^2 * sigma_S^2 + w_B_LRP^2 * sigma_B^2 + 2 * w_S_LRP * w_B_LRP * cov_S_B)
points(sigma_LRP, E_R_LRP, col = "red", pch = 8, cex = 1.5)
text(sigma_LRP, E_R_LRP, labels = "LRP Strategy", pos = 1, col = "red")


# Add grid for better visualization
grid()




# Create a data frame for the efficient frontier
eff_frontier_data <- data.frame(
  Volatility = port_volatilities,
  Expected_Return = port_returns,
  Type = ifelse(1:length(port_volatilities) %in% efficient_indices, "Efficient", "Inefficient")
)

# Create the ggplot
ggplot(eff_frontier_data, aes(x = Volatility, y = Expected_Return)) +
  #geom_point(data = subset(eff_frontier_data, Type == "Inefficient"), 
            #aes(color = "Inefficient Frontier"), alpha = 0.3, size = 1) +
  geom_point(data = subset(eff_frontier_data, Type == "Efficient"), 
            aes(color = "Efficient Frontier"), size = 1, alpha = 0.3) +
  geom_point(aes(x = gmv_volatility, y = gmv_return), color = "steelblue", size = 5, pch = 8) +
  geom_text(aes(x = gmv_volatility, y = gmv_return, label = "GMV Portfolio"), 
            hjust = 1.2, color = "steelblue") +
  geom_point(aes(x = sigma_6040, y = E_R_6040), color = "firebrick", size = 5, pch = 8) +
  geom_text(aes(x = sigma_6040, y = E_R_6040, label = "60/40/0 Strategy"), 
            vjust = -1, color = "firebrick") +
  geom_point(aes(x = sigma_MVO, y = E_R_MVO), color = "salmon", size = 5, pch = 8) +
  geom_text(aes(x = sigma_MVO, y = E_R_MVO, label = "MVO Strategy"), 
            vjust = -1, color = "salmon") +
  geom_point(aes(x = sigma_LMVO, y = E_R_LMVO), color = "darkgreen", size = 5, pch = 8) +
  geom_text(aes(x = sigma_LMVO, y = E_R_LMVO, label = "LMVO Strategy"), 
            vjust = -1, color = "darkgreen") +
  geom_point(aes(x = sigma_LRP, y = E_R_LRP), color = "purple", size = 5, pch = 8) +
  geom_text(aes(x = sigma_LRP, y = E_R_LRP, label = "LRP Strategy"), 
            vjust = -1, color = "purple") +
  labs(x = "Portfolio Volatility", 
       y = "Portfolio Expected Return", 
       title = "Efficient Frontier") +
  scale_color_manual(values = c("Inefficient Frontier" = "blue", "Efficient Frontier" = "blue")) +
  theme_minimal() +
  theme(legend.position = "none") +
  grid()  # Note: grid() may not be needed in ggplot as it is handled by the theme.








###################################### Roaring 90s Plot ########################################



# Define expected returns and volatilities 
E_R_C <- mean(Roaring90s$RF)   # Expected return for cash
E_R_S <- mean(Roaring90s$Equity)   # Expected return for stocks
E_R_B <- mean(Roaring90s$Bonds)   # Expected return for bonds
sigma_C <- sd(Roaring90s$RF)   # Volatility of cash
sigma_S <- sd(Roaring90s$Equity)   # Volatility of stocks
sigma_B <- sd(Roaring90s$Bonds)   # Volatility of bonds

# Calculate covariances
cov_S_B <- cov(Roaring90s$Equity, Roaring90s$Bonds, use = "pairwise.complete.obs")
cov_S_C <- cov(Roaring90s$Equity, Roaring90s$RF, use = "pairwise.complete.obs")
cov_B_C <- cov(Roaring90s$Bonds, Roaring90s$RF, use = "pairwise.complete.obs")

cov_matrix <- matrix(c(sigma_S^2, cov_S_B, cov_S_C,
                       cov_S_B, sigma_B^2, cov_B_C,
                       cov_S_C, cov_B_C, sigma_C^2), nrow = 3)

# Number of portfolios to simulate
n_portfolios <- 100

# Create a sequence of weights for stocks and bonds
w_S <- seq(0, 1, length.out = n_portfolios)

# Initialize vectors to store results
port_returns <- numeric(n_portfolios * n_portfolios)
port_volatilities <- numeric(n_portfolios * n_portfolios)
weights_stock <- numeric(n_portfolios * n_portfolios)
weights_bond <- numeric(n_portfolios * n_portfolios)
weights_cash <- numeric(n_portfolios * n_portfolios)

counter <- 1

# Loop through different combinations of weights for stocks, bonds, and cash
for (i in 1:n_portfolios) {
  for (j in 1:n_portfolios) {
    # Define weights for stocks, bonds, and cash
    w_S_i <- w_S[i]
    w_B_i <- w_S[j]
    w_C_i <- 1 - w_S_i - w_B_i  # Cash is the residual
    
    # Ensure weights sum to 1 and are non-negative
    if (w_C_i >= 0) {
      # Portfolio expected return
      port_returns[counter] <- w_S_i * E_R_S + w_B_i * E_R_B + w_C_i * E_R_C
      
      # Portfolio volatility (assuming zero covariance with cash)
      port_volatilities[counter] <- sqrt(w_S_i^2 * sigma_S^2 +
                                           w_B_i^2 * sigma_B^2 +
                                           2 * w_S_i * w_B_i * cov_S_B)
      
      # Store the weights for plotting later
      weights_stock[counter] <- w_S_i
      weights_bond[counter] <- w_B_i
      weights_cash[counter] <- w_C_i
      
      counter <- counter + 1
    }
  }
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
     main = "Efficient Frontier")

# Add the efficient frontier (above GMV) with a solid line
points(port_volatilities[efficient_indices], port_returns[efficient_indices], col = "blue", lwd = 2)

# Highlight the Global Minimum Variance Portfolio (GMV)
points(gmv_volatility, gmv_return, col = "green", pch = 16, cex = 1.5)
text(gmv_volatility, gmv_return, labels = "GMV Portfolio", pos = 4, col = "green")

### 60/40/0 strategy
w_S_6040 <- 0.6
w_B_6040 <- 0.4
w_C_6040 <- 0.0
E_R_6040 <- w_S_6040 * E_R_S + w_B_6040 * E_R_B + w_C_6040 * E_R_C
sigma_6040 <- sqrt(w_S_6040^2 * sigma_S^2 + w_B_6040^2 * sigma_B^2 + 2 * w_S_6040 * w_B_6040 * cov_S_B)
points(sigma_6040, E_R_6040, col = "red", pch = 8, cex = 1.5)
text(sigma_6040, E_R_6040, labels = "60/40/0 Strategy", pos = 1, col = "red")

### MVO strategy
w_S_MVO <- 0.3312331
w_B_MVO <- 0
w_C_MVO <- 0.6687669
E_R_MVO <- w_S_MVO * E_R_S + w_B_MVO * E_R_B + w_C_MVO * E_R_C
sigma_MVO <- sqrt(w_S_MVO^2 * sigma_S^2 + w_B_MVO^2 * sigma_B^2 + 2 * w_S_MVO * w_B_MVO * cov_S_B)
points(sigma_MVO, E_R_MVO, col = "red", pch = 8, cex = 1.5)
text(sigma_MVO, E_R_MVO, labels = "MVO Strategy", pos = 1, col = "red")

### MVO+L strategy
w_S_LMVO <- 0.1404295
w_B_LMVO <- 0
w_C_LMVO <- 1.35957
E_R_LMVO <- w_S_LMVO * E_R_S + w_B_LMVO * E_R_B + w_C_LMVO * E_R_C
sigma_LMVO <- sqrt(w_S_LMVO^2 * sigma_S^2 + w_B_LMVO^2 * sigma_B^2 + 2 * w_S_LMVO * w_B_LMVO * cov_S_B)
points(sigma_LMVO, E_R_LMVO, col = "red", pch = 8, cex = 1.5)
text(sigma_LMVO, E_R_LMVO, labels = "LMVO Strategy", pos = 1, col = "red")

### LRP strategy
w_S_LRP <- 0.1439084
w_B_LRP <- 0.2135776
w_C_LRP <- 1.142514
E_R_LRP <- w_S_LRP * E_R_S + w_B_LRP * E_R_B + w_C_LRP * E_R_C
sigma_LRP <- sqrt(w_S_LRP^2 * sigma_S^2 + w_B_LRP^2 * sigma_B^2 + 2 * w_S_LRP * w_B_LRP * cov_S_B)
points(sigma_LRP, E_R_LRP, col = "red", pch = 8, cex = 1.5)
text(sigma_LRP, E_R_LRP, labels = "LRP Strategy", pos = 1, col = "red")


# Add grid for better visualization
grid()




# Create a data frame for the efficient frontier
eff_frontier_data <- data.frame(
  Volatility = port_volatilities,
  Expected_Return = port_returns,
  Type = ifelse(1:length(port_volatilities) %in% efficient_indices, "Efficient", "Inefficient")
)

# Create the ggplot
ggplot(eff_frontier_data, aes(x = Volatility, y = Expected_Return)) +
  #geom_point(data = subset(eff_frontier_data, Type == "Inefficient"), 
  #aes(color = "Inefficient Frontier"), alpha = 0.3, size = 1) +
  geom_point(data = subset(eff_frontier_data, Type == "Efficient"), 
             aes(color = "Efficient Frontier"), size = 1, alpha = 0.3) +
  geom_point(aes(x = gmv_volatility, y = gmv_return), color = "steelblue", size = 5, pch = 8) +
  geom_text(aes(x = gmv_volatility, y = gmv_return, label = "GMV Portfolio"), 
            hjust = 1.2, color = "steelblue") +
  geom_point(aes(x = sigma_6040, y = E_R_6040), color = "firebrick", size = 5, pch = 8) +
  geom_text(aes(x = sigma_6040, y = E_R_6040, label = "60/40/0 Strategy"), 
            vjust = -1, color = "firebrick") +
  geom_point(aes(x = sigma_MVO, y = E_R_MVO), color = "salmon", size = 5, pch = 8) +
  geom_text(aes(x = sigma_MVO, y = E_R_MVO, label = "MVO Strategy"), 
            vjust = -1, color = "salmon") +
  geom_point(aes(x = sigma_LMVO, y = E_R_LMVO), color = "darkgreen", size = 5, pch = 8) +
  geom_text(aes(x = sigma_LMVO, y = E_R_LMVO, label = "LMVO Strategy"), 
            vjust = -1, color = "darkgreen") +
  geom_point(aes(x = sigma_LRP, y = E_R_LRP), color = "purple", size = 5, pch = 8) +
  geom_text(aes(x = sigma_LRP, y = E_R_LRP, label = "LRP Strategy"), 
            vjust = -1, color = "purple") +
  labs(x = "Portfolio Volatility", 
       y = "Portfolio Expected Return", 
       title = "Efficient Frontier") +
  scale_color_manual(values = c("Inefficient Frontier" = "blue", "Efficient Frontier" = "blue")) +
  theme_minimal() +
  theme(legend.position = "none") +
  grid()  # Note: grid() may not be needed in ggplot as it is handled by the theme.










###################################### Financial Crisis Plot ########################################



# Define expected returns and volatilities 
E_R_C <- mean(FinancialCrisis$RF)   # Expected return for cash
E_R_S <- mean(FinancialCrisis$Equity)   # Expected return for stocks
E_R_B <- mean(FinancialCrisis$Bonds)   # Expected return for bonds
sigma_C <- sd(FinancialCrisis$RF)   # Volatility of cash
sigma_S <- sd(FinancialCrisis$Equity)   # Volatility of stocks
sigma_B <- sd(FinancialCrisis$Bonds)   # Volatility of bonds

# Calculate covariances
cov_S_B <- cov(FinancialCrisis$Equity, FinancialCrisis$Bonds, use = "pairwise.complete.obs")
cov_S_C <- cov(FinancialCrisis$Equity, FinancialCrisis$RF, use = "pairwise.complete.obs")
cov_B_C <- cov(FinancialCrisis$Bonds, FinancialCrisis$RF, use = "pairwise.complete.obs")

cov_matrix <- matrix(c(sigma_S^2, cov_S_B, cov_S_C,
                       cov_S_B, sigma_B^2, cov_B_C,
                       cov_S_C, cov_B_C, sigma_C^2), nrow = 3)

# Number of portfolios to simulate
n_portfolios <- 100

# Create a sequence of weights for stocks and bonds
w_S <- seq(0, 1, length.out = n_portfolios)

# Initialize vectors to store results
port_returns <- numeric(n_portfolios * n_portfolios)
port_volatilities <- numeric(n_portfolios * n_portfolios)
weights_stock <- numeric(n_portfolios * n_portfolios)
weights_bond <- numeric(n_portfolios * n_portfolios)
weights_cash <- numeric(n_portfolios * n_portfolios)

counter <- 1

# Loop through different combinations of weights for stocks, bonds, and cash
for (i in 1:n_portfolios) {
  for (j in 1:n_portfolios) {
    # Define weights for stocks, bonds, and cash
    w_S_i <- w_S[i]
    w_B_i <- w_S[j]
    w_C_i <- 1 - w_S_i - w_B_i  # Cash is the residual
    
    # Ensure weights sum to 1 and are non-negative
    if (w_C_i >= 0) {
      # Portfolio expected return
      port_returns[counter] <- w_S_i * E_R_S + w_B_i * E_R_B + w_C_i * E_R_C
      
      # Portfolio volatility (assuming zero covariance with cash)
      port_volatilities[counter] <- sqrt(w_S_i^2 * sigma_S^2 +
                                           w_B_i^2 * sigma_B^2 +
                                           2 * w_S_i * w_B_i * cov_S_B)
      
      # Store the weights for plotting later
      weights_stock[counter] <- w_S_i
      weights_bond[counter] <- w_B_i
      weights_cash[counter] <- w_C_i
      
      counter <- counter + 1
    }
  }
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
     main = "Efficient Frontier")

# Add the efficient frontier (above GMV) with a solid line
points(port_volatilities[efficient_indices], port_returns[efficient_indices], col = "blue", lwd = 2)

# Highlight the Global Minimum Variance Portfolio (GMV)
points(gmv_volatility, gmv_return, col = "green", pch = 16, cex = 1.5)
text(gmv_volatility, gmv_return, labels = "GMV Portfolio", pos = 4, col = "green")

### 60/40/0 strategy
w_S_6040 <- 0.6
w_B_6040 <- 0.4
w_C_6040 <- 0.0
E_R_6040 <- w_S_6040 * E_R_S + w_B_6040 * E_R_B + w_C_6040 * E_R_C
sigma_6040 <- sqrt(w_S_6040^2 * sigma_S^2 + w_B_6040^2 * sigma_B^2 + 2 * w_S_6040 * w_B_6040 * cov_S_B)
points(sigma_6040, E_R_6040, col = "red", pch = 8, cex = 1.5)
text(sigma_6040, E_R_6040, labels = "60/40/0 Strategy", pos = 1, col = "red")

### MVO strategy
w_S_MVO <- 
w_B_MVO <- 
w_C_MVO <- 
E_R_MVO <- w_S_MVO * E_R_S + w_B_MVO * E_R_B + w_C_MVO * E_R_C
sigma_MVO <- sqrt(w_S_MVO^2 * sigma_S^2 + w_B_MVO^2 * sigma_B^2 + 2 * w_S_MVO * w_B_MVO * cov_S_B)
points(sigma_MVO, E_R_MVO, col = "red", pch = 8, cex = 1.5)
text(sigma_MVO, E_R_MVO, labels = "MVO Strategy", pos = 1, col = "red")

### MVO+L strategy
w_S_LMVO <- 
w_B_LMVO <- 
w_C_LMVO <- 
E_R_LMVO <- w_S_LMVO * E_R_S + w_B_LMVO * E_R_B + w_C_LMVO * E_R_C
sigma_LMVO <- sqrt(w_S_LMVO^2 * sigma_S^2 + w_B_LMVO^2 * sigma_B^2 + 2 * w_S_LMVO * w_B_LMVO * cov_S_B)
points(sigma_LMVO, E_R_LMVO, col = "red", pch = 8, cex = 1.5)
text(sigma_LMVO, E_R_LMVO, labels = "LMVO Strategy", pos = 1, col = "red")

### LRP strategy
w_S_LRP <- 
w_B_LRP <- 
w_C_LRP <- 
E_R_LRP <- w_S_LRP * E_R_S + w_B_LRP * E_R_B + w_C_LRP * E_R_C
sigma_LRP <- sqrt(w_S_LRP^2 * sigma_S^2 + w_B_LRP^2 * sigma_B^2 + 2 * w_S_LRP * w_B_LRP * cov_S_B)
points(sigma_LRP, E_R_LRP, col = "red", pch = 8, cex = 1.5)
text(sigma_LRP, E_R_LRP, labels = "LRP Strategy", pos = 1, col = "red")


# Add grid for better visualization
grid()




# Create a data frame for the efficient frontier
eff_frontier_data <- data.frame(
  Volatility = port_volatilities,
  Expected_Return = port_returns,
  Type = ifelse(1:length(port_volatilities) %in% efficient_indices, "Efficient", "Inefficient")
)

# Create the ggplot
ggplot(eff_frontier_data, aes(x = Volatility, y = Expected_Return)) +
  #geom_point(data = subset(eff_frontier_data, Type == "Inefficient"), 
  #aes(color = "Inefficient Frontier"), alpha = 0.3, size = 1) +
  geom_point(data = subset(eff_frontier_data, Type == "Efficient"), 
             aes(color = "Efficient Frontier"), size = 1, alpha = 0.3) +
  geom_point(aes(x = gmv_volatility, y = gmv_return), color = "steelblue", size = 5, pch = 8) +
  geom_text(aes(x = gmv_volatility, y = gmv_return, label = "GMV Portfolio"), 
            hjust = 1.2, color = "steelblue") +
  geom_point(aes(x = sigma_6040, y = E_R_6040), color = "firebrick", size = 5, pch = 8) +
  geom_text(aes(x = sigma_6040, y = E_R_6040, label = "60/40/0 Strategy"), 
            vjust = -1, color = "firebrick") +
  geom_point(aes(x = sigma_MVO, y = E_R_MVO), color = "salmon", size = 5, pch = 8) +
  geom_text(aes(x = sigma_MVO, y = E_R_MVO, label = "MVO Strategy"), 
            vjust = -1, color = "salmon") +
  geom_point(aes(x = sigma_LMVO, y = E_R_LMVO), color = "darkgreen", size = 5, pch = 8) +
  geom_text(aes(x = sigma_LMVO, y = E_R_LMVO, label = "LMVO Strategy"), 
            vjust = -1, color = "darkgreen") +
  geom_point(aes(x = sigma_LRP, y = E_R_LRP), color = "purple", size = 5, pch = 8) +
  geom_text(aes(x = sigma_LRP, y = E_R_LRP, label = "LRP Strategy"), 
            vjust = -1, color = "purple") +
  labs(x = "Portfolio Volatility", 
       y = "Portfolio Expected Return", 
       title = "Efficient Frontier") +
  scale_color_manual(values = c("Inefficient Frontier" = "blue", "Efficient Frontier" = "blue")) +
  theme_minimal() +
  theme(legend.position = "none") +
  grid()  # Note: grid() may not be needed in ggplot as it is handled by the theme.







###################################### Great Recession Recovery Plot ########################################



# Define expected returns and volatilities 
E_R_C <- mean(GreatRecessionRecovery$RF)   # Expected return for cash
E_R_S <- mean(GreatRecessionRecovery$Equity)   # Expected return for stocks
E_R_B <- mean(GreatRecessionRecovery$Bonds)   # Expected return for bonds
sigma_C <- sd(GreatRecessionRecovery$RF)   # Volatility of cash
sigma_S <- sd(GreatRecessionRecovery$Equity)   # Volatility of stocks
sigma_B <- sd(GreatRecessionRecovery$Bonds)   # Volatility of bonds

# Calculate covariances
cov_S_B <- cov(GreatRecessionRecovery$Equity, GreatRecessionRecovery$Bonds, use = "pairwise.complete.obs")
cov_S_C <- cov(GreatRecessionRecovery$Equity, GreatRecessionRecovery$RF, use = "pairwise.complete.obs")
cov_B_C <- cov(GreatRecessionRecovery$Bonds, GreatRecessionRecovery$RF, use = "pairwise.complete.obs")

cov_matrix <- matrix(c(sigma_S^2, cov_S_B, cov_S_C,
                       cov_S_B, sigma_B^2, cov_B_C,
                       cov_S_C, cov_B_C, sigma_C^2), nrow = 3)

# Number of portfolios to simulate
n_portfolios <- 100

# Create a sequence of weights for stocks and bonds
w_S <- seq(0, 1, length.out = n_portfolios)

# Initialize vectors to store results
port_returns <- numeric(n_portfolios * n_portfolios)
port_volatilities <- numeric(n_portfolios * n_portfolios)
weights_stock <- numeric(n_portfolios * n_portfolios)
weights_bond <- numeric(n_portfolios * n_portfolios)
weights_cash <- numeric(n_portfolios * n_portfolios)

counter <- 1

# Loop through different combinations of weights for stocks, bonds, and cash
for (i in 1:n_portfolios) {
  for (j in 1:n_portfolios) {
    # Define weights for stocks, bonds, and cash
    w_S_i <- w_S[i]
    w_B_i <- w_S[j]
    w_C_i <- 1 - w_S_i - w_B_i  # Cash is the residual
    
    # Ensure weights sum to 1 and are non-negative
    if (w_C_i >= 0) {
      # Portfolio expected return
      port_returns[counter] <- w_S_i * E_R_S + w_B_i * E_R_B + w_C_i * E_R_C
      
      # Portfolio volatility (assuming zero covariance with cash)
      port_volatilities[counter] <- sqrt(w_S_i^2 * sigma_S^2 +
                                           w_B_i^2 * sigma_B^2 +
                                           2 * w_S_i * w_B_i * cov_S_B)
      
      # Store the weights for plotting later
      weights_stock[counter] <- w_S_i
      weights_bond[counter] <- w_B_i
      weights_cash[counter] <- w_C_i
      
      counter <- counter + 1
    }
  }
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
     main = "Efficient Frontier")

# Add the efficient frontier (above GMV) with a solid line
points(port_volatilities[efficient_indices], port_returns[efficient_indices], col = "blue", lwd = 2)

# Highlight the Global Minimum Variance Portfolio (GMV)
points(gmv_volatility, gmv_return, col = "green", pch = 16, cex = 1.5)
text(gmv_volatility, gmv_return, labels = "GMV Portfolio", pos = 4, col = "green")

### 60/40/0 strategy
w_S_6040 <- 0.6
w_B_6040 <- 0.4
w_C_6040 <- 0.0
E_R_6040 <- w_S_6040 * E_R_S + w_B_6040 * E_R_B + w_C_6040 * E_R_C
sigma_6040 <- sqrt(w_S_6040^2 * sigma_S^2 + w_B_6040^2 * sigma_B^2 + 2 * w_S_6040 * w_B_6040 * cov_S_B)
points(sigma_6040, E_R_6040, col = "red", pch = 8, cex = 1.5)
text(sigma_6040, E_R_6040, labels = "60/40/0 Strategy", pos = 1, col = "red")

### MVO strategy
w_S_MVO <- 
w_B_MVO <- 
w_C_MVO <- 
E_R_MVO <- w_S_MVO * E_R_S + w_B_MVO * E_R_B + w_C_MVO * E_R_C
sigma_MVO <- sqrt(w_S_MVO^2 * sigma_S^2 + w_B_MVO^2 * sigma_B^2 + 2 * w_S_MVO * w_B_MVO * cov_S_B)
points(sigma_MVO, E_R_MVO, col = "red", pch = 8, cex = 1.5)
text(sigma_MVO, E_R_MVO, labels = "MVO Strategy", pos = 1, col = "red")

### MVO+L strategy
w_S_LMVO <- 
w_B_LMVO <- 
w_C_LMVO <- 
E_R_LMVO <- w_S_LMVO * E_R_S + w_B_LMVO * E_R_B + w_C_LMVO * E_R_C
sigma_LMVO <- sqrt(w_S_LMVO^2 * sigma_S^2 + w_B_LMVO^2 * sigma_B^2 + 2 * w_S_LMVO * w_B_LMVO * cov_S_B)
points(sigma_LMVO, E_R_LMVO, col = "red", pch = 8, cex = 1.5)
text(sigma_LMVO, E_R_LMVO, labels = "LMVO Strategy", pos = 1, col = "red")

### LRP strategy
w_S_LRP <- 
w_B_LRP <- 
w_C_LRP <- 
E_R_LRP <- w_S_LRP * E_R_S + w_B_LRP * E_R_B + w_C_LRP * E_R_C
sigma_LRP <- sqrt(w_S_LRP^2 * sigma_S^2 + w_B_LRP^2 * sigma_B^2 + 2 * w_S_LRP * w_B_LRP * cov_S_B)
points(sigma_LRP, E_R_LRP, col = "red", pch = 8, cex = 1.5)
text(sigma_LRP, E_R_LRP, labels = "LRP Strategy", pos = 1, col = "red")


# Add grid for better visualization
grid()




# Create a data frame for the efficient frontier
eff_frontier_data <- data.frame(
  Volatility = port_volatilities,
  Expected_Return = port_returns,
  Type = ifelse(1:length(port_volatilities) %in% efficient_indices, "Efficient", "Inefficient")
)

# Create the ggplot
ggplot(eff_frontier_data, aes(x = Volatility, y = Expected_Return)) +
  #geom_point(data = subset(eff_frontier_data, Type == "Inefficient"), 
  #aes(color = "Inefficient Frontier"), alpha = 0.3, size = 1) +
  geom_point(data = subset(eff_frontier_data, Type == "Efficient"), 
             aes(color = "Efficient Frontier"), size = 1, alpha = 0.3) +
  geom_point(aes(x = gmv_volatility, y = gmv_return), color = "steelblue", size = 5, pch = 8) +
  geom_text(aes(x = gmv_volatility, y = gmv_return, label = "GMV Portfolio"), 
            hjust = 1.2, color = "steelblue") +
  geom_point(aes(x = sigma_6040, y = E_R_6040), color = "firebrick", size = 5, pch = 8) +
  geom_text(aes(x = sigma_6040, y = E_R_6040, label = "60/40/0 Strategy"), 
            vjust = -1, color = "firebrick") +
  geom_point(aes(x = sigma_MVO, y = E_R_MVO), color = "salmon", size = 5, pch = 8) +
  geom_text(aes(x = sigma_MVO, y = E_R_MVO, label = "MVO Strategy"), 
            vjust = -1, color = "salmon") +
  geom_point(aes(x = sigma_LMVO, y = E_R_LMVO), color = "darkgreen", size = 5, pch = 8) +
  geom_text(aes(x = sigma_LMVO, y = E_R_LMVO, label = "LMVO Strategy"), 
            vjust = -1, color = "darkgreen") +
  geom_point(aes(x = sigma_LRP, y = E_R_LRP), color = "purple", size = 5, pch = 8) +
  geom_text(aes(x = sigma_LRP, y = E_R_LRP, label = "LRP Strategy"), 
            vjust = -1, color = "purple") +
  labs(x = "Portfolio Volatility", 
       y = "Portfolio Expected Return", 
       title = "Efficient Frontier") +
  scale_color_manual(values = c("Inefficient Frontier" = "blue", "Efficient Frontier" = "blue")) +
  theme_minimal() +
  theme(legend.position = "none") +
  grid()  # Note: grid() may not be needed in ggplot as it is handled by the theme.







###################################### Covid Plot ########################################



# Define expected returns and volatilities 
E_R_C <- mean(Covid$RF)   # Expected return for cash
E_R_S <- mean(Covid$Equity)   # Expected return for stocks
E_R_B <- mean(Covid$Bonds)   # Expected return for bonds
sigma_C <- sd(Covid$RF)   # Volatility of cash
sigma_S <- sd(Covid$Equity)   # Volatility of stocks
sigma_B <- sd(Covid$Bonds)   # Volatility of bonds

# Calculate covariances
cov_S_B <- cov(Covid$Equity, Covid$Bonds, use = "pairwise.complete.obs")
cov_S_C <- cov(Covid$Equity, Covid$RF, use = "pairwise.complete.obs")
cov_B_C <- cov(Covid$Bonds, Covid$RF, use = "pairwise.complete.obs")

cov_matrix <- matrix(c(sigma_S^2, cov_S_B, cov_S_C,
                       cov_S_B, sigma_B^2, cov_B_C,
                       cov_S_C, cov_B_C, sigma_C^2), nrow = 3)

# Number of portfolios to simulate
n_portfolios <- 100

# Create a sequence of weights for stocks and bonds
w_S <- seq(0, 1, length.out = n_portfolios)

# Initialize vectors to store results
port_returns <- numeric(n_portfolios * n_portfolios)
port_volatilities <- numeric(n_portfolios * n_portfolios)
weights_stock <- numeric(n_portfolios * n_portfolios)
weights_bond <- numeric(n_portfolios * n_portfolios)
weights_cash <- numeric(n_portfolios * n_portfolios)

counter <- 1

# Loop through different combinations of weights for stocks, bonds, and cash
for (i in 1:n_portfolios) {
  for (j in 1:n_portfolios) {
    # Define weights for stocks, bonds, and cash
    w_S_i <- w_S[i]
    w_B_i <- w_S[j]
    w_C_i <- 1 - w_S_i - w_B_i  # Cash is the residual
    
    # Ensure weights sum to 1 and are non-negative
    if (w_C_i >= 0) {
      # Portfolio expected return
      port_returns[counter] <- w_S_i * E_R_S + w_B_i * E_R_B + w_C_i * E_R_C
      
      # Portfolio volatility (assuming zero covariance with cash)
      port_volatilities[counter] <- sqrt(w_S_i^2 * sigma_S^2 +
                                           w_B_i^2 * sigma_B^2 +
                                           2 * w_S_i * w_B_i * cov_S_B)
      
      # Store the weights for plotting later
      weights_stock[counter] <- w_S_i
      weights_bond[counter] <- w_B_i
      weights_cash[counter] <- w_C_i
      
      counter <- counter + 1
    }
  }
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
     main = "Efficient Frontier")

# Add the efficient frontier (above GMV) with a solid line
points(port_volatilities[efficient_indices], port_returns[efficient_indices], col = "blue", lwd = 2)

# Highlight the Global Minimum Variance Portfolio (GMV)
points(gmv_volatility, gmv_return, col = "green", pch = 16, cex = 1.5)
text(gmv_volatility, gmv_return, labels = "GMV Portfolio", pos = 4, col = "green")

### 60/40/0 strategy
w_S_6040 <- 0.6
w_B_6040 <- 0.4
w_C_6040 <- 0.0
E_R_6040 <- w_S_6040 * E_R_S + w_B_6040 * E_R_B + w_C_6040 * E_R_C
sigma_6040 <- sqrt(w_S_6040^2 * sigma_S^2 + w_B_6040^2 * sigma_B^2 + 2 * w_S_6040 * w_B_6040 * cov_S_B)
points(sigma_6040, E_R_6040, col = "red", pch = 8, cex = 1.5)
text(sigma_6040, E_R_6040, labels = "60/40/0 Strategy", pos = 1, col = "red")

### MVO strategy
w_S_MVO <- 
w_B_MVO <- 
w_C_MVO <- 
E_R_MVO <- w_S_MVO * E_R_S + w_B_MVO * E_R_B + w_C_MVO * E_R_C
sigma_MVO <- sqrt(w_S_MVO^2 * sigma_S^2 + w_B_MVO^2 * sigma_B^2 + 2 * w_S_MVO * w_B_MVO * cov_S_B)
points(sigma_MVO, E_R_MVO, col = "red", pch = 8, cex = 1.5)
text(sigma_MVO, E_R_MVO, labels = "MVO Strategy", pos = 1, col = "red")

### MVO+L strategy
w_S_LMVO <- 
w_B_LMVO <- 
w_C_LMVO <- 
E_R_LMVO <- w_S_LMVO * E_R_S + w_B_LMVO * E_R_B + w_C_LMVO * E_R_C
sigma_LMVO <- sqrt(w_S_LMVO^2 * sigma_S^2 + w_B_LMVO^2 * sigma_B^2 + 2 * w_S_LMVO * w_B_LMVO * cov_S_B)
points(sigma_LMVO, E_R_LMVO, col = "red", pch = 8, cex = 1.5)
text(sigma_LMVO, E_R_LMVO, labels = "LMVO Strategy", pos = 1, col = "red")

### LRP strategy
w_S_LRP <- 
w_B_LRP <- 
w_C_LRP <- 
E_R_LRP <- w_S_LRP * E_R_S + w_B_LRP * E_R_B + w_C_LRP * E_R_C
sigma_LRP <- sqrt(w_S_LRP^2 * sigma_S^2 + w_B_LRP^2 * sigma_B^2 + 2 * w_S_LRP * w_B_LRP * cov_S_B)
points(sigma_LRP, E_R_LRP, col = "red", pch = 8, cex = 1.5)
text(sigma_LRP, E_R_LRP, labels = "LRP Strategy", pos = 1, col = "red")


# Add grid for better visualization
grid()




# Create a data frame for the efficient frontier
eff_frontier_data <- data.frame(
  Volatility = port_volatilities,
  Expected_Return = port_returns,
  Type = ifelse(1:length(port_volatilities) %in% efficient_indices, "Efficient", "Inefficient")
)

# Create the ggplot
ggplot(eff_frontier_data, aes(x = Volatility, y = Expected_Return)) +
  #geom_point(data = subset(eff_frontier_data, Type == "Inefficient"), 
  #aes(color = "Inefficient Frontier"), alpha = 0.3, size = 1) +
  geom_point(data = subset(eff_frontier_data, Type == "Efficient"), 
             aes(color = "Efficient Frontier"), size = 1, alpha = 0.3) +
  geom_point(aes(x = gmv_volatility, y = gmv_return), color = "steelblue", size = 5, pch = 8) +
  geom_text(aes(x = gmv_volatility, y = gmv_return, label = "GMV Portfolio"), 
            hjust = 1.2, color = "steelblue") +
  geom_point(aes(x = sigma_6040, y = E_R_6040), color = "firebrick", size = 5, pch = 8) +
  geom_text(aes(x = sigma_6040, y = E_R_6040, label = "60/40/0 Strategy"), 
            vjust = -1, color = "firebrick") +
  geom_point(aes(x = sigma_MVO, y = E_R_MVO), color = "salmon", size = 5, pch = 8) +
  geom_text(aes(x = sigma_MVO, y = E_R_MVO, label = "MVO Strategy"), 
            vjust = -1, color = "salmon") +
  geom_point(aes(x = sigma_LMVO, y = E_R_LMVO), color = "darkgreen", size = 5, pch = 8) +
  geom_text(aes(x = sigma_LMVO, y = E_R_LMVO, label = "LMVO Strategy"), 
            vjust = -1, color = "darkgreen") +
  geom_point(aes(x = sigma_LRP, y = E_R_LRP), color = "purple", size = 5, pch = 8) +
  geom_text(aes(x = sigma_LRP, y = E_R_LRP, label = "LRP Strategy"), 
            vjust = -1, color = "purple") +
  labs(x = "Portfolio Volatility", 
       y = "Portfolio Expected Return", 
       title = "Efficient Frontier") +
  scale_color_manual(values = c("Inefficient Frontier" = "blue", "Efficient Frontier" = "blue")) +
  theme_minimal() +
  theme(legend.position = "none") +
  grid()  # Note: grid() may not be needed in ggplot as it is handled by the theme.
































####################################################################################################################
####################################################################################################################
#------------------------------- Cumulative Returns ----------------------------------------------------------------
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
                     Date >= as.Date("1990-01-01") & Date <= as.Date("1999-12-01"))
row.names(Roaring90s) <- NULL
Roaring90s$Date <- format(Roaring90s$Date, "%Y-%m")

### 200201 - 200912
FinancialCrisis <- subset(FullPeriod,
                          Date >= as.Date("2000-01-01") & Date <= as.Date("2009-12-01"))
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


### 60/40/0 strategy
w_S_6040 <- 0.6
w_B_6040 <- 0.4
w_C_6040 <- 0.0

### MVO strategy
w_S_MVO <- 0.7127203
w_B_MVO <- 0.2872797
w_C_MVO <- 0

### MVO+L strategy
w_S_LMVO <- 0.5533951
w_B_LMVO <- 0.3704708
w_C_LMVO <- 0.5761342

### LRP strategy
w_S_LRP <- 0.484716
w_B_LRP <- 0.9317936
w_C_LRP <- 0.08349038

# Backtest: 60% in RF and 40% in Bonds
FullPeriod <- FullPeriod %>%
  mutate(Strategy_60_40 = w_S_6040 * Equity + w_B_6040 * Bonds + w_C_6040 * RF, ### 60/40
         Cum_Return_60_40 = cumprod(1 + Strategy_60_40), ### 60/40
         Log_Cum_Return_60_40 = log(Cum_Return_60_40), ### 60/40
         Strategy_MVO = w_S_MVO * Equity + w_B_MVO * Bonds + w_C_MVO * RF, ### MVO
         Cum_Return_MVO = cumprod(1 + Strategy_MVO), ### MVO
         Log_Cum_Return_MVO = log(Cum_Return_MVO), ### MVO
         Strategy_LMVO = w_S_LMVO * Equity + w_B_LMVO * Bonds + w_C_LMVO * RF, ### MVO+L
         Cum_Return_LMVO = cumprod(1 + Strategy_LMVO), ### MVO+L
         Log_Cum_Return_LMVO = log(Cum_Return_LMVO), ### MVO+L
         Strategy_LRP = w_S_LRP * Equity + w_B_LRP * Bonds + w_C_LRP * RF, ### LRP
         Cum_Return_LRP = cumprod(1 + Strategy_LRP), ### LRP
         Log_Cum_Return_LRP = log(Cum_Return_LRP)) ### LRP


# Plot the cumulative return with manually specified breaks
ggplot(FullPeriod, aes(x = Date)) +
  ### 60/40
  geom_line(aes(y = Log_Cum_Return_60_40, group = 1), color = "blue") +
  geom_point(aes(y = Log_Cum_Return_60_40), color = "blue") +
  ### MVO
  geom_line(aes(y = Log_Cum_Return_MVO, group = 2), color = "red") +
  geom_point(aes(y = Log_Cum_Return_MVO), color = "red") +
  ### MVO+L
  geom_line(aes(y = Log_Cum_Return_LMVO, group = 2), color = "pink") +
  geom_point(aes(y = Log_Cum_Return_LMVO), color = "pink") +
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
  annotate(geom="text", x="2015-01", y=1.7, label=paste("LRP"),
           color="darkgreen", size = 7) +
  annotate(geom="text", x="2015-01", y=3, label=paste("MVO+L"),
           color="pink", size = 7)




###################################### Roaring 90s Plot ########################################

# Convert Date to factor to keep it as "YYYY-MM"
Roaring90s$Date <- factor(Roaring90s$Date)
# Get labels for every 5th year (e.g., "1990-01", "1995-01", etc.)
Roaring90sBreaks <- Roaring90s$Date[seq(1, length(Roaring90s$Date), by = 12 * 1)]  # every 5 years


### 60/40/0 strategy
w_S_6040 <- 0.6
w_B_6040 <- 0.4
w_C_6040 <- 0.0

### MVO strategy
w_S_MVO <- 0.3312331
w_B_MVO <- 0
w_C_MVO <- 0.6687669

### MVO+L strategy
w_S_LMVO <- 0.1404295
w_B_LMVO <- 0
w_C_LMVO <- 1.35957

### LRP strategy
w_S_LRP <- 0.1439084
w_B_LRP <- 0.2135776
w_C_LRP <- 1.142514


# Backtest: 60% in RF and 40% in Bonds
Roaring90s <- Roaring90s %>%
  mutate(Strategy_60_40 = w_S_6040 * Equity + w_B_6040 * Bonds + w_C_6040 * RF, ### 60/40
         Cum_Return_60_40 = cumprod(1 + Strategy_60_40), ### 60/40
         Log_Cum_Return_60_40 = log(Cum_Return_60_40), ### 60/40
         Strategy_MVO = w_S_MVO * Equity + w_B_MVO * Bonds + w_C_MVO * RF, ### MVO
         Cum_Return_MVO = cumprod(1 + Strategy_MVO), ### MVO
         Log_Cum_Return_MVO = log(Cum_Return_MVO), ### MVO
         Strategy_LMVO = w_S_LMVO * Equity + w_B_LMVO * Bonds + w_C_LMVO * RF, ### MVO+L
         Cum_Return_LMVO = cumprod(1 + Strategy_LMVO), ### MVO+L
         Log_Cum_Return_LMVO = log(Cum_Return_LMVO), ### MVO+L
         Strategy_LRP = w_S_LRP * Equity + w_B_LRP * Bonds + w_C_LRP * RF, ### LRP
         Cum_Return_LRP = cumprod(1 + Strategy_LRP), ### LRP
         Log_Cum_Return_LRP = log(Cum_Return_LRP)) ### LRP


# Plot the cumulative return with manually specified breaks
ggplot(Roaring90s, aes(x = Date)) +
  ### 60/40
  geom_line(aes(y = Log_Cum_Return_60_40, group = 1), color = "blue") +
  geom_point(aes(y = Log_Cum_Return_60_40), color = "blue") +
  ### MVO
  geom_line(aes(y = Log_Cum_Return_MVO, group = 2), color = "red") +
  geom_point(aes(y = Log_Cum_Return_MVO), color = "red") +
  ### MVO+L
  geom_line(aes(y = Log_Cum_Return_LMVO, group = 2), color = "pink") +
  geom_point(aes(y = Log_Cum_Return_LMVO), color = "pink") +
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
           color="pink", size = 7) +
  annotate(geom="text", x="1997-01", y=1.7, label=paste("LRP"),
           color="darkgreen", size = 7)










###################################### Financial Crisis Plot ########################################

# Convert Date to factor to keep it as "YYYY-MM"
FinancialCrisis$Date <- factor(FinancialCrisis$Date)
# Get labels for every 5th year (e.g., "1990-01", "1995-01", etc.)
FinancialCrisisBreaks <- FinancialCrisis$Date[seq(1, length(FinancialCrisis$Date), by = 12 * 1)]  # every 5 years

### 60/40/0 strategy
w_S_6040 <- 0.6
w_B_6040 <- 0.4
w_C_6040 <- 0.0

### MVO strategy
w_S_MVO <- 
w_B_MVO <- 
w_C_MVO <- 

### MVO+L strategy
w_S_LMVO <- 
w_B_LMVO <- 
w_C_LMVO <- 

### LRP strategy
w_S_LRP <- 
w_B_LRP <- 
w_C_LRP <- 


# Backtest: 60% in RF and 40% in Bonds
FinancialCrisis <- FinancialCrisis %>%
  mutate(Strategy_60_40 = w_S_6040 * Equity + w_B_6040 * Bonds + w_C_6040 * RF, ### 60/40
         Cum_Return_60_40 = cumprod(1 + Strategy_60_40), ### 60/40
         Log_Cum_Return_60_40 = log(Cum_Return_60_40), ### 60/40
         Strategy_MVO = w_S_MVO * Equity + w_B_MVO * Bonds + w_C_MVO * RF, ### MVO
         Cum_Return_MVO = cumprod(1 + Strategy_MVO), ### MVO
         Log_Cum_Return_MVO = log(Cum_Return_MVO), ### MVO
         Strategy_LMVO = w_S_LMVO * Equity + w_B_LMVO * Bonds + w_C_LMVO * RF, ### MVO+L
         Cum_Return_LMVO = cumprod(1 + Strategy_LMVO), ### MVO+L
         Log_Cum_Return_LMVO = log(Cum_Return_LMVO), ### MVO+L
         Strategy_LRP = w_S_LRP * Equity + w_B_LRP * Bonds + w_C_LRP * RF, ### LRP
         Cum_Return_LRP = cumprod(1 + Strategy_LRP), ### LRP
         Log_Cum_Return_LRP = log(Cum_Return_LRP)) ### LRP


# Plot the cumulative return with manually specified breaks
ggplot(FinancialCrisis, aes(x = Date)) +
  ### 60/40
  geom_line(aes(y = Log_Cum_Return_60_40, group = 1), color = "blue") +
  geom_point(aes(y = Log_Cum_Return_60_40), color = "blue") +
  ### MVO
  geom_line(aes(y = Log_Cum_Return_MVO, group = 2), color = "red") +
  geom_point(aes(y = Log_Cum_Return_MVO), color = "red") +
  ### MVO+L
  geom_line(aes(y = Log_Cum_Return_LMVO, group = 2), color = "pink") +
  geom_point(aes(y = Log_Cum_Return_LMVO), color = "pink") +
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
           color="pink", size = 7) +
  annotate(geom="text", x="2006-01", y=1.7, label=paste("LRP"),
           color="darkgreen", size = 7)






###################################### Great Recession Recovery Plot ########################################

# Convert Date to factor to keep it as "YYYY-MM"
GreatRecessionRecovery$Date <- factor(GreatRecessionRecovery$Date)
# Get labels for every 5th year (e.g., "1990-01", "1995-01", etc.)
GreatRecessionRecoveryBreaks <- GreatRecessionRecovery$Date[seq(1, length(GreatRecessionRecovery$Date), by = 12 * 1)]  # every 5 years


### 60/40/0 strategy
w_S_6040 <- 0.6
w_B_6040 <- 0.4
w_C_6040 <- 0.0

### MVO strategy
w_S_MVO <- 
w_B_MVO <- 
w_C_MVO <- 
  
### MVO+L strategy
w_S_LMVO <- 
w_B_LMVO <- 
w_C_LMVO <- 
  
### LRP strategy
w_S_LRP <- 
w_B_LRP <- 
w_C_LRP <- 

# Backtest: 60% in RF and 40% in Bonds
GreatRecessionRecovery <- GreatRecessionRecovery %>%
  mutate(Strategy_60_40 = w_S_6040 * Equity + w_B_6040 * Bonds + w_C_6040 * RF, ### 60/40
         Cum_Return_60_40 = cumprod(1 + Strategy_60_40), ### 60/40
         Log_Cum_Return_60_40 = log(Cum_Return_60_40), ### 60/40
         Strategy_MVO = w_S_MVO * Equity + w_B_MVO * Bonds + w_C_MVO * RF, ### MVO
         Cum_Return_MVO = cumprod(1 + Strategy_MVO), ### MVO
         Log_Cum_Return_MVO = log(Cum_Return_MVO), ### MVO
         Strategy_LMVO = w_S_LMVO * Equity + w_B_LMVO * Bonds + w_C_LMVO * RF, ### MVO+L
         Cum_Return_LMVO = cumprod(1 + Strategy_LMVO), ### MVO+L
         Log_Cum_Return_LMVO = log(Cum_Return_LMVO), ### MVO+L
         Strategy_LRP = w_S_LRP * Equity + w_B_LRP * Bonds + w_C_LRP * RF, ### LRP
         Cum_Return_LRP = cumprod(1 + Strategy_LRP), ### LRP
         Log_Cum_Return_LRP = log(Cum_Return_LRP)) ### LRP


# Plot the cumulative return with manually specified breaks
ggplot(GreatRecessionRecovery, aes(x = Date)) +
  ### 60/40
  geom_line(aes(y = Log_Cum_Return_60_40, group = 1), color = "blue") +
  geom_point(aes(y = Log_Cum_Return_60_40), color = "blue") +
  ### MVO
  geom_line(aes(y = Log_Cum_Return_MVO, group = 2), color = "red") +
  geom_point(aes(y = Log_Cum_Return_MVO), color = "red") +
  ### MVO+L
  geom_line(aes(y = Log_Cum_Return_LMVO, group = 2), color = "pink") +
  geom_point(aes(y = Log_Cum_Return_LMVO), color = "pink") +
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
           color="pink", size = 7) +
  annotate(geom="text", x="2018-01", y=1.7, label=paste("LRP"),
           color="darkgreen", size = 7)





###################################### Covid Plot ########################################

# Convert Date to factor to keep it as "YYYY-MM"
Covid$Date <- factor(Covid$Date)
# Get labels for every 5th year (e.g., "1990-01", "1995-01", etc.)
CovidBreaks <- Covid$Date[seq(1, length(Covid$Date), by = 12 * 1)]  # every 5 years

### 60/40/0 strategy
w_S_6040 <- 0.6
w_B_6040 <- 0.4
w_C_6040 <- 0.0

### MVO strategy
w_S_MVO <- 
w_B_MVO <- 
w_C_MVO <- 
  
### MVO+L strategy
w_S_LMVO <- 
w_B_LMVO <- 
w_C_LMVO <- 
  
### LRP strategy
w_S_LRP <- 
w_B_LRP <- 
w_C_LRP <- 
  
# Backtest: 60% in RF and 40% in Bonds
Covid <- Covid %>%
  mutate(Strategy_60_40 = w_S_6040 * Equity + w_B_6040 * Bonds + w_C_6040 * RF, ### 60/40
         Cum_Return_60_40 = cumprod(1 + Strategy_60_40), ### 60/40
         Log_Cum_Return_60_40 = log(Cum_Return_60_40), ### 60/40
         Strategy_MVO = w_S_MVO * Equity + w_B_MVO * Bonds + w_C_MVO * RF, ### MVO
         Cum_Return_MVO = cumprod(1 + Strategy_MVO), ### MVO
         Log_Cum_Return_MVO = log(Cum_Return_MVO), ### MVO
         Strategy_LMVO = w_S_LMVO * Equity + w_B_LMVO * Bonds + w_C_LMVO * RF, ### MVO+L
         Cum_Return_LMVO = cumprod(1 + Strategy_LMVO), ### MVO+L
         Log_Cum_Return_LMVO = log(Cum_Return_LMVO), ### MVO+L
         Strategy_LRP = w_S_LRP * Equity + w_B_LRP * Bonds + w_C_LRP * RF, ### LRP
         Cum_Return_LRP = cumprod(1 + Strategy_LRP), ### LRP
         Log_Cum_Return_LRP = log(Cum_Return_LRP)) ### LRP


# Plot the cumulative return with manually specified breaks
ggplot(Covid, aes(x = Date)) +
  ### 60/40
  geom_line(aes(y = Log_Cum_Return_60_40, group = 1), color = "blue") +
  geom_point(aes(y = Log_Cum_Return_60_40), color = "blue") +
  ### MVO
  geom_line(aes(y = Log_Cum_Return_MVO, group = 2), color = "red") +
  geom_point(aes(y = Log_Cum_Return_MVO), color = "red") +
  ### MVO+L
  geom_line(aes(y = Log_Cum_Return_LMVO, group = 2), color = "pink") +
  geom_point(aes(y = Log_Cum_Return_LMVO), color = "pink") +
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
           color="pink", size = 7) +
  annotate(geom="text", x="2022-01", y=1.7, label=paste("LRP"),
           color="darkgreen", size = 7)



