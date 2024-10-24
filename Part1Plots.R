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

### 199001 - 199912
Roaring90s <- subset(FullPeriod,
                     Date >= as.Date("1990-01-01") & Date <= as.Date("1999-12-01"))
row.names(Roaring90s) <- NULL
Roaring90s$Date <- format(Roaring90s$Date, "%Y-%m")

### 200001 - 201112
FinancialCrisis <- subset(FullPeriod,
                          Date >= as.Date("2000-01-01") & Date <= as.Date("2011-12-01"))
row.names(FinancialCrisis) <- NULL
FinancialCrisis$Date <- format(FinancialCrisis$Date, "%Y-%m")

### 201201 - 201812
GreatRecessionRecovery <- subset(FullPeriod,
                                 Date >= as.Date("2012-01-01") & Date <= as.Date("2018-12-01"))
row.names(GreatRecessionRecovery) <- NULL
GreatRecessionRecovery$Date <- format(GreatRecessionRecovery$Date, "%Y-%m")

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
min_variance_pos <- which.min(port_volatilities[port_volatilities > 0])
min_variance_idx <- which(port_volatilities == port_volatilities[port_volatilities > 0][min_variance_pos])

# Extract the return and volatility of the GMV portfolio
gmv_return <- port_returns[min_variance_idx]
gmv_volatility <- port_volatilities[min_variance_idx]

# Split portfolios into efficient (above GMV) and inefficient (below GMV)
efficient_indices <- which(port_returns >= gmv_return)
inefficient_indices <- which(port_returns < gmv_return)

# Plot the efficient frontier
plot(port_volatilities[efficient_indices], port_returns[efficient_indices], col = "blue", lwd = 2,
     ylim = c(min(port_returns), max(port_returns)),
     xlim = c(min(port_volatilities), max(port_volatilities)),
     xlab = "Portfolio Volatility", ylab = "Portfolio Expected Return",
     main = "Efficient Frontier")

# Highlight the Global Minimum Variance Portfolio (GMV)
points(gmv_volatility, gmv_return, col = "green", pch = 8, cex = 1.5)
text(gmv_volatility, gmv_return, labels = "GMV Portfolio", pos = 4, col = "green")

### 60/40/0 strategy
w_S_6040 <- 0.6
w_B_6040 <- 0.4
w_C_6040 <- 0.0
E_R_6040 <- w_S_6040 * E_R_S + w_B_6040 * E_R_B + w_C_6040 * E_R_C
sigma_6040 <- sqrt(w_S_6040^2 * sigma_S^2 + w_B_6040^2 * sigma_B^2 + 2 * w_S_6040 * w_B_6040 * cov_S_B)
#E_R_6040 <- 0.00679
#sigma_6040 <- 0.02777
points(sigma_6040, E_R_6040, col = "red", pch = 8, cex = 1.5)
text(sigma_6040, E_R_6040, labels = "60/40/0 Strategy", pos = 1, col = "red")

### Market strategy
w_S_VW <- 1
w_B_VW <- 0.0
w_C_VW <- 0.0
E_R_VW <- w_S_VW * E_R_S + w_B_VW * E_R_B + w_C_VW * E_R_C
sigma_VW <- sqrt(w_S_VW^2 * sigma_S^2 + w_B_VW^2 * sigma_B^2 + 2 * w_S_VW * w_B_VW * cov_S_B)
#E_R_VW <- 0.00931
#sigma_VW <- 0.04410
points(sigma_VW, E_R_VW, col = "red", pch = 8, cex = 1.5)
text(sigma_VW, E_R_VW, labels = "Value-Weighted Strategy", pos = 1, col = "red")

### MVO strategy
w_S_MVO <- 0.7127
w_B_MVO <- 0.2873
w_C_MVO <- 0
E_R_MVO <- 0.0075
sigma_MVO <- 0.03159
#E_R_MVO <- w_S_MVO * E_R_S + w_B_MVO * E_R_B + w_C_MVO * E_R_C
#sigma_MVO <- sqrt(w_S_MVO^2 * sigma_S^2 + w_B_MVO^2 * sigma_B^2 + 2 * w_S_MVO * w_B_MVO * cov_S_B)
points(sigma_MVO, E_R_MVO, col = "red", pch = 8, cex = 1.5)
text(sigma_MVO, E_R_MVO, labels = "MVO Strategy", pos = 1, col = "red")

### MVO+L strategy
w_S_LMVO <- 0.5534
w_B_LMVO <- 0.3705
w_C_LMVO <- 0.5761
E_R_LMVO <- 0.0075
sigma_LMVO <- 0.02515
#E_R_LMVO <- w_S_LMVO * E_R_S + w_B_LMVO * E_R_B + w_C_LMVO * E_R_C
#sigma_LMVO <- sqrt(w_S_LMVO^2 * sigma_S^2 + w_B_LMVO^2 * sigma_B^2 + 2 * w_S_LMVO * w_B_LMVO * cov_S_B)
points(sigma_LMVO, E_R_LMVO, col = "red", pch = 8, cex = 1.5)
text(sigma_LMVO, E_R_LMVO, labels = "LMVO Strategy", pos = 1, col = "red")

### LRP strategy
w_S_LRP <- 0.4847
w_B_LRP <- 0.9318
w_C_LRP <- 0.0835
E_R_LRP <- 0.0075
sigma_LRP <- 0.02805
#E_R_LRP <- w_S_LRP * E_R_S + w_B_LRP * E_R_B + w_C_LRP * E_R_C
#sigma_LRP <- sqrt(w_S_LRP^2 * sigma_S^2 + w_B_LRP^2 * sigma_B^2 + 2 * w_S_LRP * w_B_LRP * cov_S_B)
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
            aes(color = "Efficient Frontier"), size = 3, alpha = 0.3) +
  geom_point(aes(x = gmv_volatility, y = gmv_return), color = "steelblue", size = 13, pch = 8) +
  geom_text(aes(x = gmv_volatility, y = gmv_return, label = "GMV"), 
            hjust = -0.3, color = "steelblue", size = 13) +
  geom_point(aes(x = sigma_6040, y = E_R_6040), color = "firebrick", size = 13, pch = 8) +
  geom_text(aes(x = sigma_6040, y = E_R_6040, label = "60/40"), 
            vjust = 2, color = "firebrick", size = 13) +
  geom_point(aes(x = sigma_VW, y = E_R_VW), color = "orange", size = 13, pch = 8) +
  geom_text(aes(x = sigma_VW, y = E_R_VW, label = "VW"), 
            vjust = 2, color = "orange", size = 13) +
  geom_point(aes(x = sigma_MVO, y = E_R_MVO), color = "salmon", size = 13, pch = 8) +
  geom_text(aes(x = sigma_MVO, y = E_R_MVO, label = "MVO"), 
            hjust = -0.3, color = "salmon", size = 13) +
  geom_point(aes(x = sigma_LMVO, y = E_R_LMVO), color = "darkgreen", size = 13, pch = 8) +
  geom_text(aes(x = sigma_LMVO, y = E_R_LMVO, label = "LMVO"), 
            hjust = 1.3, color = "darkgreen", size = 13) +
  geom_point(aes(x = sigma_LRP, y = E_R_LRP), color = "purple", size = 13, pch = 8) +
  geom_text(aes(x = sigma_LRP, y = E_R_LRP, label = "LRP"), 
            vjust = -1, color = "purple", size = 13) +
  labs(x = "Portfolio Volatility", 
       y = "Portfolio Expected Return", 
       title = "Efficient Frontier") +
  scale_color_manual(values = c("Inefficient Frontier" = "#666666", "Efficient Frontier" = "#666666")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 27),  # Title size
    axis.title.x = element_text(size = 23),  # X-axis label size
    axis.title.y = element_text(size = 23),  # Y-axis label size
    axis.text.x = element_text(size = 17),   # X-axis tick label size
    axis.text.y = element_text(size = 17)
  ) +
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
min_variance_pos <- which.min(port_volatilities[port_volatilities > 0])
min_variance_idx <- which(port_volatilities == port_volatilities[port_volatilities > 0][min_variance_pos])

# Extract the return and volatility of the GMV portfolio
gmv_return <- port_returns[min_variance_idx]
gmv_volatility <- port_volatilities[min_variance_idx]

# Split portfolios into efficient (above GMV) and inefficient (below GMV)
efficient_indices <- which(port_returns >= gmv_return)
inefficient_indices <- which(port_returns < gmv_return)

# Plot the efficient frontier
plot(port_volatilities[efficient_indices], port_returns[efficient_indices], col = "blue", lwd = 2,
     ylim = c(min(port_returns), max(port_returns)),
     xlim = c(min(port_volatilities), max(port_volatilities)),
     xlab = "Portfolio Volatility", ylab = "Portfolio Expected Return",
     main = "Efficient Frontier")

# Highlight the Global Minimum Variance Portfolio (GMV)
points(gmv_volatility, gmv_return, col = "green", pch = 8, cex = 1.5)
text(gmv_volatility, gmv_return, labels = "GMV Portfolio", pos = 4, col = "green")

### 60/40/0 strategy
w_S_6040 <- 0.60
w_B_6040 <- 0.40
w_C_6040 <- 0
E_R_6040 <- 0.01026
sigma_6040 <- 0.02705
#E_R_6040 <- w_S_6040 * E_R_S + w_B_6040 * E_R_B + w_C_6040 * E_R_C
#sigma_6040 <- sqrt(w_S_6040^2 * sigma_S^2 + w_B_6040^2 * sigma_B^2 + 2 * w_S_6040 * w_B_6040 * cov_S_B)
points(sigma_6040, E_R_6040, col = "red", pch = 8, cex = 1.5)
text(sigma_6040, E_R_6040, labels = "60/40/0 Strategy", pos = 1, col = "red")

### Market strategy
w_S_VW <- 1
w_B_VW <- 0.0
w_C_VW <- 0.0
E_R_VW <- w_S_VW * E_R_S + w_B_VW * E_R_B + w_C_VW * E_R_C
sigma_VW <- sqrt(w_S_VW^2 * sigma_S^2 + w_B_VW^2 * sigma_B^2 + 2 * w_S_VW * w_B_VW * cov_S_B)
#E_R_VW <- 0.00931
#sigma_VW <- 0.04410
points(sigma_VW, E_R_VW, col = "red", pch = 8, cex = 1.5)
text(sigma_VW, E_R_VW, labels = "Value-Weighted Strategy", pos = 1, col = "red")


### MVO strategy
w_S_MVO <- 0.3312
w_B_MVO <- 0
w_C_MVO <- 0.6688
E_R_MVO <- 0.0075
sigma_MVO <- 0.01306
#E_R_MVO <- w_S_MVO * E_R_S + w_B_MVO * E_R_B + w_C_MVO * E_R_C
#sigma_MVO <- sqrt(w_S_MVO^2 * sigma_S^2 + w_B_MVO^2 * sigma_B^2 + 2 * w_S_MVO * w_B_MVO * cov_S_B)
points(sigma_MVO, E_R_MVO, col = "red", pch = 8, cex = 1.5)
text(sigma_MVO, E_R_MVO, labels = "MVO Strategy", pos = 1, col = "red")

### MVO+L strategy
w_S_LMVO <- 0.1404
w_B_LMVO <- 0
w_C_LMVO <- 1.3596
E_R_LMVO <- 0.0075
sigma_LMVO <- 0.00572
#E_R_LMVO <- w_S_LMVO * E_R_S + w_B_LMVO * E_R_B + w_C_LMVO * E_R_C
#sigma_LMVO <- sqrt(w_S_LMVO^2 * sigma_S^2 + w_B_LMVO^2 * sigma_B^2 + 2 * w_S_LMVO * w_B_LMVO * cov_S_B)
points(sigma_LMVO, E_R_LMVO, col = "red", pch = 8, cex = 1.5)
text(sigma_LMVO, E_R_LMVO, labels = "LMVO Strategy", pos = 1, col = "red")

### LRP strategy
w_S_LRP <- 0.1439
w_B_LRP <- 0.2136
w_C_LRP <- 1.1425
E_R_LRP <- 0.0075
sigma_LRP <- 0.00814
#E_R_LRP <- w_S_LRP * E_R_S + w_B_LRP * E_R_B + w_C_LRP * E_R_C
#sigma_LRP <- sqrt(w_S_LRP^2 * sigma_S^2 + w_B_LRP^2 * sigma_B^2 + 2 * w_S_LRP * w_B_LRP * cov_S_B)
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
             aes(color = "Efficient Frontier"), size = 3, alpha = 0.3) +
  geom_point(aes(x = gmv_volatility, y = gmv_return), color = "steelblue", size = 13, pch = 8) +
  geom_text(aes(x = gmv_volatility, y = gmv_return, label = "GMV"), 
            hjust = -0.3, color = "steelblue", size = 13) +
  geom_point(aes(x = sigma_6040, y = E_R_6040), color = "firebrick", size = 13, pch = 8) +
  geom_text(aes(x = sigma_6040, y = E_R_6040, label = "60/40"), 
            vjust = 2, color = "firebrick", size = 13) +
  geom_point(aes(x = sigma_VW, y = E_R_VW), color = "orange", size = 13, pch = 8) +
  geom_text(aes(x = sigma_VW, y = E_R_VW, label = "VW"), 
            vjust = 2, color = "orange", size = 13) +
  geom_point(aes(x = sigma_MVO, y = E_R_MVO), color = "salmon", size = 13, pch = 8) +
  geom_text(aes(x = sigma_MVO, y = E_R_MVO, label = "MVO"), 
            hjust = -0.3, color = "salmon", size = 13) +
  geom_point(aes(x = sigma_LMVO, y = E_R_LMVO), color = "darkgreen", size = 13, pch = 8) +
  geom_text(aes(x = sigma_LMVO, y = E_R_LMVO, label = "LMVO"), 
            hjust = 1.3, color = "darkgreen", size = 13) +
  geom_point(aes(x = sigma_LRP, y = E_R_LRP), color = "purple", size = 13, pch = 8) +
  geom_text(aes(x = sigma_LRP, y = E_R_LRP, label = "LRP"), 
            vjust = -1, color = "purple", size = 13) +
  labs(x = "Portfolio Volatility", 
       y = "Portfolio Expected Return", 
       title = "Efficient Frontier") +
  scale_color_manual(values = c("Inefficient Frontier" = "#666666", "Efficient Frontier" = "#666666")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 27),  # Title size
    axis.title.x = element_text(size = 23),  # X-axis label size
    axis.title.y = element_text(size = 23),  # Y-axis label size
    axis.text.x = element_text(size = 17),   # X-axis tick label size
    axis.text.y = element_text(size = 17)
  ) +
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
min_variance_pos <- which.min(port_volatilities[port_volatilities > 0])
min_variance_idx <- which(port_volatilities == port_volatilities[port_volatilities > 0][min_variance_pos])

# Extract the return and volatility of the GMV portfolio
gmv_return <- port_returns[min_variance_idx]
gmv_volatility <- port_volatilities[min_variance_idx]

# Split portfolios into efficient (above GMV) and inefficient (below GMV)
efficient_indices <- which(port_returns >= gmv_return)
inefficient_indices <- which(port_returns < gmv_return)

# Plot the efficient frontier
plot(port_volatilities[efficient_indices], port_returns[efficient_indices], col = "blue", lwd = 2,
     ylim = c(min(port_returns), max(port_returns)),
     xlim = c(min(port_volatilities), max(port_volatilities)),
     xlab = "Portfolio Volatility", ylab = "Portfolio Expected Return",
     main = "Efficient Frontier")

# Highlight the Global Minimum Variance Portfolio (GMV)
points(gmv_volatility, gmv_return, col = "green", pch = 8, cex = 1.5)
text(gmv_volatility, gmv_return, labels = "GMV Portfolio", pos = 4, col = "green")

### 60/40/0 strategy
w_S_6040 <- 0.60
w_B_6040 <- 0.40
w_C_6040 <- 0
E_R_6040 <- 0.00338
sigma_6040 <- 0.02724
#E_R_6040 <- w_S_6040 * E_R_S + w_B_6040 * E_R_B + w_C_6040 * E_R_C
#sigma_6040 <- sqrt(w_S_6040^2 * sigma_S^2 + w_B_6040^2 * sigma_B^2 + 2 * w_S_6040 * w_B_6040 * cov_S_B)
points(sigma_6040, E_R_6040, col = "red", pch = 8, cex = 1.5)
text(sigma_6040, E_R_6040, labels = "60/40/0 Strategy", pos = 1, col = "red")

### Market strategy
w_S_VW <- 1
w_B_VW <- 0
w_C_VW <- 0
E_R_VW <- 0.00233
sigma_VW <- 0.04811
#E_R_VW <- w_S_VW * E_R_S + w_B_VW * E_R_B + w_C_VW * E_R_C
#sigma_VW <- sqrt(w_S_VW^2 * sigma_S^2 + w_B_VW^2 * sigma_B^2 + 2 * w_S_VW * w_B_VW * cov_S_B)
points(sigma_VW, E_R_VW, col = "red", pch = 8, cex = 1.5)
text(sigma_VW, E_R_VW, labels = "Value-Weighted Strategy", pos = 1, col = "red")


### MVO+L strategy
w_S_LMVO <- 0.2262
w_B_LMVO <- 1.1782
w_C_LMVO <- 0.5955
E_R_LMVO <- 0.0075
sigma_LMVO <- 0.02612
#E_R_LMVO <- w_S_LMVO * E_R_S + w_B_LMVO * E_R_B + w_C_LMVO * E_R_C
#sigma_LMVO <- sqrt(w_S_LMVO^2 * sigma_S^2 + w_B_LMVO^2 * sigma_B^2 + 2 * w_S_LMVO * w_B_LMVO * cov_S_B)
points(sigma_LMVO, E_R_LMVO, col = "red", pch = 8, cex = 1.5)
text(sigma_LMVO, E_R_LMVO, labels = "LMVO Strategy", pos = 1, col = "red")

### LRP strategy
w_S_LRP <- 0.5210
w_B_LRP <- 1.1364
w_C_LRP <- 0.3426
E_R_LRP <- 0.0075
sigma_LRP <- 0.02991
#E_R_LRP <- w_S_LRP * E_R_S + w_B_LRP * E_R_B + w_C_LRP * E_R_C
#sigma_LRP <- sqrt(w_S_LRP^2 * sigma_S^2 + w_B_LRP^2 * sigma_B^2 + 2 * w_S_LRP * w_B_LRP * cov_S_B)
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
             aes(color = "Efficient Frontier"), size = 3, alpha = 0.3) +
  geom_point(aes(x = gmv_volatility, y = gmv_return), color = "steelblue", size = 13, pch = 8) +
  geom_text(aes(x = gmv_volatility, y = gmv_return, label = "GMV"), 
            hjust = -0.3, color = "steelblue", size = 13) +
  geom_point(aes(x = sigma_6040, y = E_R_6040), color = "firebrick", size = 13, pch = 8) +
  geom_text(aes(x = sigma_6040, y = E_R_6040, label = "60/40"), 
            vjust = 2, color = "firebrick", size = 13) +
  geom_point(aes(x = sigma_VW, y = E_R_VW), color = "orange", size = 13, pch = 8) +
  geom_text(aes(x = sigma_VW, y = E_R_VW, label = "VW"), 
            vjust = 2, color = "orange", size = 13) +
  geom_point(aes(x = sigma_LMVO, y = E_R_LMVO), color = "darkgreen", size = 13, pch = 8) +
  geom_text(aes(x = sigma_LMVO, y = E_R_LMVO, label = "LMVO"), 
            hjust = 1.3, color = "darkgreen", size = 13) +
  geom_point(aes(x = sigma_LRP, y = E_R_LRP), color = "purple", size = 13, pch = 8) +
  geom_text(aes(x = sigma_LRP, y = E_R_LRP, label = "LRP"), 
            vjust = 2, color = "purple", size = 13) +
  labs(x = "Portfolio Volatility", 
       y = "Portfolio Expected Return", 
       title = "Efficient Frontier") +
  scale_color_manual(values = c("Inefficient Frontier" = "#666666", "Efficient Frontier" = "#666666")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 27),  # Title size
    axis.title.x = element_text(size = 23),  # X-axis label size
    axis.title.y = element_text(size = 23),  # Y-axis label size
    axis.text.x = element_text(size = 17),   # X-axis tick label size
    axis.text.y = element_text(size = 17)
  ) +
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
min_variance_pos <- which.min(port_volatilities[port_volatilities > 0])
min_variance_idx <- which(port_volatilities == port_volatilities[port_volatilities > 0][min_variance_pos])

# Extract the return and volatility of the GMV portfolio
gmv_return <- port_returns[min_variance_idx]
gmv_volatility <- port_volatilities[min_variance_idx]

# Split portfolios into efficient (above GMV) and inefficient (below GMV)
efficient_indices <- which(port_returns >= gmv_return)
inefficient_indices <- which(port_returns < gmv_return)

# Plot the efficient frontier
plot(port_volatilities[efficient_indices], port_returns[efficient_indices], col = "blue", lwd = 2,
     ylim = c(min(port_returns), max(port_returns)),
     xlim = c(min(port_volatilities), max(port_volatilities)),
     xlab = "Portfolio Volatility", ylab = "Portfolio Expected Return",
     main = "Efficient Frontier")

# Highlight the Global Minimum Variance Portfolio (GMV)
points(gmv_volatility, gmv_return, col = "green", pch = 8, cex = 1.5)
text(gmv_volatility, gmv_return, labels = "GMV Portfolio", pos = 4, col = "green")

### 60/40/0 strategy
w_S_6040 <- 0.60
w_B_6040 <- 0.40
w_C_6040 <- 0
E_R_6040 <- 0.00667
sigma_6040 <- 0.01798
#E_R_6040 <- w_S_6040 * E_R_S + w_B_6040 * E_R_B + w_C_6040 * E_R_C
#sigma_6040 <- sqrt(w_S_6040^2 * sigma_S^2 + w_B_6040^2 * sigma_B^2 + 2 * w_S_6040 * w_B_6040 * cov_S_B)
points(sigma_6040, E_R_6040, col = "red", pch = 8, cex = 1.5)
text(sigma_6040, E_R_6040, labels = "60/40/0 Strategy", pos = 1, col = "red")

### Market strategy
w_S_VW <- 1
w_B_VW <- 0
w_C_VW <- 0
E_R_VW <- 0.01063
sigma_VW <- 0.03179
#E_R_VW <- w_S_VW * E_R_S + w_B_VW * E_R_B + w_C_VW * E_R_C
#sigma_VW <- sqrt(w_S_VW^2 * sigma_S^2 + w_B_VW^2 * sigma_B^2 + 2 * w_S_VW * w_B_VW * cov_S_B)
points(sigma_VW, E_R_VW, col = "red", pch = 8, cex = 1.5)
text(sigma_VW, E_R_VW, labels = "Value-Weighted Strategy", pos = 1, col = "red")


### MVO strategy
w_S_MVO <- 0.6841
w_B_MVO <- 0.3159
w_C_MVO <- 0
E_R_MVO <- 0.0075
sigma_MVO <- 0.02059
#E_R_MVO <- w_S_MVO * E_R_S + w_B_MVO * E_R_B + w_C_MVO * E_R_C
#sigma_MVO <- sqrt(w_S_MVO^2 * sigma_S^2 + w_B_MVO^2 * sigma_B^2 + 2 * w_S_MVO * w_B_MVO * cov_S_B)
points(sigma_MVO, E_R_MVO, col = "red", pch = 8, cex = 1.5)
text(sigma_MVO, E_R_MVO, labels = "MVO Strategy", pos = 1, col = "red")

### MVO+L strategy
w_S_LMVO <- 0.6612
w_B_LMVO <- 0.4901
w_C_LMVO <- 0.3486
E_R_LMVO <- 0.0075
sigma_LMVO <- 0.01981
#E_R_LMVO <- w_S_LMVO * E_R_S + w_B_LMVO * E_R_B + w_C_LMVO * E_R_C
#sigma_LMVO <- sqrt(w_S_LMVO^2 * sigma_S^2 + w_B_LMVO^2 * sigma_B^2 + 2 * w_S_LMVO * w_B_LMVO * cov_S_B)
points(sigma_LMVO, E_R_LMVO, col = "red", pch = 8, cex = 1.5)
text(sigma_LMVO, E_R_LMVO, labels = "LMVO Strategy", pos = 1, col = "red")

### LRP strategy
w_S_LRP <- 0.6481
w_B_LRP <- 0.8519
w_C_LRP <- 0
E_R_LRP <- 0.0075
sigma_LRP <- 0.02075
#E_R_LRP <- w_S_LRP * E_R_S + w_B_LRP * E_R_B + w_C_LRP * E_R_C
#sigma_LRP <- sqrt(w_S_LRP^2 * sigma_S^2 + w_B_LRP^2 * sigma_B^2 + 2 * w_S_LRP * w_B_LRP * cov_S_B)
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
             aes(color = "Efficient Frontier"), size = 3, alpha = 0.3) +
  geom_point(aes(x = gmv_volatility, y = gmv_return), color = "steelblue", size = 13, pch = 8) +
  geom_text(aes(x = gmv_volatility, y = gmv_return, label = "GMV"), 
            hjust = -0.3, color = "steelblue", size = 13) +
  geom_point(aes(x = sigma_6040, y = E_R_6040), color = "firebrick", size = 13, pch = 8) +
  geom_text(aes(x = sigma_6040, y = E_R_6040, label = "60/40"), 
            vjust = 2, color = "firebrick", size = 13) +
  geom_point(aes(x = sigma_VW, y = E_R_VW), color = "orange", size = 13, pch = 8) +
  geom_text(aes(x = sigma_VW, y = E_R_VW, label = "VW"), 
            vjust = 2, color = "orange", size = 13) +
  geom_point(aes(x = sigma_MVO, y = E_R_MVO), color = "salmon", size = 13, pch = 8) +
  geom_text(aes(x = sigma_MVO, y = E_R_MVO, label = "MVO"), 
            hjust = -0.3, color = "salmon", size = 13) +
  geom_point(aes(x = sigma_LMVO, y = E_R_LMVO), color = "darkgreen", size = 13, pch = 8) +
  geom_text(aes(x = sigma_LMVO, y = E_R_LMVO, label = "LMVO"), 
            hjust = 1.3, color = "darkgreen", size = 13) +
  geom_point(aes(x = sigma_LRP, y = E_R_LRP), color = "purple", size = 13, pch = 8) +
  geom_text(aes(x = sigma_LRP, y = E_R_LRP, label = "LRP"), 
            vjust = -1, color = "purple", size = 13) +
  labs(x = "Portfolio Volatility", 
       y = "Portfolio Expected Return", 
       title = "Efficient Frontier") +
  scale_color_manual(values = c("Inefficient Frontier" = "#666666", "Efficient Frontier" = "#666666")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 27),  # Title size
    axis.title.x = element_text(size = 23),  # X-axis label size
    axis.title.y = element_text(size = 23),  # Y-axis label size
    axis.text.x = element_text(size = 17),   # X-axis tick label size
    axis.text.y = element_text(size = 17)
  ) +
  theme(legend.position = "none") +
  grid()  # Note: grid() may not be needed in ggplot as it is handled by the theme.


























####################################################################################################################
####################################################################################################################
#------------------------------- Cumulative Returns ----------------------------------------------------------------
####################################################################################################################
####################################################################################################################


###################################### Full Period Plot ########################################

# Convert Date to factor to keep it as "YYYY-MM"
FullPeriod$Date <- factor(FullPeriod$Date)
# Get labels for every 5th year (e.g., "1990-01", "1995-01", etc.)
FullPeriodBreaks <- FullPeriod$Date[seq(1, length(FullPeriod$Date), by = 12 * 5)]  # every 5 years


### 60/40 strategy
w_S_6040 <- 0.60
w_B_6040 <- 0.40
w_C_6040 <- 0
E_R_6040 <- 0.00679
sigma_6040 <- 0.02777

### Market (VW)
w_S_VW <- 1
w_B_VW <- 0
w_C_VW <- 0
E_R_VW <- 0.00931
sigma_VW <- 0.04410

### MVO
w_S_MVO <- 0.7127
w_B_MVO <- 0.2873
w_C_MVO <- 0
E_R_MVO <- 0.0075
sigma_MVO <- 0.03159

### LMVO
w_S_LMVO <- 0.5534
w_B_LMVO <- 0.3705
w_C_LMVO <- 0.5761
E_R_LMVO <- 0.0075
sigma_LMVO <- 0.02515


### LRP
w_S_LRP <- 0.4847
w_B_LRP <- 0.9318
w_C_LRP <- 0.0835
E_R_LRP <- 0.0075
sigma_LRP <- 0.02805


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
         Log_Cum_Return_LRP = log(Cum_Return_LRP), ### LRP
         Strategy_Market = Equity, ### Market
         Cum_Return_Market = cumprod(1 + Strategy_Market), ### Market
         Log_Cum_Return_Market = log(Cum_Return_Market)) ### Market



# Plot the cumulative return with manually specified breaks
ggplot(FullPeriod, aes(x = Date)) +
  ### 60/40
  geom_line(aes(y = Cum_Return_60_40, group = 1), color = "firebrick", linewidth = 3) +
  geom_point(aes(y = Cum_Return_60_40), color = "firebrick") +
  ### MVO
  geom_line(aes(y = Cum_Return_MVO, group = 2), color = "salmon", linewidth = 3) +
  geom_point(aes(y = Cum_Return_MVO), color = "salmon") +
  ### MVO+L
  geom_line(aes(y = Cum_Return_LMVO, group = 2), color = "darkgreen", linewidth = 3) +
  geom_point(aes(y = Cum_Return_LMVO), color = "darkgreen") +
  ### LRP
  geom_line(aes(y = Cum_Return_LRP, group = 2), color = "purple", linewidth = 3) +
  geom_point(aes(y = Cum_Return_LRP), color = "purple") +
  ### Market
  geom_line(aes(y = Cum_Return_Market, group = 2), color = "orange", linewidth = 3) +
  geom_point(aes(y = Cum_Return_Market), color = "orange") +
  ggtitle("Cumulative Return of Strategies") +
  xlab("Date") + ylab("Cumulative Return") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks = FullPeriodBreaks) +  # Use your predefined breaks
  annotate(geom="text", x="1991-11", y=30, label=paste("60/40"),
           color="firebrick", size = 13) +
  annotate(geom="text", x="1991-09", y=26, label=paste("MVO"),
           color="salmon", size = 13) +
  annotate(geom="text", x="1991-07", y=22, label=paste("LRP"),
           color="purple", size = 13) +
  annotate(geom="text", x="1991-05", y=28, label=paste("VW"),
           color="orange", size = 13) +
  annotate(geom="text", x="1992-01", y=24, label=paste("LMVO"),
           color="darkgreen", size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 27),  # Title size
    axis.title.x = element_text(size = 23),  # X-axis label size
    axis.title.y = element_text(size = 23),  # Y-axis label size
    axis.text.x = element_text(size = 17),   # X-axis tick label size
    axis.text.y = element_text(size = 17)
  )




###################################### Roaring 90s Plot ########################################

# Convert Date to factor to keep it as "YYYY-MM"
Roaring90s$Date <- factor(Roaring90s$Date)
# Get labels for every 5th year (e.g., "1990-01", "1995-01", etc.)
Roaring90sBreaks <- Roaring90s$Date[seq(1, length(Roaring90s$Date), by = 12 * 1)]  # every 5 years

### 60/40 strategy
w_S_6040 <- 0.60
w_B_6040 <- 0.40
w_C_6040 <- 0
E_R_6040 <- 0.01026
sigma_6040 <- 0.02705

### Market (VW)
w_S_VW <- 1
w_B_VW <- 0
w_C_VW <- 0
E_R_VW <- 0.01454
sigma_VW <- 0.03940

### MVO
w_S_MVO <- 0.3312
w_B_MVO <- 0
w_C_MVO <- 0.6688
E_R_MVO <- 0.0075
sigma_MVO <- 0.01306

### LMVO
w_S_LMVO <- 0.1404
w_B_LMVO <- 0
w_C_LMVO <- 1.3596
E_R_LMVO <- 0.0075
sigma_LMVO <- 0.00572


### LRP
w_S_LRP <- 0.1439
w_B_LRP <- 0.2136
w_C_LRP <- 1.1425
E_R_LRP <- 0.0075
sigma_LRP <- 0.00814


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
         Log_Cum_Return_LRP = log(Cum_Return_LRP), ### LRP
         Strategy_Market = Equity, ### Market
         Cum_Return_Market = cumprod(1 + Strategy_Market), ### Market
         Log_Cum_Return_Market = log(Cum_Return_Market)) ### Market


# Plot the cumulative return with manually specified breaks
ggplot(Roaring90s, aes(x = Date)) +
  ### 60/40
  geom_line(aes(y = Cum_Return_60_40, group = 1), color = "firebrick", linewidth = 3) +
  geom_point(aes(y = Cum_Return_60_40), color = "firebrick") +
  ### MVO
  geom_line(aes(y = Cum_Return_MVO, group = 2), color = "salmon", linewidth = 3) +
  geom_point(aes(y = Cum_Return_MVO), color = "salmon") +
  ### MVO+L
  geom_line(aes(y = Cum_Return_LMVO, group = 2), color = "darkgreen", linewidth = 3) +
  geom_point(aes(y = Cum_Return_LMVO), color = "darkgreen") +
  ### LRP
  geom_line(aes(y = Cum_Return_LRP, group = 2), color = "purple", linewidth = 3) +
  geom_point(aes(y = Cum_Return_LRP), color = "purple") +
  ### Market
  geom_line(aes(y = Cum_Return_Market, group = 2), color = "orange", linewidth = 3) +
  geom_point(aes(y = Cum_Return_Market), color = "orange") +
  ggtitle("Cumulative Return of Strategies") +
  xlab("Date") + ylab("Cumulative Return") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks = Roaring90sBreaks) +  # Use your predefined breaks
  annotate(geom="text", x="1990-08", y=5.2, label=paste("60/40"),
           color="firebrick", size = 13) +
  annotate(geom="text", x="1990-08", y=4.6, label=paste("MVO"),
           color="salmon", size = 13) +
  annotate(geom="text", x="1990-07", y=4, label=paste("LRP"),
           color="purple", size = 13) +
  annotate(geom="text", x="1990-06", y=4.9, label=paste("VW"),
           color="orange", size = 13) +
  annotate(geom="text", x="1990-09", y=4.3, label=paste("LMVO"),
           color="darkgreen", size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 27),  # Title size
    axis.title.x = element_text(size = 23),  # X-axis label size
    axis.title.y = element_text(size = 23),  # Y-axis label size
    axis.text.x = element_text(size = 17),   # X-axis tick label size
    axis.text.y = element_text(size = 17)
  )










###################################### Financial Crisis Plot ########################################

# Convert Date to factor to keep it as "YYYY-MM"
FinancialCrisis$Date <- factor(FinancialCrisis$Date)
# Get labels for every 5th year (e.g., "1990-01", "1995-01", etc.)
FinancialCrisisBreaks <- FinancialCrisis$Date[seq(1, length(FinancialCrisis$Date), by = 12 * 1)]  # every 5 years

### 60/40 strategy
w_S_6040 <- 0.60
w_B_6040 <- 0.40
w_C_6040 <- 0
E_R_6040 <- 0.00338
sigma_6040 <- 0.02724

### Market (VW)
w_S_VW <- 1
w_B_VW <- 0
w_C_VW <- 0
E_R_VW <- 0.00233
sigma_VW <- 0.04811

### MVO
w_S_MVO <- 0
w_B_MVO <- 0
w_C_MVO <- 0
E_R_MVO <- 0
sigma_MVO <- 0

### LMVO
w_S_LMVO <- 0.2262
w_B_LMVO <- 1.1782
w_C_LMVO <- 0.5955
E_R_LMVO <- 0.0075
sigma_LMVO <- 0.02612


### LRP
w_S_LRP <- 0.5210
w_B_LRP <- 1.1364
w_C_LRP <- 0.3426
E_R_LRP <- 0.0075
sigma_LRP <- 0.02991


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
         Log_Cum_Return_LRP = log(Cum_Return_LRP), ### LRP
         Strategy_Market = Equity, ### Market
         Cum_Return_Market = cumprod(1 + Strategy_Market), ### Market
         Log_Cum_Return_Market = log(Cum_Return_Market)) ### Market


# Plot the cumulative return with manually specified breaks
ggplot(FinancialCrisis, aes(x = Date)) +
  ### 60/40
  geom_line(aes(y = Cum_Return_60_40, group = 1), color = "firebrick", linewidth = 3) +
  geom_point(aes(y = Cum_Return_60_40), color = "firebrick") +
  ### MVO+L
  geom_line(aes(y = Cum_Return_LMVO, group = 2), color = "darkgreen", linewidth = 3) +
  geom_point(aes(y = Cum_Return_LMVO), color = "darkgreen") +
  ### LRP
  geom_line(aes(y = Cum_Return_LRP, group = 2), color = "purple", linewidth = 3) +
  geom_point(aes(y = Cum_Return_LRP), color = "purple") +
  ### Market
  geom_line(aes(y = Cum_Return_Market, group = 2), color = "orange", linewidth = 3) +
  geom_point(aes(y = Cum_Return_Market), color = "orange") +
  ggtitle("Cumulative Return of Strategies") +
  xlab("Date") + ylab("Cumulative Return") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks = FinancialCrisisBreaks) +  # Use your predefined breaks
  annotate(geom="text", x="2000-08", y=2.9, label=paste("60/40"),
           color="firebrick", size = 13) +
  annotate(geom="text", x="2000-07", y=2.3, label=paste("LRP"),
           color="purple", size = 13) +
  annotate(geom="text", x="2000-06", y=2.7, label=paste("VW"),
           color="orange", size = 13) +
  annotate(geom="text", x="2000-09", y=2.5, label=paste("LMVO"),
           color="darkgreen", size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 27),  # Title size
    axis.title.x = element_text(size = 23),  # X-axis label size
    axis.title.y = element_text(size = 23),  # Y-axis label size
    axis.text.x = element_text(size = 17),   # X-axis tick label size
    axis.text.y = element_text(size = 17)
  )






###################################### Great Recession Recovery Plot ########################################

# Convert Date to factor to keep it as "YYYY-MM"
GreatRecessionRecovery$Date <- factor(GreatRecessionRecovery$Date)
# Get labels for every 5th year (e.g., "1990-01", "1995-01", etc.)
GreatRecessionRecoveryBreaks <- GreatRecessionRecovery$Date[seq(1, length(GreatRecessionRecovery$Date), by = 12 * 1)]  # every 5 years


### 60/40 strategy
w_S_6040 <- 0.60
w_B_6040 <- 0.40
w_C_6040 <- 0
E_R_6040 <- 0.00667
sigma_6040 <- 0.01798

### Market (VW)
w_S_VW <- 1
w_B_VW <- 0
w_C_VW <- 0
E_R_VW <- 0.01063
sigma_VW <- 0.03179

### MVO
w_S_MVO <- 0.6841
w_B_MVO <- 0.3159
w_C_MVO <- 0
E_R_MVO <- 0.0075
sigma_MVO <- 0.02059

### LMVO
w_S_LMVO <- 0.6612
w_B_LMVO <- 0.4901
w_C_LMVO <- 0.3486
E_R_LMVO <- 0.0075
sigma_LMVO <- 0.01981


### LRP
w_S_LRP <- 0.6481
w_B_LRP <- 0.8519
w_C_LRP <- 0
E_R_LRP <- 0.0075
sigma_LRP <- 0.02075


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
         Log_Cum_Return_LRP = log(Cum_Return_LRP), ### LRP
         Strategy_Market = Equity, ### Market
         Cum_Return_Market = cumprod(1 + Strategy_Market), ### Market
         Log_Cum_Return_Market = log(Cum_Return_Market)) ### Market


# Plot the cumulative return with manually specified breaks
ggplot(GreatRecessionRecovery, aes(x = Date)) +
  ### 60/40
  geom_line(aes(y = Cum_Return_60_40, group = 1), color = "firebrick", linewidth = 3) +
  geom_point(aes(y = Cum_Return_60_40), color = "firebrick") +
  ### MVO
  geom_line(aes(y = Cum_Return_MVO, group = 2), color = "salmon", linewidth = 3) +
  geom_point(aes(y = Cum_Return_MVO), color = "salmon") +
  ### MVO+L
  geom_line(aes(y = Cum_Return_LMVO, group = 2), color = "darkgreen", linewidth = 3) +
  geom_point(aes(y = Cum_Return_LMVO), color = "darkgreen") +
  ### LRP
  geom_line(aes(y = Cum_Return_LRP, group = 2), color = "purple", linewidth = 3) +
  geom_point(aes(y = Cum_Return_LRP), color = "purple") +
  ### Market
  geom_line(aes(y = Cum_Return_Market, group = 2), color = "orange", linewidth = 3) +
  geom_point(aes(y = Cum_Return_Market), color = "orange") +
  ggtitle("Cumulative Return of Strategies") +
  xlab("Date") + ylab("Cumulative Return") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks = GreatRecessionRecoveryBreaks) +  # Use your predefined breaks
  annotate(geom="text", x="2012-06", y=2.7, label=paste("60/40"),
           color="firebrick", size = 13) +
  annotate(geom="text", x="2012-06", y=2.5, label=paste("MVO"),
           color="salmon", size = 13) +
  annotate(geom="text", x="2012-06", y=2.3, label=paste("LRP"),
           color="purple", size = 13) +
  annotate(geom="text", x="2012-05", y=2.6, label=paste("VW"),
           color="orange", size = 13) +
  annotate(geom="text", x="2012-07", y=2.4, label=paste("LMVO"),
           color="darkgreen", size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 27),  # Title size
    axis.title.x = element_text(size = 23),  # X-axis label size
    axis.title.y = element_text(size = 23),  # Y-axis label size
    axis.text.x = element_text(size = 17),   # X-axis tick label size
    axis.text.y = element_text(size = 17)
  )





