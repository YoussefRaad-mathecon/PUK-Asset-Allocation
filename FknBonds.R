



library(dplyr)
library(lubridate)

# Convert yields from percentages to decimal values
TSYdata_dec <- TSYdata %>%
  mutate(across(-Date, ~ .x / 100)) # This converts all columns except Date to decimals

# Resample to monthly data using the last value in each month
monthly_TS <- TSYdata_dec %>%
  group_by(month = floor_date(Date, "month")) %>%
  slice_tail(n = 1) %>%
  ungroup()

# Define the PV and get_r functions
PV <- function(C, Y, T) {
  pv <- 0
  if (T == 10) {
    t_j <- seq(1, 9)
  } else {
    t_j <- seq(1, 9) - 1/12
  }
  for (i in t_j) {
    pv <- pv + C / (1 + Y)^i
  }
  pv <- pv + C / (1 + Y)^T + 1 / (1 + Y)^T
  return(pv)
}

get_r <- function(Yim1, Yi) {
  Ti <- 10  # Fixed value for the 10-year maturity
  Tim1 <- Ti - 1/12  # Previous time step (1 month earlier)
  
  pv1 <- PV(Yim1, Yi, Tim1)  # Present value with shifted yield and current yield
  pv2 <- PV(Yim1, Yim1, Ti)  # Present value with shifted yield twice
  
  # Return the relative change in present values
  r <- pv1 / pv2 - 1
  return(r)
}

# Compute the monthly returns for the 10-Year yield
monthly_TS <- monthly_TS %>%
  mutate(X10.Yr.1M = lag(X10.Yr),
         X10.Yr.Return = mapply(get_r, X10.Yr.1M, X10.Yr))

monthly_TS <- monthly_TS %>%
  mutate(`10 Yr m1` = lag(`X10.Yr`),  # Shift the 10-Year yield by one month
         `10YrReturns` = mapply(get_r, `10 Yr m1`, `X10.Yr`))  # Apply the function element-wise

Bonds <- monthly_TS$X10.Yr.Return


# Filter the data 1990-01-02 to 1990-01-31
TSYdata_1990_Jan <- TSYdata %>%
  filter(Date >= as.Date("1990-01-02") & Date <= as.Date("1990-01-31"))

# Calculate the mean for each bond
avg_1990_Jan <- colMeans(TSYdata_1990_Jan[, c("X1.Mo", "X2.Mo", "X3.Mo", "X4.Mo", "X6.Mo", 
                                              "X1.Yr", "X2.Yr", "X3.Yr", "X5.Yr", "X7.Yr", 
                                              "X10.Yr", "X20.Yr", "X30.Yr")], na.rm = TRUE)
avg_1990_Jan <- avg_1990_Jan/100


Bonds[1] <- avg_1990_Jan[11]






E_R_S <- mean(market_return) / 100  # Expected return for stocks
E_R_B <- mean(Bonds, na.rm = TRUE)  # Expected return for bonds
sigma_S <- sd(market_return, na.rm = TRUE) / 100  # Volatility of stocks
sigma_B <- sd(Bonds, na.rm = TRUE)  # Volatility of bonds
covariance <- cov(market_return / 100, Bonds, use = "pairwise.complete.obs")

# Define covar matrix for the assets
cov_matrix <- matrix(c(sigma_S^2, covariance, covariance, sigma_B^2), nrow = 2)

# Define expected returns vector
expected_returns <- c(E_R_S, E_R_B)






