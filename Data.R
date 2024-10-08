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
library(stats) ### ACF plots
library(matrixcalc) ### Matrix calculations
library("RColorBrewer") ### Colors
library(latex2exp) ### Text for plots
library(matrixStats) ### ColSds


####################################################################################################################
####################################################################################################################
#----------------------------------- Working Directory and seed -------------------------------------------------------------
####################################################################################################################
####################################################################################################################
set.seed(123)
# setwd("C:/Users/youss/OneDrive - University of Copenhagen/PUK")

####################################################################################################################
####################################################################################################################
#-------------------------- Kenneth R French Data Library Data -----------------------------------------------------
####################################################################################################################
####################################################################################################################


FFdata <- read.csv("F-F_Research_Data_Factors.CSV", header = TRUE, sep = ",", skip = 2, fill = TRUE, strip.white = TRUE)
colnames(FFdata) <- c("Date", "Mkt_RF", "SMB", "HML", "RF")
head(FFdata)

MOMexp <- read.csv("6_Portfolios_ME_Prior_12_2.CSV", header = FALSE, sep = ",", skip = 12, fill = TRUE, strip.white = TRUE)
colnames(MOMexp) <- c("Date", "SMALL LoPRIOR", "ME1 PRIOR2", "SMALL HiPRIOR", "BIG LoPRIOR", "ME2 PRIOR2", "BIG HiPRIOR")
head(MOMexp)

MOMdep <- read.csv("25_Portfolios_ME_Prior_12_2.CSV", header = FALSE, sep = ",", skip = 12, fill = TRUE, strip.white = TRUE)
colnames(MOMdep) <- c("Date", "SMALL LoPRIOR", "ME1 PRIOR2", "ME1 PRIOR3", "ME1 PRIOR4",
                      "SMALL HiPRIOR", "ME2 PRIOR1", "ME2 PRIOR2", "ME2 PRIOR3", "ME2 PRIOR4", "ME2 PRIOR5",
                      "ME3 PRIOR1", "ME3 PRIOR2", "ME3 PRIOR3", "ME3 PRIOR4", "ME3 PRIOR5", 
                      "ME4 PRIOR1", "ME4 PRIOR2", "ME4 PRIOR3", "ME4 PRIOR4", "ME4 PRIOR5",
                      "BIG LoPRIOR", "ME5 PRIOR2", "ME5 PRIOR3", "ME5 PRIOR4", "BIG HiPRIOR")
head(MOMdep)

####################################################################################################################
####################################################################################################################
#----------------------------------- US Treasury Data --------------------------------------------------------------
####################################################################################################################
####################################################################################################################


TSYdata <- read.csv("yield-curve-rates-1990-2023.csv", header = TRUE)
head(TSYdata)

MonthlyReturn <- numeric(length(TSYdata$X1.Mo))
for (i in 2:length(TSYdata$X1.Mo)){
  MonthlyReturn[i] <- TSYdata$X1.Mo[i - 1]/TSYdata$X1.Mo[i] - 1
}
TSYdata$MR <- MonthlyReturn
head(TSYdata)



####################################################################################################################
####################################################################################################################
#---------------------------------- Monthly Returns ----------------------------------------------------------------
####################################################################################################################
####################################################################################################################

# Subset the dataframe
TSYdata_clean <- TSYdata[, c("X1.Mo", "X2.Mo", "X3.Mo", "X4.Mo", "X6.Mo", 
                         "X1.Yr", "X2.Yr", "X3.Yr", "X5.Yr", "X7.Yr", 
                         "X10.Yr", "X20.Yr", "X30.Yr")]
length(TSYdata_clean)
length(mat)
### Define the present value function
PV <- function(C, Y, T) {
  ### Ensure yields are in decimal form
  Y <- Y / 100
  t_j <- seq(1, T)  ### Payment dates (annual coupon payments, i.e. 1Y)
  
  ### Calculate the present value for coupon payments
  PV_value <- sum((C / (1 + Y) ^ t_j) + (1 / (1 + Y) ^ T))
  return(PV_value)
}

### Initialize an empty data frame to hold the returns
MonthlyReturn <- data.frame(Date = TSYdata$Date)  ### Dates for the new returns, minus the first row with return zero

mat <- c(1/12, 2/12, 3/12, 4/12, 6/12, 1, 2, 3, 5, 7, 10, 20, 30) ### Maturities


# Subset the dataframe
TSYdata_clean <- TSYdata[, c("X1.Mo", "X2.Mo", "X3.Mo", "X4.Mo", "X6.Mo", 
                             "X1.Yr", "X2.Yr", "X3.Yr", "X5.Yr", "X7.Yr", 
                             "X10.Yr", "X20.Yr", "X30.Yr")]

mat <- c(1/12, 2/12, 3/12, 4/12, 6/12, 1, 2, 3, 5, 7, 10, 20, 30)  # Maturities

# Define the present value function
PV <- function(C, Y, T) {
  # Ensure yields are in decimal form
  
  # Payment dates (annual coupon payments, i.e. 1Y steps)
  t_j <- seq(1, T)
  
  # Calculate the present value for coupon payments
  coupon_PV <- sum(C / (1 + Y) ^ t_j)
  
  # Calculate the present value for the principal repayment at time T
  principal_PV <- 1 / (1 + Y) ^ T
  
  # Total present value
  PV_value <- coupon_PV + principal_PV
  
  return(PV_value)
}


# Initialize an empty data frame to hold the returns
MonthlyReturn <- data.frame(Date = TSYdata$Date)  # Dates for the new returns, minus the first row with return zero

# Loop through each row to calculate the monthly returns
for (i in 2:nrow(TSYdata_clean)) {
  for (j in 1:ncol(TSYdata_clean)) {  # Use TSYdata_clean (without Date and MR) for looping through yields
    Y_prev <- TSYdata_clean[i - 1, j]  # Previous period yield
    Y_current <- TSYdata_clean[i, j]   # Current period yield
    
    T <- mat[j]  # Use correct indexing for maturities (no need to adjust by -1)
    
    # Calculate present values for Y_prev and Y_current
    PV_Y_current <- PV(Y_prev, Y_current, T - 1/12)  # PV(Y_{i-1}; Y_i, T - 1M)
    PV_Y_prev <- PV(Y_prev, Y_prev, T)               # PV(Y_{i-1}; Y_{i-1}, T)
    
    # Calculate the return using the formula
    r_i <- (PV_Y_current / PV_Y_prev) - 1
    
    # Store the return in the new data frame
    MonthlyReturn[i, j + 1] <- r_i  # j + 1 to account for the Date column
  }
}

# Name the columns of the returns data frame
colnames(MonthlyReturn)[-1] <- colnames(TSYdata_clean)

# View the results
head(MonthlyReturn)

# Test the PV function for 100 different values of C > -1
C_values <- runif(100, min = -0.99, max = 5)  # Generate 100 random C values greater than -1

# Integer value of T (e.g., T = 5 years)
T <- 5

# Apply the PV function for C = Y over the 100 different values of C
results <- data.frame(C = C_values, PV = sapply(C_values, function(C) PV(C, C, T)))

# Print the results
print(results)



# ...or manually test it
test_PV <- function(C, T) {
  return(PV(C, C, T))
}
# Run the test with C = 5% and T = 10 years
test_PV(5, 10)
