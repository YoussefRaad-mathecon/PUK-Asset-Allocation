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
setwd("C:/Users/youss/OneDrive - University of Copenhagen/PUK")

####################################################################################################################
####################################################################################################################
#----------------------------------- Data preperation -------------------------------------------------------------
####################################################################################################################
####################################################################################################################
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
MOMexp_Average_Value_Weighted_Returns_Monthly_part2 <- MOMexp_Average_Value_Weighted_Returns_Monthly[1:1164,]
# Average Equal Weighted Returns -- Monthly
MOMexp_Average_Equal_Weighted_Returns_Monthly_part2 <- MOMexp_Average_Equal_Weighted_Returns_Monthly[1:1164,]
# Average Value Weighted Returns -- Annual
MOMexp_Average_Value_Weighted_Returns_Annual_part2 <- MOMexp_Average_Value_Weighted_Returns_Annual[1:97,]
# Average Equal Weighted Returns -- Annual
MOMexp_Average_Equal_Weighted_Returns_Annual_part2 <- MOMexp_Average_Equal_Weighted_Returns_Annual[1:97,]
# Number of Firms in Portfolios
MOMexp_Number_of_Firms_in_Portfolios_part2 <- MOMexp_Number_of_Firms_in_Portfolios[1:1164,]
# Average Firm Size
MOMexp_Average_Firm_Size_part2 <- MOMexp_Average_Firm_Size[1:1164,]
# Equally-Weighted Average of Prior Returns
MOMexp_Equally_Weighted_Average_of_Prior_Returns_part2 <- MOMexp_Equally_Weighted_Average_of_Prior_Returns[1:1164,]
# Value-Weighted Average of Prior Returns
MOMexp_Value_Weighted_Average_of_Prior_Returns_part2 <- MOMexp_Value_Weighted_Average_of_Prior_Returns[1:1164,]



# Remove the first column (assumed to be character) and keep only numeric columns
num_firms <- MOMexp_Number_of_Firms_in_Portfolios_part2[, -1]
avg_firm_size <- MOMexp_Average_Firm_Size_part2[, -1]
avg_value_weighted_returns <- MOMexp_Average_Value_Weighted_Returns_Monthly_part2[, -1]

# Calculate the portfolio weights for all portfolios in one step
portfolio_weights <- num_firms * avg_firm_size

# Calculate total weight by summing all portfolio weights
total_weight <- rowSums(portfolio_weights)

# Calculate the market return using vectorized operations
market_return_exp <- rowSums(avg_value_weighted_returns * portfolio_weights) / total_weight



## Split into subsets of subsets and cut timeline of the CSV file 6_Portfolios_ME_Prior_12_2.CSV
# Average Value Weighted Returns -- Monthly
MOMdep_Average_Value_Weighted_Returns_Monthly_part2 <- MOMdep_Average_Value_Weighted_Returns_Monthly[1:1164,]
# Average Equal Weighted Returns -- Monthly
MOMdep_Average_Equal_Weighted_Returns_Monthly_part2 <- MOMdep_Average_Equal_Weighted_Returns_Monthly[1:1164,]
# Average Value Weighted Returns -- Annual
MOMdep_Average_Value_Weighted_Returns_Annual_part2 <- MOMdep_Average_Value_Weighted_Returns_Annual[1:97,]
# Average Equal Weighted Returns -- Annual
MOMdep_Average_Equal_Weighted_Returns_Annual_part2 <- MOMdep_Average_Equal_Weighted_Returns_Annual[1:97,]
# Number of Firms in Portfolios
MOMdep_Number_of_Firms_in_Portfolios_part2 <- MOMdep_Number_of_Firms_in_Portfolios[1:1164,]
# Average Firm Size
MOMdep_Average_Firm_Size_part2 <- MOMdep_Average_Firm_Size[1:1164,]
# Equally-Weighted Average of Prior Returns
MOMdep_Equally_Weighted_Average_of_Prior_Returns_part2 <- MOMdep_Equally_Weighted_Average_of_Prior_Returns[1:1164,]
# Value-Weighted Average of Prior Returns
MOMdep_Value_Weighted_Average_of_Prior_Returns_part2 <- MOMdep_Value_Weighted_Average_of_Prior_Returns[1:1164,]



# Remove the first column (assumed to be character) and keep only numeric columns
num_firms <- MOMdep_Number_of_Firms_in_Portfolios_part2[, -1]
avg_firm_size <- MOMdep_Average_Firm_Size_part2[, -1]
avg_value_weighted_returns <- MOMdep_Average_Value_Weighted_Returns_Monthly_part2[, -1]

# Calculate the portfolio weights for all portfolios in one step
portfolio_weights <- num_firms * avg_firm_size

# Calculate total weight by summing all portfolio weights
total_weight <- rowSums(portfolio_weights)

# Calculate the market return using vectorized operations
market_return_dep <- rowSums(avg_value_weighted_returns * portfolio_weights) / total_weight


# Small-minus-big factor from MOMexp
SMB <- 1/3 * (MOMexp_Average_Value_Weighted_Returns_Monthly_part2$'SMALL_LoPRIOR'
                + MOMexp_Average_Value_Weighted_Returns_Monthly_part2$'ME1_PRIOR2' 
                + MOMexp_Average_Value_Weighted_Returns_Monthly_part2$'SMALL_HiPRIOR') -
  1/3 * (MOMexp_Average_Value_Weighted_Returns_Monthly_part2$'BIG_LoPRIOR' 
           + MOMexp_Average_Value_Weighted_Returns_Monthly_part2$'ME2_PRIOR2' 
           + MOMexp_Average_Value_Weighted_Returns_Monthly_part2$'BIG_HiPRIOR')

MOMexp <- MOMexp %>%
  mutate(SMB = (1/3) * ('SMALL_LoPRIOR' + 'ME1_PRIOR2' + 'SMALL_HiPRIOR') -
           (1/3) * ('BIG_LoPRIOR' + 'ME2_PRIOR2' + 'BIG_HiPRIOR'))

# Momentum factor from MOMexp
MOM <- 1/2 * (MOMexp_Average_Value_Weighted_Returns_Monthly_part2$'SMALL_HiPRIOR'
              + MOMexp_Average_Value_Weighted_Returns_Monthly_part2$'BIG_HiPRIOR') -
  1/2 * (MOMexp_Average_Value_Weighted_Returns_Monthly_part2$'SMALL_LoPRIOR' 
         + MOMexp_Average_Value_Weighted_Returns_Monthly_part2$'BIG_LoPRIOR')

MOMexp <- MOMexp %>%
  mutate(MOM = (1/2) * ('SMALL_HiPRIOR' + 'BIG_HiPRIOR') -
           (1/2) * ('SMALL_LoPRIOR' + 'BIG_LoPRIOR'))

# Prepare the dependent variable: MOMdep - RF excess return
Dependent <- MOMdep %>%
  mutate(ExcessReturn = MOMdep - RF,
         MarketExcessReturn = MOMexp - RF)

Part2Data <- data.frame("RF" = RF,
                        "ExcessReturn" = market_return_dep - RF,
                        "MarketExcessReturn" = market_return_exp - RF,
                        "SMB" = SMB,
                        "MOM" = MOM)


Regression <- lm(formula = ExcessReturn ~ MarketExcessReturn + SMB + MOM, data = Part2Data)


