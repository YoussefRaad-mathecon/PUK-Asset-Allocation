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
#------------------------------------------- A ---------------------------------------------------------------------
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


### Small-minus-big factor from MOMexp
SMB <- 1/3 * (MOMexp_Average_Value_Weighted_Returns_Monthly_part2$'SMALL.LoPRIOR'
                + MOMexp_Average_Value_Weighted_Returns_Monthly_part2$'ME1.PRIOR2' 
                + MOMexp_Average_Value_Weighted_Returns_Monthly_part2$'SMALL.HiPRIOR') -
  1/3 * (MOMexp_Average_Value_Weighted_Returns_Monthly_part2$'BIG.LoPRIOR' 
           + MOMexp_Average_Value_Weighted_Returns_Monthly_part2$'ME2.PRIOR2' 
           + MOMexp_Average_Value_Weighted_Returns_Monthly_part2$'BIG.HiPRIOR')


### Momentum factor from MOMexp
MOM <- 1/2 * (MOMexp_Average_Value_Weighted_Returns_Monthly_part2$'SMALL.HiPRIOR'
              + MOMexp_Average_Value_Weighted_Returns_Monthly_part2$'BIG.HiPRIOR') -
  1/2 * (MOMexp_Average_Value_Weighted_Returns_Monthly_part2$'SMALL.LoPRIOR' 
         + MOMexp_Average_Value_Weighted_Returns_Monthly_part2$'BIG.LoPRIOR')


### Data frame for regression
Part2Full <- data.frame("RF" = FFdata_Monthly_Factors$RF[1:1164],
                        "RM" = market_return_exp,
                        "MarketExcessReturn" = market_return_exp - FFdata_Monthly_Factors$RF[1:1164],
                        "SMB" = SMB,
                        "MOM" = MOM,
                        "ExcessReturn_S_LPrior" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:1164,2] - FFdata_Monthly_Factors$RF[1:1164],
                        "ExcessReturn_S_2" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:1164,3] - FFdata_Monthly_Factors$RF[1:1164],
                        "ExcessReturn_S_3" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:1164,4] - FFdata_Monthly_Factors$RF[1:1164],
                        "ExcessReturn_S_4" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:1164,5] - FFdata_Monthly_Factors$RF[1:1164],
                        "ExcessReturn_S_HPrior" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:1164,6] - FFdata_Monthly_Factors$RF[1:1164],
                        "ExcessReturn_2_LPrior" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:1164,7] - FFdata_Monthly_Factors$RF[1:1164],
                        "ExcessReturn_2_2" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:1164,8] - FFdata_Monthly_Factors$RF[1:1164],
                        "ExcessReturn_2_3" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:1164,9] - FFdata_Monthly_Factors$RF[1:1164],
                        "ExcessReturn_2_4" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:1164,10] - FFdata_Monthly_Factors$RF[1:1164],
                        "ExcessReturn_2_HPrior" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:1164,11] - FFdata_Monthly_Factors$RF[1:1164],
                        "ExcessReturn_3_LPrior" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:1164,12] - FFdata_Monthly_Factors$RF[1:1164],
                        "ExcessReturn_3_2" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:1164,13] - FFdata_Monthly_Factors$RF[1:1164],
                        "ExcessReturn_3_3" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:1164,14] - FFdata_Monthly_Factors$RF[1:1164],
                        "ExcessReturn_3_4" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:1164,15] - FFdata_Monthly_Factors$RF[1:1164],
                        "ExcessReturn_3_HPrior" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:1164,16] - FFdata_Monthly_Factors$RF[1:1164],
                        "ExcessReturn_4_LPrior" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:1164,17] - FFdata_Monthly_Factors$RF[1:1164],
                        "ExcessReturn_4_2" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:1164,18] - FFdata_Monthly_Factors$RF[1:1164],
                        "ExcessReturn_4_3" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:1164,19] - FFdata_Monthly_Factors$RF[1:1164],
                        "ExcessReturn_4_4" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:1164,20] - FFdata_Monthly_Factors$RF[1:1164],
                        "ExcessReturn_4_HPrior" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:1164,21] - FFdata_Monthly_Factors$RF[1:1164],
                        "ExcessReturn_B_LPrior" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:1164,22] - FFdata_Monthly_Factors$RF[1:1164],
                        "ExcessReturn_B_2" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:1164,23] - FFdata_Monthly_Factors$RF[1:1164],
                        "ExcessReturn_B_3" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:1164,24] - FFdata_Monthly_Factors$RF[1:1164],
                        "ExcessReturn_B_4" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:1164,25] - FFdata_Monthly_Factors$RF[1:1164],
                        "ExcessReturn_B_HPrior" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:1164,26] - FFdata_Monthly_Factors$RF[1:1164])


RegressionFull_S_LPrior <- lm(formula = ExcessReturn_S_LPrior ~ MarketExcessReturn + SMB + MOM, data = Part2Full)
RegressionFull_S_2 <- lm(formula = ExcessReturn_S_2 ~ MarketExcessReturn + SMB + MOM, data = Part2Full)
RegressionFull_S_3 <- lm(formula = ExcessReturn_S_3 ~ MarketExcessReturn + SMB + MOM, data = Part2Full)
RegressionFull_S_4 <- lm(formula = ExcessReturn_S_4 ~ MarketExcessReturn + SMB + MOM, data = Part2Full)
RegressionFull_S_HPrior <- lm(formula = ExcessReturn_S_HPrior ~ MarketExcessReturn + SMB + MOM, data = Part2Full)

RegressionFull_2_LPrior <- lm(formula = ExcessReturn_2_LPrior ~ MarketExcessReturn + SMB + MOM, data = Part2Full)
RegressionFull_2_2 <- lm(formula = ExcessReturn_2_2 ~ MarketExcessReturn + SMB + MOM, data = Part2Full)
RegressionFull_2_3 <- lm(formula = ExcessReturn_2_3 ~ MarketExcessReturn + SMB + MOM, data = Part2Full)
RegressionFull_2_4 <- lm(formula = ExcessReturn_2_4 ~ MarketExcessReturn + SMB + MOM, data = Part2Full)
RegressionFull_2_HPrior <- lm(formula = ExcessReturn_2_HPrior ~ MarketExcessReturn + SMB + MOM, data = Part2Full)

RegressionFull_3_LPrior <- lm(formula = ExcessReturn_3_LPrior ~ MarketExcessReturn + SMB + MOM, data = Part2Full)
RegressionFull_3_2 <- lm(formula = ExcessReturn_3_2 ~ MarketExcessReturn + SMB + MOM, data = Part2Full)
RegressionFull_3_3 <- lm(formula = ExcessReturn_3_3 ~ MarketExcessReturn + SMB + MOM, data = Part2Full)
RegressionFull_3_4 <- lm(formula = ExcessReturn_3_4 ~ MarketExcessReturn + SMB + MOM, data = Part2Full)
RegressionFull_3_HPrior <- lm(formula = ExcessReturn_3_HPrior ~ MarketExcessReturn + SMB + MOM, data = Part2Full)

RegressionFull_4_LPrior <- lm(formula = ExcessReturn_4_LPrior ~ MarketExcessReturn + SMB + MOM, data = Part2Full)
RegressionFull_4_2 <- lm(formula = ExcessReturn_4_2 ~ MarketExcessReturn + SMB + MOM, data = Part2Full)
RegressionFull_4_3 <- lm(formula = ExcessReturn_4_3 ~ MarketExcessReturn + SMB + MOM, data = Part2Full)
RegressionFull_4_4 <- lm(formula = ExcessReturn_4_4 ~ MarketExcessReturn + SMB + MOM, data = Part2Full)
RegressionFull_4_HPrior <- lm(formula = ExcessReturn_4_HPrior ~ MarketExcessReturn + SMB + MOM, data = Part2Full)

RegressionFull_B_LPrior <- lm(formula = ExcessReturn_B_LPrior ~ MarketExcessReturn + SMB + MOM, data = Part2Full)
RegressionFull_B_2 <- lm(formula = ExcessReturn_B_2 ~ MarketExcessReturn + SMB + MOM, data = Part2Full)
RegressionFull_B_3 <- lm(formula = ExcessReturn_B_3 ~ MarketExcessReturn + SMB + MOM, data = Part2Full)
RegressionFull_B_4 <- lm(formula = ExcessReturn_B_4 ~ MarketExcessReturn + SMB + MOM, data = Part2Full)
RegressionFull_B_HPrior <- lm(formula = ExcessReturn_B_HPrior ~ MarketExcessReturn + SMB + MOM, data = Part2Full)



####################################################################################################################
####################################################################################################################
#------------------------------------------- B ---------------------------------------------------------------------
####################################################################################################################
####################################################################################################################

####################################################################################################################
#----------------------------------- Pre Fama-French ---------------------------------------------------------------
####################################################################################################################



### MOMexp
# Remove the first column (assumed to be character) and keep only numeric columns
num_firms <- MOMexp_Number_of_Firms_in_Portfolios[1:438, -1]
avg_firm_size <- MOMexp_Average_Firm_Size[1:438, -1]
avg_value_weighted_returns <- MOMexp_Average_Value_Weighted_Returns_Monthly[1:438, -1]

# Calculate the portfolio weights for all portfolios in one step
portfolio_weights <- num_firms * avg_firm_size

# Calculate total weight by summing all portfolio weights
total_weight <- rowSums(portfolio_weights)

# Calculate the market return using vectorized operations
market_return_exp <- rowSums(avg_value_weighted_returns * portfolio_weights) / total_weight



### MOMdep
# Remove the first column (assumed to be character) and keep only numeric columns
num_firms <- MOMdep_Number_of_Firms_in_Portfolios[1:438, -1]
avg_firm_size <- MOMdep_Average_Firm_Size[1:438, -1]
avg_value_weighted_returns <- MOMdep_Average_Value_Weighted_Returns_Monthly[1:438, -1]

# Calculate the portfolio weights for all portfolios in one step
portfolio_weights <- num_firms * avg_firm_size

# Calculate total weight by summing all portfolio weights
total_weight <- rowSums(portfolio_weights)

# Calculate the market return using vectorized operations
market_return_dep <- rowSums(avg_value_weighted_returns * portfolio_weights) / total_weight


### Small-minus-big factor from MOMexp
SMB <- 1/3 * (MOMexp_Average_Value_Weighted_Returns_Monthly[1:438,]$'SMALL.LoPRIOR'
              + MOMexp_Average_Value_Weighted_Returns_Monthly[1:438,]$'ME1.PRIOR2' 
              + MOMexp_Average_Value_Weighted_Returns_Monthly[1:438,]$'SMALL.HiPRIOR') -
  1/3 * (MOMexp_Average_Value_Weighted_Returns_Monthly[1:438,]$'BIG.LoPRIOR' 
         + MOMexp_Average_Value_Weighted_Returns_Monthly[1:438,]$'ME2.PRIOR2' 
         + MOMexp_Average_Value_Weighted_Returns_Monthly[1:438,]$'BIG.HiPRIOR')


### Momentum factor from MOMexp
MOM <- 1/2 * (MOMexp_Average_Value_Weighted_Returns_Monthly[1:438,]$'SMALL.HiPRIOR'
              + MOMexp_Average_Value_Weighted_Returns_Monthly[1:438,]$'BIG.HiPRIOR') -
  1/2 * (MOMexp_Average_Value_Weighted_Returns_Monthly[1:438,]$'SMALL.LoPRIOR' 
         + MOMexp_Average_Value_Weighted_Returns_Monthly[1:438,]$'BIG.LoPRIOR')



### Data frame for regression
PreFF <- data.frame("RF" = FFdata_Monthly_Factors$RF[1:438],
                    "RM" = market_return_exp,
                    "MarketExcessReturn" = market_return_exp - FFdata_Monthly_Factors$RF[1:438],
                    "SMB" = SMB,
                    "MOM" = MOM,
                    "ExcessReturn_S_LPrior" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:438,2] - FFdata_Monthly_Factors$RF[1:438],
                    "ExcessReturn_S_2" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:438,3] - FFdata_Monthly_Factors$RF[1:438],
                    "ExcessReturn_S_3" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:438,4] - FFdata_Monthly_Factors$RF[1:438],
                    "ExcessReturn_S_4" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:438,5] - FFdata_Monthly_Factors$RF[1:438],
                    "ExcessReturn_S_HPrior" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:438,6] - FFdata_Monthly_Factors$RF[1:438],
                    "ExcessReturn_2_LPrior" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:438,7] - FFdata_Monthly_Factors$RF[1:438],
                    "ExcessReturn_2_2" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:438,8] - FFdata_Monthly_Factors$RF[1:438],
                    "ExcessReturn_2_3" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:438,9] - FFdata_Monthly_Factors$RF[1:438],
                    "ExcessReturn_2_4" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:438,10] - FFdata_Monthly_Factors$RF[1:438],
                    "ExcessReturn_2_HPrior" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:438,11] - FFdata_Monthly_Factors$RF[1:438],
                    "ExcessReturn_3_LPrior" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:438,12] - FFdata_Monthly_Factors$RF[1:438],
                    "ExcessReturn_3_2" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:438,13] - FFdata_Monthly_Factors$RF[1:438],
                    "ExcessReturn_3_3" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:438,14] - FFdata_Monthly_Factors$RF[1:438],
                    "ExcessReturn_3_4" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:438,15] - FFdata_Monthly_Factors$RF[1:438],
                    "ExcessReturn_3_HPrior" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:438,16] - FFdata_Monthly_Factors$RF[1:438],
                    "ExcessReturn_4_LPrior" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:438,17] - FFdata_Monthly_Factors$RF[1:438],
                    "ExcessReturn_4_2" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:438,18] - FFdata_Monthly_Factors$RF[1:438],
                    "ExcessReturn_4_3" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:438,19] - FFdata_Monthly_Factors$RF[1:438],
                    "ExcessReturn_4_4" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:438,20] - FFdata_Monthly_Factors$RF[1:438],
                    "ExcessReturn_4_HPrior" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:438,21] - FFdata_Monthly_Factors$RF[1:438],
                    "ExcessReturn_B_LPrior" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:438,22] - FFdata_Monthly_Factors$RF[1:438],
                    "ExcessReturn_B_2" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:438,23] - FFdata_Monthly_Factors$RF[1:438],
                    "ExcessReturn_B_3" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:438,24] - FFdata_Monthly_Factors$RF[1:438],
                    "ExcessReturn_B_4" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:438,25] - FFdata_Monthly_Factors$RF[1:438],
                    "ExcessReturn_B_HPrior" = MOMdep_Average_Value_Weighted_Returns_Monthly[1:438,26] - FFdata_Monthly_Factors$RF[1:438])


RegressionPreFF_S_LPrior <- lm(formula = ExcessReturn_S_LPrior ~ MarketExcessReturn + SMB + MOM, data = PreFF)
RegressionPreFF_S_2 <- lm(formula = ExcessReturn_S_2 ~ MarketExcessReturn + SMB + MOM, data = PreFF)
RegressionPreFF_S_3 <- lm(formula = ExcessReturn_S_3 ~ MarketExcessReturn + SMB + MOM, data = PreFF)
RegressionPreFF_S_4 <- lm(formula = ExcessReturn_S_4 ~ MarketExcessReturn + SMB + MOM, data = PreFF)
RegressionPreFF_S_HPrior <- lm(formula = ExcessReturn_S_HPrior ~ MarketExcessReturn + SMB + MOM, data = PreFF)

RegressionPreFF_2_LPrior <- lm(formula = ExcessReturn_2_LPrior ~ MarketExcessReturn + SMB + MOM, data = PreFF)
RegressionPreFF_2_2 <- lm(formula = ExcessReturn_2_2 ~ MarketExcessReturn + SMB + MOM, data = PreFF)
RegressionPreFF_2_3 <- lm(formula = ExcessReturn_2_3 ~ MarketExcessReturn + SMB + MOM, data = PreFF)
RegressionPreFF_2_4 <- lm(formula = ExcessReturn_2_4 ~ MarketExcessReturn + SMB + MOM, data = PreFF)
RegressionPreFF_2_HPrior <- lm(formula = ExcessReturn_2_HPrior ~ MarketExcessReturn + SMB + MOM, data = PreFF)

RegressionPreFF_3_LPrior <- lm(formula = ExcessReturn_3_LPrior ~ MarketExcessReturn + SMB + MOM, data = PreFF)
RegressionPreFF_3_2 <- lm(formula = ExcessReturn_3_2 ~ MarketExcessReturn + SMB + MOM, data = PreFF)
RegressionPreFF_3_3 <- lm(formula = ExcessReturn_3_3 ~ MarketExcessReturn + SMB + MOM, data = PreFF)
RegressionPreFF_3_4 <- lm(formula = ExcessReturn_3_4 ~ MarketExcessReturn + SMB + MOM, data = PreFF)
RegressionPreFF_3_HPrior <- lm(formula = ExcessReturn_3_HPrior ~ MarketExcessReturn + SMB + MOM, data = PreFF)

RegressionPreFF_4_LPrior <- lm(formula = ExcessReturn_4_LPrior ~ MarketExcessReturn + SMB + MOM, data = PreFF)
RegressionPreFF_4_2 <- lm(formula = ExcessReturn_4_2 ~ MarketExcessReturn + SMB + MOM, data = PreFF)
RegressionPreFF_4_3 <- lm(formula = ExcessReturn_4_3 ~ MarketExcessReturn + SMB + MOM, data = PreFF)
RegressionPreFF_4_4 <- lm(formula = ExcessReturn_4_4 ~ MarketExcessReturn + SMB + MOM, data = PreFF)
RegressionPreFF_4_HPrior <- lm(formula = ExcessReturn_4_HPrior ~ MarketExcessReturn + SMB + MOM, data = PreFF)

RegressionPreFF_B_LPrior <- lm(formula = ExcessReturn_B_LPrior ~ MarketExcessReturn + SMB + MOM, data = PreFF)
RegressionPreFF_B_2 <- lm(formula = ExcessReturn_B_2 ~ MarketExcessReturn + SMB + MOM, data = PreFF)
RegressionPreFF_B_3 <- lm(formula = ExcessReturn_B_3 ~ MarketExcessReturn + SMB + MOM, data = PreFF)
RegressionPreFF_B_4 <- lm(formula = ExcessReturn_B_4 ~ MarketExcessReturn + SMB + MOM, data = PreFF)
RegressionPreFF_B_HPrior <- lm(formula = ExcessReturn_B_HPrior ~ MarketExcessReturn + SMB + MOM, data = PreFF)








####################################################################################################################
#--------------------------------- During Fama-French --------------------------------------------------------------
####################################################################################################################


### MOMexp
# Remove the first column (assumed to be character) and keep only numeric columns
num_firms <- MOMexp_Number_of_Firms_in_Portfolios[439:780, -1]
avg_firm_size <- MOMexp_Average_Firm_Size[439:780, -1]
avg_value_weighted_returns <- MOMexp_Average_Value_Weighted_Returns_Monthly[439:780, -1]

# Calculate the portfolio weights for all portfolios in one step
portfolio_weights <- num_firms * avg_firm_size

# Calculate total weight by summing all portfolio weights
total_weight <- rowSums(portfolio_weights)

# Calculate the market return using vectorized operations
market_return_exp <- rowSums(avg_value_weighted_returns * portfolio_weights) / total_weight



### MOMdep
# Remove the first column (assumed to be character) and keep only numeric columns
num_firms <- MOMdep_Number_of_Firms_in_Portfolios[439:780, -1]
avg_firm_size <- MOMdep_Average_Firm_Size[439:780, -1]
avg_value_weighted_returns <- MOMdep_Average_Value_Weighted_Returns_Monthly[439:780, -1]

# Calculate the portfolio weights for all portfolios in one step
portfolio_weights <- num_firms * avg_firm_size

# Calculate total weight by summing all portfolio weights
total_weight <- rowSums(portfolio_weights)

# Calculate the market return using vectorized operations
market_return_dep <- rowSums(avg_value_weighted_returns * portfolio_weights) / total_weight


### Small-minus-big factor from MOMexp
SMB <- 1/3 * (MOMexp_Average_Value_Weighted_Returns_Monthly[439:780,]$'SMALL.LoPRIOR'
              + MOMexp_Average_Value_Weighted_Returns_Monthly[439:780,]$'ME1.PRIOR2' 
              + MOMexp_Average_Value_Weighted_Returns_Monthly[439:780,]$'SMALL.HiPRIOR') -
  1/3 * (MOMexp_Average_Value_Weighted_Returns_Monthly[439:780,]$'BIG.LoPRIOR' 
         + MOMexp_Average_Value_Weighted_Returns_Monthly[439:780,]$'ME2.PRIOR2' 
         + MOMexp_Average_Value_Weighted_Returns_Monthly[439:780,]$'BIG.HiPRIOR')


### Momentum factor from MOMexp
MOM <- 1/2 * (MOMexp_Average_Value_Weighted_Returns_Monthly[439:780,]$'SMALL.HiPRIOR'
              + MOMexp_Average_Value_Weighted_Returns_Monthly[439:780,]$'BIG.HiPRIOR') -
  1/2 * (MOMexp_Average_Value_Weighted_Returns_Monthly[439:780,]$'SMALL.LoPRIOR' 
         + MOMexp_Average_Value_Weighted_Returns_Monthly[439:780,]$'BIG.LoPRIOR')



### Data frame for regression
DuringFF <- data.frame("RF" = FFdata_Monthly_Factors$RF[439:780],
                       "RM" = market_return_exp,
                       "MarketExcessReturn" = market_return_exp - FFdata_Monthly_Factors$RF[439:780],
                       "SMB" = SMB,
                       "MOM" = MOM,
                       "ExcessReturn_S_LPrior" = MOMdep_Average_Value_Weighted_Returns_Monthly[439:780,2] - FFdata_Monthly_Factors$RF[439:780],
                       "ExcessReturn_S_2" = MOMdep_Average_Value_Weighted_Returns_Monthly[439:780,3] - FFdata_Monthly_Factors$RF[439:780],
                       "ExcessReturn_S_3" = MOMdep_Average_Value_Weighted_Returns_Monthly[439:780,4] - FFdata_Monthly_Factors$RF[439:780],
                       "ExcessReturn_S_4" = MOMdep_Average_Value_Weighted_Returns_Monthly[439:780,5] - FFdata_Monthly_Factors$RF[439:780],
                       "ExcessReturn_S_HPrior" = MOMdep_Average_Value_Weighted_Returns_Monthly[439:780,6] - FFdata_Monthly_Factors$RF[439:780],
                       "ExcessReturn_2_LPrior" = MOMdep_Average_Value_Weighted_Returns_Monthly[439:780,7] - FFdata_Monthly_Factors$RF[439:780],
                       "ExcessReturn_2_2" = MOMdep_Average_Value_Weighted_Returns_Monthly[439:780,8] - FFdata_Monthly_Factors$RF[439:780],
                       "ExcessReturn_2_3" = MOMdep_Average_Value_Weighted_Returns_Monthly[439:780,9] - FFdata_Monthly_Factors$RF[439:780],
                       "ExcessReturn_2_4" = MOMdep_Average_Value_Weighted_Returns_Monthly[439:780,10] - FFdata_Monthly_Factors$RF[439:780],
                       "ExcessReturn_2_HPrior" = MOMdep_Average_Value_Weighted_Returns_Monthly[439:780,11] - FFdata_Monthly_Factors$RF[439:780],
                       "ExcessReturn_3_LPrior" = MOMdep_Average_Value_Weighted_Returns_Monthly[439:780,12] - FFdata_Monthly_Factors$RF[439:780],
                       "ExcessReturn_3_2" = MOMdep_Average_Value_Weighted_Returns_Monthly[439:780,13] - FFdata_Monthly_Factors$RF[439:780],
                       "ExcessReturn_3_3" = MOMdep_Average_Value_Weighted_Returns_Monthly[439:780,14] - FFdata_Monthly_Factors$RF[439:780],
                       "ExcessReturn_3_4" = MOMdep_Average_Value_Weighted_Returns_Monthly[439:780,15] - FFdata_Monthly_Factors$RF[439:780],
                       "ExcessReturn_3_HPrior" = MOMdep_Average_Value_Weighted_Returns_Monthly[439:780,16] - FFdata_Monthly_Factors$RF[439:780],
                       "ExcessReturn_4_LPrior" = MOMdep_Average_Value_Weighted_Returns_Monthly[439:780,17] - FFdata_Monthly_Factors$RF[439:780],
                       "ExcessReturn_4_2" = MOMdep_Average_Value_Weighted_Returns_Monthly[439:780,18] - FFdata_Monthly_Factors$RF[439:780],
                       "ExcessReturn_4_3" = MOMdep_Average_Value_Weighted_Returns_Monthly[439:780,19] - FFdata_Monthly_Factors$RF[439:780],
                       "ExcessReturn_4_4" = MOMdep_Average_Value_Weighted_Returns_Monthly[439:780,20] - FFdata_Monthly_Factors$RF[439:780],
                       "ExcessReturn_4_HPrior" = MOMdep_Average_Value_Weighted_Returns_Monthly[439:780,21] - FFdata_Monthly_Factors$RF[439:780],
                       "ExcessReturn_B_LPrior" = MOMdep_Average_Value_Weighted_Returns_Monthly[439:780,22] - FFdata_Monthly_Factors$RF[439:780],
                       "ExcessReturn_B_2" = MOMdep_Average_Value_Weighted_Returns_Monthly[439:780,23] - FFdata_Monthly_Factors$RF[439:780],
                       "ExcessReturn_B_3" = MOMdep_Average_Value_Weighted_Returns_Monthly[439:780,24] - FFdata_Monthly_Factors$RF[439:780],
                       "ExcessReturn_B_4" = MOMdep_Average_Value_Weighted_Returns_Monthly[439:780,25] - FFdata_Monthly_Factors$RF[439:780],
                       "ExcessReturn_B_HPrior" = MOMdep_Average_Value_Weighted_Returns_Monthly[439:780,26] - FFdata_Monthly_Factors$RF[439:780])


RegressionDuringFF_S_LPrior <- lm(formula = ExcessReturn_S_LPrior ~ MarketExcessReturn + SMB + MOM, data = DuringFF)
RegressionDuringFF_S_2 <- lm(formula = ExcessReturn_S_2 ~ MarketExcessReturn + SMB + MOM, data = DuringFF)
RegressionDuringFF_S_3 <- lm(formula = ExcessReturn_S_3 ~ MarketExcessReturn + SMB + MOM, data = DuringFF)
RegressionDuringFF_S_4 <- lm(formula = ExcessReturn_S_4 ~ MarketExcessReturn + SMB + MOM, data = DuringFF)
RegressionDuringFF_S_HPrior <- lm(formula = ExcessReturn_S_HPrior ~ MarketExcessReturn + SMB + MOM, data = DuringFF)

RegressionDuringFF_2_LPrior <- lm(formula = ExcessReturn_2_LPrior ~ MarketExcessReturn + SMB + MOM, data = DuringFF)
RegressionDuringFF_2_2 <- lm(formula = ExcessReturn_2_2 ~ MarketExcessReturn + SMB + MOM, data = DuringFF)
RegressionDuringFF_2_3 <- lm(formula = ExcessReturn_2_3 ~ MarketExcessReturn + SMB + MOM, data = DuringFF)
RegressionDuringFF_2_4 <- lm(formula = ExcessReturn_2_4 ~ MarketExcessReturn + SMB + MOM, data = DuringFF)
RegressionDuringFF_2_HPrior <- lm(formula = ExcessReturn_2_HPrior ~ MarketExcessReturn + SMB + MOM, data = DuringFF)

RegressionDuringFF_3_LPrior <- lm(formula = ExcessReturn_3_LPrior ~ MarketExcessReturn + SMB + MOM, data = DuringFF)
RegressionDuringFF_3_2 <- lm(formula = ExcessReturn_3_2 ~ MarketExcessReturn + SMB + MOM, data = DuringFF)
RegressionDuringFF_3_3 <- lm(formula = ExcessReturn_3_3 ~ MarketExcessReturn + SMB + MOM, data = DuringFF)
RegressionDuringFF_3_4 <- lm(formula = ExcessReturn_3_4 ~ MarketExcessReturn + SMB + MOM, data = DuringFF)
RegressionDuringFF_3_HPrior <- lm(formula = ExcessReturn_3_HPrior ~ MarketExcessReturn + SMB + MOM, data = DuringFF)

RegressionDuringFF_4_LPrior <- lm(formula = ExcessReturn_4_LPrior ~ MarketExcessReturn + SMB + MOM, data = DuringFF)
RegressionDuringFF_4_2 <- lm(formula = ExcessReturn_4_2 ~ MarketExcessReturn + SMB + MOM, data = DuringFF)
RegressionDuringFF_4_3 <- lm(formula = ExcessReturn_4_3 ~ MarketExcessReturn + SMB + MOM, data = DuringFF)
RegressionDuringFF_4_4 <- lm(formula = ExcessReturn_4_4 ~ MarketExcessReturn + SMB + MOM, data = DuringFF)
RegressionDuringFF_4_HPrior <- lm(formula = ExcessReturn_4_HPrior ~ MarketExcessReturn + SMB + MOM, data = DuringFF)

RegressionDuringFF_B_LPrior <- lm(formula = ExcessReturn_B_LPrior ~ MarketExcessReturn + SMB + MOM, data = DuringFF)
RegressionDuringFF_B_2 <- lm(formula = ExcessReturn_B_2 ~ MarketExcessReturn + SMB + MOM, data = DuringFF)
RegressionDuringFF_B_3 <- lm(formula = ExcessReturn_B_3 ~ MarketExcessReturn + SMB + MOM, data = DuringFF)
RegressionDuringFF_B_4 <- lm(formula = ExcessReturn_B_4 ~ MarketExcessReturn + SMB + MOM, data = DuringFF)
RegressionDuringFF_B_HPrior <- lm(formula = ExcessReturn_B_HPrior ~ MarketExcessReturn + SMB + MOM, data = DuringFF)




####################################################################################################################
#---------------------------------- Post Fama-French ---------------------------------------------------------------
####################################################################################################################


### MOMexp
# Remove the first column (assumed to be character) and keep only numeric columns
num_firms <- MOMexp_Number_of_Firms_in_Portfolios[781:1164, -1]
avg_firm_size <- MOMexp_Average_Firm_Size[781:1164, -1]
avg_value_weighted_returns <- MOMexp_Average_Value_Weighted_Returns_Monthly[781:1164, -1]

# Calculate the portfolio weights for all portfolios in one step
portfolio_weights <- num_firms * avg_firm_size

# Calculate total weight by summing all portfolio weights
total_weight <- rowSums(portfolio_weights)

# Calculate the market return using vectorized operations
market_return_exp <- rowSums(avg_value_weighted_returns * portfolio_weights) / total_weight



### MOMdep
# Remove the first column (assumed to be character) and keep only numeric columns
num_firms <- MOMdep_Number_of_Firms_in_Portfolios[781:1164, -1]
avg_firm_size <- MOMdep_Average_Firm_Size[781:1164, -1]
avg_value_weighted_returns <- MOMdep_Average_Value_Weighted_Returns_Monthly[781:1164, -1]

# Calculate the portfolio weights for all portfolios in one step
portfolio_weights <- num_firms * avg_firm_size

# Calculate total weight by summing all portfolio weights
total_weight <- rowSums(portfolio_weights)

# Calculate the market return using vectorized operations
market_return_dep <- rowSums(avg_value_weighted_returns * portfolio_weights) / total_weight


### Small-minus-big factor from MOMexp
SMB <- 1/3 * (MOMexp_Average_Value_Weighted_Returns_Monthly[781:1164,]$'SMALL.LoPRIOR'
              + MOMexp_Average_Value_Weighted_Returns_Monthly[781:1164,]$'ME1.PRIOR2' 
              + MOMexp_Average_Value_Weighted_Returns_Monthly[781:1164,]$'SMALL.HiPRIOR') -
  1/3 * (MOMexp_Average_Value_Weighted_Returns_Monthly[781:1164,]$'BIG.LoPRIOR' 
         + MOMexp_Average_Value_Weighted_Returns_Monthly[781:1164,]$'ME2.PRIOR2' 
         + MOMexp_Average_Value_Weighted_Returns_Monthly[781:1164,]$'BIG.HiPRIOR')


### Momentum factor from MOMexp
MOM <- 1/2 * (MOMexp_Average_Value_Weighted_Returns_Monthly[781:1164,]$'SMALL.HiPRIOR'
              + MOMexp_Average_Value_Weighted_Returns_Monthly[781:1164,]$'BIG.HiPRIOR') -
  1/2 * (MOMexp_Average_Value_Weighted_Returns_Monthly[781:1164,]$'SMALL.LoPRIOR' 
         + MOMexp_Average_Value_Weighted_Returns_Monthly[781:1164,]$'BIG.LoPRIOR')




### Data frame for regression
PostFF <- data.frame("RF" = FFdata_Monthly_Factors$RF[781:1164],
                     "RM" = market_return_exp,
                     "MarketExcessReturn" = market_return_exp - FFdata_Monthly_Factors$RF[781:1164],
                     "SMB" = SMB,
                     "MOM" = MOM,
                     "ExcessReturn_S_LPrior" = MOMdep_Average_Value_Weighted_Returns_Monthly[781:1164,2] - FFdata_Monthly_Factors$RF[781:1164],
                     "ExcessReturn_S_2" = MOMdep_Average_Value_Weighted_Returns_Monthly[781:1164,3] - FFdata_Monthly_Factors$RF[781:1164],
                     "ExcessReturn_S_3" = MOMdep_Average_Value_Weighted_Returns_Monthly[781:1164,4] - FFdata_Monthly_Factors$RF[781:1164],
                     "ExcessReturn_S_4" = MOMdep_Average_Value_Weighted_Returns_Monthly[781:1164,5] - FFdata_Monthly_Factors$RF[781:1164],
                     "ExcessReturn_S_HPrior" = MOMdep_Average_Value_Weighted_Returns_Monthly[781:1164,6] - FFdata_Monthly_Factors$RF[781:1164],
                     "ExcessReturn_2_LPrior" = MOMdep_Average_Value_Weighted_Returns_Monthly[781:1164,7] - FFdata_Monthly_Factors$RF[781:1164],
                     "ExcessReturn_2_2" = MOMdep_Average_Value_Weighted_Returns_Monthly[781:1164,8] - FFdata_Monthly_Factors$RF[781:1164],
                     "ExcessReturn_2_3" = MOMdep_Average_Value_Weighted_Returns_Monthly[781:1164,9] - FFdata_Monthly_Factors$RF[781:1164],
                     "ExcessReturn_2_4" = MOMdep_Average_Value_Weighted_Returns_Monthly[781:1164,10] - FFdata_Monthly_Factors$RF[781:1164],
                     "ExcessReturn_2_HPrior" = MOMdep_Average_Value_Weighted_Returns_Monthly[781:1164,11] - FFdata_Monthly_Factors$RF[781:1164],
                     "ExcessReturn_3_LPrior" = MOMdep_Average_Value_Weighted_Returns_Monthly[781:1164,12] - FFdata_Monthly_Factors$RF[781:1164],
                     "ExcessReturn_3_2" = MOMdep_Average_Value_Weighted_Returns_Monthly[781:1164,13] - FFdata_Monthly_Factors$RF[781:1164],
                     "ExcessReturn_3_3" = MOMdep_Average_Value_Weighted_Returns_Monthly[781:1164,14] - FFdata_Monthly_Factors$RF[781:1164],
                     "ExcessReturn_3_4" = MOMdep_Average_Value_Weighted_Returns_Monthly[781:1164,15] - FFdata_Monthly_Factors$RF[781:1164],
                     "ExcessReturn_3_HPrior" = MOMdep_Average_Value_Weighted_Returns_Monthly[781:1164,16] - FFdata_Monthly_Factors$RF[781:1164],
                     "ExcessReturn_4_LPrior" = MOMdep_Average_Value_Weighted_Returns_Monthly[781:1164,17] - FFdata_Monthly_Factors$RF[781:1164],
                     "ExcessReturn_4_2" = MOMdep_Average_Value_Weighted_Returns_Monthly[781:1164,18] - FFdata_Monthly_Factors$RF[781:1164],
                     "ExcessReturn_4_3" = MOMdep_Average_Value_Weighted_Returns_Monthly[781:1164,19] - FFdata_Monthly_Factors$RF[781:1164],
                     "ExcessReturn_4_4" = MOMdep_Average_Value_Weighted_Returns_Monthly[781:1164,20] - FFdata_Monthly_Factors$RF[781:1164],
                     "ExcessReturn_4_HPrior" = MOMdep_Average_Value_Weighted_Returns_Monthly[781:1164,21] - FFdata_Monthly_Factors$RF[781:1164],
                     "ExcessReturn_B_LPrior" = MOMdep_Average_Value_Weighted_Returns_Monthly[781:1164,22] - FFdata_Monthly_Factors$RF[781:1164],
                     "ExcessReturn_B_2" = MOMdep_Average_Value_Weighted_Returns_Monthly[781:1164,23] - FFdata_Monthly_Factors$RF[781:1164],
                     "ExcessReturn_B_3" = MOMdep_Average_Value_Weighted_Returns_Monthly[781:1164,24] - FFdata_Monthly_Factors$RF[781:1164],
                     "ExcessReturn_B_4" = MOMdep_Average_Value_Weighted_Returns_Monthly[781:1164,25] - FFdata_Monthly_Factors$RF[781:1164],
                     "ExcessReturn_B_HPrior" = MOMdep_Average_Value_Weighted_Returns_Monthly[781:1164,26] - FFdata_Monthly_Factors$RF[781:1164])


RegressionPostFF_S_LPrior <- lm(formula = ExcessReturn_S_LPrior ~ MarketExcessReturn + SMB + MOM, data = PostFF)
RegressionPostFF_S_2 <- lm(formula = ExcessReturn_S_2 ~ MarketExcessReturn + SMB + MOM, data = PostFF)
RegressionPostFF_S_3 <- lm(formula = ExcessReturn_S_3 ~ MarketExcessReturn + SMB + MOM, data = PostFF)
RegressionPostFF_S_4 <- lm(formula = ExcessReturn_S_4 ~ MarketExcessReturn + SMB + MOM, data = PostFF)
RegressionPostFF_S_HPrior <- lm(formula = ExcessReturn_S_HPrior ~ MarketExcessReturn + SMB + MOM, data = PostFF)

RegressionPostFF_2_LPrior <- lm(formula = ExcessReturn_2_LPrior ~ MarketExcessReturn + SMB + MOM, data = PostFF)
RegressionPostFF_2_2 <- lm(formula = ExcessReturn_2_2 ~ MarketExcessReturn + SMB + MOM, data = PostFF)
RegressionPostFF_2_3 <- lm(formula = ExcessReturn_2_3 ~ MarketExcessReturn + SMB + MOM, data = PostFF)
RegressionPostFF_2_4 <- lm(formula = ExcessReturn_2_4 ~ MarketExcessReturn + SMB + MOM, data = PostFF)
RegressionPostFF_2_HPrior <- lm(formula = ExcessReturn_2_HPrior ~ MarketExcessReturn + SMB + MOM, data = PostFF)

RegressionPostFF_3_LPrior <- lm(formula = ExcessReturn_3_LPrior ~ MarketExcessReturn + SMB + MOM, data = PostFF)
RegressionPostFF_3_2 <- lm(formula = ExcessReturn_3_2 ~ MarketExcessReturn + SMB + MOM, data = PostFF)
RegressionPostFF_3_3 <- lm(formula = ExcessReturn_3_3 ~ MarketExcessReturn + SMB + MOM, data = PostFF)
RegressionPostFF_3_4 <- lm(formula = ExcessReturn_3_4 ~ MarketExcessReturn + SMB + MOM, data = PostFF)
RegressionPostFF_3_HPrior <- lm(formula = ExcessReturn_3_HPrior ~ MarketExcessReturn + SMB + MOM, data = PostFF)

RegressionPostFF_4_LPrior <- lm(formula = ExcessReturn_4_LPrior ~ MarketExcessReturn + SMB + MOM, data = PostFF)
RegressionPostFF_4_2 <- lm(formula = ExcessReturn_4_2 ~ MarketExcessReturn + SMB + MOM, data = PostFF)
RegressionPostFF_4_3 <- lm(formula = ExcessReturn_4_3 ~ MarketExcessReturn + SMB + MOM, data = PostFF)
RegressionPostFF_4_4 <- lm(formula = ExcessReturn_4_4 ~ MarketExcessReturn + SMB + MOM, data = PostFF)
RegressionPostFF_4_HPrior <- lm(formula = ExcessReturn_4_HPrior ~ MarketExcessReturn + SMB + MOM, data = PostFF)

RegressionPostFF_B_LPrior <- lm(formula = ExcessReturn_B_LPrior ~ MarketExcessReturn + SMB + MOM, data = PostFF)
RegressionPostFF_B_2 <- lm(formula = ExcessReturn_B_2 ~ MarketExcessReturn + SMB + MOM, data = PostFF)
RegressionPostFF_B_3 <- lm(formula = ExcessReturn_B_3 ~ MarketExcessReturn + SMB + MOM, data = PostFF)
RegressionPostFF_B_4 <- lm(formula = ExcessReturn_B_4 ~ MarketExcessReturn + SMB + MOM, data = PostFF)
RegressionPostFF_B_HPrior <- lm(formula = ExcessReturn_B_HPrior ~ MarketExcessReturn + SMB + MOM, data = PostFF)





