####################################################################################################################
####################################################################################################################
#----------------------------------- Working Directory -------------------------------------------------------------
####################################################################################################################
####################################################################################################################


set.seed(123)
# setwd("C:/Users/youss/OneDrive - University of Copenhagen/PUK")
setwd("~/Documents/KU/PUKAssetAllocation/Exam/RCode")


####################################################################################################################
####################################################################################################################
#---------------------------------------- Full Period --------------------------------------------------------------
####################################################################################################################
####################################################################################################################


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

MarketExcessReturn <- market_return_exp - FFdata_Monthly_Factors$RF[1:1164]



### Means, Sds, and t-stats
NumObs <- length(market_return_exp)
Means <- cbind("RM" = mean(market_return_exp),
               "RM-RF" = mean(MarketExcessReturn),
               "SMB" = mean(SMB),
               "MOM" = mean(MOM))
Sds <- cbind("RM" = sd(market_return_exp),
             "RM-RF" = sd(MarketExcessReturn),
             "SMB" = sd(SMB),
             "MOM" = sd(MOM))
tstats <- Means/(Sds / sqrt(NumObs))


### Autocorrelations
market_return_exp_TS <- ts(market_return_exp) ### If not a time series already
MarketExcessReturn_TS <- ts(MarketExcessReturn) ### If not a time series already
SMB_TS <- ts(SMB) ### If not a time series already
MOM_TS <- ts(MOM) ### If not a time series already
AutoCorr <- cbind("Lag1" = rbind("RM" = acf(market_return_exp_TS, plot = FALSE)$acf[2],     ### Lag 1
                                 "RM-RF" = acf(MarketExcessReturn_TS, plot = FALSE)$acf[2], ### Lag 1
                                 "SMB" = acf(SMB_TS, plot = FALSE)$acf[2],                  ### Lag 1
                                 "MOM" = acf(MOM_TS, plot = FALSE)$acf[2]),                 ### Lag 1
                  "Lag2" = rbind("RM" = acf(market_return_exp_TS, plot = FALSE)$acf[3],     ### Lag 2
                                 "RM-RF" = acf(MarketExcessReturn_TS, plot = FALSE)$acf[3], ### Lag 2
                                 "SMB" = acf(SMB_TS, plot = FALSE)$acf[3],                  ### Lag 2
                                 "MOM" = acf(MOM_TS, plot = FALSE)$acf[3]),                 ### Lag 2
                  "Lag12" = rbind("RM" = acf(market_return_exp_TS, plot = FALSE)$acf[13],   ### Lag 12
                                 "RM-RF" = acf(MarketExcessReturn_TS, plot = FALSE)$acf[13],### Lag 12
                                 "SMB" = acf(SMB_TS, plot = FALSE)$acf[13],                 ### Lag 12
                                 "MOM" = acf(MOM_TS, plot = FALSE)$acf[13]))                ### Lag 12

### Correlations
cor(SMB,MOM)
cor(SMB,MarketExcessReturn)
cor(MOM,MarketExcessReturn)



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



NumObs <- nrow(MOMdep_Average_Value_Weighted_Returns_Monthly[1:1164,-1] - FFdata_Monthly_Factors$RF[1:1164])
Means <- colMeans(as.matrix(MOMdep_Average_Value_Weighted_Returns_Monthly[1:1164,-1] - FFdata_Monthly_Factors$RF[1:1164]))
Sds <- colSds(as.matrix(MOMdep_Average_Value_Weighted_Returns_Monthly[1:1164,-1] - FFdata_Monthly_Factors$RF[1:1164]))
tstats <- Means/(Sds / sqrt(NumObs))



####################################################################################################################
####################################################################################################################
#------------------------------------------ Pre FF -----------------------------------------------------------------
####################################################################################################################
####################################################################################################################


### Means, Sds, and t-stats
NumObs <- length(market_return_exp[1:438])
Means <- cbind("RM" = mean(market_return_exp[1:438]),
               "RM-RF" = mean(MarketExcessReturn[1:438]),
               "SMB" = mean(SMB[1:438]),
               "MOM" = mean(MOM[1:438]))
Sds <- cbind("RM" = sd(market_return_exp[1:438]),
             "RM-RF" = sd(MarketExcessReturn[1:438]),
             "SMB" = sd(SMB[1:438]),
             "MOM" = sd(MOM[1:438]))
tstats <- Means/(Sds / sqrt(NumObs))


### Autocorrelations
market_return_exp_TS <- ts(market_return_exp[1:438]) ### If not a time series already
MarketExcessReturn_TS <- ts(MarketExcessReturn[1:438]) ### If not a time series already
SMB_TS <- ts(SMB[1:438]) ### If not a time series already
MOM_TS <- ts(MOM[1:438]) ### If not a time series already
AutoCorr <- cbind("Lag1" = rbind("RM" = acf(market_return_exp_TS, plot = FALSE)$acf[2],     ### Lag 1
                                 "RM-RF" = acf(MarketExcessReturn_TS, plot = FALSE)$acf[2], ### Lag 1
                                 "SMB" = acf(SMB_TS, plot = FALSE)$acf[2],                  ### Lag 1
                                 "MOM" = acf(MOM_TS, plot = FALSE)$acf[2]),                 ### Lag 1
                  "Lag2" = rbind("RM" = acf(market_return_exp_TS, plot = FALSE)$acf[3],     ### Lag 2
                                 "RM-RF" = acf(MarketExcessReturn_TS, plot = FALSE)$acf[3], ### Lag 2
                                 "SMB" = acf(SMB_TS, plot = FALSE)$acf[3],                  ### Lag 2
                                 "MOM" = acf(MOM_TS, plot = FALSE)$acf[3]),                 ### Lag 2
                  "Lag12" = rbind("RM" = acf(market_return_exp_TS, plot = FALSE)$acf[13],   ### Lag 12
                                  "RM-RF" = acf(MarketExcessReturn_TS, plot = FALSE)$acf[13],### Lag 12
                                  "SMB" = acf(SMB_TS, plot = FALSE)$acf[13],                 ### Lag 12
                                  "MOM" = acf(MOM_TS, plot = FALSE)$acf[13]))                ### Lag 12

### Correlations
cor(SMB[1:438],MOM[1:438])
cor(SMB[1:438],MarketExcessReturn[1:438])
cor(MOM[1:438],MarketExcessReturn[1:438])




### Means, Sds, and t-stats for 25 portfolios
NumObs <- nrow(MOMdep_Average_Value_Weighted_Returns_Monthly[1:438,-1] - FFdata_Monthly_Factors$RF[1:438])
Means <- colMeans(as.matrix(MOMdep_Average_Value_Weighted_Returns_Monthly[1:438,-1] - FFdata_Monthly_Factors$RF[1:438]))
Sds <- colSds(as.matrix(MOMdep_Average_Value_Weighted_Returns_Monthly[1:438,-1] - FFdata_Monthly_Factors$RF[1:438]))
tstats <- Means/(Sds / sqrt(NumObs))






####################################################################################################################
####################################################################################################################
#---------------------------------------- During FF ----------------------------------------------------------------
####################################################################################################################
####################################################################################################################


### Means, Sds, and t-stats
NumObs <- length(market_return_exp[439:780])
Means <- cbind("RM" = mean(market_return_exp[439:780]),
               "RM-RF" = mean(MarketExcessReturn[439:780]),
               "SMB" = mean(SMB[439:780]),
               "MOM" = mean(MOM[439:780]))
Sds <- cbind("RM" = sd(market_return_exp[439:780]),
             "RM-RF" = sd(MarketExcessReturn[439:780]),
             "SMB" = sd(SMB[439:780]),
             "MOM" = sd(MOM[439:780]))
tstats <- Means/(Sds / sqrt(NumObs))


### Autocorrelations
market_return_exp_TS <- ts(market_return_exp[439:780]) ### If not a time series already
MarketExcessReturn_TS <- ts(MarketExcessReturn[439:780]) ### If not a time series already
SMB_TS <- ts(SMB[439:780]) ### If not a time series already
MOM_TS <- ts(MOM[439:780]) ### If not a time series already
AutoCorr <- cbind("Lag1" = rbind("RM" = acf(market_return_exp_TS, plot = FALSE)$acf[2],     ### Lag 1
                                 "RM-RF" = acf(MarketExcessReturn_TS, plot = FALSE)$acf[2], ### Lag 1
                                 "SMB" = acf(SMB_TS, plot = FALSE)$acf[2],                  ### Lag 1
                                 "MOM" = acf(MOM_TS, plot = FALSE)$acf[2]),                 ### Lag 1
                  "Lag2" = rbind("RM" = acf(market_return_exp_TS, plot = FALSE)$acf[3],     ### Lag 2
                                 "RM-RF" = acf(MarketExcessReturn_TS, plot = FALSE)$acf[3], ### Lag 2
                                 "SMB" = acf(SMB_TS, plot = FALSE)$acf[3],                  ### Lag 2
                                 "MOM" = acf(MOM_TS, plot = FALSE)$acf[3]),                 ### Lag 2
                  "Lag12" = rbind("RM" = acf(market_return_exp_TS, plot = FALSE)$acf[13],   ### Lag 12
                                  "RM-RF" = acf(MarketExcessReturn_TS, plot = FALSE)$acf[13],### Lag 12
                                  "SMB" = acf(SMB_TS, plot = FALSE)$acf[13],                 ### Lag 12
                                  "MOM" = acf(MOM_TS, plot = FALSE)$acf[13]))                ### Lag 12

### Correlations
cor(SMB[439:780],MOM[439:780])
cor(SMB[439:780],MarketExcessReturn[439:780])
cor(MOM[439:780],MarketExcessReturn[439:780])




### Means, Sds, and t-stats for 25 portfolios
NumObs <- nrow(MOMdep_Average_Value_Weighted_Returns_Monthly[439:780,-1] - FFdata_Monthly_Factors$RF[439:780])
Means <- colMeans(as.matrix(MOMdep_Average_Value_Weighted_Returns_Monthly[439:780,-1] - FFdata_Monthly_Factors$RF[439:780]))
Sds <- colSds(as.matrix(MOMdep_Average_Value_Weighted_Returns_Monthly[439:780,-1] - FFdata_Monthly_Factors$RF[439:780]))
tstats <- Means/(Sds / sqrt(NumObs))









####################################################################################################################
####################################################################################################################
#----------------------------------------- Post FF -----------------------------------------------------------------
####################################################################################################################
####################################################################################################################



### Means, Sds, and t-stats
NumObs <- length(market_return_exp[781:1164])
Means <- cbind("RM" = mean(market_return_exp[781:1164]),
               "RM-RF" = mean(MarketExcessReturn[781:1164]),
               "SMB" = mean(SMB[781:1164]),
               "MOM" = mean(MOM[781:1164]))
Sds <- cbind("RM" = sd(market_return_exp[781:1164]),
             "RM-RF" = sd(MarketExcessReturn[781:1164]),
             "SMB" = sd(SMB[781:1164]),
             "MOM" = sd(MOM[781:1164]))
tstats <- Means/(Sds / sqrt(NumObs))


### Autocorrelations
market_return_exp_TS <- ts(market_return_exp[781:1164]) ### If not a time series already
MarketExcessReturn_TS <- ts(MarketExcessReturn[781:1164]) ### If not a time series already
SMB_TS <- ts(SMB[781:1164]) ### If not a time series already
MOM_TS <- ts(MOM[781:1164]) ### If not a time series already
AutoCorr <- cbind("Lag1" = rbind("RM" = acf(market_return_exp_TS, plot = FALSE)$acf[2],     ### Lag 1
                                 "RM-RF" = acf(MarketExcessReturn_TS, plot = FALSE)$acf[2], ### Lag 1
                                 "SMB" = acf(SMB_TS, plot = FALSE)$acf[2],                  ### Lag 1
                                 "MOM" = acf(MOM_TS, plot = FALSE)$acf[2]),                 ### Lag 1
                  "Lag2" = rbind("RM" = acf(market_return_exp_TS, plot = FALSE)$acf[3],     ### Lag 2
                                 "RM-RF" = acf(MarketExcessReturn_TS, plot = FALSE)$acf[3], ### Lag 2
                                 "SMB" = acf(SMB_TS, plot = FALSE)$acf[3],                  ### Lag 2
                                 "MOM" = acf(MOM_TS, plot = FALSE)$acf[3]),                 ### Lag 2
                  "Lag12" = rbind("RM" = acf(market_return_exp_TS, plot = FALSE)$acf[13],   ### Lag 12
                                  "RM-RF" = acf(MarketExcessReturn_TS, plot = FALSE)$acf[13],### Lag 12
                                  "SMB" = acf(SMB_TS, plot = FALSE)$acf[13],                 ### Lag 12
                                  "MOM" = acf(MOM_TS, plot = FALSE)$acf[13]))                ### Lag 12

### Correlations
cor(SMB[781:1164],MOM[781:1164])
cor(SMB[781:1164],MarketExcessReturn[781:1164])
cor(MOM[781:1164],MarketExcessReturn[781:1164])




### Means, Sds, and t-stats for 25 portfolios
NumObs <- nrow(MOMdep_Average_Value_Weighted_Returns_Monthly[781:1164,-1] - FFdata_Monthly_Factors$RF[781:1164])
Means <- colMeans(as.matrix(MOMdep_Average_Value_Weighted_Returns_Monthly[781:1164,-1] - FFdata_Monthly_Factors$RF[781:1164]))
Sds <- colSds(as.matrix(MOMdep_Average_Value_Weighted_Returns_Monthly[781:1164,-1] - FFdata_Monthly_Factors$RF[781:1164]))
tstats <- Means/(Sds / sqrt(NumObs))







