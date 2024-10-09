####################################################################################################################
####################################################################################################################
#------------------------------- Current Investment Universe -------------------------------------------------------
####################################################################################################################
####################################################################################################################

### Data
RF <- FFdata$RF ### Risk-Free Rate
Bonds <- MonthlyReturn$X10.Yr ### Bonds: S&P U.S. Treasury Bond Current 10-Year Index
### Convert from "chr" to "num"
MOMexp_Average_Value_Weighted_Returns_Monthly[,2:7] <- lapply(MOMexp_Average_Value_Weighted_Returns_Monthly[,2:7], as.numeric)
MOMexp_Number_of_Firms_in_Portfolios[,2:7] <- lapply(MOMexp_Number_of_Firms_in_Portfolios[,2:7], as.numeric)
MOMexp_Average_Firm_Size[,2:7] <- lapply(MOMexp_Average_Firm_Size[,2:7], as.numeric)
Equity <- MOMexp_Average_Value_Weighted_Returns_Monthly[,2:7] / (MOMexp_Number_of_Firms_in_Portfolios[,2:7] * MOMexp_Average_Firm_Size[,2:7])
  ### Equity: Calculate (entire) stock market return from MOMexp by appropriately 
  ### weighting ‘Average Value Weighted Returns – Monthly’ by ‘Number of Firms in
  ### Portfolio’ and ‘Average Firm Size’.


