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


FullPeriod <- data.frame("Date" = FFdata$Date[763:1170],    ### 199001-202312 
                         "RF" = as.numeric(RF[763:1170]),
                         "Bonds" = Bonds,
                         "SMALL LoPRIOR" = Equity[763:1170,1],
                         "ME1 PRIOR2" = Equity[763:1170,2],
                         "SMALL HiPRIOR" = Equity[763:1170,3],
                         "BIG LoPRIOR" = Equity[763:1170,4],
                         "ME2 PRIOR2" = Equity[763:1170,5],
                         "BIG HiPRIOR" = Equity[763:1170,6],
                         "Equity" = (Equity[763:1170,1] + Equity[763:1170,2] + Equity[763:1170,3] + Equity[763:1170,4] + Equity[763:1170,5] + Equity[763:1170,6])/6)


### Convert Date to Date type
FullPeriod$Date <- as.Date(paste0(FullPeriod$Date, "01"), format="%Y%m%d")

### Backtest: 60% in Equity and 40% in Bonds
FullPeriod <- FullPeriod %>%
  mutate(Strategy_Return = 0.6 * Equity + 0.4 * Bonds,
         Cumulative_Return = cumprod(1 + Strategy_Return),
         Log_Cumulative_Return = log(Cumulative_Return))

# Plot the cumulative return in log scale
ggplot(FullPeriod, aes(x = Date, y = Cumulative_Return)) +
  geom_line(color = "blue") +
  geom_point() +
  ggtitle("Cumulative Return of Strategy") +
  xlab("Date") + ylab("Cumulative Return") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_labels = "%Y", 
               breaks = as.Date(c("1990-01-01", "1995-01-01", "2000-01-01",
                                  "2005-01-01", "2010-01-01", "2015-01-01", "2020-01-01")))





