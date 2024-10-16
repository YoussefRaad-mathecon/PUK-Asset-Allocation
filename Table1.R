####################################################################################################################
####################################################################################################################
#---------------------------------------- Full Period --------------------------------------------------------------
####################################################################################################################
####################################################################################################################


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




### Average of annual averages of firm size
colMeans(MOMdep_Average_Firm_Size_part2[,-1])


### Average of annual $BE/ME$ ratios for portfolio




### Average of annual percent of market value in portfolio
# remove date
num_firms <- MOMdep_Number_of_Firms_in_Portfolios_part2[, -1]
avg_firm_size <- MOMdep_Average_Firm_Size_part2[, -1]
avg_value_weighted_returns <- MOMdep_Average_Value_Weighted_Returns_Monthly_part2[, -1]

# calculate pf weights
portfolio_weights <- num_firms * avg_firm_size

# calculate total market value for each portfolio
total_market_value <- rowSums(portfolio_weights)

# create a data frame with dates and total market values
market_value_df <- data.frame(Date = MOMdep_Average_Value_Weighted_Returns_Monthly_part2[, 1],
                              Total_Market_Value = total_market_value)

# correct formatting
market_value_df$Date <- as.Date(market_value_df$Date)

# Find year
market_value_df$Year <- year(market_value_df$Date)

# Combine the data to calculate annual market values for each portfolio
annual_market_values <- data.frame(Year = market_value_df$Year)

# loop to calculate annual percent of market value
for (i in 1:ncol(num_firms)) {
  annual_market_values[paste("PF", i, sep = "_")] <- 
    (num_firms[, i] * avg_firm_size[, i]) / total_market_value
}

# average percent of market value across years for each i=25 pf's
average_percent_by_portfolio <- annual_market_values %>%
  summarise(across(starts_with("PF"), ~ mean(.x, na.rm = TRUE)))
average_percent_by_portfolio <- average_percent_by_portfolio*100
print(average_percent_by_portfolio)



### Average of annual number of firms in portfolio
colMeans(MOMdep_Number_of_Firms_in_Portfolios_part2[,-1])


### Average of annual $E/P$ ratios (in percent) for portfolio



### Average of annual $D/P$ ratios (in percent) for portfolio







####################################################################################################################
####################################################################################################################
#------------------------------------------ Pre FF -----------------------------------------------------------------
####################################################################################################################
####################################################################################################################




### Average of annual averages of firm size
colMeans(MOMdep_Average_Firm_Size_part2[1:438,-1])


### Average of annual $BE/ME$ ratios for portfolio




### Average of annual percent of market value in portfolio



### Average of annual number of firms in portfolio



### Average of annual $E/P$ ratios (in percent) for portfolio



### Average of annual $D/P$ ratios (in percent) or portfolio













####################################################################################################################
####################################################################################################################
#---------------------------------------- During FF ----------------------------------------------------------------
####################################################################################################################
####################################################################################################################






### Average of annual averages of firm size
colMeans(MOMdep_Average_Firm_Size_part2[439:780,-1])


### Average of annual $BE/ME$ ratios for portfolio




### Average of annual percent of market value in portfolio



### Average of annual number of firms in portfolio



### Average of annual $E/P$ ratios (in percent) for portfolio



### Average of annual $D/P$ ratios (in percent) or portfolio












####################################################################################################################
####################################################################################################################
#------------------------------------------ Post FF ----------------------------------------------------------------
####################################################################################################################
####################################################################################################################



### Average of annual averages of firm size
colMeans(MOMdep_Average_Firm_Size_part2[781:1164,-1])


### Average of annual $BE/ME$ ratios for portfolio




### Average of annual percent of market value in portfolio



### Average of annual number of firms in portfolio



### Average of annual $E/P$ ratios (in percent) for portfolio



### Average of annual $D/P$ ratios (in percent) or portfolio





