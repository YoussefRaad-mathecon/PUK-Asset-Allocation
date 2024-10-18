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
MOMdep_Average_Firm_Size$Date <- as.Date(MOMdep_Average_Firm_Size$Date, format="%Y-%m-%d")
MOMdep_Average_Firm_Size$Year <- format(MOMdep_Average_Firm_Size$Date, "%Y")
Firm_Size_Annual <- MOMdep_Average_Firm_Size[1:1164,] %>%
  group_by(Year) %>%
  summarise(
    SMALL.LoPRIOR = mean(SMALL.LoPRIOR, na.rm = TRUE),
    ME1.PRIOR2 = mean(ME1.PRIOR2, na.rm = TRUE),
    ME1.PRIOR3 = mean(ME1.PRIOR3, na.rm = TRUE),
    ME1.PRIOR4 = mean(ME1.PRIOR4, na.rm = TRUE),
    SMALL.HiPRIOR = mean(SMALL.HiPRIOR, na.rm = TRUE),
    ME2.PRIOR1 = mean(ME2.PRIOR1, na.rm = TRUE),
    ME2.PRIOR2 = mean(ME2.PRIOR2, na.rm = TRUE),
    ME2.PRIOR3 = mean(ME2.PRIOR3, na.rm = TRUE),
    ME2.PRIOR4 = mean(ME2.PRIOR4, na.rm = TRUE),
    ME2.PRIOR5 = mean(ME2.PRIOR5, na.rm = TRUE),
    ME3.PRIOR1 = mean(ME3.PRIOR1, na.rm = TRUE),
    ME3.PRIOR2 = mean(ME3.PRIOR2, na.rm = TRUE),
    ME3.PRIOR3 = mean(ME3.PRIOR3, na.rm = TRUE),
    ME3.PRIOR4 = mean(ME3.PRIOR4, na.rm = TRUE),
    ME3.PRIOR5 = mean(ME3.PRIOR5, na.rm = TRUE),
    ME4.PRIOR1 = mean(ME4.PRIOR1, na.rm = TRUE),
    ME4.PRIOR2 = mean(ME4.PRIOR2, na.rm = TRUE),
    ME4.PRIOR3 = mean(ME4.PRIOR3, na.rm = TRUE),
    ME4.PRIOR4 = mean(ME4.PRIOR4, na.rm = TRUE),
    ME4.PRIOR5 = mean(ME4.PRIOR5, na.rm = TRUE),
    BIG.LoPRIOR = mean(BIG.LoPRIOR, na.rm = TRUE),
    ME5.PRIOR2 = mean(ME5.PRIOR2, na.rm = TRUE),
    ME5.PRIOR3 = mean(ME5.PRIOR3, na.rm = TRUE),
    ME5.PRIOR4 = mean(ME5.PRIOR4, na.rm = TRUE),
    BIG.HiPRIOR = mean(BIG.HiPRIOR, na.rm = TRUE)
  )

colMeans(Firm_Size_Annual[,-1])

### Average of annual $BE/ME$ ratios for portfolio

### NOT RELEVANT (we think)


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
MOMdep_Number_of_Firms_in_Portfolios$Date <- as.Date(MOMdep_Number_of_Firms_in_Portfolios$Date, format="%Y-%m-%d")
MOMdep_Number_of_Firms_in_Portfolios$Year <- format(MOMdep_Number_of_Firms_in_Portfolios$Date, "%Y")
Number_of_Firms_in_Portfolios_Annual <- MOMdep_Number_of_Firms_in_Portfolios[1:1164,] %>%
  group_by(Year) %>%
  summarise(
    SMALL.LoPRIOR = mean(SMALL.LoPRIOR, na.rm = TRUE),
    ME1.PRIOR2 = mean(ME1.PRIOR2, na.rm = TRUE),
    ME1.PRIOR3 = mean(ME1.PRIOR3, na.rm = TRUE),
    ME1.PRIOR4 = mean(ME1.PRIOR4, na.rm = TRUE),
    SMALL.HiPRIOR = mean(SMALL.HiPRIOR, na.rm = TRUE),
    ME2.PRIOR1 = mean(ME2.PRIOR1, na.rm = TRUE),
    ME2.PRIOR2 = mean(ME2.PRIOR2, na.rm = TRUE),
    ME2.PRIOR3 = mean(ME2.PRIOR3, na.rm = TRUE),
    ME2.PRIOR4 = mean(ME2.PRIOR4, na.rm = TRUE),
    ME2.PRIOR5 = mean(ME2.PRIOR5, na.rm = TRUE),
    ME3.PRIOR1 = mean(ME3.PRIOR1, na.rm = TRUE),
    ME3.PRIOR2 = mean(ME3.PRIOR2, na.rm = TRUE),
    ME3.PRIOR3 = mean(ME3.PRIOR3, na.rm = TRUE),
    ME3.PRIOR4 = mean(ME3.PRIOR4, na.rm = TRUE),
    ME3.PRIOR5 = mean(ME3.PRIOR5, na.rm = TRUE),
    ME4.PRIOR1 = mean(ME4.PRIOR1, na.rm = TRUE),
    ME4.PRIOR2 = mean(ME4.PRIOR2, na.rm = TRUE),
    ME4.PRIOR3 = mean(ME4.PRIOR3, na.rm = TRUE),
    ME4.PRIOR4 = mean(ME4.PRIOR4, na.rm = TRUE),
    ME4.PRIOR5 = mean(ME4.PRIOR5, na.rm = TRUE),
    BIG.LoPRIOR = mean(BIG.LoPRIOR, na.rm = TRUE),
    ME5.PRIOR2 = mean(ME5.PRIOR2, na.rm = TRUE),
    ME5.PRIOR3 = mean(ME5.PRIOR3, na.rm = TRUE),
    ME5.PRIOR4 = mean(ME5.PRIOR4, na.rm = TRUE),
    BIG.HiPRIOR = mean(BIG.HiPRIOR, na.rm = TRUE)
  )

colMeans(Number_of_Firms_in_Portfolios_Annual[,-1])


### Average of annual $E/P$ ratios (in percent) for portfolio

### NOT RELEVANT



### Average of annual $D/P$ ratios (in percent) for portfolio

### NOT RELEVANT




####################################################################################################################
####################################################################################################################
#------------------------------------------ Pre FF -----------------------------------------------------------------
####################################################################################################################
####################################################################################################################




## Split into subsets of subsets and cut timeline of the CSV file 6_Portfolios_ME_Prior_12_2.CSV
# Average Value Weighted Returns -- Monthly
MOMdep_Average_Value_Weighted_Returns_Monthly_preFF <- MOMdep_Average_Value_Weighted_Returns_Monthly_part2[1:438,]
# Number of Firms in Portfolios
MOMdep_Number_of_Firms_in_Portfolios_preFF <- MOMdep_Number_of_Firms_in_Portfolios_part2[1:438,]
# Average Firm Size
MOMdep_Average_Firm_Size_preFF <- MOMdep_Average_Firm_Size_part2[1:438,]




### Average of annual averages of firm size
MOMdep_Average_Firm_Size$Date <- as.Date(MOMdep_Average_Firm_Size$Date, format="%Y-%m-%d")
MOMdep_Average_Firm_Size$Year <- format(MOMdep_Average_Firm_Size$Date, "%Y")
Firm_Size_Annual <- MOMdep_Average_Firm_Size[1:438,] %>%
  group_by(Year) %>%
  summarise(
    SMALL.LoPRIOR = mean(SMALL.LoPRIOR, na.rm = TRUE),
    ME1.PRIOR2 = mean(ME1.PRIOR2, na.rm = TRUE),
    ME1.PRIOR3 = mean(ME1.PRIOR3, na.rm = TRUE),
    ME1.PRIOR4 = mean(ME1.PRIOR4, na.rm = TRUE),
    SMALL.HiPRIOR = mean(SMALL.HiPRIOR, na.rm = TRUE),
    ME2.PRIOR1 = mean(ME2.PRIOR1, na.rm = TRUE),
    ME2.PRIOR2 = mean(ME2.PRIOR2, na.rm = TRUE),
    ME2.PRIOR3 = mean(ME2.PRIOR3, na.rm = TRUE),
    ME2.PRIOR4 = mean(ME2.PRIOR4, na.rm = TRUE),
    ME2.PRIOR5 = mean(ME2.PRIOR5, na.rm = TRUE),
    ME3.PRIOR1 = mean(ME3.PRIOR1, na.rm = TRUE),
    ME3.PRIOR2 = mean(ME3.PRIOR2, na.rm = TRUE),
    ME3.PRIOR3 = mean(ME3.PRIOR3, na.rm = TRUE),
    ME3.PRIOR4 = mean(ME3.PRIOR4, na.rm = TRUE),
    ME3.PRIOR5 = mean(ME3.PRIOR5, na.rm = TRUE),
    ME4.PRIOR1 = mean(ME4.PRIOR1, na.rm = TRUE),
    ME4.PRIOR2 = mean(ME4.PRIOR2, na.rm = TRUE),
    ME4.PRIOR3 = mean(ME4.PRIOR3, na.rm = TRUE),
    ME4.PRIOR4 = mean(ME4.PRIOR4, na.rm = TRUE),
    ME4.PRIOR5 = mean(ME4.PRIOR5, na.rm = TRUE),
    BIG.LoPRIOR = mean(BIG.LoPRIOR, na.rm = TRUE),
    ME5.PRIOR2 = mean(ME5.PRIOR2, na.rm = TRUE),
    ME5.PRIOR3 = mean(ME5.PRIOR3, na.rm = TRUE),
    ME5.PRIOR4 = mean(ME5.PRIOR4, na.rm = TRUE),
    BIG.HiPRIOR = mean(BIG.HiPRIOR, na.rm = TRUE)
  )

colMeans(Firm_Size_Annual[,-1])

### Average of annual $BE/ME$ ratios for portfolio

### NOT RELEVANT (we think)


### Average of annual percent of market value in portfolio

# remove date
num_firms <- MOMdep_Number_of_Firms_in_Portfolios_preFF[, -1]
avg_firm_size <- MOMdep_Average_Firm_Size_preFF[, -1]
avg_value_weighted_returns <- MOMdep_Average_Value_Weighted_Returns_Monthly_preFF[, -1]

# calculate pf weights
portfolio_weights <- num_firms * avg_firm_size

# calculate total market value for each portfolio
total_market_value <- rowSums(portfolio_weights)

# create a data frame with dates and total market values
market_value_df <- data.frame(Date = MOMdep_Average_Value_Weighted_Returns_Monthly_preFF[, 1],
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
MOMdep_Number_of_Firms_in_Portfolios$Date <- as.Date(MOMdep_Number_of_Firms_in_Portfolios$Date, format="%Y-%m-%d")
MOMdep_Number_of_Firms_in_Portfolios$Year <- format(MOMdep_Number_of_Firms_in_Portfolios$Date, "%Y")
Number_of_Firms_in_Portfolios_Annual <- MOMdep_Number_of_Firms_in_Portfolios[1:438,] %>%
  group_by(Year) %>%
  summarise(
    SMALL.LoPRIOR = mean(SMALL.LoPRIOR, na.rm = TRUE),
    ME1.PRIOR2 = mean(ME1.PRIOR2, na.rm = TRUE),
    ME1.PRIOR3 = mean(ME1.PRIOR3, na.rm = TRUE),
    ME1.PRIOR4 = mean(ME1.PRIOR4, na.rm = TRUE),
    SMALL.HiPRIOR = mean(SMALL.HiPRIOR, na.rm = TRUE),
    ME2.PRIOR1 = mean(ME2.PRIOR1, na.rm = TRUE),
    ME2.PRIOR2 = mean(ME2.PRIOR2, na.rm = TRUE),
    ME2.PRIOR3 = mean(ME2.PRIOR3, na.rm = TRUE),
    ME2.PRIOR4 = mean(ME2.PRIOR4, na.rm = TRUE),
    ME2.PRIOR5 = mean(ME2.PRIOR5, na.rm = TRUE),
    ME3.PRIOR1 = mean(ME3.PRIOR1, na.rm = TRUE),
    ME3.PRIOR2 = mean(ME3.PRIOR2, na.rm = TRUE),
    ME3.PRIOR3 = mean(ME3.PRIOR3, na.rm = TRUE),
    ME3.PRIOR4 = mean(ME3.PRIOR4, na.rm = TRUE),
    ME3.PRIOR5 = mean(ME3.PRIOR5, na.rm = TRUE),
    ME4.PRIOR1 = mean(ME4.PRIOR1, na.rm = TRUE),
    ME4.PRIOR2 = mean(ME4.PRIOR2, na.rm = TRUE),
    ME4.PRIOR3 = mean(ME4.PRIOR3, na.rm = TRUE),
    ME4.PRIOR4 = mean(ME4.PRIOR4, na.rm = TRUE),
    ME4.PRIOR5 = mean(ME4.PRIOR5, na.rm = TRUE),
    BIG.LoPRIOR = mean(BIG.LoPRIOR, na.rm = TRUE),
    ME5.PRIOR2 = mean(ME5.PRIOR2, na.rm = TRUE),
    ME5.PRIOR3 = mean(ME5.PRIOR3, na.rm = TRUE),
    ME5.PRIOR4 = mean(ME5.PRIOR4, na.rm = TRUE),
    BIG.HiPRIOR = mean(BIG.HiPRIOR, na.rm = TRUE)
  )

colMeans(Number_of_Firms_in_Portfolios_Annual[,-1])


### Average of annual $E/P$ ratios (in percent) for portfolio

### NOT RELEVANT

### Average of annual $D/P$ ratios (in percent) for portfolio

### NOT RELEVANT








####################################################################################################################
####################################################################################################################
#---------------------------------------- During FF ----------------------------------------------------------------
####################################################################################################################
####################################################################################################################




## Split into subsets of subsets and cut timeline of the CSV file 6_Portfolios_ME_Prior_12_2.CSV
# Average Value Weighted Returns -- Monthly
MOMdep_Average_Value_Weighted_Returns_Monthly_duringFF <- MOMdep_Average_Value_Weighted_Returns_Monthly_part2[439:780,]
# Number of Firms in Portfolios
MOMdep_Number_of_Firms_in_Portfolios_duringFF <- MOMdep_Number_of_Firms_in_Portfolios_part2[439:780,]
# Average Firm Size
MOMdep_Average_Firm_Size_duringFF <- MOMdep_Average_Firm_Size_part2[439:780,]




### Average of annual averages of firm size
MOMdep_Average_Firm_Size$Date <- as.Date(MOMdep_Average_Firm_Size$Date, format="%Y-%m-%d")
MOMdep_Average_Firm_Size$Year <- format(MOMdep_Average_Firm_Size$Date, "%Y")
Firm_Size_Annual <- MOMdep_Average_Firm_Size[439:780,] %>%
  group_by(Year) %>%
  summarise(
    SMALL.LoPRIOR = mean(SMALL.LoPRIOR, na.rm = TRUE),
    ME1.PRIOR2 = mean(ME1.PRIOR2, na.rm = TRUE),
    ME1.PRIOR3 = mean(ME1.PRIOR3, na.rm = TRUE),
    ME1.PRIOR4 = mean(ME1.PRIOR4, na.rm = TRUE),
    SMALL.HiPRIOR = mean(SMALL.HiPRIOR, na.rm = TRUE),
    ME2.PRIOR1 = mean(ME2.PRIOR1, na.rm = TRUE),
    ME2.PRIOR2 = mean(ME2.PRIOR2, na.rm = TRUE),
    ME2.PRIOR3 = mean(ME2.PRIOR3, na.rm = TRUE),
    ME2.PRIOR4 = mean(ME2.PRIOR4, na.rm = TRUE),
    ME2.PRIOR5 = mean(ME2.PRIOR5, na.rm = TRUE),
    ME3.PRIOR1 = mean(ME3.PRIOR1, na.rm = TRUE),
    ME3.PRIOR2 = mean(ME3.PRIOR2, na.rm = TRUE),
    ME3.PRIOR3 = mean(ME3.PRIOR3, na.rm = TRUE),
    ME3.PRIOR4 = mean(ME3.PRIOR4, na.rm = TRUE),
    ME3.PRIOR5 = mean(ME3.PRIOR5, na.rm = TRUE),
    ME4.PRIOR1 = mean(ME4.PRIOR1, na.rm = TRUE),
    ME4.PRIOR2 = mean(ME4.PRIOR2, na.rm = TRUE),
    ME4.PRIOR3 = mean(ME4.PRIOR3, na.rm = TRUE),
    ME4.PRIOR4 = mean(ME4.PRIOR4, na.rm = TRUE),
    ME4.PRIOR5 = mean(ME4.PRIOR5, na.rm = TRUE),
    BIG.LoPRIOR = mean(BIG.LoPRIOR, na.rm = TRUE),
    ME5.PRIOR2 = mean(ME5.PRIOR2, na.rm = TRUE),
    ME5.PRIOR3 = mean(ME5.PRIOR3, na.rm = TRUE),
    ME5.PRIOR4 = mean(ME5.PRIOR4, na.rm = TRUE),
    BIG.HiPRIOR = mean(BIG.HiPRIOR, na.rm = TRUE)
  )

colMeans(Firm_Size_Annual[,-1])

### Average of annual $BE/ME$ ratios for portfolio

### NOT RELEVANT (we think)


### Average of annual percent of market value in portfolio

# remove date
num_firms <- MOMdep_Number_of_Firms_in_Portfolios_duringFF[, -1]
avg_firm_size <- MOMdep_Average_Firm_Size_duringFF[, -1]
avg_value_weighted_returns <- MOMdep_Average_Value_Weighted_Returns_Monthly_duringFF[, -1]

# calculate pf weights
portfolio_weights <- num_firms * avg_firm_size

# calculate total market value for each portfolio
total_market_value <- rowSums(portfolio_weights)

# create a data frame with dates and total market values
market_value_df <- data.frame(Date = MOMdep_Average_Value_Weighted_Returns_Monthly_duringFF[, 1],
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
MOMdep_Number_of_Firms_in_Portfolios$Date <- as.Date(MOMdep_Number_of_Firms_in_Portfolios$Date, format="%Y-%m-%d")
MOMdep_Number_of_Firms_in_Portfolios$Year <- format(MOMdep_Number_of_Firms_in_Portfolios$Date, "%Y")
Number_of_Firms_in_Portfolios_Annual <- MOMdep_Number_of_Firms_in_Portfolios[439:780,] %>%
  group_by(Year) %>%
  summarise(
    SMALL.LoPRIOR = mean(SMALL.LoPRIOR, na.rm = TRUE),
    ME1.PRIOR2 = mean(ME1.PRIOR2, na.rm = TRUE),
    ME1.PRIOR3 = mean(ME1.PRIOR3, na.rm = TRUE),
    ME1.PRIOR4 = mean(ME1.PRIOR4, na.rm = TRUE),
    SMALL.HiPRIOR = mean(SMALL.HiPRIOR, na.rm = TRUE),
    ME2.PRIOR1 = mean(ME2.PRIOR1, na.rm = TRUE),
    ME2.PRIOR2 = mean(ME2.PRIOR2, na.rm = TRUE),
    ME2.PRIOR3 = mean(ME2.PRIOR3, na.rm = TRUE),
    ME2.PRIOR4 = mean(ME2.PRIOR4, na.rm = TRUE),
    ME2.PRIOR5 = mean(ME2.PRIOR5, na.rm = TRUE),
    ME3.PRIOR1 = mean(ME3.PRIOR1, na.rm = TRUE),
    ME3.PRIOR2 = mean(ME3.PRIOR2, na.rm = TRUE),
    ME3.PRIOR3 = mean(ME3.PRIOR3, na.rm = TRUE),
    ME3.PRIOR4 = mean(ME3.PRIOR4, na.rm = TRUE),
    ME3.PRIOR5 = mean(ME3.PRIOR5, na.rm = TRUE),
    ME4.PRIOR1 = mean(ME4.PRIOR1, na.rm = TRUE),
    ME4.PRIOR2 = mean(ME4.PRIOR2, na.rm = TRUE),
    ME4.PRIOR3 = mean(ME4.PRIOR3, na.rm = TRUE),
    ME4.PRIOR4 = mean(ME4.PRIOR4, na.rm = TRUE),
    ME4.PRIOR5 = mean(ME4.PRIOR5, na.rm = TRUE),
    BIG.LoPRIOR = mean(BIG.LoPRIOR, na.rm = TRUE),
    ME5.PRIOR2 = mean(ME5.PRIOR2, na.rm = TRUE),
    ME5.PRIOR3 = mean(ME5.PRIOR3, na.rm = TRUE),
    ME5.PRIOR4 = mean(ME5.PRIOR4, na.rm = TRUE),
    BIG.HiPRIOR = mean(BIG.HiPRIOR, na.rm = TRUE)
  )

colMeans(Number_of_Firms_in_Portfolios_Annual[,-1])


### Average of annual $E/P$ ratios (in percent) for portfolio

### NOT RELEVANT

### Average of annual $D/P$ ratios (in percent) for portfolio

### NOT RELEVANT











####################################################################################################################
####################################################################################################################
#------------------------------------------ Post FF ----------------------------------------------------------------
####################################################################################################################
####################################################################################################################



## Split into subsets of subsets and cut timeline of the CSV file 6_Portfolios_ME_Prior_12_2.CSV
# Average Value Weighted Returns -- Monthly
MOMdep_Average_Value_Weighted_Returns_Monthly_postFF <- MOMdep_Average_Value_Weighted_Returns_Monthly_part2[781:1164,]
# Number of Firms in Portfolios
MOMdep_Number_of_Firms_in_Portfolios_postFF <- MOMdep_Number_of_Firms_in_Portfolios_part2[781:1164,]
# Average Firm Size
MOMdep_Average_Firm_Size_postFF <- MOMdep_Average_Firm_Size_part2[781:1164,]




### Average of annual averages of firm size
MOMdep_Average_Firm_Size$Date <- as.Date(MOMdep_Average_Firm_Size$Date, format="%Y-%m-%d")
MOMdep_Average_Firm_Size$Year <- format(MOMdep_Average_Firm_Size$Date, "%Y")
Firm_Size_Annual <- MOMdep_Average_Firm_Size[781:1164,] %>%
  group_by(Year) %>%
  summarise(
    SMALL.LoPRIOR = mean(SMALL.LoPRIOR, na.rm = TRUE),
    ME1.PRIOR2 = mean(ME1.PRIOR2, na.rm = TRUE),
    ME1.PRIOR3 = mean(ME1.PRIOR3, na.rm = TRUE),
    ME1.PRIOR4 = mean(ME1.PRIOR4, na.rm = TRUE),
    SMALL.HiPRIOR = mean(SMALL.HiPRIOR, na.rm = TRUE),
    ME2.PRIOR1 = mean(ME2.PRIOR1, na.rm = TRUE),
    ME2.PRIOR2 = mean(ME2.PRIOR2, na.rm = TRUE),
    ME2.PRIOR3 = mean(ME2.PRIOR3, na.rm = TRUE),
    ME2.PRIOR4 = mean(ME2.PRIOR4, na.rm = TRUE),
    ME2.PRIOR5 = mean(ME2.PRIOR5, na.rm = TRUE),
    ME3.PRIOR1 = mean(ME3.PRIOR1, na.rm = TRUE),
    ME3.PRIOR2 = mean(ME3.PRIOR2, na.rm = TRUE),
    ME3.PRIOR3 = mean(ME3.PRIOR3, na.rm = TRUE),
    ME3.PRIOR4 = mean(ME3.PRIOR4, na.rm = TRUE),
    ME3.PRIOR5 = mean(ME3.PRIOR5, na.rm = TRUE),
    ME4.PRIOR1 = mean(ME4.PRIOR1, na.rm = TRUE),
    ME4.PRIOR2 = mean(ME4.PRIOR2, na.rm = TRUE),
    ME4.PRIOR3 = mean(ME4.PRIOR3, na.rm = TRUE),
    ME4.PRIOR4 = mean(ME4.PRIOR4, na.rm = TRUE),
    ME4.PRIOR5 = mean(ME4.PRIOR5, na.rm = TRUE),
    BIG.LoPRIOR = mean(BIG.LoPRIOR, na.rm = TRUE),
    ME5.PRIOR2 = mean(ME5.PRIOR2, na.rm = TRUE),
    ME5.PRIOR3 = mean(ME5.PRIOR3, na.rm = TRUE),
    ME5.PRIOR4 = mean(ME5.PRIOR4, na.rm = TRUE),
    BIG.HiPRIOR = mean(BIG.HiPRIOR, na.rm = TRUE)
  )

colMeans(Firm_Size_Annual[,-1])

### Average of annual $BE/ME$ ratios for portfolio

### NOT RELEVANT (we think)


### Average of annual percent of market value in portfolio

# remove date
num_firms <- MOMdep_Number_of_Firms_in_Portfolios_postFF[, -1]
avg_firm_size <- MOMdep_Average_Firm_Size_postFF[, -1]
avg_value_weighted_returns <- MOMdep_Average_Value_Weighted_Returns_Monthly_postFF[, -1]

# calculate pf weights
portfolio_weights <- num_firms * avg_firm_size

# calculate total market value for each portfolio
total_market_value <- rowSums(portfolio_weights)

# create a data frame with dates and total market values
market_value_df <- data.frame(Date = MOMdep_Average_Value_Weighted_Returns_Monthly_postFF[, 1],
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
MOMdep_Number_of_Firms_in_Portfolios$Date <- as.Date(MOMdep_Number_of_Firms_in_Portfolios$Date, format="%Y-%m-%d")
MOMdep_Number_of_Firms_in_Portfolios$Year <- format(MOMdep_Number_of_Firms_in_Portfolios$Date, "%Y")
Number_of_Firms_in_Portfolios_Annual <- MOMdep_Number_of_Firms_in_Portfolios[781:1164,] %>%
  group_by(Year) %>%
  summarise(
    SMALL.LoPRIOR = mean(SMALL.LoPRIOR, na.rm = TRUE),
    ME1.PRIOR2 = mean(ME1.PRIOR2, na.rm = TRUE),
    ME1.PRIOR3 = mean(ME1.PRIOR3, na.rm = TRUE),
    ME1.PRIOR4 = mean(ME1.PRIOR4, na.rm = TRUE),
    SMALL.HiPRIOR = mean(SMALL.HiPRIOR, na.rm = TRUE),
    ME2.PRIOR1 = mean(ME2.PRIOR1, na.rm = TRUE),
    ME2.PRIOR2 = mean(ME2.PRIOR2, na.rm = TRUE),
    ME2.PRIOR3 = mean(ME2.PRIOR3, na.rm = TRUE),
    ME2.PRIOR4 = mean(ME2.PRIOR4, na.rm = TRUE),
    ME2.PRIOR5 = mean(ME2.PRIOR5, na.rm = TRUE),
    ME3.PRIOR1 = mean(ME3.PRIOR1, na.rm = TRUE),
    ME3.PRIOR2 = mean(ME3.PRIOR2, na.rm = TRUE),
    ME3.PRIOR3 = mean(ME3.PRIOR3, na.rm = TRUE),
    ME3.PRIOR4 = mean(ME3.PRIOR4, na.rm = TRUE),
    ME3.PRIOR5 = mean(ME3.PRIOR5, na.rm = TRUE),
    ME4.PRIOR1 = mean(ME4.PRIOR1, na.rm = TRUE),
    ME4.PRIOR2 = mean(ME4.PRIOR2, na.rm = TRUE),
    ME4.PRIOR3 = mean(ME4.PRIOR3, na.rm = TRUE),
    ME4.PRIOR4 = mean(ME4.PRIOR4, na.rm = TRUE),
    ME4.PRIOR5 = mean(ME4.PRIOR5, na.rm = TRUE),
    BIG.LoPRIOR = mean(BIG.LoPRIOR, na.rm = TRUE),
    ME5.PRIOR2 = mean(ME5.PRIOR2, na.rm = TRUE),
    ME5.PRIOR3 = mean(ME5.PRIOR3, na.rm = TRUE),
    ME5.PRIOR4 = mean(ME5.PRIOR4, na.rm = TRUE),
    BIG.HiPRIOR = mean(BIG.HiPRIOR, na.rm = TRUE)
  )

colMeans(Number_of_Firms_in_Portfolios_Annual[,-1])


### Average of annual $E/P$ ratios (in percent) for portfolio

### NOT RELEVANT

### Average of annual $D/P$ ratios (in percent) for portfolio

### NOT RELEVANT

