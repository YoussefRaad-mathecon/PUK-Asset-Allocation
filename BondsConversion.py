import pandas as pd
import numpy as np

# Load 
bonds = pd.read_csv('C:/Users/youss/OneDrive - University of Copenhagen/PUK/yield-curve-rates-1990-2023.csv')

# Convert the 'Date' column to datetime format, specifying the format to avoid the warning
bonds['Date'] = pd.to_datetime(bonds['Date'], format='%m/%d/%y')

# Filter for end-of-month data using pd.Grouper(freq='M') to get the actual last day of each month
bonds_eom = bonds.set_index('Date').groupby(pd.Grouper(freq='M')).last().reset_index()

# Define the present value (PV) function
def pv(coupon, yield_rate, maturity):
    times = np.arange(1, maturity + 1)
    pv_coupons = np.sum([coupon / (1 + yield_rate)**t for t in times])
    pv_principal = 1 / (1 + yield_rate)**maturity
    return pv_coupons + pv_principal

# Function to compute monthly returns
def compute_monthly_return(yield_prev, yield_curr, maturity):
    # Previous period bond value
    pv_prev = pv(yield_prev, yield_prev, maturity)
    # Current period bond value with 1 month less to maturity
    pv_curr = pv(yield_prev, yield_curr, maturity - 1/12)
    # Calculate the return
    return pv_curr / pv_prev - 1

# Calculate monthly returns for 10-year bonds
returns = []
maturity = 10  # Maturity of 10-year bond
for i in range(1, len(bonds_eom)):
    yield_prev = bonds_eom['10 Yr'].iloc[i-1] / 100
    yield_curr = bonds_eom['10 Yr'].iloc[i] / 100
    monthly_return = compute_monthly_return(yield_prev, yield_curr, maturity)
    returns.append(monthly_return)

# Add returns to the dataframe
bonds_eom['Monthly Return 10 Yr'] = [None] + returns

# Save 
output_path = 'C:/Users/youss/OneDrive - University of Copenhagen/PUK/bonds_with_returns.csv'
bonds_eom.to_csv(output_path, index=False)
