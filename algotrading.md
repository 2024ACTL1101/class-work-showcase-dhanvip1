
## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1. **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2. **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3. **Customize Trading Period:** Choose your entry and exit dates.

4. **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5. **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI. 

6. **Discussion:** Summarise your finding.


## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates. 

```r
# Load data from CSV file
amd_df <- read.csv("AMD.csv")
# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)
amd_df <- amd_df[, c("date", "close")]
```

#### Plotting the Data
Plot the closing prices over time to visualize the price movement.
```r
plot(amd_df$date, amd_df$close,'l')
```

### Step 2: Trading Algorithm
Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the dataframe to execute trades based on the set conditions.

- Initialize Columns: Start by ensuring dataframe has columns 'trade_type', 'costs_proceeds' and 'accumulated_shares'.
- Change the algorithm by modifying the loop to include the cost and proceeds metrics for buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one:
  - If the previous price = 0, set 'trade_type' to 'buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable.
  - Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100.
  - You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price.
  - If this is the last day of trading, set the 'trade_type' to 'sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.



```r
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

for (i in 1:nrow(amd_df)) {
# Fill your code here
 #When trading first begins, i.e. when i==1, I set the shares to be bought for simplicity. Then the costs proceeds are the share size times the price of the share. Accumulated shares are now set to be the share size as only one set of shares are bought.
  if (i==1){
    amd_df$trade_type[1] <- 'buy'
    amd_df$costs_proceeds[1] <- -share_size*amd_df$close[1]
    amd_df$accumulated_shares[1]<- share_size
    
  #The second case is when the price of a share on the current day is lower than the price of the share on the previous day in which case I set the share to be buy, the accumulated shares of the previous day increase by the share size as shares are bought.
  } else if (amd_df$close[i]<amd_df$close[i-1]) {
    amd_df$trade_type[i]<-'buy'
    amd_df$costs_proceeds[i] <- amd_df$close[i] * -share_size
    amd_df$accumulated_shares[i] <- amd_df$accumulated_shares[i-1] + share_size
    
  #This is the case when the previous day's share price is zero, where we set the shares to be bought.
  } else if (amd_df$close[i-1]==0) {
    amd_df$trade_type[i]<-'buy'
    amd_df$costs_proceeds[i] <- amd_df$close[i]*-share_size
    amd_df$accumulated_shares[i] <- amd_df$accumulated_shares[i-1] + share_size
    
  #This is the case for the last and final day where we sell all shares. Hence accumulated shares becomes 0 as all shares are sold. Costs proceeds is the current day's closing price times the previous day's accumulated shares as this accounts for the final price of all the shares.
  } else if (i==1259) {
  amd_df$trade_type[1259] <- 'sell'
  amd_df$accumulated_shares[1259] <- 0
  amd_df$costs_proceeds[1259] <- amd_df$close[1259]*amd_df$accumulated_shares[1258]
  
  #This is the case for all other cases wherein the instructions state the algorithm is not modified.
  } else {
    amd_df$trade_type[i] <- 0
    amd_df$costs_proceeds[i]<- 0
    amd_df$accumulated_shares[i] <- amd_df$accumulated_shares[i-1]
  }
}
```


### Step 3: Customize Trading Period
- Define a trading period you wanted in the past five years 
```r
# Fill your code here
#I have chosen my period from 4/01/2021 to 31/12/2021 hence whey they are defined as start_date and end_date respectively.
start_date <- as.Date('2021-01-04')
end_date <- as.Date('2021-12-31')

#Here I am filtering the data so that I select all rows of my data frame between the start date and end date.
amd_df <- amd_df[amd_df$date >= start_date & amd_df$date <= end_date, ]

# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

#Now, I am applying the same instructions as STEP 2 to the modified trading period.
for (i in 1:nrow(amd_df)) {
  #When trading first begins, i.e. when i==1, I set the shares to be bought for simplicity. Then the costs proceeds are the share size times the price of the share. Accumulated shares are now set to be the share size as only one set of shares are bought.
  if (i==1){
    amd_df$trade_type[1] <- 'buy'
    amd_df$costs_proceeds[1] <- -share_size*amd_df$close[1]
    amd_df$accumulated_shares[1]<- share_size
    
  #Here I am considering the last row which happens to be the 250th row in the restricted data frame. Once again, since it is the final row I set the trade type to sell and accumulated shares are now 0 as they are all sold. The costs proceeds is the current day's closing price times the accumulated shares for the previous day.
  } else if (i==250) {
  amd_df$trade_type[i] <- 'sell'
  amd_df$accumulated_shares[i] <- 0
  amd_df$costs_proceeds[i] <- amd_df$close[i]*amd_df$accumulated_shares[i-1]
  
  #This case is when the price of a share on the current day is lower than the price of the share on the previous day in which case I set the share to be buy, the accumulated shares of the previous day increase by the share size as shares are bought.
  } else if (amd_df$close[i]< amd_df$close[i-1]) {
    amd_df$trade_type[i]<-'buy'
    amd_df$costs_proceeds[i] <- amd_df$close[i] * -share_size
    amd_df$accumulated_shares[i] <- amd_df$accumulated_shares[i-1] + share_size
    
  #This is the case when the previous day's closing price equals 0, where we set the shares to be bought.
  } else if (amd_df$close[i-1]==0) {
    amd_df$trade_type[i]<-'buy'
    amd_df$costs_proceeds[i] <- amd_df$close[i]*-share_size
    amd_df$accumulated_shares[i] <- amd_df$accumulated_shares[i-1] + share_size
    
  #This is the case for all other cases wherein the instructions state the algorithm is not modified.
  } else {
    amd_df$trade_type[i] <- 0
    amd_df$costs_proceeds[i]<- 0
    amd_df$accumulated_shares[i] <- amd_df$accumulated_shares[i-1]
  }
}
```


### Step 4: Run Your Algorithm and Analyze Results
After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

- Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
- Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
- ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

```r
# Fill your code here
#Firstly I am specifying a period as I did previously.
start_date <- as.Date('2021-01-04')
end_date <- as.Date('2021-12-31')
amd_df <- amd_df[amd_df$date >= start_date & amd_df$date <= end_date, ]

#I am initializing columns for trade.
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA
amd_df$accumulated_shares <- 0

# Initialize variables for trading logic.
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

#This is the for loop which adjusts my data frame per the given instructions, it is identical to STEP 3.
for (i in 1:nrow(amd_df)) {
  
  if (i==1){
    amd_df$trade_type[1] <- 'buy'
    amd_df$costs_proceeds[1] <- -share_size*amd_df$close[1]
    amd_df$accumulated_shares[1]<- share_size
    
  } else if (i==252) {
  amd_df$trade_type[i] <- 'sell'
  amd_df$accumulated_shares[i] <- 0
  amd_df$costs_proceeds[i] <- amd_df$close[i]*amd_df$accumulated_shares[i-1]
  
  } else if (amd_df$close[i]< amd_df$close[i-1]) {
    amd_df$trade_type[i]<-'buy'
    amd_df$costs_proceeds[i] <- amd_df$close[i] * -share_size
    amd_df$accumulated_shares[i] <- amd_df$accumulated_shares[i-1] + share_size
    
  } else if (amd_df$close[i-1]==0) {
    amd_df$trade_type[i]<-'buy'
    amd_df$costs_proceeds[i] <- amd_df$close[i]*-share_size
    amd_df$accumulated_shares[i] <- amd_df$accumulated_shares[i-1] + share_size
    
  } else {
    amd_df$trade_type[i] <- 0
    amd_df$costs_proceeds[i]<- 0
    amd_df$accumulated_shares[i] <- amd_df$accumulated_shares[i-1]
  }
}

#Here I am now attempting to calculate the ROI. To do so, first I will calculate the total profit.

#I define the variable total profit to be the sum of all entries in the 'costs_proceeds' column of my data frame. Hence, I do the sum of the whole column but I add the na.rm=TRUE part to ensure that the presence of 'NA' in my column will not affect my total_profit.
total_profit <- sum(amd_df$costs_proceeds, na.rm=TRUE)

#Here I am printing out the total profit and rounding it to 2 decimal places.
print(paste("Total Profit:", round(total_profit,2)))

#Next, to find the invested capital, I am defining the variable invested_capital as the sum of the cost_proceeds column, but ensuring that it only includes the rows where the trade type is buy. i.e. for all 'buy' transactions.
invested_capital <- -sum(amd_df$costs_proceeds[amd_df$trade_type == 'buy'])

#I then print the invested capital out and round it to 2 decimal places.
print(paste("Invested Capital:", round(invested_capital, 2)))

#To calculate the ROI I use the given formula, where I divIde the total profit by invested capital and multiply the fraction by 100. I print it as a percentage that is rounded to 2 decimal places.
ROI <- (total_profit/invested_capital)*100
print(paste("ROI:", round(ROI, 2), "%"))
```

### Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Choose 1)
- Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.
- Option 2: Implement a stop-loss mechanism in the trading strategy that you sell half of your holdings if the stock falls by a certain percentage (e.g., 20%) from the average purchase price. You don't need to buy 100 stocks on the days that the stop-loss mechanism is triggered.


```r
# Fill your code here
#I am going to choose to use the Profit-Taking Strategy, such that I will sell half of my holdings if the price has increased by 20% from the average purchase price.

#Firstly I am specifying a period as I did previously.
start_date <- as.Date('2021-01-04')
end_date <- as.Date('2021-12-31')
amd_df <- amd_df[amd_df$date >= start_date & amd_df$date <= end_date, ]

#I am initializing columns for trade.
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA
amd_df$accumulated_shares <- 0

#To start my profit taking strategy, I will start a new column to represent the average purchase price to make the algorithm more simpler to understand.
amd_df$average_purchase_price<- 0

# Initialize variables for trading logic.
previous_price <- 0
share_size <- 100
accumulated_shares <- 0
accumulated_cost <- 0

# This is the for loop which adjusts my data frame per the given instructions and also addresses the profit taking strategy instructions.
for (i in 1:nrow(amd_df)) {

  #Initial case where we buy the shares.
  if (i==1){
    amd_df$trade_type[1] <- 'buy'
    amd_df$costs_proceeds[1] <- -share_size*amd_df$close[1]
    accumulated_shares <- 100
    accumulated_cost <- amd_df$close[i] * share_size

  #Case when we continue as normal as the price of share has not increased by more than 20% of the average price but the current day's closing price is higher than the previous day's closing price. The accumulated shares increase each time by the previous value plus the additional shares purchased i.e. share size. Accumulated costs is now the previous cost plus the value of the additional shares i.e. closing price times share size.
  } else if (amd_df$close[i]<amd_df$close[i-1]) {
    amd_df$trade_type[i] <- 'buy'
    amd_df$costs_proceeds[i] <- -amd_df$close[i]*share_size
    accumulated_shares <- accumulated_shares + share_size
    accumulated_cost <- accumulated_cost + (amd_df$close[i]*share_size)
  
  # Case when we undertake the profit taking strategy, i.e. we sell half our holdings as the price has increased by 20% from the average purchase price. Here, I ensure in the condition that accumulated shares is >0, to ensure that it is valid to by shares. I defined a new variable which represents the selling of half of the accumulated shares. The new definition for accumulated shares as a result is now the previous accumulated shares - shares sold. Accumulated cost also decreases by the shares sold value i.e. shares sold * average purchase price.
  } else if (accumulated_shares > 0 && amd_df$close[i]>= 1.2*(accumulated_cost/accumulated_shares)) {
    amd_df$trade_type[i] <- 'sell'
    shares_sold <- accumulated_shares/2
    amd_df$costs_proceeds[i] <- amd_df$close[i]*(shares_sold)
    accumulated_shares <- accumulated_shares - shares_sold
    accumulated_cost <- accumulated_cost - (shares_sold*average_purchase_price)
    
  #Case where we hold the shares. This is done when the current day's closing price is higher than the previous price AND there is less than a 20% increase in the average purchasing price. There are no costs proceeds earned in these transactions.
  } else {
    amd_df$trade_type[i] <- 'hold'
    amd_df$costs_proceeds[i] <- 0
  }

  #Here I am now defining the columns of accumulated shares and accumulated cost to be the variable that has undergone the whole loop and thus been altered for the specific conditions.
amd_df$accumulated_shares[i] <- accumulated_shares
amd_df$accumulated_cost[i] <- accumulated_cost

 #Updating the average purchase price. Regardless of shares being sold, the average purchase price will always remain the accumulated cost at that point over the accumulated shares. Otherwise, the average is just 0 when there is a negative number of accumulated shares.
 if (accumulated_shares >0){
  average_purchase_price <- accumulated_cost/accumulated_shares
 } else {
   average_purchase_price <- 0
 }

#Here I am now redefining the average purchase price column to be the average purchase price after it has undergone the whole loop.
amd_df$average_purchase_price[i] <- average_purchase_price

  #The last day where all shares are sold. Since the shares are all sold, accumulated shares becomes zero. The costs proceeds from these transactions are the selling price i.e. closing price times the previous accumulated shares.
   if (i==nrow(amd_df)){
    amd_df$trade_type[i] <- 'sell'
    amd_df$costs_proceeds[i] <- accumulated_shares*amd_df$close[i]
    amd_df$accumulated_shares[i]<- 0
   }
}

#Here I am now attempting to calculate the ROI. This is the exact same method as STEP 4. To do so, first I will calculate the total profit.

#I define the variable total profit to be the sum of all entries in the 'costs_proceeds' column of my data frame. Hence, I do the sum of the whole column but I add the na.rm=TRUE part to ensure that the presence of 'NA' in my column will not affect my total_profit.
total_profit <- sum(amd_df$costs_proceeds, na.rm=TRUE)

#Here I am printing out the total profit and rounding it to 2 decimal places.
print(paste("Total Profit:", round(total_profit,2)))

#Next, to find the invested capital, I am defining the variable invested_capital as the sum of the cost_proceeds column, but ensuring that it only includes the rows where the trade type is buy. i.e. for all 'buy' transactions.
invested_capital <- -sum(amd_df$costs_proceeds[amd_df$trade_type == 'buy'])

#I then print the invested capital out and round it to 2 decimal places.
print(paste("Invested Capital:", round(invested_capital, 2)))

#To calculate the ROI I use the given formula, where I divde the total profit by invested capital and multiply the fraction by 100. I print it as a percentage that is rounded to 2 decimal places.
ROI <- (total_profit/invested_capital)*100
print(paste("ROI:", round(ROI, 2), "%"))
```


### Step 6: Summarize Your Findings
- Did your P/L and ROI improve over your chosen period?
- Relate your results to a relevant market event and explain why these outcomes may have occurred.


```r
# Fill your code here and Disucss
change_in_pnl <- 249695.99 - 538758.93 
print
change_in_ROI <- 20.05 - 43.25 

cat("The change in P/L is:", round(change_in_pnl, 2), "\n")

cat("The change in ROI is:", round(change_in_ROI, 2), "%")
```

Between 04/01/2021 to 31/12/2021, despite an initial decrease in share price to the trough price of 76.22 there was an overall increase in the share price as detailed by the graph. The peaks of the share price graph and the given data show how AMD’s share price began to grow after May. The share price undertakes a steeper upward curve from July, likely due to AMD’s July earnings conference call where AMD highlighted the growth in enterprise demand for its server processors which would increase by more than 100 systems due to companies such as Dell, Hewlett Packard Enterprise, Lenovo and Cisco using it. This would have further increased investors’ confidence on AMD’s future growth and hence contributed to the upward growth of share prices. Furthermore, towards the end of the year, there is another peak that occurs October onwards and follows through the rest of the year likely due to AMD’s collaboration with Microsoft to make AMD Ryzen processors used in Windows 11. Partnering with a large company boosts investors’ confidence in AMD’s technology and its growth, contributing to this upward growth of share prices. This has led towards the end of the year peak at 161.91. The negative P/L change of -289,062.94 represents that the second strategy earned less than the first strategy. This negative ROI change of -23.2% indicates that the second strategy provides a lower return. Hence, the second strategy was not more effective in generating earnings. This may be because the closing prices increasing, they did not exceed the average price by more than 1.2 times and hence shares were primarily being bought in the second strategy. This contributed to greater negative cost proceeds and, in turn, a lower profit and therefore lower return. This can be improved by setting the price increase condition to less than 20% as prices increase minimally over the period.



