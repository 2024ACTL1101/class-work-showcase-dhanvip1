
# CAPM Analysis

## Introduction

In this assignment, you will explore the foundational concepts of the Capital Asset Pricing Model (CAPM) using historical data for AMD and the S&P 500 index. This exercise is designed to provide a hands-on approach to understanding how these models are used in financial analysis to assess investment risks and returns.

## Background

The CAPM provides a framework to understand the relationship between systematic risk and expected return, especially for stocks. This model is critical for determining the theoretically appropriate required rate of return of an asset, assisting in decisions about adding assets to a diversified portfolio.

## Objectives

1. **Load and Prepare Data:** Import and prepare historical price data for AMD and the S&P 500 to ensure it is ready for detailed analysis.
2. **CAPM Implementation:** Focus will be placed on applying the CAPM to examine the relationship between AMD's stock performance and the overall market as represented by the S&P 500.
3. **Beta Estimation and Analysis:** Calculate the beta of AMD, which measures its volatility relative to the market, providing insights into its systematic risk.
4. **Results Interpretation:** Analyze the outcomes of the CAPM application, discussing the implications of AMD's beta in terms of investment risk and potential returns.

## Instructions

### Step 1: Data Loading

- We are using the `quantmod` package to directly load financial data from Yahoo Finance without the need to manually download and read from a CSV file.
- `quantmod` stands for "Quantitative Financial Modelling Framework". It was developed to aid the quantitative trader in the development, testing, and deployment of statistically based trading models.
- Make sure to install the `quantmod` package by running `install.packages("quantmod")` in the R console before proceeding.

```r
# Set start and end dates
start_date <- as.Date("2019-05-20")
end_date <- as.Date("2024-05-20")

# Load data for AMD, S&P 500, and the 1-month T-Bill (DTB4WK)
amd_data <- getSymbols("AMD", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
gspc_data <- getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
rf_data <- getSymbols("DTB4WK", src = "FRED", from = start_date, to = end_date, auto.assign = FALSE)

# Convert Adjusted Closing Prices and DTB4WK to data frames
amd_df <- data.frame(Date = index(amd_data), AMD = as.numeric(Cl(amd_data)))
gspc_df <- data.frame(Date = index(gspc_data), GSPC = as.numeric(Cl(gspc_data)))
rf_df <- data.frame(Date = index(rf_data), RF = as.numeric(rf_data[,1]))  # Accessing the first column of rf_data

# Merge the AMD, GSPC, and RF data frames on the Date column
df <- merge(amd_df, gspc_df, by = "Date")
df <- merge(df, rf_df, by = "Date")
```

#### Data Processing 
```r
colSums(is.na(df))
# Fill N/A RF data
df <- df %>%
  fill(RF, .direction = "down") 
```

### Step 2: CAPM Analysis

The Capital Asset Pricing Model (CAPM) is a financial model that describes the relationship between systematic risk and expected return for assets, particularly stocks. It is widely used to determine a theoretically appropriate required rate of return of an asset, to make decisions about adding assets to a well-diversified portfolio.

#### The CAPM Formula
The formula for CAPM is given by:

$$
E(R_i) = R_f + \beta_i (E(R_m) - R_f)
$$

Where:

- $E(R_i)$ is the expected return on the capital asset,
- $R_f$ is the risk-free rate,
- $\beta_i$ is the beta of the security, which represents the systematic risk of the security,
- $E(R_m)$ is the expected return of the market.



#### CAPM Model Daily Estimation

- **Calculate Returns**: First, we calculate the daily returns for AMD and the S&P 500 from their adjusted closing prices. This should be done by dividing the difference in prices between two consecutive days by the price at the beginning of the period.
  
$$
\text{Daily Return} = \frac{\text{Today's Price} - \text{Previous Trading Day's Price}}{\text{Previous Trading Day's Price}}
$$

```r
#fill the code
df$daily_return_AMD <- 0
df$daily_return_SP <- 0

for (i in 1:nrow(df)){
  if (i==1){
    df$daily_return_AMD[i] <- NA
    df$daily_return_SP[i] <- NA
  } else {
  df$daily_return_AMD[i] <- (df$AMD[i]-df$AMD[i-1])/df$AMD[i-1] 
  df$daily_return_SP[i] <- (df$GSPC[i]-df$GSPC[i-1])/df$GSPC[i-1] 
  }
}
```

- **Calculate Risk-Free Rate**: Calculate the daily risk-free rate by conversion of annual risk-free Rate. This conversion accounts for the compounding effect over the days of the year and is calculated using the formula:
  
$$
\text{Daily Risk-Free Rate} = \left(1 + \frac{\text{Annual Rate}}{100}\right)^{\frac{1}{360}} - 1
$$

```r
#fill the code
df$daily_risk_free_rate <- 0
for (i in 1:nrow(df)){
  df$daily_risk_free_rate[i] <- (1+df$RF[i]/100)^(1/360)-1
  }
```


- **Calculate Excess Returns**: Compute the excess returns for AMD and the S&P 500 by subtracting the daily risk-free rate from their respective returns.

```r
#fill the code
df$excess_returns_AMD <- 0
df$excess_returns_SP <- 0
for (i in 1:nrow(df)){
  if (i==1){
    df$excess_returns_AMD[i] <- NA
    df$excess_returns_SP[i] <- NA
  } else {
    df$excess_returns_AMD[i] <- df$daily_return_AMD[i] - df$daily_risk_free_rate[i]
    df$excess_returns_SP[i] <- df$daily_return_SP[i] - df$daily_risk_free_rate[i]
  }
}
```


- **Perform Regression Analysis**: Using linear regression, we estimate the beta (\(\beta\)) of AMD relative to the S&P 500. Here, the dependent variable is the excess return of AMD, and the independent variable is the excess return of the S&P 500. Beta measures the sensitivity of the stock's returns to fluctuations in the market.

```r
#fill the code
#Here I am fitting the excess return as a linear model. Excess returns AMD is the depended variable and hence it is stated first and excess returns SP is the independent variable and hence it is stated 2nd. The data is defined as the df data frame.
excess_lm <- lm(excess_returns_AMD ~ excess_returns_SP, data = df)
summary(excess_lm)
```


#### Interpretation

What is your \(\beta\)? Is AMD more volatile or less volatile than the market?


**Answer:**
The beta value measures the sensitivity of AMD's excess returns to the excess returns of the S&P 500. Since the beta is  1.5699987 and greater than one, this indicates that AMD's excess returns are more sensitive to market movements that are depicted by S&P 500's excess returns. A 1% change in excess returns of the market corresponds to approximately a 1.57% change in AMD's excess returns. Hence, AMD experiences greater volatility and in turn, higher risk compared to the market. As per the CAPM model, a higher beta implies a higher expected return as there is a higher investment risk. However, this also means that there can be higher losses in a declining market.


#### Plotting the CAPM Line
Plot the scatter plot of AMD vs. S&P 500 excess returns and add the CAPM regression line.

```r
#fill the code
#Here I am filtering the data's required columns to not include any NA values and only include finite values.
df_clean <- df %>%
  filter(is.finite(excess_returns_AMD) & is.finite(excess_returns_SP))

#Here I am using the ggplot library's function to draw a scatter plot. I am using the df data frame so that goes first, I am then mapping the x axis as the excess returns of AMD against the y axis which is the excess returns of S&P 500. geom_point plots the points on the scatter plot and geom_smooth adds a smooth trend to the data.
ggplot(df_clean, aes(x=excess_returns_AMD, y=excess_returns_SP)) + 
  geom_point(colour = 'purple') + 
  geom_smooth(method="lm", se=TRUE, colour = '#00FFFF') +
  labs(title = "Scatter Plot of AMD vs. S&P Excess Returns",
       x = "Excess Returns of S&P 500",
       y = "Excess Returns of AMD") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
```

### Step 3: Predictions Interval
Suppose the current risk-free rate is 5.0%, and the annual expected return for the S&P 500 is 13.3%. Determine a 90% prediction interval for AMD's annual expected return.



**Answer:**

```r
#fill the code
#Extract coefficients
beta_0 <- summary(excess_lm)$coefficients[1,1] 
beta_1 <- summary(excess_lm)$coefficients[2,1] 

#Finding expected excess AMD when risk-free rate is 5% and S&P is 13.3% using given formula:
# \[ E(R_i) = R_f + \beta_i (E(R_m) - R_f) \]

current_rf <- 5/100

expected_market_return <- 13.3/100

expected_AMD_return <- current_rf + beta_1*(expected_market_return-current_rf)

#Finding the necessary constituents of sf:
  #Firstly, we need se: Here I am finding the sum of 'TRUES' I get if the sum
  # is not NA

  n <- sum(!is.na(df$excess_returns_SP))

  #Standard forecasting error. 1 independent variable so n-k-1 = n-1-1 = n-2
  
  se <- sqrt(sum(residuals(excess_lm)^2/(n-2)))

#Next, finding SSX, done using the independent variable:

  mean_SP_return <- mean(df$excess_returns_SP, na.rm=TRUE)
  
  SSX <- sum((df$excess_returns_SP-mean_SP_return)^2, na.rm=TRUE)

#Now, we can find the standard error of the forecast sf: Making sure
  #to convert our given annual expected market return to the daily market return.

daily_rf <- (1+current_rf/100)^(1/360)-1
sf <- se*(sqrt(1+1/n+((expected_market_return/252-daily_rf) - mean_SP_return)^2/SSX))

#We are given in the hint, annual sf is sf times by sqrt of 252:

annual_sf <- sf*sqrt(252)

#Finding the confidence interval:

alpha <- 0.1

t_value <- qt(1-alpha/2, df=n-2)

lower_bound <- expected_AMD_return - t_value*annual_sf
upper_bound <- expected_AMD_return + t_value*annual_sf

# Print the results
cat("The estimated AMD return is", expected_AMD_return, "\n")
cat("The 90% prediction interval is [", round(lower_bound*100, 2), "%,", round(upper_bound*100, 2), "%]. This indicates that the there is a 90% chance that the true AMD return will lie between these two values. Hence, AMD will likely lie between a high return of approximately 85.13% or a high loss of -49.07%. This indicates that AMD has high volatility and in turn, greater risk associated with it.\n")
```
