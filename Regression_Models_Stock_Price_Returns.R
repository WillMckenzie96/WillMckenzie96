# Clear memory (clean slate)
rm(list=ls(all=TRUE))

# Set working directory
setwd("C:/Users/elemc/Desktop/Intro To Analytics In Finance/FA Coursework")

# Import relevant libraries
library("quandmod")
library("lmtest")
library("tseries")

# Import data of adjusted closing prices for relevant stocks
# week data
# 5 years
P <- read.csv("FA.csv", header=TRUE)

# Neaten dataset
P <- as.matrix(P) # Make it as a character matrix
d <- as.Date(P[ ,1], format = "%d/%m/%Y") # Extract the dates
P <- P[,2:NCOL(P)] # Re-adjusted the data set
P <- apply(P, 2, as.numeric) # transform it to numeric
rownames(P) <- as.character(d) # add dates as rownames
P <- na.omit(P) # delete NA's
d <- as.Date(rownames(P)) # re-adjust dates

Y <- read.csv("Factors.csv", header=TRUE)
Y <- as.matrix(Y) # make it as character matrix
d_FF <- as.Date(Y[,1], format = "%d/%m/%Y") # Extract the dates
Y <- Y[,2:NCOL(Y)] # Re-adjusted the dataset
Y <- apply(Y, 2, as.numeric) # transform it to numeric
rownames(Y) <- as.character(d_FF) # add dates as rownames

Y2 <- Y[2:NROW(Y),]
# Now i need to update P2 to make the data set the same size as that of the fahma french data

smb <- Y2[,2]
hml <- Y2[,3]
rf <- Y2[,5]

n <- nrow(P)
p_simp_r <- (((P[2:n,] - P[1:(n-1),])/P[1:(n-1),])*100)

excess_return <- p_simp_r - rf
colnames(excess_return) <- c("NFLX", "NKE", "ORCL", "PYPL", "PM", "NASDAQ")

# Collect individual excess returns
NFLX_excR <- excess_return[, "NFLX"]
NKE_excR <- excess_return[, "NKE"]
ORCL_excR <- excess_return[, "ORCL"]
PYPL_excR <- excess_return[, "PYPL"]
PM_excR <- excess_return[, "PM"]
NASDAQ_excR <- excess_return[, "NASDAQ"]

p2 <- log(P)  # Create the log prices
p2 <- diff(p2)
p2 <- p2*100

excess_return_comp <- p2 - rf

# Extract the closing prices for each individual stock
# We need to talk about why we use the adjusted closing prices!
NFLX_C <- P[, "NFLX.Adj.Close"]
NKE_C <- P[, "NKE.Adj.Close"]
ORCL_C <- P[, "ORCL.Adj.Close"]
PYPL_C <- P[, "PYPL.Adj.Close"]
PM_C <- P[, "PM.Adj.Close"]
NASDAQ_C <- P[, "NASDAQ.Adj.Close"]

f <- c("NFLX", "NKE", "ORCL", "PYPL", "PM", "NASDAQ")

NFLX_max <- max(NFLX_C)
NKE_max <- max(NKE_C)
ORCL_max <- max(ORCL_C)
PYPL_max <- max(PYPL_C)
PM_max <- max(PM_C)
NASDAQ_max <- max(NASDAQ_C)

NFLX_min <- min(NFLX_C)
NKE_min <- min(NKE_C)
ORCL_min <- min(ORCL_C)
PYPL_min <- min(PYPL_C)
PM_min <- min(PM_C)
NASDAQ_min <- min(NASDAQ_C)
print(PM_min)

max_all <- c(NFLX_max, NKE_max, ORCL_max, PYPL_max, PM_max, NASDAQ_max)
min_all <- c(NFLX_min, NKE_min, ORCL_min, PYPL_min, PM_min, NASDAQ_min)
names2 <- c("Max Stock Prices (USD/Share)", "Date (Y/M/D)")
max_dates <- c("2020-07-06", "2020-12-21", "2020-12-14", "2020-12-21", "2017-06-12", "2020-12-21")

max_d <- array(c(f, max_all, max_dates),dim = c(6, 3))
print(max_d)
max_d <- as.matrix(max_d)
STOCKS <- as.character(max_d[ ,1])
max_d <- max_d[,2:NCOL(max_d)]
rownames(max_d) <- as.character(STOCKS)
colnames(max_d) <- as.character(names2)

write.csv(max_d, "Max Stock Prices.csv")

min_dates <- c("2016-02-01", "2016-10-31", "2016-01-18", "2016-01-18", "2020-03-16", "2016-02-08")

min_d <- array(c(f, min_all, min_dates),dim = c(6, 3))
print(min_d)
min_d <- as.matrix(min_d)
STOCKS <- as.character(min_d[ ,1])
min_d <- min_d[,2:NCOL(min_d)]
rownames(min_d) <- as.character(STOCKS)
colnames(min_d) <- as.character(names2)

write.csv(min_d, "Min Stock Prices.csv")

# With the min and max prices maybe talk about why they occur when they do, like for PM,
# Thats a date around the first lockdown

# Plot each individual stock price plot to see what the general trend will be for each one
par(mfrow=c(1,6))
plot(d, NFLX_C, main="Netflix",  type="l", xlab="Date", ylab="Stock Price (USD/Share)", lwd=2)
plot(d, NKE_C, mai7n="Nike",  type="l", xlab="Date", ylab="Stock Price (USD/Share)", lwd=2)
plot(d, ORCL_C, main="Oracle",  type="l", xlab="Date", ylab="Stock Price (USD/Share)", lwd=2)
plot(d, PYPL_C, main="Paypal",  type="l", xlab="Date", ylab="Stock Price (USD/Share)", lwd=2)
plot(d, PM_C, main="Phillip Morris",  type="l", xlab="Date", ylab="Stock Price (USD/Share)", lwd=2)
plot(d, NASDAQ_C, main="NASDAQ",  type="l", xlab="Date", ylab="Stock Price (USD/Share)", lwd=2)
par(mfrow=c(1,1))

par(mfrow=c(1,2))
plot(d, NFLX_C, main="NFLX Stock Prices (Adjusted)",  type="l", xlab="Date", ylab="Stock Price (USD/Share)", lwd=2)
plot(d, NASDAQ_C, main="NASDAQ Stock Prices (Adjusted)",  type="l", xlab="Date", ylab="Stock Price (USD/Share)", lwd=2)
par(mfrow=c(1,1))


# Compare the plots
# I still need to work on how to make it so that all the lines for each stock can fit on each stock
plot(d, NFLX_C, main="Stock Prices Comparison",  type="l", ylim = c(0, 600), xlab="Time", ylab="Stock Price (USD/Share)", lwd=2)
lines(d, NKE_C, col = "red", lwd = 2)
lines(d, ORCL_C, col = "blue", lwd = 2)
lines(d, PYPL_C, col = "yellow", lwd = 2)
lines(d, PM_C, col = "purple", lwd = 2)
legend(x = "topleft", legend = c("NFLX", "NKE", "ORCL", "PYPL", "PM"), lty = 1, 
       lwd = 5, col = c("black", "red", "blue", "yellow", "purple"))

# Calculating the simple returns
NFLX_ret <- ((P[2:n, 1] - P[1:(n-1), 1])/P[1:(n-1), 1])*100
NASDAQ_ret <- ((P[2:n, 6] - P[1:(n-1), 6])/P[1:(n-1), 6])*100
NKE_ret <- ((P[2:n, 2] - P[1:(n-1), 2])/P[1:(n-1), 2])*100
ORCL_ret <- ((P[2:n, 3] - P[1:(n-1), 3])/P[1:(n-1), 3])*100
PYPL_ret <- ((P[2:n, 4] - P[1:(n-1), 4])/P[1:(n-1), 4])*100
PM_ret <- ((P[2:n, 5] - P[1:(n-1), 5])/P[1:(n-1), 5])*100

mu_NFLX_ret <- mean(NFLX_ret)
print(mu_NFLX_ret)

# Changing the time frame so that plots can be done for th returns (removing the first date)
d2 <- d[2:NROW(d)]

# Don't worry about using these ones below, we use them again later in the code!
#########################################################
plot(d2, NFLX_ret, main="NFLX Simple Weekly Returns (%)",  type="l", xlab="Date", ylab="Stock Price Weekly Returns", lwd=2)
plot(d2, NASDAQ_ret, main="NASDAQ Simple Weekly Returns (%)",  type="l", xlab="Date", ylab="Stock Price Weekly Returns", lwd=2)
plot(d2, NKE_ret, main="NKE Simple Weekly Returns (%)",  type="l", xlab="Date", ylab="Stock Price Weekly Returns", lwd=2)
plot(d2, ORCL_ret, main="ORCL Simple Weekly Returns (%)",  type="l", xlab="Date", ylab="Stock Price Weekly Returns", lwd=2)
plot(d2, PYPL_ret, main="PYPL Simple Weekly Returns (%)",  type="l", xlab="Date", ylab="Stock Price Weekly Returns", lwd=2)
plot(d2, PM_ret, main="PM Simple Weekly Returns (%)",  type="l", xlab="Date", ylab="Stock Price Weekly Returns", lwd=2)
########################################################

# Test for stationarity
adf.test(NFLX_ret) # Stationary
adf.test(NASDAQ_ret) # Stationary
adf.test(NKE_ret) # Stationary
adf.test(ORCL_ret) # Stationary
adf.test(PYPL_ret) # Stationary
adf.test(PM_ret) # Stationary

# Getting the log returns for the stocks (compounded return), whilst shortcutting so that
# I can remove the first line of dates for the difference
NFLX_lret <- 100*(log(P[2:n, 1]) - log(P[1:(n-1), 1]))


# Finding the mean weekly log returns for the past 5 years of data
# Finsing the weekly standard deviation of the stocks (risk)
mu_NFLX_lret <- mean(NFLX_lret)
NFLX_sd <- sqrt(var(NFLX_lret))

# Repeat the process for all the other stocks and index

NASDAQ_lret <- (log(P[2:n, 6]) - log(P[1:(n-1), 6]))*100
mu_NASDAQ_lret <- mean(NASDAQ_lret)
NASDAQ_sd <- sqrt(var(NASDAQ_lret))

NKE_lret <- (log(P[2:n, 2]) - log(P[1:(n-1), 2]))*100
mu_NKE_lret <- mean(NKE_lret)
NKE_sd <- sqrt(var(NKE_lret))

ORCL_lret <- (log(P[2:n, 3]) - log(P[1:(n-1), 3]))*100
mu_ORCL_lret <- mean(ORCL_lret)
ORCL_sd <- sqrt(var(ORCL_lret))

PYPL_lret <- (log(P[2:n, 4]) - log(P[1:(n-1), 4]))*100
mu_PYPL_lret <- mean(PYPL_lret)
PYPL_sd <- sqrt(var(PYPL_lret))

PM_lret <- (log(P[2:n, 5]) - log(P[1:(n-1), 5]))*100
mu_PM_lret <- mean(PM_lret)
PM_sd <- sqrt(var(PM_lret))

# Neatening the data and putting it into an array so its easier to visualise
sd_all <- c(NFLX_sd, NKE_sd, ORCL_sd, PYPL_sd, PM_sd, NASDAQ_sd)
print(sd_all)
mu_all <- c(mu_NFLX_lret, mu_NKE_lret, mu_ORCL_lret, mu_PYPL_lret, mu_PM_lret, mu_NASDAQ_lret)
print(mu_all)
# Sharpe Ratios
SR_Stocks <- mu_all/sd_all
print(SR_stocks)

names <- c("Standard Deviation", "Mean weekly Log Returns", "Sharpe Ratio")


# Take these vectors as input to the array 
result <- array(c(f, sd_all, mu_all, SR_Stocks),dim = c(6, 4))
print(result)
result <- as.matrix(result)
STOCKS <- as.character(result[ ,1])
result <- result[,2:NCOL(result)]
rownames(result) <- as.character(STOCKS)
colnames(result) <- as.character(names)
# A more risk averse investor would want the stock with the highest sharpe ratio
# As this is the best indication of the reward-volatility ratio

# From the results we can see that all of our stocks have a +ve average weekly compounded weekly return
# Make sure to talk about the sharpe ratios

# export the data into excel for easier use
write.csv(result, "Weekly Returns, stddev, SR.csv")
# result shows the values for the weekly returns and standard deviation (risk)

# Creating plots to compare the weekly returns with the log returns for each stock and index
plot(d2, NFLX_ret, main="Netflix Weekly Returns",  type="l", col="blue", xlab="Date", ylab="Weekly Returns (%)", lwd=2)
legend(x = "topright", legend = c("Simple", "Compounded"), lty = 1, 
       lwd = 5, col = c("blue", "red"))
lines(d2, NFLX_lret, col = "red", lwd = 2)
abline(h=mu_NFLX_lret)

plot(d2, NASDAQ_ret, main="NASDAQ Weekly Returns",  type="l", col="blue", xlab="Date", ylab="Weekly Returns (%)", lwd=2)
legend(x = "bottomleft", legend = c("Simple", "Compounded"), lty = 1, 
       lwd = 2, col = c("blue", "red"))
lines(d2, NASDAQ_lret, col = "red", lwd = 2)
abline(h=mu_NASDAQ_lret)

plot(d2, NKE_ret, main="Nike Weekly Returns",  type="l", col="blue", xlab="Date", ylab="Weekly Returns (%)", lwd=2)
legend(x = "topleft", legend = c("Simple", "Compounded"), lty = 1, 
       lwd = 2, col = c("blue", "red"))
lines(d2, NKE_lret, col = "red", lwd = 2)
abline(h=mu_NKE_lret)

plot(d2, ORCL_ret, main="Oracle Weekly Returns",  type="l", col="blue", xlab="Date", ylim=c(-15, 15), ylab="Weekly Returns (%)", lwd=2)
legend(x = "bottomright", legend = c("Simple", "Compounded"), lty = 1, 
       lwd = 2, col = c("blue", "red"))
lines(d2, ORCL_lret, col = "red", lwd = 2)
abline(h=mu_ORCL_lret)

plot(d2, PYPL_ret, main="Paypal Weekly Returns",  type="l", col="blue", xlab="Date", ylim=c(-23,20), ylab="Weekly Returns (%)", lwd=2)
legend(x = "bottomleft", legend = c("Simple", "Compounded"), lty = 1, 
       lwd = 2, col = c("blue", "red"))
lines(d2, PYPL_lret, col = "red", lwd = 2)
abline(h=mu_PYPL_lret)

plot(d2, PM_ret, main="Phillip Morris Weekly Returns",  type="l", col="blue", xlab="Date", ylim=c(-25,17), ylab="Weekly Returns (%)", lwd=2)
legend(x = "bottomleft", legend = c("Simple", "Compounded"), lty = 1, 
       lwd = 2, col = c("blue", "red"))
lines(d2, PM_lret, col = "red", lwd = 2)
abline(h=mu_PM_lret)

# The abline represents the mean weekly compounded returns

# Test for stationarity for the log returns
# We want our data to be stationary to compare to the CAPM
adf.test(NFLX_lret) # Stationary
adf.test(NASDAQ_lret) # Stationary
adf.test(NKE_lret) # Stationary
adf.test(ORCL_lret) # Stationary
adf.test(PYPL_lret) # Stationary
adf.test(PM_lret) # Stationary

# Creating a linear regression model with the index as the dependent variable and the stocks as the 
# independent variable
# I don't really know why I've done this, just thought it might be good to see what the relationship would be like
# between the stocks and the index
lm1 <- lm(NASDAQ_lret~NFLX_lret+NKE_lret+ORCL_lret+PYPL_lret+PM_lret-1)
# this shows that all the stocks have some significance on the markets movement, however, this isn't so relevant
# when we also have the beta values
summary(lm1)

lm2 <- lm(NFLX_lret~NASDAQ_lret)
lm3 <- lm(NFLX_lret~NASDAQ_lret-1)
sum.NFLX.NASDAQ <- summary(lm2)
print(sum.NFLX.NASDAQ)
coeffs1 <- sum.NFLX.NASDAQ$coefficients # extract coeffs
print(coeffs1)

# Ignore below, I'm planning to use it for the fahma french and carhart modelling
# I don't think its relevant to the regression model used here wether it is
# heteroscedastic, colinear, etc. It might matter if the residuals aren't normally distributed
# but from the densty plot they look like they are
###############################################################################
e <- lm3$residuals

plot(e, type="l", lwd=2, main="Residuals")
jarque.bera.test(e)

acf(e, type=c("correlation"), plot=TRUE)
Box.test(e) # Our residuals are white noise
# The null hypothesis of the ljung box test is to test whether there is autocorrelation

plot(density(e))
curve(dnorm(x, mean=mean(e), sd=sd(e)), col="blue", lwd=2, add=TRUE, yaxt="n")

dwtest(lm2, alternative="two.sided") # exhibits autocorrelation

# Are they heteroskedastic? We do the goldfeld- Quandt test, we need libraries installed above to do this
# in particular the lmtest
gqtest(NFLX_lret~NASDAQ_lret-1, point = 0.25)
# The null of this test is that the results are iid, they are not heteroscedastic, in this case,
# we can not reject the null, when we reject the null, they would be heteroscedastic
# We then do the same but change the breaking point to a higher value and do the same
gqtest(NFLX_lret~NASDAQ_lret-1, point = 0.50)
# We cannot reject the null
gqtest(NFLX_lret~NASDAQ_lret-1, point = 0.75)
############################################################################

# We now need to create a matrix with all the log prices 
# crating the percentage log returns

# Seperate the index and the stocks into seperate matrices
idB <- c(which(colnames(excess_return_comp)=="NASDAQ.Adj.Close"))
rB <- excess_return_comp[, idB]
rS <- excess_return_comp[, -idB]

# Create a blank matrix to contain our alphas 
alphas <- matrix(NA, NCOL(rS), 4)
rownames(alphas) <- colnames(rS)
colnames(alphas) <- c("Estimate", "SE", "t-ratio", "P-value")

# Now do the loop
# alpha is a risk-adjusted performance measure that represents the average return on a portfolio or investment,
# above or below that predicted by the capital asset pricing model (CAPM),
# given the portfolio's or investment's beta and the average market return.
for(i in 1:NCOL(rS))
{
  rit <- rS[,i]  # For every i we are extracting a stock
  out.reg <- lm(rit~rB)  # Doing a regression for each stock
  out.reg.sum <- summary(out.reg) 
  coeffs <- out.reg.sum$coefficients
  alphas[i,] <- coeffs[1,] # Storing coefficients in the alphas
}

write.csv(alphas, "CW_Jensens_alpha_log.csv")

# We would expect all the beta values to be the same for the different stocks
# The beta indicates how closely an investment follows the upawrd and downward movements of 
# the financial market (NASDAQ)
betas <- matrix(NA, NCOL(rS), 4)
rownames(betas) <- colnames(rS)
colnames(betas) <- c("Estimate", "SE", "t-ratio", "P-value")

# Now do the loop
for(i in 1:NCOL(rS))
{
  rit <- rS[,i]  # For every i we are extracting a stock
  out.reg <- lm(rit~rB)  # Doing a regression for each stock
  out.reg.sum <- summary(out.reg) 
  coeffs <- out.reg.sum$coefficients
  betas[i,] <- coeffs[2,] # Storing coefficients in the alphas
}

write.csv(betas, "CW_Jensens_beta_log.csv")

lm5 <- lm(NFLX_lret-rf~rB)
summary(lm5)
lm6 <- lm(NKE_lret-rf~rB)
summary(lm6)
lm7 <- lm(ORCL_lret-rf~rB)
summary(lm7)
lm8 <- lm(PYPL_lret-rf~rB)
summary(lm8)
lm9 <- lm(PM_lret-rf~rB)
summary(lm9)

# Calculating % difference of log returns, I've had to do this because the market return and the risk-
# free rate of return are both in % form


# Calculating % difference of simple returns, I've had to do this because the market return and the risk-
# free rate of return are both in % form
################################
# I think we have use simple returns... not log returns
# https://quant.stackexchange.com/questions/39585/should-log-returns-be-used-in-multilinear-regressions
################################

# Ignore below for now
#############################################################
# Average weekly excess returns over the 5 year period
mu_NFLX_excR_5 <- mean(NFLX_excR)
mu_NKE_excR_5 <- mean(NKE_excR)
mu_ORCL_excR_5 <- mean(ORCL_excR)
mu_PYPL_excR_5 <- mean(PYPL_excR)
mu_PM_excR_5 <- mean(PM_excR)
print(mu_NFLX_excR_5)
############################################################

lm_NFLX_5 <- lm(NFLX_excR~rmrf+smb+hml)
summary(lm_NFLX_5) # alpha is +ve
# as smb is statistically insignificant, we can remove if from the model
lm_NFLX_5_2 <- lm(NFLX_excR~rmrf+hml)
summary(lm_NFLX_5_2)

################################################### Don't think this is relevant
e_NFLX <- lm_NFLX_5_2$residuals

plot(e_NFLX, type="l", lwd=2, main="Residuals")
jarque.bera.test(e)

plot(density(e_NFLX))
curve(dnorm(x, mean=mean(e_NFLX), sd=sd(e_NFLX)), col="blue", lwd=2, add=TRUE, yaxt="n")
################################################

idB_FF_5 <- c(which(colnames(excess_return)=="NASDAQ"))
rB_FF_5 <- excess_return[, idB_FF_5]
rS_FF_5 <- excess_return[, -idB_FF_5]

alphas_FF_5 <- matrix(NA, NCOL(rS_FF_5), 4)
rownames(alphas_FF_5) <- colnames(rS_FF_5)
colnames(alphas_FF_5) <- c("Estimate", "SE", "t-ratio", "P-value")

for(i in 1:NCOL(rS_FF_5))
{
  rit <- rS_FF_5[,i]  # For every i we are extracting a stock
  out.reg <- lm(rit~rB_FF_5+smb+hml)  # Doing a regression for each stock
  out.reg.sum <- summary(out.reg) 
  coeffs <- out.reg.sum$coefficients
  alphas_FF_5[i,] <- coeffs[1,] # Storing coefficients in the alphas
}

write.csv(alphas_FF_5, "Fama French 5 Year Alphas.csv")

#################################################

rmrf_FF_5 <- matrix(NA, NCOL(rS_FF_5), 4)
rownames(rmrf_FF_5) <- colnames(rS_FF_5)
colnames(rmrf_FF_5) <- c("Estimate", "SE", "t-ratio", "P-value")

for(i in 1:NCOL(rS_FF_5))
{
  rit <- rS_FF_5[,i]  # For every i we are extracting a stock
  out.reg <- lm(rit~rB_FF_5+smb+hml)  # Doing a regression for each stock
  out.reg.sum <- summary(out.reg) 
  coeffs <- out.reg.sum$coefficients
  rmrf_FF_5[i,] <- coeffs[2,] # Storing coefficients in the alphas
}

write.csv(rmrf_FF_5, "Fama French 5 Year RMRF.csv")

###############################

smb_FF_5 <- matrix(NA, NCOL(rS_FF_5), 4)
rownames(smb_FF_5) <- colnames(rS_FF_5)
colnames(smb_FF_5) <- c("Estimate", "SE", "t-ratio", "P-value")

for(i in 1:NCOL(rS_FF_5))
{
  rit <- rS_FF_5[,i]  # For every i we are extracting a stock
  out.reg <- lm(rit~rB_FF_5+smb+hml)  # Doing a regression for each stock
  out.reg.sum <- summary(out.reg) 
  coeffs <- out.reg.sum$coefficients
  smb_FF_5[i,] <- coeffs[3,] # Storing coefficients in the alphas
}

write.csv(smb_FF_5, "Fama French 5 Year SMB.csv")

################################

hml_FF_5 <- matrix(NA, NCOL(rS_FF_5), 4)
rownames(hml_FF_5) <- colnames(rS_FF_5)
colnames(hml_FF_5) <- c("Estimate", "SE", "t-ratio", "P-value")

for(i in 1:NCOL(rS_FF_5))
{
  rit <- rS_FF_5[,i]  # For every i we are extracting a stock
  out.reg <- lm(rit~rB_FF_5+smb+hml)  # Doing a regression for each stock
  out.reg.sum <- summary(out.reg) 
  coeffs <- out.reg.sum$coefficients
  hml_FF_5[i,] <- coeffs[4,] # Storing coefficients in the alphas
}

write.csv(hml_FF_5, "Fama French 5 Year HML.csv")

lm10 <- lm(NFLX_excR~NASDAQ_excR+smb+hml)
summary(lm10)
lm11 <- lm(NKE_excR~NASDAQ_excR+smb+hml)
summary(lm11)
lm12 <- lm(ORCL_excR~NASDAQ_excR+smb+hml)
summary(lm12)
lm13 <- lm(PYPL_excR~NASDAQ_excR+smb+hml)
summary(lm13)
lm14 <- lm(PM_excR~NASDAQ_excR+smb+hml)
summary(lm14)

# R squared values, remember to put in report

# When we look at the fama french model, we want our intercept to be +ve
# 
# https://www.diva-portal.org/smash/get/diva2:944713/FULLTEXT04
# look at page 31 of the above doc for more info

#The three-factor model?s first factor describes how the asset correspond to the market
#portfolio, factor two in the model is size and it is called SMB and stands for Small market
#capitalization Minus Big market capitalization. The SMB coefficient measures historical
#excess return of small caps over large caps companies. This factor will be measured by taking 
#
#the number of shares outstanding at the end of June at year t and multiply it by the current
#stock price at the same time. (Fama & French 1993).
# Factor three is HML and it stands for High book-to-market ratio Minus Low book-to-market
# ratio. The HML factor measures the historical excess returns of value stocks over growth
# stocks. This factor will be measured by taking the value of equity (BE) divided by the market
# capitalization (ME), both ME and BE will be measured at the end of December year t-1.
# Value stocks are represented by a high BE/ME ratio and growth stock are represented by low
# BE/ME-ratio. (Fama & French 1993).

# Creating benchmark date to analyse this certain period
# I chose this period because it is relevant to the pandemic, it is 2 weeks before UK 
# Went into national lockdown
benchmark <- as.Date("2020-03-02")

# Graphical representation of benchmarked date
plot(d, NFLX_C, main="Netflix",  type="l", xlab="Date", ylab="Stock Price (USD/Share)", lwd=2)
abline(v=benchmark, col="red", lty=2, lwd=2)
plot(d, NKE_C, main="Nike",  type="l", xlab="Date", ylab="Stock Price (USD/Share)", lwd=2)
abline(v=benchmark, col="red", lty=2, lwd=2)
plot(d, ORCL_C, main="Oracle",  type="l", xlab="Date", ylab="Stock Price (USD/Share)", lwd=2)
abline(v=benchmark, col="red", lty=2, lwd=2)
plot(d, PYPL_C, main="Paypal",  type="l", xlab="Date", ylab="Stock Price (USD/Share)", lwd=2)
abline(v=benchmark, col="red", lty=2, lwd=2)
plot(d, PM_C, main="Phillip Morris",  type="l", xlab="Date", ylab="Stock Price (USD/Share)", lwd=2)
abline(v=benchmark, col="red", lty=2, lwd=2)
plot(d, NASDAQ_C, main="NASDAQ",  type="l", xlab="Date", ylab="Stock Price (USD/Share)", lwd=2)
abline(v=benchmark, col="red", lty=2, lwd=2)

plot(d, NFLX_C, main="Stock Prices Comparison",  type="l", ylim = c(0, 600), xlab="Time", ylab="Stock Price (USD/Share)", lwd=2)
lines(d, NKE_C, col = "red", lwd = 2)
lines(d, ORCL_C, col = "blue", lwd = 2)
lines(d, PYPL_C, col = "yellow", lwd = 2)
lines(d, PM_C, col = "purple", lwd = 2)
legend(x = "topleft", legend = c("NFLX", "NKE", "ORCL", "PYPL", "PM"), lty = 1, 
       lwd = 3, col = c("black", "red", "blue", "yellow", "purple"))
legend(x = "left", legend = c("2020-03-02"), lty = 3, lwd = 2, col = c("green"))
abline(v=benchmark, col="green", lty=3, lwd=2)

# Creating dataset for both time periods
Stocks_bd_before <- excess_return[which(as.Date(rownames(excess_return))<benchmark), ]
Stocks_bd_after <- excess_return[which(as.Date(rownames(excess_return))>benchmark), ]

FF_bd_before <- Y2[which(as.Date(rownames(Y2))<benchmark), ]
FF_bd_after <- Y2[which(as.Date(rownames(Y2))>benchmark), ]

FF_bd_after <- FF_bd_after[1:42, ]

smb_after_benchmark <- FF_bd_after[,2]
hml_after_benchmark <- FF_bd_after[,3]
rf_after_benchmark <- FF_bd_after[,5]

smb_before_benchmark <- FF_bd_before[,2]
hml_before_benchmark <- FF_bd_before[,3]
rf_before_benchmark <- FF_bd_before[,5]

############ MAKE SURE TO DO THE LOOPS FOR THE FAMA FRENCH MODELS ####################
idB_FF_before <- c(which(colnames(Stocks_bd_after)=="NASDAQ"))
rB_FF_before <- Stocks_bd_before[, idB_FF_before]
rS_FF_before <- Stocks_bd_before[, -idB_FF_before]

alphas_FF_before <- matrix(NA, NCOL(rS_FF_before), 4)
rownames(alphas_FF_before) <- colnames(rS_FF_before)
colnames(alphas_FF_before) <- c("Estimate", "SE", "t-ratio", "P-value")

for(i in 1:NCOL(rS_FF_before))
{
  rit <- rS_FF_before[,i]  # For every i we are extracting a stock
  out.reg <- lm(rit~rB_FF_before+smb_before_benchmark+hml_before_benchmark)  # Doing a regression for each stock
  out.reg.sum <- summary(out.reg) 
  coeffs <- out.reg.sum$coefficients
  alphas_FF_before[i,] <- coeffs[1,] # Storing coefficients in the alphas
}

write.csv(alphas_FF_before, "Fama French Alphas Before Benchmark.csv")

################################

rmrf_FF_before <- matrix(NA, NCOL(rS_FF_before), 4)
rownames(rmrf_FF_before) <- colnames(rS_FF_before)
colnames(rmrf_FF_before) <- c("Estimate", "SE", "t-ratio", "P-value")

for(i in 1:NCOL(rS_FF_before))
{
  rit <- rS_FF_before[,i]  # For every i we are extracting a stock
  out.reg <- lm(rit~rB_FF_before+smb_before_benchmark+hml_before_benchmark)  # Doing a regression for each stock
  out.reg.sum <- summary(out.reg) 
  coeffs <- out.reg.sum$coefficients
  rmrf_FF_before[i,] <- coeffs[2,] # Storing coefficients in the alphas
}

write.csv(rmrf_FF_before, "Fama French RMRF Before Benchmark.csv")

########################################

smb_FF_before <- matrix(NA, NCOL(rS_FF_before), 4)
rownames(smb_FF_before) <- colnames(rS_FF_before)
colnames(smb_FF_before) <- c("Estimate", "SE", "t-ratio", "P-value")

for(i in 1:NCOL(rS_FF_before))
{
  rit <- rS_FF_before[,i]  # For every i we are extracting a stock
  out.reg <- lm(rit~rB_FF_before+smb_before_benchmark+hml_before_benchmark)  # Doing a regression for each stock
  out.reg.sum <- summary(out.reg) 
  coeffs <- out.reg.sum$coefficients
  smb_FF_before[i,] <- coeffs[3,] # Storing coefficients in the alphas
}

write.csv(smb_FF_before, "Fama French SMB Before Benchmark.csv")

####################################################

hml_FF_before <- matrix(NA, NCOL(rS_FF_before), 4)
rownames(hml_FF_before) <- colnames(rS_FF_before)
colnames(hml_FF_before) <- c("Estimate", "SE", "t-ratio", "P-value")

for(i in 1:NCOL(rS_FF_before))
{
  rit <- rS_FF_before[,i]  # For every i we are extracting a stock
  out.reg <- lm(rit~rB_FF_before+smb_before_benchmark+hml_before_benchmark)  # Doing a regression for each stock
  out.reg.sum <- summary(out.reg) 
  coeffs <- out.reg.sum$coefficients
  hml_FF_before[i,] <- coeffs[4,] # Storing coefficients in the alphas
}

write.csv(hml_FF_before, "Fama French HML Before Benchmark.csv")

###########################################
# Now do the same for after the benchmark
###########################################

idB_FF_after <- c(which(colnames(Stocks_bd_after)=="NASDAQ"))
rB_FF_after <- Stocks_bd_after[, idB_FF_after]
rS_FF_after <- Stocks_bd_after[, -idB_FF_after]

alphas_FF_after <- matrix(NA, NCOL(rS_FF_after), 4)
rownames(alphas_FF_after) <- colnames(rS_FF_after)
colnames(alphas_FF_after) <- c("Estimate", "SE", "t-ratio", "P-value")

for(i in 1:NCOL(rS_FF_after))
{
  rit <- rS_FF_after[,i]  # For every i we are extracting a stock
  out.reg <- lm(rit~rB_FF_after+smb_after_benchmark+hml_after_benchmark)  # Doing a regression for each stock
  out.reg.sum <- summary(out.reg) 
  coeffs <- out.reg.sum$coefficients
  alphas_FF_after[i,] <- coeffs[1,] # Storing coefficients in the alphas
}

write.csv(alphas_FF_after, "FamaFrench Alphas After Benchmark.csv")

################################

rmrf_FF_after <- matrix(NA, NCOL(rS_FF_after), 4)
rownames(rmrf_FF_after) <- colnames(rS_FF_after)
colnames(rmrf_FF_after) <- c("Estimate", "SE", "t-ratio", "P-value")

for(i in 1:NCOL(rS_FF_after))
{
  rit <- rS_FF_after[,i]  # For every i we are extracting a stock
  out.reg <- lm(rit~rB_FF_after+smb_after_benchmark+hml_after_benchmark)  # Doing a regression for each stock
  out.reg.sum <- summary(out.reg) 
  coeffs <- out.reg.sum$coefficients
  rmrf_FF_after[i,] <- coeffs[2,] # Storing coefficients in the alphas
}

write.csv(rmrf_FF_after, "Fama French RMRF After Benchmark.csv")

################################

smb_FF_after <- matrix(NA, NCOL(rS_FF_after), 4)
rownames(smb_FF_after) <- colnames(rS_FF_after)
colnames(smb_FF_after) <- c("Estimate", "SE", "t-ratio", "P-value")

for(i in 1:NCOL(rS_FF_after))
{
  rit <- rS_FF_after[,i]  # For every i we are extracting a stock
  out.reg <- lm(rit~rB_FF_after+smb_after_benchmark+hml_after_benchmark)  # Doing a regression for each stock
  out.reg.sum <- summary(out.reg) 
  coeffs <- out.reg.sum$coefficients
  smb_FF_after[i,] <- coeffs[3,] # Storing coefficients in the alphas
}

write.csv(smb_FF_after, "Fama French SMB After Benchmark.csv")

################################

hml_FF_after <- matrix(NA, NCOL(rS_FF_after), 4)
rownames(hml_FF_after) <- colnames(rS_FF_after)
colnames(hml_FF_after) <- c("Estimate", "SE", "t-ratio", "P-value")

for(i in 1:NCOL(rS_FF_after))
{
  rit <- rS_FF_after[,i]  # For every i we are extracting a stock
  out.reg <- lm(rit~rB_FF_after+smb_after_benchmark+hml_after_benchmark)  # Doing a regression for each stock
  out.reg.sum <- summary(out.reg) 
  coeffs <- out.reg.sum$coefficients
  hml_FF_after[i,] <- coeffs[4,] # Storing coefficients in the alphas
}

write.csv(hml_FF_after, "Fama French HML After Benchmark.csv")


###############################################
# Carhart model
#########################################

umd <- Y2[, 4]

alphas_C_5 <- matrix(NA, NCOL(rS_FF_5), 4)
rownames(alphas_C_5) <- colnames(rS_FF_5)
colnames(alphas_C_5) <- c("Estimate", "SE", "t-ratio", "P-value")

for(i in 1:NCOL(rS_FF_5))
{
  rit <- rS_FF_5[,i]  # For every i we are extracting a stock
  out.reg <- lm(rit~rB_FF_5+smb+hml+umd)  # Doing a regression for each stock
  out.reg.sum <- summary(out.reg) 
  coeffs <- out.reg.sum$coefficients
  alphas_C_5[i,] <- coeffs[1,] # Storing coefficients in the alphas
}

write.csv(alphas_C_5, "Carhart 5 Year Alphas.csv")

#################################################

rmrf_C_5 <- matrix(NA, NCOL(rS_FF_5), 4)
rownames(rmrf_C_5) <- colnames(rS_FF_5)
colnames(rmrf_C_5) <- c("Estimate", "SE", "t-ratio", "P-value")

for(i in 1:NCOL(rS_FF_5))
{
  rit <- rS_FF_5[,i]  # For every i we are extracting a stock
  out.reg <- lm(rit~rB_FF_5+smb+hml+umd)  # Doing a regression for each stock
  out.reg.sum <- summary(out.reg) 
  coeffs <- out.reg.sum$coefficients
  rmrf_C_5[i,] <- coeffs[2,] # Storing coefficients in the alphas
}

write.csv(rmrf_C_5, "Carhart 5 Year RMRF.csv")

###############################

smb_C_5 <- matrix(NA, NCOL(rS_FF_5), 4)
rownames(smb_C_5) <- colnames(rS_FF_5)
colnames(smb_C_5) <- c("Estimate", "SE", "t-ratio", "P-value")

for(i in 1:NCOL(rS_FF_5))
{
  rit <- rS_FF_5[,i]  # For every i we are extracting a stock
  out.reg <- lm(rit~rB_FF_5+smb+hml+umd)  # Doing a regression for each stock
  out.reg.sum <- summary(out.reg) 
  coeffs <- out.reg.sum$coefficients
  smb_C_5[i,] <- coeffs[3,] # Storing coefficients in the alphas
}

write.csv(smb_C_5, "Carhart 5 Year SMB.csv")

################################

hml_C_5 <- matrix(NA, NCOL(rS_FF_5), 4)
rownames(hml_C_5) <- colnames(rS_FF_5)
colnames(hml_C_5) <- c("Estimate", "SE", "t-ratio", "P-value")

for(i in 1:NCOL(rS_FF_5))
{
  rit <- rS_FF_5[,i]  # For every i we are extracting a stock
  out.reg <- lm(rit~rB_FF_5+smb+hml+umd)  # Doing a regression for each stock
  out.reg.sum <- summary(out.reg) 
  coeffs <- out.reg.sum$coefficients
  hml_C_5[i,] <- coeffs[4,] # Storing coefficients in the alphas
}

write.csv(hml_C_5, "Carhart 5 Year HML.csv")

######################################


umd_C_5 <- matrix(NA, NCOL(rS_FF_5), 4)
rownames(umd_C_5) <- colnames(rS_FF_5)
colnames(umd_C_5) <- c("Estimate", "SE", "t-ratio", "P-value")

for(i in 1:NCOL(rS_FF_5))
{
  rit <- rS_FF_5[,i]  # For every i we are extracting a stock
  out.reg <- lm(rit~rB_FF_5+smb+hml+umd)  # Doing a regression for each stock
  out.reg.sum <- summary(out.reg) 
  coeffs <- out.reg.sum$coefficients
  umd_C_5[i,] <- coeffs[5,] # Storing coefficients in the alphas
}

write.csv(umd_C_5, "Carhart 5 Year UMD.csv")

# as the 4th factor umd is statistically insignificant, we can remove it from our model
# and stick with using either the 3-factor model or the CAPM model

lm16 <- lm(NFLX_excR~NASDAQ_excR+smb+hml+umd)
summary(lm16)
lm17 <- lm(NKE_excR~NASDAQ_excR+smb+hml+umd)
summary(lm17)
lm18 <- lm(ORCL_excR~NASDAQ_excR+smb+hml+umd)
summary(lm18)
lm19 <- lm(PYPL_excR~NASDAQ_excR+smb+hml+umd)
summary(lm19)
lm20 <- lm(PM_excR~NASDAQ_excR+smb+hml+umd)
summary(lm20)

###################### MODEL TO FIND THE ADJUSTED R SWUAREDV VALUES #######################


