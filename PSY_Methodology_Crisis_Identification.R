rm(list=ls(all=TRUE))

# Use your own working directory, the packages below are essential for the code, certain versions won't work unless
# You have Rtools installed!!!
setwd("C:/Users/elemc/Desktop/Big Data Analytics and Visualisation/Coursework")
install.packages("Rtools")
install.packages("psymonitor")
library("psymonitor")

# This is just the data used from one of the old seminars, we would use our own data taken from databases
# Dependent on what we choose for our variables and economies
data <- read.csv("US_SP_corrected.csv", header=TRUE)
date <- as.Date(data$Date, format = "%d/%m/%Y")

# Use the variable you want
SP <- (data$SP) # adjusted closing values
NK <- (data$Nikkei) # adjusted closing values
US <- (data$Y.spread_US)
JP <- (data$Y.spread_Japan)
NQ <- (data$NASDAQ)

# Make a first plot of the series
plot(date, SP, type="l", xlab="Time", ylab="Stock Price (USD/Share)", main="S&P 500", lwd=2)
plot(date, NK, type="l", xlab="Time", ylab="Stock Price (USD/Share)", main="Nikkei 225", lwd=2)
plot(date, US, type="l", xlab="Time", ylab="Stock Price (USD/Share)", main="U.S 10Yr-3Mo Yield Spread", lwd=2)
plot(date, JP, type="l", xlab="Time", ylab="Stock Price (USD/Share)", main="Japan 10Yr-3Mo Yield Spread", lwd=2)
plot(date, NQ, type="l", xlab="Time", ylab="Stock Price (USD/Share)", main="NASDAQ", lwd=2)


#  This gives us a visual representation of any explosive behavior, without having to do any further analysis

# Now we want to attempt the PSY test 
# The sample size is
N <- NROW(SP)

# Set the min window size
# This below, is a value that makes sure that the impact of previous bubbles does not
# affect the next ones, we can check the Phillips paper for this
minwin <- 0.07*N

# for the 27 year period, a min window of 0.1 with lag order 12 is used

# Round it
minwin <- round(minwin)

acf(SP, lag.max = 100)
acf(NK, lag.max = 100)
acf(US, lag.max = 100)
acf(JP, lag.max = 100)
acf(NQ, lag.max = 100)


# Calculate the recursive ADF
# To make it quicker set the lags in ADF
# otherwise you can select them by AIC or BIC
plags <- 5
plags_NK <- 4
# This can be ignored as we are using the AIC
# IC represents the information criterion, 1= AIC, 2= AIC 
ADF_SP <- PSY(SP, swindow0=minwin, IC = 0, adflag = plags)
ADF_NK <- PSY(NK, swindow0=minwin, IC = 0, adflag = plags_NK)
ADF_US <- PSY(US, swindow0=minwin, IC = 0, adflag = plags)
ADF_JP <- PSY(JP, swindow0=minwin, IC = 0, adflag = plags)
ADF_NQ <- PSY(NQ, swindow0=minwin, IC = 0, adflag = plags)



# Plot the ADF, giving us visual representation
plot(date[minwin:N], ADF_SP, main="ADF S&P 500", type="p", pch=15, col="blue", lwd=2)
plot(date[minwin:N], ADF_NK, main="Nikkei 225", type="p", pch=15, col="blue", lwd=2)
plot(date[minwin:N], ADF_US, main="ADF U.S Yield Spread", type="p", pch=15, col="blue", lwd=2)
plot(date[minwin:N], ADF_JP, main="ADF Japan Yield Spread", type="p", pch=15, col="blue", lwd=2)
plot(date[minwin:N], ADF_NQ, main="ADF NASDAQ", type="p", pch=15, col="blue", lwd=2)


# Use bootstrap and calculate the p-values
# IC stands for information criterion, look up PSY
qADF_SP <- cvPSYwmboot(SP, swindow0=minwin, IC=0, adflag = plags,Tb=minwin, nboot=10000, useParallel = TRUE)

qADF_NK <- cvPSYwmboot(NK, swindow0=minwin, IC=0,adflag = plags_NK,Tb=minwin, nboot=10000, useParallel = TRUE)

qADF_US <- cvPSYwmboot(US, swindow0=minwin, IC=0,adflag = plags,Tb=minwin, nboot=10000, useParallel = TRUE)

qADF_JP <- cvPSYwmboot(JP, swindow0=minwin, IC=0, adflag = plags,Tb=minwin, nboot=10000, useParallel = TRUE)

qADF_NQ <- cvPSYwmboot(NQ, swindow0=minwin, IC=0, adflag = plags,Tb=minwin, nboot=10000, useParallel = TRUE)


# Now plot the ADF and the critical value of 90% (blue), 95% (green), 99% (red)
plot(date[minwin:N], ADF_SP, ylab = "ADF", xlab = "Time", main="ADF - S&P 500", type="p", pch=15, col="blue", lwd=2)
abline(h=qADF_SP[1], col="purple")
abline(h=qADF_SP[2], col="green")
abline(h=qADF_SP[3], col="red")
legend(x = "topleft", legend = c("90%", "95%", "99%"), lty = 1, 
       lwd = 2, col = c("purple", "green", "red"))

plot(date[minwin:N], ADF_NK, main="ADF - NIKKEI 225",  ylab = "ADF", xlab = "Time", type="p", pch=15, col="blue", lwd=2)
abline(h=qADF_NK[1], col="purple")
abline(h=qADF_NK[2], col="green")
abline(h=qADF_NK[3], col="red")
legend(x = "topleft", legend = c("90%", "95%", "99%"), lty = 1, 
       lwd = 2, col = c("purple", "green", "red"))

plot(date[minwin:N], ADF_US, main="ADF - U.S Yield Spread",  ylab = "ADF", xlab = "Time", type="p", pch=15, col="blue", lwd=2)
abline(h=qADF_US[1], col="purple")
abline(h=qADF_US[2], col="green")
abline(h=qADF_US[3], col="red")
legend(x = "topleft", legend = c("90%", "95%", "99%"), lty = 1, 
       lwd = 2, col = c("purple", "green", "red"))

plot(date[minwin:N], ADF_JP, main="ADF - Japan Yield Spread",  ylab = "ADF", xlab = "Time", type="p", pch=15, col="blue", lwd=2)
abline(h=qADF_JP[1], col="purple")
abline(h=qADF_JP[2], col="green")
abline(h=qADF_JP[3], col="red")
legend(x = "topright", legend = c("90%", "95%", "99%"), lty = 1, 
       lwd = 2, col = c("purple", "green", "red"))

plot(date[minwin:N], ADF_NQ, main="ADF - NASDAQ",  ylab = "ADF", xlab = "Time", type="p", pch=15, col="blue", lwd=2)
abline(h=qADF_NQ[1], col="purple")
abline(h=qADF_NQ[2], col="green")
abline(h=qADF_NQ[3], col="red")
legend(x = "topleft", legend = c("90%", "95%", "99%"), lty = 1, 
       lwd = 2, col = c("purple", "green", "red"))

# The above gives us a visual representation of what periods can be considered bubble periods
# everything above the lines would be considered a bubble period
# We would reject the null in this case

# Use the 90% critical value --> choose 1
# Use the 95% critical value --> choose 2
# Use the 99% critical value --> choose 3
alpha.level <- 2 # We have chosen in this case, 95% critical value

benchmark_SP <- rep(qADF_SP[alpha.level], NROW(ADF)) # S&P 500
benchmark_NK <- rep(qADF_NK[alpha.level], NROW(ADF)) # Nikkei 225
benchmark_US <- rep(qADF_US[alpha.level], NROW(ADF)) # S&P 500
benchmark_JP <- rep(qADF_JP[alpha.level], NROW(ADF)) # S&P 500
benchmark_NQ <- rep(qADF_NQ[alpha.level], NROW(ADF)) # S&P 500



# bubble gives us all the points on the ADF plot that can be considered bubble behaviour
bubble_SP <- (ADF_SP>benchmark_SP)
bubble_NK <- (ADF_NK>benchmark_NK)
bubble_US <- (ADF_US>benchmark_US)
bubble_JP <- (ADF_JP>benchmark_JP)
bubble_NQ <- (ADF_NQ>benchmark_NQ)



# For whatever reason, this R code doesn't process at all, particularly with ADF line
# Make sure to check over this with Illias

# Make the plot with the minimum window, so that we don't miss any potential bubble behaviour
plot(date[minwin:N], SP[minwin:N], type="l",lwd=2, xlab = "Time", ylab = "Stock Price (USD/Share)", main="S&P 500")


for(i in 1:NROW(bubble_SP)){
  if(bubble_SP[i]==TRUE){
    abline(v=date[minwin:N][i], col="Gray", lwd=3)
  }
}
# add the BTC lines again, as it's not visible
lines(date[minwin:N], SP[minwin:N], lwd=2,)

# Do the same for Nikkei

plot(date[minwin:N], NK[minwin:N], type="l",lwd=2, xlab = "Time", ylab = "Stock Price (JPY/Share)", main="Nikkei 225")


for(i in 1:NROW(bubble_NK)){
  if(bubble_NK[i]==TRUE){
    abline(v=date[minwin:N][i], col="Gray", lwd=3)
  }
}
# add the BTC lines again, as it's not visible
lines(date[minwin:N], NK[minwin:N], lwd=2,)

# Do the same for US Yield Spread

plot(date[minwin:N], US[minwin:N], type="l",lwd=2, xlab = "Time", ylab = "T-bill Yield Spread (% per Annum)", main="U.S 10Yr-3Mo Yield Spread")


for(i in 1:NROW(bubble_US)){
  if(bubble_US[i]==TRUE){
    abline(v=date[minwin:N][i], col="gray", lwd=3)
    abline(h=0, col="red")
  }
}
# add the BTC lines again, as it's not visible
lines(date[minwin:N], US[minwin:N], lwd=2,)

# Do the same for Japan Yield Spread

plot(date[minwin:N], JP[minwin:N], type="l",lwd=2, xlab = "Time", ylab = "T-bill Yield Spread (% per Annum)", main="Japan 10Yr-3Mo Yield Spread")


for(i in 1:NROW(bubble_JP)){
  if(bubble_JP[i]==TRUE){
    abline(v=date[minwin:N][i], col="Gray", lwd=3)
    abline(h=0, col="red")
  }
}
# add the BTC lines again, as it's not visible
lines(date[minwin:N], JP[minwin:N], lwd=2)

# NASDAQ

plot(date[minwin:N], NQ[minwin:N], type="l",lwd=2, xlab = "Time", ylab = "Stock Price (USD/Share)", main="NASDAQ")


for(i in 1:NROW(bubble_NQ)){
  if(bubble_NQ[i]==TRUE){
    abline(v=date[minwin:N][i], col="Gray", lwd=3)
  }
}
# add the BTC lines again, as it's not visible
lines(date[minwin:N], NQ[minwin:N], lwd=2)

# Extracting time frames

swindow1 <- minwin
dat <- date[swindow1:N]
SP_dates <- locate(bubble_SP, dat)
SPd <- disp(SP_dates, swindow1)
print(SPd)
write.csv(SPd, "S&P500_Bubble_Dates.csv")

NK_dates <- locate(bubble_NK, dat)
print(NK_dates)
NKd <- disp(NK_dates, swindow1)
print(NKd)
write.csv(NKd, "NIKKEI225_Bubble_Dates.csv")

US_dates <- locate(bubble_US, dat)
print(US_dates)
USd <- disp(US_dates, swindow1)
print(USd)
write.csv(USd, "US_Yield_Bubble_Dates.csv")

JP_dates <- locate(bubble_JP, dat)
print(JP_dates)
JPd <- disp(JP_dates, swindow1)
print(JPd)
write.csv(JPd, "JP_Yield_Bubble_Dates.csv")

NQ_dates <- locate(bubble_NQ, dat)
print(NQ_dates)
NQd <- disp(NQ_dates, swindow1)
print(NQd)
write.csv(NQd, "NQ_Yield_Bubble_Dates.csv")

data <- data[128:NROW(data), ]
date <- date[128:NROW(date)]
