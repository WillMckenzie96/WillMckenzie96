rm(list=ls(all=TRUE))

# Mathematical model for the effect of a virus outbreak on the makret and securities

setwd("C:/Users/elemc/Desktop/Careers Work/Citi Internship")

install.packages("deSolve")
library(deSolve)

# The goal is to create a model around a non-existent virus, to see how quickly it may spread
# Will it reach globally?
# Then to analyse which sectors might be greatly affected

# The virus has first been found in London
# Ages 0-24 are most affected and are the only age group that has seen mortality
# Mortality rate is 0.01%

# S is the proportion of the amount of population that is susceptible to the virus

# Population of age group affected
# Population of London
pop_lon = 8.982 #million

# Important to note, births, immigration and emigration are ignored
# percentage of people within the susceptible age range
S = (0.013+0.051+0.062+0.054+0.049+0.074)*pop_lon

# population that could be potentially affected
St = S/pop_lon # in millions

# Say at current point, 10000 confirmed cases
cases = 10000
# cases per million
cpm = cases/1000000

# infected fraction of population
It = cpm/pop_lon

# There are currently 100 recovered individuals
R = 100/1000000
rt = R/pop_lon

# Now that data has been collected, a function can be made for how easily the virus can spread and 
# How many people it could potentially affect in the London area

proj_v <- function(pars){
  
  # A timeline is created for an entire year, with it being indexed
  init <- c(St = St, It = It, rt = rt) # this parameter sets the initial conditions
  t2  <- seq(0, 500, by = 1) # Time indexed (per day)
  
  # Below is the code for the SIR model in the pace of a function
  SIR <- function(t2, yinit, pars){
    
    with(as.list(c(yinit,pars)), {
      
      dS  <- -beta*It*St - mr*St
      dI  <- beta*It*St - gamma*It - mr*It
      dR <- gamma*It - mr*rt
      
      return(list(c(dS, dI, dR)))})
  }
  
  ## run the ode solver for the function specified (function defined above is used)
  ## return the value of each compartment (Susc, Infected, Recovered) for each time step.
  results <- ode(func = SIR, times = t2, y = init, parms = pars)
  results <- as.data.frame(results)
  
  ## Return result
  return(results)
}

##############################################################################

pars_test <- c(beta = 0.2, gamma = 0.01, mr = 0.0001)
results2   <- proj_v(pars_test)

matplot(x = results2[,1], y = results2[, 2:4], type="l", main="Forecasted Pro-V21 Virus Affect on London", lty=1, xlab = "Time", ylab = "Fraction of London Population")
legend("right", col=1:3, legend=c("Susceptible", "Infected", "Recovered"), lwd=1)

# Need to check how many people could potentially die

# find the total deaths
dr <- pars_test[3]*S
print(dr*1e+6)

write.csv(results2, "Virus parameters.csv")

