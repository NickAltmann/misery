# Load zoo timeseries library and Quandl financial data library
library(zoo)
library(Quandl)


load_misery <- function() {
  # Download the data for UK monthly unemployment and cpi inflation figures.
  # Symbols previously looked up on Quandl
  unemployment <-  Quandl('BCB/3791', type="zoo")
  cpi <-  Quandl('BCB/3798', type="zoo")
  
  # Calculate year-on-year inflation via rolling window of 13 months data.
  inflation <- rollapplyr(cpi, 13, function(x) 100 * (x[13] - x[1])/x[1])
  
  # Calculate the "misery" index as the sum of inflation and unemployment
  misery <- unemployment + inflation
  
  # Find limits of the data
  begin_dt <- max(c(start(unemployment),start(inflation)))
  end_dt <- min(c(end(unemployment),end(inflation)))
  
  # Combine into a single series and trim to limits
  all <- window(merge(unemployment, inflation, misery), start = begin_dt, end = end_dt)
  
  return (all)
}

summarise_by_pm <- function(data) {
  pms <- data.frame( pm = c("major", "blair", "brown", "cameron"), 
                     start = c(as.Date('1990-11-28'), 
                               as.Date('1997-05-02'), 
                               as.Date('2007-06-27'),
                               as.Date('2010-05-11')))
  
  aggregate(data, 
            list(sapply(index(data), function(x) pms[findInterval(as.Date(x), pms$start),"pm"])), 
            function (x) round(mean(x),1))
}

data <- load_misery()
plot(data)
print (summarise_by_pm(data))



