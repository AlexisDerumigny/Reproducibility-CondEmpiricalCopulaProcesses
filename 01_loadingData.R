
#################################"
# Library for loading the data

library(quantmod)
library(fGarch)

library(zoo)
library(timeSeries)
library(gridExtra)
library(pcaPP)
library(data.tree)
library(tidyverse)


# Loading the data from Yahoo Finance

getSymbols(
  # CAC40, Dow Jones, Nasdaq, 1EUR in USD, 1Bitcoin in EUR
  c("^FCHI", "^DJI", "^IXIC", "EURUSD=X" , "BTC-EUR"),
  src = "yahoo")

# Renaming some variables
EUR_USD = `EURUSD=X`
rm(`EURUSD=X`)
BTC_EUR = `BTC-EUR`
rm(`BTC-EUR`)

getSymbols(
  # Nikkei, 5 year Treasury, SP500, Eurostoxx
  c("^N225", "^FVX", "^GSPC", "^STOXX50E"),
  src = "yahoo")

getSymbols(
  # DAX Index, Amsterdam index
  c("^GDAXI",  "^AEX"),
  src = "yahoo")


# Automatic computation of returns

vecVariables = 
  c("DJI", "IXIC", "FVX",
    "EUR_USD", "BTC_EUR", "N225",
    "FCHI", "AEX", "GSPC", "STOXX50E")

for (vari in vecVariables) {
  eval(parse(text = paste0(vari, "<- xts::last(", vari, ",50)")))
}

source("lib_GARCH.R")
source("lib_computationReturnsInnov.R")

for (vari in vecVariables) {
  computeReturns(variableName = vari, max_global_lag = 2)
}

# We merge all the data
commandForMerging = 
  paste0("zoo_all <- merge.zoo(",
         paste0(vecVariables, collapse = ", "),
         ")")
eval(parse(text = commandForMerging))

## Cleaning of the variables
if (FALSE){
  rm(commandForMerging)
  for (vari in vecVariables) {
    eval(parse(text = paste0("rm(", vari, ")")))
  }
}

# Saving of all the data
write.zoo(zoo_all, "zoo_all.csv", sep = ";")


