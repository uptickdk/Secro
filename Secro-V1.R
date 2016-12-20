# DK- Probabilistic momentum
#
# Suppresses warnings
library(quantmod, quantstrat, FinancialInstrument)
options("getSymbols.warning4.0" = FALSE)

# Do some house cleaning
rm(list = ls(.blotter), envir = .blotter)

# Set the currency and the timezone
currency('USD')
Sys.setenv(TZ = "UTC")
initDate= "1990-01-01"   # must be before from date
from_date= "2014-12-31"
to_date= today()

# for analytics- need to specify- initial equity, trade size, and initialize the strategy
tradeSize<- 10000

# *** not sure what to use for universe  ******
# initEq<- tradeSize* length(symbols)

# Define symbols of interest
symobls<- spl('SPY,SLY,EEM,EFA,GLD,QQQ,ZIV,TLO')
getSymbols(tickers, src = 'yahoo', from = from_date, to= to_date , env = data, auto.assign = T)


