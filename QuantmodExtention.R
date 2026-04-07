# ─────────────────────────────────────────
# INSTALL PACKAGES (run once only)
# ─────────────────────────────────────────
install.packages(c("quantmod", "lmtest", "sandwich", "car", "tseries"))

# ─────────────────────────────────────────
# LOAD LIBRARIES
# ─────────────────────────────────────────
library(quantmod)
library(lmtest)
library(sandwich)
library(car)
library(tseries)

# ─────────────────────────────────────────
# DOWNLOAD ALL DATA AUTOMATICALLY
# ─────────────────────────────────────────
start <- as.Date("2023-01-01")
end   <- as.Date("2026-03-31")

# Divi's Laboratories stock (NSE)
getSymbols("DIVISLAB.NS", src = "yahoo", from = start, to = end)

# Nifty 50 index
getSymbols("^NSEI", src = "yahoo", from = start, to = end)

# Exchange rates
getSymbols("USDINR=X", src = "yahoo", from = start, to = end)
getSymbols("EURINR=X", src = "yahoo", from = start, to = end)
getSymbols("GBPINR=X", src = "yahoo", from = start, to = end)

# ─────────────────────────────────────────
# EXTRACT CLOSING PRICES ONLY
# ─────────────────────────────────────────
divislab <- Cl(DIVISLAB.NS)
nifty    <- Cl(NSEI)
usdinr   <- Cl(`USDINR=X`)
eurinr   <- Cl(`EURINR=X`)
gbpinr   <- Cl(`GBPINR=X`)