# ─────────────────────────────────────────
# INSTALL PACKAGES (run once only)
# ─────────────────────────────────────────
# install.packages(c("quantmod", "lmtest", "sandwich", "car", "tseries"))

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

# ─────────────────────────────────────────
# CALCULATE DAILY LOG RETURNS
# ─────────────────────────────────────────
r_divislab <- dailyReturn(divislab, type = "log")
r_nifty    <- dailyReturn(nifty,    type = "log")
r_usd      <- dailyReturn(usdinr,   type = "log")
r_eur      <- dailyReturn(eurinr,   type = "log")
r_gbp      <- dailyReturn(gbpinr,   type = "log")

# ─────────────────────────────────────────
# MERGE ALL INTO ONE DATAFRAME
# ─────────────────────────────────────────
df <- merge(r_divislab, r_nifty, r_usd, r_eur, r_gbp)
colnames(df) <- c("r_divislab", "r_nifty", 
                  "r_usd", "r_eur", "r_gbp")
df <- na.omit(df)

# ─────────────────────────────────────────
# RISK FREE RATE (daily)
# ─────────────────────────────────────────
df$year <- as.integer(format(index(df), "%Y"))

df$rf <- ifelse(df$year == 2023, 6.80/365/100,
                ifelse(df$year == 2024, 6.80/365/100,
                       6.50/365/100))

# ─────────────────────────────────────────
# EXCESS RETURNS
# ─────────────────────────────────────────
df$STOCKRF <- df$r_divislab - df$rf
df$MKRF    <- df$r_nifty    - df$rf

# Convert to dataframe for regression
df_reg <- as.data.frame(df)

# ─────────────────────────────────────────
# BASELINE REGRESSION
# ─────────────────────────────────────────
model1 <- lm(STOCKRF ~ MKRF + r_usd + r_eur + r_gbp,
             data = df_reg)
summary(model1)

# ─────────────────────────────────────────
# DIAGNOSTIC TESTS
# ─────────────────────────────────────────

# (a) Serial Correlation
bgtest(model1, order = 5)

# (b) Heteroscedasticity
bptest(model1)

# (c) Multicollinearity
vif(model1)

# (d) Normality of residuals
jarque.bera.test(residuals(model1))

# ─────────────────────────────────────────
# ROBUST STANDARD ERRORS (if needed)
# ─────────────────────────────────────────
coeftest(model1, vcov = vcovHAC(model1))

# ─────────────────────────────────────────
# RESIDUAL PLOTS
# ─────────────────────────────────────────
par(mfrow = c(2,2))
plot(model1)

# ─────────────────────────────────────────
# REDUCED MODEL (if GBP insignificant)
# ─────────────────────────────────────────
model2 <- lm(STOCKRF ~ MKRF + r_usd + r_eur,
             data = df_reg)
summary(model2)

# Compare models
AIC(model1, model2)
BIC(model1, model2)