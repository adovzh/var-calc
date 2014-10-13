deffxspot <- function(currency, position, valuation, refdata) {
    rates <- refdata$rates()
    xrate <- rates[rates$Date == valuation, currency]
    amount <- xrate * position
    structure(list(currency = currency, amount = amount), class="fxspot")
}

price.fxspot <- function(fxspot, valuation, refdata) {
    rates <- refdata$rates()
    xrate <- rates[rates$Date == valuation, fxspot$currency]
    
    fxspot$amount / xrate
}

pricev.fxspot <- function(fxspot, valuation, refdata) {
    price(fxspot, valuation, refdata)
}

priceh.fxspot <- function(fxspot, valuation, refdata) {
    function(r) {
        fxspot$amount * r
    }
}

returns.fxspot <- function(fxspot, valuation, refdata, lookback) {
    rates <- refdata$rates()
    int <- as.interval(lookback, as.Date(valuation) - lookback)
    rs <- stocks[stocks$Date %within% int, fxspot$currency]
    rs[, fxspot$currency] <- 1 / rs[, fxspot$currency]
    diff(rs) / rs[-length(rs)]
}

returns.rf_currency <- function(fxspot, valuation, refdata, lookback) {
    rs <- history(fxspot, valuation, refdata, lookback)
    diff(rs) / rs[-length(rs)]
}

history.rf_currency <- function(fxspot, valuation, refdata, lookback) {
    rates <- refdata$rates()
    int <- as.interval(lookback, as.Date(valuation) - lookback)
    rs <- rates[rates$Date %within% int, fxspot$currency]
    1 / rs    
}

delta.fxspot <- function(fxspot, ...) 1
gamma.fxspot <- function(fxspot, ...) 0

riskfactors.fxspot <- function(fxspot, ...) {
    rf <- structure(list(currency = fxspot$currency), class="rf_currency")
    list(rf)
}

deffxfwd <- function(currency, amount, fwdrate, maturity) {
    structure(list(currency = currency, amount = amount, fwdrate = fwdrate,
                   maturity = maturity), class = "fxfwd")
}

price.fxfwd <- function(fxfwd, valuation, refdata) {
    # load refdata
    curves <- refdata$curves()
    rates <- refdata$rates()
    
    # current exchange rate
    xrate <- rates[rates$Date == valuation, fxfwd$currency]

    # domestic interest rate
    r <- onDate.rate(curves$AUD, valuation, fxfwd$maturity)

    # foreign interest rate
    rf <- onDate.rate(curves[[fxfwd$currency]], valuation, fxfwd$maturity)
    
    dt <- as.numeric(as.Date(fxfwd$maturity) - as.Date(valuation)) / 365
    
    # express rates in base currency
    S <- 1 / xrate
    K <- 1 / fxfwd$fwdrate
    
    # price
    (S * exp(-rf * dt) - K * exp(-r * dt)) * fxfwd$amount
}

deffxoption <- function(currency, callFlag, pos, amount, strike, maturity) {
    structure(list(currency = currency, callFlag = callFlag, pos = pos,
                   amount = amount, strike = strike, maturity = maturity), 
              class = "fxoption")
}

price.fxoption <- function(fxo, valuation, refdata) {
    require(lubridate)
    
    curves <- refdata$curves()
    rates <- refdata$rates()    
    
    # put factor
    pfactor <- if (fxo$callFlag == "c") 1 else -1
    
    # short factor
    sfactor <- if (fxo$pos == "long") 1 else -1
    
    # estimate number of historic data points in similar period last year
    lyStart <- ymd(val.date) - years(1)
#     lyEnd <- ymd(fxo$maturity) - years(1)
#     n <- nrow(rates[rates$Date >= as.Date(lyStart) & rates$Date < as.Date(lyEnd), ])
    n <- nrow(rates[rates$Date > as.Date(lyStart), ])
    
    # volatility estimate
    # we do not need to take a reciprocal of exchange rates
    # as it does not have any effect on a vlotility of log returns
    vol <- sd(diff(log(tail(rates, n + 1)[, fxo$currency]))) * sqrt(252)
    vol <- vol * pfactor
    
    S <- 1 / rates[rates$Date == valuation, fxo$currency]
    K <- 1 / fxo$strike
    
    # domestic interest rate
    r <- onDate.rate(curves$AUD, valuation, fxo$maturity)
    
    # foreign interest rate
    rf <- onDate.rate(curves[[fxo$currency]], valuation, fxo$maturity)
    
    dt <- as.numeric(as.Date(fxo$maturity) - as.Date(valuation)) / 365
    
    d1 <- (log(S / K) + (r - rf + vol^2 / 2) * dt) / (vol * sqrt(dt))
    d2 <- d1 - vol * sqrt(dt)
    p <- (S * exp(-rf * dt) * pnorm(d1) - K * exp(-r * dt) * pnorm(d2))
    p * pfactor * sfactor * fxo$amount
}