defstock <- function(symbol, amount, pos = "long") {
    structure(list(symbol = symbol, amount = amount, pos = pos), class="stock")
}

# stock generic implementations

price.stock <- function(stock, valuation, refdata) {
    stocks <- refdata$stocks()
    
    # short factor
    sfactor <- if (stock$pos == "long") 1 else -1
    
    stocks[stocks$Date == valuation, stock$symbol] * stock$amount * sfactor
}

pricev.stock <- function(stock, valuation, refdata) {
    price(stock, valuation, refdata)
}

priceh.stock <- function(stock, valuation, refdata) {
    # short factor
    sfactor <- if (stock$pos == "long") 1 else -1
    
    function(r) r * stock$amount * sfactor
}

returns.stock <- function(stock, valuation, refdata, lookback) {
    stocks <- refdata$stocks()
    int <- as.interval(lookback, as.Date(valuation) - lookback)
    rs <- stocks[stocks$Date %within% int, stock$symbol]
    diff(rs) / rs[-length(rs)]
}

delta.stock <- function(stock, ...) 1
gamma.stock <- function(stock, ...) 0

riskfactors.stock <- function(stock, ...) {
    rf <- structure(list(symbol = stock$symbol), class="rf_stock")
    list(rf)
}

deltaNormal.stock <- function(stock, valuation, refdata) {
    function(conf, days) {
        x <- price(stock, valuation, refdata)
        sdev <- sd(returns(stock, valuation, refdata, years(2)))
        qnorm(conf) * abs(x) * sdev * sqrt(days)
    }
}

# options

defoption <- function(symbol, callFlag, pos, amount, strike, maturity, vol) {
    structure(list(symbol = symbol, callFlag = callFlag, pos = pos, amount = amount,
                   strike = strike, maturity = maturity, vol = vol), class="option")
}

# option generic implementations

price.option <- function(option, valuation, refdata) {
    curves <- refdata$curves()
    stocks <- refdata$stocks()
    
    # put factor
    pfactor <- if (option$callFlag == "c") 1 else -1
    
    # short factor
    sfactor <- if (option$pos == "long") 1 else -1
    
    # negative volatility ;)
    vol <- option$vol * pfactor
    
    S <- stocks[stocks$Date == valuation, option$symbol]
    K <- option$strike
    
    # domestic interest rate
    r <- onDate.rate(curves$AUD, valuation, option$maturity)
    
    dt <- as.numeric(as.Date(option$maturity) - as.Date(valuation)) / 365
    
    d1 <- (log(S / K) + (r + vol^2 / 2) * dt) / (vol * sqrt(dt))
    d2 <- d1 - vol * sqrt(dt)
    p <- (S * pnorm(d1) - K * exp(-r * dt) * pnorm(d2))
    p * pfactor * sfactor * option$amount
}

pricev.option <- function(option, valuation, refdata) {
    price(underlying(option), valuation, refdata)
}

priceh.option <- function(option, valuation, refdata) {
    curves <- refdata$curves()
    stocks <- refdata$stocks()
    
    # put factor
    pfactor <- if (option$callFlag == "c") 1 else -1
    
    # short factor
    sfactor <- if (option$pos == "long") 1 else -1
    
    # negative volatility ;)
    vol <- option$vol * pfactor
    
    # strike
    K <- option$strike
    
    # domestic interest rate
    r <- onDate.rate(curves$AUD, valuation, option$maturity)
    
    dt <- as.numeric(as.Date(option$maturity) - as.Date(valuation)) / 365
    
    function (S) {
        d1 <- (log(S / K) + (r + vol^2 / 2) * dt) / (vol * sqrt(dt))
        d2 <- d1 - vol * sqrt(dt)
        p <- (S * pnorm(d1) - K * exp(-r * dt) * pnorm(d2))
        p * pfactor * sfactor * option$amount
    }
}

delta.option <- function(option, valuation, refdata) {
    curves <- refdata$curves()    
    stocks <- refdata$stocks()
    
    # put factor
    pfactor <- if (option$callFlag == "c") 1 else -1
    
    # short factor
    sfactor <- if (option$pos == "long") 1 else -1
    
    # negative volatility (makes d1 => -d1)
    vol <- option$vol * pfactor
        
    S <- stocks[stocks$Date == valuation, option$symbol]
    K <- option$strike
    
    # domestic interest rate
    r <- onDate.rate(curves$AUD, valuation, option$maturity)
    
    dt <- as.numeric(as.Date(option$maturity) - as.Date(valuation)) / 365
    
    d1 <- (log(S / K) + (r + vol^2 / 2) * dt) / (vol * sqrt(dt))
    pnorm(d1) * pfactor
}

gamma.option <- function(option, valuation, refdata) {
    curves <- refdata$curves()    
    stocks <- refdata$stocks()
    
    # short factor
    sfactor <- if (option$pos == "long") 1 else -1
    
    vol <- option$vol

    S <- stocks[stocks$Date == valuation, option$symbol]
    K <- option$strike
    
    # domestic interest rate
    r <- onDate.rate(curves$AUD, valuation, option$maturity)
    
    dt <- as.numeric(as.Date(option$maturity) - as.Date(valuation)) / 365
    
    d1 <- (log(S / K) + (r + vol^2 / 2) * dt) / (vol * sqrt(dt))
    
    dnorm(d1) / (S * vol * sqrt(dt))
}

riskfactors.option <- function(option, ...) {
    rf <- structure(list(symbol = option$symbol), class = "rf_stock")
    list(rf)
}

underlying.option <- function(option) {
    defstock(symbol = option$symbol, amount = option$amount, pos = option$pos)
}

returns.rf_stock <- function(stock, valuation, refdata, lookback) {
    rs <- history(stock, valuation, refdata, lookback)
    diff(rs) / rs[-length(rs)]
}

history.rf_stock <- function(stock, valuation, refdata, lookback) {
    stocks <- refdata$stocks()
    int <- as.interval(lookback, as.Date(valuation) - lookback)
    stocks[stocks$Date %within% int, stock$symbol]    
}

deltaNormal.option <- function(option, valuation, refdata) {
    deltaNormal(defportfolio(option), valuation, refdata)
}

deltaGammaMC.option <- function(option, valuation, refdata) {
    deltaGammaMC(defportfolio(option), valuation, refdata)
}