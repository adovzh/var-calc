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

returns.stock <- function(stock, valuation, refdata, lookback) {
    stocks <- refdata$stocks()
    int <- as.interval(lookback, as.Date(valuation) - lookback)
    rs <- stocks[stocks$Date %within% int, stock$symbol]
    diff(rs) / rs[-length(rs)]
}

delta.stock <- function(stock, ...) stock$amount
gamma.stock <- function(stock, ...) 0

factormap.stock <- function(stock, ...) defportfolio(stock)

is.same.stock <- function(stock, that) {
    class(stock) == class(that) && stock$symbol == that$symbol
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
    
    vol <- option$vol

    S <- stocks[stocks$Date == valuation, option$symbol]
    K <- option$strike
    
    # domestic interest rate
    r <- onDate.rate(curves$AUD, valuation, option$maturity)
    
    dt <- as.numeric(as.Date(option$maturity) - as.Date(valuation)) / 365
    
    d1 <- (log(S / K) + (r + vol^2 / 2) * dt) / (vol * sqrt(dt))
    
    dnorm(d1) / (S * vol * sqrt(dt))
}

factormap.option <- function(option, valuation, refdata) {
    amount <- delta(option, valuation, refdata) * option$amount
    pos <- if (amount > 0) option$pos else if (option$pos == "long") "short" else "long"
    defportfolio(defstock(symbol = option$symbol, amount = abs(amount), pos = pos))
}