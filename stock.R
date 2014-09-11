defstock <- function(symbol, amount, pos = "long") {
    structure(list(symbol = symbol, amount = amount, pos = pos), class="stock")
}

price.stock <- function(stock, valuation, refdata) {
    stocks <- refdata$stocks()
    
    # short factor
    sfactor <- if (stock$pos == "long") 1 else -1
    
    stocks[stocks$Date == valuation, stock$symbol] * stock$amount * sfactor
}

defoption <- function(symbol, callFlag, pos, amount, strike, maturity, vol) {
    structure(list(symbol = symbol, callFlag = callFlag, pos = pos, amount = amount,
                   strike = strike, maturity = maturity, vol = vol), class="option")
}

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
