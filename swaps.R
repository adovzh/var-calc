defswap <- function(rate, freq, pos, maturity, notional) {
    structure(list(rate = rate, freq = freq, pos = pos, maturity = maturity,
                   notional = notional), class="swap")
}

price.swap <- function(swap, valuation, refdata) {
    # short factor
    sfactor <- if (swap$pos == "long") 1 else -1
    
    # pricing fixed leg as a bond
    b <- defbond(coupon = swap$rate, maturity = swap$maturity, 
                 face = swap$notional, freq = swap$freq)
    b$swap.leg = TRUE
    fixed <- price(b, valuation, refdata)
    
    # pricing floating leg
    cashflows <- cashflow.dates(valuation, as.Date(swap$maturity), swap$freq, prev.date = TRUE)
    # should contain previous and next payments
    stopifnot(length(cashflows) > 1)
    
    bbsw <- refdata$swaps()
    # P * exp(fstar * dt - f * dT)
    fstar <- onDate.rate(bbsw, cashflows[1], cashflows[2])
    f <- onDate.rate(bbsw, valuation, cashflows[2])
    dt <- as.numeric(as.Date(cashflows[2]) - as.Date(cashflows[1])) / 365
    dT <- as.numeric(as.Date(cashflows[2]) - as.Date(valuation)) / 365
    floating <- swap$notional * exp(fstar * dt - f * dT)
    
    sfactor * (fixed - floating)
}
