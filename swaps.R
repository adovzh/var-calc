defswap <- function(rate, freq, pos, maturity, notional) {
    structure(list(rate = rate, freq = freq, pos = pos, maturity = maturity,
                   notional = notional), class="swap")
}

price.swap <- function(swap, valuation, refdata) {
    b <- defbond(coupon = swap$rate, maturity = swap$maturity, 
                 face = swap$notional, freq = swap$freq)
    b$swap.leg = TRUE
    price(b, valuation, refdata)
}
