onDate.curve <- function(curve, date) curve[curve[,1] == date, -1]

zero.maturities <- function(curve, valuation) {
    do.call("c", lapply(cnames.as.period(names(curve)), function(p) {
        as.Date(valuation) + p
    }))
}

# returns interpolated value of interest rate
# at the specified date (can be a vector)
# from a given curves data frame
# on a given valuation date
onDate.rate <- function(curves, valuation, date) {
    vcurve <- onDate.curve(curves, valuation)
    zm <- zero.maturities(vcurve, valuation)
    x <- sapply(date, function(m) {
        # i - next zero index
        i <- min(which(zm > m))
        m <- as.Date(m)
        a <- as.numeric(m - zm[i - 1]) / as.numeric(zm[i] - zm[i - 1])
        as.numeric(vcurve[i - 1] * (1 - a) + vcurve[i] * a)        
    }, USE.NAMES = FALSE)
}

cashflow.dates <- function(valuation, maturity, freq = 2, prev.date = FALSE) {
    require(lubridate)
    if (maturity <= valuation) { if (prev.date) maturity else as.Date(vector()) }
    else c(cashflow.dates(valuation, maturity - months(12 / freq), freq, prev.date), maturity)
}

defbond <- function(coupon, maturity, face, freq = 2) {
    structure(list(coupon = coupon, maturity = maturity, 
                   face = face, freq = freq), class="bond")
}

price.bond <- function(bond, valuation, refdata) {
    require(lubridate)
    
    zcurve <- if (exists("swap.leg", bond)) refdata$swaps() else refdata$curves()$AUD
    
    # maturities (cashflow dates)
    maturity <- cashflow.dates(as.Date(valuation), as.Date(bond$maturity), bond$freq)
    
    # cashflows
    coupon.paym <- rep(bond$face * bond$coupon / bond$freq, length(maturity))
    fv.paym <- c(rep(0, length(maturity) - 1), bond$face)
    cashflows <- coupon.paym + fv.paym
    
    # rates
    v1 <- onDate.rate(zcurve, valuation, maturity)
    
    # annualised maturities
    dt <- as.numeric(as.Date(maturity) - as.Date(valuation)) / 365
    sum(cashflows * exp(-1 * v1 * dt))
}

