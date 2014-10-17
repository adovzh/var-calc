onDate.curve <- function(curve, date) curve[curve[,1] == date, -1]

zero.maturities <- function(curvenames, valuation) {
    do.call("c", lapply(cnames.as.period(curvenames), function(p) {
        as.Date(valuation) + p
    }))
}

# returns interpolated value of interest rate
# at the specified date (can be a vector)
# from a given curves data frame
# on a given valuation date
onDate.rate <- function(curves, valuation, date) {
    vcurve <- onDate.curve(curves, valuation)
    zm <- zero.maturities(names(vcurve), valuation)
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

getcurves <- function(bond, refdata) {
    if (exists("swap.leg", bond)) refdata$swaps() else refdata$curves()$AUD
}

price.bond <- function(bond, valuation, refdata) {
    sum(pricev(bond, valuation, refdata))
}

pricev.bond <- function(bond, valuation, refdata) {
    require(lubridate)
    
    zcurve <- getcurves(bond, refdata)
    
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
    cashflows * exp(-1 * v1 * dt)
}

priceh.bond <- function(bond, valuation, refdata) {
    require(lubridate)
    
    zcurve <- getcurves(bond, refdata)
    
    # maturities (cashflow dates)
    maturity <- cashflow.dates(as.Date(valuation), as.Date(bond$maturity), bond$freq)
    
    # cashflows
    coupon.paym <- rep(bond$face * bond$coupon / bond$freq, length(maturity))
    fv.paym <- c(rep(0, length(maturity) - 1), bond$face)
    cashflows <- coupon.paym + fv.paym
    
    # annualised maturities
    dt <- as.numeric(as.Date(maturity) - as.Date(valuation)) / 365
    
    function(r) cashflows * exp(-1 * r * dt)
}

delta.bond <- function(bond, valuation, refdata) {
    # maturities (cashflow dates)
    maturity <- cashflow.dates(as.Date(valuation), as.Date(bond$maturity), bond$freq)
    -as.numeric(maturity - as.Date(val.date)) / 365
}

gamma.bond <- function(bond, valuation, refdata) {
    # maturities (cashflow dates)
    maturity <- cashflow.dates(as.Date(valuation), as.Date(bond$maturity), bond$freq)
    t <- as.numeric(maturity - as.Date(val.date)) / 365
    t * t / 2
}

riskfactors.bond <- function(bond, valuation, refdata) {
    require(lubridate)
    curves <- getcurves(bond, refdata)
    
    # maturities (cashflow dates)
    maturity <- cashflow.dates(as.Date(valuation), as.Date(bond$maturity), bond$freq)
    
    colnames(curves)
    lapply(maturity, function(m) {
        structure(list(tenor = as.period(new_interval(ymd(valuation), m))),
                  class="rf_zero")
    })
}

deltaNormal.bond <- function(bond, valuation, refdata) {
    deltaNormal(defportfolio(bond), valuation, refdata)
}

returns.rf_zero <- function(rf_zero, valuation, refdata, lookback) {
    rs <- history(rf_zero, valuation, refdata, lookback)
    diff(rs)
}

history.rf_zero <- function(rf_zero, valuation, refdata, lookback) {
    require(lubridate)
    
    zcurve <- getcurves(rf_zero, refdata)
    int <- as.interval(lookback, as.Date(valuation) - lookback)
    
    zm <- zero.maturities(colnames(zcurve)[-1], valuation)
    
    # m - date of interest
    m <- as.Date(valuation) + rf_zero$tenor
    # i - next zero index
    i <- min(which(zm > m))
    a <- as.numeric(m - zm[i - 1]) / as.numeric(zm[i] - zm[i - 1])
    
    snapshot <- zcurve[zcurve$Date %within% int, c(i, i + 1)]
    snapshot[, 1] * (1 - a) + snapshot[, 2] * a
}

as.character.rf_zero <- function(rf_zero, ...) {
    paste("Zero:", as.character(rf_zero$tenor))
}
