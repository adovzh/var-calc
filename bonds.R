onDate.curve <- function(curve, date) curve[curve[,1] == date, -1]

zero.maturities <- function(curve, valuation) {
    r <- regexec("\\w{2}(\\d{2})Y(\\d{2})", names(curve))
    # names should match
    stopifnot(all(sapply(r, function(x) length(x) > 1)))
    
    z <- mapply(function(name, o) substring(name, o, o + attr(o, "match.length") - 1),
                names(curve), r, USE.NAMES = FALSE)[-1,]
    z <- apply(z, 2, as.list)
    
    do.call("c", lapply(z, function(x) {
        y <- as.numeric(x[[1]])
        m <- as.numeric(x[[2]])
        as.Date(valuation) + years(y) + months(m)
    }))    
}

# returns interpolated value of interest rate
# at the specified date (can be a vector)
# from a given curves data frame
# on a given valuation date
onDate.rate <- function(curves, valuation, date) {
    vcurve <- onDate.curve(curves, valuation)
    zm <- zero.maturities(vcurve, valuation)
    
    sapply(date, function(m) {
        # i - next zero index
        i <- min(which(zm > m))
        m <- as.Date(m)
        a <- as.numeric(m - zm[i - 1]) / as.numeric(zm[i] - zm[i - 1])
        as.numeric(vcurve[i - 1] * (1 - a) + vcurve[i] * a)        
    })
}

cashflow.dates <- function(valuation, maturity) {
    require(lubridate)
    if (maturity < valuation) as.Date(vector())
    else c(cashflow.dates(valuation, maturity - months(6)), maturity)
}

defbond <- function(coupon, maturity, face) {
    structure(list(coupon = coupon, maturity = maturity, face = face), class="bond")
}

price <- function(sec, ...) UseMethod("price")

price.bond <- function(bond, valuation, refdata) {
    require(lubridate)
    
    zcurve <- refdata$curves()$AUD
    
    # maturities (cashflow dates)
    maturity <- cashflow.dates(as.Date(valuation), as.Date(bond$maturity))
    
    # cashflows
    coupon.paym <- rep(bond$face * bond$coupon / 2, length(maturity))
    fv.paym <- c(rep(0, length(maturity) - 1), bond$face)
    cashflows <- coupon.paym + fv.paym
    
    # rates
    v1 <- onDate.rate(zcurve, valuation, maturity)
    
    # annualised maturities
    dt <- as.numeric(as.Date(maturity) - as.Date(valuation)) / 365
    sum(cashflows * exp(-1 * v1 * dt))
}

