curve.profile <- data.frame(
    sheet = c("AUSTRALIA_ZERO_CURVE",
              "EURO_ZERO_CURVE",
              "US_ZERO_CURVE",
              "JAPAN_ZERO_CURVE",
              "UK_ZERO_CURVE"),
    row.names = c("AUD", "EUR", "USD", "JPY", "GBP"),
    stringsAsFactors = FALSE)

read.curve <- function(currency = "AUD") {
#     AUSTRALIA_ZERO_CURVE_FILE <- "AUSTRALIA_ZERO_CURVE.csv"
    curvefile <- paste0(curve.profile[currency, "sheet"], ".csv")
    
    if (file.exists(curvefile)) {
        curve <- read.csv(file = curvefile)
        curve[,1] <- as.Date(as.character(curve[,1]), format = "%Y-%m-%d")
        curve
    } else {
        options(java.parameters = "-Xmx1024m")
        require(xlsx)
        
        XLSX_FILE <- "ASSIGNMENT_DATA_2014.xlsx"
#         AUS_ZERO_CURVE_SHEET <- "AUSTRALIA_ZERO_CURVE"
        curvesheet <- curve.profile[currency, "sheet"]
        curve <- read.xlsx(file = XLSX_FILE, sheetName = curvesheet, 
                           startRow = 2, header = TRUE)
        # remove empty rows and columns
        has.empty.values <- function(x) any(!is.na(x))
        non.empty.rows <- apply(curve, 1, has.empty.values)
        non.empty.columns <- apply(curve, 2, has.empty.values)
        curve <- curve[non.empty.rows, non.empty.columns]
        
        # cache zero curve on disk
        write.csv(format(curve, digits=15), file = curvefile, 
                  row.names = FALSE)
        curve
    }
}

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

price.bond <- function(zcurve, bond, valuation) {
    require(lubridate)
    
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

