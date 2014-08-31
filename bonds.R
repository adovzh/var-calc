read.curve <- function() {
    AUSTRALIA_ZERO_CURVE_FILE <- "AUSTRALIA_ZERO_CURVE.csv"
    
    if (file.exists(AUSTRALIA_ZERO_CURVE_FILE)) {
        curve <- read.csv(file = AUSTRALIA_ZERO_CURVE_FILE)
        curve[,1] <- as.Date(as.character(curve[,1]), format = "%Y-%m-%d")
        curve
    } else {
        require(xlsx)
        
        XLSX_FILE <- "ASSIGNMENT_DATA_2014.xlsx"
        AUS_ZERO_CURVE_SHEET <- "AUSTRALIA_ZERO_CURVE"
        curve <- read.xlsx(file = XLSX_FILE, sheetName = AUS_ZERO_CURVE_SHEET, 
                           startRow = 2, header = TRUE)
        # remove empty rows and columns
        has.empty.values <- function(x) any(!is.na(x))
        non.empty.rows <- apply(curve, 1, has.empty.values)
        non.empty.columns <- apply(curve, 2, has.empty.values)
        curve <- curve[non.empty.rows, non.empty.columns]
        
        # cache zero curve on disk
        write.csv(format(curve, digits=15), file = AUSTRALIA_ZERO_CURVE_FILE, 
                  row.names = FALSE)
        curve
    }
}

zcurve <- read.curve()

cashflow.dates <- function(valuation, maturity) {
    require(lubridate)
    if (maturity < valuation) as.Date(vector())
    else c(cashflow.dates(valuation, maturity - months(6)), maturity)
}

# maturities <- c("2014-10-21", "2015-04-15", "2016-06-15", 
#                 "2017-02-15", "2018-01-21")
# cashflows <- lapply(maturities, 
#                     function(x) 
#                         cashflow.dates(as.Date("2014-08-07"), as.Date(x)))
# # curve on valuation date
val.date <- "2014-08-07"
# vcurve <- zcurve[zcurve[,1] == val.date, -1]

defbond <- function(coupon, maturity, face) {
    structure(list(coupon = coupon, maturity = maturity, face = face), class="bond")
}

price.bond <- function(zcurve, bond, valuation) {
    require(lubridate)
    vcurve <- zcurve[zcurve[,1] == valuation, -1]

    r <- regexec("AU(\\d{2})Y(\\d{2})", names(vcurve))
    # names should match
    stopifnot(all(sapply(r, function(x) length(x) > 1)))

    z <- mapply(function(name, o) substring(name, o, o + attr(o, "match.length") - 1),
           names(vcurve), r, USE.NAMES = FALSE)[-1,]
    z <- apply(z, 2, as.list)
    
    # zm - zero maturities
    zm <- do.call("c", lapply(z, function(x) {
        y <- as.numeric(x[[1]])
        m <- as.numeric(x[[2]])
        as.Date(valuation) + years(y) + months(m)
    }))
    
    # maturities
    maturity <- cashflow.dates(as.Date(valuation), as.Date(bond$maturity))
    
    # cashflows
    coupon.paym <- rep(bond$face * bond$coupon / 2, length(maturity))
    fv.paym <- c(rep(0, length(maturity) - 1), bond$face)
    cashflows <- coupon.paym + fv.paym
#     print(cashflows)
    
    # rates
    v1 <- sapply(maturity, function(m) {
        # i - next zero index
        i <- min(which(zm > m))
        m <- as.Date(m)
        a <- as.numeric(m - zm[i - 1]) / as.numeric(zm[i] - zm[i - 1])
        as.numeric(vcurve[i - 1] * (1 - a) + vcurve[i] * a)
    })
    
    # annualised maturities
    v2 <- as.numeric(as.Date(maturity) - as.Date(valuation)) / 365
    sum(cashflows * exp(-1 * v1 * v2))
}

# bonds definitions
b1 <- defbond(coupon = .045, maturity = "2014-10-21", face = 3e7)
b2 <- defbond(coupon = .0625, maturity = "2015-04-15", face = 9e7)
b3 <- defbond(coupon = .0475, maturity = "2016-06-15", face = 7.5e7)
b4 <- defbond(coupon = .06, maturity = "2017-02-15", face = 5e7)
b5 <- defbond(coupon = .055, maturity = "2018-01-21", face = 5e7)

# portfolio
b <- list(b1, b2, b3, b4, b5)
p1 <- sum(unlist(lapply(b, function(bond) price.bond(zcurve, bond, val.date))))
