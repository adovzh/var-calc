source("bonds.R")
source("rates.R")

# read curves
curves <- sapply(rownames(curve.profile), read.curve, simplify = FALSE)
# zcurve <- read.curve()
zcurve <- curves$AUD

# curve on valuation date
val.date <- "2014-08-07"

# bonds definitions
b1 <- defbond(coupon = .045, maturity = "2014-10-21", face = 3e7)
b2 <- defbond(coupon = .0625, maturity = "2015-04-15", face = 9e7)
b3 <- defbond(coupon = .0475, maturity = "2016-06-15", face = 7.5e7)
b4 <- defbond(coupon = .06, maturity = "2017-02-15", face = 5e7)
b5 <- defbond(coupon = .055, maturity = "2018-01-21", face = 5e7)

# portfolio
b <- list(b1, b2, b3, b4, b5)
p1 <- sum(unlist(lapply(b, function(bond) price.bond(zcurve, bond, val.date))))

ff1 <- deffxfwd(currency = "JPY", amount = 1.5e9, fwdrate = 92.1, maturity = "2014-11-07")
ff2 <- deffxfwd(currency = "USD", amount = 1.2e8, fwdrate = 0.9315, maturity = "2014-12-08")
ff3 <- deffxfwd(currency = "EUR", amount = 8e7, fwdrate = 0.6915, maturity = "2015-01-15")

rates <- read.rates()
ff <- list(ff1, ff2, ff3)
ffp <- lapply(ff, function(fxfwd) price.fxfwd(fxfwd, val.date, curves, rates))

fo1 <- deffxoption(currency = "USD", callFlag = "c", pos = "short",
                   amount = 6.5e8, strike = 0.9531, maturity = "2014-10-07")
fo2 <- deffxoption(currency = "USD", callFlag = "p", pos = "short",
                   amount = 5e8, strike = 0.925, maturity = "2014-12-08")
fo3 <- deffxoption(currency = "EUR", callFlag = "c", pos = "long",
                   amount = 4.5e8, strike = 0.701, maturity = "2015-04-07")
fo4 <- deffxoption(currency = "EUR", callFlag = "p", pos = "long",
                   amount = 6e8, strike = 0.6709, maturity = "2015-02-06")
fo <- list(fo1, fo2, fo3, fo4)

fop <- lapply(fo, function(fxoption) price.fxoption(fxoption, val.date, curves, rates))
              
