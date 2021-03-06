source("common.R")
source("generic.R")
source("refdata.R")
source("bonds.R")
source("rates.R")
source("stock.R")
source("portfolio.R")
source("swaps.R")

install.required("lubridate", "xlsx", "xtable")

# refdata
refdata <- get.refdata()
dummy <- refdata$preload()

# valuation date
val.date <- "2014-08-07"

# bonds definitions
b1 <- defbond(coupon = .045, maturity = "2014-10-21", face = 3e7)
b2 <- defbond(coupon = .0625, maturity = "2015-04-15", face = 9e7)
b3 <- defbond(coupon = .0475, maturity = "2016-06-15", face = 7.5e7)
b4 <- defbond(coupon = .06, maturity = "2017-02-15", face = 5e7)
b5 <- defbond(coupon = .055, maturity = "2018-01-21", face = 5e7)

# portfolio
p1 <- defportfolio(b1, b2, b3, b4, b5)

# spot fx
fx1 <- deffxspot(currency = "USD", position = -9e7, "2014-08-07", refdata)
fx2 <- deffxspot(currency = "EUR", position = 6e7, "2014-08-07", refdata)
fx3 <- deffxspot(currency = "GBP", position = 7e7, "2014-08-07", refdata)
fx4 <- deffxspot(currency = "NZD", position = 4e7, "2014-08-07", refdata)
fx5 <- deffxspot(currency = "IR", position = -7e7, "2014-08-07", refdata)
fx6 <- deffxspot(currency = "JPY", position = -6e7, "2014-08-07", refdata)

p2 <- defportfolio(fx1, fx2, fx3, fx4, fx5, fx6)

# currency options and currency forwards
fo1 <- deffxoption(currency = "USD", callFlag = "c", pos = "short",
                   amount = 6.5e8, strike = 0.9531, maturity = "2014-10-07")
fo2 <- deffxoption(currency = "USD", callFlag = "p", pos = "short",
                   amount = 5e8, strike = 0.925, maturity = "2014-12-08")
fo3 <- deffxoption(currency = "EUR", callFlag = "c", pos = "long",
                   amount = 4.5e8, strike = 0.701, maturity = "2015-04-07")
fo4 <- deffxoption(currency = "EUR", callFlag = "p", pos = "long",
                   amount = 6e8, strike = 0.6709, maturity = "2015-02-06")

ff1 <- deffxfwd(currency = "JPY", amount = 1.5e9, fwdrate = 92.1, maturity = "2014-11-07")
ff2 <- deffxfwd(currency = "USD", amount = 1.2e8, fwdrate = 0.9315, maturity = "2014-12-08")
ff3 <- deffxfwd(currency = "EUR", amount = 8e7, fwdrate = 0.6915, maturity = "2015-01-15")

p3 <- defportfolio(fo1, fo2, fo3, fo4, ff1, ff2, ff3)

# portfolio 4
s1 <- defstock(symbol = "CBA", amount = 8e4, pos = "short")
s2 <- defstock(symbol = "ANZ", amount = 1.2e5, pos = "long")
s3 <- defstock(symbol = "RIO", amount = 1e5, pos = "long")
s4 <- defstock(symbol = "NCM", amount = 3e5, pos = "short")
s5 <- defstock(symbol = "WPL", amount = 9e4, pos = "long")
s6 <- defstock(symbol = "TLS", amount = 3e5, pos = "long")

o1 <- defoption(symbol = "BHP", callFlag = "p", pos = "long", amount = 5.5e5,
                strike = 38, maturity = "2014-10-07", vol = 0.2453)
o2 <- defoption(symbol = "RIO", callFlag = "p", pos = "long", amount = 2e5,
                strike = 63, maturity = "2015-02-08", vol = 0.2949)
o3 <- defoption(symbol = "RIO", callFlag = "c", pos = "long", amount = 2e5,
                strike = 63, maturity = "2015-01-08", vol = 0.2949)
o4 <- defoption(symbol = "NCM", callFlag = "p", pos = "short", amount = 3e5,
                strike = 8.8, maturity = "2015-03-06", vol = 0.41)
o5 <- defoption(symbol = "NCM", callFlag = "c", pos = "short", amount = 2.5e5,
                strike = 13, maturity = "2015-04-06", vol = 0.435)
o6 <- defoption(symbol = "WPL", callFlag = "p", pos = "long", amount = 2e5,
                strike = 37, maturity = "2015-06-08", vol = 0.2722)

p4 <- defportfolio(s1, s2, s3, s4, s5, s6, o1, o2, o3, o4, o5, o6)
# p4p <- price(p4, val.date, refdata)

sw1 <- defswap(rate = 3.2e-2, freq = 2, pos = "long", maturity = "2014-11-07",
               notional = 2e7)
sw2 <- defswap(rate = 3.15e-2, freq = 4, pos = "short", maturity = "2015-08-07",
               notional = 8e7)
sw3 <- defswap(rate = 3.6e-2, freq = 4, pos = "long", maturity = "2015-11-06",
               notional = 7e7)

p5 <- defportfolio(sw1, sw2, sw3)

# Mark-to-Market
all.portfolios <- list("Portfolio 1" = p1, 
                       "Portfolio 2" = p2,
                       "Portfolio 3" = p3,
                       "Portfolio 4" = p4,
                       "Portfolio 5" = p5)

mtm <- sapply(all.portfolios, function(p) price(p, val.date, refdata))

for (i in seq_along(all.portfolios)) {
#     pprice <- price(all.portfolios[[i]], val.date, refdata)
    pprice <- mtm[i]
    assign(sprintf("p%dp", i), pprice)
    underlined(sprintf("\n%s Mark-to-Market: %.2f", names(all.portfolios)[i], pprice), "=")
}

dfmt <- function(x) {
    fmt <- paste0("$", formatC(abs(x), format = "f", big.mark=",", digits = 2))
    if (x < 0) fmt <- paste0("-", fmt)
    else fmt
}

portfolios <- all.portfolios
methods <- list("deltaNormal", "deltaGammaMC", "historical")
dir.create("report", showWarnings = FALSE)

require(xtable)

pricetable <- data.frame(Price = sapply(mtm, dfmt))
print(xtable(pricetable, caption = "MtM", label = "table:pricex", 
             align="l|r"), 
      file = "report/pricex.tex", include.colnames = FALSE)

for (pname in names(portfolios)) {
    cat(sprintf("Processing %s...\n", pname))
    conf <- rep(c(.95, .99), times = 2)
    days <- rep(c(1, 10), each = 2)
    cols <- mapply(function(c, d) {
        cs <- sprintf("%d%%", c * 100)
        ds <- sprintf("%d", d)
        c(cs, ds, lapply(methods, function(m) {
            VaR <- match.fun(m)
            dfmt(VaR(portfolios[[pname]], val.date, refdata)(c, d))
        }))
    }, conf, days, SIMPLIFY = FALSE)
    ptable <- data.frame(Reduce(cbind, cols))
    rownames(ptable) <- c("Confidence Level", "Holding Perdiod", methods)
    label <- paste0("table:", substr(pname, 1, 1), 
                    substr(pname, nchar(pname), nchar(pname)))
    print(xtable(ptable, caption=pname, label = label, align = c("l|cccc")), 
          file=sprintf("report/%s.tex", pname),
          include.colnames = FALSE, hline.after = c(-1, 1, 2, 5), scalebox = .9)
    print(ptable)
}
