read.rates <- function() {
    EXCHANGE_RATES_FILE <- "EXCHANGE_RATES.csv"
    
    if (file.exists(EXCHANGE_RATES_FILE)) {
        rates <- read.csv(file = EXCHANGE_RATES_FILE)
        rates[,1] <- as.Date(as.character(rates[,1]), format = "%Y-%m-%d")
        rates
    } else {
        require(xlsx)
        
        XLSX_FILE <- "ASSIGNMENT_DATA_2014.xlsx"
        EXCHANGE_RATES_SHEET <- "EXCHANGE RATES"
        rates <- read.xlsx(file = XLSX_FILE, sheetName = EXCHANGE_RATES_SHEET,
                           startRow = 2, header = FALSE)
        rates <- rates[,-7]
        colnames(rates) <- c("Date", "USD", "EUR", "NZD", "BOT", "GBP", "TR", "JPY")
        
        # cache exchange rates on disk
        write.csv(rates, file = EXCHANGE_RATES_FILE, row.names = FALSE)
        
        rates
    }
}

deffxfwd <- function(currency, amount, fwdrate, maturity) {
    structure(list(currency = currency, amount = amount, fwdrate = fwdrate,
                   maturity = maturity), class = "fxfwd")
}

price.fxfwd <- function(fxfwd, valuation, curves, rates = read.rates()) {
    # current exchange rate
    xrate <- rates[rates$Date == valuation, fxfwd$currency]

    # domestic interest rate
    r <- onDate.rate(curves$AUD, valuation, fxfwd$maturity)

    # foreign interest rate
    rf <- onDate.rate(curves[[fxfwd$currency]], valuation, fxfwd$maturity)
    
    dt <- as.numeric(as.Date(fxfwd$maturity) - as.Date(valuation)) / 365
    
    # express rates in base currency
    S <- 1 / xrate
    K <- 1 / fxfwd$fwdrate
    
    # price
    (S * exp(-rf * dt) - K * exp(-r * dt)) * fxfwd$amount
}

deffxoption <- function(currency, callFlag, pos, amount, strike, maturity) {
    structure(list(currency = currency, callFlag = callFlag, pos = pos,
                   amount = amount, strike = strike, maturity = maturity), 
              class = "fxoption")
}

price.fxoption <- function(fxo, valuation, curves, rates) {
    require(lubridate)
    
    # put factor
    pfactor <- if (fxo$callFlag == "c") 1 else -1
    print(pfactor)
    
    # short factor
    sfactor <- if (fxo$pos == "long") 1 else -1
    print(sfactor)
    
    # estimate number of historic data points in similar period last year
    lyStart <- ymd(val.date) - years(1)
    lyEnd <- ymd(fxo$maturity) - years(1)
    n <- nrow(rates[rates$Date >= as.Date(lyStart) & rates$Date < as.Date(lyEnd), ])
    print(n)
    
    # volatility estimate
    # we do not need to take a reciprocal of exchange rates
    # as it does not have any effect on a vlotility of log returns
    vol <- sd(diff(log(tail(rates, n + 1)[, fxo$currency]))) * sqrt(252)
    vol <- vol * pfactor
    
    print(vol)
    
    S <- 1 / rates[rates$Date == valuation, fxo$currency]
    K <- 1 / fxo$strike
    
    # domestic interest rate
    r <- onDate.rate(curves$AUD, valuation, fxo$maturity)
    
    # foreign interest rate
    rf <- onDate.rate(curves[[fxo$currency]], valuation, fxo$maturity)
    
    dt <- as.numeric(as.Date(fxo$maturity) - as.Date(valuation)) / 365
    
    
    print(S)
    print(K)
    
    d1 <- (log(S / K) + (r - rf + vol^2) * dt) / (vol * sqrt(dt))
    d2 <- d1 - vol * sqrt(dt)
    
    print(d1)
    print(d2)
    p <- (S * exp(-rf * dt) * pnorm(d1) - K * exp(-r * dt) * pnorm(d2))
    p <- p * pfactor * sfactor * fxo$amount
    print(p)
    p
}