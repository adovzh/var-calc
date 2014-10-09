curve.profile <- data.frame(
    sheet = c("AUSTRALIA_ZERO_CURVE",
              "EURO_ZERO_CURVE",
              "US_ZERO_CURVE",
              "JAPAN_ZERO_CURVE",
              "UK_ZERO_CURVE"),
    row.names = c("AUD", "EUR", "USD", "JPY", "GBP"),
    stringsAsFactors = FALSE)

# remove empty rows and columns
clean.data <- function(data) {
    has.empty.values <- function(x) any(!is.na(x))
    non.empty.rows <- apply(data, 1, has.empty.values)
    non.empty.columns <- apply(data, 2, has.empty.values)
    data[non.empty.rows, non.empty.columns]    
}

# tarnsform curves column names
curves.cnames <- function(cnames) {
    r <- regexec("\\w{2}(\\d{2})Y(\\d{2})", cnames)
    # names should match
    stopifnot(all(sapply(r, function(x) length(x) > 1)))
    
    z <- mapply(function(name, o) substring(name, o, o + attr(o, "match.length") - 1),
                cnames, r, USE.NAMES = FALSE)[-1,]
    z <- apply(z, 2, as.list)
    
    do.call("c", lapply(z, function(x) {
        y <- as.numeric(x[[1]])
        m <- as.numeric(x[[2]])
        sprintf("Y%02dM%02d", y, m)
    }))        
}

# transform swap data column names
swaps.cnames <- function(cnames) {
    r <- regexec("\\w+(\\d)([MY])", cnames)
    # names should match
    stopifnot(all(sapply(r, function(x) length(x) > 1)))

    z <- mapply(function(name, o) substring(name, o, o + attr(o, "match.length") - 1),
                cnames, r, USE.NAMES = FALSE)[-1,]
    z <- apply(z, 2, as.list)
    
    do.call("c", lapply(z, function(x) {
        n <- as.numeric(x[[1]])
        d <- x[[2]]
        
        if (d == "M") sprintf("Y00M%02d", n)
        else if (d == "Y") sprintf("Y%02dM00", n)
        else stop("upsupported duration character: ", d)
    }))
}

cnames.as.period <- function(cnames) {
    r <- regexec("Y(\\d{2})M(\\d{2})", cnames)
    # names should match
    stopifnot(all(sapply(r, function(x) length(x) > 1)))
    
    z <- mapply(function(name, o) substring(name, o, o + attr(o, "match.length") - 1),
                cnames, r, USE.NAMES = FALSE)[-1,]
    z <- apply(z, 2, as.list)
    
    lapply(z, function(cn) {
       y <- as.numeric(cn[1])
       m <- as.numeric(cn[2])
       years(y) + months(m)
   }) 
}

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
        curve <- clean.data(curve)
        
        # transform column names
        names(curve) <- c("Date", curves.cnames(names(curve)[-1]))
        
        # cache zero curve on disk
        write.csv(format(curve, digits=15), file = curvefile, 
                  row.names = FALSE)
        curve
    }
}

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
        colnames(rates) <- c("Date", "USD", "EUR", "NZD", "BOT", "GBP", "IR", "JPY")
        
        # cache exchange rates on disk
        write.csv(rates, file = EXCHANGE_RATES_FILE, row.names = FALSE)
        
        rates
    }
}

read.stocks <- function() {
    STOCK_PRICES_FILE <- "STOCK_PRICES.csv"
    
    if (file.exists(STOCK_PRICES_FILE)) {
        stock <- read.csv(file = STOCK_PRICES_FILE)
        stock[,1] <- as.Date(as.character(stock[,1]), format = "%Y-%m-%d")
        stock        
    } else {
        options(java.parameters = "-Xmx1024m")
        require(xlsx)
        
        XLSX_FILE <- "ASSIGNMENT_DATA_2014.xlsx"
        STOCK_PRICES_SHEET  <- "STOCK PRICES"
        stock <- read.xlsx(file = XLSX_FILE, sheetName = STOCK_PRICES_SHEET,
                           startRow = 2, header = TRUE)
        
        # remove empty rows and columns
        stock <- clean.data(stock)
        
        # fix names
        names <- gsub("\\.", "", names(stock))
        names[1] <- "Date"
        names(stock) <- names
        
        # cache stock prices on disk
        write.csv(format(stock, digits=15), file = STOCK_PRICES_FILE, 
                  row.names = FALSE)
        
        stock
    }
}

read.bbsw <- function() {
    BBSW_RATES_FILE <- "BBSW_RATES.csv"
        
    if (file.exists(BBSW_RATES_FILE)) {
        bbsw <- read.csv(file = BBSW_RATES_FILE)
        bbsw[,1] <- as.Date(as.character(bbsw[,1]), format = "%Y-%m-%d")
        bbsw
    } else {
        options(java.parameters = "-Xmx1024m")
        require(xlsx)
        
        XLSX_FILE <- "ASSIGNMENT_DATA_2014.xlsx"
        BBSW_RATES_SHEET  <- "Interest Rate Swap Data"
        bbsw <- read.xlsx(file = XLSX_FILE, sheetName = BBSW_RATES_SHEET, 
                          startRow = 2, header = TRUE)
        # remove empty rows and columns
        bbsw <- clean.data(bbsw)
        
        # transform column names
        names(bbsw) <- c("Date", swaps.cnames(names(bbsw)[-1]))
        
        # cache zero curve on disk
        write.csv(format(bbsw, digits=15), file = BBSW_RATES_FILE, 
                  row.names = FALSE)
        bbsw
    }
}

get.refdata <- function(x = list()) {
    curves <- function() {
        if (is.null(x$curves)) {
            print("Loading curves")
            x$curves <<- sapply(rownames(curve.profile), read.curve, simplify = FALSE)
        }
        x$curves
    }
    rates <- function() {
        if (is.null(x$rates)) {
            print("Loading rates")
            x$rates <<- read.rates()
        }
        x$rates
    }
    stocks <- function() {
        if (is.null(x$stocks)) {
            print("Loading stocks")
            x$stocks <<- read.stocks()
        }
        x$stocks
    }
    swaps <- function() {
        if (is.null(x$swaps)) {
            print("Loading swap rates")
            x$swaps <<- read.bbsw()
        }
        x$swaps
    }
    list(curves = curves, rates = rates, stocks = stocks, swaps = swaps)
}
