defstock <- function(symbol, amount, pos = "long") {
    structure(list(symbol = symbol, amount = amount, pos = pos), class="stock")
}

price.stock <- function(stock, valuation, refdata) {
    stocks <- refdata$stocks()
    
    # short factor
    sfactor <- if (stock$pos == "long") 1 else -1
    
    stocks[stocks$Date == valuation, stock$symbol] * stock$amount * sfactor
}