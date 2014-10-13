price <- function(sec, ...) UseMethod("price")
# price vector
pricev <- function(sec, ...) UseMethod("pricev")
priceh <- function(sec, ...) UseMethod("priceh")

# returns
returns <- function(sec, ...) UseMethod("returns")
history <- function(sec, ...) UseMethod("history")

# Greeks
delta <- function(sec, ...) UseMethod("delta")
gamma <- function(sec, ...) UseMethod("gamma")

riskfactors <- function(sec, ...) UseMethod("riskfactors")
underlying <- function(sec, ...) UseMethod("underlying")

# VaR Delta-Normal
deltaNormal <- function(sec, ...) UseMethod("deltaNormal")
deltaGammaMC <- function(sec, ...) UseMethod("deltaGammaMC")

# VaR Historical Simulation
historical <- function(sec, ...) UseMethod("historical")
