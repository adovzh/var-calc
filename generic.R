price <- function(sec, ...) UseMethod("price")
# price vector
pricev <- function(sec, ...) UseMethod("pricev")
priceh <- function(sec, ...) UseMethod("priceh")

# returns
returns <- function(sec, ...) UseMethod("returns")
history <- function(sec, ...) UseMethod("history")

delta <- function(sec, ...) UseMethod("delta")
deltarf <- function(sec, ...) UseMethod("deltarf")

gamma <- function(sec, ...) UseMethod("gamma")
gammarf <- function(sec, ...) UseMethod("gammarf")

# risk factor mapping
factormap <- function(sec, ...) UseMethod("factormap")
is.same <- function(sec, ...) UseMethod("is.same")
riskfactors <- function(sec, ...) UseMethod("riskfactors")
underlying <- function(sec, ...) UseMethod("underlying")

# VaR Delta-Normal
deltaNormal <- function(sec, ...) UseMethod("deltaNormal")
deltaGammaMC <- function(sec, ...) UseMethod("deltaGammaMC")

# VaR Historical Simulation
historical <- function(sec, ...) UseMethod("historical")
