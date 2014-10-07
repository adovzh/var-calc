price <- function(sec, ...) UseMethod("price")
# price vector
pricev <- function(sec, ...) UseMethod("pricev")

# returns
returns <- function(sec, ...) UseMethod("returns")

delta <- function(sec, ...) UseMethod("delta")

gamma <- function(sec, ...) UseMethod("gamma")

# risk factor mapping
factormap <- function(sec, ...) UseMethod("factormap")
is.same <- function(sec, ...) UseMethod("is.same")

# VaR Delta-Normal
deltaNormal <- function(sec, ...) UseMethod("deltaNormal")