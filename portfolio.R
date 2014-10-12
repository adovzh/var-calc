defportfolio <- function(...) {
    structure(list(...), class="portfolio")
}

# portfolio generic implementation

price.portfolio <- function(p, valuation, refdata) {
    sum(unlist(lapply(p, function(s) price(s, valuation, refdata))))
}

pricev.portfolio <- function(p, valuation, refdata) {
    unlist(lapply(p, function(s) price(s, valuation, refdata)))
}

priceh.portfolio <- function(p, valuation, refdata, masks) {
    phs <- lapply(p, function(s) priceh(s, valuation, refdata))
    function(r) {
       sum(mapply(function(ph, m) {
           sum(ph(r[m]))
       }, phs, masks))
    }
}

returns.portfolio <- function(p, valuation, refdata, lookback) {
    sapply(p, function(s) returns(s, valuation, refdata, lookback))
}

delta.portfolio <- function(p, valuation, refdata) {
    sapply(p, function(s) delta(s, valuation, refdata))
}

gamma.portfolio <- function(p, valuation, refdata) {
    sapply(p, function(s) gamma(s, valuation, refdata))
}

# not actually a generic
addto.portfolio <- function(p, rf) {
    found <- which(as.logical(sapply(p, function(s) is.same(s, rf))))
    if (length(found) > 0) {
        s <- p[[found]]
        sfactor <- if (s$pos == rf$pos) 1 else -1
        s$amount <- s$amount + sfactor * rf$amount
        if (s$amount < 0) {
            s$amount <- -s$amount
            s$pos <- if (s$pos == "short") "long" else "short"
        }
        p[[found]] <- s
    } else {
        p[[length(p) + 1]] <- rf
    }
    
    p
}

factormap.portfolio <- function(p, valuation, refdata) {
    P <- defportfolio()
    
    for (i in 1:length(p)) {
        rf <- factormap(p[[i]], valuation, refdata)
        
        for (j in 1:length(rf)) 
            P <- addto.portfolio(P, rf[[j]])
    }
    
    P
}

deltaNormal.portfolio <- function(p, valuation, refdata) {
    function(conf, days) {
        require(lubridate)
        
        # list of risk factors of portfolio components
        rfprofile <- lapply(p, function(s) riskfactors(s, valuation, refdata))

        # aggregated list of portfolio risk factors
        rf <- unique(do.call("c", rfprofile))

        # lists of "masks" of portfolio components
        masks <- lapply(rfprofile, function(r) {
            rc <- sapply(r, as.character)
            rfc <- sapply(rf, as.character)
            match(rc, rfc)
        })
        
        # vector X, dollar deltas
        x <- Reduce("+", mapply(function(s, m) {
            drf <- rep(0, length(rf))
            drf[m] <- delta(s, valuation, refdata) * pricev(s, valuation, refdata)
            drf
        }, p, masks, SIMPLIFY = FALSE))
        
        # covariance matrix of risk factors returns
        sigma <- cov(sapply(rf, function(f) returns(f, valuation, refdata, years(2))))
        
        # quantile of the distribution
        qnorm(conf) * sqrt(as.numeric(t(x) %*% sigma %*% x * days))
    }
}

deltaGammaMC.portfolio <- function(p, valuation, refdata) {
    function(conf, days) {
        # list of risk factors of portfolio components
        rfprofile <- lapply(p, function(s) riskfactors(s, valuation, refdata))
        
        # aggregated list of portfolio risk factors
        rf <- unique(do.call("c", rfprofile))
        
        # lists of "masks" of portfolio components
        masks <- lapply(rfprofile, function(r) {
            rc <- sapply(r, as.character)
            rfc <- sapply(rf, as.character)
            match(rc, rfc)
        })
        
        # vector X, dollar deltas
        x <- Reduce("+", mapply(function(s, m) {
            drf <- rep(0, length(rf))
            drf[m] <- delta(s, valuation, refdata) * pricev(s, valuation, refdata)
            drf
        }, p, masks, SIMPLIFY = FALSE))
        
        # covariance matrix of risk factors returns
        sigma <- cov(sapply(rf, function(f) returns(f, valuation, refdata, years(2))))

        gamma <- Reduce("+", mapply(function(s, m) {
            drf <- rep(0, length(rf))
            drf[m] <- gamma(s, valuation, refdata) * pricev(s, valuation, refdata)
            drf
        }, p, masks, SIMPLIFY = FALSE))

        G <- diag(gamma, nrow = length(gamma))
        nsim <- 100000
        set.seed(42)
        R <- mvrnorm(nsim, sigma)

        # dV distribution
        d <- apply(R, 1, function(r) t(x) %*% r + 0.5 * t(r) %*% G %*% r)
        -quantile(d, 1 - conf, names = FALSE) * sqrt(days)
    }
}

historical.portfolio <- function(p, valuation, refdata) {
    function(conf, days) {
        # obtain the value of the portfolio on the valuation date
        pp <- price(p, valuation, refdata)
        
        # list of risk factors of portfolio components
        rfprofile <- lapply(p, function(s) riskfactors(s, valuation, refdata))
        
        # aggregated list of portfolio risk factors
        rf <- unique(do.call("c", rfprofile))
        
        # lists of "masks" of portfolio components
        masks <- lapply(rfprofile, function(r) {
            rc <- sapply(r, as.character)
            rfc <- sapply(rf, as.character)
            match(rc, rfc)
        })
        
        # obtain the history of risk factors
        h <- sapply(rf, function(r) history(r, valuation, refdata, years(2)))
        # returns (1+dr, cur / prev)
        rt <- apply(h, 2, function(x) tail(x, -days) / head(x, -days))
        rfcurrent <- tail(h, n = 1)
        # apply returns to current levels
        adj <- t(apply(rt, 1, function(x) x * rfcurrent))

        # price function for a given vector of risk factors
        ph <- priceh(p, valuation, refdata, masks)

        d <- apply(adj, 1, function(r) {
            ph(r) - pp
        })

        -quantile(d, probs = 1 - conf, type = 4, names = FALSE)
    }
}
