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
        rf <- unique(do.call("c", lapply(p, riskfactors)))
        sigma <- cov(sapply(rf, function(f) returns(f, valuation, refdata, years(2))))
        x <- Reduce("+", lapply(p, function(s) deltarf(s, valuation, refdata)(rf)))
        gamma <- Reduce("+", lapply(p, function(s) gammarf(s, valuation, refdata)(rf)))
        G <- diag(gamma, nrow = length(gamma))
        e <- eigen(sigma)
        C <- e$vectors
        D <- diag(e$values, nrow = length(e$values))
        Q <- sqrt(D) %*% t(C)

        nsim <- 100000
        set.seed(42)
        R <- rnorm(nsim * nrow(sigma))
        dim(R) <- c(nsim, nrow(sigma))
        R <- R %*% Q
        
        # dV distribution
        d <- apply(R, 1, function(r) t(x) %*% r + 0.5 * t(r) %*% G %*% r)
        -quantile(d, 1 - conf, names = FALSE) * sqrt(days)
    }
}