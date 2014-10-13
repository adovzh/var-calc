# generate n multivariate normals ~ N(0, sigma)
mvrnorm <- function(n, sigma, method = "eigen") {
    if (method != "eigen")
        stop("methods other than eigen are not supported yet")

    # First, prepare Q matrix multiplyer
    repeat {
        e <- eigen(sigma)
        C <- e$vectors
        v <- e$values
        m <- min(v)
        if (m <= 0) {
            eps <- 1e-19
            d <- if (abs(m) > eps) abs(m) * 1.01 else eps
            sigma <- sigma + d * diag(length(v))
#             warning(paste("covariance matrix is not positive-definite, corrected by", d))
        } else break
    }
    
    # nrow parameter is very important in case when sigma is a scalar
    D <- diag(v, nrow = length(v))
    Q <- sqrt(D) %*% t(C)
    
    # generate independent normal random variable
    R <- rnorm(n * nrow(sigma))
    
    # reshape matrix
    dim(R) <- c(n, nrow(sigma))
    
    # account for correlation
    R %*% Q
}

# pretty print the message
underlined <- function(s, c) {
    len <- nchar(gsub("\n", "", s))
    u <- paste0(rep(c, len), collapse = "")
    cat(sprintf("%s\n%s\n", s, u))
}
