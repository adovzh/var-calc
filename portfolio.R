defportfolio <- function(...) {
    structure(list(...), class="portfolio")
}

price.portfolio <- function(p, valuation, refdata) {
    sum(unlist(lapply(p, function(s) price(s, val.date, refdata))))
}
