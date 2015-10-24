source("benchmark.r")
source("load-tweets.r")
library("plyr")
require("zoo")

all.files <- dir("../../data/json/raw", full.names=T)
##all.files <- dir("/tmp/test", full.names=T)
all.data <- do.call(rbind, Map(load.data, all.files))
all.data <- all.data[,names(all.data) %in% c("t.bucket", "is_td", "is.retweet")]
all.data <- ddply(all.data, c("t.bucket"), summarise,
                  td.sum = sum(is_td),
                  rt.sum = sum(is.retweet))

bucket.millis <- 5000
rounded.tds <- all.touchdowns
rounded.tds$t.bucket <- trunc(all.touchdowns$timestamp / bucket.millis) * bucket.millis
all.data$is.td <- all.data$t.bucket %in% rounded.tds$t.bucket

in.last <- function(l, n) {
    m <- Map(function(s) seq(from=s+1, to=min(s+n+1, length(l))), which(l))
    i <- unlist(m)
    l[i] <- TRUE
    return(l)
}

## last.n <- function(x, n) {
##     seq(from=max(1, x - n), to=max(1, x - 1))
## }

## all.data$last.30.mean <- filter(
##     all.data$td.sum, rep(1/(30000 / bucket.millis), (30000 / bucket.millis)),
##     sides=1)

lags <- c(30, 60, 90)
lag.title <- function(n) { paste("in.last.", n, sep="") }
all.data[,sapply(lags, lag.title)] <- Map(function(n) in.last(all.data$is.td, n),
                                          lags / (bucket.millis / 1000))
all.data$lagged.2 <- lag(zoo(all.data$td.sum), -2, na.pad=T)
all.data$lagged.4 <- lag(zoo(all.data$td.sum), -4, na.pad=T)
all.data$lagged.8 <- lag(zoo(all.data$td.sum), -8, na.pad=T)

write.csv(file="../../data/all-data-2.csv", x=all.data, row.names=F)


