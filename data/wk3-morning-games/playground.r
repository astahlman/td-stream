library(ggplot2)

plot.signal <- function() {
    prefix <- "/Users/astahlman/Documents/Programming/ML/td-stream/data/wk3-morning-games"
    touchdowns <- read.csv(file=paste(prefix, "touchdowns.tsv", sep="/"), header=TRUE, sep='\t')
    tds.truth <- touchdowns[which(!is.na(touchdowns$timestamp)),]

    signal.files <- lapply(X=c("17","18","19"),
                           FUN=function(h) {
                               paste(prefix, "/default.default.td-signal.csv.2015-09-27.", h,  sep="")
                           })

    signal <- lapply(FUN=function(f) {
                         read.csv(file=f, header=FALSE, sep=',')
                     }, signal.files)
    signal <- Reduce(f=rbind, x=signal)
    names(signal) <- c("timestamp", "signal.val")

    ggplot(data=signal, aes(x=timestamp, y=signal.val)) + geom_line(colour="red") +
        geom_vline(xintercept=tds.truth$timestamp / 1000)
}

## Testing regex per-team approach
plot.team.regex <- function() {
    as.numeric.factor <- function(x) { as.numeric(levels(x))[x] }
    x <- scan(file="/Users/astahlman/Documents/Programming/ML/td-stream/data/wk3-morning-games/raw-tweets.log.2015-09-27.19.json", what=character(), sep="\n")

    tweets <- do.call(rbind.data.frame, lapply(X=x, FUN=fromJSON))
    tweets$text <- as.character(tweets$text)
    tweets$timestamp_ms <- as.numeric.factor(tweets$timestamp_ms)

    start.t <- 1443383893907 - (6 * 60 * 1000)
    end.t <- 1443383960189 + (3 * 60 * 1000)
    noisy.tweets <- tweets[which(tweets$timestamp_ms >= start.t & tweets$timestamp_ms < end.t),]

    jac.terms <- c("jacksonville", "jaguars", "jac", "jags", "jax")

    colts <- unlist(mapply(c("touchdown bengals"), FUN=function(t) { grep(t, tweets$text, ignore.case=T) }), use.names=F)
    times <- tweets[colts,"timestamp_ms"]
    t <- lapply(times, FUN=function(x) { 5000 * (x %/% 5000) })
    t.x <- data.frame(table(unlist(t)))
    names(t.x) <- c("t", "count")
    ggplot(t.x, aes(x=t, y=count, group=1)) + geom_line()
}


load.team.signals <- function() {
    base.dir <- "/Users/astahlman/Documents/Programming/ML/td-stream/data/wk3-morning-games"
    tweet.files <- c("raw-tweets.log.2015-09-27.17.json", "raw-tweets.log.2015-09-27.18.json", "raw-tweets.log.2015-09-27.19.json")
    truth.file <- "truth-morning-only.tsv"
    setwd(base.dir)

    tweets <- load.all.data(files=tweet.files)
    signal <- sapply(X=teams, USE.NAMES=T, simplify=F,
                     FUN=function(team) {
                         df.for.team <- tweets[grep(team, tweets$text, ignore.case=T),]
                         signal <- as.ts.amp(df.for.team, 5000)
                         signal$team <- team
                         return(signal)
                     })
    return(signal)
}
