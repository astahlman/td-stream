library("rjson")
library("plyr")
library("ggplot2")
source("benchmark.r")

load.data <- function(f="../../data/json/tweets.2014-12-15.01.json") {
    d <- lapply(readLines(f), fromJSON)
    df <- data.frame(do.call("rbind", d), stringsAsFactors=F)
    names(df) <- c("timestamp", "text")
    df$timestamp <- as.numeric(df$timestamp)
    
    isTouchdown <- function(text) {
        grepl(pattern="touchdown",
              ignore.case=T,
              x=text)
    }

    df$is_td <- isTouchdown(df$text)
    return(df)
}

source("detect-touchdowns.r")
# detector must retain a data.frame with timestamp and, optionally, players
all.detectors <- ls(all.names=T)[grep("detect.fn", ls(all.names=T))]

# Don't reload every time if we are running this interactively
if (!exists("tweets")) {
    print("Loading tweets...")
    tweets <- load.data()
}

do.profile <- function(detectors=all.detectors) {
    do.run <- function(detector) {
        print(paste("Running", detector))
        get(detector)(tweets)
    }
    do.benchmark <- function(results) benchmark(results, check.players=FALSE)
    scores <- sapply(detectors, function(f) {
        tryCatch(score.benchmark(do.benchmark(do.run(f))),
                 error=function(cond) {
                     message("Failure!")
                     return(-1)
                 })
                 
    })
    #print(which(scores == max(scores)))
    scores
 }

graph.results <- function(df, results) {
    df$t.bucket <- trunc(df$timestamp / BUCKET) * BUCKET
    ts <- ddply(df, "t.bucket", summarise, td.count = sum(is_td))
    #png("td-timeseries.png")
    plot <- ggplot(ts, aes(t.bucket, td.count)) + geom_line() + xlab("Time") + ylab("Count") + ggtitle("Tweets containing the word 'touchdown'") + scale_x_continuous(breaks=seq(from=min(df$t.bucket), to=max(df$t.bucket), by=(60000 * 5))) + theme(axis.text.x = element_text(angle=90, vjust=.5, size=6)) + geom_vline(intercept=results$true.pos$timestamp[[1]])
    print(plot)
    #dev.off()
}


score.benchmark <- function(b) {
    precision <- nrow(b$true.pos) / (nrow(b$true.pos) + nrow(b$false.pos))
    precision <- if (is.nan(precision)) 0 else precision
    recall <- nrow(b$true.pos) / (nrow(b$true.pos) + nrow(b$false.neg))
    (.5 * precision) + (.5 * recall)
}


