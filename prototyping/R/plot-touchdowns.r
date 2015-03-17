library("plyr")
library("ggplot2")

source("load-tweets.r")

td.to.chars <- function(df=NULL) {
    if (is.null(df)) {
        df <- load.data()
    }

    df$char.count <- nchar(df$text)
    ts <- ddply(df, "t.bucket", summarise,
                td.count = sum(is_td),
                mean.len = mean(char.count),
                signal = num.tds / mean.len)
}


td.count <- function(df=NULL) {
    ## lazy-load data so we don't reload when re-sourcing
    ## the file from the REPL
    if (is.null(df)) {
        df <- load.data()
    }

    ddply(df, "t.bucket", summarise, signal = sum(is_td))
}

# print(plot.signal(ts)) to display the graph, png to save to file
plot.signal <- function(ts) {
    plot <- ggplot(ts, aes(t.bucket, signal)) + geom_line() + xlab("Epoch Time") + ylab("Count") + ggtitle("Tweets Containing the Word 'touchdown'") + scale_x_continuous(breaks=seq(from=min(ts$t.bucket), to=max(ts$t.bucket), by=(60000 * 15))) + theme(axis.text.x = element_text(angle=90, vjust=.5, size=6)) # TODO: Relabel this
    return(plot)
}

# TODO: Standardize name for the dataframe with cols "timestamp" and "text"
# TODO: Abstract results parameter, just take a vector of timestamps for vertical lines
# Adds vertical lines wherever for the results
plot.results <- function(df, results) {
    ts <- td.count(df)
    plot <- plot.signal(ts)
    plot <- Reduce(f=function(g1, g2) g1 + g2, init=plot,
                   x=Map(f=function(t) geom_vline(xintercept=t, color="red"),
                           c(results$true.pos$timestamp, results$false.pos$timestamp)))
    return(plot)
}
