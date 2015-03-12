library("rjson")
library("plyr")
library("ggplot2")

json_file <- "~/Documents/Programming/ML/td-stream/data/json/tweets.2014-12-15.01.json"
#json_file <- "/tmp/sample-tweets.json"
d <- lapply(readLines(json_file), fromJSON)
df <- data.frame(do.call("rbind", d), stringsAsFactors=F)
names(df) <- c("time", "text")
df$time <- as.numeric(df$time)
            
isTouchdown <- function(text) {
    grepl(pattern="touchdown",
                  ignore.case=T,
                  x=text)
}

BUCKET <- 5000 # 5 seconds
df$t.bucket <- trunc(df$time / BUCKET) * BUCKET
df$is_td <- isTouchdown(df$text)
df$char.count <- nchar(df$text)
df$t.bucket <- ceiling(df$timestamp / bucket) * bucket

ts <- ddply(df, "t.bucket", summarise,
            num.tds = sum(is_td), # TODO: Rename this to "signal"
            mean.len = mean(char.count),
            td.count = num.tds / mean.len)
#png("td-timeseries.png")
plot <- ggplot(ts, aes(index, td.count)) + geom_line() + xlab("Time") + ylab("Count") + ggtitle("Tweets containing the word 'touchdown'") + scale_x_continuous(breaks=seq(1,max(df$index), 1000)) + theme(axis.text.x = element_text(angle=90, vjust=.5, size=6))
plot <- 
#dev.off()

    
graph <- function(df) {
    df$t.bucket <- trunc(df$timestamp / BUCKET) * BUCKET
    ts <- ddply(df, "t.bucket", summarise, td.count = sum(is_td))
    #png("td-timeseries.png")
    plot <- ggplot(ts, aes(t.bucket, td.count)) + geom_line() + xlab("Time") + ylab("Count") + ggtitle("Tweets containing the word 'touchdown'") + scale_x_continuous(breaks=seq(from=min(df$t.bucket), to=max(df$t.bucket), by=(60000 * 5))) + theme(axis.text.x = element_text(angle=90, vjust=.5, size=6))
    print(plot)
    #dev.off()
}
