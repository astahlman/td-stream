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

df$is_td <- isTouchdown(df$text)
df <- df[which(df$is_td),]

BUCKET <- 5000 # 5 seconds

df$t.bucket <- trunc(df$time / BUCKET) * BUCKET
df$index <- c(1:dim(df)[1])
ts <- ddply(df, "t.bucket", summarise, td.count = sum(is_td), index = min(index))
png("td-timeseries.png")
ggplot(ts, aes(index, td.count)) + geom_line() + xlab("Time") + ylab("Count") + ggtitle("Tweets containing the word 'touchdown'") + scale_x_continuous(breaks=seq(1,max(df$index), 1000)) + theme(axis.text.x = element_text(angle=90, vjust=.5, size=6))
dev.off()
