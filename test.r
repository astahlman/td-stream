library("rjson")
library("plyr")

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

df$t_bucket <- as.integer(df$time / BUCKET)
ts <- ddply(df, "t_bucket", summarise, td.count = sum(is_td))
