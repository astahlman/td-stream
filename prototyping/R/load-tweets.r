library("rjson")

sample.data <- "../../data/json/tweets.2014-12-15.01.json"

load.data <- function(f=sample.data, bucket.millis=5000) {
    df <- lapply(readLines(f), fromJSON)
    df <- data.frame(do.call("rbind", df), stringsAsFactors=F)
    names(df) <- c("timestamp", "text")
    df$timestamp <- as.numeric(df$timestamp)
    
    is.touchdown <- function(text) {
        grepl(pattern="touchdown",
              ignore.case=T,
              x=text)
    }
    
    df$t.bucket <- trunc(df$time / bucket.millis) * bucket.millis
    df$is_td <- is.touchdown(df$text) # TODO: -> is.td
    return(df)
}
