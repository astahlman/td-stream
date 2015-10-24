library("rjson")

sample.data <- "../../data/json/raw/tweets.2014-12-15.01.json"
BUCKET.MILLIS = 5000

load.data <- function(f=sample.data, bucket.millis=BUCKET.MILLIS) {
    df <- lapply(readLines(f), fromJSON)
    df <- data.frame(do.call("rbind", df), stringsAsFactors=F)
    names(df)[names(df) == "timestamp_ms"] <- "timestamp"
    df$timestamp <- as.numeric(df$timestamp)
    
    is.touchdown <- function(text) {
        grepl(pattern="touchdown",
              ignore.case=T,
              x=text)
    }

    is.retweet <- function(text) {
        grepl(pattern="\\brt\\b",
              x=text,
              ignore.case=T)
    }
    
    df$t.bucket <- bucketize(df$time)
    df$is_td <- is.touchdown(df$text) # TODO: -> is.td
    df$is.retweet <- is.retweet(df$text)
    return(df)
}

load.all.data <- function(base.dir="../../data/json/raw/", files=NULL) {
    if (is.null(files)) {
        files <- dir(base.dir, full.names=T)
    }
    all.dfs <- lapply(files,
                      function(f) {
                          d <- load.data(f)
                          d$file <- basename(f)
                          return(d)
                      })
    all.dfs <- do.call(rbind, all.dfs)
    all.dfs$file <- as.factor(all.dfs$file)
    return(all.dfs)
}

bucketize <- function(t, bucket.millis=BUCKET.MILLIS) {
    trunc(t / bucket.millis) * bucket.millis
}
