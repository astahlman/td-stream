library("rjson")
library("plyr")
library("ggplot2")

BUCKET <- 5000

load.data <- function(f) {
    d <- lapply(readLines(json.file), fromJSON)
    df <- data.frame(do.call("rbind", d), stringsAsFactors=F)
    names(df) <- c("time", "text")
    df$time <- as.numeric(df$time)
    
    isTouchdown <- function(text) {
        grepl(pattern="touchdown",
              ignore.case=T,
              x=text)
    }

    df$is_td <- isTouchdown(df$text)
    return(df)
}

make.window <- function(df, start, end) {
    df[which(df$time > start & df$time <= end),]
}

as.ts <- function(df) {
    #df <- df[which(df$is_td),]
    #df$t.bucket <- trunc(df$time / BUCKET)
    df$t.bucket <- ceiling(df$time / BUCKET)
    ts <- ddply(df, "t.bucket", summarise, td.count = sum(is_td))
}

threshold <- function(ts) {
    mean(ts) + (3 * sd(ts))
}

#json_file <- "/tmp/sample-tweets.json"
json.file <- "~/Documents/Programming/ML/td-stream/data/json/tweets.2014-12-15.01.json"
json.file <- "~/Documents/Programming/ML/td-stream/data/json/test-data.json"
#ts <- load.data(json.file)

run <- function(f) {
    df <- load.data(f)
    STEP <- 2500 # 2 seconds in millis.
    WINDOW_SIZE <- 60000
    START <- min(df$time)
    STOP <- max(df$time)
    now <- START
    td.count <- 0
    while(now < STOP - WINDOW_SIZE) {
        old.window <- as.ts(make.window(df, now, now + WINDOW_SIZE - STEP))
        new.window <- as.ts(make.window(df, now + WINDOW_SIZE - STEP, now + WINDOW_SIZE))
        window <- rbind(old.window, new.window)
        alarm <- threshold(old.window$td.count)
        if (max(new.window$td.count) > alarm) {
            td.count <- td.count + 1
            print(paste(max(new.window$td.count), " > ", alarm))
            print(paste("TOUCHDOWN at ", now))
            png(paste("td-graph-", td.count, ".png", sep=""))
            print(ggplot(window, aes(t.bucket, td.count)) + geom_line() + geom_hline(yintercept=alarm))
            dev.off()
         }
        now <- now + STEP
    }
}


