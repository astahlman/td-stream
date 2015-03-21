library("plyr")
source("load-tweets.r")

make.window <- function(ts, start, end) {
    ts[which(ts$t.bucket > start & ts$t.bucket <= end),]
}

as.ts <- function(df, bucket) {
    df$t.bucket <- ceiling(df$timestamp / bucket) * bucket
    ts <- ddply(df, "t.bucket", summarise, td.count = sum(is_td))
}

threshold <- function(ts, THRESH=3) {
    mean(ts) + (THRESH * sd(ts))
}

do.detection <- function(df=NULL,
                W=30,
                STEP=1000,
                THRESH=3)
{

    if (is.null(df)) {
        df = load.data()
    }
    
    ts <- as.ts(df, STEP)
    START <- min(ts$t.bucket) + (W * STEP)
    STOP <- max(ts$t.bucket)
    now <- START + (W * STEP)
    timestamp <- c()
    while(now <= STOP) {
        window <- make.window(ts, now - (W * STEP), now)
        next.point <- make.window(ts, now, now + STEP)
        if (nrow(window) > 1 & nrow(next.point) > 0) {
            alarm <- threshold(window$td.count)
            if (next.point$td.count[[1]] > alarm) {
                timestamp <- append(timestamp, now + STEP)
            }
        }
        now <- now + STEP
    }

    data.frame(timestamp)
}
