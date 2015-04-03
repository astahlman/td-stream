library("plyr")
source("load-tweets.r")

make.window <- function(ts, start, end) {
    ts[which(ts$t.bucket > start & ts$t.bucket <= end),]
}

as.ts.amp <- function(df, bucket) {
    df$char.count <- nchar(df$text)
    df$t.bucket <- ceiling(df$timestamp / bucket) * bucket
    ts <- ddply(df, "t.bucket", summarise,
                num.tds = sum(is_td), # TODO: Rename this to "signal"
                mean.len = mean(char.count),
                td.count = num.tds / mean.len)
                
}

threshold <- function(ts, THRESH=3) {
    mean(ts) + (THRESH * sd(ts))
}

detection.with.state <- function(df=NULL,
                                 W=30,
                                 STEP=5000,
                                 THRESH=10) {
    if (is.null(df)) {
        df = load.data()
    }
    
    ts <- as.ts(df, STEP)
    START <- min(ts$t.bucket) + (W * STEP)
    STOP <- max(ts$t.bucket)

    now <- START + (W * STEP)
    state <- "steady"
    timestamp <- c()
    last.alarm.val <- 0
    while(now <= STOP) {
        window <- make.window(ts, now - (W * STEP), now)
        next.point <- make.window(ts, now, now + STEP)
        if (nrow(window) > 1 & nrow(next.point) > 0) {
            alarm <- threshold(window$td.count, THRESH)
            current <- next.point$td.count[[1]]
            if (state == "alarm" & current <= last.alarm.val) {
                state = "steady"
                print(paste("Downshifting:", current, "<", last.alarm.val))
            } else if (state == "steady" &  current >= alarm) {
                state = "alarm"
                last.alarm.val <- alarm
                timestamp <- append(timestamp, now + STEP)
                print(paste("Alarming:", current, ">", alarm))
            }
        }
        now <- now + STEP
    }

    data.frame(timestamp)
}
