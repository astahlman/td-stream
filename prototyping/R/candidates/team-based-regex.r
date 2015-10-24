source("load-tweets.r")
source("detect-touchdowns.r")
library("zoo")

teams <- read.csv(file="teams.tsv", sep="\t", stringsAsFactors=F)$simple.name

do.detections <- function(df=NULL,
                          ts=NULL,
                          OLD_WINDOW_SIZE=90000,
                          NEW_WINDOW_SIZE=10000,
                          STEP=5000,
                          BUCKET=5000,
                          THRESH=10) {
    # Make amplified time-series from dataframe if no time-series is supplied
    if (is.null(ts)) {
        ts <- as.ts.amp(df, BUCKET)
    }
    
    START <- min(ts$t.bucket) + OLD_WINDOW_SIZE
    STOP <- max(ts$t.bucket) - NEW_WINDOW_SIZE
    now <- START + OLD_WINDOW_SIZE
    timestamp <- c()
    while(now <= STOP) {
        old.window <- make.window(ts, now - OLD_WINDOW_SIZE, now)
        new.window <- make.window(ts, now, now + NEW_WINDOW_SIZE)
        if (nrow(old.window) > 1 & nrow(new.window) > 0) {
            current <- mean(new.window$td.count)
            alarm.thresh <- threshold(old.window$td.count, THRESH)
            if (current >= alarm.thresh) {
                timestamp <- append(timestamp, now + NEW_WINDOW_SIZE)
                now <- now + (3 * 60 * 1000) # Set a 3 minute freeze on touchdowns
            }
        }
        now <- now + STEP
    }
    
    return(data.frame(timestamp))
}

mad.threshold <- function(ts,
                           OLD_WINDOW_SIZE=90000) {
    threshold <- 10
    min.thresh <- .05
    ut <- function(x) {
        m = median(x)
        median(x) + threshold * median(abs(x - m))
    }
    
    thresh <- rollapply(zoo(ts), OLD_WINDOW_SIZE / 5000, ut, align="right")
    # Set thresh = NA until at least one window has elapsed
    thresh <- rbind.zoo(rep(NA, times=((OLD_WINDOW_SIZE / 5000) - 1)), thresh)
    # Ensure threshold >= 0.1
    thresh <- as.vector(thresh)
    thresh <- sapply(thresh, FUN=function(x) { ifelse(x < min.thresh, min.thresh, x) })
    return(thresh)
}

## Returns a time-series with columns "t.bucket" and "val"
signal.for.team <- function(tweets, team) {
    bucket.ms <- 5000
    df <- data.frame(tweets[,c("timestamp", "text")])
    df$char.count <- nchar(df$text)
    df$t.bucket <- ceiling(df$timestamp / bucket.ms) * bucket.ms
    df$mentions.team <- FALSE
    #df[grep(team, tweets$text, ignore.case=T), "mentions.team"] <- TRUE
    df$mentions.team <- grepl(team, tweets$text, ignore.case=T)
    ts <- ddply(df, "t.bucket", summarise,
                num.mentions = sum(mentions.team),
                mean.len = mean(char.count),
                val = num.mentions / mean.len)
    return(ts[,c("t.bucket", "val")])
}

team.based.detection <- function(tweets,
                                 OLD_WINDOW_SIZE=90000,
                                 NEW_WINDOW_SIZE=10000,
                                 STEP=5000,
                                 BUCKET=5000,
                                 THRESH=10) {
    all.detections <- lapply(teams, FUN=function(team) {
                                 sig <- signal.for.team(tweets, team)
                                 sig$thresh <- mad.detection(sig$val)

                                 breaches <- sig[which(sig$val > sig$thresh), "t.bucket"]
                                 detections <- c()
                                 if (length(breaches) > 0) {

                                     last.breach <- breaches[[1]]
                                     detections <- c(breaches[[1]])
                                     continue <- T
                                     three.minutes <- 1000 * 60 * 3
                                     while (continue) {
                                         
                                         next.breaches <- Filter(f=function(x) {
                                                                     x > last.breach + three.minutes
                                                                 },
                                                                 breaches)
                                         if (length(next.breaches) > 0) {
                                             last.breach <- next.breaches[[1]]
                                             detections <- append(detections, last.breach)
                                         } else {
                                             continue <- F
                                         }
                                     }
                                 }

                                 if (length(detections) > 0) {
                                     detections <- data.frame("t.bucket"=detections)
                                     detections$team <- team
                                 }
                                 return(detections)
                             })
    do.call("rbind.data.frame", all.detections)
}
