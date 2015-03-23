library("plyr")
source("load-tweets.r")

make.window <- function(ts, start, end) {
    ts[which(ts$t.bucket > start & ts$t.bucket <= end),]
}

remove.rt <- function(tweets) {
    tweets[!grepl("\\brt\\b", tweets$text, ignore.case=T),]
}

as.ts <- function(df, bucket, include.rt=T) {
    df <- if (include.rt) df else remove.rt(df)
    df$t.bucket <- ceiling(df$timestamp / bucket) * bucket
    ts <- ddply(df, "t.bucket", summarise, td.count = sum(is_td))
}

as.ts.amp <- function(df, bucket, include.rt=T) {
    df <- if (include.rt) df else remove.rt(df)
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

do.detection <- function(df=NULL,
                         W=30,
                         STEP=5000,
                         THRESH=10,
                         TS.FUN=as.ts,
                         INCLUDE.RT=T) {
    print(W)
    print(STEP)
    print(THRESH)
    print(deparse(substitute(TS.FUN)))
    print(INCLUDE.RT)
    
    if (is.null(df)) {
        df = load.data()
    }
    
    ts <- TS.FUN(df, STEP, INCLUDE.RT)
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

make.variables <- function() {
    windows <- c(30, 60, 90)
    steps <- c(2500, 5000)
    thresholds <- c(9,12, 15)
    #ts.type <- c(as.ts, as.ts.amp)
    ts.type <- c(as.ts)
    include.rt <- c(T, F)
    vars <- expand.grid(windows, steps, thresholds, ts.type, include.rt)
    apply(vars, 1, function(r) {
        r <- as.list(r)
        names(r) <- c("W","STEP","THRESH","TS.FUN", "INCLUDE.RT")
        r
    })
}

get.candidates <- function() {
    fs <- lapply(make.variables(), function(vars) {
        this.f <- do.detection ## capture the current function
        return(list(
            description=paste(
                "Stateful moving avg. with these params:",
                paste(vars, collapse=",")),
            f=function(df) {
                vars[["df"]] <- df
                do.call(this.f, vars)
            }))
    })
    return(fs)
}
