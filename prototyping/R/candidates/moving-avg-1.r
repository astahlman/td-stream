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
                STEP=5000,
                THRESH=10)
{

    print(W)
    print(STEP)
    print(THRESH)
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
            alarm <- threshold(window$td.count, THRESH)
            if (next.point$td.count[[1]] > alarm) {
                timestamp <- append(timestamp, now + STEP)
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
    vars <- expand.grid(windows, steps, thresholds)
    apply(vars, 1, function(r) {
        r <- as.list(r)
        names(r) <- c("W","STEP","THRESH")
        r
    })
}

get.candidates <- function() {
    fs <- lapply(make.variables(), function(vars) {
        this.f <- do.detection ## capture the current function
        return(list(
            description=paste(
                "Stateless moving avg. with these params:",
                paste(vars, collapse=",")),
            f=function(df) {
                vars[["df"]] <- df
                do.call(this.f, vars)
            }))
    })
    return(fs)
}
