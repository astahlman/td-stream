library("plyr")

make.window <- function(ts, start, end) {
    ts[which(ts$t.bucket > start & ts$t.bucket <= end),]
}

# TODO: This is duplicated in plot-touchdowns.r
as.ts <- function(df, bucket) {
    df$t.bucket <- ceiling(df$timestamp / bucket) * bucket
    ts <- ddply(df, "t.bucket", summarise, td.count = sum(is_td))
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

make.variables <- function() {
    old.sizes <- seq(from=30000, to=120000, by=30000)
    new.sizes <- c(2500, 5000, 10000)
    step.sizes <- c(1000)
    bucket.sizes <- c(2500, 5000)
    thresholds <- seq(from=6, to=12, by=3)
    expand.grid(old.sizes, new.sizes, step.sizes, bucket.sizes, thresholds)
}

if (!exists("var.combos")) {
    var.combos <- make.variables()
}

make.functions <- function() {
    make.partial <- function(vars) {
        force(vars) # argument evaluation in R is lazy...
        function(df) {
            do.detection(df, vars[[1]], vars[[2]], vars[[3]], vars[[4]], vars[[5]])
       }
    }

    put.in.env <- function(f) {
        i <- 1
        e <- globalenv()
        function(v) {
            #e[[paste("detect.fn.composed.", i, sep="")]] <- f(v)
            e[[paste("detect.fn.with.state", i, sep="")]] <- f(v)
            i <<- i + 1
        }
    }
    apply(var.combos, 1, put.in.env(make.partial))
}

make.functions()

do.detection <- function(df,
                OLD_WINDOW_SIZE=30000,
                NEW_WINDOW_SIZE=5000,
                STEP=2500,
                BUCKET=2500,
                THRESH=3) {
    print(OLD_WINDOW_SIZE)
    print(NEW_WINDOW_SIZE)
    print(STEP)
    print(BUCKET)
    print(THRESH)
    ts <- as.ts(df, BUCKET)
    #STEP <- 2500
    #OLD_WINDOW_SIZE <- 20000
    #NEW_WINDOW_SIZE <- 5000
    START <- min(ts$t.bucket) + OLD_WINDOW_SIZE
    STOP <- max(ts$t.bucket) - NEW_WINDOW_SIZE
    now <- START + OLD_WINDOW_SIZE
    timestamp <- c()
    while(now <= STOP) {
        old.window <- make.window(ts, now - OLD_WINDOW_SIZE, now)
        new.window <- make.window(ts, now, now + NEW_WINDOW_SIZE)
        if (nrow(old.window) > 1 & nrow(new.window) > 0) {
            alarm <- threshold(old.window$td.count)
            if (mean(new.window$td.count) > alarm) {
                timestamp <- append(timestamp, now + NEW_WINDOW_SIZE)
            }
        }
        now <- now + STEP
    }

    data.frame(timestamp)
}


detection.with.state <- function(df,
                                 OLD_WINDOW_SIZE=90000,
                                 NEW_WINDOW_SIZE=10000,
                                 STEP=5000,
                                 BUCKET=5000,
                                 THRESH=10) {
    ts <- as.ts(df, BUCKET)
    START <- min(ts$t.bucket) + OLD_WINDOW_SIZE
    STOP <- max(ts$t.bucket) - NEW_WINDOW_SIZE
    now <- START + OLD_WINDOW_SIZE
    state <- "steady"
    timestamp <- c()
    last.alarm.val <- 0
    while(now <= STOP) {
        old.window <- make.window(ts, now - OLD_WINDOW_SIZE, now)
        new.window <- make.window(ts, now, now + NEW_WINDOW_SIZE)
        if (nrow(old.window) > 1 & nrow(new.window) > 0) {
            current <- mean(new.window$td.count)
            alarm <- threshold(old.window$td.count, THRESH)
            if (state == "alarm" & current <= last.alarm.val) {
                state = "steady"
                print(paste("Downshifting:", current, "<", last.alarm.val))
            } else if (state == "steady" &  current >= alarm) {
                state = "alarm"
                last.alarm.val <- alarm
                timestamp <- append(timestamp, now + NEW_WINDOW_SIZE)
                print(paste("Alarming:", current, ">", alarm))
            }
        }
        now <- now + STEP
    }

    data.frame(timestamp)
}
                                 
detection.with.state.signal.amp <- function(df,
                                 OLD_WINDOW_SIZE=90000,
                                 NEW_WINDOW_SIZE=10000,
                                 STEP=5000,
                                 BUCKET=5000,
                                 THRESH=10) {
    ts <- as.ts.amp(df, BUCKET)
    START <- min(ts$t.bucket) + OLD_WINDOW_SIZE
    STOP <- max(ts$t.bucket) - NEW_WINDOW_SIZE
    now <- START + OLD_WINDOW_SIZE
    state <- "steady"
    timestamp <- c()
    last.alarm.val <- 0
    while(now <= STOP) {
        old.window <- make.window(ts, now - OLD_WINDOW_SIZE, now)
        new.window <- make.window(ts, now, now + NEW_WINDOW_SIZE)
        if (nrow(old.window) > 1 & nrow(new.window) > 0) {
            current <- mean(new.window$td.count)
            alarm <- threshold(old.window$td.count, THRESH)
            if (state == "alarm" & current <= last.alarm.val) {
                state = "steady"
                print(paste("Downshifting:", current, "<", last.alarm.val))
            } else if (state == "steady" &  current >= alarm) {
                state = "alarm"
                last.alarm.val <- alarm
                timestamp <- append(timestamp, now + NEW_WINDOW_SIZE)
                print(paste("Alarming:", current, ">", alarm))
            }
        }
        now <- now + STEP
    }

    data.frame(timestamp)
}
