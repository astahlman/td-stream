source("load-tweets.r")
source("detect-touchdowns.r")
library("zoo")

teams <- read.csv(file="teams.tsv", sep="\t", stringsAsFactors=F)

matchups <- read.csv(file="/Users/astahlman/Documents/Programming/ML/td-stream/data/wk3-morning-games/fixtures.tsv", sep="\t")[,c("home", "away")]

mad.threshold <- function(ts,
                          old.window.ms=180000,
                          bucket.ms=5000,
                          threshold=6,
                          min.thresh=.05) {
    ut <- function(x) {
        m = median(x)
        median(x) + threshold * median(abs(x - m))
    }
    
    thresh <- rollapply(zoo(ts), old.window.ms / bucket.ms, ut, align="right")
    
    # Set thresh = NA until at least one window has elapsed
    thresh <- rbind.zoo(rep(NA, times=((old.window.ms / bucket.ms) - 1)), thresh)
    
    # Ensure threshold >= 0.1
    thresh <- as.vector(thresh)
    thresh <- sapply(thresh, FUN=function(x) { ifelse(x < min.thresh, min.thresh, x) })
    return(thresh)
}

## Returns a time-series with columns "t.bucket" and "val"
signal.for.team <- function(tweets,
                            team,
                            bucket.ms=5000) {
    df <- data.frame(tweets[,c("timestamp", "text")])
    df$char.count <- nchar(df$text)
    df$t.bucket <- ceiling(df$timestamp / bucket.ms) * bucket.ms
    df$mentions.team <- grepl(team, tweets$text, ignore.case=T)
    ts <- ddply(df, "t.bucket", summarise,
                num.mentions = sum(mentions.team),
                mean.len = mean(char.count),
                val = num.mentions / mean.len)
    return(ts[,c("t.bucket", "val")])
}

# Look for the first True, then set the next n to False, recursively
wipe.subsequent.n <- function(xs, n) {
    if (length(which(xs)) == 0) {
        return(xs)
    }

    first.t <- which(xs)[[1]]
    remaining <- length(xs) - (first.t + n)
    
    if (remaining > 0) {
        rest <- xs[first.t + n + 1:remaining]
        return(c(xs[1:first.t],
                 rep(F, times=n),
                 wipe.subsequent.n(rest, n)))
                 
    } else {
        return(c(xs[1:first.t],
                 rep(F, times=length(xs) - first.t)))
    }
}

lagpad <- function(x, k) {
    c(rep(NA, k), x)[1 : length(x)] 
}

df.detection <- function(tweets,
                         window.ms=180000,
                         thresh.mult=6,
                         min.thresh=.05,
                         bucket.ms=5000,
                         num.lags=1, # == two buckets where val > thresh
                         td.timeout.ms=180000) {

    tweets <- tweets[which(!tweets$is.retweet),] # don't use retweets
    
    abbrev.for.team <- function(name) { teams[which(teams$simple.name == name), "abbrev"] }

    make.signal <- function(team) {
        df <- signal.for.team(tweets, team, bucket.ms)
        df$team <- abbrev.for.team(team)
        df$thresh <- mad.threshold(df$val,
                                   window.ms,
                                   bucket.ms,
                                   thresh.mult,
                                   min.thresh)
        return(df)
    }
    
    signals <- sapply(teams$simple.name, make.signal, simplify=F)
    names(signals) <- teams$abbrev

    h2h.signals <- apply(matchups, 1, function(matchup) {
                             home <- as.character(matchup[[1]])
                             away <- as.character(matchup[[2]])
                             r <- merge(signals[[home]], signals[[away]], by="t.bucket")
                             names(r) <- c("t.bucket",
                                           "val.home",
                                           "team.home",
                                           "thresh.home",
                                           "val.away",
                                           "team.away",
                                           "thresh.away")

                             r$noisier.team <- NA
                             r[which(r$val.home > r$val.away), "noisier.team"] <- r$team.home[[1]]
                             r[which(r$val.away > r$val.home), "noisier.team"] <- r$team.away[[1]]


                             for (i in 1:num.lags) {
                                 r[[paste("noisier.team.lag.", i, sep="")]] <- lagpad(r$noisier.team, i)
                             }
                         
                             r$is.td.home <- r$val.home >= r$thresh.home & r$noisier.team == home
                             r$is.td.away <- r$val.away >= r$thresh.away & r$noisier.team == away
                             
                             lag.indices <- which(names(r) %in% sapply(1:num.lags,
                                                                       function(i) {
                                                                           paste("noisier.team.lag.", i, sep="")
                                                                       }))

                             r$is.td.home <- r$is.td.home &
                                 apply(r, 1,
                                       function(row) {
                                           Reduce(`&`, row[lag.indices] == home)
                                       })


                             r$is.td.away <- r$is.td.away &
                                 apply(r, 1,
                                       function(row) {
                                           Reduce(`&`, row[lag.indices] == away)
                                       })

                             buckets.to.wipe <- ceiling(td.timeout.ms / bucket.ms)
                             r$is.td.home <- wipe.subsequent.n(r$is.td.home, buckets.to.wipe)
                             r$is.td.away <- wipe.subsequent.n(r$is.td.away, buckets.to.wipe)
                             return(r)
                         })

    ## merge and return data frame with columns: timestamp, team
    result <- do.call(rbind.data.frame,
                      lapply(h2h.signals, function(m) {
                                 m[which(m$is.td.home | m$is.td.away), ]
                             }))
    
    result$team <- ifelse(result$is.td.home, result$team.home, result$team.away)
    result <- result[,c("t.bucket", "team")]
    names(result) <- c("timestamp", "team")
    return(list(detections=result, signals=h2h.signals))
}

make.variables <- function() {
    window.ms <- c(90, 120, 180) * 1000
    bucket.ms <- c(2500, 5000, 10000)
    thresh.mult <- c(3, 6, 9)
    min.thresh <- c(0.03, 0.06, 0.09)
    num.lags <- 1:2
    vars <- expand.grid(window.ms, bucket.ms, thresh.mult, min.thresh, num.lags)
    apply(vars, 1, function(r) {
        r <- as.list(r)
        names(r) <- c("window.ms", "bucket.ms", "thresh.mult", "min.thresh", "num.lags")
        r
    })
}

get.candidates <- function() {
    fs <- lapply(make.variables(), function(vars) {
        this.f <- df.detection ## capture the current function
        return(list(
            description=paste(
                "Opponent away team based regex with these params:",
                paste(vars, collapse=",")),
            f=function(tweets) {
                vars[["tweets"]] <- tweets
                do.call(this.f, vars)
            }))
    })
    return(fs)
}
