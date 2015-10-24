source("load-tweets.r")
source("detect-touchdowns.r")
library("zoo")

teams <- read.csv(file="teams.tsv", sep="\t", stringsAsFactors=F)

matchups <- read.csv(file="/Users/astahlman/Documents/Programming/ML/td-stream/data/wk3-morning-games/fixtures.tsv", sep="\t")[,c("home", "away")]

away.first <- as.list(setNames(
    as.character(matchups$away),
    as.character(matchups$home)))

home.first <- as.list(setNames(
    as.character(matchups$home),
    as.character(matchups$away)))

matchups.map <- append(home.first, away.first)

mad.threshold <- function(ts,
                          old.window.ms=180000,
                          bucket.ms=5000,
                          thresh=6,
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
                         td.timeout=180) {

    tweets <- tweets[which(!tweets$is.retweet),]
    
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

                             r$noisier.team.lag.1 <- lagpad(r$noisier.team, 1)
                             r$noisier.team.lag.2 <- lagpad(r$noisier.team, 2)
                             r$noisier.team.lag.3 <- lagpad(r$noisier.team, 3)
                             
                             r$is.td.home <- r$val.home >= r$thresh.home & r$noisier.team == r$team.home & r$noisier.team.lag.1 == r$team.home #& r$noisier.team.lag.2 == r$team.home & r$noisier.team.lag.3 == r$team.home 
                             r$is.td.away <- r$val.away >= r$thresh.away & r$noisier.team == r$team.away & r$noisier.team.lag.1 == r$team.away #& r$noisier.team.lag.2 == r$team.away & r$noisier.team.lag.3 == r$team.away

                             three.minutes <- 36
                             r$is.td.home <- wipe.subsequent.n(r$is.td.home, three.minutes)
                             r$is.td.away <- wipe.subsequent.n(r$is.td.away, three.minutes)
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
