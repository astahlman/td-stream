## Ground truth from sample data - tweets.2014-12-15.01.json
expected <- data.frame(
    rbind(
        c(1418607396778,"demarco murray"),
        c(1418608620457,"dez bryant"),
        c(1418609918817,"dez bryant"),
        c(1418610531857,"chris polk"),
        c(1418614014826,"chris polk"),
        c(1418614584563,"darren sproles"),
        c(1418615124627,"demarco murray"),
        c(1418615763277,"dez bryant")),
    stringsAsFactors=FALSE)
colnames(expected) <- c("timestamp", "player")
expected$timestamp <- as.numeric(as.character(expected$timestamp))

benchmark <- function(actual, exp=expected, check.players=F) {

    checked.cols <- if (check.players) c("timestamp", "player") else c("timestamp")

    if (length(actual) == 0) {
        return(list("true.pos"=data.frame(),
                    "false.neg"=exp,
                    "false.pos"=data.frame()))
    }
    
    actual <- data.frame(actual[,checked.cols])
    exp <- data.frame(exp[,checked.cols])
    colnames(actual) <- checked.cols
    colnames(exp) <- checked.cols
    exp$matches = NA

    ## Match each detected event, x, with at most one actual touchdown
    each.row <- split(actual, rownames(actual))
    time.diffs <- Reduce(function(actual, x) find.match(actual, x),
                   x=each.row,
                   init=exp)

    ## Rename column
    names(time.diffs)[names(time.diffs) == "matches"] <- "latency"
    
    true.pos <- time.diffs[which(!is.na(time.diffs$latency)),]
    false.neg <- time.diffs[which(is.na(time.diffs$latency)),]

    matched.true.pos <- true.pos
    matched.true.pos$timestamp <- matched.true.pos$timestamp + matched.true.pos$latency
    matched.true.pos <- data.frame(matched.true.pos[,checked.cols])
    names(matched.true.pos) <- checked.cols

    false.pos <- df.setdiff(actual, matched.true.pos)

    ## Every duplicate detection is a false positive
    duplicates <- data.frame(actual[which(duplicated(actual)),])
    names(duplicates) <- checked.cols

    false.pos <- data.frame(rbind(false.pos, duplicates))
    names(false.pos) <- checked.cols
    
    result <- list("true.pos"=true.pos,
                   "false.neg"=false.neg,
                   "false.pos"=false.pos)
    return(result)
}

## Set the detection latency for the actual touchdown event
## corresponding to detection x, if the actual touchdown exists
find.match <- function(actuals, x) {
    
    THRESHOLD <-  30 * 1000 # x must fall in actual + THRESHOLD to be a match
    x <- data.frame(x)
    actuals <- data.frame(actuals)
    if (nrow(x) == 0) {
        return(actuals)
    }

    latencies <- sapply(x$timestamp - actuals$timestamp,
                        function (l) if (l >= 0 & l <= THRESHOLD) l else Inf)

    other.cols <- Filter(function(col) !col %in% c("timestamp", "matches"), names(x))
    other.cols.match <- function(x, y) {
        if (length(other.cols) == 0) {
            return(TRUE)
        } else {
            return(x[, other.cols] == y[which.min(latencies), other.cols])
        }
    }

    if (is.finite(min(latencies)) & other.cols.match(x, actuals)) {
        latency <- x$timestamp - actuals[which.min(latencies), "timestamp"]
        index <- which.min(latencies)
        if (is.na(actuals$matches[index]) | latency < actuals$matches[index]) {
            actuals$matches[index] = latency
        }
    }
    return(actuals)
}

## Like setdiff, but for data.frames
df.setdiff <- function(x, y) {
    diff <- x[!duplicated(rbind(y,x))[-seq_len(nrow(y))],]
    diff <- data.frame(diff)
    names(diff) <- names(x)
    return(diff)
}

## 1/2 score comes from precision, 1/2 from recall
score.benchmark <- function(b) {
    precision <- nrow(b$true.pos) / (nrow(b$true.pos) + nrow(b$false.pos))
    precision <- if (is.nan(precision)) 0 else precision
    recall <- nrow(b$true.pos) / (nrow(b$true.pos) + nrow(b$false.neg))
    (.5 * precision) + (.5 * recall)
}


## Contains all touchdowns in our entire dataset
all.touchdowns <- data.frame(
    rbind(
        ## From tweets.2014-12-15.01:
        ## Cowboys vs. Eagles
        c(1418607396778,"demarco murray"),
        c(1418608620457,"dez bryant"),
        c(1418609918817,"dez bryant"),
        c(1418610531857,"chris polk"),
        c(1418614014826,"chris polk"),
        c(1418614584563,"darren sproles"),
        c(1418615124627,"demarco murray"),
        c(1418615763277,"dez bryant"),

        ## From tweets.2015-01-03.21:
        ## Panthers vs. Cardinals
        c(1420322307784, "jonathan stewart"),
        c(1420323311546, "darren fells"),
        c(1420325444064, "marion grice"), ## after an official review
        c(1420328443799, "fozzy whittaker"), ## stream cut off in mid-jubilation

        ## From tweets.2015-01-04.00:
        ## Ravens vs. Steelers
        c(1420336225464, "bernard pierce"),
        c(1420341051666, "torrey smith"),
        c(1420342472537, "antonio brown"), ## challenged and reversed
        c(1420342780503, "martavis bryant"),
        c(1420343794738, "crockett gillmore"),

        ## From tweets.2015-01-10.21:
        ## Ravens vs. Patriots
        c(1420925929770, "kamar aiken"),
        c(1420927164259, "steve smith"),
        c(1420927826867, "tom brady"),
        c(1420930017796, "danny amendola"),
        c(1420931329205, "owen daniels"),
        c(1420933189578, "justin forsett"),
        c(1420933853390, "rob gronkowski"),
        c(1420934675490, "danny amendola"), ## pass from edelman

        ## From tweets.2015.01.11.00:
        ## Seahawks vs. Panthers
        c(1420941439960, "doug baldwin"),
        c(1420942356604, "kelvin benjamin"),
        c(1420942868721, "jermaine kearse"),
        c(1420948391051, "luke willson"),
        c(1420948923760, "kam chancellor"),
        c(1420949535778, "kelvin benjamin"),

        ## From tweets.2015.01.11.18
        ## Packers vs. Cowboys
        c(1421000075195, "andrew quarless"),
        c(1421001163846, "tyler clutts"),
        c(1421002527647, "terrance williams"),

        ## From tweets.2015.01.18.20
        ## Packers vs. Seahawks
        c(1421614079309, "randall cobb"),
        c(1421618825996, "garry gilliam"), ## fake fg pass from jon ryan
        c(1421621824353, "marshawn lynch"), ## reversed
        c(1421622090788, "russell wilson"),
        c(1421622554679, "marshawn lynch"),
        c(1421623760853, "jermaine kearse"),

        ## Patriots vs. Colts
        c(1421625677420, "legarrette blount"),
        c(1421627195617, "james develin"),
        c(1421629101030, "zurlon tipton"),
        c(1421632134037, "nate solder"),
        c(1421633258404, "rob gronkowski"),
        c(1421633815193, "legarrette blount"),

        ## From tweets.2015.01.19.02
        ## Patriots vs. Colts
        c(1421634938146, "legarrette blount")
))
