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

benchmark <- function(actual, exp, check.players=F) {

    if (check.players) {
        checked.cols <- c("timestamp", "player")
    } else {
        checked.cols <- c("timestamp")
    }
    
    if (length(actual) == 0) {
        return(list("true.pos"=data.frame(),
                    "false.neg"=exp,
                    "false.pos"=data.frame()))
    }

    if (nrow(exp) == 0) {
        return(list("true.pos"=data.frame(),
                    "false.neg"=data.frame(),
                    "false.pos"=exp))
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


score.benchmark <- function(b) {

    ## precision is undefined
    if (nrow(b$true.pos) == 0 & nrow(b$false.pos) == 0) {
        return(ifelse(nrow(b$false.neg) > 0, 0, 1))
    }
    
    ## recall is undefined
    if (nrow(b$true.pos) == 0 & nrow(b$false.neg) == 0) {
        return(ifelse(nrow(b$false.pos) == 0, 1, 0))
    }
    
    precision <- nrow(b$true.pos) / (nrow(b$true.pos) + nrow(b$false.pos))
    recall <- nrow(b$true.pos) / (nrow(b$true.pos) + nrow(b$false.neg))

    # F1 score (http://en.wikipedia.org/wiki/F1_score)
    return(2 * (precision * recall)/(precision + recall))
}

## Contains all touchdowns in our entire dataset
all.touchdowns <- data.frame(
    rbind(
        ## From tweets.2014-12-15.01.json:
        ## Cowboys vs. Eagles
        c(1418607396778,"demarco murray", "tweets.2014-12-15.01.json"),
        c(1418608620457,"dez bryant", "tweets.2014-12-15.01.json"),
        c(1418609918817,"dez bryant", "tweets.2014-12-15.01.json"),
        c(1418610531857,"chris polk", "tweets.2014-12-15.01.json"),
        c(1418614014826,"chris polk", "tweets.2014-12-15.01.json"),
        c(1418614584563,"darren sproles", "tweets.2014-12-15.01.json"),
        c(1418615124627,"demarco murray", "tweets.2014-12-15.01.json"),
        c(1418615763277,"dez bryant", "tweets.2014-12-15.01.json"),

        ## From tweets.2015-01-03.21.json:
        ## Panthers vs. Cardinals
        c(1420322307784, "jonathan stewart", "tweets.2015-01-03.21.json"),
        c(1420323311546, "darren fells", "tweets.2015-01-03.21.json"),
        c(1420325444064, "marion grice", "tweets.2015-01-03.21.json"), ## after an official review
        c(1420328443799, "fozzy whittaker", "tweets.2015-01-03.21.json"), ## stream cut off in mid-jubilation

        ## From tweets.2015-01-04.00.json:
        ## Ravens vs. Steelers
        c(1420336225464, "bernard pierce", "tweets.2015-01-04.00.json"),
        c(1420341051666, "torrey smith", "tweets.2015-01-04.00.json"),
        c(1420342472537, "antonio brown", "tweets.2015-01-04.00.json"), ## challenged and reversed
        c(1420342780503, "martavis bryant", "tweets.2015-01-04.00.json"),
        c(1420343794738, "crockett gillmore", "tweets.2015-01-04.00.json"),

        ## From tweets.2015-01-10.21.json:
        ## Ravens vs. Patriots
        c(1420925929770, "kamar aiken", "tweets.2015-01-10.21.json"),
        c(1420927164259, "steve smith", "tweets.2015-01-10.21.json"),
        c(1420927826867, "tom brady", "tweets.2015-01-10.21.json"),
        c(1420930017796, "danny amendola", "tweets.2015-01-10.21.json"),
        c(1420931329205, "owen daniels", "tweets.2015-01-10.21.json"),
        c(1420933189578, "justin forsett", "tweets.2015-01-10.21.json"),
        c(1420933853390, "rob gronkowski", "tweets.2015-01-10.21.json"),
        c(1420934675490, "danny amendola", "tweets.2015-01-10.21.json"), ## pass from edelman

        ##### PARTITION @ 1420935046297 ######
        
        ## From tweets.2015-01-11.00.json:
        ## Seahawks vs. Panthers
        c(1420941439960, "doug baldwin", "tweets.2015-01-11.00.json"),
        c(1420942356604, "kelvin benjamin", "tweets.2015-01-11.00.json"),
        c(1420942868721, "jermaine kearse", "tweets.2015-01-11.00.json"),
        c(1420948391051, "luke willson", "tweets.2015-01-11.00.json"),
        c(1420948923760, "kam chancellor", "tweets.2015-01-11.00.json"),
        c(1420949535778, "kelvin benjamin", "tweets.2015-01-11.00.json"),

        ## From tweets.2015-01-11.18.json
        ## Packers vs. Cowboys
        c(1421000075195, "andrew quarless", "tweets.2015-01-11.18.json"),
        c(1421001163846, "tyler clutts", "tweets.2015-01-11.18.json"),
        c(1421002527647, "terrance williams", "tweets.2015-01-11.18.json"),

        ## From tweets.2015-01-18.20.json
        ## Packers vs. Seahawks
        c(1421614079309, "randall cobb", "tweets.2015-01-18.20.json"),
        c(1421618825996, "garry gilliam", "tweets.2015-01-18.20.json"), ## fake fg pass from jon ryan
        c(1421621824353, "marshawn lynch", "tweets.2015-01-18.20.json"), ## reversed
        c(1421622090788, "russell wilson", "tweets.2015-01-18.20.json"),
        c(1421622554679, "marshawn lynch", "tweets.2015-01-18.20.json"),
        c(1421623760853, "jermaine kearse", "tweets.2015-01-18.20.json"),

        ## Patriots vs. Colts
        c(1421625677420, "legarrette blount", "tweets.2015-01-18.20.json"),
        c(1421627195617, "james develin", "tweets.2015-01-18.20.json"),
        c(1421629101030, "zurlon tipton", "tweets.2015-01-18.20.json"),
        c(1421632134037, "nate solder", "tweets.2015-01-18.20.json"),
        c(1421633258404, "rob gronkowski", "tweets.2015-01-18.20.json"),
        c(1421633815193, "legarrette blount", "tweets.2015-01-18.20.json"),

        ## From tweets.2015-01-19.02.json
        ## Patriots vs. Colts
        c(1421634938146, "legarrette blount", "tweets.2015-01-19.02.json")))

names(all.touchdowns) <- c("timestamp", "player", "file")
all.touchdowns$timestamp <- as.numeric(as.character(all.touchdowns$timestamp))
