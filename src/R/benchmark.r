library("rjson")
library("plyr")
library("ggplot2")

THRESHOLD <-  30 * 1000
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

benchmark <- function(actual, exp=expected, check.players=TRUE) {

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

    each.row <- split(actual, rownames(actual))
    time.diffs <- Reduce(function(actual, x) find.match(actual, x),
                   x=each.row,
                   init=exp)

    names(time.diffs)[names(time.diffs) == "matches"] <- "latency"
    
    true.pos <- time.diffs[which(!is.na(time.diffs$latency)),]
    false.neg <- time.diffs[which(is.na(time.diffs$latency)),]

    matched.true.pos <- true.pos
    matched.true.pos$timestamp <- matched.true.pos$timestamp + matched.true.pos$latency
    matched.true.pos <- data.frame(matched.true.pos[,checked.cols])
    names(matched.true.pos) <- checked.cols

    false.pos <- data.frame(df.setdiff(actual, matched.true.pos))

    ## every duplicate alarm is a false positive
    duplicates <- data.frame(actual[which(duplicated(actual)),])
    #names(duplicates) <- checked.cols
    false.pos <- data.frame(rbind(false.pos, duplicates))
    names(false.pos) <- checked.cols
    
    result <- list("true.pos"=true.pos,
                   "false.neg"=false.neg,
                   "false.pos"=false.pos)
    result
}

find.match <- function(actuals, x) {
    
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

# like setdiff, but for data.frames
df.setdiff <- function(x, y) {
    diff <- x[!duplicated(rbind(y,x))[-seq_len(nrow(y))],]
}
