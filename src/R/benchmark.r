library("rjson")
library("plyr")
library("ggplot2")

THRESHOLD <-  30 * 1000
expected <- data.frame(
    rbind(
        c(1418607396778,"demarco murray"),
        c(1418608625870,"dez bryant"),
        c(1418609918817,"dez bryant"),
        c(1418610531857,"chris polk"),
        c(1418614014826,"chris polk"),
        c(1418614593480,"darren sproles"),
        c(1418615124627,"demarco murray"),
        c(1418615763277,"dez bryant")),
    stringsAsFactors=FALSE)
colnames(expected) <- c("timestamp", "player")
expected$timestamp <- as.numeric(as.character(expected$timestamp))

benchmark <- function(actual, exp=expected) {

    exp$matches = NA
    ## TODO: There's probably a more idiomatic way to do this...
    each.row <- lapply(seq_len(nrow(actual)),
                       FUN=function(i) list(
                           player=actual[i, "player"],
                           timestamp=as.numeric(actual[i, "timestamp"])))


    time.diffs <- Reduce(function(actual, x) find.match(actual, x),
                   x=each.row,
                   init=exp)

    names(time.diffs)[names(time.diffs) == "matches"] <- "latency"
    
    true.pos <- time.diffs[which(!is.na(time.diffs$latency)),]
    false.neg <- time.diffs[which(is.na(time.diffs$latency)), c("player", "timestamp")]

    matched.true.pos <- true.pos
    matched.true.pos$timestamp <- matched.true.pos$timestamp + matched.true.pos$latency
    matched.true.pos <- matched.true.pos[,c("timestamp", "player")]
    false.pos <- df.setdiff(actual, matched.true.pos)
    ## every duplicate alarm is a false positive
    false.pos <- rbind(false.pos, actual[which(duplicated(actual)),]) 
    
    result <- list("true.pos"=true.pos,
                   "false.neg"=false.neg,
                   "false.pos"=false.pos)
}

find.match <- function(actuals, x) {
    
    x <- data.frame(x)

    if (nrow(x) == 0) {
        return(actuals)
    }

    latencies <- sapply(x$timestamp - actuals$timestamp, function (l) if (l >= 0 & l <= THRESHOLD) l else Inf)

    if (is.finite(min(latencies)) & x$player == actuals[which.min(latencies), "player"]) {
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
    x[!duplicated(rbind(y,x))[-seq_len(nrow(y))],]
}
