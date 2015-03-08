library("rjson")
library("plyr")
library("ggplot2")
library("Runit")

load.data <- function(f) {
    d <- lapply(readLines(json.file), fromJSON)
    df <- data.frame(do.call("rbind", d), stringsAsFactors=F)
    names(df) <- c("time", "text")
    df$time <- as.numeric(df$time)
    
    isTouchdown <- function(text) {
        grepl(pattern="touchdown",
              ignore.case=T,
              x=text)
    }

    df$is_td <- isTouchdown(df$text)
    return(df)
}

latency.test <- function() {
    fake.results <- data.frame(
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
    colnames(fake.results) <- c("timestamp", "player")
    fake.results$timestamp <- as.numeric(as.character(fake.results$timestamp))
    
    ## Latencies are 2,4,6...,16 summing to 72. Mean latency is 9 millis.
    fake.results$timestamp <- fake.results$timestamp + sapply(c(1:8), function(x) x * 2)

    results <- benchmark(fake.results)
    

}

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

    # TODO: There's probably a more idiomatic way to do this...
    each.row <- lapply(1:dim(actual)[1],
               FUN=function(i) list(
                   player=actual[i, "player"],
                   timestamp=as.numeric(actual[i, "timestamp"])))
    Reduce(function(actual, x) find.match(actual, x),
           x=each.row,
           init=exp)
}

find.match <- function(actuals, x) {

    x <- data.frame(x)
    latencies <- sapply(x$timestamp - actuals$timestamp, function (l) if (l >= 0 & l <= THRESHOLD) l else Inf)
     #latencies <- sapply(-actuals$timestamp + x["timestamp"], function (l) if (l >= 0 & l <= THRESHOLD) l else Inf)
    if (is.finite(min(latencies)) & x$player == actuals[which.min(latencies), "player"]) {
        latency <- x$timestamp - actuals[which.min(latencies), "timestamp"]
        index <- which.min(latencies)
        if (is.na(actuals$matches[index]) | latency < actuals$matches[index]) {
            actuals$matches[index] = latency
        }
    }
    return(actuals)
}
