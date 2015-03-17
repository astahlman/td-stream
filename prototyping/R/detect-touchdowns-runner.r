source("benchmark.r")
source("load-tweets.r")
source("detect-touchdowns.r")

# detector must retain a data.frame with timestamp and, optionally, players
all.detectors <- ls(all.names=T)[grep("detect.fn", ls(all.names=T))]

# Don't reload every time if we are running this interactively
if (!exists("tweets")) {
    print("Loading tweets...")
    tweets <- load.data()
}

do.profile <- function(detectors=all.detectors) {
    do.run <- function(detector) {
        print(paste("Running", detector))
        get(detector)(tweets)
    }
    do.benchmark <- function(results) benchmark(results, check.players=FALSE)
    scores <- sapply(detectors, function(f) {
        tryCatch(score.benchmark(do.benchmark(do.run(f))),
                 error=function(cond) {
                     message("Failure!")
                     return(-1)
                 })
                 
    })
    scores
 }
