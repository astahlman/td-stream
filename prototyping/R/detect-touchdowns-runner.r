source("benchmark.r")
source("load-tweets.r")
source("detect-touchdowns.r")

blacklist <- c("moving-avg-1.r")
#blacklist <- c()

load.candidates <- function() {
    files <- dir("candidates")
    files <- Filter(function(c) !(c %in% blacklist), files)
    candidates <- lapply(files, function(f) {
        source(paste("candidates", f, sep="/"))
        get.candidates()
    })

    unlist(candidates, recursive=F)
}

do.run <- function(candidates=NULL) {
    
    ## Don't reload every time if we are running this interactively
    if (!exists("tweets")) {
        print("Loading tweets...")
        tweets <- load.data()
    }

    df <- tweets
    candidates <- if (!is.null(candidates)) candidates else load.candidates()
    results <- Map(function(c) {
        print(c)
        c$result <- c$f(df)
        return(c)
    }, candidates)

    scores <- lapply(
        results,
        function(x) {
            list(
                score=score.benchmark(benchmark(x$result)),
                description=x$description)
        })
    return(do.call(rbind.data.frame, scores))
}





