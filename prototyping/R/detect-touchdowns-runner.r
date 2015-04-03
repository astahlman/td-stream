library("reshape2")
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

benchmark.candidates <- function(tweets, exp.touchdowns, candidates) {

    results <- Map(function(c) {
        print(c)
        c$result <- c$f(tweets)
        return(c)
    }, candidates)

    benchmarks <- lapply(
        results,
        function(x) {
            list(
                benchmark=benchmark(x$result, exp.touchdowns),
                description=x$description)
        })

    return(benchmarks)
}

partition.data <- function(tweets) {
    files <- levels(tweets$file)
    train.files <- files[1:(.5 * length(files))]
    validation.files <- files[-seq_along(train.files)]
    
    ## cutoff <- 1420935046297
    
    list(train.data=tweets[which(tweets$file %in% train.files),],
         train.actual=all.touchdowns[which(all.touchdowns$file %in% train.files),],
         validation.data=tweets[which(tweets$file %in% validation.files),],
         validation.actual=all.touchdowns[which(all.touchdowns$file %in% validation.files),])
}

score.candidates.by.file <- function(tweets, actual, cand) {
    
    results <- Map(function(f) {
        benchmark.candidates(
            tweets[which(tweets$file == f),],
            actual[which(actual$file == f),],
            cand)
    }, levels(droplevels(tweets$file)))
    
    scores <- sapply(results,
                     function(file.results) {
                         sapply(file.results,
                                function(candidate) {
                                    score.benchmark(candidate$benchmark)
                                })
                            })

    results.df <- melt(scores)
    colnames(results.df) <- c("candidate", "file", "score")
    return(results.df)
}

run <- function(df, candidates) {
    
    #candidates <- tail(candidates, 4) ## faster testing
    candidates <- Map(
        function(i) append(candidates[[i]], list("id"=i)),
        seq_along(candidates))
    
    d <- partition.data(df)
    
    train.results <- score.candidates.by.file(
        d$train.data,
        d$train.actual,
        candidates)

    avg.scores <- ddply(train.results,
                        .(candidate),
                        summarise,
                        score.mean=mean(score))
    avg.scores <- avg.scores[with(avg.scores, order(-score.mean)),]
    top.10 <- avg.scores[1:min(nrow(avg.scores), 10),]

    validation.results <- score.candidates.by.file(
        d$validation.data,
        d$validation.actual,
        candidates[top.10$candidate])
    
    return(list(
        train=train.results,
        validation=validation.results))
}
