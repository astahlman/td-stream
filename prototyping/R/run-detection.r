source("benchmark.r")
source("detect-touchdowns.r")
source("load-tweets.r")

## Takes a set of raw-tweets files and a ground-truth for the detections
## and checks the detections against the truth

base.dir <- "/Users/astahlman/Documents/Programming/ML/td-stream/data/wk3-morning-games"
tweet.files <- c("raw-tweets.log.2015-09-27.17.json", "raw-tweets.log.2015-09-27.18.json", "raw-tweets.log.2015-09-27.19.json")
truth.file <- "truth-morning-only.tsv"
setwd(base.dir)


tweets <- load.all.data(files=tweet.files)
#detections <- detection.with.state.signal.amp(tweets)
detections <- team.based.detection(tweets)
truth <- read.csv(file=truth.file, sep="\t")
results <- benchmark(detections, truth)
score <- score.benchmark(results)
