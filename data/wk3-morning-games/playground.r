library(ggplot2)

prefix <- "/Users/astahlman/Documents/Programming/ML/td-stream/data/wk3-morning-games"
touchdowns <- read.csv(file=paste(prefix, "touchdowns.tsv", sep="/"), header=TRUE, sep='\t')
#fixtures <- read.csv(file=paste(prefix, "fixtures.tsv", sep="/"), header=TRUE, sep='\t')

#sun.morning.tds.home <- merge(touchdowns, fixtures, by.x="team", by.y="home")
#sun.morning.tds.home <- sun.morning.tds.home[,!(names(sun.morning.tds.home) %in% c("away"))]
#sun.morning.tds.away <- merge(touchdowns, fixtures, by.x="team", by.y="away")
#sun.morning.tds.away <- sun.morning.tds.away[,!(names(sun.morning.tds.away) %in% c("home"))]
#sun.morning.tds <- rbind(sun.morning.tds.home, sun.morning.tds.away)
#sun.morning.tds <- sun.morning.tds[which(sun.morning.tds$week == 3 & sun.morning.tds$weekday == "SUN" & sun.morning.tds$start == "01:00:00 PM"),]
tds.truth <- touchdowns[which(!is.na(touchdowns$timestamp)),]

signal.files <- lapply(X=c("17","18","19"),
                       FUN=function(h) {
                           paste(prefix, "/default.default.td-signal.csv.2015-09-27.", h,  sep="")
                       })

signal <- lapply(FUN=function(f) {
                     read.csv(file=f, header=FALSE, sep=',')
                 }, signal.files)
signal <- Reduce(f=rbind, x=signal)
names(signal) <- c("timestamp", "signal.val")

ggplot(data=signal, aes(x=timestamp, y=signal.val)) + geom_line(colour="red") +
    geom_vline(xintercept=tds.truth$timestamp / 1000)
    #lapply(FUN=function(x) { geom_vline(aes(xintercept=x)) }, X=tds.truth$timestamp)
