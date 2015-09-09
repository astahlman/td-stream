sig <- read.table(file="/tmp/csv_reporter/default.default.td-signal.csv", sep=",", header=T, colClasses=c("numeric", "character"), col.names=c("t", "val"))
threshold <- read.table(file="/tmp/csv_reporter/default.default.td-threshold.csv", sep=",", header=T, colClasses=c("numeric", "character"), col.names=c("t", "val"))

require(ggplot2)

start.t <- 1441335000
end.t <- 1441338000
sig <- sig[which(sig$val != "null" & sig$t >= start.t & sig$t <= end.t),]
threshold <- threshold[which(threshold$val != "null" & threshold$t >= start.t & threshold$t <= end.t),]

frac.to.decimal <- function(x) { eval(parse(text=x)) }
sig$val <- sapply(sig$val, frac.to.decimal)
threshold$val <- sapply(threshold$val, frac.to.decimal)

plot(x=sig$t, y=sig$val, col="blue")
points(x=threshold$t, y=threshold$val, col="red")
lines(x=sig$t, y=sig$val, col="blue")
lines(x=threshold$t, y=threshold$val, col="red")

