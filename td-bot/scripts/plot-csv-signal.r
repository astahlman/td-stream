sig <- read.table(file="/tmp/csv_reporter/default.default.td-signal.csv", sep=",", header=T, colClasses=c("numeric", "character"), col.names=c("t", "val"))
sig <- sig[which(sig$val != "null"),]
frac.to.decimal <- function(x) { eval(parse(text=x)) }
sig$val <- sapply(sig$val, frac.to.decimal)
plot(x=sig$t, y=sig$val)
lines(x=sig$t, y=sig$val)

