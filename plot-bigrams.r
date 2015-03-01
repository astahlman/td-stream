csv_file <- "~/Documents/Programming/ML/td-stream/tagging/bigrams-eg.tsv"
df <- read.csv(csv_file, header=T, sep="\t")
df <- setNames(aggregate(df$count, by=list(df$bigram), FUN=sum), c("bigram", "count"))
df <- df[with(df, order(-count)),]
df$bigram <- factor(df$bigram, levels=df$bigram[rev(order(df$count))], ordered=TRUE)

library(ggplot2)
ggplot(data=df, aes(x=bigram, y=count)) + geom_bar(stat="identity") +
    theme(axis.text.x=element_text(angle=90, hjust=0)) +
    xlab("Bigram") + ylab("Occurrences") +
    ggtitle("Bigram Frequency Following a Demarco Murray Touchdown") +
    scale_color_manual(values=c("dodgerblue4", "darkolivegreen4"))
