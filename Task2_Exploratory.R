con <- file('task1.txt', 'r')
text <- readLines(con)
close(con)
source("Task1_Bag.R")

freq <- bags(text)
words <- freq$word
sum <- sum(words)
t <- sum/2
a <- 0
for(i in 1:ncol(words)){
    a <- a + words[,i]
    if(a>=t){n=i; break}
}
n
