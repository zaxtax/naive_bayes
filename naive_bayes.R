#!/usr/bin/env Rscript

suppressMessages(library('rjags'))
suppressMessages(library('coda'))

load("20news.Rdata")
topics <- topics + 1
words  <- words  + 1
docs   <- docs   + 1

vocabSize <- length(unique(words))
topicSize <- length(unique(topics))


args = commandArgs(trailingOnly=TRUE)
if (length(args) != 2) {
  cat("naive_bayes.R <docsPerTopic> <trial>\n")
} else {

docsPerTopic <- as.numeric(args[1])
trial        <- as.numeric(args[2])

# Use docsPerTopic*topicSize documents
trainTestSplit <- 0.9
docIndices     <- c(sapply(0:(topicSize-1),
                           function(i) (1000*i+1):(1000*i+docsPerTopic)))
topics         <- topics[docIndices]
words          <- words[docs %in% docIndices]
docs           <- as.numeric(as.factor(docs[docs %in% docIndices]))

testDocsPerTopic <- floor(docsPerTopic * (1 - trainTestSplit))
topicIndices <- c(sapply(0:(topicSize-1),
                         function(i)
                           (docsPerTopic*i+1):(docsPerTopic*i+testDocsPerTopic)))

zTrues <- topics[topicIndices]
topics[topicIndices] <- NA

start.time <- Sys.time()

jags <- jags.model('naive_bayes.jags',
                   data = list('Nwords'     = length(words),
                               'Ndocs'      = length(topics),
                               'Ntopics'    = topicSize,
                               'Nvocab'     = vocabSize,
                               'onesTopics' = rep(1,topicSize),
                               'onesVocab'  = rep(1,vocabSize),
                               'z'          = topics,
                               'w'          = words,
                               'doc'        = docs),
                   n.chains = 1,
                   n.adapt = 10,
                   quiet=TRUE)

start2.time <- Sys.time()

update(jags, 1);

samples <- jags.samples(jags, c('z'), 1);
zPredicts <- samples$"z"[topicIndices]

end.time  <- Sys.time()
duration  <- difftime(end.time, start.time,  units="sec")
duration2 <- difftime(end.time, start2.time, units="sec")

accuracy  <- length(zTrues[zPredicts == zTrues])/length(zTrues)

cat("JAGS_init",
    as.numeric(duration),
    as.numeric(docsPerTopic*20),
    format(accuracy),
    format(trial),
    sep=",",
    fill=TRUE)
cat("JAGS",
    as.numeric(duration2),
    as.numeric(docsPerTopic*20),
    format(accuracy),
    format(trial),
    sep=",",
    fill=TRUE)

#print(zPredicts)
#print(zTrues)

}
