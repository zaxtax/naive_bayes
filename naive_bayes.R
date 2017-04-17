library('rjags')
library('coda')

load("20news.Rdata")
topics <- topics + 1
words  <- words  + 1
docs   <- docs   + 1

vocabSize <- length(unique(words))
topicSize <- length(unique(topics))


# Use docsPerTopic*topicSize documents
docsPerTopic   <- 100
trainTestSplit <- 0.9
docIndices     <- c(sapply(0:(topicSize-1),
                           function(i) (1000*i+1):(1000*i+docsPerTopic)))
topics         <- topics[docIndices]
words          <- words[docs %in% docIndices]
docs           <- as.numeric(as.factor(docs[docs %in% docIndices]))

testDocsPerTopic <- floor(docsPerTopic * trainTestSplit)
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

end.time <- Sys.time()
duration  <- end.time - start.time
duration2 <- end.time - start2.time
# format(duration)
cat("JAGS",format(duration), format(duration2), sep=",", fill=TRUE)

print(zPredicts)
print(zTrues)
print(length(zTrues[zPredicts == zTrues])/length(zTrues))
