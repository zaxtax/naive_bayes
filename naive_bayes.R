library('rjags')
library('coda')

load("20news.Rdata")
topics <- topics + 1
words  <- words  + 1
docs   <- docs   + 1

vocabSize <- length(unique(words))
topicSize <- length(unique(topics))


# Use docsPerTopic*topicSize documents
docsPerTopic <- 100
docIndices   <- c(sapply(0:(topicSize-1),
                         function(i) (1000*i):(1000*i+docsPerTopic-1))) + 1
topics       <- topics[docIndices]
words        <- words[docs %in% docIndices]
docs         <- as.numeric(as.factor(docs[docs %in% docIndices]))

# Classify 10 documents of each topic
topicIndices <- c(sapply(0:(topicSize-1),
                         function(i) (docsPerTopic*i):(docsPerTopic*i+10-1))) + 1

zTrues <- topics[topicIndices]
topics[topicIndices] <- NA

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

start.time <- Sys.time()

update(jags, 1);

samples <- jags.samples(jags, c('z'), 1);

end.time <- Sys.time()
duration <- end.time - start.time
# format(duration)
cat("JAGS",format(duration), sep=",", fill=TRUE)

print(samples$"z"[topicIndices])
print(zTrues)
