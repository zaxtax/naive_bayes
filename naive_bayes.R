library('rjags')
library('coda')

load("20news.Rdata")
topics <- topics + 1
words  <- words  + 1
docs   <- docs   + 1

vocabSize <- length(unique(words))
topicSize <- length(unique(topics))


# Only hold onto docSize documents
docSize <- 2000
topics  <- topics[1:docSize]
words   <- words[docs <= docSize]

# Classify document 1190
z1190_True <- topics[1190]
topics[1190] <- NA

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

## coda.samples(jags,
##              c('z', 'phi'),
##              1)
samples <- jags.samples(jags,
                        c('z[1190]'),
                        1);

end.time <- Sys.time()
duration <- end.time - start.time
# format(duration)
cat("JAGS",format(duration), sep=",", fill=TRUE)
cat(samples$"z[1190]", z1190_True, fill=TRUE)
