library('rjags')
library('coda')

load("20news.Rdata")

vocabSize <- length(unique(words))
#topics <- topics[1:100]
#words  <- words[1:10000]

jags <- jags.model('naive_bayes.jags',
                   data = list('Nwords'     = length(words),
                               'Ndocs'      = length(topics),
                               'Ntopics'    = 20,
                               'Nvocab'     = vocabSize,
                               'onesTopics' = rep(1,20),
                               'onesVocab'  = rep(1,vocabSize),
                               'z'          = topics + 1,
                               'w'          = words + 1,
                               'doc'        = docs + 1),
                   n.chains = 1,
                   n.adapt = 10,
                   quiet=TRUE)

start.time <- Sys.time()
 
update(jags, 1);

## coda.samples(jags,
##              c('z', 'phi'),
##              1)
samples <- jags.samples(jags,
                        c('z[2]'),
                        1);

end.time <- Sys.time()
duration <- end.time - start.time
# format(duration)
cat("JAGS",as.double(duration), sep=",", fill=TRUE)
