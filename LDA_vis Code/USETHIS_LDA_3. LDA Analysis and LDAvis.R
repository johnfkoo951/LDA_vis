### Main Analysis: LDA Analysis with Gibbs Sampling ###

K = 34
G <- 10000   # Default value of Iteration = 2000
alpha <- 0.75
alpha <- 1.5

#m = LDA(dtm, method = "Gibbs", k = K,  control = list(iter=G, alpha = alpha))
m = LDA(dtm, method = "Gibbs", k = K,  control = list(iter=G, alpha = alpha), mc.cores = 8)
m = LDA(dtm, method = "Gibbs", k = K,  control = list(iter=G), mc.cores = 8)
m = LDA(dtm, method = "Gibbs", k = K,  control = list(alpha=alpha, 
                                                      estimate.beta=TRUE, 
                                                      verbose=0, 
                                                      prefix=tempfile(), 
                                                      save=0, 
                                                      keep=0, 
                                                      seed=as.integer(Sys.time()), 
                                                      nstart=1, 
                                                      best=TRUE, 
                                                      delta=0.1, 
                                                      iter=G, 
                                                      burnin=0, 
                                                      thin=2000),
        mc.cores=8)

m = LDA(dtm, method = "Gibbs", k = K,  control = list(alpha=50/K, 
                                                      estimate.beta=TRUE, 
                                                      verbose=0, 
                                                      prefix=tempfile(), 
                                                      save=0, 
                                                      keep=0, 
                                                      seed=as.integer(Sys.time()), 
                                                      nstart=1, 
                                                      best=TRUE, 
                                                      delta=0.1, 
                                                      iter=2000, 
                                                      burnin=0, 
                                                      thin=2000),
        mc.cores=8)

### Model Save
LDA_Model_n1321_k34_R07s <- m


##############################################################################
### LDA Visualization ###


library(LDAvis)

dtm = dtm[slam::row_sums(dtm) > 0, ]
phi = as.matrix(posterior(m)$terms)
### dentist code 
### phi <- posterior(m)$terms %>% as.matrix
theta <- as.matrix(posterior(m)$topics)
vocab <- colnames(phi)
doc.length = slam::row_sums(dtm)
term.freq = slam::col_sums(dtm)[match(vocab, colnames(dtm))]

json = createJSON(phi = phi,
                  theta = theta,
                  vocab = vocab,
                  doc.length = doc.length,
                  term.frequency = term.freq)
serVis(json)

serVis(json, out.dir = '/Users/yhn_hac/Library/Mobile Documents/com~apple~CloudDocs/1. R Projects/LDA_vis/Results_LDAvis/jan8_n1321_k34_R07s', open.browser = FALSE)

#serVis(json, out.dir = 'vis', open.browser = FALSE)
#serVis(json, out.dir='LDAvis_20201229_k33_a1.5', open.browser=FALSE)
