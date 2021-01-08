library(ldatuning)

### Optimal N of Topics with LDAtuning ###
G <- 10000   # Default value of Iteration = 2000
alpha <- 1.5



result <- FindTopicsNumber(dtm,topics = seq(from = 2, to = 100, by = 1),
                           metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
                           method = "Gibbs",
                           # control = list(seed = 77),
                           control = list(seed = 77, iter=G, alpha = alpha),
                           #   control = list(seed = 77, iter=G),
                           mc.cores = 8,
                           verbose = TRUE)
result

#write.csv(result,file="LDAtuning_result.csv")
write.csv(result,file="LDAtuning_result_20201231_a1.5_G10k.csv")
FindTopicsNumber_plot(result)


### References
# Arun, R., Suresh, V., Madhavan, C. V., & Murthy, M. N. (2010, June). On finding the natural number of topics with latent dirichlet allocation: Some observations. In Pacific-Asia conference on knowledge discovery and data mining (pp. 391-402). Springer, Berlin, Heidelberg.
# Cao, J., Xia, T., Li, J., Zhang, Y., & Tang, S. (2009). A density-based method for adaptive LDA model selection. Neurocomputing, 72(7-9), 1775-1781.
# Deveaud, R., SanJuan, E., & Bellot, P. (2014). Accurate and effective latent concept modeling for ad hoc information retrieval. Document numérique, 17(1), 61-84.
# Griffiths, T. L., & Steyvers, M. (2004). Finding scientific topics. Proceedings of the National academy of Sciences, 101(suppl 1), 5228-5235.