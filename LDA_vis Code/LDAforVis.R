### Install Packages ### 
# install.packages("corpus")
# install.packages("quanteda")
# install.packages("textclean")
# install.packages("LDAvis")
# install.packages("SnowballC")
# install.packages("topicmodels")
# install.packages("ldatuning")


### LDA Data Preparation ###
library(corpus)
library(quanteda)
library(tm)  # general text mining functions, making document term matrixes
library(textclean)

# library(corpus)
# library(quanteda)
library(topicmodels)  # for LDA topic modelling 
# library(tm)
# library(textclean)
library(SnowballC)  # for stemming


library(ldatuning)
library(topicmodels)

library(LDAvis)
library(stringr)  # for replacement

##############################################################################


### Read csv file & convert to CORPUS ####
#dataset_original = read.csv(file.choose(), stringsAsFactors = FALSE) 

### Self-Stoped
dataset_original = read.csv('/Users/yhn_hac/Hanyang University/01-2. Study_Alone/R Data Analysis/LDA (Latent Dirichlet Allocation)/LDA_vis/data/USETHIS_scopus_INT_n1321_20210108.csv', stringsAsFactors = FALSE) #1321 Data

#dataset_original <- read_csv("../input/deceptive-opinion.csv")
# #data <- Corpus(VectorSource(dataset_original$Abstract))
# ### for Self-Stoped
# data <- Corpus(VectorSource(dataset_original$Abstract_R3)) #4
# 
# library(readxl)
# dataset_original <- read_excel("/Users/yhn_hac/Hanyang University/01-2. Study_Alone/R Data Analysis/LDA (Latent Dirichlet Allocation)/LDA_vis/data/USETHIS_scopus_INT_n1326_20201231_R01.xlsx")
data <- Corpus(VectorSource(dataset_original$Abstract_R4)) #4


### 어근추출 ### <- 필요 시 사용
#data <- tm_map(data, stemDocument)
#data_stem <- tm_map(data, stemDocument)

### 
mycorp <- corpus(data)
mycorporp = corpus_reshape(mycorp, to = "paragraphs")
#save(mycorporp, file="mycorporp_4comp.rdata")

### corpus 파일에서 불용어 처리하기 ###
### Customized stopwords업로드
#stopwordsINT <- readLines(file.choose(), encoding="UTF-8")#.txt 파일만 선택하기
#stopwordsINT <- readLines("/Users/yhn_hac/Hanyang University/01-2. Study_Alone/R Data Analysis/LDA (Latent Dirichlet Allocation)/LDA Research/stopwordsINT.txt", encoding="UTF-8")#.txt 파일만 선택하기
#corpus 파일에서 불용어 처리하기
# dfm = dfm(mycorporp, remove_punct=T, remove=c(stopwords("english"), stopwordsINT))
# dfm = dfm_trim(dfm, min_docfreq = 5)


# 적어도 k=5개 이상 문서에서 나온 단어만 유지 min_docfreq = k
dfm = dfm(mycorporp, remove_punct=T, remove=stopwords("english"))
dfm = dfm_trim(dfm, min_docfreq = 5)

# ### After Corpus before LDA
# stopWords <- c()
# dtm <- DocumentTermMatrix(dfm, control=list(
#   removePunctuation=TRUE, stopwords=stopWords,
#   removeNumbers=TRUE, wordLengths=c(3, 20), weighting=weightTf))

### Dataset for LDA ###
dtm = convert(dfm, to = "topicmodels")
#dtm = convert(dfm, to = "topicmodels", stopwords=stopWords)


# dtm <- DocumentTermMatrix(mycorp, control=list(
#   removePunctuation=TRUE, stopwords=stopWords,
#   removeNumbers=TRUE, wordLengths=c(3, 20), weighting=weightTf))

#set.seed(1)
#set.seed(1234)
set.seed(1)
set.seed(77)




##############################################################################


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

# topic_num <-
#   result$topics[min(which.min(result$CaoJuan2009),
#                     which.min(result$Arun2010),
#                     which.max(result$Deveaud2014),
#                     which.max(result$Griffiths2004))]
# 
# print(paste("The optimum number of topics for the data set is ",topic_num))

# Arun, R., Suresh, V., Madhavan, C. V., & Murthy, M. N. (2010, June). On finding the natural number of topics with latent dirichlet allocation: Some observations. In Pacific-Asia conference on knowledge discovery and data mining (pp. 391-402). Springer, Berlin, Heidelberg.
# Cao, J., Xia, T., Li, J., Zhang, Y., & Tang, S. (2009). A density-based method for adaptive LDA model selection. Neurocomputing, 72(7-9), 1775-1781.
# Deveaud, R., SanJuan, E., & Bellot, P. (2014). Accurate and effective latent concept modeling for ad hoc information retrieval. Document numérique, 17(1), 61-84.
# Griffiths, T. L., & Steyvers, M. (2004). Finding scientific topics. Proceedings of the National academy of Sciences, 101(suppl 1), 5228-5235.


##############################################################################

### Main Analysis: LDA Analysis with Gibbs Sampling ###

### Intra Best N --> 14, 21 // 9, 29, 33

K = 32
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
mname <- paste0("LDAmodel_",K,"Topics", collapse = ,".Rdata")
save(m, file=mname)
m2 <- load("/Users/yhn_hac/Hanyang University/01-2. Study_Alone/R Data Analysis/LDA (Latent Dirichlet Allocation)/LDA_vis/LDAmodel_32Topics.Rdata")
#rm(LDA_Model_n1326_k32_R01)
LDA_Model_n1321_k32_R08s <- m
# m <- LDA_Model_n1326_k32_R05s

# class(m)

#토픽별 상위 30개 추출
terms(m, 30)
m #30개 추출된 결과 보기


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

serVis(json, out.dir = '/Users/yhn_hac/Library/Mobile Documents/com~apple~CloudDocs/1. R Projects/LDA_vis/Results_LDAvis/jan8_n1321_k32_R08s', open.browser = FALSE)

#serVis(json, out.dir = 'vis', open.browser = FALSE)
#serVis(json, out.dir='LDAvis_20201229_k33_a1.5', open.browser=FALSE)



### 살펴보아야 함...
##############################################################################

ldaOut.topics <- as.matrix(topics(chapters_lda))
ldaOut.terms <- as.matrix(terms(chapters_lda,6))
topicmodels2LDAvis <- function(x, ...){
  post <- topicmodels::posterior(x)
  if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
  mat <- x@wordassignments
  LDAvis::createJSON(
    phi = post[["terms"]],
    theta = post[["topics"]],
    vocab = colnames(post[["terms"]]),
    doc.length = slam::row_sums(mat, na.rm = TRUE),
    term.frequency = slam::col_sums(mat, na.rm = TRUE))}
serVis(topicmodels2LDAvis(chapters_lda)) #*토픽 간 거리 시각화

############

####################################################################################################################
####### 치과 결과부분

K <- 25
G <- 5000   # iteration : 각각 문서에 대해서 루프를 얼마나 돌리는지를 제어, to maximize is good
alpha <- 0.02

fit <- LDA(dtm, k=K, method='Gibbs', control=list(iter=G, alpha=alpha))
fit <- LDA(dtm, k=K, method='Gibbs', control=list(iter=G, alpha=alpha))


phi <- posterior(fit)$terms %>% as.matrix
theta <- posterior(fit)$topics %>% as.matrix
vocab <- colnames(phi)
#doc length
doc_length <- c()
for(i in 1:length(corpus)) {
  temp <- paste(corpus[[i]]$content, collapse=" ")
  doc_length <- c(doc_length, stri_count(temp, regex='\\S+'))
}
temp_frequency <- as.matrix(dtm)
freq_matrix <- data.frame(ST=colnames(temp_frequency),
                          Freq=colSums(temp_frequency))
rm(temp_frequency)

json_lda <- createJSON(phi=phi,
                       theta=theta,
                       vocab=vocab,
                       doc.length=doc_length,
                       term.frequency=freq_matrix$Freq)

serVis(json_lda, out.dir='2020-12-17-complaint-vis', open.browser=FALSE)
serVis(json_lda, out.dir='2020-12-17-complaint2-vis')



#LDA 결과 데이터 시각화(LDAvis 패키지)
#install.packages(“LDAvis”)
#library(LDAvis)
ldaOut.topics <- as.matrix(topics(chapters_lda))
ldaOut.terms <- as.matrix(terms(chapters_lda,6))
topicmodels2LDAvis <- function(x, ...){
  post <- topicmodels::posterior(x)
  if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
  mat <- x@wordassignments
  LDAvis::createJSON(
    phi = post[["terms"]],
    theta = post[["topics"]],
    vocab = colnames(post[["terms"]]),
    doc.length = slam::row_sums(mat, na.rm = TRUE),
    term.frequency = slam::col_sums(mat, na.rm = TRUE))}
serVis(topicmodels2LDAvis(chapters_lda)) #토픽 간 거리 시각화
###########################

