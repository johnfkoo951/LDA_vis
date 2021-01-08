### Install Packages ### 
# install.packages("corpus")
# install.packages("quanteda")
# install.packages("textclean")
# install.packages("LDAvis")
# install.packages("SnowballC")
# install.packages("topicmodels")
# install.packages("ldatuning")

### Loading Packages ###
library(corpus)
library(quanteda)
library(tm)  # general text mining functions, making document term matrixes
library(textclean)
library(topicmodels)  # for LDA topic modelling 
library(SnowballC)  # for stemming
library(stringr)  # for replacement

library(LDAvis)
library(ldatuning)

##############################################################################


### Read csv file & convert to CORPUS ####
# Choose
#dataset_original = read.csv(file.choose(), stringsAsFactors = FALSE) 
# CSV
dataset_original = read.csv('/Users/yhn_hac/Hanyang University/01-2. Study_Alone/R Data Analysis/LDA (Latent Dirichlet Allocation)/LDA_vis/data/USETHIS_scopus_INT_n1321_20210108.csv', stringsAsFactors = FALSE)
# Excel
# library(readxl)
# dataset_original <- read_excel("/Users/yhn_hac/Hanyang University/01-2. Study_Alone/R Data Analysis/LDA (Latent Dirichlet Allocation)/LDA_vis/data/USETHIS_scopus_INT_n1326_20201231_R01.xlsx")

### Select Data
data <- Corpus(VectorSource(dataset_original$Abstract_R4)) 

### 어근추출 ### <- 필요 시 사용
#data <- tm_map(data, stemDocument)
#data_stem <- tm_map(data, stemDocument)

### Corpus
mycorp <- corpus(data)
mycorporp = corpus_reshape(mycorp, to = "paragraphs")

### 적어도 k=5개 이상 문서에서 나온 단어만 유지 min_docfreq = k
dfm = dfm(mycorporp, remove_punct=T, remove=stopwords("english"))
dfm = dfm_trim(dfm, min_docfreq = 5)

### Dataset for LDA ###
dtm = convert(dfm, to = "topicmodels")

set.seed(77)

##############################################################################


