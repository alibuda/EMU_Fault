library(tm)
library(xlsx)
library(RWeka)
library(Rwordseg)
setwd("E:/Ruitian")

data <- read.xlsx2('2016年动车组故障汇总.xls',sheetIndex = 1,startRow = 2,colIndex = c(2,8,10:11),stringsAsFactors = F)
data_all <- apply(data, 1, paste,collapse = ' ')

# installDict("动车组工作1.scel",dictname = "dongche")
keywords <- read.csv("keyword.csv",header = F,stringsAsFactors = F)
insertWords(unlist(keywords))
stopword <- c('车','故障','代码','原因','现象')
temp <- segmentCN(data_all,returnType = 'tm')
CRH_F <- VCorpus(VectorSource(temp))
CRH_F <- tm_map(CRH_F,stripWhitespace)
CRH_F <- tm_map(CRH_F,removeNumbers)
CRH_F <- tm_map(CRH_F,removePunctuation)
CRH_F <- tm_map(CRH_F,removeWords,stopword)


Ngram <- function(data,n,sparse = 0.99,filename){
    btm <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n))
    dtm_gram3 <- DocumentTermMatrix(data, control = list(tokenize = btm))
    dtmgram3 <- removeSparseTerms(dtm_gram3,sparse)
    termfreq_gram3 <- apply(dtmgram3,2,sum)
    termfreq_gram3 <- sort(termfreq_gram3,decreasing = T)
    names(termfreq_gram3) <- gsub(pattern = ' ',replacement = '',names(termfreq_gram3))
    write.table(termfreq_gram3,file = paste("./",filename,".txt",sep = ''))
}

Ngram(CRH_F,3,filename = "gram3")
Ngram(CRH_F,2,filename = "gram2")



