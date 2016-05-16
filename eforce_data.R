library(tm)
library(xlsx)
library(Rwordseg)
setwd("E:/Ruitian")

data <- read.xlsx2('2016年动车组故障汇总.xls',sheetIndex = 1,startRow = 2,colIndex = c(2,8,10:11),stringsAsFactors = F)


keywords <- read.csv("keyword.csv",header = F,stringsAsFactors = F)
insertWords(unlist(keywords))
stopword <- c('车','故障')

n <- 2;sparse <- 0.99
for(n in 1:4){
    data_all <- apply(data[,1:n], 1, paste,collapse = ' ')
    temp <- segmentCN(data_all,returnType = 'tm')
    CRH_F <- VCorpus(VectorSource(temp))
    CRH_F <- tm_map(CRH_F,stripWhitespace)
    CRH_F <- tm_map(CRH_F,removeNumbers)
    CRH_F <- tm_map(CRH_F,removePunctuation)
    CRH_F <- tm_map(CRH_F,removeWords,stopword)
    dtm2 <- DocumentTermMatrix(CRH_F,control=list(removePunctuation=T,wordLengths = c(1, Inf),weighting = weightTfIdf,minDocFreq=5))
    # dtm2 <- DocumentTermMatrix(CRH_F,control=list(removePunctuation=T,wordLengths = c(1, Inf)))
    dtm3 <- removeSparseTerms(dtm2,sparse)
    dt <- as.matrix(dtm3)
    # d <- dist(dtm3,method="euclidean")
    # hclustRes <- hclust(d,method="complete")
    # hclustRes.type <- cutree(hclustRes,k=10)
    # hlzj.hclustRes <- list(content=temp,type=hclustRes.type)
    # hlzj.hclustRes <- as.data.frame(hlzj.hclustRes)
    # fix(hlzj.hclustRes)
    d <- as.dist(1 - cor(dt))
    dn <- abs(1 - as.matrix(d))
    dn[dn < 0.1] <- 0 #关联系数过小的直接清除
    temp <- c(1:ncol(dt))
    temp <- cbind(temp,temp)
    dn[temp] <- 0
    assign(paste("dn",n,sep=''),dn*100)  #修改系数
}

save(list = ls(pattern = 'dn[0-9]'),file = 'dn.RData')

