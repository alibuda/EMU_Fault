library(tm)
library(xlsx)
library(Rwordseg)
library(wordcloud)
library(RColorBrewer)
library(recharts)
setwd("E:/Ruitian")
source('echartR.R')

data <- read.xlsx2('2016年动车组故障汇总.xls',sheetIndex = 1,startRow = 2,colIndex = c(2,8,10:11),stringsAsFactors = F)


keywords <- read.csv("keyword.csv",header = F,stringsAsFactors = F)
insertWords(unlist(keywords))
stopword <- c('车','故障','代码','原因','现象')

n <- 1;sparse <- 0.99
data_all <- apply(data[,4], 1, paste,collapse = ' ')
temp <- segmentCN(unlist(data[,4]),returnType = 'tm')
CRH_F <- VCorpus(VectorSource(temp))
CRH_F <- tm_map(CRH_F,stripWhitespace)
CRH_F <- tm_map(CRH_F,removeNumbers)
CRH_F <- tm_map(CRH_F,removePunctuation)
CRH_F <- tm_map(CRH_F,removeWords,stopword)

dtm <- DocumentTermMatrix(CRH_F,control=list(removePunctuation=T,wordLengths = c(1, Inf)))
dtm2 <- removeSparseTerms(dtm,sparse)
termfreq <- apply(dtm2,2,sum)
termfreq <- sort(termfreq,decreasing = T)
# write.table(termfreq,'wordfreq.txt')
# write.xlsx(termfreq,'wordfreq.xlsx')
write.xlsx(termfreq,'repair.xlsx')

rc <- brewer.pal(5,"Set1") 
windowsFonts(A = windowsFont("幼圆")) 
par(family = "A")
wordcloud(names(termfreq), termfreq, col = rc,random.color=TRUE,random.order = F,scale=c(2,.2))



