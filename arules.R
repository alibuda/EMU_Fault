library(tm)
library(xlsx)
library(reshape2)
library(plyr)
library(RWeka)
library(jiebaR)
library(arules)
setwd("E:/Ruitian")

data <- read.xlsx2('2016年动车组故障汇总.xls',sheetIndex = 1,startRow = 2,colIndex = c(2,8,10:11),stringsAsFactors = F)
data$id = 1:nrow(data)

# #Exrtact false code
# temp <- regexpr(pattern = '码：?[0-9A-Z]{4}|报：?[0-9A-Z]{4}',text = data[,1])
# f_code <- substring(data[,1], first = temp+attr(temp,"match.length")-4, last = temp+attr(temp,"match.length")-1)
# f_code <- sort(table(f_code),decreasing = T)[-1]
# output <- paste(names(f_code),'code',sep = ' ')
# writeLines(output,"f_code.txt")

data_all <- melt(data,factorsAsStrings = T,id.vars = 'id',measure.vars = names(data)[-5])
data_all <- arrange(data_all,data_all$id)
cc <- worker(user = 'f_code.txt')
temp <- sapply(data_all$value,segment,cc,USE.NAMES = F)
temp <- lapply(temp,paste,collapse = ' ')
# names(temp) <- paste(data_all$id,data_all$variable,sep = '-')


CRH_F <- VCorpus(VectorSource(temp))
CRH_F <- tm_map(CRH_F,stripWhitespace)

Ngram <- function(data,n,sparse = 0.999,filename){
    # data <- CRH_F;n <- 2;sparse = 0.999
    btm <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n))
    dtm_gram3 <- DocumentTermMatrix(data, control = list(tokenize = btm))
    dtmgram3 <- removeSparseTerms(dtm_gram3,sparse)
    termfreq_gram3 <- apply(dtmgram3,2,sum)
    termfreq_gram3 <- sort(termfreq_gram3,decreasing = T)
    names(termfreq_gram3) <- gsub(pattern = ' ',replacement = '',names(termfreq_gram3))
    output <- paste(names(termfreq_gram3),termfreq_gram3,sep = ' ')
    writeLines(output,paste("./",filename,".txt",sep = ''))
}

Ngram(CRH_F,3,filename = "gram3")
Ngram(CRH_F,2,filename = "gram2")
