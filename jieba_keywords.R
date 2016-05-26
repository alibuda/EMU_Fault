library(tm)
library(xlsx)
library(reshape2)
library(plyr)
library(RWeka)
library(jiebaR)

setwd("E:/Ruitian")

data <- read.xlsx2('2016年动车组故障汇总.xls',sheetIndex = 1,startRow = 2,colIndex = c(2,8,10:11),stringsAsFactors = F)
data$id = 1:nrow(data)

union <- read.xlsx2('故障案例库.xlsx',sheetIndex = 1,stringsAsFactors = F)
unit <- read.xlsx2('故障案例库.xlsx',sheetIndex = 2,stringsAsFactors = F,startRow = 3,colIndex = c(3:7,12:16))
fact2 <- read.xlsx2('故障案例库.xlsx',sheetIndex = 2,stringsAsFactors = F,startRow = 3,colIndex = c(8,17))
maintain <- read.xlsx2('故障案例库.xlsx',sheetIndex = 2,stringsAsFactors = F,startRow = 3,colIndex = 18)

unit <- melt(unit,na.rm = T,measure.vars = names(unit))
unit <- unique(unit$value)
unit <- unit[which(unit != '')]
output <- paste(unit,'UNIT',sep = ' ')
key_word <- readLines(file("keyword.txt","r"),encoding = "UTF-8")
key_word <- sub(pattern = " [0-9]{1,3}\\s*",replacement = ' ',key_word)
key_word <- sub(pattern = "\\s$",replacement = '',key_word)
ind <- grep(pattern = "[^A-Z]$",x = key_word)
key_word[ind] <- paste(key_word[ind],"FACT",sep = " ")
output <- unique(c(key_word,output))

fact2 <- melt(fact2,na.rm = T,measure.vars = names(fact2))
fact2 <- unique(fact2$value)
temp <- paste(fact2,'FACT',sep = ' ')
output <- unique(c(temp,output))

maintain <- unique(maintain$X.)
temp <- paste(maintain,'MAINTAIN',sep = ' ')
output <- unique(c(temp,output))

ind <- grep(pattern = "^[1-2]",x = union$编码)
fact1 <- unique(union$名称[ind])
fact1 <- paste(fact1,'FACT',sep = ' ')
maintain2 <- unique(union$名称[-ind])
maintain2 <- paste(maintain2,'MAINTAIN',sep = ' ')
output <- unique(c(fact1,maintain2,output))

ind <- grep(pattern = "^\\s",x = output)
output <- output[-ind]
output <- gsub(pattern = "^有",replacement = "",x = output)

writeLines(output,"keyword_n.txt")

#Exrtact false code
temp <- regexpr(pattern = '码：?[0-9A-Z]{4}|报：?[0-9A-Z]{4}',text = data[,1])
f_code <- substring(data[,1], first = temp+attr(temp,"match.length")-4, last = temp+attr(temp,"match.length")-1)
f_code <- sort(table(f_code),decreasing = T)[-1]
output <- paste(names(f_code),f_code,'code',sep = ' ')
writeLines(output,"f_code.txt")

data_all <- melt(data,factorsAsStrings = T,id.vars = 'id',measure.vars = names(data)[-5])
data_all <- arrange(data_all,data_all$id)
cc <- worker(user = 'keyword_n.txt')
temp <- sapply(data_all$value,segment,cc,USE.NAMES = F)
temp <- lapply(temp,paste,collapse = ' ')
tmp <- unlist(temp)

# names(temp) <- paste(data_all$id,data_all$variable,sep = '-')

#Bigram ketwords
CRH_F <- VCorpus(VectorSource(temp))
CRH_F <- tm_map(CRH_F,stripWhitespace)
Ngram <- function(data,n,sparse = 0.995,filename){
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