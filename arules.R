library(tm)
library(xlsx)
library(reshape2)
library(plyr)
library(jiebaR)
library(arules)
setwd("E:/Ruitian")

data <- read.xlsx2('2016年动车组故障汇总.xls',sheetIndex = 1,startRow = 2,colIndex = c(2,8,10:11),stringsAsFactors = F)
data$id = 1:nrow(data)

# #Exrtact false code
# temp <- regexpr(pattern = '码：?[0-9A-Z]{4}|报：?[0-9A-Z]{4}',text = data[,1])
# f_code <- substring(data[,1], first = temp+attr(temp,"match.length")-4, last = temp+attr(temp,"match.length")-1)
# f_code <- sort(table(f_code),decreasing = T)[-1]
# output <- paste(names(f_code),f_code,'code',sep = ' ')
# writeLines(output,"f_code.txt")

data_all <- melt(data,factorsAsStrings = T,id.vars = 'id',measure.vars = names(data)[-5])
data_all <- arrange(data_all,data_all$id)
cc <- worker(user = 'keyword_n.txt')
temp <- sapply(data_all$value,segment,cc,USE.NAMES = F)
temp <- lapply(temp,paste,collapse = ' ')
tmp <- unlist(temp)
# names(temp) <- paste(data_all$id,data_all$variable,sep = '-')

# #Bigram ketwords
# CRH_F <- VCorpus(VectorSource(temp))
# CRH_F <- tm_map(CRH_F,stripWhitespace)
# Ngram <- function(data,n,sparse = 0.995,filename){
#     # data <- CRH_F;n <- 2;sparse = 0.999
#     btm <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n))
#     dtm_gram3 <- DocumentTermMatrix(data, control = list(tokenize = btm))
#     dtmgram3 <- removeSparseTerms(dtm_gram3,sparse)
#     termfreq_gram3 <- apply(dtmgram3,2,sum)
#     termfreq_gram3 <- sort(termfreq_gram3,decreasing = T)
#     names(termfreq_gram3) <- gsub(pattern = ' ',replacement = '',names(termfreq_gram3))
#     output <- paste(names(termfreq_gram3),termfreq_gram3,sep = ' ')
#     writeLines(output,paste("./",filename,".txt",sep = ''))
# }
# 
# Ngram(CRH_F,3,filename = "gram3")
# Ngram(CRH_F,2,filename = "gram2")

stopword <- c('车','故障','代码','原因','现象','维修','手段','维护','工序')

sub_ind <- c('-f','-r')

for (i in 1:length(sub_ind)){
    # i <- 1
    ind <- which(data_all$variable == names(data)[i+1])
    crh_f <- VCorpus(VectorSource(tmp[ind]))
    crh_f <- tm_map(crh_f,removeWords,stopword)
    crh_f <- tm_map(crh_f,stripWhitespace)
    # dtm <- DocumentTermMatrix(crh_f)
    # dtm <- removeSparseTerms(dtm,0.995)
    # termfreq <- apply(dtm,2,sum)
    # names(termfreq) <- toupper(names(termfreq))
    # termfreq <- sort(termfreq,decreasing = T)
    # termfreq
    output <- unlist(lapply(crh_f,as.character))
    ind <- grep(pattern = '^\\s',output)
    output[ind] <- sub(pattern = '\\s+',replacement = '',output[ind])
    output <- gsub(pattern = '\\s+',replacement = paste(sub_ind[i],' ',sep = ''),output)
    ind <- grep(pattern = '\\s$',output)
    output[-ind] <- paste(output[-ind],sub_ind[i],sep = '')
    writeLines(text = output,con = paste("trans",i,".txt",sep = ''))
    assign(paste("output",i,sep = ''),value = output)
}
output <- paste(output1,output2,sep = ' ')
writeLines(text = output,con = "trans.txt")

key_word <- readLines(file("keyword_n.txt",encoding = "UTF-8"),encoding = "UTF-8")
key_word <- strsplit(key_word,split = ' ')
dict <- data.frame(WORD = unlist(lapply(key_word,function(x) x[1])),
                   TYPE = unlist(lapply(key_word,function(x) x[2])))

trans <- read.transactions('trans.txt',format = "basket",rm.duplicates = T)
itemFreq <- itemFrequency(trans)
summary(itemFreq)
image(trans)
trans_rules <- apriori(trans, parameter = list(support = 0.0013,confidence = 0.25,minlen = 3))
trans1_rules <- sort(trans_rules,by='lift',decreasing = T)
sub_rules <- subset(trans1_rules, (!lhs %pin% c("-r"))&(!rhs %pin% c("-f")),minlen = 3)
summary(sub_rules)
rules1 <- subset(trans1_rules, lhs %pin% c("塞拉门"))
inspect(rules1)
