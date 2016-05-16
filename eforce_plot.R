setwd("E:/Ruitian")
load('dn2.RData')
library(recharts)

dn <- dn2
temp <- as.vector(dn)
temp <- temp[temp!=0]
summary(temp)

for(i in 1:nrow(dn)){
    for(j in 1:nrow(dn))
        dn[i,j] <- ifelse(dn[i,j]<12,0,dn[i,j])
}

plot(eForce(dn))
