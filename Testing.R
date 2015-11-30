require(plyr)
require(data.table)
require(ggplot2)
require(reshape2)
require(scales)

predictProb <- function(w1,w2,w3,w4) {
    w1 <- match(w1,vcb$w)
    w2 <- match(w2,vcb$w)
    w3 <- match(w3,vcb$w)
    w4 <- match(w4,vcb$w)
    w4[is.na(w4)] <- KEYWORDS$UNK
    probBO <- backoffRoot(w1,w2,w3)$Pbo[w4]
    probKN <- KneserNeyRoot(w1,w2,w3)$Pkn[w4]
    names(probBO) <- vcb$w[w4]
    names(probKN) <- vcb$w[w4]
    list(BO=sort(probBO,decreasing=TRUE),KN=sort(probKN,decreasing=TRUE))
}

tks <- readLines('res/tokens_77.txt')
tks <- tks[1:which(tks=='BREAK')[1001]]
tks <- match(tks,vcb$w)
tks[is.na(tks)] <- KEYWORDS$UNK
test <- build_ngram(tks,4)
setkey(test,w1,w2,w3)
test_keys <- unique(test[,.(w1,w2,w3)])

for(i in 1:nrow(test_keys)) {
    if(i%%100==0) print(i)
    w1 <- test_keys[i,w1]
    w2 <- test_keys[i,w2]
    w3 <- test_keys[i,w3]
    bo <- backoffRoot(w1,w2,w3)$Pbo
    kn <- KneserNeyRoot(w1,w2,w3)$Pkn
    w <- list(w1,w2,w3)
    test[w,Pbo:=bo[w4]]
    test[w,Pkn:=kn[w4]]
}

test <- test[test$Pbo>0 & test$Pkn>0]
test <- test[!(is.na(test$Pbo) | is.na(test$Pkn))]
bo <- 2^(-sum(log(test$Pbo,2)*test$freq)/sum(test$freq))
kn <- 2^(-sum(log(test$Pkn,2)*test$freq)/sum(test$freq))

perplexity_N <- readRDS('perplexity_N.RDS')
perplexity_S <- readRDS('perplexity_S.RDS')

ggplot(perplexity_N,aes(x=N_gram,y=Perplexity,fill=Method))+
  geom_bar(stat='identity',position='dodge')+
  scale_y_continuous(trans="log2")
dcast(perplexity_N,Method~N_gram)

ggplot(perplexity_S,aes(x=Sample,y=Perplexity,color=Method,shape=Method))+
  geom_line()+geom_point(size=3)+
  scale_x_continuous(breaks=c(0.01,0.03,0.05,0.1,0.15,0.2),labels=percent)
