require(plyr)
require(data.table)

preCleaning <- function(){
  
  topWords <- vcb[-unlist(KEYWORDS)]
  setkey(topWords,freq)
  topWords <- topWords[-(1:(nrow(topWords)-30))]
  
  setkey(bg,freq)
  firstWord <- bg[bg$w1==KEYWORDS$BREAK]
  firstWord <- firstWord[-(1:(nrow(firstWord)-30))]
  lastWord <- bg[bg$w2==KEYWORDS$BREAK,]
  lastWord <- lastWord[-(1:(nrow(lastWord)-30))]
  topBigrams <- bg[bg$w1!=KEYWORDS$BREAK & bg$w2!=KEYWORDS$BREAK]
  topBigrams <- topBigrams[-(1:(nrow(topBigrams)-30))]
  topBigrams$w1 <- vcb[topBigrams$w1,w]
  topBigrams$w2 <- vcb[topBigrams$w2,w]
  firstWord$w2 <- vcb[firstWord$w2,w]
  lastWord$w1 <- vcb[lastWord$w1,w]
  setkey(bg,w1)
  
  setkey(tg,freq)
  first2Words <- tg[tg$w1==KEYWORDS$BREAK & tg$w3!=KEYWORDS$BREAK]
  first2Words <- first2Words[-(1:(nrow(first2Words)-30))]
  last2Words <- tg[tg$w1!=KEYWORDS$BREAK & tg$w3==KEYWORDS$BREAK]
  last2Words <- last2Words[-(1:(nrow(last2Words)-30))]
  topTrigrams <- tg[tg$w1!=KEYWORDS$BREAK & tg$w3!=KEYWORDS$BREAK]
  topTrigrams <- topTrigrams[-(1:(nrow(topTrigrams)-30))]
  topTrigrams$w1 <- vcb[topTrigrams$w1,w]
  topTrigrams$w2 <- vcb[topTrigrams$w2,w]
  topTrigrams$w3 <- vcb[topTrigrams$w3,w]
  first2Words$w2 <- vcb[first2Words$w2,w]
  first2Words$w3 <- vcb[first2Words$w3,w]
  last2Words$w1 <- vcb[last2Words$w1,w]
  last2Words$w2 <- vcb[last2Words$w2,w]
  setkey(tg,w1,w2)
  
  setkey(qg,freq)
  top4grams <- qg[qg$w1!=KEYWORDS$BREAK & qg$w4!=KEYWORDS$BREAK]
  top4grams <- top4grams[-(1:(nrow(top4grams)-30))]
  top4grams$w1 <- vcb[top4grams$w1,w]
  top4grams$w2 <- vcb[top4grams$w2,w]
  top4grams$w3 <- vcb[top4grams$w3,w]
  top4grams$w4 <- vcb[top4grams$w4,w]
  setkey(qg,w1,w2,w3)
  
  list(topWords=topWords,topBigrams=topBigrams,
       topTrigrams=topTrigrams,top4grams=top4grams,
       firstWord=firstWord,lastWord=lastWord,
       first2Words=first2Words,last2Words=last2Words)
}

preCleaned <- preCleaning()

backoffRoot <- function(w1,w2,w3) {

    df <- data.frame(w=1:nrow(vcb),
                     prob1=rep(NA,nrow(vcb)),
                     prob2=rep(NA,nrow(vcb)),
                     prob3=rep(NA,nrow(vcb)),
                     prob4=rep(NA,nrow(vcb)))
    beta1 <- 1
    beta2 <- 1
    beta3 <- 1

    if(!is.na(w1) & !is.na(w2) & !is.na(w3)
       & w2!=KEYWORDS$BREAK & w3!=KEYWORDS$BREAK){
        w <- list(w1,w2,w3)
        firstCut <- qg[w]
        firstCut <- d_qg(firstCut)
        df$prob1[firstCut$w4] <- firstCut$d*firstCut$freq/sum(firstCut$freq)
        beta1 <- 1-sum(df$prob1,na.rm=TRUE)
    }

    if(!is.na(w2) & !is.na(w3)
       & w3!=KEYWORDS$BREAK) {
        w <- list(w2,w3)
        secondCut <- tg[w]
        secondCut <- d_tg(secondCut)
        df$prob2[secondCut$w3] <- secondCut$d*secondCut$freq/sum(secondCut$freq)
        beta2 <- 1-sum(df$prob2,na.rm=TRUE)
    }

    if(!is.na(w3)) {
        w <- list(w3)
        thirdCut <- bg[w]
        thirdCut <- d_bg(thirdCut)
        df$prob3[thirdCut$w2] <- thirdCut$d*thirdCut$freq/sum(thirdCut$freq)
        beta3 <- 1-sum(df$prob3,na.rm=TRUE)
    }

    df$prob4 <- vcb$freq/sum(vcb$freq)

    alpha1 <- beta1/(1-sum(df$prob2[!is.na(df$prob1)],na.rm = TRUE))
    alpha2 <- beta2/(1-sum(df$prob3[!is.na(df$prob2)],na.rm = TRUE))
    alpha3 <- beta3/(1-sum(df$prob4[!is.na(df$prob3)],na.rm = TRUE))

    df$Pbo <- ifelse(!is.na(df$prob1),df$prob1,
                     alpha1*ifelse(!is.na(df$prob2),df$prob2,
                                   alpha2*ifelse(!is.na(df$prob3),df$prob3,
                                                 alpha3*df$prob4)))

    df
}

KneserNeyRoot <- function(w1,w2,w3) {

    df <- data.frame(w=1:nrow(vcb),
                     prob1=rep(0,nrow(vcb)),
                     prob2=rep(0,nrow(vcb)),
                     prob3=rep(0,nrow(vcb)),
                     prob4=rep(0,nrow(vcb)))
    alpha1 <- 1
    alpha2 <- 1
    alpha3 <- 1

    if(!is.na(w1) & !is.na(w2) & !is.na(w3)
       & w2!=KEYWORDS$BREAK & w3!=KEYWORDS$BREAK){
        w <- list(w1,w2,w3)
        firstCut <- qg[w]
        firstCut <- D_qg(firstCut)
        df$prob1[firstCut$w4] <- (firstCut$freq-firstCut$D)/sum(firstCut$freq)
        alpha1 <- 1-sum(df$prob1)
    }

    if(!is.na(w2) & !is.na(w3)
       & w3!=KEYWORDS$BREAK) {
        w <- list(w2,w3)
        secondCut <- contiTri[w]
        secondCut <- D_contiTri(secondCut)
        df$prob2[secondCut$w4] <- (secondCut$freq-secondCut$D)/sum(secondCut$freq)
        alpha2 <- 1-sum(df$prob2)
    }

    if(!is.na(w3)) {
        w <- list(w3)
        thirdCut <- contiBi[w]
        thirdCut <- D_contiBi(thirdCut)
        df$prob3[thirdCut$w3] <- (thirdCut$freq-thirdCut$D)/sum(thirdCut$freq)
        alpha3 <- 1-sum(df$prob3)
    }

    df$prob4[contiUni$w2] <- contiUni$freq/sum(contiUni$freq)

    df$Pkn <- df$prob1+alpha1*(df$prob2+alpha2*(df$prob3+alpha3*df$prob4))

    df
}

backoff <- function(w1,w2,w3) {
    w1 <- match(w1,vcb$w)
    w2 <- match(w2,vcb$w)
    w3 <- match(w3,vcb$w)
    df <- backoffRoot(w1,w2,w3)
    df$w <- vcb$w
    arrange(df,desc(Pbo))
}

KneserNey <- function(w1,w2,w3) {
    w1 <- match(w1,vcb$w)
    w2 <- match(w2,vcb$w)
    w3 <- match(w3,vcb$w)
    df <- KneserNeyRoot(w1,w2,w3)
    df$w <- vcb$w
    arrange(df,desc(Pkn))
}
