library(shiny)
library(plyr)
library(reshape2)
library(data.table)
library(ggplot2)
library(scales)
library(wordcloud)
library(tm)
library(openNLP)
load("LM.RData")

custom_tokenizer <- function(str) {
    if(str=='') str='.'
    hint <- NULL
    if(grepl(' [a-z]$',str)) {
        hint <- substr(str,nchar(str),nchar(str))
        str <- substr(str,1,nchar(str)-2)
    }
    Sent_Annotator <- Maxent_Sent_Token_Annotator()
    sent <- as.character(as.String(str)[annotate(str,Sent_Annotator)])
    
    sent <- gsub('[^[:print:]]','',sent)
    sent <- tolower(sent)
    sent <- gsub('\\d+[[:punct:]]*\\s*\\d*',' NUM ',sent)
    sent <- gsub('-',' ',sent)
    sent <- removePunctuation(sent)
    sent <- stripWhitespace(sent)
    sent <- sent[grepl('[[:alpha:]]',sent)]
    sent <- paste('BREAK ',sent)

    tks <- scan_tokenizer(sent)
    list(tokens=tks[-1],hint=hint)
}

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

SortedFreqPlot <- function(tf){
    y <- sort(tf,decreasing=TRUE)
    x <- seq_along(y)
    fit <- lm(log(y)~log(x),weights=y)
    fitted_y <- exp(predict(fit,as.data.frame(log(x))))
    plot(x,y,type='l',log='xy',
         xlab=expression(paste('k'^'th',' most common word')),
         ylab='Occurence')
    lines(x,fitted_y,col='red',lwd=2)
}