chain_tokens <- function(idx) {
    tokens <- 'BREAK'
    for(k in idx) {
        tks <- readLines(paste('res/tokens_',k,'.txt',sep=''))[-1]
        tokens <- c(tokens,tks)
    }
    tokens
}

get_keywords <- function(vcb){
    list(BREAK=match('BREAK',vcb$w),
         UNK=match('UNK',vcb$w),
         NUM=match('NUM',vcb$w))
}

build_ngram <- function(tokens,ngram) {
    tks <- as.data.frame(tokens)
    for(i in 2:ngram) tks <- data.frame(tks[-nrow(tks),],tks[-1,ncol(tks)])
    names(tks) <- paste('w',1:ngram,sep='')
    as.data.table(count(tks))
}

# Simple Good-Turing smoothing function
SGT <- function(tf) {
    
    Nr <- table(tf)
    r <- as.integer(names(Nr))
    len <- length(r)
    if(len<=2) return(list(p0=1,pr=NA))
    Nr1 <-  Nr[match(r+1,r)]
    Nr1[is.na(Nr1)] <- 0
    
    q <- c(0,r[-len])
    t <- c(r[-1],2*r[len]-q[len])
    Zr <- Nr/0.5/(t-q)
    
    df <- data.frame(r,Zr)
    fit <- lm(log(Zr)~log(r),data=df)
    Sr <- exp(predict(fit,df))
    Sr1 <- exp(predict(fit,transform(df,r=r+1)))
    
    x <- (r+1)*Nr1/Nr
    y <- (r+1)*Sr1/Sr
    rStar <- ifelse(cummax(abs(x-y) > 1.65*sqrt((1+r)*x/Nr*(1+Nr1/Nr))),y,x)
    
    N <- sum(Nr*r)
    p0 <- Nr[1]/N
    pr <- rStar/N
    pr <- (1-p0)*pr/sum(pr*Nr)
    Dr <- round((r-rStar)*Nr)
    
    #par(mfrow=c(1,2))
    #plot(r,Nr,log='xy')
    #plot(r,Zr,log='xy')
    #lines(r,Sr,col='red',lwd=2)
    
    list(r=r,Nr=Nr,rStar=rStar,pr=pr,Dr=Dr,p0=p0)
}

tks <- chain_tokens(1:10)
vcb <- as.data.table(count(data.frame(w=tks)))
vcb$w <- as.character(vcb$w)
vcb <- vcb[!grepl('[^A-Za-z]',w)]
vcb <- rbind(vcb,list('UNK',nrow(vcb)+1))
vcb$freq <- vcb$freq-1
vcb <- vcb[freq>0]
setkey(vcb,w)

KEYWORDS <- get_keywords(vcb)
tks <- match(tks,vcb$w)
tks[is.na(tks)] <- KEYWORDS$UNK
bg <- build_ngram(tks,2)
tg <- build_ngram(tks,3)
qg <- build_ngram(tks,4)

contiUni <- as.data.table(count(bg[,1:2,with=FALSE],'w2'))
contiBi <- as.data.table(count(tg[,1:3,with=FALSE],c('w2','w3')))
tg <- tg[w2!=KEYWORDS$BREAK]
qg <- qg[w3!=KEYWORDS$BREAK]
contiTri <- as.data.table(count(qg[,1:4,with=FALSE],c('w2','w3','w4')))
qg <- qg[w2!=KEYWORDS$BREAK]

bg <- bg[w1!=KEYWORDS$UNK]
bg <- bg[w2!=KEYWORDS$UNK]
tg <- tg[w1!=KEYWORDS$UNK]
tg <- tg[w2!=KEYWORDS$UNK]
tg <- tg[w3!=KEYWORDS$UNK]
qg <- qg[w1!=KEYWORDS$UNK]
qg <- qg[w2!=KEYWORDS$UNK]
qg <- qg[w3!=KEYWORDS$UNK]
qg <- qg[w4!=KEYWORDS$UNK]
contiBi <- contiBi[w2!=KEYWORDS$UNK]
contiBi <- contiBi[w3!=KEYWORDS$UNK]
contiTri <- contiTri[w2!=KEYWORDS$UNK]
contiTri <- contiTri[w3!=KEYWORDS$UNK]
contiTri <- contiTri[w4!=KEYWORDS$UNK]
bg <- bg[w1!=KEYWORDS$NUM]
bg <- bg[w2!=KEYWORDS$NUM]
tg <- tg[w1!=KEYWORDS$NUM]
tg <- tg[w2!=KEYWORDS$NUM]
tg <- tg[w3!=KEYWORDS$NUM]
qg <- qg[w1!=KEYWORDS$NUM]
qg <- qg[w2!=KEYWORDS$NUM]
qg <- qg[w3!=KEYWORDS$NUM]
qg <- qg[w4!=KEYWORDS$NUM]
contiBi <- contiBi[w2!=KEYWORDS$NUM]
contiBi <- contiBi[w3!=KEYWORDS$NUM]
contiTri <- contiTri[w2!=KEYWORDS$NUM]
contiTri <- contiTri[w3!=KEYWORDS$NUM]
contiTri <- contiTri[w4!=KEYWORDS$NUM]

smoothBG <- SGT(bg$freq)
smoothTG <- SGT(tg$freq)
smoothQG <- SGT(qg$freq)

C <- 6*smoothBG$Nr[6]/smoothBG$Nr[1]
d <- (smoothBG$rStar[1:5]/1:5-C)/(1-C)
print(d)
d_bg <- discount(d)
C <- 6*smoothTG$Nr[6]/smoothTG$Nr[1]
d <- (smoothTG$rStar[1:5]/1:5-C)/(1-C)
print(d)
d_tg <- discount(d)
C <- 6*smoothQG$Nr[6]/smoothQG$Nr[1]
d <- (smoothQG$rStar[1:5]/1:5-C)/(1-C)
d_qg <- discount(d)
print(d)

Y <- smoothQG$Nr[1]/(smoothQG$Nr[1]+2*smoothQG$Nr[2])
D <- 1:3-2:4*Y*smoothQG$Nr[2:4]/smoothQG$Nr[1:3]
print(D)
D_qg <- discount(D)
Nr <- table(contiTri$freq)
Y <- Nr[1]/(Nr[1]+2*Nr[2])
D <- 1:3-2:4*Y*Nr[2:4]/Nr[1:3]
print(D)
D_contiTri <- discount(D)
Nr <- table(contiBi$freq)
Y <- Nr[1]/(Nr[1]+2*Nr[2])
D <- 1:3-2:4*Y*Nr[2:4]/Nr[1:3]
print(D)
D_contiBi <- discount(D)

setkey(bg,w1)
setkey(tg,w1,w2)
setkey(qg,w1,w2,w3)
setkey(contiUni,w2)
setkey(contiBi,w2)
setkey(contiTri,w2,w3)

discount <- function(dis) {
    if(length(dis)==3) {
        function(tb) {
            tb$D <- dis[3]
            tb[tb$freq==2,D:=dis[2]]
            tb[tb$freq==1,D:=dis[1]]
            tb
        }
    } else {
        function(tb) {
            tb$d <- 1
            for(i in 1:5) tb[tb$freq==i,d:=dis[i]]
            tb
        }
    }
}
