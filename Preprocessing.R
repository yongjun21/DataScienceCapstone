library(tm)
library(openNLP)
set.seed(628)


# loading in the data
conB <- file('raw/en_US/en_US.blogs.txt','rb')
conN <- file('raw/en_US/en_US.news.txt','rb')
conT <- file('raw/en_US/en_US.twitter.txt','rb')

rawB <- readLines(conB,encoding='UTF-8',skipNul=TRUE)
rawN <- readLines(conN,encoding='UTF-8',skipNul=TRUE)
rawT <- readLines(conT,encoding='UTF-8',skipNul=TRUE)

close(conB)
close(conN)
close(conT)
rm(conB,conN,conT)

n <- c(length(rawB),length(rawN),length(rawT))
resample <- split(1:sum(n),sample(1:100,sum(n),replace=TRUE))

preProcess <- function(idx) {
    # split into sentences
    Sent_Annotator <- Maxent_Sent_Token_Annotator()
    corpus <- lapply(c(rawB,rawN,rawT)[resample[[idx]]],
                     function(x) as.String(x)[annotate(x,Sent_Annotator)])
    corpus <- unlist(corpus)

    # remove non-printable char
    corpus <- gsub('[^[:print:]]','',corpus)
    # convert lowercase
    corpus <- tolower(corpus)
    # replace numbers with the word 'NUM'
    corpus <- gsub('\\d+[[:punct:]]*\\s*\\d*',' NUM ',corpus)
    # replace dash with space
    corpus <- gsub('-',' ',corpus)
    # and remove other punctuations
    corpus <- removePunctuation(corpus)
    # clean up extra space
    corpus <- stripWhitespace(corpus)
    #remove empty rows
    corpus <- corpus[grepl('[[:alpha:]]',corpus)]
    #insert 'BREAK' between sentences
    corpus <- paste(corpus,' BREAK')
    corpus <- c('BREAK',corpus)

    tokens <- scan_tokenizer(corpus)
    write(tokens,paste('res/tokens_',idx,'.txt',sep=''))
    tokens
}

for(i in 1:3) {preProcess(i)}

sent <- NULL
word <- NULL
for(k in 1:100) {
    tks <- readLines(paste('res/tokens_',k,'.txt',sep=''))
    word <- c(word,length(tks))
    sent <- c(sent,length(which(tks=='BREAK')))
}
word <- word-sent
sent <- sent-1
sum(word)
sum(sent)
