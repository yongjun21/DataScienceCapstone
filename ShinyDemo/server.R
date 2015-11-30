shinyServer(function(input, output, session) {
    tokens <- reactive(custom_tokenizer(input$sent_frag))
    
    output$tokens <- renderTable({
        
        tokenized <- tokens()
        hint <- tokenized$hint
        tokenized <- tokenized$tokens
        tokenized[tokenized=='BREAK'] <- '.'
        tokenized <- c(tokenized,paste0(hint,'...'))
        as.data.frame(tokenized)
    })
    
    prediction <- reactive({
        
        input$goButton
        
        tks <- isolate(tokens())
        hint <- tks$hint
        tks <- tks$tokens
        if(length(tks)<3) {tks <- c(rep('BREAK',3-length(tks)),tks)}
        w1 <- match(tks[length(tks)-2],vcb$w)
        w2 <- match(tks[length(tks)-1],vcb$w)
        w3 <- match(tks[length(tks)],vcb$w)
        
        Pbo <- backoffRoot(w1,w2,w3)$Pbo
        Pkn <- KneserNeyRoot(w1,w2,w3)$Pkn
        Pavg <- (Pbo+Pkn)/2
        w <- vcb$w
        w[KEYWORDS$BREAK] <- '.'
        df <- data.frame(w,Pbo,Pkn,Pavg)
        df <- df[-c(KEYWORDS$UNK,KEYWORDS$NUM),]
        if(!is.null(hint)) {
            df <- df[grepl(paste0('^',hint),df$w),]
            df <- transform(df,Pbo=Pbo/sum(Pbo),Pkn=Pkn/sum(Pkn),Pavg=(Pbo+Pkn)/2)
        }
        df <- arrange(df,desc(Pavg))
    })
    
    output$guess <- renderPlot({
        
        top10 <- prediction()[1:10,1:3]
        top10[top10$w=='BREAK',1] <- '.'
        top10 <- melt(top10)
        levels(top10$variable) <- c("Katz's backoff","Kneser-Ney")
        ggplot(top10,aes(x=factor(w,unique(w)),y=value,fill=variable))+
            geom_bar(stat='identity',position='dodge')+
            labs(title='Top 10 best guess',y='Probability',x='Words',fill='Method')+
            scale_y_continuous(labels=percent)+
            theme(axis.text.x=element_text(angle=90,hjust=1,size=12))
            
        
    })
    
    output$wordcloud <- renderPlot({
        
        top100 <- prediction()[1:60,]
        wordcloud(top100$w,
                  top100$Pavg,
                  scale=c(10,1),
                  rot.per=0.3,
                  colors=brewer.pal(8, "Dark2"),
                  random.order=FALSE,
                  random.color=TRUE)
    })
    
    output$top12gram <- renderPlot({
        
        par(mfrow=c(1,2))
        with(preCleaned$topWords,barplot(freq,names.arg=w,horiz=TRUE,las=1,main='Words'))
        with(preCleaned$topBigrams,barplot(freq,names.arg=paste(w1,w2),horiz=TRUE,las=1,main='Bigrams'))
    })
    
    output$top34gram <- renderPlot({
        
        par(mfrow=c(1,2),mar=c(5,8,4,2))
        with(preCleaned$topTrigrams,barplot(freq,names.arg=paste(w1,w2,w3),horiz=TRUE,las=1,main='Trigrams'))
        with(preCleaned$top4grams,barplot(freq,names.arg=paste(w1,w2,w3,w4),horiz=TRUE,las=1,main='4-grams'))
    })
    
    output$firstNlastWord <- renderPlot({
        
        par(mfrow=c(1,2))
        with(preCleaned$firstWord,barplot(freq,names.arg=w2,horiz=TRUE,las=1,main='First Word'))
        with(preCleaned$lastWord,barplot(freq,names.arg=w1,horiz=TRUE,las=1,main='Last Word'))
    })
    
    output$firstNlast2Words <- renderPlot({
        
        par(mfrow=c(1,2),mar=c(5,5,4,2))
        with(preCleaned$first2Words,barplot(freq,names.arg=paste(w2,w3),horiz=TRUE,las=1,main='First 2-words'))
        with(preCleaned$last2Words,barplot(freq,names.arg=paste(w1,w2),horiz=TRUE,las=1,main='Last 2-words'))
    })
    
    output$ZipfLaw <- renderPlot({
        
        SortedFreqPlot(vcb$freq)
    })
    
})
