shinyUI(fluidPage(
    
titlePanel("Predictive Text Demo"),
tabsetPanel(type='tab',
    tabPanel("Guess Next Word",
        sidebarLayout(

            sidebarPanel(
                textInput('sent_frag',label="Your sentence:",value="To be or not to "),
                actionButton('goButton',"Guess"),
                br(),br(),
                helpText("Note:",br(), 
                         "First letter of the next word can be used as HINT to improve prediction result",
                         br(),br(),
                         "Example:",br(),
                         "Type 'b' behind 'To be or not to '. Hit 'Guess' and see the difference"),
                br(),
                uiOutput('tokens')),
        
            mainPanel(
                 plotOutput('guess',width='700px',height='500px'),
                 br(),br(),strong("Can't find your word above? See other possible choices here:"),
                 plotOutput('wordcloud',width='600px',height='500px'))
        )
    ),
    
    tabPanel("About the Training Corpus",
         h3("Most Common Words, Bigrams & Trigrams"),
         plotOutput('top12gram',width='1000px',height='500px'),
         plotOutput('top34gram',width='1000px',height='500px'),
         h3("Most Common Words to Start & End a Sentence"),
         plotOutput('firstNlastWord',width='1000px',height='500px'),
         plotOutput('firstNlast2Words',width='1000px',height='500px'),
         h3("Word Appearance Follows a Zipf's Law Power Distribution"),
         plotOutput('ZipfLaw',width='500px',height='500px')
    )
)
))