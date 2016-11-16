library(shiny)
shinyUI(
  fluidPage(
    titlePanel("Interactive Twitter Analytics"),
    
    sidebarLayout(
      sidebarPanel(
        textInput(inputId = "key_word", 
                  label = "Enter your key words", 
                  value = "Search Twitter"
        ),
        
        sliderInput("numOfTweetes",
                    "Number of tweets",
                    min = 0,  max = 10000, value = 1000),
        
        actionButton("search", label = "Get Tweets!", icon = icon("search", lib = "font-awesome")),
        
        hr(),
        
        sliderInput("numOfTopics",
                    "Number of topics",
                    min = 0,  max = 20, value = 5),
        
        sliderInput("numOfTopTerms",
                    "Number of top terms",
                    min = 5,  max = 20, value = 6)
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Wordcloud", plotOutput(outputId = "word_map", height = "500px")),
          tabPanel("Histogram of Retweet Count", plotOutput(outputId = "hist", height = "500px")),
          tabPanel("Extracted Topics", verbatimTextOutput('topics')),
          tabPanel("Retweetable Topics", plotOutput('retw_topics'))
        )
      )
    )
  )
  
)


