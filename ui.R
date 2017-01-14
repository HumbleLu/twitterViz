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
          ## tabPanel("Histogram of Retweet Count", plotOutput(outputId = "hist", height = "500px")), 
          ## shut down this bloody boring thing at 20170114
          tabPanel("Extracted Topics", verbatimTextOutput('topics')),
          tabPanel("Retweetable Topics", plotOutput('retw_topics')),
          tabPanel("Analyze your tweet!", 
                   textAreaInput(inputId = "tweet_input", 
                             label = "Enter your tweet",
                             width = "500"
                   ),
                   # actionButton("analyze",
                   #              label = "Analyze Tweet!",
                   #              icon = icon("cogs", lib = "font-awesome")),
                   plotOutput('radar_chart'))
        )
      )
    )
  )
  
)


