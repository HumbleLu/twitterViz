library(shiny)

## Twitter API
library(twitteR)
api_key <- "b5c0z8ROGJSXhF53XkAkf2JVm"
api_secret <- "OKEOYlcVGdckeaCALVURP9BCikR0lMPd7sYdAzOgkwPiLKKSRJ"
access_token <- "1871917196-hzrcO6xBVKdmk7LeG0wsHq8GD0a72UkautxvCMz"
access_token_secret <- "LY0YIbRC1KznfLcp1ZWxDF940OXYqchsfYaXHxFrncM2v"
setup_twitter_oauth(api_key,api_secret,access_token, access_token_secret)

## Word cloud
library(tm)
library(wordcloud)
wordcloud.twitter<- function(tweeets, n = 1000, min.freq = 10){
  
  text<- sapply(tweeets, function(x) x$getText())
  
  corpus<- Corpus(VectorSource(text))
  corpus <- tm_map(corpus,
                   content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')),
                   ##content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),
                   mc.cores=1
  )
  corpus <- tm_map(corpus, content_transformer(tolower), mc.cores=1)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, stripWhitespace)
  
  wordcloud(corpus, min.freq = min.freq)
}

## Topic model
library(topicmodels)

LDA.twtter<- function(tweets, k){
  text<- sapply(tweets, function(x) x$getText())
  
  corpus<- Corpus(VectorSource(text))
  corpus <- tm_map(corpus,
                   content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')),
                   #content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')), for mac user (I am)
                   mc.cores=1
  )
  corpus <- tm_map(corpus, content_transformer(tolower), mc.cores=1)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, stripWhitespace)
  
  dtm<- DocumentTermMatrix(corpus)
  
  LDA(dtm, k = k, method = 'Gibbs')
}

library(ggplot2)

## Server
shinyServer(
  function(input, output) {
    v <- reactiveValues(tweets = NULL, topics = NULL)
    
    observeEvent(input$search, {
      withProgress(message = 'Get tweets from Twitter', {
        v$tweets <- searchTwitter(input$key_word, n = input$numOfTweetes)
      })
      withProgress(message = 'LDA modeling', {
        v$topics <- LDA.twtter(v$tweets, input$numOfTopics)
      })
    })
    
    observeEvent(input$numOfTopics, {
      withProgress(message = 'LDA modeling', {
        v$topics <- LDA.twtter(v$tweets, input$numOfTopics)
      })
    })
    
    output$word_map <- renderPlot({
      if (is.null(v$tweets)) return(NULL)
      withProgress(message = "wordcloud", {
        wordcloud.twitter(v$tweets)
      })
    })
    
    output$hist <- renderPlot({
      if (is.null(v$tweets)) return()
      hist(sapply(v$tweets, function(x) x$getRetweetCount()), 
           main = NULL,
           xlab = "Retweet Count",
           col = "grey")
    })
    
    output$topics<- renderPrint({
      if (is.null(v$tweets)) return(NULL)
      terms(v$topics, input$numOfTopTerms)
    })
    
    output$retw_topics<- renderPlot({
      if (is.null(v$tweets)) return(NULL)
      reTweetNum<- crossprod(v$topics@gamma, sapply(v$tweets, function(x) x$getRetweetCount()))
      reTweetNum<- data.frame(num = reTweetNum, topic = sapply(1:input$numOfTopics, function(x) paste("topic", x)))
      g<- ggplot(reTweetNum, aes(topic, num)) + geom_bar(stat = "identity") + xlab("") + ylab("")
      g
    })
  }
)


