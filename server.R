library(shiny)

## Twitter API
library(twitteR)
api_key <- "b5c0z8ROGJSXhF53XkAkf2JVm"
api_secret <- "OKEOYlcVGdckeaCALVURP9BCikR0lMPd7sYdAzOgkwPiLKKSRJ"
access_token <- "1871917196-hzrcO6xBVKdmk7LeG0wsHq8GD0a72UkautxvCMz"
access_token_secret <- "LY0YIbRC1KznfLcp1ZWxDF940OXYqchsfYaXHxFrncM2v"
setup_twitter_oauth(api_key,api_secret,access_token, access_token_secret)

top_rtw<- function(tweets){
  retweetCount<- sapply(tweets, function(x) x$getRetweetCount())
  tweets[[order(retweetCount, decreasing = TRUE)[1]]]
}

## Word cloud
library(tm)
library(wordcloud)
wordcloud.twitter<- function(tweeets, n = 1000, min.freq = 10){
  
  text<- sapply(tweeets, function(x) x$getText())
  
  corpus<- Corpus(VectorSource(text))
  corpus <- tm_map(corpus,
                   ## content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')),
                   content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),
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

LDA.twitter<- function(tweets, k, alpha = .1, delta = .001){
  text<- sapply(tweets, function(x) x$getText())
  
  corpus<- Corpus(VectorSource(text))
  corpus <- tm_map(corpus,
                   ## content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')),
                   content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')), # for mac user (I am)
                   mc.cores=1
  )
  corpus <- tm_map(corpus, content_transformer(tolower), mc.cores=1)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, stripWhitespace)
  
  dtm<- DocumentTermMatrix(corpus)
  
  LDA(dtm, k = k, method = 'Gibbs', control = list(alpha = alpha, delta = delta, iter = 5000))
}

predict_topics<- function(tweets, lda){
  corpus<- Corpus(VectorSource(tweets))
  corpus <- tm_map(corpus,
                   ## content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')),
                   content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')), # for mac user (I am)
                   mc.cores=1
  )
  corpus <- tm_map(corpus, content_transformer(tolower), mc.cores=1)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, stripWhitespace)
  
  dtm<- DocumentTermMatrix(corpus)
  
  posterior(lda, dtm)$topics
}

library(ggplot2)

## Radar chart
library(fmsb)

radar_plot<- function(val, names = NULL){
  
  if(is.null(names)){
    names<- sapply(1:length(val), function(i) paste("Topic", i))
  }else{
    if(length(val) != length(names)) stop("length of val and name should be the same!")
  }
  
  # Create data
  n_class<- length(val)
  data<- as.data.frame(matrix(val, nrow=1))
  colnames(data)<- names
  # add 2 lines to the dataframe: the max and min of each topic to show on the plot!
  data=rbind(rep(1,n_class) , rep(0,n_class) , data)
  
  # The default radar chart proposed by the library
  radarchart(data)
  
  # Ploish the radarChart !
  radarchart(data  , axistype=1 , 
             
             #custom polygon
             pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
             
             #custom the grid
             cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,1,5), cglwd=0.8,
             
             #custom labels
             vlcex=0.8 
  )
}


## Server
shinyServer(
  function(input, output, session) {
    v <- reactiveValues(tweets = NULL, topics = NULL)
    
    observeEvent(input$search, {
      withProgress(message = 'Get tweets from Twitter', {
        v$tweets <- searchTwitter(input$key_word, n = input$numOfTweetes)
      })
      
      updateTextInput(session, "tweet_input", value = top_rtw(v$tweets)$text)
      
      withProgress(message = 'LDA modeling', {
        v$topics <- LDA.twitter(v$tweets, input$numOfTopics)
      })
    })
    
    observeEvent(input$numOfTopics, {
      withProgress(message = 'LDA modeling', {
        v$topics <- LDA.twitter(v$tweets, input$numOfTopics)
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
    
    output$radar_chart <- renderPlot({
      if (is.null(v$tweets)) return(NULL)
      radar_plot(predict_topics(input$tweet_input, v$topics))
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


