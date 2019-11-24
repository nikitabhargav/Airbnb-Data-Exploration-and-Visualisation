# server.R

library(shiny) # to create interactive shiny app
library(ggplot2) # to create bar graphs
library(leaflet) # to create geographical map
library(dplyr) # for data manipulation
library(RColorBrewer) # to colour palette
library(text2vec)  # to create word vector space matrix
library(wordcloud) # to create wordcloud
library(alluvial)  # to create multivariate plot
library(readr)  # to read csv file
library(dygraphs)  # to create time series plot
library(xts)  # to create time series objects
library(gridExtra) # to merge multiple plots


listing <-read.csv("newlistings.csv") # to load listing data from newlistings.csv file

reviews <- read.csv("reviews.csv")  # to load review data from reviews.csv file


# to create a word vector matrix
tokens <- space_tokenizer(as.character(reviews$comments)) # create tokens
gv = GlobalVectors$new(word_vectors_size = 60, 
                       vocabulary = create_vocabulary(itoken(tokens, progressbar = FALSE)), x_max = 10)

# `modifying the gv object using fit_transform
wordvector = gv$fit_transform(create_tcm(itoken(tokens, progressbar = FALSE), 
                                         vocab_vectorizer(create_vocabulary(itoken(tokens, progressbar = FALSE))), 
                                         skip_grams_window = 5L), n_iter = 10)




# Define server logic required to draw plots
shinyServer(function(input, output) {
  
# creating leaflet map 
  output$leafletmaplisting <- renderLeaflet({

    listingroomtype <- listing[listing$room_type %in% input$roomtype, ] # filtering room type
    listingdt <- listingroomtype[as.Date(listingroomtype$host_since, format = "%d/%m/%Y") >= as.Date(input$listingdate[1]) & 
                                   as.Date(listingroomtype$host_since, format = "%d/%m/%Y") <= as.Date(input$listingdate[2]), ] #filtering date 
    listingneighbour <- listingdt[listingdt$neighbourhood_cleansed %in% input$neighbourhood, ]  #filtering neighbourhood
    listingprice <- listingneighbour[listingneighbour$price >= input$price[1] & listingneighbour$price <= input$price[2], ] #filtering price
    listingscore <- listingprice[listingprice$review_scores_rating >= input$ReviewScoreRatings[1] & listingprice$review_scores_rating <= input$ReviewScoreRatings[2], ] #filtering score review rating
    
    pal <- colorFactor(c("red", "green", "blue"),listing$room_type) # for colouring circles
    
    leaflet(listingscore) %>%  #geographical map
      addTiles() %>%
      addCircleMarkers(
        ~longitude,
        ~latitude , 
        group = "circles", # listing will be displayed in circle of different colours
        popup = ~paste("<b/>","Room Type:", listingscore$room_type, # to label each listing
                       "<br/>","<b/>","city:", listingscore$city,
                       "<br/>","<b/>","price:", listingscore$price,
                       "<br/>","<b/>","Review Ratings:", listingscore$review_scores_rating,
                       "<br/>","<b/>","neighbourhood:",listingscore$neighbourhood_cleansed),
        color = ~pal(input$roomtype), # colouring circles
        stroke = FALSE, 
        fillOpacity = 0.5) 
  })

# Creating bar graphs foraverage  price, number of listing and average review score rating.
  
  output$bargraph <- renderPlot({ 
    listingroomtype <- listing[listing$room_type %in% input$roomtype, ] # filtering room type
    listingdt <- listingroomtype[as.Date(listingroomtype$host_since, format = "%d/%m/%Y") >= as.Date(input$listingdate[1]) & 
                                   as.Date(listingroomtype$host_since, format = "%d/%m/%Y") <= as.Date(input$listingdate[2]), ] #filtering date
    listingneighbour <- listingdt[listingdt$neighbourhood_cleansed %in% input$neighbourhood, ]  #filtering neighbourhood
    listingprice <- listingneighbour[listingneighbour$price >= input$price[1] & listingneighbour$price <= input$price[2], ] #filtering price
    listingscore <- listingprice[listingprice$review_scores_rating >= input$ReviewScoreRatings[1] & listingprice$review_scores_rating <= input$ReviewScoreRatings[2], ] #filtering score review rating
    
    #creating dataframe with count and room type
    totalrcount<-  listingneighbour %>% group_by(room_type) %>% summarize(sum = n()) 

    #ploting bargraph for number of listings
    countplot <- ggplot(totalrcount, aes(x= room_type, y=sum, fill =room_type))+
        geom_bar(stat="identity" , width=0.5 , show.legend = FALSE) +
        labs(title = "Number of Listing vs Room Type", x = "room type", y = "count")
    
    #creating dataframe with average price and room type  
    totalrprice <-  listingprice %>% group_by(room_type) %>% summarize(averagePrice = mean(price))
  
    #ploting bargraph for average price
    priceplot <-  ggplot(totalrprice, aes(x= room_type, y=averagePrice, fill =room_type))+
        geom_bar(stat="identity" , width=0.5 , show.legend = FALSE)+
        labs(title = "Average Price vs Room Type", x = "room type", y = "Avg Price")

    #creating dataframe with average score and room type
    totalrscore<-  listingscore %>% group_by(room_type) %>% summarize(averageScore = mean(review_scores_rating))
    
    #ploting bargraph for average review score ratings
    scoreplot <- ggplot(totalrscore, aes(x= room_type, y=averageScore, fill =room_type))+
              geom_bar(stat="identity" , width=0.5)+
              labs(title = "Average Review Rating vs Price", x = "room type", y = "Avg Review Scores Rating")
    
    # to combine all 3 bargraphs into a single row
    grid.arrange(countplot,priceplot,scoreplot, ncol=3)      
# Reference1
  })
  
# ploting word cloud
  
  output$wcplot  <- renderPlot({
    
    # handling filter when nothing is passed to inout
    if (input$label ==""){
        textvalue = "safe"}
    else {
      textvalue = input$label 
    }
      
    wv = wordvector[textvalue, , drop = FALSE] # creating a word vector
    cossim = sim2(x = wordvector, y = wv, method = "cosine", norm = "l2")
    sortcossim = sort(cossim[,1], decreasing = TRUE) # ordering the wordvector matrix
    feedback = data.frame(comments = as.character(names(sortcossim)),freq = as.numeric(sortcossim))#creating a word vector consist of words and its count
    feedback = feedback[!duplicated(gsub(",","",feedback$comments)), ] # removing duplicate words 
    
    par(mar = rep(0, 4)) # for setting the margin for word cloud
    
    # creating a word cloud
    wordcloud(words = feedback$comments, freq = feedback$freq, scale = c(4,0.2), 
              max.words=70, random.order=FALSE, rot.per=0.2,
              colors = brewer.pal(8,"Dark2"))
    #Reference 4
  })
 
# Creating alluvial plot.
  
  output$Sanplot <- renderPlot({

    # handling null values from input
    if (input$sroomtype ==""){
      textvalue = "Entire home/apt"}
    else {
      textvalue = input$sroomtype 
    }
    
    listing <- read_csv('newlistings.csv') # loading listings data from newlistings.csv file
    listingrmtype <- listing[listing$room_type %in% textvalue, ] #Filtering  roomtype
    
    #selecting only 4 column from listingrmtype dataframe
    listing1 <- listingrmtype[,c("instant_bookable","cancellation_policy","room_type","host_is_superhost")]
    
    #grouping the values with this 4 colums
    listing2 <-  listing1 %>% group_by(instant_bookable,cancellation_policy,room_type,host_is_superhost) %>% summarise(count = n())
    
    listing2[listing2=='f']<- "NotAllowed" # replacing f with Notallowed in listing2
    listing2[listing2=='t']<- "Allowed" # replacing t with allowed in listing2
    # replacing strict_14_with_grace_period,super_strict_30,super_strict_60 with strict in listing2
    listing2[listing2 == 'strict_14_with_grace_period' | listing2=='super_strict_30' | listing2=='super_strict_60']<- "strict"
    
    # creating a matrix
    layout(matrix(1:2, 2, 1), heights=c( 0.10, 0.90 ) )
    
    par(mar = rep(0, 4)) # setting the margin
    
    plot(0:1, type="n", axes=FALSE)
    legend("center", pch=15, # creating legend for host and superhost
           bty="n",
           ncol=2,
           pt.cex=2,
           col=c("#f2a2be","#36c9c6"),
           legend=c( "Host","Super Host"),
           title="Type of Host")
    
    par(mar = rep(0, 4)) # setting the margin
    
    # creating alluvial plot
    alluvial( listing2[,c("instant_bookable","cancellation_policy","room_type")],
              freq=listing2$count, 
              alpha=1,
              cw=0.20,
              gap.width=0.05,
              col=ifelse( listing2$host_is_superhost == "Allowed", "#36c9c6", "#f2a2be"),
              border="grey",
              axis_labels=c("Instant Booking","Cancellation Policy","Room Type"))
    
  # Reference2
  })
  
  # Ploting Time series trend graph
  
  output$Reviewstrend <- renderDygraph({
    
    datacount <- reviews %>% group_by(date)  %>% summarise(Freq = n()) # calculating count of reviews on different dates
    
    datacount <- datacount[as.Date(datacount$date, format = "%d/%m/%Y") >= as.Date(input$reviewsdate[1]) & 
                      as.Date(datacount$date, format = "%d/%m/%Y") <= as.Date(input$reviewsdate[2]), ] #filtering dates
    
    # creating time series object dataframe with count and date
    xts(datacount$Freq, as.Date(datacount$date, format = "%d/%m/%Y")) %>%
    
      # ploting time series graph
      dygraph() %>%
      dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = TRUE, colors="blue") %>% # colouring the trend line
      dyRangeSelector() %>%
      dyCrosshair(direction = "vertical") %>% # creating vertical selector
      dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>% # creating circle marker
      dyRoller(rollPeriod = 1)
# Reference3
  })
})

# Reference1.http://www.moneyandscience.com/2016/05/05/shiny-r-code-for-multiple-plots-using/  
# Reference2. https://cran.r-project.org/web/packages/alluvial/vignettes/alluvial.html  
# Reference3. https://stackoverflow.com/questions/30176303/using-dygraph-with-shiny
# Reference4. https://github.com/Ankit-Peshin/airbnb/blob/master/code/Airbnb_final_knit.rmd