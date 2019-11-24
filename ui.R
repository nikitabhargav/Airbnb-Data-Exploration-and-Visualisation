# ui.R

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

listing <-read.csv("newlistings.csv") # load listing data from newlistings.csv
reviews <- read.csv("reviews.csv") # load reviews data from reviews.csv

mystartdate <- min(as.Date(listing$host_since, format = "%d/%m/%Y")) #select minimum date from host_since column
myenddate <- max(as.Date(listing$host_since, format = "%d/%m/%Y")) #select maximum date from host_since column

rmystartdate <- min(as.Date(reviews$date, format = "%d/%m/%Y")) #select minimum date from date column
rmyenddate <- max(as.Date(reviews$date, format = "%d/%m/%Y")) #select minimum date from date column
# Reference 4

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel(""),
  
  # # Sidebar 
   sidebarLayout(
sidebarPanel(
  
  # creating 4 tabs in the layout
  tabsetPanel(
    tabPanel("Listing", value = 1), # creating listing tab
    tabPanel("Sentimental Analysis", value = 2), # creating sentimental Analysis tab
    tabPanel("Superhosts", value = 3), # creating Superhosts tab
    tabPanel("Reviews Trend", value = 4), # creating Reviews Trend tab
    id ="tab"
  ),
      #creating multiselect input for Room type
       conditionalPanel( condition= "input.tab == 1",
         selectizeInput("roomtype", "Room Type:",
                     c("Entire home/apt" = "Entire home/apt",
                       "Private room" = "Private room",
                       "Shared room" = "Shared room"
                       ), multiple = TRUE , selected = c("Entire home/apt",
                                                         "Private room", 
                                                         "Shared room")
         ),
         #creating multiselect input for Neighbourhood
         selectizeInput("neighbourhood", "Neighbourhood:",
                       c("Colac Otway" = "Colac Otway",
                         "Corangamite" = "Corangamite",
                         "Glenelg" = "Glenelg",
                         "Greater Geelong" = "Greater Geelong",
                         "Moyne" = "Moyne",
                         "Queenscliffe" = "Queenscliffe",
                         "Southern Grampians" = "Southern Grampians",
                         "Surf Coast" = "Surf Coast",
                         "Warrnambool" = "Warrnambool"
                       ), multiple = TRUE , selected = c("Colac Otway",
                                                         "Corangamite",
                                                         "Glenelg",
                                                         "Greater Geelong",
                                                         "Moyne",
                                                         "Queenscliffe",
                                                         "Southern Grampians",
                                                         "Surf Coast" ,
                                                         "Warrnambool")
         ),
         #creating slider input for price 
         sliderInput("price", "Price",
                     min(listing$price), 
                     max(listing$price),
                     value = range(listing$price)
         ),
         #creating slider input for Review Score Ratings
         sliderInput("ReviewScoreRatings", "Review Score Ratings",
                     min(listing$review_scores_rating),
                     max(listing$review_scores_rating), 
                     value = range(listing$review_scores_rating)
         ),
         #creating date range filter for listing tab
         dateRangeInput('listingdate',
                        label = paste('Select Dates'),
                        start = mystartdate,
                        end =   myenddate,
                        min = mystartdate,
                        max = myenddate,
                        format = "dd/mm/yy"
         )
       ),
      #creating text input for entering word for wordcloud 
       conditionalPanel(condition = "input.tab==2",
                        textInput("label", "Enter word to analyse guests review:", ""),selected ="safe"
                        
       ),
        #creating multiselect input for room type for superhosts tab
       conditionalPanel(condition = "input.tab==3",
                        selectizeInput("sroomtype", "Room Type:",
                                       c("Entire home/apt" = "Entire home/apt",
                                         "Private room" = "Private room",
                                         "Shared room" = "Shared room"
                                       ), multiple = TRUE , selected = c("Entire home/apt",
                                                                         "Private room", 
                                                                         "Shared room")
                        )
       ),
      #creating date range filter for reviews trend tab
       conditionalPanel(condition = "input.tab==4",
                        dateRangeInput('reviewsdate',
                                       label = paste('Select Dates'),
                                       start = rmystartdate,
                                       end =   rmyenddate,
                                       min = rmystartdate,
                                       max = rmyenddate,
                                       format = "dd/mm/yy"
                        )
       )
),
    
    # for main visualisations graphs
  mainPanel( 
       conditionalPanel(condition = "input.tab==1", # to write text for listing tab
                        div(strong(h2("Barwon South West, Victoria Airbnb Listing", align = "center")), style = "color:blue"),
                        p("It can be observe that in past 4-5 years airbnb has become popular in these 
                          locations."),
                        p("The first bar graphs shows that host prefer to rent Entire Apartment as compared to
                          private room or shared room.
                          The second bargraph shows guest have affordable prices for properties.
                          The third bargraph shows guests usually give high ratings to the rooms 
                          they stay"),
                        em(p("(Takes some time to load)")),
                            leafletOutput("leafletmaplisting"), # displaying leaflet
                            plotOutput("bargraph") # displaying bar graph
                            ),
       
       conditionalPanel(condition = "input.tab==2", # to write text for wordcloud tab
                        div(strong(h2("AirBnb Word Cloud(Vector Space) Generator", align = "center")), style = "color:blue"),
                        h4("Check this sample Word Cloud automatically generated for term SAFE and you can 
                           observe that the nearest related words are calm, quiet,secure etc"),
                        strong(p("
                                 Word Cloud show the words related to the word you have
                                 entered and all the nearest words with the reviews already given
                                 by the guests")),
                        plotOutput("wcplot") # displaying wordcloud
                   ),
       
       conditionalPanel(condition = "input.tab==3", # to write text for Superhosts tab
                        div(strong(h2("Superhost vs Normal Host", align = "center")), style = "color:blue"),
                        p("It can be observe from the chart that for most of the properties 
                            instant Bookings are not allowed."),
                        
                        p("For the entire home properties where instant booking allowed
                          have strict cancellation policy because last moment cancellation 
                          may result in a huge loss of money to the host."),
                        
                        p("Multiple links to  Private rooms and shared rooms shows that these
                          hosts are regular renters. They have flexible, moderate cancellation 
                          policy. They also allows instant booking for the properties."),
                        
                        p("Finally, Superhost are the host that have more previlages than 
                            normal hosts."), 
                        plotOutput("Sanplot") # displaying alluvial plot
       ),
       
       conditionalPanel(condition = "input.tab==4", # to write text for Reviews trend tab
                        div(strong(h2("Trend of Reviews", align = "center")), style = "color:blue"),

                        p("The trend graph shows that number of reviews given by guests in january
                          of every year are comparetively higher than other period of year."),
                        p("This signifies that in summer season of Australia guests books more
                          property at this period of year."),
                        p("Zooming out the chart shows that guests give higher reviews on weekends
                          that means guests prefer weekends bookings over weekdays."),
                        dygraphOutput("Reviewstrend") # displaying time series 
       )
       
       )

     )
  
))

#Reference4. https://stackoverflow.com/questions/48633984/pass-a-dynamic-date-date-range-to-daterangeinput-in-r-shiny