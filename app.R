#
# This is a Shiny web application for the Nexus Lab's Sentigram.
library(shiny)
library(shinythemes)
library(shinycssloaders)

source(file = "./appSource.R")

# Define UI for application that draws a histogram
ui <- navbarPage( "InTo", theme = shinytheme("united"),
                  
                  # Sidebar with a slider input for number of bins 
                  tabPanel(title = "Healthcare Pressure",
                           
                           div(tags$head(includeCSS("styles.css"))),
                           
                           absolutePanel(id = "controls", class = "panel panel-default",
                                         top = 250, left = 0, right = 0, bottom = 0, 
                                         width = 300, height = 850, fixed=TRUE, draggable = TRUE,
                                         
                                         selectInput(inputId = "city", label = "Choose a Location", 
                                                     choices = c( "New Delhi" # ,
                                                                  # "Bangkok", "Jakarta", "Mumbai"
                                                     ), 
                                                     selected = "New Delhi"),
                                         br() ,
                                         dateRangeInput(inputId = "dates", label = "Select a Date Range",
                                                        min = "2020-01-01", max = "2020-12-31",
                                                        start = "2020-04-01", end = "2020-04-30"),
                                         br(),
                                         actionButton("display", "Select"),
                                         br(),
                                         plotOutput("sentiMeter", height="183px", width="275px"),
                                         br(),
                                         plotOutput("hospTime", height="183px", width="275px"),
                                         br(),
                                         plotOutput("caseTime", height="183px", width="275px")
                           ),
                           
                           h6("To get started, choose your Location and Date Range in the panel on the right and
                                 then click Select."),
                           
                           leafletOutput("krigingMap", height = 900)
                  ),
                  
                  tabPanel(title = "Top words",
                           
                           fluidRow(
                             
                             column(width = 6, 
                                    plotOutput("top_bigrams") %>% withSpinner())
                           ),
                           fluidRow(
                             h3("Tweet Emotions over Time"),
                             column(width = 5, 
                                    plotOutput("top_emotions") %>% withSpinner()),
                             column(width = 5, offset = 1,
                                    plotOutput("emoTime") %>% withSpinner()))
                           ),
                  
                  tabPanel(title = "Predictability"),
                  
                  tabPanel(title = "Potential Misinformation",
                           
                           column(width = 7,
                                  h4("Help us identify misinformation. Select a misinforming text below."),
                                  DT::dataTableOutput("topTweets")%>% withSpinner()),
                           column(width = 4,
                                  h4("Selected Misinformation"),
                                  DT::dataTableOutput("misinform_selected"),
                                  actionButton("email", "Send")
                                  # a(actionButton(inputId = "email1", label = "Send", 
                                  #                icon = icon("envelope", lib = "font-awesome")),
                                  #   href="mailto:elroy.galbraith@gmail.com?
                                  #   &subject='Misinforming Tweets'
                                  #   &body='Attached is a list of misinforming tweets'
                                  #   &attachment='./data/misinformation.csv'")
                                  )
                           ),
                  
                  tabPanel(title = "Healthcare Satisfaction"),
                  
                  tabPanel(title = "About",
                           
                           h3("What are you InTo?"),
                           p("The Covid 19 pandemic provides a unique opportunity to investigate the relationship between
                           social chatter and health-seeking behaviour. We here at the Nexus Lab believe that the sentiments 
                           in the words we use can help public health officials forecast the expected demand on hospital resources.
                           Imagine, analyzing the billions of tweets on Twitter and not only getting an understanding of what people
                           know or how they feel about a disease, but using that information to inform how well local hospitals 
                           need to prepare, in the days, weeks or months ahead. So we created this tool: a infodemiological tomograph 
                             called InTo."),
                           br(),
                           
                           p("We conduct sentiment analysis on tweets about covid 19 in specific cities. 
                             Then we compared how the words used and sentiments expressed changed over time simultaneously with the
                             incidence of the disease and the rate of hospitalization in that area. The statistical relationship between
                             these variables are then used to create a forecasting model to predict future health care demand. 
                             This interactive dashboard displays the results of this analysis."),
                           br(),
                           
                           h3("Who are we?"),
                           p("We are the NEXUS Lab, a team of scientists lead by Dr. Matteo Convertino. 
                             At the Nexus Lab we are broadly interested in quantifying connections in complex socio-ecological 
                             and biological systems (the Nexus!) for understanding how ecosystem works, and designing them 
                             considering the desired objectives (”ecosystem services”).  
                             This can also lead to learning bio-inspired rules and models to adopt for realizing bioinspired technology, 
                             management solutions and design. "),
                           br(),
                           
                           h3("Disclaimers"),
                           p("We do not plan to Tweet or Retweet any content. The aforementioned information from our analysis 
                           (predictability indicators over time and space, inferred sentiments, pressure on healthcare system, 
                           events and Tweets popularity, and vulnerability classes) will be shared on a public dashboard without 
                           revealing private information of users and information banned by Twitter for public disclosure (e.g. Tweet text). 
                           Thus, the only information that will be shared is about our post-processing data indicators 
                           (cross correlation functions and transfer entropy) that have health policy value for WHO and potentially for 
                           the healthcare system at smaller administrative scales as well as for research community. The top ten words in 
                           terms of frequency shared by Tweets in the sample populations will be shared. These words will not be attached 
                             to a specific user because in fact they are representative of the sampled population.")
                           
                  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # TweetMap
  observeEvent(input$display, {
    
    #Generate tweet data
    ifelse(input$city == "Bangkok",
           tweetCoord <- filter(tweetCoord_Bang, 
                                as.Date(day_created) >= input$dates[1], 
                                as.Date(day_created) <= input$dates[2]),
           ifelse(input$city == "New Delhi",
                  tweetCoord <- filter(tweetCoord_Del, 
                                       as.Date(day_created) >= input$dates[1], 
                                       as.Date(day_created) <= input$dates[2]),
                  ifelse(input$city == "Jakarta",
                         tweetCoord <- filter(tweetCoord_Jak, 
                                              as.Date(day_created) >= input$dates[1], 
                                              as.Date(day_created) <= input$dates[2]),
                         tweetCoord <- filter(tweetCoord_Mum, 
                                              as.Date(day_created) >= input$dates[1], 
                                              as.Date(day_created) <= input$dates[2]))
                  )
               )
    # 
    # #  
    # Tweet Timeline
    output$sentiMeter <- renderPlot({
      
      p <- tweetCoord %>%
        group_by(day_created) %>%
        summarise(Sent = mean(Sent, na.rm = T),
                  nTweets = mean(nTweets, na.rm = T)) %>%
        ungroup() %>%
        ggplot(aes(x = as.Date(day_created), y = Sent, color = nTweets, group = 1)) +
        geom_line(size = 1, color = "black") +
        geom_point(size = 5) +
        scale_color_gradient2(mid = "yellow",midpoint = 5) +
        guides(color = F) +
        labs(x = "", y = "Positivity", color = "Mean Daily Tweets")+
        tweetPlotTheme
      
      
      p
      
    })
    # Generate tweet data
    ifelse(input$city == "Bangkok",
           tweetBigrams <- tweetBigrams_Bang,
           ifelse(input$city == "New Delhi",
                  tweetBigrams <- tweetBigrams_Del,
                  ifelse(input$city == "Jakarta",
                         tweetBigrams <- tweetBigrams_Jak,
                         tweetBigrams <- tweetBigrams_Mum)
                  )
    )
    # 
    # Top Bigrams
    output$top_bigrams <- renderPlot({

      tweetBigramsPlot <- tweetBigrams %>%
        ggplot(aes(x = reorder(pairs, n), y = n)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(x = "", y = "Frequency", title = "Most Frequent Pairs") +
        tweetPlotTheme

      tweetBigramsPlot

    })
    
    # Top tweets and tweeters
    misinform <- topTweets %>%
      filter(as.Date(day_created) >= input$dates[1], 
             as.Date(day_created) <= input$dates[2]) %>%
      select("Date" = day_created,"User" = screen_name, 
             "Tweet" = text, "Retweet Count" = retweet_count)
    
    output$topTweets <- DT::renderDataTable({
      
      datatable(misinform, 
                options = list(dom = "ftp",
                               ordering = F,
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$('th').css({'background-color': '#000', 'color': '#fff'});",
                                 "}"))) 
      
    }, server = T)
    
    output$misinform_selected <- DT::renderDataTable({

      datatable(select(topTweets[c(input$topTweets_rows_selected),], "Tweet" = text),
                options = list(dom = "tp", ordering = F))
      
    })
    
    observeEvent(input$email,{

      select(topTweets[c(input$topTweets_rows_selected),], "Tweet" = text) %>%
      write.csv("./data/misinformation.csv")

      send.mail(
        from = "elroy.galbraith@gmail.com",
        to = c("nexuslabhokkaido@gmail.com"),
        subject = "Selected misinforming tweets",
        body = "Hi Nexus Lab, \n Attached are some misinforming tweets to track. \n Regards.",
        smtp = list(host.name = "aspmx.l.google.com", port = 25),
        authenticate = F,
        attach.files = "./data/misinformation.csv",
        send = T
      )

    })
    
    # Top Emotions
    ifelse(input$city == "Bangkok",
           tweetEmotions <- tweetEmo_Bang,
           ifelse(input$city == "New Delhi",
                  tweetEmotions <- tweetEmo_Del,
                  ifelse(input$city == "Jakarta",
                         tweetEmotions <- tweetEmo_Jak,
                         tweetEmotions <- tweetEmo_Mum)
           )
    )
    
    output$top_emotions <- renderPlot({
      
      ggwordcloud(words = tweetEmotions$sentiment, freq = tweetEmotions$n) +
        labs(title = "Emotion of Most Retweeted Tweets") +
        tweetPlotTheme
      
    })
    # # 
    # Generate cases data
    ifelse(input$city == "Bangkok",
           covidCases <- get_cases_data(input$city)%>%
             filter(Date >= input$dates[1], 
                    Date <= input$dates[2]) ,
           ifelse(input$city == "New Delhi",
                  covidCases <- covidCases_Del%>%
                    filter(Date >= input$dates[1], 
                           Date <= input$dates[2]) ,
                  ifelse(input$city == "Jakarta",
                         covidCases <- get_cases_data(input$city)%>%
                           filter(Date >= input$dates[1], 
                                  Date <= input$dates[2]) ,
                         covidCases <- get_cases_data(input$city)%>%
                           filter(Date >= input$dates[1], 
                                  Date <= input$dates[2]) ))
    )
    # 
    # Sentiment by Cases
    output$caseTime <- renderPlot({

      cases_plot <- covidCases %>%
        ggplot(aes(x = Date, y = value, group  = 1)) +
        geom_point() +
        geom_line() +
        labs(x = "", y = "New Cases") +
        tweetPlotTheme

      cases_plot

    })
    # # 
    # Generate hospitalization data
    ifelse(input$city == "Bangkok",
           covidHosp <- get_hosp_data(input$city)%>%
             filter(Date >= input$dates[1], 
                    Date <= input$dates[2]) ,
           ifelse(input$city == "New Delhi",
                  covidHosp <- covidHosp_Del%>%
                    filter(Date >= input$dates[1], 
                           Date <= input$dates[2]) ,
                  ifelse(input$city == "Jakarta",
                         covidHosp <- get_hosp_data(input$city)%>%
                           filter(Date >= input$dates[1], 
                                  Date <= input$dates[2]) ,
                         covidHosp <- get_hosp_data(input$city)%>%
                           filter(Date >= input$dates[1], 
                                  Date <= input$dates[2]) ))
    )
    # 
    # Sentiment by Hospitalization
    output$hospTime <- renderPlot({

      hosp_plot <- covidHosp  %>%
        ggplot(aes(x = Date, y = n, group = 1)) +
        geom_point() +
        geom_line() +
        labs(x = "", y = "Hospitalizations") +
        tweetPlotTheme

      hosp_plot

    })
    # 
    # Generate kriging data
    kriginData <- kriging_Del
    #
    # Krigin output
    output$krigingMap <- renderLeaflet({
      
      mPred = mean(kriginData$var1.pred, na.rm = T) 

      kde <- bkde2D(kriginData[ , c("lng", "lat")],
                    bandwidth=c(.0045, .0068), gridsize = c(100,100))

      CL <- contourLines(kde$x1 , kde$x2 , kde$fhat)

      # EXTRACT CONTOUR LINE LEVELS
      LEVS <- as.factor(sapply(CL, `[[`, "level"))
      NLEV <- length(levels(LEVS))

      # CONVERT CONTOUR LINES TO POLYGONS
      pgons <- lapply(1:length(CL), function(i)
        Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
      spgons = SpatialPolygons(pgons)
      
      pal <- colorFactor(palette = "YlOrRd", levels = LEVS, reverse = F)

      leaflet(spgons) %>% 
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
        addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OSM") %>%
        addPolygons( group = "Hospitalization",
          color = pal(LEVS),
                    popup = htmlEscape(paste0(
                      "Approximately ", 
                      ifelse(as.numeric(as.character(LEVS)) - mPred > 0,
                             round(as.numeric(as.character(LEVS)) - mPred), 0),
                             " people here may require hospitalization next week.")
                                       )
                    ) %>%
        addPolygons( group = "Cases",
                     color = pal(LEVS),
                     popup = htmlEscape(paste0(
                       "Approximately ", 
                       ifelse(as.numeric(as.character(LEVS)) - mPred > 0,
                              round(as.numeric(as.character(LEVS)) - mPred), 0),
                       " people here may require hospitalization next week.")
                     )
        ) %>%
        addLegend(pal = pal, values = LEVS, position = "bottomright", title = "Healthcare Pressure Indicator") %>%
        addLayersControl(position = "bottomright",
          baseGroups = c("Satellite", "OSM"),
          overlayGroups = c("Hospitalization", "Cases"),
          options = layersControlOptions(collapsed = FALSE)
        )

    })
    
    # # present correlation and time series values
    # output$vals <- renderTable(select(assocVals_Del, -X))
    # 
    # tweet emotions over time
    output$emoTime <- renderPlot({

      emoPlot <- tweetEmotions_del %>%
        filter(!sentiment %in% c("positive", "negative")) %>%
        ggplot(aes(x = as.Date(day_created), y = n, color = sentiment, group = sentiment)) +
        geom_line(size = 1) +
        geom_point(size = 3) +
        scale_color_manual(values = emoColors,
                           labels = c("Anger",
                                      "Anticipation",
                                      "Disgust",
                                      "Fear",
                                      "Joy",
                                      "Sadness",
                                      "Surprise",
                                      "Trust")) +
        labs(x = "Date", y = "Number of words", color = "Emotion") +
        tweetPlotTheme

      emoPlot

    })
    
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
