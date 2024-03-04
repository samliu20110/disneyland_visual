
# Set up

library(shiny)
library(leaflet)
library(ggplot2)
library(plotly)
library(lubridate)
library(scales)
library(zoo)
library(tidyverse)
library(rgdal)
library(RColorBrewer)
library(shinydashboard)


# File reading

q1_all <- read_csv("q1_all.csv")

q1_year <- read_csv("q1_year.csv")

q1_month <- read_csv("q1_month.csv")

q2_best <- read_csv("q2_best.csv")

q2_cali <- read_csv("q2_cali.csv")

q2_hk <- read_csv("q2_hk.csv")

q2_paris <- read_csv("q2_paris.csv")


world_spdf <- readOGR( 
  dsn= paste0(getwd(),"/world_shape_file/") , 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)


# Dataframe manipulation

q1_all$Year_Month <- as.yearmon(q1_all$Year_Month, "%Y-%m")    # Change "Year_Month" to type yearmon

org <- world_spdf@data    # Save a copy of data from the world shape file

cali <- world_spdf

cali@data <- left_join(org, q2_cali, by = c("NAME" = "NAME"))    # Map data for California branch

hk <- world_spdf

hk@data <- left_join(org, q2_hk, by = c("NAME" = "NAME"))    # Map data for Hong Kong branch

paris <- world_spdf

paris@data <- left_join(org, q2_paris, by = c("NAME" = "NAME"))    # Map data for Paris branch








# shiny


ui <- dashboardPage(
  
  # Set dashboard theme
  skin = "black",    
  
  
  # Set dashboard title
  dashboardHeader(title = "Disneyland rating analysis"),
  
  
  # Set dashboard sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("compass")),
      menuItem("Average rating across time", tabName = "linegraph", icon = icon("chart-line")),
      menuItem("Average rating across countries", tabName = "maps", icon = icon("globe")),
      menuItem("Conclusion", tabName = "conclusion", icon = icon("th"))
    )
  ),
  
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              
              h3("Basic information of the three Disneyland branches"),
              
              box(htmlOutput("overview")),
              
              hr(),
              
              verticalLayout(box(htmlOutput("cali_info")),
              box(htmlOutput("hk_info")),
              box(htmlOutput("paris_info"))
              )
              
              
            
              ),
      
      
      tabItem(tabName = "linegraph",
              
              h2("Average ratings of the branches in line graph"),
              
              hr(),
              
              verticalLayout(
                
                box(title = h3("Tips for using this page"), htmlOutput("text")),
                
                # Checkbox for line graph
                box(
                  title = h3("Display checkbox"),
                  radioButtons("lineCheckbox", label = "Please select the display of x-axis", 
                               choices = list("By month" = "q1_month_plot", "By year" = "q1_year_plot", "By month and year" = "q1_all_plot"),
                               selected = "q1_month_plot"))
                
                
              ),
              
              
              verticalLayout(
                
                # Line graph output
                box(plotlyOutput("linegraph1")),
                
                box(title = h3("Findings of the graphes"), htmlOutput("text2")))
              ),
      
      
      
      tabItem(tabName = "maps",
              
              fluidRow(
                h2("Average ratings of the branches against visitors' country of origin")),
              
              
              hr(),
              
              verticalLayout(
                
                box(title = h3("Tips for using this page"), htmlOutput("text3")),
                
                
                # Checkbox for maps
                box(
                  title = h3("Map display checkbox"),
                  radioButtons("mapCheckbox", label = "Please select the map you wish to explore", 
                               choices = list("Favourite Branch" = "fav_map", "Disneyland California" = "cali_map", "Disneyland Hong Kong" = "hk_map", "Disneyland Paris" = "paris_map"),
                               selected = "fav_map")
                ),
                
                # Map output
                box(leafletOutput("mymap")),
                
                box(title = h3("Findings of the maps"), htmlOutput("text4"))
                )
              ),
      
      
      
      tabItem(tabName = "conclusion",
              
              h2("Conclusion"),
              
              htmlOutput("conclusion"),
              
              h2("Reference"),
              
              hr(),
              
              p("https://publicaffairs.disneyland.com/disneyland-resort-generates-5-7-billion-for-southern-california-economy-2/#:~:text=An%20independent%20study%20concluded%20that,in%20state%20and%20local%20taxes."),
              
              p("https://en.wikipedia.org/wiki/Disneyland"),
              
              p("https://www.ocregister.com/2021/10/06/disneyland-attendance-bounces-back-to-nearly-85-of-2019-levels-report-says/"),
            
              p("https://en.wikipedia.org/wiki/Hong_Kong_Disneyland"),
              
              p("https://hkcorporate.hongkongdisneyland.com/pdf/AnnualBusinessReview19.pdf"),
              
              p("https://www.statista.com/statistics/236183/attendance-at-the-hong-kong-disneyland-theme-park/"),
              
              p("https://en.wikipedia.org/wiki/Disneyland_Paris"),
              
              p("https://www.forbes.com/sites/csylt/2019/04/04/disney-books-500-million-hotel-revenue-in-paris/?sh=10d497bf7913")
              
              )
      
      
    ) 
  ) 
  
  
  
) 
  
  
  
  


server <- function(input, output, session) {
  
  # Text output
  
  output$overview <- renderUI({
    HTML(paste("Ever since the establishment of the first ever Disneyland amusement park, it has been over have a century. With more and more branches of Disneyland welcoming visitors all across the globe every day, it is unavoidable to compare and contrast the quality of the overall customer experience of each branch. In this project, we have obtained over 42,000 reviews of three Disneyland branches - California, Hong Kong and Paris - on TripAdvisor regarding the reviewer's experience in the branches rating from 1 to 5. To start with, the following provides some basic information of the three branches. Once you are finished, feel free to explore the tabs on the left for detailed findings on the data. ",
               sep = "<br/>"))
    
  })
  
  output$cali_info <- renderUI({
    HTML(paste("Disneyland California <br/>",
               "Location: Anaheim, California, U.S.",
               "Year of completion: July 17, 1956",
               "Area: 40 ha",
               "Annual revenue: USD$5.7 billion",
               "Attendance: 18.7 million (2019)", 
               sep = "<br/>"))
    
  })
  
  output$hk_info <- renderUI({
    HTML(paste("Disneyland Hong Kong <br/>",
               "Location: Penny's Bay, Lantau Island, Hong Kong",
               "Year of completion: September 12, 2005",
               "Area: 49.9 ha",
               "Annual revenue: HK$6,047 million (2019)",
               "Attendance: 5.7 million (2019)", 
               sep = "<br/>"))
    
  })
  
  output$paris_info <- renderUI({
    HTML(paste("Disneyland Paris <br/>",
               "Location: Chessy, France",
               "Year of completion: April 12, 1992",
               "Area: 1700 acres",
               "Annual revenue: USD$497.1 million (2019)",
               "Attendance: 14.99 million (2019)",
               sep = "<br/>"))
    
  })
  
  
  output$text <- renderUI({
    HTML(paste("Please check out the tool bar on the top side of the line graph box for better exploration experience. ",
               "Move your mouse over the line to check the average rating value of each point. ", 
               "Click on the legend of the branches (on the right of the line graph) to show/hide selected line(s). ", sep = "<br/>"))
  })
  
  
  output$text2 <- renderUI({
    HTML(paste(" From the 'By month' graph, it can be observed that the trends of the California branch and the Hong Kong Branch coincide for most of the time, especially from April to October. The Paris branch also shares similar result with the other two branches from April to September. However, the lowest average rating of the Paris branch appears in May rather than August for the California branch andthe Hong Kong branch. Such highly similar trends from the three branches may be caused by the weather conditions in the northern hemisphere or school holidays. However, due to limitation, this cause requires further investigation on the review text. <br/>",
               "As it can be seen in the 'By year' graph, the California branch has experienced consistent decrease in rating since 2010.Unlike the California branch, both Hong Kong and Paris branches share an overall increasing trend until 2017 and drop drastically after 2017. <br/>",
               "From the 'By month and year' graph, the average rating from 2010 to 2011 shows high variance for all three branches, which is highly likely due to limited reviews in this period. From 2011, the average ratings of all three branches lie within the range from 3 to 5. ", sep = "<br/>"))
  })
  
  output$text3 <- renderUI({
    HTML(paste("Select the map you wish to explore in the checkbox", 
               "Click on the + and - to zoom in and out of the map",
               "Move your mouse over the country of your interest for more information", sep = "<br/>"))
  })
  
  output$text4 <- renderUI({
    HTML(paste("As it can be seen in the 'Favourite Branch' map, there are several Asian country’s visitors prefer Disneyland California despite the fact that Disneyland Hong Kong may be the closest option. Disneyland California is also undoubtly favoured across North and South America. <br/>", 
               "In the map of 'Disneyland California', it can be observed that most of the countries have the average ratings being above 4.0, which also explains why it is often the most favourite branch out of the three. <br/> ", 
               "Despite the fact that Disneyland Hong Kong is located in Asia, the average ratings of this branch from the surrounding coountries may tell a different story. That is, it is hard to spot any of them giving an average rating over 4.5. <br/>",
               "From the 'Disneyland Paris' map, it is interesting to notice that Disneyland Paris does not have high rating by the surrouding European counties, which may strengthen the observation that Disneyland Paris has the worst reputation among all three branches. <br/>",
               sep = "<br/>"))
  })
  
  output$conclusion <- renderUI({
    HTML(paste("Finally, it can be concluded that Disneyland California is the most favoured among all three branches to the foreign visitors, but the overall rating is showing a decreasing trend throughout the years. However, since both Disneyland Hong Kong and Disneyland Paris are also giving signs of decrease in the yearly average ratings, Disneyland California might keep its position of being the most favoured branch. Looking at maps of favourite branch, there are precisely more countries marked in green which represents Disneyland California. The map implies that Disneyland California is more favoured among most countries in the dataset, with Disneyland Hong Kong being the second in place. However, some countries only contain less than ten reviews, which could largely affect the reliability of the result due to small sample size. <br/>",
               "Therefore, for future development, it is essential to further investigate the reason for having low ratings of Disneyland Paris for longterm profit. Furthermore, the comparison could be done with competitor's amusement park for improvement ideas.",
               sep = "<br/>"))
    
  })
  
  
  # Line graph button input
  output$value1 <- renderText({ input$lineCheckbox })
  
  # Line graph based on month
  q1_month_plot <- ggplot(q1_month, aes(x = Month, y = avg_rating, colour = Branch)) + 
    geom_line(size = 1) +
    scale_x_continuous(breaks = 1:12) +
    scale_colour_brewer(palette = 'Set2') +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    labs(x = "Month of year", y = "Average rating from foreign visitors")
  
  
  # Line graph based on year
  q1_year_plot <- ggplot(q1_year, aes(x = Year, y = avg_rating, colour = Branch)) + 
    geom_line(size = 1) +
    scale_x_continuous(breaks = 2010:2019) +
    scale_colour_brewer(palette = 'Set2') +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    labs(x = "Year", y = "Average rating from foreign visitors")
  
  
  # Line graph based on month and year
  q1_all_plot <- ggplot(q1_all, aes(x = Year_Month, y = avg_rating, colour = Branch)) + 
    geom_line(size = 1) +
    scale_colour_brewer(palette = 'Set2') +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    labs(x = "Month and year", y = "Average rating from foreign visitors")
  
  # Line graph output based on checkbox input
  output$linegraph1 <- renderPlotly({
    ggplotly(
      get(input$lineCheckbox)
    )
  })
  
  
  
  
  # Maps
  
  output$value2 <- renderText({ input$mapCheckbox })
  
  
  ## Favourite branch map
  
  
  # Compute dataframe from shapefile for selected countries
  world_spdf@data <- left_join(org, q2_best, by = c("NAME" = "NAME"))
  
  # Select colours for each branch and NAs
  favpalette <- colorFactor(palette = c("#66C2A4", "#FD8D3C", "#9E9AC8"), domain = world_spdf@data$Branch, na.color = "transparent")
  
  # Compute text for tooltip
  favtext <- paste(
    "Country: ", world_spdf@data$NAME, "<br/>",
    "Favourite Branch: ", world_spdf@data$Branch, "<br/>",
    "Average rating of favourite branch: ", world_spdf@data$avg_rating, 
    sep = ""
  ) %>%
    lapply(htmltools::HTML)
  
  # Compute map for favourite branch
  fav_map <- leaflet(data = world_spdf) %>% 
    addTiles()  %>% 
    clearShapes() %>%
    setView( lat=10, lng=0 , zoom=2) %>%
    addPolygons( 
      fillColor = ~favpalette(Branch), 
      stroke=TRUE,
      fillOpacity = 0.9,
      color = "white",
      weight = 0.3,
      highlight = highlightOptions(    # Add highlight when mouse hover over
        weight = 3,
        fillOpacity = 0.9,
        color = "yellow",
        opacity = 1.0,
        bringToFront = TRUE,
        sendToBack = TRUE
      ),
      label = favtext,
      labelOptions = labelOptions( 
        style = list("font-weight" = "normal", padding = "3px 8px"), 
        textsize = "13px", 
        direction = "auto"
      )
    ) %>%
    addLegend( pal=favpalette, values=~Branch, opacity=0.9, title = "Branches", position = "bottomleft" )

  
  
  ## California branch map
  
  
  # Compute bins for continuous data (average rating)
  mybins <- c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)
  
  # Compute continuous colour palette for average rating
  calipalette <- colorBin(palette = "BuGn", domain = cali@data$avg_rating, na.color = "grey", bins = mybins)
  
  # Compute text for California branch map tooltip
  calitext <- paste(
    "Country: ", cali@data$NAME, "<br/>",
    "Average rating of California branch: ", cali@data$avg_rating, 
    sep = ""
  ) %>%
    lapply(htmltools::HTML)
  
  # Compute map for California branch
  cali_map <- leaflet(data = cali) %>% 
    addTiles()  %>% 
    clearShapes() %>%
    setView( lat=10, lng=0 , zoom=2) %>%
    addPolygons( 
      fillColor = ~calipalette(avg_rating), 
      stroke=TRUE,
      fillOpacity = 0.9,
      color = "white",
      weight = 0.3,
      highlight = highlightOptions(    # Add highlight when mouse hover over
        weight = 3,
        fillOpacity = 0.9,
        color = "yellow",
        opacity = 1.0,
        bringToFront = TRUE,
        sendToBack = TRUE
      ),
      label = calitext,
      labelOptions = labelOptions( 
        style = list("font-weight" = "normal", padding = "3px 8px"), 
        textsize = "13px", 
        direction = "auto"
      )
    ) %>%
    addLegend( pal=calipalette, values=~avg_rating, opacity=0.9, title = "Average rating of Disneyland California", position = "bottomleft" )
  
  
  
  ## HK branch map
  
  
  # Compute colour palette for Hong Kong branch average rating
  hkpalette <- colorBin(palette = "Oranges", domain = hk@data$avg_rating, na.color = "grey", bins = mybins)
  
  
  # Compute text for Hong Kong branch map tooltip
  hktext <- paste(
    "Country: ", hk@data$NAME, "<br/>",
    "Average rating of Hong Kong branch: ", hk@data$avg_rating, 
    sep = ""
  ) %>%
    lapply(htmltools::HTML)
  
  
  hk_map <- leaflet(data = hk) %>% 
    addTiles()  %>% 
    clearShapes() %>%
    setView( lat=10, lng=0 , zoom=2) %>%
    addPolygons( 
      fillColor = ~hkpalette(avg_rating), 
      stroke=TRUE,
      fillOpacity = 0.9,
      color = "white",
      weight = 0.3,
      highlight = highlightOptions(
        weight = 3,
        fillOpacity = 0.9,
        color = "yellow",
        opacity = 1.0,
        bringToFront = TRUE,
        sendToBack = TRUE
      ),
      label = hktext,
      labelOptions = labelOptions( 
        style = list("font-weight" = "normal", padding = "3px 8px"), 
        textsize = "13px", 
        direction = "auto"
      )
    ) %>%
    addLegend( pal=hkpalette, values=~avg_rating, opacity=0.9, title = "Average rating of Disneyland Hong Kong", position = "bottomleft" )
  
  
  
  
  ## Paris map
  
  
  # Compute colour palette for Paris branch map
  parispalette <- colorBin(palette = "Purples", domain = paris@data$avg_rating, na.color = "grey", bins = mybins)
  
  # Compute text for Paris map tooltip
  paristext <- paste(
    "Country: ", paris@data$NAME, "<br/>",
    "Average rating of Paris branch: ", paris@data$avg_rating, 
    sep = ""
  ) %>%
    lapply(htmltools::HTML)
  
  paris_map <- leaflet(data = paris) %>% 
    addTiles()  %>% 
    clearShapes() %>%
    setView( lat=10, lng=0 , zoom=2) %>%
    addPolygons( 
      fillColor = ~parispalette(avg_rating), 
      stroke=TRUE,
      fillOpacity = 0.9,
      color = "white",
      weight = 0.3,
      highlight = highlightOptions(
        weight = 3,
        fillOpacity = 0.9,
        color = "yellow",
        opacity = 1.0,
        bringToFront = TRUE,
        sendToBack = TRUE
      ),
      label = paristext,
      labelOptions = labelOptions( 
        style = list("font-weight" = "normal", padding = "3px 8px"), 
        textsize = "13px", 
        direction = "auto"
      )
    ) %>%
    addLegend( pal=parispalette, values=~avg_rating, opacity=0.9, title = "Average rating of Disneyland Paris", position = "bottomleft" )
  
  # Output leaflet
  
  output$mymap <- renderLeaflet(get(input$mapCheckbox))
}

shinyApp(ui, server)
