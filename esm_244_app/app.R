library(shiny)
library(tidyverse)
library(shinythemes)

# Define UI for application
ui <- fluidPage(
  theme = shinytheme("sandstone"),
  
  # Application title
  titlePanel(tags$img(src = "https://copr.nrs.ucsb.edu/sites/default/files/copr-logo.jpg", "Snowy Plover and Bird Abundance at Coal Oil Point Reserve"),
             "Snowy Plover and Bird Abundance at Coal Point Oil Reserve"),
  
  navbarPage( "",
              
              # First tab
              tabPanel("Summary",
                       h1("About this App "),
                       p("This data exploration tool is intended for use by educators, COPR visitors and the public."),
                       h1("Coal Oil Point Reserve "),
                       p("Coal Oil Point Reserve (COPR) is a small area that includes dune vegetation and rare wildlife, like the dune spider, the globose dune beetle and the threatened Western Snowy Plover surrounded by increasing urbanization.  The main challenge  at the reserve is to maintain the habitats and species that live here, despite habitat fragmentation, pollution, disturbances, and climate change.  Many of our wildlife and habitat conservation programs started because we noticed a species' population declining over time.  We first try to understand the factors causing the decline and then implement actions to reduce or eliminate these threats.  For many species, the reserve's boundaries are not sufficient for their safety and survival.  Cooperation with neighboring properties in a much larger spacial scale is essential to the protection of some species.Check out all of the different research and conservation efforts happening at COPR using the hyperlink below."),
                       tags$a(href="https://copr.nrs.ucsb.edu/about/programs/snowy-plover-conservation", "COPR website"),
                       h2("Western Snowy Plover"),
                       p("The Western Snowy Plover, Charadrius nivosus nivosus, is a shorebird that inhabits beaches and lake shores.  The Pacific Coast population of the Western Snowy Plover was listed as \"threatened\" under the Endangered Species Act in 1993 because of declining populations mainly due to loss of habitat.  The stretch of beach between Isla Vista and Ellwood (including Sands Beach) was designated \"critical habitat\" in December of 1999; at the time of the critical habitat designation, the population in the entire Pacific Coast of the United States was estimated at less than 1500 individuals.  Coal Oil Point Reserve, with its sandy beach, sand dunes, and adjacent estuary mouth is one of a few choice west coast locations where the snowy plovers can still breed and thrive.  With public education and symbolic fences, the plovers at COPR made a comeback. "),
                       tags$img( style="display: block; margin-left: auto; margin-right: auto;", src = "https://copr.nrs.ucsb.edu/sites/default/files/styles/top_image/public/images/17711743361_bfdb440d8a_o.jpg?itok=-c8bPJ9X", alt = "Image of Western Snowy Plover chicks"),
                       h2("Bird Abundance"),
                       p("COPR is one of the Audubon's Important bird Areas. Birders count organinisms at the reserve daily and have been doing this for many years, but these data are seldom associated with a specific area.  Therefore, they cannot be used to study trends over time.  FOr accurate analysis abundance and diversity of birds need to be measured in the same way, and within the same area, every time.  
                         
                         Beginning in February 2015, Coal Oil Point Reserve implemented a long-term monitoring program to measure bird abundance and diversity within 10 permanent sampling areas on the reserve known as polygons.  These surveys provide useful data for researchers and students and allow us to understand how birds use these diverse habitats on the reserve. The polygons are outlined in white on the interactive map below. The polygons are outlined in white on the interactive map below. More detailed observations are mapped on COPR's website at the hyperlink provided."), 
                       
                       tags$a(href="https://fusiontables.google.com/data?docid=1RihfY8XXjWT6EpkxoId2m4JzyPLD_j7KU6rsHNR6#map:id=3", "Individual Bird Observations", align = "center"),
                       HTML("<br><br><br>"),
                       leafletOutput("polygon_map")
                       
                       ),
              
              
              # Second tab
              tabPanel("Western Snowy Plover",
                       titlePanel("Nest Failure by Year"),
                       
                       # Sidebar with select box input for number of year 
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("year", 
                                       "Select year:",
                                       choices = c( "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"),
                                       selected = "2018")
                         ),
                         
                         # Show a plot of the generated distribution
                         mainPanel(
                           plotOutput("distPlot")
                         )
                       ),
                       
                       titlePanel("Breeding Success"),
                       
                       # Sidebar with input for breeding stage
                       sidebarLayout(
                         sidebarPanel(
                           checkboxGroupInput("stage",
                                              "Select breeding stage:",
                                              choices = c("Nests", "Eggs laid", "Eggs hatched", "Fledged chicks"),
                                              selected = "Nests")
                         ),
                         
                         
                         
                         # Show a plot of the generated distribution
                         mainPanel(
                           plotOutput("linesPlot")
                         )
                       ),
                       fluidRow(
                         HTML("<div class=\"panel panel-primary\"><div class=\"panel-heading\">Note:</div>
                           <div class=\"panel-body\">
                              Eggs data is the recorded number of “confirmed eggs”. In some cases, it is not possible to confirm the number of eggs due to the level of disturbance this would cause to nearby nests or chicks. This explains why in year 2007 there are no eggs laid, but there are eggs hatched and fledged chicks.
                              </div>
                              </div>")
                       )
                       
                       
                       
              ),
              
              # Third tab
              tabPanel("All Birds",
                       titlePanel("Species Diversity by Year"), #titles widget
                       
                       # Sidebar with a slider input for number of bins 
                       sidebarLayout(
                         sidebarPanel(
                           
                           radioButtons("div_year", 
                                        "Select Survey Year:",
                                        choices = c("2015","2016","2017"))
                         ),
                         
                         # Show a plot of the generated distribution
                         mainPanel(
                           plotOutput("diversity")
                         )
                       ),
                       
                       titlePanel("Top Bird Species by Abundance"), #titles widget
                       
                       # Sidebar with a radio button input for year and select input for number of top species
                       sidebarLayout(
                         sidebarPanel(
                           
                           radioButtons("top_year", 
                                        "Select Survey Year:",
                                        choices = c("2015","2016","2017")
                           ),
                           selectInput("top_number", 
                                       "Select number of top species:",
                                       choices = c(1,2,3,4,5,6,7,8,9),
                                       selected = 6)
                         ),
                         
                         
                         # Show a plot of the generated distribution
                         mainPanel(
                           plotOutput("topPlot", height = "600px"),
                           dataTableOutput("topTable")
                         )
                       )
                       
                       
              ),
              hr(),
              tags$footer("Developed by Angie Bouche <abouche@bren.ucsb.edu> and Anna Calle <annagcalle@bren.ucsb.edu> in programming language R version 3.5.1 (2018-07-02). Code on",tags$a(href ="https://github.com/annagaby/esm-244-final-project","GitHub." )),
              hr()
              )
)





# Define server logic
server <- function(input, output) {
  
  
  output$distPlot <- renderPlot({
    # Render a column graph for predators data
    predators_graph <- snowyplover %>% 
      select(year, failure_cause) %>% 
      filter( failure_cause != "NA") %>%
      filter( year == input$year) %>% 
      count(failure_cause, year) %>%
      arrange(-n) %>% 
      mutate( failure_cause = factor(failure_cause, levels = failure_cause)) %>% 
      mutate(type_failure = case_when(
        failure_cause == "Skunk" ~ "Animal Predator",
        failure_cause == "Crow" ~ "Animal Predator",
        failure_cause == "Gull" ~ "Animal Predator",
        failure_cause == "Raccoon" ~ "Animal Predator",
        failure_cause == "Owl" ~ "Animal Predator",
        failure_cause == "Dog" ~ "Animal Predator",
        failure_cause == "Whimbrel" ~ "Animal Predator",
        failure_cause == "Tide" ~ "Environmental Factor",
        failure_cause == "Flooded" ~ "Environmental Factor",
        failure_cause == "Wind" ~ "Environmental Factor",
        failure_cause == "Car" ~ "Other",
        failure_cause == "Human" ~ "Other",
        failure_cause == "Unknown" ~ "Other",
        failure_cause == "Abandoned" ~ "Other"
      ) )
    
    ggplot(predators_graph, aes( x = failure_cause, y = n)) +
      geom_col(aes(fill = type_failure)) +
      xlab("Cause of Failure") +
      ylab("Number of Affected Nests") +
      scale_y_continuous(expand = c(0,0),limits = c(0,20), breaks = c(0,5,10,15,20)) +
      theme_classic() +
      scale_fill_discrete(breaks = c("Animal Predator", "Environmental Factor", "Other"),
                          name = "Type of failure")
  })
  
  
  
  
  # Render line graph for breeding data
  output$linesPlot <- renderPlot({
    if(is.null(input$stage)){
      return()
    }
    else{
      ggplot(breeding_table[ breeding_table$stage %in% input$stage,],
             aes(x = year, y = count, group = stage)) +
        geom_line(aes(color = stage)) +
        theme_classic() +
        scale_y_continuous(expand = c(0,0), limits = c(0,200)) +
        scale_x_continuous(expand = c(0,0), breaks = c(2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)) +
        xlab("Year") +
        ylab("Number") +
        labs( color = "Stage")
      
    }
  })
  
  
  output$diversity <- renderPlot({
    
    bird_sp <- shorebirds %>% 
      filter(Year == input$div_year) %>% #Filter data set by year (use radio buttons or drop down menu to select)
      group_by(Species) %>% 
      count(Polygon_Location, Species) #Turn list of observed species into counts of how many different species observe
    
    #Graph formatting
    ggplot(bird_sp, aes(x=Polygon_Location))+
      geom_bar(fill = "navajowhite3")+
      labs(x= "Location", y = "Number of Species Observed")+ 
      ggtitle(paste("Bird Diversity at COPR", input$div_year))+
      theme_classic()+
      theme(plot.title = element_text(hjust = 0.5)) 
  })
  
  
  
  
  # Render column graphs for selected top bird species
  
  #Converted dates into standard formate
  shorebirds$Date <- as.Date(shorebirds$Date, "%m/%d/%Y")
  
  # Top species by year
  top_by_year <- reactive({
    shorebirds %>%
      mutate( Month = month(Date)) %>%
      select(Month, Year, Species, Count) %>% 
      filter( Year == input$top_year) %>% 
      group_by(Species) %>% 
      summarize( count = sum(Count)) %>% 
      arrange(-count) %>% 
      head(as.numeric(input$top_number))
  })
  
  
  # Monthly data filtered for top species by year
  top_filtered <-  reactive({
    shorebirds %>%
      mutate( Month = month(Date)) %>%
      select(Month, Year, Species, Count) %>% 
      filter( Year == input$top_year) %>% 
      group_by(Month, Species)%>%
      summarise( count = sum(Count))%>% 
      arrange(Species, -count)
  })
  
  # Render column graph
  output$topPlot <- renderPlot({
    ggplot( top_filtered()[top_filtered()$Species %in% top_by_year()$Species,], aes( x = Month, y = count)) + geom_col(fill = "skyblue3") + 
      facet_wrap(~Species) +
      ggtitle(paste("Top Bird Species at COPR in Monthly Observations", input$top_year))+  
      theme_classic()+
      theme(plot.title = element_text(hjust = 0.5))+ 
      scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
      scale_y_continuous(expand = c(0,0))
  })
  
  
  
  output$topTable <- renderDataTable({
    
    bird_table <- bird_names[bird_names$alpha_code %in% top_by_year()$Species,] 
    colnames(bird_table) <- c("Alpha", "Common", "Scientific")
    bird_table
  }, options= list(paging = FALSE, searching = FALSE))
  
  
  # Render map
  output$polygon_map <- renderLeaflet({
    labels <- sprintf(
      "<strong>%s</strong>",
      COPR_polygons$NAME
    ) %>% lapply(htmltools::HTML)
    
    leaflet(COPR_polygons) %>% 
      addTiles( group = "Open Street Map") %>%
      addProviderTiles("Esri.WorldImagery", group = "Esri World Imagery") %>%
      addPolygons(weight = 1.0,
                  opacity = 1.0,
                  color = "white",
                  fillOpacity = 0.3,
                  fillColor = topo.colors(12),
                  highlight = highlightOptions(
                    weight = 5,
                    color = "white",
                    fillOpacity = 0.7),
                  group = "Polygons",
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>% 
      addMarkers(lat = 34.411371, lng = -119.876618,
                 label = "Nature Center Coal Oil Point Reserve",
                 group = "Markers") %>% 
      addMarkers(lat = 34.407912, lng = -119.878954,
                 label = "Sands Beach Entrance",
                 group = "Markers") %>% 
      addLayersControl(
        baseGroups = c("Esri World Imagery", "Open Street Map"),
        overlayGroups = c("Polygons", "Markers"),
        options = layersControlOptions(collapsed = FALSE))%>% 
      addEasyButton(easyButton(
        icon="fa-globe", title="Reset zoom",
        onClick=JS("function(btn, map){ map.setZoom(15); }")))%>% 
      addMiniMap(
        tiles = providers$Esri.WorldImagery,
        toggleDisplay = TRUE,
        position = "bottomleft"
      )
    
  })
}


# Run the application 
shinyApp(ui = ui, server = server)


