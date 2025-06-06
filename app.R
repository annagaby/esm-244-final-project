#load relevant packages
library(shiny)
library(tidyverse)
library(shinythemes)
library(leaflet)
library(sf)
library(lubridate)

# Define UI for application
ui <- fluidPage(
  theme = shinytheme("sandstone"),
  
  # Application title
  titlePanel(
    tagList(
      tags$img(src = "COPR-logo.png", height = "80px"),
      "Snowy Plover and Bird Abundance at Coal Oil Point Reserve"
    )),
  
  navbarPage( "",
              
              # First tab
              tabPanel("Summary",
                       h1("Coal Oil Point Reserve "),
                       p("Coal Oil Point Reserve (COPR) was established in 1970 to protect part of the natural habitat along the South Coast of Santa Barbara County that a wide variety of bird species inhabit. It is one of the 35 reserves that form part of the", a( href="https://ucnrs.org", "University of California Natural Reserve System.", target="_blank"), "COPR has several  different research and conservation efforts including habitat restoration, bird monitoring, water quality monitoring, and snowy plover conservation which are described in more detail on their", a(href="https://copr.nrs.ucsb.edu/about/programs/snowy-plover-conservation", "website.", target="_blank"), align = "justify"),
                         p("This data exploration tool is intended for educators, birdwatchers, beach users, and anyone interested in the conservation efforts taking place at COPR. It provides interactive visualizations of data collected in two of COPR’s research programs: Snowy Plover Conservation and Bird Monitoring.", align = "justify"),
                       h2("Snowy Plover Conservation"),
                       p("The Snowy Plover Conservation program is the reserve’s best known research effort. It focuses on protecting the habitat of the Western Snowy Plover", tags$i("(Charadrius nivosus nivosus)"), "a threatened shorebird species. In 2001, Snowy Plover nests were discovered on the reserve for the first time in decades, inspiring COPR to develop a targeted conservation program. As part of the program, staff collect Snowy Plover data at least three times per week during the breeding season. This data includes number of adults, number of nests, metrics to track reproduction, and fate of nests. Once a nest is identified, COPR’s observers will count the number or eggs, eggs hatched, and chicks fledged throughout the breeding season. Some nests are destroyed during the breeding season. When this is the case, COPR’s observers will identify the type of animal predator or environmental condition responsible.", align = "justify"),
p("Under the “Snowy Plover Conservation” tab in this app, users can view the frequency of different forms of nest destruction for a given year and trends in nest, egg, and chick counts since 2001.", align = "justify"),

# Add Snowy Plover image
                       tags$img( style="display: block; margin-left: auto; margin-right: auto;", width ="100%", src = "https://lh3.googleusercontent.com/rAmbw7zPfHeh9-lOkmnL2Sl5ij1jShcfZejayPakYyiQvwnDuazcy6uzrxSry-Q6PoI9LTxTj1KFUJkpKa15DGXmV2IKV4au1ThwBwIi_f-J45BUmplIkTbMGYxO83dboZ1MclRx=w1920-h1080", alt = "Image of Western Snowy Plover chicks"),
                       h2("Bird Monitoring"),
                       p("COPR maintains a long-term monitoring program focused on measuring bird species abundance and diversity throughout the reserve. Since February 2015, bird surveys have been conducted once a month within ten permanent sampling areas on the reserve. These areas are shown in the map below as polygons. The surveys are conducted by two observers following COPR’s", a( href= "https://copr.nrs.ucsb.edu/sites/default/files/images/Protocol_Bird%20Abundance%20at%20Coal%20Oil%20Point%20Reserve_20170529.pdf", target="_blank", "bird monitoring protocol."), "Once a month, two of COPR’s observers will traverse the reserve on foot and spend a specific amount of time in each polygon confirming the present of all birds in that polygon visually or auditorily. For each bird observed its species, location, and behavior is recorded. Individual observations with their locations are available in a", a(href="https://fusiontables.google.com/data?docid=1RihfY8XXjWT6EpkxoId2m4JzyPLD_j7KU6rsHNR6#map:id=3", target="_blank", "Google Fusion Table and Map."), align = "justify"), 
                       p("Under the “Bird Monitoring” tab, users can view the number of species observed in the different polygons for each year that observations have been recorded. They can also choose to view up to nine graphs showing the most abundant species at the reserve at a selected year and observe seasonal trends in abundance.", align = "justify"),

              # Add reserve's map         
                       leafletOutput("polygon_map")
                       
                       ),
              
              
              # Second tab
              tabPanel("Snowy Plover Conservation",
                       titlePanel("Nest Failure by Year"),
                       
                       # Sidebar with select box input for number of year 
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("year", 
                                       "Select year:",
                                       choices = c( "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"),
                                       selected = "2018"),
                           tags$hr(style="border-color: gray;"),
                        p("Select a year to observe the number of nests destroyed or abandoned throughout the Snowy Plover breeding season due to various factors. ")   
                         ),
                         
                         # Show a plot of the generated nest failure column graph
                         mainPanel(
                           plotOutput("failurePlot")
                         )
                       ),
                       
                       titlePanel("Breeding Success"),
                       
                       # Sidebar with checkbox input for breeding stage
                       sidebarLayout(
                         sidebarPanel(
                           checkboxGroupInput("stage",
                                              "Select breeding stage:",
                                              choices = c("Nests", "Eggs Laid", "Eggs Hatched", "Fledged Chicks"),
                                              selected = "Nests"),
                           tags$hr(style="border-color: gray;"),
                           p ("Use the check boxes to view trends in indicators of breeding from when COPR was first became a restored nesting site in 2001 until 2018.")
                         ),
                         
                         
                         
                         # Show generated breeding lines plot 
                         mainPanel(
                           plotOutput("linesPlot")
                         )
                       ),
                       HTML("<br>"),
                       fluidRow(
                         HTML("<div class=\"panel panel-primary\"><div class=\"panel-heading\">Note:</div>
                           <div class=\"panel-body\">
                              <b>Nests:</b> Data shows total number of nests identified in the breeding season, including those successfully used for raising chicks and those that were unused. A high number of nests does not necessarily mean a more productive breeding season, as it can also mean that high levels of disturbance caused the Snowy Plovers to abandon and rebuild their nests.<br><b>Eggs laid:</b> Data shows the number of “confirmed eggs”. In some cases, it is not possible to confirm the number of eggs due to the level of disturbance this would cause to nearby nests or chicks. This explains why in year 2007 there are no confirmed eggs laid, while there are eggs hatched and fledged chicks.
                              </div>
                              </div>")
                       )
                       
                       
                       
              ),
              
              # Third tab
              tabPanel("Bird Monitoring",
                       titlePanel("Species Diversity by Year"),
                       
                       # Sidebar with a select box input for survey year
                       sidebarLayout(
                         sidebarPanel(
                           
                           radioButtons("div_year", 
                                        "Select Survey Year:",
                                        choices = c("2015","2016","2018")),
                           tags$hr(style="border-color: gray;"),
                           p("Select a year to see the number of different species recorded in a given area of the reserve.")
                         ),
                         
                         # Show generated diversity bar graph
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
                                        choices = c("2015","2016","2018")
                           ),
                           selectInput("top_number", 
                                       "Select number of top species:",
                                       choices = c(1,2,3,4,5,6,7,8,9),
                                       selected = 6),
                           tags$hr(style="border-color: gray;"),
                           p("Use the buttons to select a year, and the drop down menu to select how many graphs you would like to view. The graphs show up to nine of the most abundant bird species at the reserve during a given yearordered alphabetically.")
                         ),
                         
                         
                         # Show generated column top species graph and table
                         mainPanel(
                           plotOutput("topPlot", height = "600px"),
                           dataTableOutput("topTable")
                         )
                       ),
                       HTML("<br>"),
                       fluidRow(
                         HTML("<div class=\"panel panel-primary\"><div class=\"panel-heading\">Note:</div>
                              <div class=\"panel-body\">
                              In 2018 COPR switched to digitalized bird observations. At this time 2017 data has not been converted to a digital format, but will be by the end of 2019.
                              </div>
                              </div>")
                         )
                       
                       
              ),
              hr(),
# Create footer
              tags$footer("Developed by Angie Bouche <abouche@bren.ucsb.edu> and Anna Calle <annagcalle@bren.ucsb.edu> in programming language R version 3.5.1 (2018-07-02). Code on",tags$a(href ="https://github.com/annagaby/esm-244-final-project", target="_blank", "GitHub."), " Data sources:", tags$ul(
                tags$li("Coal Oil Point Reserve, UCSB Natural Reserve System (2019), Western Snowy Plover Breeding at Coal Oil Point Reserve 2001-2018, unpublished raw data."),
tags$li("Coal Oil Point Reserve, UCSB Natural Reserve System (2019), Bird Abundance at Coal Oil Point Reserve 2015, 2016, 2018, unpublished raw data.")
)),
              hr()
              )
)





# Define server logic
server <- function(input, output) {
  
  # Read files
  snowyplover <- read_csv("SnowyPlover.csv")
  shorebirds <- read_csv("COPR_Shorebird.csv")
  bird_names <- read_csv("bird_names.csv")
  COPR_polygons <- st_read(dsn = ".", layer = "COPR_polygons-polygon") 
  
  breeding <- snowyplover %>%
    select(year, total_eggs, eggs_hatched, chicks_fledged) %>%
    mutate(nests = 1)
  
  # Identify character columns only
  char_cols <- sapply(breeding, is.character)
  
  # Replace ".", "?", "unconfirmed" in character columns
  breeding[char_cols] <- lapply(breeding[char_cols], function(x) {
    x[x %in% c(".", "?", "unconfirmed")] <- NA
    x
  })
  
  # Convert all columns to numeric
  breeding <- breeding %>%
    mutate(across(everything(), as.numeric))
  
  # Replace remaining NA with 0
  breeding[is.na(breeding)] <- 0
  
  
  # Creating count table by year and by breeding stage
  
  breeding_table <- breeding %>% 
    group_by(year) %>% 
    summarise( "Nests" = sum(nests), "Eggs Laid" = sum(total_eggs), "Eggs Hatched" = sum(eggs_hatched), "Fledged Chicks" = sum(chicks_fledged)) %>% 
    gather("stage", "count", 2:5)
  
  
  # Render a column graph for predators data
  output$failurePlot <- renderPlot({
   # Data wrangling for graph
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
    
    # Creating graph 
    ggplot(predators_graph, aes( x = failure_cause, y = n)) +
      geom_col(aes(fill = type_failure)) +
      xlab("Cause of Failure") +
      ylab("Number of Affected Nests") +
      scale_y_continuous(expand = c(0,0),limits = c(0,20), breaks = c(0,5,10,15,20)) +
      theme_classic() +
      ggtitle(paste("Causes of Nest Failure at COPR", input$year))+
      theme(plot.title = element_text(hjust = 0.5))+ 
      scale_fill_manual(labels = c("Animal Predator", "Environmental Factor", "Other"),
                          name = "Type of failure", values = c("navajowhite3", "skyblue3", "olivedrab2"))
  })
  
  
  # Render line graph for breeding data
  output$linesPlot <- renderPlot({
    if(is.null(input$stage)){
      return()
    }
    else{
      ggplot(breeding_table[ breeding_table$stage %in% input$stage,],
             aes(x = year, y = count, group = stage)) +
        geom_line(aes(color = stage), lwd =1.5) +
        theme_classic() +
        ggtitle(paste("Snowy Plover Breeding Trends at COPR"))+
        theme(plot.title = element_text(hjust = 0.5))+ 
        scale_y_continuous(expand = c(0,0), limits = c(0,200)) +
        scale_x_continuous(expand = c(0,0), breaks = c(2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)) +
        scale_color_manual(values = c("navajowhite3", "skyblue3", "indianred1","olivedrab2"))+
        xlab("Year") +
        ylab("Number") +
        labs( color = "Stage")
      
    }
  })
  
 # Render bar graph for diversity at each polygon
  output$diversity <- renderPlot({
    # Wrangling data
    bird_sp <- shorebirds %>% 
      filter(Year == input$div_year) %>% #Filter data set by year (use radio buttons or drop down menu to select)
      group_by(Species) %>% 
      count(Polygon_Location, Species) #Turn list of observed species into counts of how many different species observe
    
    # Creating graph
    ggplot(bird_sp, aes(x=Polygon_Location))+
      geom_bar(fill = "navajowhite3")+
      labs(x= "Location", y = "Number of Species Observed")+ 
      ggtitle(paste("Bird Diversity at COPR", input$div_year))+
      theme_classic()+
      theme(plot.title = element_text(hjust = 0.5))+
      scale_y_continuous(expand = c(0,0), limits = c(0,85), breaks= c(0,20,40,60,80))
      
   
  })
  
  
  
  
  # Render column graphs for selected top bird species
  
  #Converted dates into standard format
  shorebirds$Date <- as.Date(shorebirds$Date, "%m/%d/%Y")
  
  # List of top species using selected year input and selected number of species input
  top_by_year <- reactive({
    shorebirds %>%
      mutate( Month = month(Date)) %>%
      select(Month, Year, Species, Count) %>% 
      filter( Year == input$top_year) %>% 
      group_by(Species) %>% 
      summarize( count = sum(Count)) %>% 
      arrange(-count) %>% 
      head(as.numeric(input$top_number)) %>% 
      mutate( Species == as.factor(Species))
  })
  
  
  # Data wrangling to group observations by month for selected year
  top_filtered <-  reactive({
    shorebirds %>%
      mutate( Month = as.factor(month(Date))) %>%
      select(Month, Year, Species, Count)%>% 
      filter( Year == input$top_year) %>% 
      group_by(Month, Species)%>%
      summarise( count = sum(Count))%>% 
      arrange(Species, -count) %>% 
      mutate(Species = as.factor(Species))
    
    
    
  })
  
  ordered_species_vector <- reactive({
    top_by_year[['Species']]
  })
  
  
  # Render column graph
  output$topPlot <- renderPlot({
    
    # Add common name to 4-digit bird watching code
    bird_names2 <- c(   `AMWI` = "American Wigeon",
                        `CAGU`= "California Gull",
                        `ANHU` = "Anna's Hummingbird", 
                        `BBPL` = "Black-Bellied Plover",
                        `CALT` = "California Towhee",
                        `CATE`= "Caspian Tern",
                        `HOFI` = "House Finch",
                        `MODO` = "Mourning Dove",
                        `NSHO` = "Northern Shoveler",
                        `RUDU` = "Ruddy Duck",
                        `SAND` = "Sanderling",
                        `SEPL` = "Semipalmated Plover",
                        `SNPL` = "Snowy Plover",
                        `WCSP`= "White Crowned Sparrow",
                        `WEBL`= "Western Bluebird",
                        `WESA` = "Western Sandpiper",
                        `WILL` = "Willet",
                        `CLSW` = "Cliff Swallow",
                        `SOSP` = "Song Sparrow",
                        `LESA` = "Least Sandpiper")
    
    # Create top species column graph
    ggplot( top_filtered()[top_filtered()$Species %in% top_by_year()$Species,], aes( x = Month, y = count)) +  
      geom_col(fill = "skyblue3") + 
      facet_wrap(~ Species, scale = 'free_x', labeller = as_labeller(bird_names2)) +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_discrete( drop = FALSE, breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
      scale_y_continuous(expand = c(0, 0))+
      labs( y = "Number Observed")+
      ggtitle(paste("Top Bird Species at COPR in Monthly Observations", input$top_year))
  })
  
  # Render table of codes and names
  output$topTable <- renderDataTable({
    bird_table <- bird_names[bird_names$alpha_code %in% top_by_year()$Species,] 
    colnames(bird_table) <- c("Alpha Code (English)", "Alpha Code (Scientific)", "Common Name", "Scientific Name")
    bird_table
  }, options= list(paging = FALSE, searching = FALSE,  pageLength = 6))

  
  
  # Render reserve map
  output$polygon_map <- renderLeaflet({
    
    
    labels <- sprintf(
      "%s",
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
                 group = "Markers",
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", padding = "3px 8px"),
                   textsize = "15px")) %>% 
      addMarkers(lat = 34.407912, lng = -119.878954,
                 label = "Sands Beach Entrance",
                 group = "Markers",
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", padding = "3px 8px"),
                   textsize = "15px")) %>% 
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


