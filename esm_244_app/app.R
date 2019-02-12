library(shiny)
library(tidyverse)
library(shinythemes)

# Define UI for application
ui <- fluidPage(
  theme = shinytheme("sandstone"),
  
  # Application title
  titlePanel(tags$img(src = "https://copr.nrs.ucsb.edu/sites/default/files/copr-logo.jpg", "Snowy Plover and Shorebird Abundance at Coal Oil Point Reserve"),
             "Snowy Plover and Shorebird Abundance at Coal Point Oil Reserve"),
  
  navbarPage( "",
              
              # First tab
              tabPanel("Summary",
                       h1("About this App "),
                       p("This data exploration tool is intended for use by educators, COPR visitors and the public."),
                       h1("Coal Oil Point Reserve "),
                       p("COPR is a small area surrounded by increasing urbanization.  Our main challenge is to maintain the habitats and species that live here, despite habitat fragmentation, pollution, disturbances, and climate change.  Most of our conservation programs started because we noticed a species' population declining over time.  We first try to understand the factors causing the decline and then implement actions to reduce or eliminate these threats.  For many species, the reserve's boundaries are not sufficient for their safety and survival.  Cooperation with neighboring properties in a much larger spacial scale is essential to the protection of some species."),
                       tags$a(href="https://copr.nrs.ucsb.edu/about/programs/snowy-plover-conservation", "COPR website"),
                       h2("Western Snowy Plover"),
                       p("The Western Snowy Plover, Charadrius nivosus nivosus, is a shorebird that inhabits beaches and lake shores.  The Pacific Coast population of the Western Snowy Plover was listed as \"threatened\" under the Endangered Species Act in 1993 because of declining populations mainly due to loss of habitat.  The stretch of beach between Isla Vista and Ellwood (including Sands Beach) was designated \"Critical Habitat\" in December of 1999; at the time of the critical habitat designation, the population in the entire Pacific Coast of the United States was estimated at less than 1500 individuals.  Coal Oil Point Reserve, with its sandy beach, sand dunes, and adjacent estuary mouth is one of a few choice west coast locations where the snowy plovers can still breed and thrive.  With public education and symbolic fences, the plovers at COPR made a comeback. "),
                       h2("Shorebird Abundance"),
                       p("Birders count birds at the reserve daily, but these data are seldom associated with a specific area.  Therefore, they cannot be used to study trends over time.  For this type of study, the abundance and diversity of birds need to be measured in the same way, and within the same area, every time.  
                         
                         In February 2015, Coal Oil Point Reserve implemented a long-term monitoring program to measure bird abundance and diversity within 10 permanent sampling areas on the reserve.  These surveys provide useful data for researchers and students and allow us to understand how birds use these habitats."),
                       tags$img(src = "https://copr.nrs.ucsb.edu/sites/default/files/styles/top_image/public/images/9.jpg?itok=Lv_jYTEA", align = "center", alt = "Image of Western Snowy Plover chicks")
                       
                       ),
              
              
              # Second tab
              tabPanel("Western Snowy Plover",
                       titlePanel("Nest Failure by year"),
                       
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
                       
                       # Sidebar with input for who knows
                       sidebarLayout(
                         sidebarPanel(
                           checkboxGroupInput("stage",
                                              "Select breeding stage:",
                                              choices = c("nests", "eggs", "hatched", "fledged"),
                                              selected = "nests")
                           
                         ),
                         # Show a plot of the generated distribution
                         mainPanel(
                           plotOutput("linesPlot")
                         )
                       )
                       
                       
              ),
              
              # Third tab
              tabPanel("Shorebirds",
                       
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
                       ))
              
              ),
  hr(),
  tags$footer("Written by Angie Bouche <abouche@bren.ucsb.edu> and Anna Calle <annagcalle@bren.ucsb.edu> in programming language R version 3.5.1 (2018-07-02) "),
  hr()
)









# Define server logic
server <- function(input, output) {
  
  
  output$distPlot <- renderPlot({
    # Render a column graph for predators data
    predators_graph2 <- snowyplover %>% 
      select(year, predators) %>% 
      filter( predators != "NA") %>%
      filter( year == input$year) %>% 
      count(predators, year)
    
    ggplot(predators_graph2, aes( x = predators, y = n)) +
      geom_col() +
      xlab("Predator") +
      ylab("Number of Predated Nests") +
      scale_y_continuous(limits = c(0,20), breaks = c(0,5,10,15,20)) +
      theme_classic()
  })
  
  output$linesPlot <- renderPlot({
    # Render a line graph
    breeding_filtered <- breeding_table %>% 
      filter( stage == input$stage[1] | stage == input$stage[2] | stage == input$stage[3] | stage == input$stage[4])
    
    ggplot(breeding_filtered, aes(x = year, y = count, group = stage)) +
      geom_line(aes(color = stage)) +
      theme_classic()
  })
  
  
  output$diversity <- renderPlot({
    
    bird_sp <- shorebirds %>% 
      filter(Year == input$div_year) %>% #Filter data set by year (use radio buttons or drop down menu to select)
      group_by(Species) %>% 
      count(Polygon_Location, Species) #Turn list of observed species into counts of how many different species observe
    
    #Graph formatting
    ggplot(bird_sp, aes(x=Polygon_Location))+
      geom_bar()+
      labs(x= "Location", y = "Number of Species Observed", title = "Shorebird Diversity at COPR (2016)")+ 
      theme_classic()+
      theme(plot.title = element_text(hjust = 0.5)) 
  })
}


# Run the application 
shinyApp(ui = ui, server = server)



