

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Packages  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~  ##

## shiny dashboard
library(shiny)
library(shinydashboard)


## graphs
library(ggplot2)
library(viridis)


## utils
library(tidyr)
library(dplyr)
library(stringr)


## interactive Map
library(sf)
library(leaflet)


## Dataset
library(activityinfo)



## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Dashboard structure - Shiny UI ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  ##

## Elaborate header with LFTW logo
dbHeader <- dashboardHeader(title = "Survey Inclusive Rapid Assessment Dashboard", titleWidth = "400px",
                            tags$li(a(href = 'http://shinyapps.company.com',
                                      icon("power-off"),
                                      title = "Back to Apps Home"),
                                    class = "dropdown"),
                            tags$li(a(href = 'https://www.light-for-the-world.org/our-work/disability-inclusion/humanitarian-action/',
                                      img(src = 'logo.jpg',
                                          title = "LFTW", height = "30px"),
                                      style = "padding-top:10px; padding-bottom:10px;"),
                                    class = "dropdown"))


ui <- dashboardPage(
  
  skin = "black", 
  
  
  dbHeader,
  
  ## Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Response Info", tabName = "resp_tab", icon = icon("dashboard")),
      menuItem("WGQ Tab", tabName = "wgq_tab", icon = icon("dashboard")),
      menuItem("Barriers Tab", tabName = "BE_tab", icon = icon("dashboard")),
      
      ## Input section
      selectInput("Location", "Select Location:", 
                  choices = unique(combDF$Parent.location.posto.distrito.district),
                  selected = unique(combDF$Parent.location.posto.distrito.district),
                  multiple = TRUE),
      selectInput("Displacement", "Select Displacement status:", 
                  choices = sort(unique(combDF$Parent.displacementHH)),
                  selected = unique(combDF$Parent.displacementHH),
                  multiple = TRUE),
      selectInput("Sex", "Sex of Respondent", 
                  choices = unique(combDF$respSEX),
                  selected = unique(combDF$respSEX),
                  multiple = TRUE),
      sliderInput("Age", "Age Range of Respondent", min = 0, max = 100, value = c(0, 100))
      
    ), 
    
    # Custom sidebar style
    tags$style(HTML("
                .main-sidebar {
                background-color: #FFEA00;
                }
                .sidebar-menu > li > a {
                color: #FFEA00;
                }
                .sidebar-menu > li.active > a {
                background-color: #FFEA00;
                color: #FFEA00;
                }
                                ")
    )),
  
  
  ## Body
  dashboardBody(
    
    # Custom body style
    tags$head(tags$style(HTML("
      .content-wrapper {
        background-color: #FFEA00;
      }
      .box {
        border-top: 3px solid #FFEA00;
      }
    "))),
    
    
    tabItems(
      
      ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Tab 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~  ##
      
      tabItem(tabName = "resp_tab",
              fluidRow(
                column(width = 12,
                       
                       box(title = "1. Interactive Map", 
                           #status = "primary", 
                           solidHeader = TRUE,
                           leafletOutput("map")),
                       box(title = "Overview of Sample size", 
                           #status = "primary", 
                           solidHeader = TRUE,
                           verbatimTextOutput("number_card")),
                       box(title = "Clicked Marker Info", 
                           #status = "primary", 
                           solidHeader = TRUE,
                           verbatimTextOutput("info"))
                       
                       
                )),
              fluidRow(
                
                column(width = 12,
                       
                       box(title = "2. Sex of Respondents", 
                           solidHeader = TRUE,
                           plotOutput("sex_disag_barplot")),
                       
                       box(title = "3. Age of Respondents", 
                           solidHeader = TRUE,
                           plotOutput("age_disag_barplot"))),
                
                fluidRow(
                  column(width = 12,
                         box(title = "4. Displacement status of Respondents", 
                             solidHeader = TRUE, width = 12,
                             plotOutput("displ_barplot"))))
                
              )
              
              
      ),
      
      ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Tab 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~  ##
      
      tabItem(tabName = "wgq_tab",
              
              fluidRow(
                
                box(title = "Functional Domains Filter (applies to all Plots)", status = NULL, 
                    solidHeader = TRUE, 
                    background = "black",
                    selectInput("FD", "Functional domains:",
                                choices = NULL,
                                selected = NULL,
                                multiple = TRUE)),
                
                ## WGQ
                box(title = "WGQ Filter (Applies to plot 1 - Overview)", solidHeader = TRUE,
                    background = "black",
                    selectizeInput("WGQ1", "Select Washington Group Question:",
                                   choices = NULL))
                
                
              ),
              
              fluidRow(
                ## Filters
                column(width = 12,
                       
                       box(title = "1. WGQ Overview of Response", 
                           solidHeader = TRUE,
                           plotOutput("WGQ_barplot")),
                       
                       box(title = "2. Estimated Disability Prevalence", 
                           solidHeader = TRUE,
                           plotOutput("WGQ_Prevalence"))))
              
              
      ),
      
      
      ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Tab 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  ##
      
      tabItem(tabName = "BE_tab",
              
              fluidRow(
                column(width = 12,
                       
                       ## Topic/group/category investigated 
                       box(title = "A. Humanitarian Assistance Type Filter", 
                           solidHeader = TRUE, background = "black",
                           selectInput("HA", "Select Type of Humanitarian Assistance:", 
                                       choices = sort(unique(combDF$HA)))),
                       
                       box(title = "1. Barriers Overview of All Responses", 
                           #status = "primary", 
                           solidHeader = TRUE,
                           plotOutput("Barriers_barplot")))
                
                
              ),
              
              
              fluidRow(
                
                
                column(width = 12,
                       
                       box(title = "B. Which Barriers were faced?", 
                           solidHeader = TRUE,
                           background = "black",
                           selectizeInput("Barriers", "Select barrier(s) faced by typing the number/item available items in Plot 1. Multiple selections are allowed:",
                                          choices = NULL,
                                          selected = NULL,
                                          multiple = TRUE, 
                                          options = list(placeholder = 'select a barrier from list'))),
                       
                       
                       
                       box(title = "2. Reason per Disability status", solidHeader = TRUE,
                           plotOutput("BE_which_disa")))),
              
              
              
              fluidRow(
                
                box(title = "C. Which reasons behind this barrier?", 
                    solidHeader = TRUE,
                    background = "black",
                    selectizeInput("Reasons", "Select reason(s) explaining barrier(s) by typing the number/item available in Plot 3. Multiple selections are allowed:",
                                   choices = NULL,
                                   selected = NULL,
                                   multiple = TRUE,
                                   options = list(placeholder = 'select a reason from list')))),
              fluidRow(
                
                box(title = "3. Reasons behind barriers", 
                    solidHeader = TRUE, width = 12,
                    plotOutput("Reasons_barplot"))),
              
              fluidRow(
                box(title = "4. Reason per Disability status", 
                    solidHeader = TRUE, width = 12,
                    plotOutput("BE_why_disa")))
              
              
      )
      
      
      
      
    )
  )
)












## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Shiny - Server ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  ##



# Define server logic
server <- function(input, output, session) {
  
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Tab 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~  ##
  
  
  # Filter data based on user input
  selected_data <- reactive({
    filter(combDF, Parent.location.posto.distrito.district %in% input$Location) %>%
      filter(Parent.displacementHH %in% input$Displacement) %>%
      filter(respSEX %in% input$Sex) %>%
      filter(respAGE >= input$Age[1] & respAGE <= input$Age[2])
    
  })
  
  
  
  # ## Specific to Barriers and enablers
  # be_selected_data <- reactive({
  #         selected_data() %>%
  #                 filter(BE %in% input$BE)
  # })
  
  ## Interactive map of survey locations
  output$map <- renderLeaflet({
    leaflet(selected_data()) %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%  # Satellite view
      addCircleMarkers(~ Parent.geo_location.longitude, ~Parent.geo_location.latitude,
                       layerId = ~Parent.hhCODE, radius = 5, 
                       color = ~ifelse(Parent.location.posto.distrito.district == "Metuge", "red", "blue")) 
  })
  
  
  
  
  ## the number cards
  # Render number card
  output$number_card <- renderPrint({
    
    ## unique entries
    hh_count <- length(unique(selected_data()[,"Parent.hhCODE"]))
    total_count <- length(unique(selected_data()[,"X.id"]))
    #wgq_total_count <- length(unique(wgq_selected_data()[,"X.id"]))
    #be_total_count <- length(unique(be_selected_data()[,"X.id"]))
    
    ## message
    cat("Number of households:", hh_count, "\n",
        "Number of individual respondents:", total_count, "\n")#,
    #"Number of WGQ respondents:", wgq_total_count, "\n") #,
    #"Number of Barriers and Enablers respondents:", be_total_count)
    
    
  })
  
  ## diagnostic tool that indicates nearest observation to clicked point
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    if (!is.null(click)) {
      selected_id <- click$id
      output$info <- renderPrint({
        selected_id
      })
    }
  })
  
  output$sex_disag_barplot <- renderPlot({
    
    # Create barplot for sex disaggregation of respondents
    ggplot(selected_data(), aes(x = Sex)) +
      geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(Sex))) +
      geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, size = 5, family = "arial",  colour = "black") +
      labs(title = "",
           x = "Sex",
           y = "Percentage") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      coord_flip() + 
      scale_fill_viridis(discrete = T, option = "H") +
      theme(legend.position = "none",
            axis.text = element_text(size = 13.5, family="arial", face = "bold", colour = "black"),
            axis.title.x = element_text(size = 16, family = "arial", face = "bold",  color = "black"),
            axis.title.y = element_text(size = 16, family = "arial", face = "bold",  color = "black"))
    
  })
  
  output$age_disag_barplot <- renderPlot({
    
    
    # Create barplot for sex disaggregation of respondents
    ggplot(selected_data(), aes(x = AgeGroups)) +
      geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(AgeGroups))) +
      geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, size = 5, family = "arial",  colour = "black") +
      labs(title = "",
           x = "Age Group",
           y = "Percentage") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      coord_flip() + 
      scale_fill_viridis(discrete = T, option = "H") +
      theme(legend.position = "none",
            axis.text = element_text(size = 13.5, family="arial", face = "bold", colour = "black"), 
            axis.title.x = element_text(size = 16, family = "arial", face = "bold",  color = "black"),
            axis.title.y = element_text(size = 16, family = "arial", face = "bold",  color = "black"))
    
    
    
  })
  
  # Create barplot for sex disaggregation of respondents
  output$displ_barplot <- renderPlot({
    
    ggplot(selected_data(), aes(x = Parent.displacementHH)) +
      geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(Parent.displacementHH))) +
      geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, size = 5, family = "arial",  colour = "black") +
      labs(title = "",
           x = "Status",
           y = "Percentage") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      coord_flip() + 
      scale_fill_viridis(discrete = T, option = "H") +
      theme(legend.position = "none",
            axis.text = element_text(size = 13.5, family="arial", face = "bold", colour = "black"),
            axis.title.x = element_text(size = 16, family = "arial", face = "bold",  color = "black"),
            axis.title.y = element_text(size = 16, family = "arial", face = "bold",  color = "black"))
    
    
    
  })
  
  
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~   Tab 2  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  ##
  
  
  
  
  
  
  ## Here two new reactive objects are needed to look at data/subset based on fully colinear variables
  # Reactive expression for unique groups
  fd <- reactive({
    unique(selected_data()$FD)
  })
  
  ## Reactive expression for unique wgq subgroups based on selected FD group
  wgq <- reactive({
    req(input$FD)
    selected_data() %>%
      filter(FD %in% input$FD) %>%
      pull(WGQ) %>%
      unique()
  })
  
  # Update group input choices
  observe({
    updateSelectInput(session, "FD", choices = fd(), selected = fd())
  })
  
  
  
  # Update the subgroup input when the group input changes
  observeEvent(input$FD, {
    
    updateSelectizeInput(session, "WGQ1", 
                         choices = wgq(), server = TRUE)
    
  })
  
  
  # Reactive expression for selected data
  d2_selected_data <- reactive({
    req(input$FD, input$WGQ1)
    selected_data() %>%
      filter(FD %in% input$FD, WGQ %in% input$WGQ1)
  })
  
  d3_selected_data <- reactive({
    req(input$FD)
    selected_data() %>%
      filter(FD %in% input$FD)
  })
  
  
  ## WGQ Barplot
  output$WGQ_barplot <- renderPlot({
    
    # Create barplot
    ggplot(d2_selected_data(), aes(x = WGQ_Response)) +
      geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(WGQ_Response))) +
      geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, size = 5, family = "arial",  colour = "black") +
      labs(title = paste("Responses for Question", input$WGQ1),
           x = "Response",
           y = "Percentage") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      ## adding the 15% threshold
      geom_hline(yintercept = 0.15, linetype = "dashed", colour = "red") +
      #ylim(0,100) +
      coord_flip() + 
      scale_fill_viridis(discrete = T, option = "H") +
      theme(legend.position = "none",
            axis.text = element_text(size = 13.5, family="arial", face = "bold", colour = "black"),
            axis.title.x = element_text(size = 16, family = "arial", face = "bold",  color = "black"),
            axis.title.y = element_text(size = 16, family = "arial", face = "bold",  color = "black"))
    
  })
  
  
  
  
  ## yes no data
  respTYPE <- reactive({
    d3_selected_data() %>%
      group_by(X.id) %>%
      summarize(
        YesNo = ifelse(any(WGQ_Response %in% c("3. A lot of difficulty", "4. Cannot do at all",
                                               "2. A lot", "4. Most days", "5. Every day",
                                               "2. Most of the day")), "Yes", "No")
      ) %>%
      ungroup()
  })       
  
  
  ## graph 
  ## WGQ Barplot
  output$WGQ_Prevalence <- renderPlot({
    
    # Create barplot
    ggplot(respTYPE(), aes(x = YesNo)) +
      geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(YesNo))) +
      geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, size = 5, family = "arial",  colour = "black") +
      labs(title = "",
           x = "Disability",
           y = "Percentage") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      ## adding the 15% threshold
      geom_hline(yintercept = 0.15, linetype = "dashed", colour = "red") +
      coord_flip() + 
      scale_fill_viridis(discrete = T, option = "H") +
      theme(legend.position = "none",
            axis.text = element_text(size = 13.5, family="arial", face = "bold", colour = "black"),
            axis.title.x = element_text(size = 16, family = "arial", face = "bold",  color = "black"),
            axis.title.y = element_text(size = 16, family = "arial", face = "bold",  color = "black"))
    
  })
  
  
  
  
  
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~   Tab 3  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  ##
  
  
  ## Filtering of specific Barrier/Enabler
  ## Reactive expression for unique subgroups based on selected group
  haDF <- reactive({
    selected_data() %>%
      filter(HA %in% input$HA) 
  })
  
  ## Barrier and Enabler barplot
  ## the graph
  output$Barriers_barplot <- renderPlot({
    
    
    # Create barplot
    ggplot(haDF(), aes(x = Barriers)) +
      geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(Barriers))) +
      geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, size = 5, family = "arial",  colour = "black") +
      labs(title = paste("Which Barriers for ", input$HA),
           x = "Response",
           y = "Percentage") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      coord_flip() + 
      scale_fill_viridis(discrete = T, option = "H") +
      theme(legend.position = "none",
            axis.text = element_text(size = 13.5, family="arial", face = "bold", colour = "black"),
            axis.title.x = element_text(size = 16, family = "arial", face = "bold",  color = "black"),
            axis.title.y = element_text(size = 16, family = "arial", face = "bold",  color = "black"))
    
  })
  
  
  # Update Barriers input choices based on selected item in HA (i.e. category)
  observe({
    updateSelectizeInput(session, "Barriers", choices = sort(haDF()$Barriers), selected = NULL, server = TRUE)
  })
  
  
  
  ## subset data based on choice of barrier
  haDF_which <- reactive({
    haDF() %>%
      filter(Barriers %in% input$Barriers)
  })
  
  
  # Update Barriers input choices based on selected item in HA (i.e. category)
  observe({
    updateSelectizeInput(session, "Reasons", choices = sort(haDF_which()$Reasons), selected = haDF_which()$reasons, server = TRUE)
  })
  
  
  
  ## Yes/No disability status based on WGQ response
  respTYPE2 <- reactive({
    haDF_which() %>%
      group_by(X.id) %>%
      summarize(
        YesNo = ifelse(any(WGQ_Response %in% c("3. A lot of difficulty", "4. Cannot do at all",
                                               "2. A lot", "4. Most days", "5. Every day",
                                               "2. Most of the day")), "Yes", "No")
      ) %>%
      ungroup()
  })
  
  
  ## bring info on disability status into main DF
  # Add the YesNo column to haDF_which()
  disa_haDF_which <- reactive({
    haDF_which() %>%
      left_join(respTYPE2(), by = "X.id")
  })
  
  
  
  ## first plot is per barrier type (WHICH)
  output$BE_which_disa <- renderPlot({
    
    # Create barplot
    ggplot(disa_haDF_which(), aes(x = YesNo)) +
      geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(YesNo))) +
      geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, size = 5, family = "arial",  colour = "black") +
      labs(title = "",
           x = "Disability",
           y = "Percentage") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      coord_flip() + 
      scale_fill_viridis(discrete = T, option = "H") +
      theme(legend.position = "none",
            axis.text = element_text(size = 13.5, family="arial", face = "bold", colour = "black"),
            axis.title.x = element_text(size = 16, family = "arial", face = "bold",  color = "black"),
            axis.title.y = element_text(size = 16, family = "arial", face = "bold",  color = "black"))
    
  })
  
  
  ## subset data based on select Reason
  haDF_why <- reactive({
    haDF_which() %>%
      filter(Reasons %in% input$Reasons)
  })
  
  
  ## Second plot: reasons given why
  output$Reasons_barplot <- renderPlot({
    
    
    # Create barplot
    ggplot(haDF_which(), aes(x = Reasons)) +
      geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(Reasons))) +
      geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, size = 5, family = "arial",  colour = "black") +
      labs(title = paste("Which Reasons for ", unique(haDF_why()$Barriers)),
           x = "Response",
           y = "Percentage") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      coord_flip() + 
      scale_fill_viridis(discrete = TRUE) +
      theme(legend.position = "none",
            axis.text = element_text(size = 13.5, family="arial", face = "bold", colour = "black"),
            axis.title.x = element_text(size = 16, family = "arial", face = "bold",  color = "black"),
            axis.title.y = element_text(size = 16, family = "arial", face = "bold",  color = "black"))
    
  })
  
  ## subset data based on select Reason
  haDF_why <- reactive({
    haDF_which() %>%
      filter(Reasons %in% input$Reasons)
  })
  
  
  ## Yes/No disability status based on WGQ response
  respTYPE3 <- reactive({
    haDF_why() %>%
      group_by(X.id) %>%
      summarize(
        YesNo = ifelse(any(WGQ_Response %in% c("3. A lot of difficulty", "4. Cannot do at all",
                                               "2. A lot", "4. Most days", "5. Every day",
                                               "2. Most of the day")), "Yes", "No")
      ) %>%
      ungroup()
  })
  
  
  ## bring info on disability status into main DF
  # Add the YesNo column to haDF_why()
  disa_haDF_why <- reactive({
    haDF_why() %>%
      left_join(respTYPE3(), by = "X.id")
  })
  
  
  
  ## Compare disability vs no disability for distinct barriers and enablers
  output$BE_why_disa <- renderPlot({
    
    # Create barplot
    ggplot(disa_haDF_why(), aes(x = YesNo)) +
      geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(YesNo))) +
      geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, size = 5, family = "arial",  colour = "black") +
      labs(title = "",
           x = "Disability",
           y = "Percentage") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      coord_flip() + scale_fill_viridis(discrete = T, option = "H") +
      theme(legend.position = "none",
            axis.text = element_text(size = 13.5, family="arial", face = "bold", colour = "black"),
            axis.title.x = element_text(size = 16, family = "arial", face = "bold",  color = "black"),
            axis.title.y = element_text(size = 16, family = "arial", face = "bold",  color = "black"))
    
  })
  
  
}


# Run the application
shinyApp(ui = ui, server = server) ## options argument ensure shiny app height improved


