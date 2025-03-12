

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Packages  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~  ##

## shiny dashboard
library(shiny)
library(shinydashboard)
library(bslib)


## graphs
library(ggplot2)
library(viridis)


## utils
library(tidyr)
library(dplyr)
library(stringr)
library(DT) ## datatables html widget

## interactive Map
#library(sf)
#library(leaflet)

## import datasets
library(readxl)
#library(rhdx) ## Github

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   Data  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  ##

## import datasets from HDX or hard disk

## load from  disk
#path_to_data = "C:\\Users\\n.abu-samra-spencer\\Downloads\\R_workspace\\Elrha_DTM" 
# hhHDX  <- read_xlsx(paste(path_to_data, "SIRA_for_HDX\\SIRA_household.xlsx", sep = "\\"))
# demoDF <- read_xlsx(paste(path_to_data, "SIRA_for_HDX\\SIRA_demographics.xlsx", sep = "\\"))
# wgqDF <- read_xlsx(paste(path_to_data, "SIRA_for_HDX\\SIRA_WGQ.xlsx", sep = "\\"))
# barriersDF <- read_xlsx(paste(path_to_data, "SIRA_for_HDX\\SIRA_Barriers.xlsx", sep = "\\"))
#enablersDF <- read_xlsx(paste(path_to_data, "SIRA_qualitative\\Translations\\enablers.xlsx", sep = "\\"))


hhHDX  <- read_xlsx("input_data/SIRA_household.xlsx")
demoDF <- read_xlsx("input_data/SIRA_demographics.xlsx")
wgqDF <- read_xlsx("input_data/SIRA_WGQ.xlsx")
barriersDF <- read_xlsx("input_data/SIRA_Barriers.xlsx")
enablersDF <- read_xlsx("input_data/enablers.xlsx")


###########################################
### Additional cleaning and formatting  ###
###########################################


## displacement status simplified
hhHDX$Displaced <- "No"
hhHDX[hhHDX$Parent.displacementHH %in% c("1. IDP - Internally displaced person",
                                         "3. Refugee", "5. Mixed - Host community and displaced"), "Displaced"] <- "Yes"
hhHDX[hhHDX$Parent.displacementHH %in% c("8. Refused", "7. I don't know", "6.None of the above applies"), "Displaced"] <- "n/a"


## for efficient handling of formatting
#indLIST <- list("demoDF" = demoDF, "wgqDF" = wgqDF, "barriersDF" = barriersDF, "enablersDF" = enablersDF)


# ## Merge and simplified age disag
# indLIST <- lapply(indLIST, function(x){
#   
#   
#   ## Age as categoried for some simple barplots
#   x$respAGE <- strtoi(x$respAGE)
#   x$AgeGroup <- "16 to 49"
#   x[is.na(x$respAGE) == F & x$respAGE < 16, "AgeGroup"] <- "0 to 15"
#   x[is.na(x$respAGE) == F & x$respAGE > 49, "AgeGroup"] <- "50 and above"
#   
#   
#   ## combine with HH info
#   y <- merge(hhHDX, x, all.y = T)
#   
#   return(y)
#   
# })
# 
# list2env(indLIST, envir = .GlobalEnv)

#demoDF <- indLIST$demoDF


## Demographics of household
demoDF$respAGE <- strtoi(demoDF$respAGE)
demoDF$AgeGroup <- "16 to 49"
demoDF[is.na(demoDF$respAGE) == F & demoDF$respAGE < 16, "AgeGroup"] <- "0 to 15"
demoDF[is.na(demoDF$respAGE) == F & demoDF$respAGE > 49, "AgeGroup"] <- "50 and above"
## combine with HH info
demoDF <- merge(hhHDX, demoDF, all.y = T)

## formatting specific to demoDF
demoDF$childSCHOOL[is.na(demoDF$childSCHOOL)] <- "Not Applicable"
demoDF$respHEALTH[is.na(demoDF$respHEALTH)] <- "Not Applicable"




## demographics of household
wgqDF$respAGE <- strtoi(wgqDF$respAGE)
wgqDF$AgeGroup <- "16 to 49"
wgqDF[is.na(wgqDF$respAGE) == F & wgqDF$respAGE < 16, "AgeGroup"] <- "0 to 15"
wgqDF[is.na(wgqDF$respAGE) == F & wgqDF$respAGE > 49, "AgeGroup"] <- "50 and above"
## combine with HH info
wgqDF <- merge(hhHDX, wgqDF, all.y = T)



## Demographics of household
barriersDF$respAGE <- strtoi(barriersDF$respAGE)
barriersDF$AgeGroup <- "16 to 49"
barriersDF[is.na(barriersDF$respAGE) == F & barriersDF$respAGE < 16, "AgeGroup"] <- "0 to 15"
barriersDF[is.na(barriersDF$respAGE) == F & barriersDF$respAGE > 49, "AgeGroup"] <- "50 and above"
## combine with HH info
barriersDF <- merge(hhHDX, as.data.frame(barriersDF), all.y = T)


## Demographics of household
enablersDF$respAGE <- strtoi(enablersDF$respAGE)
# enablersDF$AgeGroup <- "16 to 49"
# enablersDF[is.na(enablersDF$respAGE) == F & enablersDF$respAGE < 16, "AgeGroup"] <- "0 to 15"
# enablersDF[is.na(enablersDF$respAGE) == F & enablersDF$respAGE > 49, "AgeGroup"] <- "50 and above"
## combine with HH info
enablersDF <- merge(hhHDX, as.data.frame(enablersDF), all.y = T)

## Rename for human readable table
colnames(enablersDF)[(ncol(enablersDF)-4):ncol(enablersDF)] <- c("Distributions", "Services", "Livelihood", "SRH", "Fear")


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Dashboard structure - Shiny UI ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  ##

## Elaborate header with LFTW logo
dbHeader <- dashboardHeader(title = "SIRA Dashboard", titleWidth = "300px",
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
        
        
        #dashboardHeader(title = "Rapid Assessment Tool Dashboard",
        #               titleWidth = "400px"),
        dbHeader,
        
        ## Sidebar
        dashboardSidebar(
                sidebarMenu(
                        menuItem("Household Demographics", tabName = "HH_tab", icon = icon("house")),
                        menuItem("Individual Socio-Economic", tabName = "Ind_tab", icon = icon("person")),
                        menuItem("Disability Status", tabName = "WGQ_tab", icon = icon("wheelchair")),
                        menuItem("Barriers to Access", tabName = "Barrier_tab", icon = icon("hands-holding-child")),
                        menuItem("Enablers to Access", tabName = "Enabler_tab", icon = icon("table")),
                        
                        ## Input section
                        selectInput("Location", "Select Location:", 
                                    choices = c("Pemba", "Metuge"),
                                    selected = c("Pemba", "Metuge"),
                                    multiple = TRUE),
                        selectInput("Displacement", "Select Displacement status:", 
                                    choices = c("No", "Yes"),
                                    selected = c("No", "Yes"),
                                    multiple = TRUE),
                        selectInput("Sex", "Sex of Respondent", 
                                    choices = c("1. Male", "2. Female"),
                                    selected = c("1. Male", "2. Female"),
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
                        
                        tabItem(tabName = "HH_tab",
                                
                                
                                
                                fluidRow(
                                        
                                        column(width = 12,
                                               
                                               box(uiOutput("hh_number_card"),
                                                   width = 12)), 
                                        
                                        column(width = 12,              
                                               box(uiOutput("sentence_box"),
                                                   width = 12))),
                                
                                fluidRow(
                                        
                                        column(width = 12,
                                               
                                               box(title = "1. Sex of Household Head", 
                                                   solidHeader = TRUE,
                                                   plotOutput("hh_sex_disag_barplot")),
                                               
                                               box(title = "2. Age of Household Head", 
                                                   solidHeader = TRUE,
                                                   plotOutput("hh_age_disag_barplot")))),
                                
                                
                                fluidRow(
                                        
                                        column(width = 12,
                                               box(title = "3. Household Displacement Status",
                                                   width = 12,
                                                   solidHeader = TRUE, 
                                                   plotOutput("displ_barplot"))))),
                        
                        
                        tabItem(tabName = "Ind_tab",
                                
                                fluidRow(
                                        
                                        box(title = "",#Number of individual respondents", 
                                            #status = "primary", 
                                            width = 12, 
                                            solidHeader = TRUE,
                                            uiOutput("ind_number_card")),
                                        
                                        
                                        column(width = 12,
                                               
                                               box(title = "1. Sex of Surveyed Respondents",
                                                   solidHeader = TRUE,
                                                   plotOutput("ind_sex_disag_barplot")),
                                               
                                               box(title = "2. Age of Surveyed Respondents",
                                                   solidHeader = TRUE,
                                                   plotOutput("ind_age_disag_barplot"))),
                                        
                                        
                                        column(width = 12,
                                               
                                               box(title = "3. Working for an Income",
                                                   solidHeader = TRUE,
                                                   plotOutput("ind_work_barplot")),
                                               
                                               box(title = "4. Past Education of Respondents",
                                                   solidHeader = TRUE,
                                                   plotOutput("ind_edu_barplot")),
                                               
                                               box(title = "5. Current Schooling of Children (Please select relevant age in sidebar)",
                                                   solidHeader = TRUE,
                                                   width = 12,
                                                   plotOutput("ind_school_barplot")),
                                               
                                               box(title = "6. Chronic disease for Older Individuals (please select relevant age in sidebar)",
                                                   solidHeader = TRUE,
                                                   width = 12,
                                                   plotOutput("ind_disease_barplot"))
                                               
                                        ))),
                        
                        
                        
                        ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Tab 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~  ##
                        
                        tabItem(tabName = "WGQ_tab",
                                
                                fluidRow(
                                        
                                        
                                        
                                        box(title = "1. Functional Domains Filter (applies to all Plots)", status = NULL,
                                            solidHeader = TRUE,
                                            width = 12,
                                            background = "black",
                                            selectInput("FD", "Functional domains:",
                                                        choices = NULL,
                                                        selected = NULL,
                                                        multiple = TRUE)),
                                        
                                        box(title = "2. Estimated Disability Prevalence",
                                            solidHeader = TRUE,
                                            width = 12,
                                            plotOutput("WGQ_Prevalence"))),
                                
                                
                                
                                
                                
                                fluidRow(
                                        
                                        ## WGQ
                                        box(title = "4. WGQ Filter", solidHeader = TRUE,
                                            background = "black",
                                            width = 12,
                                            selectizeInput("WGQ1", "Select Washington Group Question:",
                                                           choices = NULL)),
                                        
                                        box(title = "3. WGQ Overview of Response",
                                            solidHeader = TRUE,
                                            width = 12,
                                            plotOutput("WGQ_barplot")))),
                        
                        
                        ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Tab 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  ##
                        
                        tabItem(tabName = "Barrier_tab",
                                
                                fluidRow(
                                        column(width = 12,
                                               
                                               ## Topic/group/category investigated
                                               box(title = "1. Type of Humanitarian Assistance",
                                                   solidHeader = TRUE, background = "black",
                                                   width = 12,
                                                   selectInput("Humanitarian", "Select Type of Humanitarian Assistance:",
                                                               choices = sort(c("Fear", "Livelihood", "SRH", "Distributions", "Services"))))),
                                        column(width = 12,
                                               
                                               box(title = "2. Barriers",
                                                   #status = "primary",
                                                   width = 12,
                                                   solidHeader = TRUE,
                                                   plotOutput("Barriers_where_barplot")))),
                                
                                
                                fluidRow(
                                        
                                        
                                        column(width = 12,
                                               
                                               box(title = "3. Barriers select",
                                                   solidHeader = TRUE,
                                                   width = 12,
                                                   background = "black",
                                                   selectizeInput("Barriers", "Select barrier(s) faced by typing the number/item available items in Plot 1. Multiple selections are allowed:",
                                                                  choices = NULL,
                                                                  selected = NULL,
                                                                  multiple = TRUE,
                                                                  options = list(placeholder = 'select a barrier from list')))),
                                        column(width = 12,
                                               
                                               box(title = "4. Selected Barriers and Disability status", solidHeader = TRUE,
                                                   width = 12,
                                                   plotOutput("Barriers_where_disa_barplot")))),
                                
                                
                                
                                fluidRow(
                                        
                                        column(width = 12,
                                               
                                               box(title = "5. Which reasons behind this barrier?",
                                                   solidHeader = TRUE,
                                                   background = "black",
                                                   width = 12,
                                                   selectizeInput("Reasons", "Select reason(s) explaining barrier(s) by typing the number/item available in Plot 3. Multiple selections are allowed:",
                                                                  choices = NULL,
                                                                  selected = NULL,
                                                                  multiple = TRUE,
                                                                  options = list(placeholder = 'select a reason from list')))),
                                        
                                        
                                        column(width = 12,
                                               
                                               box(title = "6. Reasons for barriers",
                                                   solidHeader = TRUE, 
                                                   width = 12,
                                                   plotOutput("Barriers_why_barplot"))),
                                        
                                        
                                        column(width = 12,
                                               
                                               box(title = "7. Reasons for Barriers and Disability status",
                                                   solidHeader = TRUE, width = 12,
                                                   plotOutput("Barriers_why_disa_barplot"))))),
                        
                        tabItem(tabName = "Enabler_tab",
                                
                                fluidRow(
                                        
                                        box(title = "1. Enablers Overview",
                                            width = 12,
                                            #status = "primary",
                                            solidHeader = TRUE,
                                            dataTableOutput("enablerTABLE", "auto"))))
                        
                        
                        
                        
                )
        )
)












## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Shiny - Server ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  ##



# Define server logic
server <- function(input, output, session) {
        
        
        ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Tab 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~  ##
        
        
        # Filter data based on user input
        selected_data <- reactive({
                filter(hhHDX, Parent.District %in% input$Location) %>%
                        filter(Displaced %in% input$Displacement) 
                
                
        })
        
        
        ## filter other datasets based on selected households 
        ## Merge and simplified age disag
        selected_demoDF <- reactive({
                filter(demoDF, Parent.District %in% input$Location) %>%
                        filter(Displaced %in% input$Displacement) %>%
                        filter(respSEX %in% input$Sex) %>%
                        filter(respAGE >= input$Age[1] & respAGE <= input$Age[2])
        })
        
        
        
        
        ## the number cards
        # Render number card
        # Reactive expression to compute a value based on input$num
        
        
        # Render the number card UI dynamically
        output$hh_number_card <- renderUI({
                # Create a styled card to display the reactive value
                div(
                        style = "border: 1px solid #ddd; padding: 20px; margin: 10px; border-radius: 10px; background-color: #f9f9f9;",
                        h3("Households surveyed with SIRA"),
                        h1(style = "color: #007bff;", length(unique(selected_demoDF()[["hhID"]])))  # Display the value in large text
                )
        })
        
        
        ## reactive sentence with household size
        reactive_sentence <- reactive({
                paste0("The average household size is: ", 
                       round(mean(rowSums(selected_demoDF()[c("Parent.hhMaleAdult", "Parent.hhFemaleAdult", "Parent.hhMaleChild", "Parent.hhFemaleChild")], na.rm = T)), 0), 
                       ", of which ", 
                       round(mean(rowSums(selected_demoDF()[c("Parent.hhFemaleAdult")], na.rm = T)), 0), " Women, ",
                       round(mean(rowSums(selected_demoDF()[c("Parent.hhMaleAdult")], na.rm = T)), 0), " Men, and ",
                       round(mean(rowSums(selected_demoDF()[c("Parent.hhFemaleChild", "Parent.hhMaleChild")], na.rm = T)), 0), " Children")
        })
        
        
        
        # Render the sentence box UI dynamically
        output$sentence_box <- renderUI({
                # Create a styled card to display the reactive sentence
                div(
                        style = "border: 1px solid #ddd; padding: 20px; margin: 10px; border-radius: 10px; background-color: #f9f9f9;",
                        h3("Household composition"),
                        p(style = "font-size: 16px;", reactive_sentence())  # Display the sentence
                )
        })
        
        
        
        
        
        ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Household characteristics ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  ##
        
        
        ## barplot age disaggregation of household heads
        output$hh_age_disag_barplot <- renderPlot({
                
                
                # Create barplot for sex disaggregation of respondents
                ggplot(selected_data(), aes(x = Parent.hhAGE)) +
                        geom_histogram(bins = 17) +
                        #geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, size = 5, family = "arial",  colour = "black") +
                        labs(title = "",
                             x = "Age of Household Head",
                             y = "Number of Households") +
                        #scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
                        scale_fill_viridis(discrete = T, option = "H") +
                        theme(legend.position = "none",
                              axis.text = element_text(size = 13.5, family="arial", face = "bold", colour = "black"), 
                              axis.title.x = element_text(size = 16, family = "arial", face = "bold",  color = "black"),
                              axis.title.y = element_text(size = 16, family = "arial", face = "bold",  color = "black"))
                
                
                
        })
        
        ## plot showing sex disaggregation of respondents
        output$hh_sex_disag_barplot <- renderPlot({
                
                # Create barplot for sex disaggregation of respondents
                ggplot(selected_data(), aes(x = reorder(Parent.hhSEX, as.numeric(gsub("[^0-9]", "", Parent.hhSEX)), decreasing = T))) + 
                        geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(Parent.hhSEX))) +
                        #geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, size = 5, family = "arial",  colour = "black") +
                        labs(title = "",
                             x = "",
                             y = "Percentage of households") +
                        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
                        coord_flip() + 
                        scale_fill_viridis(discrete = T, option = "H") +
                        theme(legend.position = "none",
                              axis.text = element_text(size = 13.5, family="arial", face = "bold", colour = "black"),
                              axis.title.x = element_text(size = 16, family = "arial", face = "bold",  color = "black"),
                              axis.title.y = element_text(size = 16, family = "arial", face = "bold",  color = "black"))
                
        })
        
        
        # Create barplot for displacement status disaggregation of respondents
        output$displ_barplot <- renderPlot(execOnResize = TRUE, {
                
                ggplot(selected_data(), aes(x = reorder(Parent.displacementHH, as.numeric(gsub("[^0-9]", "", Parent.displacementHH)), decreasing = T))) + 
                        geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(Parent.displacementHH))) +
                        #geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, size = 5, family = "arial",  colour = "black") +
                        labs(title = "",
                             x = "",
                             y = "Percentage of Households") +
                        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
                        coord_flip() + 
                        scale_fill_viridis(discrete = T, option = "H") +
                        theme(legend.position = "none",
                              axis.text = element_text(size = 13, family="arial", face = "bold", colour = "black"),
                              axis.title.x = element_text(size = 16, family = "arial", face = "bold",  color = "black"),
                              axis.title.y = element_text(size = 16, family = "arial", face = "bold",  color = "black"))
                
                
                
        })
        
        
        
        
        ###################################################################################################################################
        
        ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Individual level demograohics  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  ##
        
        ###################################################################################################################################
        
        
        
        
        
        
        
        # Render the number card UI dynamically
        output$ind_number_card <- renderUI({
                # Create a styled card to display the reactive value
                div(
                        style = "border: 1px solid #ddd; padding: 20px; margin: 10px; border-radius: 10px; background-color: #f9f9f9;",
                        h3("Individual respondents"),
                        h1(style = "color: #007bff;", length(unique(selected_demoDF()[["indID"]])))  # Display the value in large text
                )
        })
        
        
        ## plot showing sex disaggregation of respondents
        output$ind_sex_disag_barplot <- renderPlot({
                
                # Create barplot for sex disaggregation of respondents
                ggplot(selected_demoDF(), aes(x = reorder(respSEX, as.numeric(gsub("[^0-9]", "", respSEX)), decreasing = T))) + 
                        geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(respSEX))) +
                        #geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, size = 5, family = "arial",  colour = "black") +
                        labs(title = "",
                             x = "",
                             y = "Percentage of Respondents") +
                        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
                        coord_flip() + 
                        scale_fill_viridis(discrete = T, option = "H") +
                        theme(legend.position = "none",
                              axis.text = element_text(size = 13.5, family="arial", face = "bold", colour = "black"),
                              axis.title.x = element_text(size = 16, family = "arial", face = "bold",  color = "black"),
                              axis.title.y = element_text(size = 16, family = "arial", face = "bold",  color = "black"))
                
        })
        
        
        ## barplot age disaggregation of respondents
        output$ind_age_disag_barplot <- renderPlot({
                
                
                # Create barplot for sex disaggregation of respondents
                ggplot(selected_demoDF(), aes(x = reorder(AgeGroup, as.numeric(gsub("[^0-9]", "", AgeGroup)), decreasing = T))) + 
                        geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(AgeGroup))) +
                        #geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, size = 5, family = "arial",  colour = "black") +
                        labs(title = "",
                             x = "Age Group",
                             y = "Percentage  of Respondents") +
                        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
                        coord_flip() + 
                        scale_fill_viridis(discrete = T, option = "H") +
                        theme(legend.position = "none",
                              axis.text = element_text(size = 13.5, family="arial", face = "bold", colour = "black"), 
                              axis.title.x = element_text(size = 16, family = "arial", face = "bold",  color = "black"),
                              axis.title.y = element_text(size = 16, family = "arial", face = "bold",  color = "black"))
                
                
                
        })
        
        
        ## barplot age disaggregation of respondents
        output$ind_work_barplot <- renderPlot({
                
                
                # Create barplot for sex disaggregation of respondents
                ggplot(selected_demoDF(), aes(x = reorder(respWORK, as.numeric(gsub("[^0-9]", "", respWORK)), decreasing = T))) + 
                        geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(respWORK))) +
                        #geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, size = 5, family = "arial",  colour = "black") +
                        labs(title = "",
                             x = "",
                             y = "Percentage") +
                        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
                        coord_flip() + 
                        scale_fill_viridis(discrete = T, option = "H") +
                        theme(legend.position = "none",
                              axis.text = element_text(size = 13.5, family="arial", face = "bold", colour = "black"), 
                              axis.title.x = element_text(size = 16, family = "arial", face = "bold",  color = "black"),
                              axis.title.y = element_text(size = 16, family = "arial", face = "bold",  color = "black"))
                
                
                
        })
        
        
        ## barplot age disaggregation of respondents
        output$ind_edu_barplot <- renderPlot({
                
                
                # Create barplot for sex disaggregation of respondents
                ggplot(selected_demoDF(), aes(x = reorder(respSCHOOL, as.numeric(gsub("[^0-9]", "", respSCHOOL)), decreasing = T))) + 
                        geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(respSCHOOL))) +
                        #geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, size = 5, family = "arial",  colour = "black") +
                        labs(title = "",
                             x = "Educational level achieved",
                             y = "Percentage") +
                        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
                        coord_flip() + 
                        scale_fill_viridis(discrete = T, option = "H") +
                        theme(legend.position = "none",
                              axis.text = element_text(size = 13.5, family="arial", face = "bold", colour = "black"), 
                              axis.title.x = element_text(size = 16, family = "arial", face = "bold",  color = "black"),
                              axis.title.y = element_text(size = 16, family = "arial", face = "bold",  color = "black"))
                
                
                
        })
        
        
        ## barplot age disaggregation of respondents
        output$ind_school_barplot <- renderPlot({
                
                
                # Create barplot for sex disaggregation of respondents
                ggplot(selected_demoDF(), aes(x = reorder(childSCHOOL, as.numeric(gsub("[^0-9]", "", childSCHOOL)), decreasing = T))) +
                        geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(childSCHOOL))) +
                        #geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, size = 5, family = "arial",  colour = "black") +
                        labs(title = "",
                             x = "",
                             y = "Percentage") +
                        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
                        coord_flip() + 
                        scale_fill_viridis(discrete = T, option = "H") +
                        theme(legend.position = "none",
                              axis.text = element_text(size = 13.5, family="arial", face = "bold", colour = "black"), 
                              axis.title.x = element_text(size = 16, family = "arial", face = "bold",  color = "black"),
                              axis.title.y = element_text(size = 16, family = "arial", face = "bold",  color = "black"))
                
                
                
        })
        
        
        ## barplot age disaggregation of respondents
        output$ind_disease_barplot <- renderPlot({
                
                
                # Create barplot for sex disaggregation of respondents
                ggplot(selected_demoDF(), aes(x = reorder(respHEALTH, as.numeric(gsub("[^0-9]", "", respHEALTH)), decreasing = T))) + 
                        geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(respHEALTH))) +
                        #geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, size = 5, family = "arial",  colour = "black") +
                        labs(title = "",
                             x = "Chronic disease",
                             y = "Percentage") +
                        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
                        coord_flip() + 
                        scale_fill_viridis(discrete = T, option = "H") +
                        theme(legend.position = "none",
                              axis.text = element_text(size = 13.5, family="arial", face = "bold", colour = "black"), 
                              axis.title.x = element_text(size = 14, family = "arial", face = "bold",  color = "black"),
                              axis.title.y = element_text(size = 16, family = "arial", face = "bold",  color = "black"))
                
                
                
        })
        
        
        #######################################################################################
        ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~   Tab 2  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  ##
        
        ######################################################################################
        
        ## filter other datasets based on selected households 
        ## Merge and simplified age disag
        selected_wgqDF <- reactive({
                filter(wgqDF, Parent.District %in% input$Location) %>%
                        filter(Displaced %in% input$Displacement) %>%
                        filter(respSEX %in% input$Sex) %>%
                        filter(respAGE >= input$Age[1] & respAGE <= input$Age[2])
        })
        
        
        
        
        
        ## Here two new reactive objects are needed to look at data/subset based on fully colinear variables
        # Reactive expression for unique groups
        fd <- reactive({
                unique(selected_wgqDF()$wgqFD)
        })
        
        ## Reactive expression for unique wgq subgroups based on selected wgqFD group
        wgq <- reactive({
                req(input$FD)
                selected_wgqDF() %>%
                        filter(wgqFD %in% input$FD) %>%
                        pull(wgqQUES) %>%
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
        d2_selected_wgqDF <- reactive({
                req(input$FD, input$WGQ1)
                selected_wgqDF() %>%
                        filter(wgqFD %in% input$FD, wgqQUES %in% input$WGQ1)
        })
        
        d3_selected_wgqDF <- reactive({
                req(input$FD)
                selected_wgqDF() %>%
                        filter(wgqFD %in% input$FD)
        })
        
        
        ## WGQ Barplot
        output$WGQ_barplot <- renderPlot({
                
                # Create barplot
                ggplot(d2_selected_wgqDF(), aes(x = reorder(wgqRESP, as.numeric(gsub("[^0-9]", "", wgqRESP)), decreasing = T))) + 
                        geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(wgqRESP))) +
                        #geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, size = 5, family = "arial",  colour = "black") +
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
                d3_selected_wgqDF() %>%
                        group_by(indID) %>%
                        summarize(
                                YesNo = ifelse(any(wgqRESP %in% c("3. A lot of difficulty", "4. Cannot do at all",
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
                        #geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, size = 5, family = "arial",  colour = "black") +
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
        
        
        
        
        
        
        
        #####################################################################################
        ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~   Tab 3  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  ##
        #####################################################################################
        
        
        
        
        ## filter other datasets based on selected households 
        ## Merge and simplified age disag
        selected_barriersDF <- reactive({
                filter(barriersDF, Parent.District %in% input$Location) %>%
                        filter(Displaced %in% input$Displacement) %>%
                        filter(respSEX %in% input$Sex) %>%
                        filter(respAGE >= input$Age[1] & respAGE <= input$Age[2])
        })
        
        
        ## Filtering of specific Barrier/Enabler
        ## Reactive expression for unique subgroups based on selected group
        haDF <- reactive({
                req(input$Humanitarian)
                selected_barriersDF() %>%
                        filter(barrierWHICH %in% input$Humanitarian)
        })
        
        ## Barrier and Enabler barplot
        ## the graph
        output$Barriers_where_barplot <- renderPlot({
                
                # Create barplot
                ggplot(haDF(), aes(x = reorder(barrierWHERE, as.numeric(gsub("[^0-9]", "", barrierWHERE)), decreasing = T))) + 
                        geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(barrierWHERE))) +
                        labs(title = paste("Barriers for ", input$Humanitarian),
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
                updateSelectizeInput(session, "Barriers", choices = sort(unique(haDF()$barrierWHERE)), selected = NULL, server = TRUE)
        })
        
        
        
        ## subset data based on choice of barrier
        haDF_where <- reactive({
                req(input$Barriers)
                haDF() %>%
                        filter(barrierWHERE %in% input$Barriers)
        })
        
        
        # Update Barriers input choices based on selected item in HA (i.e. category)
        observe({
                updateSelectizeInput(session, "Reasons", choices = sort(unique(haDF_where()$barrierWHY)), selected = NULL, server = TRUE)
        })
        
        
        ## reactive WGQ dataset changing based on selected haDF why
        wgq_yn_barriers_where <- reactive({
                
                wgqDF %>%
                        filter(indID %in% unique(haDF_where()[,"indID"]))
                
        }) 
        
        
        ## Yes/No disability status based on WGQ response
        respTYPE2 <- reactive({
                wgq_yn_barriers_where() %>%
                        group_by(indID) %>%
                        summarize(
                                YesNo = ifelse(any(wgqRESP %in% c("3. A lot of difficulty", "4. Cannot do at all",
                                                                  "2. A lot", "4. Most days", "5. Every day",
                                                                  "2. Most of the day")), "Yes", "No")
                        ) %>%
                        ungroup()
        })
        
        
        ## bring info on disability status into main DF
        # Add the YesNo column to haDF_which()
        disa_haDF_where <- reactive({
                haDF_where() %>%
                        left_join(respTYPE2(), by = "indID")
        })
        
        
        
        ## first plot is per barrier type (WHICH)
        output$Barriers_where_disa_barplot <- renderPlot({
                
                # Create barplot
                ggplot(disa_haDF_where(), aes(x = YesNo)) +
                        geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(YesNo))) +
                        #geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, size = 5, family = "arial",  colour = "black") +
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
        
        
        ## Second plot: reasons given why
        output$Barriers_why_barplot <- renderPlot({
                
                
                # Create barplot
                ggplot(haDF_where(), aes(x = reorder(barrierWHY, as.numeric(gsub("[^0-9]", "", barrierWHY)), decreasing = T))) + 
                        geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(barrierWHY))) +
                        #geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, size = 5, family = "arial",  colour = "black") +
                        labs(title = paste("Which Reasons for ", unique(haDF_where()$Barriers)),
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
        
        # ## subset data based on select Reason
        # haDF_why <- reactive({
        #   haDF_which() %>%
        #     filter(barrierWHY %in% input$Reasons)
        # })
        
        
        
        ## subset data based on select Reason
        haDF_why <- reactive({
                haDF_where() %>%
                        filter(barrierWHY %in% input$Reasons)
        })
        
        
        ## reactive WGQ dataset changing based on selected haDF why
        wgq_yn_barriers_why <- reactive({
                
                wgqDF %>%
                        filter(indID %in% unique(haDF_why()[,"indID"]))
                
        }) 
        
        
        ## Yes/No disability status based on WGQ response
        respTYPE3 <- reactive({
                wgq_yn_barriers_why() %>%
                        group_by(indID) %>%
                        summarize(
                                YesNo = ifelse(any(wgqRESP %in% c("3. A lot of difficulty", "4. Cannot do at all",
                                                                  "2. A lot", "4. Most days", "5. Every day",
                                                                  "2. Most of the day")), "Yes", "No")
                        ) %>%
                        ungroup()
        })
        
        
        ## bring info on disability status into main DF
        # Add the YesNo column to haDF_why()
        disa_haDF_why <- reactive({
                haDF_why() %>%
                        left_join(respTYPE3(), by = "indID")
        })
        
        
        
        
        ## Compare disability vs no disability for distinct barriers and enablers
        output$Barriers_why_disa_barplot <- renderPlot({
                
                # Create barplot
                ggplot(disa_haDF_why(), aes(x = YesNo)) +
                        geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(YesNo))) +
                        #geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, size = 5, family = "arial",  colour = "black") +
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
        
        
        
        ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Tab 4  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  ##
        
        ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  downloads enablers as per
        
        
        
        ## filter other datasets based on selected households 
        ## Merge and simplified age disag
        selected_enablersDF <- reactive({
                filter(enablersDF, Parent.District %in% input$Location) %>%
                        filter(Displaced %in% input$Displacement) %>%
                        filter(respSEX %in% input$Sex) %>%
                        filter(respAGE >= input$Age[1] & respAGE <= input$Age[2])
        })
        
        
        
        ## reactive WGQ dataset changing based on selected haDF why
        wgq_yn_enablers <- reactive({
                
                wgqDF %>%
                        filter(indID %in% unique(selected_enablersDF()[,"indID"]))
                
        }) 
        
        
        ## Yes/No disability status based on WGQ response
        respTYPE4 <- reactive({
                wgq_yn_enablers() %>%
                        group_by(indID) %>%
                        summarize(
                                Disability = ifelse(any(wgqRESP %in% c("3. A lot of difficulty", "4. Cannot do at all",
                                                                       "2. A lot", "4. Most days", "5. Every day",
                                                                       "2. Most of the day")), "Yes", "No")
                        ) %>%
                        ungroup()
        })
        
        
        ## bring info on disability status into main DF
        # Add the YesNo column to haDF_which()
        disa_selected_enablersDF <- reactive({
                selected_enablersDF() %>%
                        left_join(respTYPE4(), by = "indID")
        })
        
        
        
        
        output$enablerTABLE <- renderDT({
                datatable(
                        disa_selected_enablersDF()[, (ncol(disa_selected_enablersDF())-5):ncol(disa_selected_enablersDF())],
                        extensions = "Buttons",
                        options = list(
                                dom = "Bfrtip",
                                pageLength = 50,
                                buttons = list(
                                        list(
                                                extend = "csv",
                                                exportOptions = list(
                                                        modifier = list(
                                                                search = "applied"
                                                        )
                                                )
                                        )
                                )
                        ),
                        filter = "top"
                )
        }, server = TRUE)
        
        
}


# Run the application
shinyApp(ui = ui, server = server) ## options argument ensure shiny app height improved


