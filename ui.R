#----------------------------------
# This file controls the visual appearance of the dashboard. That is how information is laid out, what charts are shown, and also how a user will interact with the interface
#----------------------------------
library(DT)
library(shiny)
library(leaflet)
library(leaflet.minicharts) #makes pie charts and other little things
library(shinydashboard)
library(shinycssloaders)



#------ TITLE AND OTHER INFORMATION -------
header <- dashboardHeader(
  title = "Outbreak Explorer",
  titleWidth = 450
)


# ----- MAJORITY OF THE CONTENT GOES HERE ------
body<-dashboardBody(
  #The code below loads a custom css that changes 
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "customStyle.css")
  ),
  #The different visualizations are put within each of these tabs
  tabItems(
    #-------- PHYLOGENETIC TREE --------
    tabItem("Tree",
            column(7,
              h3("Phylogenetic Tree"),
              radioGroupButtons("treeLayout",
                                label = "Tree Layout",
                                choiceNames = c("Rectangular","Circular","Unrooted"),
                                choiceValues = c("rectangular","circular","equal_angle"),
                                selected = "rectangular"),
              withSpinner(plotOutput("phyloTree"))
            ),
            column(5,
                   h3("Tree Metadata"))
    ),
    #-------- GEOGRAPHIC MAP --------
    tabItem("Map",
            leafletOutput("map")
    ),
    #-------- TIMELINE --------
    tabItem("Timeline",
            plotOutput("timeline1",height="500px")),
    #-------- DATA TABLE--------
    tabItem("DataTable",
            dataTableOutput("epiDataTab",width="90%")),
    #-------- ALL TOGETHER  --------
    # BONUS! We can re-use a lot of the code we already came up with
    tabItem("Everything",
            p("EVERYTHINGs"),
    
            fluidRow(
              column(6,
                     leafletOutput("map_allTogether")),
              column(6,
                     plotOutput("phyloTree_allTogether"))
            ),
            plotOutput("timeline1_allTogether")
      )
  )
)

#------ SIDEBAR CONTENT ----
sidebar<-dashboardSidebar(
  sidebarMenu(id = "itemID",
    menuItem("Tree", tabName = "Tree",icon = icon("tree")),
    menuItem("Map", tabName = "Map",icon = icon("map")),
    menuItem("Timeline",tabName = "Timeline",icon = icon("calendar")),
    menuItem("DataTable",tabName = "DataTable",icon = icon("table")),
    menuItem("Everything",tabName = "Everything",icon = icon("diagnoses"),selected = TRUE)
  ),
  
  h3("Overlaying Metadata"),
  # selectizeInput(inputId = "mapGlyph",
  #                label = "Overlay the following metadata",
  #                choices =  c("Country_isolated",
  #                             "Phylogroup",
  #                             "Source_Host"
  #                )
  # )
  uiOutput("metadataColorOpts")
)

#-------- ! PUTTING IT ALL TOGETHER --------
dashboardPage(
  header,
  sidebar,
  body,
  skin="black"
)