#----------------------------------
# This file controls the visual appearance of the dashboard. That is how information is laid out, what charts are shown, and also how a user will interact with the interface
#----------------------------------
library(DT)
library(shiny)
library(leaflet)
library(leaflet.minicharts) #makes pie charts and other little things
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(phylocanvas)



#------ TITLE AND OTHER INFORMATION -------
header <- dashboardHeader(
  title = "Outbreak Explorer",
  titleWidth = 450
)


# ----- MAJORITY OF THE CONTENT GOES HERE ------
body<-dashboardBody(
  #----- BELOW IS SOME CUSTOM CODE THAT CHANGES THE APPEARANCE AND FUNCTIONALITY OF THE SHINY APP -----
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "customStyle.css")
  ),
  #----- ABOVE IS SOME CUSTOM CODE THAT CHANGES THE APPEARANCE AND FUNCTIONALITY OF THE SHINY APP -----
  #The different visualizations are put within each of these tabs
  tabItems(
    #-------- PHYLOGENETIC TREE --------
    tabItem("Tree",
            column(4,
            radioGroupButtons("treeLayout",
                              label = "Tree Layout",
                              choiceNames = c("Rectangular","Circular","Unrooted"),
                              choiceValues = c("rectangular","circular","equal_angle"),
                              selected = "rectangular",
                              individual = TRUE,
                              checkIcon = list(
                                yes = tags$i(class = "fa fa-circle", 
                                             style = "color: steelblue"),
                                no = tags$i(class = "fa fa-circle-o", 
                                            style = "color: steelblue"))),
            sliderInput("nodeSize",
                        "Node Size",
                        min = 1, 
                        max = 10,
                        step=1,
                        value = 1),
            sliderInput("nodeTransparency",
                        "Node transparency",
                        min = 0.1,
                        max=1,
                        step = 0.05,
                        value=0.25),
            checkboxInput("showLabel","Show Node Labels",value=FALSE),
            checkboxInput("treeMetadataTable","Add Metadata table to tree",value=FALSE),
            uiOutput("treeDataOverlay")
            ),
            column(8,
            p("Zoom in by brushing (dragging mouse to form a square) and then double clicking. Double click again to reset the view"),
            br(),
              withSpinner(plotOutput("phyloTree",
                                     height = "750px",
                                     dblclick = "phyloTree_dblclick",
                                     brush = brushOpts(
                                       id = "phyloTree_brush",
                                       resetOnNew = TRUE
                                     )))
            )
    ),
    #-------- GEOGRAPHIC MAP --------
    tabItem("Map",
            leafletOutput("map")
    ),
    #-------- TIMELINE --------
    tabItem("Timeline",
            plotOutput("timeline2",height="500px")),
    #-------- DATA TABLE--------
    tabItem("DataTable",
            dataTableOutput("epiDataTab",width="90%")),
    #-------- ALL TOGETHER  --------
    # BONUS! We can re-use a lot of the code we already came up with
    tabItem("Everything",
          fluidRow(
              column(6,
                     leafletOutput("map_allTogether")),
              column(6,
                     plotOutput("phyloTree_allTogether"))
            ),
            plotOutput("timeline2_allTogether")
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
  uiOutput("metadataColorOpts")
)

#-------- ! PUTTING IT ALL TOGETHER --------
dashboardPage(
  header,
  sidebar,
  body,
  skin="black"
)