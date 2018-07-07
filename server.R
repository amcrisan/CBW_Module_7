library(shiny)
library(magrittr)
library(dplyr)
library(treeio)
library(ggplot2)
library(ggtree)
library(ape)
library(DT)
library(RColorBrewer)


# A small note. When something is written as follows 'treeio::read.tree' it tells are to use the 'read.tree' function from the 'treeio' package. It is not necessary to do this in your own, R is clever enough to figure it out. BUT, for education and learning purposes, I have done that is this code so it is clear how the different packages are used in different parts of the analysis. When it becomes cumbersome to write :: then I will simply state the package that I've used

metadata<-read.csv(file="data/kleb-pneumo-meta-data.csv",na.strings = c("","\\s+",NA)) #loads csv
tree<-treeio::read.tree(file="data/kleb-pneumo-meta-tree.nwk") #reads a newick tree

# it helps if the metadata levels equal the tree ordering levels
# the tree$tip.label return the tree label from BOTTOM to TOP
metadata$id<-factor(metadata$id,levels=rev(tree$tip.label))

#filter any data points without lat,long co-ordinates.
# %<>% is a special type of pipe
metadata %<>% filter(!is.na(lat) & !is.na(long))

#Lastly, set up the base map
#some data pre-processing: find what points are right on top of eachother
#and make those just one marker
mapData<-metadata %>%
  group_by(Location,lat,long)%>%
  count()


#now draw the map
baseMap<-leaflet(mapData) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMinicharts(
    mapData$long, 
    mapData$lat,
    type = "pie",
    layerId = mapData$Location,
    opacity = 0.8,
    width = 60 * sqrt(mapData$n) / sqrt(max(mapData$n)), 
    height = 45
  )


#-------------- SHINY SERVER CODE --------------

shinyServer(function(input, output, session) {
  session$onSessionEnded(stopApp) #piece of code that  stops app when browser is closed
  
  #----------------- REACTIVE ELEMENTS --------------------
  #reactive values, which store useful data that can be used my variables
  values<-reactiveValues(
    mapData = mapData,
    colorPal = NULL #a unified color pallette
  )
  
  #----------------- DATA MANIPULATIONS -------------------
  #setting the colour pallett by the metadata variable
  observe({
    if(!is.null(input$mapGlyph)){
      levelItems<-levels(metadata[,input$mapGlyph])
      values$colorPal<-brewer.pal(n=length(levelItems),"Set3")
    }
  })
  
  #----------------- WIDGETS ------------------------------
  
  output$metadataColorOpts<-renderUI({
    
    #All users to overlay metadata with any variables that have 12
    #categories or fewer
    colClass<-sapply(metadata,function(x){
        classInfo<-class(x)
        levelNum= 0
        if(classInfo=="factor"){
          levelNum<-length(levels(x))
        }
        c(classInfo,levelNum)
    }) %>%t()
    
    #make a clean dataframe
    colClass<-data.frame(item = rownames(colClass),
                         varClass = colClass[,1],
                         levelNum = as.numeric(colClass[,2]))
    
    #final filter out factors with colClass greater than 0, but 12 or less
    #12 is the magic number that is reasonable to support with a colour pallet
    
    colClass %<>% filter(varClass == "factor" & (levelNum >=0 & levelNum<=12))
    
    #now make a dropdown list to choose items
    selectizeInput(inputId = "mapGlyph",
                   label = "Overlay the following metadata",
                   choices =  as.character(colClass$item),
                   selected = as.character(colClass$item)[1]
    )

  })
  
  #----------------- VISUALIZATIONS -----------------------
  #TABLE OF DATA
  output$epiDataTab<-DT::renderDataTable({
    metadata
  },
  extensions = c('Responsive','FixedHeader'),
  options = list(
    autoWidth = TRUE,
    fixedHeader=TRUE,
    scrollX = TRUE))

  #PHYLOGENETIC TREE
  #Note, the changed syntax output$phyloTree_allTogether<-output$phyloTree 
  #is unique to this shiny application. The normal and most common thing
  #to do is just output$phyloTree <-renderPlot({...}). The reason this
  #chaining is needed here is because I have used the SAME PLOT
  #on multiple tab panels. There is a good discussion about why
  #this is a good solution here: https://github.com/rstudio/shiny/issues/867
  output$phyloTree_allTogether<-output$phyloTree<-renderPlot({
    if(is.null(input$mapGlyph)){
      return(NULL)
    }
    
    tree<-ggtree::ggtree(tree,layout=input$treeLayout)+geom_treescale()
    
    if(!is.null(input$mapGlyph)){
      #a hack that is needed specifically for geom_tippoint
      metadata$colorBy<-metadata[,c(input$mapGlyph)]
        
      tree<-tree%<+%metadata + 
        geom_tippoint(aes(color=colorBy),alpha=0.25)+
        scale_colour_manual(values=values$colorPal)
      
    }
    
    tree
  })
  

  #GEOGRAPHIC MAP
  output$map_allTogether<-output$map<-renderLeaflet({
    baseMap
  })
    
    #HELPER METHOD FOR GEOGRAPHIC MAP
  
    observeEvent({c(input$mapGlyph,
      input$itemID=="Map")},{
        
                if(is.null(input$mapGlyph)){
                  return(NULL)
                }
                 tmp<-values$mapData #getting that map data we saved earlier
                 
                 tmp2<-metadata %>%
                   group_by_("Location",input$mapGlyph) %>%
                   count() %>%
                   tidyr::spread_(input$mapGlyph,"n",fill=0)%>%
                   ungroup()%>%
                   select(-Location)
                 
                 if(!is.null(tmp)){
                   #update the individual map view
                   leafletProxy("map", session) %>%
                     updateMinicharts(
                       tmp$Location,
                       maxValues = max(as.matrix(tmp2)),
                       chartdata = tmp2,
                       colorPalette= values$colorPal,
                       type = ifelse(ncol(tmp2) < 2, "polar-area", "pie")
                     )
                 }
        })
    
    observeEvent({c(input$mapGlyph,
      input$itemID=="Everything")},{
        if(is.null(input$mapGlyph)){
          return(NULL)
        }
        
      tmp<-values$mapData #getting that map data we saved earlier
      
      tmp2<-metadata %>%
        group_by_("Location",input$mapGlyph) %>%
        count() %>%
        tidyr::spread_(input$mapGlyph,"n",fill=0)%>%
        ungroup()%>%
        select(-Location)
      
      if(!is.null(tmp)){
        #update the individual map view
        leafletProxy("map_allTogether", session) %>%
          updateMinicharts(
            tmp$Location,
            maxValues = max(as.matrix(tmp2)),
            chartdata = tmp2,
            colorPalette= values$colorPal,
            type = ifelse(ncol(tmp2) < 2, "polar-area", "pie")
          )
      }
    })

    #TIMELINE EXAMPLE 1
    output$timeline1_allTogether<-output$timeline1<-renderPlot({
      if(is.null(input$mapGlyph)){
        return(NULL)
      }
      
      tmp<-metadata%>%
        group_by(Year_isolated)%>%
        count()
      
      ggplot(metadata, aes(x = Year_isolated)) + 
        geom_dotplot(aes_string(fill=input$mapGlyph),colour="black",binwidth=1)+
        ylim(c(0,max(tmp$n)))+
        scale_fill_manual(values=values$colorPal)+
        theme_bw()
    })
    
    #TIMELINE EXAMPLE 2
    output$timeline2_allTogether<-output$timeline2<-renderPlot({
      if(is.null(input$mapGlyph)){
        return(NULL)
      }
      
      ggplot(metadata, aes(x = Year_isolated)) + 
        geom_histogram(aes_string(fill = input$mapGlyph),colour="black")+
        scale_fill_manual(values=values$colorPal)+
        theme_bw()
    })

})


