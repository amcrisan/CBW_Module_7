library(shiny)
library(magrittr)
library(dplyr)
library(treeio)
library(ggplot2)
library(ggtree)
library(ape)
library(DT)
library(RColorBrewer)
library(cowplot)


# A small note. When something is written as follows 'treeio::read.tree' it tells are to use the 'read.tree' function from the 'treeio' package. It is not necessary to do this in your own, R is clever enough to figure it out. BUT, for education and learning purposes, I have done that is this code so it is clear how the different packages are used in different parts of the analysis. When it becomes cumbersome to write :: then I will simply state the package that I've used

metadata<-read.csv(file="data/kleb-pneumo-meta-data.csv",na.strings = c("","\\s+","NA")) #loads csv
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
  #reactive metadata table so that it can be easier to filter data
  #right now, there haven't been any filters set up,
  #but it's ready!
  metaTab<-reactive({
    metadata
  })
  
  #reactive values, which store useful data that can be used my variables
  values<-reactiveValues(
    tree=NULL,
    mapData = mapData,
    metaTreeTable = NULL,
    colorPal = NULL, #a unified color pallette
    tree_zoom_x = NULL,
    tree_zoom_y = NULL
  )
  
  #----------------- DATA MANIPULATIONS -------------------
  #updating the tree everytime a different layout is triggered
  # observeEvent(input$treeLayout,{
  #   values$tree<-ggtree::ggtree(tree,layout=input$treeLayout)
  # })
  
  #setting the colour pallett by the metadata variable
  observe({
    if(!is.null(input$metadataOverlayCol)){
      levelItems<-levels(metadata[,input$metadataOverlayCol])
      values$colorPal<-brewer.pal(n=length(levelItems),"Set3")
    }
  })
  
  #check methods to support zooming in on the tree
  #just copied from here: https://shiny.rstudio.com/gallery/plot-interaction-zoom.html
  observeEvent(input$phyloTree_dblclick, {
    brush <- input$phyloTree_brush
    if (!is.null(brush)) {
      values$tree_zoom_x <- c(brush$xmin, brush$xmax)
      values$tree_zoom_y <- c(brush$ymin, brush$ymax)
      
    } else {
      values$tree_zoom_x <- NULL
      values$tree_zoom_y <- NULL
    }
  })
  
  #----------------- WIDGETS ------------------------------
  
  #Widget : dropdown box where user can use what metadata to overlay onto charts
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
    
    #going to save a variable here for the tree metadata
    #so, limit to factors, but can have MORE than 12 options 
    #can actually expand the design so that it's also possible
    #do continous var. But, that's a bonus!
    values$metaTreeTable<-filter(colClass,varClass=="factor" & levelNum<nrow(metadata))
    
    #final filter out factors with colClass greater than 0, but 12 or less
    #12 is the magic number that is reasonable to support with a colour pallet
    colClass %<>% filter(varClass == "factor" & (levelNum >=0 & levelNum<=12))
    
    #now make a dropdown list to choose items
    selectizeInput(inputId = "metadataOverlayCol",
                   label = "Overlay the following metadata",
                   choices =  as.character(colClass$item),
                   selected = as.character(colClass$item)[1]
    )

  })
  
  #Widget Show all tree metadata
  output$treeDataOverlay<-renderUI({
    if(input$treeMetadataTable){
      if(input$treeLayout == "rectangular"){
      selectizeInput("treeData",
                     "Overlay Tree Data",
                     choices = values$metaTreeTable$item,
                     selected=NULL,
                     multiple=TRUE)
      }else{
        HTML("<b>METADATA CAN CURRENTLY ONLY BE SHOWN WITH RECTANGULAR LAYOUT")
      }
    }else{
      NULL
    }
  })
  

  
  
  #----------------- VISUALIZATIONS -----------------------
  #Visualization : TABLE OF DATA
  output$epiDataTab<-DT::renderDataTable({
    metadata
  },
  extensions = c('Responsive','FixedHeader'),
  options = list(
    autoWidth = TRUE,
    fixedHeader=TRUE,
    scrollX = TRUE))

  #Visualization : PHYLOGENETIC TREE
  #Note, the changed syntax output$phyloTree_allTogether<-output$phyloTree 
  #is unique to this shiny application. The normal and most common thing
  #to do is just output$phyloTree <-renderPlot({...}). The reason this
  #chaining is needed here is because I have used the SAME PLOT
  #on multiple tab panels. There is a good discussion about why
  #this is a good solution here: https://github.com/rstudio/shiny/issues/867
  output$phyloTree_allTogether<-output$phyloTree<-renderPlot({
    if(is.null(input$metadataOverlayCol)){
      return(NULL)
    }
    
     tree<-ggtree::ggtree(tree,layout=input$treeLayout)+
       geom_treescale() #needed so that things don't overlap
    #tree<-values$tree + geom_treescale()
    
    if(!is.null(input$metadataOverlayCol)){
      #a hack that is needed specifically for geom_tippoint
      tmp<-metaTab()
      tmp$colorBy<-tmp[,c(input$metadataOverlayCol)]
        
      tree<-tree%<+%tmp + 
        geom_tippoint(aes(color=colorBy),alpha=input$nodeTransparency,size = input$nodeSize)+
        scale_colour_manual(values=values$colorPal,name = "")
      
      #add node labels
      if(input$showLabel){
        tree<-tree + geom_tiplab()
      }
      
      #add relevant metadata
      if(!is.null(input$treeData)){
        pList<-c()
        pList[['tree']]<-tree
        
        for(var in input$treeData){
          pList[[var]]<-ggplot(tree$data,aes_string(x = var, y = "y")) + 
            geom_point()+
            scale_x_discrete(na.translate=FALSE)+
            scale_y_continuous(breaks = sort(tree$data$y),
                               labels = levels(tree$data$id))+
            theme_bw()+
            theme(axis.text.y = element_blank(),
                  axis.title.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  plot.margin = unit(c(0,0,0,0),"points"),
                  panel.grid.minor = element_blank(),
                  panel.grid.major.y = element_blank(),
                  axis.text.x = element_text(angle=90))
        }
        
        
        tree<-cowplot::plot_grid(plotlist=pList, nrow=1,align="h")
      }
      
      #lastly, support zooming in on tree nodes
      if(!is.null(values$tree_zoom_x) & !is.null(values$tree_zoom_y)){
        tree<-tree + 
          coord_cartesian(xlim = values$tree_zoom_x, ylim = values$tree_zoom_y, expand = FALSE)
      }
    }
    
    tree
  })
  
  #Visualization : Alternative tree made with phylocanvas
  output$phyloTree_alt<-renderPhylocanvas({
    treeLayout <- ifelse(input$treeLayout=="equal angle","radial",input$treeLayout)
    phylocanvas(tree,treetype = treeLayout, alignlabels = T,height = '750px')
  })
  

  #Visualization : GEOGRAPHIC MAP
  output$map_allTogether<-output$map<-renderLeaflet({
    baseMap
  })
    
    #HELPER METHOD FOR GEOGRAPHIC MAP
    observeEvent({c(input$metadataOverlayCol,
      input$itemID=="Map")},{
        
                if(is.null(input$metadataOverlayCol)){
                  return(NULL)
                }
                 tmp<-values$mapData #getting that map data we saved earlier
                 
                 tmp2<-metaTab() %>%
                   group_by_("Location",input$metadataOverlayCol) %>%
                   count() %>%
                   tidyr::spread_(input$metadataOverlayCol,"n",fill=0)%>%
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
    
    observeEvent({c(input$metadataOverlayCol,
      input$itemID=="Everything")},{
        if(is.null(input$metadataOverlayCol)){
          return(NULL)
        }
        
      tmp<-values$mapData #getting that map data we saved earlier
      
      tmp2<-metaTab() %>%
        group_by_("Location",input$metadataOverlayCol) %>%
        count() %>%
        tidyr::spread_(input$metadataOverlayCol,"n",fill=0)%>%
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

    #Visualization : TIMELINE EXAMPLE 1
    output$timeline1_allTogether<-output$timeline1<-renderPlot({
      if(is.null(input$metadataOverlayCol)){
        return(NULL)
      }
      
      tmp<-metadata%>%
        group_by(Year_isolated)%>%
        count()
      
      ggplot(metaTab(), aes(x = Year_isolated)) + 
        geom_dotplot(aes_string(fill=input$metadataOverlayCol),colour="black",binwidth=1)+
        ylim(c(0,max(tmp$n)))+
        scale_fill_manual(values=values$colorPal)+
        theme_bw()
    })
    
    #Visualization : TIMELINE EXAMPLE 2
    output$timeline2_allTogether<-output$timeline2<-renderPlot({
      if(is.null(input$metadataOverlayCol)){
        return(NULL)
      }
      
      ggplot(metadata, aes(x = Year_isolated)) + 
        geom_histogram(aes_string(fill = input$metadataOverlayCol),colour="black")+
        scale_fill_manual(values=values$colorPal)+
        theme_bw()
    })

})


