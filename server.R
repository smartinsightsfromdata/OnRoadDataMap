################################################################################
# Display OnRoad Data Map with the Shiny app
# Alex Bigazzi, abigazzi@pdx.edu
# v1: Initial version: 10/31/2013 (branched from "OnBikeExposure_DayData")
################################################################################
library(shiny)
require(rCharts)  # This should be the development version modified to correct a lat/long resolution issue.
  # Install with devtools package using "> install_github('rCharts', 'ramnathv', ref = 'dev')"
  
shinyServer(function(input, output, session) {

##################################################################
# Reactive functions (update with new input)
#===================================================  
  loadData <- reactive({
    AllData <- read.csv(input$dataset)
    AllData$Time <- as.POSIXct(AllData$Time)
    AllData
  })

  getSubset <- reactive({
    AllData <- loadData()
    Fields <- c("Time",'lat_MyTracks','lon_MyTracks',input$field1,input$field2)
    Fields <- Fields[Fields!="None"]
    AllData <- AllData[AllData$Time>=getTimeRange()[1] & AllData$Time<=getTimeRange()[2],Fields]
    AllData$Time <- format(AllData$Time)
    AllData
  })
  
  getPlotField1 <- reactive({
    Test <- input$field1
    if(length(Test)==0) return(NULL)
    if(Test!="None") return(Test) else return(NULL)
  })
  getPlotField2 <- reactive({
    Test <- input$field2
    if(length(Test)==0) return(NULL)
    if(Test!="None") return(Test) else return(NULL)
  })

  getTimeRange <- reactive({
     AllData <- loadData()
     round(AllData$Time[1], "days") + input$timeRange*3600
  })
  
##################################################################
# Titles and Dynamic UI
#===================================================
  # Dynamically determine the fields available to plot
  output$field1 <- renderUI({
    AllData <- loadData()
    selectInput("field1", "Color Field:", c("None",names(AllData)))
  })
  output$field2 <- renderUI({
    AllData <- loadData()
    selectInput("field2", "Size Field:", c("None",names(AllData)))
  })

  # Dynamically determine the slider bar for the graphing scales
  output$slider1 <- renderUI({
    if(is.null(getPlotField1())) return ()
    AllData <- loadData()
    DataRange <- range(AllData[,input$field1], na.rm=T)
    DataRange <- round(DataRange,1)
    Limits <- c(DataRange[1] - diff(DataRange)*.6, DataRange[2] + diff(DataRange)*.6)
    Limits <- round(Limits, 1)
    sliderInput("range1", "Range for color field:", min = Limits[1], max = Limits[2], value=DataRange, format="#,##0.#")    
  }) 
  output$slider2 <- renderUI({
    if(is.null(getPlotField2())) return ()
    AllData <- loadData()
    DataRange <- range(AllData[,input$field2], na.rm=T)
    DataRange <- round(DataRange,1)
    Limits <- c(DataRange[1] - diff(DataRange)*.6, DataRange[2] + diff(DataRange)*.6)
    Limits <- round(Limits, 1)
    sliderInput("range2", "Range for size field:", min = Limits[1], max = Limits[2], value=DataRange, format="#,##0.#")    
  }) 

  # Slider to determine plotting time range
  output$timeSlider <- renderUI({
    AllData <- loadData()
    DataRange <- as.numeric(format(range(AllData$Time, na.rm=T), "%H")) + as.numeric(format(range(AllData$Time, na.rm=T), "%M"))/60
    sliderInput("timeRange", "Display Time Range (hours of the day):", DataRange[1], DataRange[2], value=DataRange, format="#0.##")    
  })    
  
##################################################################
# Output plots and tables
#===================================================
 output$map <- renderMap({
   map <- Leaflet$new()
   map$setView(c(45.51, -122.63), zoom=14)
   map$set(height=700)
   map$tileLayer(provider = 'Stamen.Terrain')    #'OpenStreetMap'
   map$enablePopover(TRUE)
   mapData <- getMapData()
   if(!is.null(mapData)){
     map$geoJson(toGeoJSON(mapData), 
        onEachFeature = '#! function(feature, layer){layer.bindPopup(feature.properties.popup)} !#',
        pointToLayer =  "#! function(feature, latlng){
          return L.circleMarker(latlng, {
               radius: feature.properties.radius || 6,
               fillColor: feature.properties.fillColor || 'red',    
               color: '#000',
               weight: 1.2,
               fillOpacity: 0.7 
          }) } !#")
     if(!is.null(getPlotField1()))map$legend(position="topright", colors=Colors, labels=c(levels(cut(0:2,breaks=input$range1[1] + diff(input$range1)/5 * 0:5)),'NA'))
   }
   map
 })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste(input$dataset, '_export.csv', sep='') },
    content = function(file) {
      write.csv(getSubset(), file)
  })
  
###################
# Leaflet
#==================
  Colors <- c(colorRampPalette(c('red','orange3','green3'))(5),'#000')

  getMapData <- reactive({
    PlotData <- getSubset()
      if(is.null(PlotData)) return(NULL)  # Don't plot points if no data
    PlotField1 <- getPlotField1()
      PlotField2 <- getPlotField2()
    
    # Set point colors
    if(!is.null(PlotField1) & all(is.numeric(input$range1))) {     
      if(is.numeric(PlotData[,PlotField1])) {
        Cat <- cut(PlotData[,PlotField1], breaks=input$range1[1] + diff(input$range1)/5 * 0:5)
        Cat[is.na(Cat)] <- 6
      } else Cat <- rep(6,nrow(PlotData))
    } else {    
       Cat <- rep(6,nrow(PlotData))
    }
    
    # Set Point Sizes
    sizeRange <- c(2,12)
    if(!is.null(PlotField2) & all(is.numeric(input$range2))) {     
      if(is.numeric(PlotData[,PlotField2])) {
        Size <- (PlotData[,PlotField2] - input$range2[1])/diff(input$range2)*diff(sizeRange) + sizeRange[1]
        Size[is.na(Size)] <- 6
      } else Size <- rep(6,nrow(PlotData))
    } else {    
      Size <- rep(6,nrow(PlotData))
    }    
    
    MapPoints <- list()
    SampleRate <- input$SampleRate 
     
    for(r in 1:floor(nrow(PlotData)/SampleRate)*SampleRate) {  # Add data points to map  
      if(any(sapply(PlotData[r,c('lat_MyTracks','lon_MyTracks')],is.na))) next
      MapPoints <- c(MapPoints,list(list(
        latitude=PlotData$lat_MyTracks[r],
        longitude=PlotData$lon_MyTracks[r],
        fillColor=Colors[Cat[r]],
        radius=Size[r],
        popup=paste('<p>',
          PlotData$Time[r],'<br>',
          PlotField1,':',if(!is.null(PlotField1))format(PlotData[r,PlotField1])else"NA",'<br>',
          PlotField2,':',if(!is.null(PlotField2))format(PlotData[r,PlotField2])else"NA",                    
          '</p>'
          )
      )))
    }
    MapPoints
  })
})