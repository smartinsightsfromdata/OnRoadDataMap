library(shiny)

AllFiles <- list.files(pattern='AllData.csv')

shinyUI(pageWithSidebar(
  # Application title
  headerPanel("On-Road Data Map -Alex Bigazzi"),

  # Sidebar with controls to select the compound
  sidebarPanel(
    selectInput("dataset", "Dataset:", AllFiles),
    uiOutput("field1"),
    uiOutput("slider1"),
    uiOutput("field2"),
    uiOutput("slider2"),
    uiOutput("timeSlider"),
    sliderInput("SampleRate","Sample rate (point every x seconds):",min=1, max=120, step=1,animate=F, value=20, format="#", locale="us"),
    downloadButton('downloadData', 'Download data subset')
  ),

  mainPanel(
    mapOutput("map")               
  )
))
