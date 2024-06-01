#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# required libraries
library(RCurl)
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(leaflet)
library(plotly)

##### FUNCTIONS #####
# adjusted doy function
adj_date = function(x, start.month = 10L, start.dayMonth = 1L){
  start.doy = lubridate::yday(lubridate::make_date(2001, start.month, start.dayMonth))
  start.yr = lubridate::year(x) - ( lubridate::yday(x) < start.doy)
  start.date = lubridate::make_date(start.yr, start.month, start.dayMonth)
  adj.doy = as.integer(x - start.date + 1L)
  # Year offset
  offset = ifelse(lubridate::yday(x) >= start.doy, 1, 0)
  # Water year
  adj.year = lubridate::year(x) + offset
  df<-data.frame(x,adj.doy,adj.year)
  colnames(df) <- c("date","adjDOY","adjYear")
  return(df)
}
######

##### INITIAL VALS ----
# initial point for marker
latIn<-36
lonIn<--109
#####
# https://stackoverflow.com/questions/53370679/r-leaflet-how-to-obtain-lat-long-where-marker-has-been-dragged-to





# Define UI for application that draws a histogram
ui <- fluidPage(

  # css tags for progress bar
  tags$head(tags$style(".shiny-progress .progress-text {
                          position: absolute;
                          right: 10px;
                          height: 30px;
                          width: 300px;
                          background-color: #FF6633;
                          margin: 0px;
                          padding: 2px 3px;
                          opacity: 0.85;
                          font-size: 18px;
                        }")),
  
    # Application title
    titlePanel("Growing Degree Day Plotter"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            leafletOutput("MyMap",width = "100%", height = "250px"),
            verbatimTextOutput("latSel"),
            verbatimTextOutput("lonSel"),
            actionButton("refresh","Set location"),
            hr(),
            sliderInput("baseT",
                        "GDD Base Temp(F)",
                        min = 32,
                        max = 50,
                        value = 50),
            # Default value is the date in client's time zone
            dateInput("dateInput", "Date:",value = as.Date(paste0(format(Sys.Date(),"%Y"),"-01-01")), max = Sys.Date()-1, min="1981-01-01"),
            numericInput("gddTarget", "GDD Target", value = 1000)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           #plotOutput("distPlot")
           plotlyOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # ##### Initial download
  # ##### download and process data for location ---- 
  # start.time <- Sys.time()
  # # set location 
  # lat=latIn
  # lon=lonIn 
  # 
  # #download daily PRISM 
  # endDate<-Sys.Date()-1
  # jsonQuery=paste0('{"loc":"',lon,',',lat,'","sdate":"1981-01-01","edate":"',endDate,'","grid":"21",
  #                                   "meta":"ll,elev","elems":[{"name":"pcpn","units":"in"},{"name":"mint","units":"degreeF"},{"name":"maxt","units":"degreeF"}]}')
  # 
  # outDaily<-postForm("http://data.rcc-acis.org/GridData",.opts = list(postfields = jsonQuery, 
  #                                                                     httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
  # # json to datframe
  # outDaily<-fromJSON(outDaily)
  # dataDaily<-data.frame(outDaily$data)
  # #
  # 
  # # character to numeric conversion
  # data <- dataDaily[,2:4] %>% mutate_if(is.character, as.numeric)
  # # bind back together
  # dataDaily<-cbind.data.frame(as.Date(dataDaily$X1,"%Y-%m-%d"),data)
  # # set colnames
  # colnames(dataDaily)<-c("date","precip","minT","maxT")
  # # calculate daily average temperature
  # dataDaily$avgT<-(dataDaily$maxT+dataDaily$minT)/2
  # 
  # # add in date elements
  # dataDaily$month<-as.numeric(format(dataDaily$date, "%m"))
  # dataDaily$year<-as.numeric(format(dataDaily$date, "%Y"))
  # dataDaily$doy<-as.numeric(format(dataDaily$date, "%j"))
  # tempData<-dataDaily
  # #
  # end.time <- Sys.time()
  # end.time - start.time
  # #####
  
    #####
    # original map click get lat/lon
    # add in leaflet map, overlay PRISM avg precip map or DEM grid...
    output$MyMap <- renderLeaflet({
      m <- leaflet() %>% setView(lng = -100.77156166441775, lat = 35.967440759643765, zoom = 3)
      m %>% addProviderTiles("Esri.WorldTopoMap") 
      #%>% addMarkers(lonIn, latIn)
    })

    observeEvent(input$MyMap_click, {
      leafletProxy("MyMap")%>% clearMarkers() %>%
        addMarkers(input$MyMap_click$lng, input$MyMap_click$lat)
      latIn<-input$MyMap_click$lat
      lonIn<-input$MyMap_click$lng
      output$latSel<-renderText({paste("Latitude: ",round(latIn,2))})
      output$lonSel<-renderText({paste("Longitude: ",round(lonIn,2))})
    })
    #####
    
    ##### 
    # draggable marker with observe
    #new drag marker to get lat/lon
    # output$MyMap = renderLeaflet({
    #   m <- leaflet() %>% setView(lng = -109.054530, lat = 36.992438, zoom = 6)
    #   m %>% addProviderTiles("Esri.WorldTopoMap") %>%
    #     addMarkers(lat = latIn,lng = lonIn, options = markerOptions(draggable = TRUE))
    # })
    # 
    # observe({
    #   #print(input$MyMap_marker_dragend)
    #   latIn<-input$MyMap_marker_dragend$lat
    #   lonIn<-input$MyMap_marker_dragend$lng
    #   output$latSel<-renderText({paste("Latitude: ",latIn)})
    #   output$lonSel<-renderText({paste("Longitude: ",lonIn)})
    # })
    #####
    
    ######
    # error catch on no map click with download button
      showModal(modalDialog(
        title = "Click on map to add location, then click 'Set location'",
        easyClose = TRUE,
        footer = NULL
      ))
    #shiny::validate(need(!is.null(input$MyMap_click), message = "Click map to add location"))
    #shiny::validate(need(input$MyMap_click != '', message = "Click map to add location"))
    #req(input$MyMap_click)  
    #####
    
    
    
    observeEvent(input$refresh, {
      
      ######
      # error catch on no map click with download button
      if(is.null(input$MyMap_click))
        showModal(modalDialog(
          title = "Click on map to add location first, then click 'Set location'",
          easyClose = TRUE,
          footer = NULL
        ))
      #shiny::validate(need(!is.null(input$MyMap_click), message = "Click map to add location"))
      shiny::validate(need(input$MyMap_click != '', message = "Click map to add location"))
      #req(input$MyMap_click)  
      #####
      
      
      withProgress(message = 'Downloading data set', style="old",
                   detail = 'Please wait...',{
    
                     # set location 
                     lat=input$MyMap_click$lat # input from map
                     lon=input$MyMap_click$lng # input from map
                     
                     cat(lat)
                     cat(lon)
                     
                     #download daily PRISM 
                     endDate<-Sys.Date()-1
                     jsonQuery=paste0('{"loc":"',lon,',',lat,'","sdate":"1981-01-01","edate":"',endDate,'","grid":"21",
                                    "meta":"ll,elev","elems":[{"name":"pcpn","units":"in"},{"name":"mint","units":"degreeF"},{"name":"maxt","units":"degreeF"}]}')
                     
                     outDaily<-postForm("http://data.rcc-acis.org/GridData",.opts = list(postfields = jsonQuery, 
                                                                                         httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
                     # json to datframe
                     outDaily<-fromJSON(outDaily)
                     dataDaily<-data.frame(outDaily$data)
                     #
                     print("downloading, processing")
                     # character to numeric conversion
                     data <- dataDaily[,2:4] %>% mutate_if(is.character, as.numeric)
                     # bind back together
                     dataDaily<-cbind.data.frame(as.Date(dataDaily$X1,"%Y-%m-%d"),data)
                     # set colnames
                     colnames(dataDaily)<-c("date","precip","minT","maxT")
                     # calculate daily average temperature
                     dataDaily$avgT<-(dataDaily$maxT+dataDaily$minT)/2
                     
                     # add in date elements
                     dataDaily$month<-as.numeric(format(dataDaily$date, "%m"))
                     dataDaily$year<-as.numeric(format(dataDaily$date, "%Y"))
                     dataDaily$doy<-as.numeric(format(dataDaily$date, "%j"))
                     cat(mean(dataDaily$avgT))
                     tempData<-dataDaily
                     print("dailyData processed")
                     #
                   })
                   
 
      
    output$distPlot <- renderPlotly({
     ######### PROCESS GDD -------
     # input vars
     startDate<-input$dateInput
     baseT<-input$baseT
     targetGDD<-input$gddTarget
     
     ######### PROCESS GDD -------
     #tempData<-dataDaily # work with copy of downloaded data
     cat(mean(tempData$avgT))
     
     # set external vars
     #startDate<-input$dateInput
     #baseT<-input$baseT
     
     # extract date vars 
     startMo<-as.numeric(format(startDate,"%m"))
     startDayMo<-as.numeric(format(startDate,"%d"))
     startYr<-as.numeric(format(startDate,"%Y"))
     
     # create adjusted date like water year doy/year
     adjDates<-adj_date(tempData$date, startMo, startDayMo)
     tempData$adjDOY<-adjDates$adjDOY
     tempData$adjYear<-adjDates$adjYear
     
     # drop leap year excess days
     tempData<-subset(tempData, adjDOY!=0)
     
     # calc day avgT with base temp for gdd
     tempData$avgT_base <- unlist(lapply(tempData$avgT, function(x) ifelse(x>=baseT, x-baseT, 0)))
     
     # cumulative doy climate - precip and temp
     cumClim <- tempData %>% 
       group_by(adjYear, adjDOY) %>% # still doesn't quite work adjYear kicks off before adjDOY
       summarise(precip = sum(precip, na.rm = T),
                 temp = sum(avgT_base, na.rm = T)) %>%
       mutate(psum = cumsum(precip),
              tsum = cumsum(temp))
     tempData$cumPrecip<-cumClim$psum
     tempData$gdd<-cumClim$tsum
     #####
     
     ##### DEVELOP DAILY CLIMO #####
     
     tempClimo<-subset(tempData, adjYear>=1991 & adjYear<=2020)
     
     doyClimo<-tempClimo %>% group_by(adjDOY) %>%
       summarise(min=min(gdd),
                 max=max(gdd),
                 median=median(gdd),
                 mean=mean(gdd))
     doyClimo$date<-doyClimo$adjDOY+(startDate-1)
     doyClimo[366,c(2:5)]<-doyClimo[365,c(2:5)]
     #####
     
     ##### GET 6-DAY NWS TEMP FORECAST #####
     #####
     
     ##### PLOT GDD
     #targetGDD<-input$gddTarget
     climoLong<-tidyr::pivot_longer(data = doyClimo, cols = "min":"mean", names_to = "vars", values_to = "values")
     currData<-subset(tempData, date>=startDate)
     
     # climo-based forecast
     bias<-currData$gdd[nrow(currData)]-doyClimo$mean[nrow(currData)]
     climoFcst<-doyClimo[nrow(currData):nrow(doyClimo),]
     bias<-ifelse(length(bias)==0,NA,bias) 
     climoFcst$mean<-climoFcst$mean+bias
     
     # ggplot(climoLong, aes(date, values, color=vars))+
     #   geom_line()+
     #   geom_line(data=currData, aes(date, gdd), color="red")+
     #   geom_vline(xintercept = doyClimo$date[which.max(doyClimo$mean>=targetGDD)],
     #              linetype = "dashed", color="black")+
     #   geom_vline(xintercept = currData$date[which.max(currData$gdd>=targetGDD)],
     #            linetype = "solid", color="red")+
     #   ylab("Cumulative GDD")+
     #   theme_bw()
     
     # targetGDD dates
     targClimoTxt<-paste0("Mean: ",as.Date(ifelse(which.max(doyClimo$mean>=targetGDD)==1,NA,
            doyClimo$date[which.max(doyClimo$mean>=targetGDD)])))
     targDateTxt<-paste0("This year: ",as.Date(ifelse(which.max(currData$gdd>=targetGDD)==1,NA,
            currData$date[which.max(currData$gdd>=targetGDD)])))
     targFcstTxt<-paste0("Climo Fcst: ",as.Date(ifelse(which.max(climoFcst$mean>=targetGDD)==1,NA,
                                                       climoFcst$date[which.max(climoFcst$mean>=targetGDD)])))
     
     
     targClimoNum<-as.numeric(ifelse(which.max(doyClimo$mean>=targetGDD)==1,NA,
                               doyClimo$date[which.max(doyClimo$mean>=targetGDD)]))
     targDateNum<-as.numeric(ifelse(which.max(currData$gdd>=targetGDD)==1,NA,
                              currData$date[which.max(currData$gdd>=targetGDD)]))
     targFcstNum<-as.numeric(ifelse(which.max(climoFcst$mean>=targetGDD)==1,NA,
                                    climoFcst$date[which.max(climoFcst$mean>=targetGDD)]))
     
          
     # render gdd plot
      ggplotly(ggplot(doyClimo, aes(date,mean))+
         geom_line()+
         geom_ribbon(aes(x=date, ymin=min,ymax=max), fill = "grey70", alpha=0.3)+
         geom_line(data=currData, aes(date, gdd), color="red")+
           geom_line(data=climoFcst, aes(date,mean), color="green")+
         # geom_vline(xintercept = (ifelse(which.max(doyClimo$mean>=targetGDD)==1,NA,
         #                                doyClimo$date[which.max(doyClimo$mean>=targetGDD)])),
         #            linetype = "dashed", color="black")+
         # geom_vline(xintercept = (ifelse(which.max(currData$gdd>=targetGDD)==1,NA,
         #                                currData$date[which.max(currData$gdd>=targetGDD)])),
         #            linetype = "solid", color="red")+
           geom_vline(aes(xintercept = targClimoNum, text =targClimoTxt),
                      linetype = "dashed", color="black")+
           geom_vline(aes(xintercept = targDateNum, text =targDateTxt),
                      linetype = "solid", color="red")+
           geom_vline(aes(xintercept = targFcstNum, text =targFcstTxt),
                      linetype = "solid", color="green")+
         ylab("Cumulative GDD")+
         theme_bw()+
         #ggtitle(paste0("GDD through: ", currData$date[nrow(currData)], " (at ",latIn,",",lonIn,")"))
         ggtitle(paste0("GDD through: ", currData$date[nrow(currData)]))
      )
       #####
     })     
     
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
