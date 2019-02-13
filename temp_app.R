### Author: Kevin Zolea ###
### Date: 11/2018 ###
### App for temperature impairments ###
###############################################################################
### Download necessary packages ###
if (!require(pacman)) {
  install.packages('pacman')
  
}

pacman::p_load("ggplot2","tidyr","plyr","dplyr","readxl","shinycssloaders",
               "readr","cowplot","lubridate","scales","shinythemes","plotly",
               "gridExtra","stringr","ggpmisc","data.table","rlang","purrr",
               "shiny","DT","leaflet","sf","rmapshaper","shinyWidgets",
               "rsconnect","shinyjs","htmltools","htmlwidgets","leaflet.extras")


###############################################################################
### Read in temp data ###
temp_data<- read_xlsx("qad_temperature_data_1997_to_2018.xlsx",sheet ="Sheet 1",col_names = T)%>%
  select(orgid,stdate,sttime,locid,charnam,val,valunit,MonLocName,MonLocTyp,LatDeg,LongDeg,swqs,HUC14,WMA)%>%
  mutate(stdate = as.Date(stdate))%>%
  mutate(year = lubridate::year(stdate))%>%
  mutate(val = ifelse(valunit == "deg F",round((val-32)*5/9,digits=2),val))%>%
  distinct(HUC14,stdate,sttime,val,.keep_all = T)
### Need to take out HUC part of HUC # to match with HUC_wx_station_relate ###
temp_data$HUC14<-gsub("HUC","",temp_data$HUC14)
temp_data$valunit<-gsub("deg F","deg C",temp_data$valunit)
### Read in continuous temp data ###
final_continuous_temp_data<-read_csv("final_continuous_temp.csv",col_names = T)#%>%
  #filter(HUC14 %in% impaired_hucs_df)

continuous_impaired_df<-unique(final_continuous_temp_data$HUC14)
### Read in station relate data to get swqs joined to continuous temp data ###
swqs_cont<-read_xls("BFF_station_relate_final.xls",col_names = T)%>%
  dplyr::select(Site,CATEGORY)%>%
  dplyr::rename("Station"="Site")
### Join final_continuous_temp_data to swqs_cont ###
final_continuous_temp_data<-left_join(final_continuous_temp_data,swqs_cont)
### Read in weather station, HUC relate dataset ###
HUC_wx_station_relate<- read_xls("weather_station_HUC_relate.xls",col_names = T)%>%
  select(HUC14,WMA,STATION,Lat,Long)
### Now read in Weather station dataset ###
multistation_wx_data<-read_xlsx("Multistation_Wx_Data.xlsx",col_names = T)%>%
  select(STATION,stdate,TMAX)%>%
  mutate(stdate = as.Date(stdate))
### Join HUC_wx_station_relate with multistation_wx_data to get Max temp data for weather stations ###
final_wx_temp_data<-left_join(HUC_wx_station_relate,multistation_wx_data,by = "STATION")
### Need to change column name to match with temp_data to be able to plot ###
colnames(final_wx_temp_data)[colnames(final_wx_temp_data)=="TMAX"]<-"val"
### Convert F to C for air temp ###
final_wx_temp_data<-final_wx_temp_data%>%
  mutate(val = round((val-32)*5/9,digits=2))

### Read in shapefiles ###
wx_stations<-st_read(getwd(),layer = "WxStationLocations")
temp_impaired_hucs<-st_read(getwd(),layer = "Temp_Impairments")
NJ_HUCs<-st_read(getwd(),layer = "2014_NJ_Integrated_Report_AU")
imp_temp_HUC<-unique(temp_impaired_hucs$HUC14TXT)
monitoring_stations<-st_read(getwd(),layer = "2016_MonitoringSites_FINAL")
streams<-st_read(getwd(),layer = "3rdorderupStreams_HUC14_join")%>%
  select(HUC14,W_NAME,STREAMORDE,geometry)
BFF_stations<-st_read(getwd(),layer = "BFFstation_huc_join")%>%
  select(Site,Location,Lat,Long_,HUC14,geometry)
### Change projections to work with leaflet map ###
NJ_HUCs<-st_transform(NJ_HUCs, crs="+init=epsg:4326")
temp_impaired_hucs<-st_transform(temp_impaired_hucs, crs="+init=epsg:4326")
wx_stations<-st_transform(wx_stations, crs="+init=epsg:4326")
monitoring_stations<-st_transform(monitoring_stations,crs="+init=epsg:4326")
monitoring_stations$HUC14<-gsub("HUC","",monitoring_stations$HUC14)
streams<-st_transform(streams,crs = "+init=epsg:4326")
BFF_stations<-st_transform(BFF_stations,crs = "+init=epsg:4326")
### Needed to get map to work in leaflet ###
### ms_simplify is used to simplify polygons so map can render faster ###
NJ_HUCs<-st_zm(NJ_HUCs, drop = T, what = "ZM")%>%
  ms_simplify(.)
temp_impaired_hucs<-st_zm(temp_impaired_hucs, drop = T, what = "ZM")%>%
  ms_simplify(.)
wx_stations<-st_zm(wx_stations, drop = T, what = "ZM")
BFF_stations<-st_zm(BFF_stations, drop = T, what = "ZM")
###############################################################################
### Needed to get polygons on map because ms_simplify gives names to geometry; which gives error ###
names(st_geometry(NJ_HUCs)) = NULL
names(st_geometry(temp_impaired_hucs)) = NULL
### Vector of imparied HUCs ###
impaired_hucs_df<-as.character(temp_impaired_hucs$HUC14TXT)
impaired_hucs_df<-gsub("HUC","",impaired_hucs_df)
remove<-c("Delaware River 17","Delaware River 16","Delaware River 15","Delaware River 18")
impaired_hucs_df<-impaired_hucs_df[!impaired_hucs_df %in% remove]
NJ_HUCs$HUC14TXT<- gsub("HUC","",NJ_HUCs$HUC14TXT)
###############################################################################
### Theme for plots ###
shiny_plot_theme<- theme_linedraw()+
  theme(plot.title=element_text(size=15, face="bold",vjust=0.5,hjust = 0.1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.text=element_text(size=10, face="bold"),
        legend.title = element_blank(),
        plot.subtitle = element_text(size=15, face="bold",vjust=0.5,hjust = 0.1))
###############################################################################
### Define UI for application ###
ui <- navbarPage(theme = shinytheme("yeti"),
                 tags$b("NJDEP DWMS Temperature Impairments"),
                 tabPanel("About App",
                          div(class= "outer",
                              tags$head(
                                #includeCSS("/Users/kevinzolea/Desktop/Temp_Impairments/www/styles.css")),
                              includeCSS("V:/lum/WM&S/BEAR (Bureau of Environmental Analysis and Restoration)/Envpln/Hourly Employees/KevinZolea/Rwork/Temperature_Plots/Temperature_Impairment_Work/www/styles.css")),
                              h1("Welcome to the NJDEP's Temperature Impairment's app!"),
                              h2("Introduction:"),
                              h3("The purpose of this app is to better understand the water temperature impairments
                                 throughout the state. This app will make it easier to view and play with the 
                                 data, as well as gain different insight from the data through maps/plots."),
                              br(),
                              HTML('<center><img src="http://media.nj.com/centraljersey_impact/photo/9558763-large.jpg"></center>')),
                          tags$head(tags$script(HTML('
                                                 var customHref = function(tabName) {
                                                     var dropdownList = document.getElementsByTagName("a");
                                                     for (var i = 0; i < dropdownList.length; i++) {
                                                     var link = dropdownList[i];
                                                     if(link.getAttribute("data-value") == tabName) {
                                                     link.click();
                                                     };
                                                     }
                                                     };
                                                     ')))),
                 tabPanel("Data",
                          selectInput("tables",label="Choose Data to View:",
                                      choices = c("Discrete Water Temperature",
                                                  "Continuous Water Temperature")),
                          DT::dataTableOutput("data")%>%
                            withSpinner(type = 5, color = "blue")),
                 tabPanel("Map",leafletOutput("temp_map", height = "95vh")%>%
                            withSpinner(type = 5, color = "blue")),
                 tabPanel("Plots",
                          sidebarLayout(
                            sidebarPanel(width = 3,
                                         tags$style(".well {background-color:#222222;}"),
                                         radioButtons("button",label = strong("Choose Data Type:",style = "color:white;font-weight: bold;font-size:1.3em;"),
                                                      choiceNames = list(
                                                        HTML("<font color = 'white'>Stations With Discrete</font>"),
                                                        tags$span(style = "color:white","Stations With Discrete & Continuous")), 
                                                      selected = "Stations With Discrete",inline = F,
                                                      choiceValues = c("Stations With Discrete","Stations With Discrete & Continuous")),
                                         h5(style="color:white;","Continuous Data Funding Source: Federal(Sports Fish Restoration)"),
                                         uiOutput("huc"),
                                         selectizeInput("swqs",label = strong("Select Temperature Standard:",
                                                                              style = "color:white;font-weight: bold;font-size:1.3em;"),
                                                        choices = NULL,selected = NULL,multiple = T),
                                         tags$a(class="btn btn-default btn-sm", href="https://www.nj.gov/dep/rules/rules/njac7_9b.pdf",
                                                "Learn More About Standards",target = "_blank"),
                                         br(),br(),
                                         chooseSliderSkin(skin = "Flat"),
                                         sliderInput("alpha",label = strong("Select Shade of Standards Line:",
                                                                            style = "color:white;font-weight: bold;font-size:1.3em;"),
                                                     min = 0,max = 0.8,value=0.5),
                                         sliderInput("alpha2",label = strong("Select Shade of Air Temperature Line:",
                                                                             style= "color:white;font-weight: bold;font-size:1.3em;"),
                                                     min = 0,max = 0.8,value=0.5),
                                         sliderInput("alpha3",label = strong("Select Shade of Continuous Water Temperature Line:",
                                                                             style= "color:white;font-weight: bold;font-size:1.3em;"),
                                                     min = 0,max = 0.8,value=0.5),
                                         checkboxInput("check_air",label = strong("Take Away Air Temperature Data",
                                                                                  style = "color:white;font-weight: bold;font-size:1.3em;"),
                                                       value = F),
                                         HTML("<font color = 'white'>Author: Kevin Zolea\n (kevin.zolea@dep.nj.gov)</font>")),
                            mainPanel(plotlyOutput("plot1")%>%withSpinner(type = 5, color = "blue"),
                                      leafletOutput("map2")%>%withSpinner(type = 5, color = "blue")))))
###############################################################################
### Creates server for app ###
server <- function(input, output,session) {
  ### Creates a data table for the data tab ###
  output$data <- DT::renderDataTable({
    if(input$tables == "Discrete Water Temperature"){
    DT::datatable(temp_data,filter = 'top',options = list(scrollX = TRUE,
                                                          pageLength = 100))
    }
    
    else if (input$tables == "Continuous Water Temperature"){
      DT::datatable(final_continuous_temp_data,filter = 'top',options = list(scrollX = TRUE,
                                                            pageLength = 100))
    }
  })
  ###############################################################################
  ### Creates a custom icon for weather stations on leaflet map ### 
  weather_icon<-makeIcon(
    iconUrl = "www/cloud_flat.png",
    iconWidth = 25 , iconHeight = 25,
    iconAnchorX =30 , iconAnchorY = 30
  )
  ###############################################################################      
  ### Creates interactive map ###
  output$temp_map<-renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 7))%>%
      addTiles()%>%
      addResetMapButton()%>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$OpenStreetMap.BlackAndWhite, group = "Grey") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      setView(lng = -74.4 ,lat =40, zoom = 8)%>%
      addPolygons(data = temp_impaired_hucs,color = "#F3161B",weight = 1,smoothFactor = 1,
                  opacity = 0.5, fillOpacity = 0.6,
                  group = "Impaired HUC14s",
                  highlightOptions = highlightOptions(color = "blue",
                                                      weight = 2,bringToFront = TRUE),
                  label = ~ paste(HUC14TXT,AU_name),
                  layerId = ~ temp_impaired_hucs$AU_name)%>%
      addPolygons(data= NJ_HUCs,color = "#636060",weight = 1,smoothFactor = 1,
                  opacity = 0.5, fillOpacity = 0.1,group = "HUC14s",fillColor = "white",
                  highlightOptions = highlightOptions(color = "blue",
                                                      weight = 2,bringToFront = TRUE),
                  popup = paste("<h6> HUC Name:</h6>",NJ_HUCs$AU_NAME,"\n","<h6>HUC14 #:<h6/>\n",NJ_HUCs$HUC14TXT, sep = ""))%>%
      addMarkers(data = wx_stations, group = "Weather Stations",
                 icon = weather_icon,popup = ~paste("<h4> Station:</h4>",STATION,sep = ""))%>%
      addLayersControl(
        baseGroups = c("OSM (default)", "Grey", "Satellite"),
        overlayGroups = c("Impaired HUC14s","HUC14s","Weather Stations"),
        options = layersControlOptions(collapsed = FALSE))%>%
      addLegend("bottomright",colors = c("#636060","#F3161B"),opacity = 0.6,
                labels = c("HUC14s","Impaired HUCs"))
  })
  ###############################################################################   
  ### Allows user to have map zoomed in when impaired HUC is clicked ###
  observe({
    click <- input$temp_map_shape_click
    if(is.null(click))
      return()
    else
      leafletProxy("temp_map")%>%
      setView(lng = click$lng , lat = click$lat, zoom=10)
  })
  ##################################################################################    
  ### Drop down menu updates based on input from radio buttons ###
  output$huc<-renderUI({
    if(input$button == "Stations With Discrete"){
      selectizeInput("huc_input",label = strong("Select HUC14:",style = "color:white;font-weight: bold;font-size:1.3em;"),
                     choices = sort(impaired_hucs_df))
    }
    
    else{
      selectizeInput("huc_input",label = strong("Select HUC14:",style = "color:white;font-weight: bold;font-size:1.3em;"),
                     choices = c("02040104130010", "02030105020040", "02030105050070", "02040104140040" ,"02030104100080" ,"02040105050050",
                                 "02030103050080" ,"02030103100050", "02030105010080", "02030105020020" ,"02030103070060", "02030103070040",
                                 "02030103030100", "02030103020010" ,"02030103050070", "02030103100070"))
    }
    
  })
  
  ############################################################################
  ### Make temp data Reactive ###
  reac_temp_data<-reactive({
    temp_data%>%
      filter(HUC14 == input$huc_input)
  })
  ###############################################################################
  ### Make weather statation temp data reactive ###
  wx_station_temp<-reactive({
    final_wx_temp_data%>%
      filter(HUC14 ==  input$huc_input)
  })
  ###############################################################################
  ### Make continuous data reactive ###
  continuous_data_reactive<-reactive({
    final_continuous_temp_data%>%
      filter(HUC14 == input$huc_input)
  })
  ############################################################################### 
  ### Updates Standards drop down based on HUC drop down ###
   observe({
   #if(input$huc_input != ""){
     req(input$huc_input)
     swqs_choices<- as.list(reac_temp_data()$swqs[reac_temp_data()$HUC14==input$huc_input])
     names(swqs_choices)<-reac_temp_data()$swqs[reac_temp_data()$HUC14==input$huc_input]
  
      updateSelectizeInput(session,"swqs",choices = swqs_choices,selected = swqs_choices)
   # }
  
  
    })
  
  ###############################################################################
  ### Creates Plot ###
  output$plot1<-renderPlotly({
    req(input$huc_input)
    if(input$button == "Stations With Discrete"&input$check_air == FALSE){
      #if(input$check_air == TRUE){
      p<-ggplot(data= reac_temp_data(),aes(x=stdate,y=val,group=1,
                                           text = paste("Date:",stdate,
                                                        "<br>Temperature:",val,
                                                        "<br>Station:",reac_temp_data()$locid,
                                                        "<br>SWQS:",reac_temp_data()$swqs)))+
        geom_point(aes(color = locid),size=1.3)+
        #geom_line(data =  continuous_data_reactive(),aes(color = Organization),
        # size = 1.3, stat = "identity")+
        geom_line(data = wx_station_temp(),aes(color = "Air Temperature"),
                  size =1.3,stat = "identity", alpha = input$alpha2)+
        scale_x_date(date_breaks = "2 years",date_labels = "%Y")+
        ggtitle(paste("Temperature(?C) 1997-2018\n","HUC14#:",input$huc_input,sep = ""))+
        labs(x="Year", y="Temperature(?C)")+
        shiny_plot_theme
    }
      else {
        p<-ggplot(data= reac_temp_data(),aes(x=stdate,y=val,group=1,
                                             text = paste("Date:",stdate,
                                                          "<br>Temperature:",val,
                                                          "<br>Station:",reac_temp_data()$locid,
                                                          "<br>SWQS:",reac_temp_data()$swqs)))+
          geom_point(aes(color = locid),size=1.3)+
          #geom_line(data =  continuous_data_reactive(),aes(color = Organization),
          # size = 1.3, stat = "identity")+
          scale_x_date(date_breaks = "2 years",date_labels = "%Y")+
          ggtitle(paste("Temperature(?C) 1997-2018\n","HUC14#:",input$huc_input,sep = ""))+
          labs(x="Year", y="Temperature(?C)")+
          shiny_plot_theme
      }
      
      if (input$button == "Stations With Discrete & Continuous"&input$check_air == FALSE){
        req(input$huc_input)
        p<-ggplot(data= reac_temp_data(),aes(x=stdate,y=val,group=1,
                                             text = paste("Date:",stdate,
                                                          "<br>Temperature:",val,
                                                          "<br>Station:",reac_temp_data()$locid,
                                                          "<br>SWQS:",reac_temp_data()$swqs)))+
          geom_point(aes(color = locid),size=1.3)+
          geom_line(data =  continuous_data_reactive(),aes(color = "BFF Continuous Data"),
           size = 1.3, stat = "identity",alpha = input$alpha3)+
          geom_line(data = wx_station_temp(),aes(color = "Air Temperature"),
                    size =1.3,stat = "identity", alpha = input$alpha2)+
          scale_x_date(date_breaks = "2 years",date_labels = "%Y")+
          ggtitle(paste("Temperature(?C) 1997-2018\n","HUC14#:",input$huc_input,sep = ""))+
          labs(x="Year", y="Temperature(?C)")+
          shiny_plot_theme
        
      }
      
    
   if(input$button == "Stations With Discrete & Continuous"&input$check_air == TRUE){
      p<-ggplot(data= reac_temp_data(),aes(x=stdate,y=val,group=1,
                                           text = paste("Date:",stdate,
                                                        "<br>Temperature:",val,
                                                        "<br>Station:",reac_temp_data()$locid,
                                                        "<br>SWQS:",reac_temp_data()$swqs,sep = "")))+
        geom_point(aes(color = locid),size=1.3)+
        geom_line(data =  continuous_data_reactive(),aes(color = "BFF Continuous Data",group=1,
                                                         text = paste("Date:",stdate,
                                                                      "<br>Station:",Station,
                                                                      "<br>SWQS:",CATEGORY,
                                                                      "<br>Temperature:",val,sep = "")),
                  size = 1.3, stat = "identity", alpha = input$alpha3)+
        scale_x_date(date_breaks = "2 years",date_labels = "%Y")+
        ggtitle(paste("Temperature(?C) 1997-2018\n","HUC14#:",input$huc_input,sep = ""))+
        labs(x="Year", y="Temperature(?C)")+
        shiny_plot_theme}
    
    if ("FW2-NT" %in% input$swqs){
     p<-p+
      geom_hline(aes(yintercept = 31,color="FW2-NT",group =1,
                   text=paste("Standard:FW2-NT(31?C)")),size=1.3,alpha = input$alpha)
     }
    
      if("FW2-TP" %in% input$swqs){
       p<-p+geom_hline(aes(yintercept = 22,color="FW2-TP",group =1,
                          text = paste("Standard:FW2-TP(22?C)")),size=1.3,alpha = input$alpha)
     }
    
     if("FW2-TM"%in% input$swqs){
       p<-p+geom_hline(aes(yintercept = 25,color="FW2-TM",group =1,
                         text = paste("Standard:FW2-TM(25?C)")),size=1.3,alpha = input$alpha)
     }
    
      if("PL-TM" %in% input$swqs){
        p<-p+geom_hline(aes(yintercept = 25 , color= "PL-TM",group=1,
                            text=paste("Standard:PL-TM(25?C)")),size = 1.3,alpha = input$alpha)
       }
    
      if("SE1" %in% input$swqs){
        p<- p+ geom_hline(aes(yintercept = 29.4, color = "SE1",group=1,
                              text=paste("Standard:SE1(29.4?C)")),size = 1.3,alpha = input$alpha)
      }
    
    #p<-p+scale_color_manual(values=c("Water Temperature"="#1515E1","Air Temperature"="#27600A",
    #"FW2-NT"="#FF9900","FW2-TP"="#CC0000","FW2-TM"="#5125C3",
    #"SE1"="#00FF00","PL-TM" = "#5125C3"))
    
    #p<-p +scale_color_manual(values = randomColor(length(unique(temp_data$HUC14)),luminosity = "dark"))
    
    ### Converts ggplot object into plotly object ###
    ggplotly(p,dynamicTicks = "x",tooltip = "text")%>%
      config(collaborate = F,displaylogo=F,modeBarButtonsToRemove = list("lasso2d","hoverClosestCartesian",
                                                                         "hoverCompareCartesian","select2d"))

    
  })
  
  ### Creates map under plot to see where each huc is ###
  output$map2<-renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 7))%>%
      addTiles()%>%
      addResetMapButton()%>%
      addProviderTiles(providers$OpenStreetMap.BlackAndWhite, group = "Grey") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      setView(lng = -74.4 ,lat =40, zoom = 7)%>%
      addPolygons(data = NJ_HUCs,color = "#636060",fillColor = "#00FFFFFF",layerId = ~HUC14TXT,weight = 1,
                  popup = paste("HUC14 Name:\n",NJ_HUCs$AU_NAME,"<br>",
                                "HUC14#:\n",NJ_HUCs$HUC14TXT,"<br>",sep = ""),
                  highlightOptions = highlightOptions(color = "blue",
                                                      weight = 2,bringToFront = TRUE))%>%
      addLayersControl(
        baseGroups = c("Grey", "Satellite"),
        overlayGroups = c("Discrete Monitoring Stations","Continuous Monitoring Stations"),
        options = layersControlOptions(collapsed = FALSE))
  })
  ### Highlights polygon when huc is clicked on drop down menu ###
############################################################################
  observeEvent(input$huc_input,{
    
   proxy<- leafletProxy("map2")
    
    click<-input$map2_shape_click
    
    hucsub<-subset(NJ_HUCs, HUC14TXT == input$huc_input)
    
    selected<- NJ_HUCs[NJ_HUCs$HUC14TXT == input$huc_input,]
    
    bbox<-st_bbox(selected)%>%
      as.vector()
    
    if(nrow(hucsub) == 0){
      proxy %>% removeShape(layerId = "Selected")
    } else if(length(click$id) && input$huc_input != click$id){
      proxy %>% 
        setView(lng = click$lng , lat = click$lat, zoom=10)%>%
        addPolygons(data = selected,
                    #fillColor = "yellow",
                    fillOpacity = 0.1,
                    color = "orange",
                    opacity = 2,
                    weight = 5,
                    stroke = T,
                    layerId = "Selected",
                    popup = paste("HUC14 Name:\n",selected$AU_NAME,"<br>",
                                  "HUC14#:\n",selected$HUC14TXT,"<br>",sep = ""),
                    highlightOptions = highlightOptions(color = "blue",
                                                        weight = 2,bringToFront = TRUE))%>%
        fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])
      } else if(!length(click$id)){
      proxy %>% 
        setView(lng = click$lng , lat = click$lat, zoom=10)%>%
        addPolygons(data = selected,
                    #fillColor = "yellow",
                    fillOpacity = 0.1,
                    color = "orange",
                   opacity = 2,
                    weight = 5,
                    stroke = T,
                    layerId = "Selected",
                    popup = paste("HUC14 Name:\n",selected$AU_NAME,"<br>",
                                  "HUC14#:\n",selected$HUC14TXT,"<br>",sep = ""),
                    highlightOptions = highlightOptions(color = "blue",
                                                        weight = 2,bringToFront = TRUE))%>%
        fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])
      }
  
  })
############################################################################  
  ### Allows user to have map zoomed in when impaired HUC is clicked ###
  #observe({
  # click <- input$map2_shape_click
  # if(is.null(click))
  # return()
  # else
  #leafletProxy("map2")%>%
  # setView(lng = click$lng , lat = click$lat, zoom=10)
  # })
  
  ### Makes monitoring stations reactive to huc input ###
  monitor_reactive<-reactive({
    req(input$huc_input)
    monitoring_stations%>%
    filter(HUC14 == input$huc_input)
  })
  ### Make BFF monitoring stations reactive to huc input ####
  BFF_stations_reactive<-reactive({
    req(input$huc_input)
    BFF_stations%>%
      filter(HUC14 == input$huc_input)
  })
  
  ### Make streams reactive to huc input ###
  streams_reactive<-reactive({
    req(input$huc_input)
    streams%>%
      filter(HUC14 == input$huc_input)
  })
  

  
  ### Updates map ###
  observe({
    leafletProxy("map2") %>%
      clearMarkers() %>%
      addMarkers(data = monitor_reactive(),popup = paste("Station:",monitor_reactive()$NewStation,sep = ""),
                 group = "Discrete Monitoring Stations")%>%
      addPolylines(data = streams_reactive(),label = paste("Stream Name:",streams_reactive()$W_NAME,sep = ""))%>%
      addMarkers(data = BFF_stations_reactive(),popup = paste("Station:",BFF_stations_reactive()$Site,sep = ""),
                        group = "Continuous Monitoring Stations")
  
  })
}
###############################################################################
# Run the application 
shinyApp(ui = ui, server = server)

