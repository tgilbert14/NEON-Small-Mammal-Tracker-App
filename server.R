#--Server----------------------------------------------------------------
server <- function(input, output, session) {
  
  ## hiding elements/tabs until data loaded
  shinyjs::hide("WebLinks")
  shinyjs::hide("inTabset")
  
  ## move to next tab when species selected
  observeEvent(input$SelectID, {
    updateTabsetPanel(session, "inTabset",selected = "Cap History")
  })
  
  site_select <- reactive({
    req(input$Select)
    site<- input$Select #saving site selection

    if (is.null(input$Select))
      return(NULL)
    
    ## hide info after data loads in
    shinyjs::hide("more")
    
    start_d<-format(input$dateRange[1]) # start date
    end_d<-format(input$dateRange[2])  # end date
    
    #Downloading NEON portal data since 2016 to present w/ dpID
    raw <- loadByProduct(dpID = "DP1.10072.001", site = site, startdate = start_d, enddate = end_d, package = 'basic', check.size = 'F' )
    data.raw <- as_tibble(raw$mam_pertrapnight)    #Getting raw data

    # insertUI(selector = "#dateRange",
    #          where = "beforeEnd",
    #          ui = textInput("SelectID", "Type in tag of individual species:",
    #                         value = "", placeholder = "Type in a tagID", width = "100%"))
    data.raw
  })
  
  map_radius_size<- reactive({
    r_size<- input$rad_size
  })
  
  view_select <- reactive({
    view_pick<- input$view
  })
  

  ID_select <- reactive({
    req(input$SelectID)
    
    ID.raw<- input$SelectID #saving tagID selection
    site<- input$Select #saving site selection
    data.raw<<- site_select()
    
    ## show other tabs after data loaded and species selected
    shinyjs::show("inTabset")
    
    if (is.null(site))
      return(NULL)

    d<- data.raw %>%
      dplyr::filter(substr(data.raw$tagID,14,length(data.raw$tagID)) == ID.raw)
    
    ## pop up for images for selected species
    pop_up<- unique(d$scientificName)[1]
    
    pos<- as.numeric(gregexpr(" ", pop_up)[[1]][1])

    genus<- substr(pop_up, 1, pos-1)
    epithet<- substr(pop_up, pos+1, nchar(pop_up))
    ## formatting links
    url_records<- paste0("https://biorepo.neonscience.org/portal/collections/list.php?usethes=1&taxa=",genus,"+",epithet)
    url_images<- paste0("https://biorepo.neonscience.org/portal/imagelib/search.php?usethes=1&taxa=",genus,"+",epithet)
    
    ## UI updates for link pop up
    insertUI(selector = "#WebLinks",
             where = "afterEnd", 
             ui = tags$a(href=url_records, paste0("NEON BioRepository Samples for -> ",genus," ",epithet),
                         target = "_blank"))
    insertUI(selector = "#WebLinks",
             where = "afterEnd", 
             ui = br())
    insertUI(selector = "#WebLinks",
             where = "afterEnd", 
             ui = tags$a(href=url_images, paste0("NEON BioRepository Images for -> ",genus," ",epithet),
             target = "_blank"))

    ID<- paste0("NEON.MAM.",d$domainID[1],".",ID.raw)
    ID
  })
  
  # observe({
  #   
  #   ## info box
  #   output$data1 <-renderDataTable({
  #     req(input$Select)
  #     data.raw<- site_select()
  #     
  #     data.raw
  #   })
  #   # ## info box
  #   # output$data2 <-renderValueBox({
  #   #   valueBox(2,"Species",color='blue') #icon was list
  #   # })
  #   # ## info box
  #   # output$data3 <-renderValueBox({
  #   #   valueBox(3,"Species",color='blue') #icon was list
  #   # })
  # 
  # 
  # })

  
  
  output$FitSelect <- renderDataTable({    
    data.raw <- site_select()
    
    if (is.null(data.raw))
      return("No data")
    
    ## counting species by tagID and plot
    fit<- data.raw %>%
      group_by(tagID, taxonID, scientificName, plotID) %>% 
      summarise("total captures" = n()) %>% 
      filter(!is.na(tagID)) %>% 
      arrange(desc(`total captures`))
    ## for hind foot length
    fit_hf <- data.raw %>%
      group_by(tagID, taxonID, scientificName, plotID) %>%
      filter(lifeStage == "adult") %>%
      filter(!is.na(hindfootLength)) %>% 
      summarise("avg adult HF length (mm)" = mean(hindfootLength)) %>% 
      filter(!is.na(tagID))
    ## for weight
    fit_w <- data.raw %>%
      group_by(tagID, taxonID, scientificName, plotID) %>%
      filter(lifeStage == "adult") %>% 
      filter(!is.na(weight)) %>% 
      summarise("avg adult weight (g)" = mean(weight)) %>% 
      filter(!is.na(tagID))

    ## joining by tagID only
    fit<- left_join(fit, fit_hf)
    fit<- left_join(fit, fit_w)
    
    ## rounding digits
    fit$`avg adult HF length (mm)` <- round(fit$`avg adult HF length (mm)`, 2)
    fit$`avg adult weight (g)` <- round(fit$`avg adult weight (g)`, 2)

    ## renaming columns
    fit$tagID<- substr(fit$tagID,14,50)
    #colnames(fit)[4]<- 'plotID'
    
    ## inserting UI for date range
    insertUI(selector = "#dateRange",
             where = "beforeEnd",
             ui = br())
    
    sp<- unique(fit$tagID)
    
    if (length(sp) > 0) {
      ## inset tagID selection
      insertUI(selector = "#dateRange",
               where = "beforeEnd",
               ui = selectizeInput("SelectID", "Type in tag of individual species:",
                                choices = c("",sp),
                                width = "100%"))
    } else {fit<- data.frame('No data -> '=double(),'Try again'=integer())}

    datafile<-datatable(fit,options = list(pageLength = 20),
                        style='bootstrap',
                        class='compact cell-border hover display',
                        filter=list(position='top',plain=TRUE))
    
  })
  
  mymap <- reactive({
    # here I have specified a tile from openstreetmap
    data.raw <- site_select()
    view_pick<- view_select()
    r_size<- map_radius_size()
    
    if (is.null(data.raw))
      return(NULL)
    
    ###Make code to look at sites lat/long and choose a focus point! Only works for SRER for now...
    
    data<- data.raw %>% 
      filter(!is.na(tagID)) %>% 
      select(plotID, decimalLongitude,decimalLatitude)
    colnames(data)[2]<- 'Longitude'
    colnames(data)[3]<- 'Latitude'
    
    ##For total # of species captured at each plot
    per<- data.raw %>% 
      group_by(taxonID, collectDate) %>% 
      filter(!is.na(taxonID))
    
    per$year <-  str_sub(per$collectDate, 1, 4) 
    
    all_species<- per %>% 
      group_by(year) %>% 
      summarise(Totalcount=n())
    
    y2016<- all_species %>% 
      filter(year == '2016')
    y2017<- all_species %>% 
      filter(year == '2017')
    y2018<- all_species %>% 
      filter(year == '2018')
    y2019<- all_species %>% 
      filter(year == '2019')
    y2020<- all_species %>% 
      filter(year == '2020')
    y2021<- all_species %>% 
      filter(year == '2021')
    y2022<- all_species %>%
      filter(year == '2022')
    
    ##Not using as of yet
    per_sp<- data.raw %>% 
      select(scientificName, plotID, collectDate) %>% 
      group_by(plotID, scientificName) %>% 
      summarise(count=n()) %>% 
      filter(!is.na(scientificName)) %>% 
      arrange(desc(count))
    
    geo_per_plot <- left_join(per_sp, data, by = 'plotID')
    
    new_geo<- unique(geo_per_plot)
    
    #For setting map view center
    coord<- new_geo %>% 
      mutate(meanLon = mean(Longitude)) %>% 
      mutate(meanLat = mean(Latitude))
    
    nb.cols <- length(new_geo$scientificName)
    mycolors <- colorRampPalette(brewer.pal(8, "Set1"))(nb.cols)
    
    pal <- colorFactor(
      palette = mycolors,
      domain = new_geo$scientificName)
    
    # Prepare the text for the tooltip:
    mytext2 <- paste(
      "ScientificName: ", new_geo$scientificName, "<br/>", 
      "Count: ", new_geo$count, "<br/>",
      "PlotID: ", new_geo$plotID, "<br/>", 
      "Long: ", new_geo$Longitude,"<br/>",
      "Lat: ", new_geo$Latitude, sep="") %>%
      lapply(htmltools::HTML)
    
    m <- leaflet(new_geo, height=2000, width=2000) %>%
      addTiles()  %>% 
      setView(lat=coord$meanLat[1], lng=coord$meanLon[1], zoom=11) %>%
      addProviderTiles(view_pick) %>%
      addCircleMarkers(~Longitude, ~Latitude,
                       fillColor =~pal(new_geo$scientificName), opacity = .6, fillOpacity = .5, radius=~(count/2)*r_size,
                       popup = paste0(new_geo$scientificName) %>%
                         lapply(htmltools::HTML),
                       stroke = T, weight = 1,  color = 'white', 
                       label = mytext2,
                       labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
      ) %>% 
      addLegend( pal=pal, values=~new_geo$scientificName, opacity=0.9, title = "Species Diversity Per Plot", position = "bottomright" )
      m 
    
  })
  
  output$map <- renderLeaflet({   
    mymap()
  })
  
  

  
  # observe({
  #   ## info boxes
  #   output$total<- renderInfoBox({
  #     data.raw <- site_select()
  #     req(data.raw)
  #     #count(unique(data.raw$tagID))
  #     infoBox(
  #       "Total Captures",value =count(unique(data.raw$tagID)), color = "blue"
  #     )
  #   })
  #   
  # })
  
      
  output$DataSet1 <- renderPlotly({    
    data.raw <- site_select()
    
    if (is.null(data.raw))
      return(NULL)

    data.raw$year_month <-  str_sub(data.raw$collectDate, 1, 7)

    data.sum<- data.raw %>%
      group_by(plotID, scientificName, year_month) %>% 
      summarise(count = n()) %>% 
      filter(!is.na(scientificName)) %>% 
      arrange(desc(count))
    
    col_num<- data.sum %>% 
      group_by(scientificName) %>% 
      count()
    
    nb.cols <- nrow(col_num)
    mycolors <- colorRampPalette(brewer.pal(8, "Set1"))(nb.cols)
    
    a.plot<- ggplot(data.sum, aes(year_month, count, group = scientificName, fill = scientificName))+
      geom_point(aes(color=scientificName), size=.7)+
      geom_line(aes(color=scientificName)) +
      facet_wrap(facets = vars(plotID), scales = 'free')+
      scale_fill_manual(values = mycolors) +
      ggtitle(paste0(data.raw$siteID[1]," Species Per Plot by Month"))+
      #scale_fill_gradientn(colours=rev(rainbow(100, start=0, end=0.19)))+
      xlab("Date (Year_month)")+
      ylab("Species Count")+
      theme(axis.text.x = element_text(size = 8, angle = 45))
    
    a.plot <- a.plot + theme(panel.spacing.x=unit(1, "lines"),panel.spacing.y=unit(1.5, "lines"))
    a.plot<- ggplotly(a.plot, width = 1500, height = 800)
    a.plot
    
  })
  
  output$NEONDataSet1 <- renderDataTable({    
    #pulling data from NEON
    data.raw <- site_select()
    ID<- ID_select()
    
    if (is.null(ID))
      return(NULL) 
    if (is.null(data.raw))
      return(NULL)
    
    tagID<- data.raw %>% 
      filter(tagID == ID) %>% 
      select(tagID, plotID, collectDate, recapture, scientificName, identificationQualifier, hindfootLength, weight, lifeStage, sex, testes, nipples, pregnancyStatus, vagina, fate, remarks)
    
    tagID$collectDate<- substr(tagID$collectDate,1,10)
    
    if (nrow(tagID) == 0) {
      datafile<-datatable(data = data.frame("no tag selected to filter through data",check.names = F),
                          rownames = F,colnames = 'Select a tagID first'
      )} else 
        datafile<-datatable(tagID,options = list(pageLength = 20),
                            style='bootstrap',
                            class='compact cell-border hover display',
                            filter=list(position='top',plain=TRUE))
  })
  
  #---ploting each species HF vs W to look for outliers- prints from current data
  output$data1 <- renderDataTable({
    #pulling data from NEON
    data.raw <- site_select()
    ID<- ID_select()
    
    if (is.null(ID))
      return(NULL) 
    if (is.null(data.raw))
      return(NULL) 
    
    tagID_h<- data.raw %>% 
      filter(tagID == ID)
    
    if (nrow(tagID_h) == 0) {
      datafile<-datatable(data = data.frame("no tag selected to filter through data",check.names = F),
                          rownames = F,colnames = 'Select a tagID first'
                          )} else
                            datafile<-datatable(tagID_h,options = list(pageLength = 20),
                                                style='bootstrap',
                                                class='compact cell-border hover display',
                                                filter=list(position='top',plain=TRUE))
  })
  
  ## Measurement Data
  output$plot1 <- renderPlotly({
    data.raw <- site_select()
    ID<- ID_select()
    
    if (is.null(ID))
      return(NULL) 
    if (is.null(data.raw))
      return(NULL)
    
    tagID<- data.raw %>% 
      filter(tagID == ID) %>% 
      select(tagID, plotID, collectDate, recapture, scientificName, 
             identificationQualifier, hindfootLength, weight, 
             lifeStage, sex, testes, nipples, pregnancyStatus, 
             vagina, fate, remarks)
    
    tagID$collectDate<- substr(tagID$collectDate,1,10)
    
    col_num<- tagID %>% 
      group_by(scientificName) %>% 
      count()
    
    tagID$collectDate <- as.Date(tagID$collectDate)
    tagID$weight <- as.numeric(tagID$weight)
    tagID$hindfootLength <- as.numeric(tagID$hindfootLength)
    
    nb.cols <- nrow(col_num)
    mycolors <- colorRampPalette(brewer.pal(8, "Set1"))(nb.cols)
    
    d.plot<- plot_ly() # plotting first analyte selcetion from SWC data
    d.plot<- d.plot %>% add_trace(data=tagID,
                                  type='scatter',
                                  mode='markers+lines',
                                  x=~collectDate,
                                  y=~weight,
                                  name=paste0("Weight (g)"),
                                  color=I("Blue"),
                                  alpha = .9,
                                  text=~paste0("Collect Date: ", collectDate, '\n',
                                               'weight: ', weight), alpha=.6, 
                                  hoverinfo='text', color=I("Blue")

    )%>%
      layout(title='MesoMeasurements Through Time',yaxis=list(title='Weight (g)', color="blue"),xaxis=list(title='Collect Date'))
    
    # defining another y-axis
    ay <- list(
      tickfont = list(color = "red"),
      color = "red",
      overlaying = "y",
      side = "right",
      title = "HindFootLength (mm)"
    )
    
    d.plot<- d.plot%>% add_trace(data=tagID, x=~collectDate,y=~hindfootLength, yaxis = 'y2', name=paste0("HindFootLength (mm)"),
                                 mode='lines+markers', type='scatter', color=I("Red"),
                                 text=~paste0("Collect Date: ", collectDate, '\n',
                                              'hindfootLength: ', hindfootLength, '\n'), alpha=.5, 
                                 hoverinfo='text'
                                 )
    # making look nicer
    d.plot<- d.plot%>% layout(
      title = "Meso Mesurements", yaxis2 = ay,
      xaxis = list(title="Collect Date", color = "black"))
    
  })
  
  output$plot2 <- renderPlotly({
    data.raw <- site_select()
    ID<- ID_select()
    site<- site_select()
    
    if (is.null(ID))
      return(NULL) 
    if (is.null(data.raw))
      return(NULL) 
    
    data1<- data.raw %>% 
      filter(tagID == ID)%>%
      group_by(trapCoordinate,plotID,tagID) %>%
      summarise(count = n())
    
    y<- matrix(data=0, nrow = 10,ncol = 10)
    colnames(y)<- c("A", "B", "C", "D", "E", "F", "G", "H", "J", "K")
    num<- c(1:10)
    
    x=1
    while(x<12){
      i=1
      while (i<11) {
        data2<- data1%>%
          filter(trapCoordinate == paste0(LETTERS[x],num[i]))
        if (nrow(data2)>0){
          y[i,x]<- data2$count
        }
        i=i+1
      }
      x=x+1
    }
    
    data_melt<- melt(y)
    data_melt<- data_melt %>% 
      mutate(captures = value)
    #View(data_melt)
    #View(data1)
    ggp<-ggplot(data_melt, aes(X2,X1))+
      geom_tile(aes(fill=captures))+
      scale_y_discrete('Trap Number', limits= factor(c(1:10)))+
      scale_x_discrete('Trap Letter', position= 'top')+
      scale_fill_gradientn(colours=rev(rainbow(100, start=0, end=0.19)))+
      ggtitle(paste0("Capture Heat Map at ",data1$plotID[1]))
    ggplotly(ggp, text =~paste0("Captures: ",data_melt$captures, '\n',"PlotID: ",data1$plotID, '\n', "TrapCoordinate: ",data1$trapCoordinate), hoverinfo='text')
    ##Custom hover text does not work for now
  })
  
  output$plot3 <- renderPlotly({
    data.raw <- site_select()
    ID<- ID_select()
    
    if (is.null(ID))
      return(NULL) 
    if (is.null(data.raw))
      return(NULL)
    
    data1<- data.raw %>% 
      filter(tagID == ID)%>%
      group_by(trapCoordinate,plotID,tagID) %>%
      summarise(count = n())
    
    y<- matrix(data=0, nrow = 10,ncol = 10)
    colnames(y)<- c("A", "B", "C", "D", "E", "F", "G", "H", "J", "K")
    num<- c(1:10)
    
    x=1
    while(x<12){
      i=1
      while (i<11) {
        data2<- data1%>%
          filter(trapCoordinate == paste0(LETTERS[x],num[i]))
        if (nrow(data2)>0){
          y[i,x]<- data2$count
        }
        i=i+1
      }
      x=x+1
    }
    #rm(y2)
    y2<- exp(y)*2
    #View(y2)      
    x=1
    while (x < 10) {
      i=1
      while(i < 10) {
        if(y2[i,x] >= 5) {
          y2[i,x] <- y2[i,x]+3
          y2[i-1,x]<- y2[i-1,x]+1.1
          y2[i+1,x]<- y2[i+1,x]+1.1
          y2[i,1+x]<- y2[i,1+x]+1.1
          y2[i,1-x]<- y2[i,1-x]+1.1
          
        }
        i=i+1
      }
      x=x+1 
    }
    
    x=1
    while (x < 10) {
      i=1
      while(i < 10) {
        if(y2[i,x] >= 14) {
          y2[i,x] <- y2[i,x]+6
          y2[i-1,x]<- y2[i-1,x]+1.8
          y2[i+1,x]<- y2[i+1,x]+1.8
          y2[i,1+x]<- y2[i,1+x]+1.8
          y2[i,1-x]<- y2[i,1-x]+1.8
          
        }
        i=i+1
      }
      x=x+1 
    }     
    
    x=1
    while (x < 10) {
      i=1
      while(i < 10) {
        if(y2[i,x] >= 36) {
          y2[i,x] <- y2[i,x]+9
          y2[i-1,x]<- y2[i-1,x]+2.5
          y2[i+1,x]<- y2[i+1,x]+2.5
          y2[i,1+x]<- y2[i,1+x]+2.5
          y2[i,1-x]<- y2[i,1-x]+2.5
          
        }
        i=i+1
      }
      x=x+1 
    }     
    #----------
    #View(y2)
    data_melt2<- melt(y2)
    data_melt2<- data_melt2 %>% 
      mutate(hot_spots = value)
    
    ggp2<-ggplot(data_melt2, aes(X2,X1))+
      geom_tile (aes(fill=hot_spots))
    ggp2<- ggp2+scale_y_discrete('Trap Number', limits= factor(c(1:10)))
    ggp2<- ggp2+scale_x_discrete('Trap Letter')
    ggp2<- ggp2+scale_fill_gradientn(colours=rev(rainbow(100, start=0, end=.6)))+
      ggtitle(paste0("Hot Spot Heat Map at ", data1$plotID[1]))
    ggplotly(ggp2)
  })
}


