## NEON Mammal History App##
# Will look into NEON database and find tagID if it exists on the selected site in the app
# Will print out what tag selected, history of captures on NEON database, and plot when it
# was caught (collection date) vs where on the plot it was captured (trap coordinate)
# If MAM history and plot has no info = could not find that tagID at selected site on NEON database
# Must enter domain # and tagID in correct format, leading w/ domain followed by tag
# identification number [D##.tagID#] {Ex: 'club foot' at JORN - R2861}


#---user interface--------------------------------------------------

ui <- fluidPage(theme = shinytheme("paper"),collapsable = TRUE,
  #titlePanel("NEON Mammal History App"),
  tags$head(HTML("<link
                   href='http://fonts.googleapis.com/css?family=Jura'
                   rel='stylesheet' type='text/css'>")),
  
  h2("NEON Mammal History App", style= "font-family: 'Jura';
       color: darkturquoise; font-size: 46px;"),
  p("Select Site, Date Range and Process - Then select Tag from Top Capture List to Process Ind Species:",
    style= "font-family: 'Jura'; color: dark blue; font-size: 12px;"),
  p("Data Only Encompasses Mammal Data Published on NEON Data Portal. Last App Update: 2022-07-14",
    style= "font-family: 'Jura'; color: dark blue; font-size: 12px;"),
  p("Contact tsgilbert@arizona.edu with feedback, errors, or additional app features requests",
    style= "font-family: 'Jura'; color: turquoise; font-size: 12px;"),
  
  sidebarLayout(
    shinydashboard::box(width = 2, status = "info",
      selectInput("Select", "Please select site(s):"
      , width = "100%", choices = 
      c("SRER", "JORN", "BART", "HARV", "BLAN", "SCBI",
      "SERC", "DSNY", "JERC", "OSBS", "GUAN", "LAJA", "STEI",
      "TREE", "UNDE", "KONA", "KONZ", "UKFS", "GRSM", "MLBS",
      "ORNL", "DELA", "LENO", "TALL", "DCFS", "NOGP", "WOOD",
      "CPER", "RMNP", "STER", "CLBJ", "OAES", "YELL", "MOAB",
      "NIWO", "JORN", "SRER", "ONAQ", "ABBY", "WREF", "SJER",
      "SOAP", "TEAK", "BARR", "TOOL", "BONA", "DEJU", "HEAL"),
      selected = F, multiple = T),
        dateRangeInput(width = "100%", "dateRange",
        label = "Select Date Range (YYYY-MM)",
        format = "yyyy-mm", start = Sys.Date() - (2000),
        end = Sys.Date() - 365, startview = "year"),
        submitButton("Process Picks", icon("globe-americas"),
        width = "100%"),
      # type in tagID looking for and will save as variable
      textInput("SelectID", "Type in tag of individual species:",
      value = "", placeholder = "R2094", width = "100%"),
      submitButton("Process Tag", icon("tag"), width = "100%"),
    ),
    
   
    
    shinydashboard::box(

      width = 10, status = "primary",
      title = "MAM STATS - NEON Small Mammal Capture History ",
      tabsetPanel(
        footer = 'Select a site, date range... then select an individual tagID and investigate...',
        
        fluidRow(
          infoBox(width = 3, "Shiny version", "0.12", icon= icon("desktop")),
          infoBoxOutput(width = 3, "total")),
        
 

        # downloadButton('downloadmap', label = "Save Map")),

        tabPanel(
          "Top Captured Ind.",
          withSpinner(dataTableOutput("FitSelect"),
            proxy.height = "150px",
            image.height = "150px",
            image = "rat-72.gif"
          )
        ),
        tabPanel(
          "Ind. Cap Histroy",
          withSpinner(dataTableOutput("NEONDataSet1"),
            proxy.height = "150px",
            image.height = "150px",
            image = "rat-72.gif"
          )
        ),
        tabPanel(
          "Raw Cap Data",
          withSpinner(dataTableOutput("data1"),
            proxy.height = "150px",
            image.height = "150px",
            image = "rat-72.gif"
          )
        ),
        tabPanel(
          "Life History Measurements",
          withSpinner(plotlyOutput("plot1"),
            proxy.height = "150px",
            image.height = "150px",
            image = "rat-72.gif"
          ),
          ## adding violin plot
          plotlyOutput("Vplot")
        ),
        tabPanel(
          "Trap Coord Heat Map",
          withSpinner(plotlyOutput("plot2"),
            proxy.height = "150px",
            image.height = "150px",
            image = "rat-72.gif"
          )
        ),
        tabPanel(
          "Intensity Heat Map",
          withSpinner(plotlyOutput("plot3"),
            proxy.height = "150px",
            image.height = "150px",
            image = "rat-72.gif"
          )
        ),
        tabPanel(
          "Captures Per Plot",
          withSpinner(plotlyOutput("DataSet1"),
            proxy.height = "150px",
            image.height = "150px",
            image = "rat-57.gif"
          )
        ),
        tabPanel("Map View of All Captured by Species Between Selected Date Range",
                 sliderInput("rad_size", "Adj Capture Radius Size on Map:",
                             min = .1, max = 2,
                             value = 1, step = .1, width = "25%"
                 ),
                 selectInput("view", "Select map view:",
                             width = "85%", choices = c("Esri.WorldImagery",
                                                        "TomTom.Hybrid", "Stamen.TopOSMRelief")),
                 submitButton("Reload Map..."),
                 withSpinner(leafletOutput("map", width = "85%", height = "700px"),
                             proxy.height = "150px",
                             image.height = "150px",
                             image = "rat1.gif"
                 ),
                 style = "height:500px;overflow-y:scroll"
        )#,
        
        # ## testing
        # tabPanel("Map View of Ind tagID Captures Between Selected Date Range",
        #          sliderInput("rad_size", "Adj Capture Radius Size on Map:",
        #                      min = .1, max = 2,
        #                      value = 1, step = .1, width = "25%"
        #          ),
        #          selectInput("view", "Select map view:",
        #                      width = "85%", choices = c("Esri.WorldImagery",
        #                                                 "TomTom.Hybrid", "Stamen.TopOSMRelief")),
        #          submitButton("Reload Map..."),
        #          withSpinner(leafletOutput("map2", width = "85%", height = "700px"),
        #                      proxy.height = "150px",
        #                      image.height = "150px",
        #                      image = "rat1.gif"
        #          ),
        #          style = "height:500px;overflow-y:scroll"
        # )
        
        
        
      )
    )
  )
)
