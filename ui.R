# NEON Mammal History App##
# Will look into NEON database and find tagID if it exists on the selected site in the app
# Will print out what tag selected, history of captures on NEON database, and plot when it
# was caught (collection date) vs where on the plot it was captured (trap coordinate)
# If MAM history and plot has no info = could not find that tagID at selected site on NEON database
# Must enter domain # and tagID in correct format, leading w/ domain followed by tag
# identification number [D##.tagID#] {Ex: 'club foot' at JORN - R2861}


#---user interface--------------------------------------------------

ui <- fluidPage(
  ## getting js style for www/confirm.js
  tags$head(
    tags$script(src = "https://cdn.jsdelivr.net/npm/sweetalert2@11.10.0/dist/sweetalert2.all.min.js"),
    tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/sweetalert2@11.10.0/dist/sweetalert2.min.css"),
    tags$script(src = "confirm.js")
  ),
  br(),
  theme = shinytheme("slate"),
  
  ## Valid themes are:
  ## cerulean, cosmo, cyborg, darkly, flatly, journal, lumen, paper,
  ## readable, sandstone, simplex, slate, spacelab, superhero, united, yeti

  tags$head(HTML("<link
                   href='http://fonts.googleapis.com/css?family=Jura'
                   rel='stylesheet' type='text/css'>")),
  
  div(
    style = "padding: 6px 12px 0 12px;",
    h2(
      icon("paw"), " NEON Small Mammal Tracker ",
      tags$span("(unofficial)",
                style = "font-size: 16px; color: #9aa4ad; font-weight: normal; letter-spacing: 1px;"),
      style = "font-family: 'Jura'; color: teal; font-size: 40px; font-weight: bold; margin-bottom: 0;"
    ),
    p("Chase individual rodents across NEON sites — rank captures, dig into measurements, and map where they showed up.",
      style = "font-family: 'Jura'; color: #b8c2cc; font-size: 14px; margin-top: 4px;")
  ),

  shiny::actionButton(inputId = "help", "What do I do?", icon = icon("circle-question")),

  sidebarLayout(
    shinydashboard::box(
      br(),
      width = 3, status = "info",
      selectInput("Select", "Please select site(s):",
                  width = "100%", choices =
                    c("",
                      "SRER", "JORN", "BART", "HARV", "BLAN", "SCBI",
                      "SERC", "DSNY", "JERC", "OSBS", "GUAN", "LAJA", "STEI",
                      "TREE", "UNDE", "KONA", "KONZ", "UKFS", "GRSM", "MLBS",
                      "ORNL", "DELA", "LENO", "TALL", "DCFS", "NOGP", "WOOD",
                      "CPER", "RMNP", "STER", "CLBJ", "OAES", "YELL", "MOAB",
                      "NIWO", "JORN", "ONAQ", "ABBY", "WREF", "SJER",
                      "SOAP", "TEAK", "BARR", "TOOL", "BONA", "DEJU", "HEAL"
                    ),
                  selected = F, multiple = F
      ),
      dateRangeInput(
        width = "100%", "dateRange",
        label = "Select Date Range:",
        format = "yyyy-mm", start = Sys.Date() - (2000),
        end = Sys.Date() - 365, startview = "year"
      ),
      shiny::actionButton("loadBtn", "Load...", icon = icon("globe-americas"),
                          width = "100%", class = "btn-primary"),
      useShinyjs(), ## to hide...
      ## place holder to insert species links to later after selection
      shiny::actionButton(inputId = "WebLinks", "Welcome!")
    ),
    shinydashboard::box(
      width = 9,
      # status = "primary",
      # title = "MAM STATS - NEON Small Mammal Capture History ",
      tabsetPanel(id = "inTabset",
                  
        #footer = "Select a site, date range... then select an individual tagID and investigate...",
        
        #fluidRow(
          #infoBox(width = 3, "data1", icon= icon("desktop")),
          # valueBox(value = "data1", subtitle = ""),
          # valueBox(value= "data2", subtitle = ""),
          # valueBox(value = "data3", subtitle = "")
        #),
        
        # downloadButton('downloadmap', label = "Save Map")),
        
        tabPanel(
          title = tagList(icon("trophy"), "Capture Ranks"),
          withSpinner(dataTableOutput("FitSelect"),
                      proxy.height = "150px",
                      image.height = "150px",
                      image = "rat-72.gif"
          ),
          div(id = "more",
              hr(),
              p(icon("database"), " Data sourced live from the NEON Data Portal (DP1.10072.001).",
                style = "font-family: 'Jura'; color: #6fb1d1; font-size: 12px; margin-bottom: 2px;"
              ),
              p(icon("envelope"), " Feedback, bugs, or feature requests? ",
                tags$a(href = "mailto:tsgilbert@arizona.edu", "tsgilbert@arizona.edu"),
                style = "font-family: 'Jura'; color: #b8c2cc; font-size: 12px; margin-bottom: 2px;"
              ),
              tags$a(
                class = "vgsLink",
                href = "https://github.com/tgilbert14/NEON-Small-Mammal-Tracker-App",
                target = "_blank",
                icon("github"), " View source on GitHub"
              ),
              p("")
          ),
          # bsModal(id = "clientData", title = "Client Data", trigger = "showData",
          #         verbatimTextOutput("clientdataText"))
        ),
        tabPanel(
          title = tagList(icon("clock-rotate-left"), "Cap History"),
          withSpinner(dataTableOutput("NEONDataSet1"),
                      proxy.height = "150px",
                      image.height = "150px",
                      image = "rat-72.gif"
          )
        ),
        tabPanel(
          title = tagList(icon("table"), "Raw"),
          withSpinner(dataTableOutput("data1"),
                      proxy.height = "150px",
                      image.height = "150px",
                      image = "rat-72.gif"
          )
        ),
        tabPanel(
          title = tagList(icon("ruler"), "Measurements"),
          withSpinner(plotlyOutput("plot1"),
                      proxy.height = "150px",
                      image.height = "150px",
                      image = "rat-72.gif"
          ),
          ## adding violin plot
          plotlyOutput("Vplot")
        ),
        tabPanel(
          title = tagList(icon("fire"), "Heat Map"),
          withSpinner(plotlyOutput("plot2"),
                      proxy.height = "150px",
                      image.height = "150px",
                      image = "rat-72.gif"
          )
        ),
        tabPanel(
          title = tagList(icon("fire-flame-curved"), "V2 Heat Map"),
          withSpinner(plotlyOutput("plot3"),
                      proxy.height = "150px",
                      image.height = "150px",
                      image = "rat-72.gif"
          )
        ),
        tabPanel(
          title = tagList(icon("chart-line"), "Captures Per Plot"),
          withSpinner(plotlyOutput("DataSet1"),
                      proxy.height = "150px",
                      image.height = "150px",
                      image = "rat-57.gif"
          )
        ),
        tabPanel(title = tagList(icon("map-location-dot"), "Capture Map"),
                 sliderInput("rad_size", "Adj Capture Radius Size on Map:",
                             min = .1, max = 2,
                             value = 1, step = .1, width = "25%"
                 ),
                 selectInput("view", "Select map view:",
                             width = "85%", choices = c(
                               "Esri.WorldImagery",
                               "TomTom.Hybrid", "Stamen.TopOSMRelief"
                             )
                 ),
                 shiny::actionButton("reloadMapBtn", "Reload Map...", icon = icon("rotate")),
                 withSpinner(leafletOutput("map", width = "85%", height = "700px"),
                             proxy.height = "150px",
                             image.height = "150px",
                             image = "rat1.gif"
                 ),
                 style = "height:500px;overflow-y:scroll"
        )
      )
    )
  )
)
