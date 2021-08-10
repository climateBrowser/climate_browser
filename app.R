library(shiny)
library(tidyverse)
library(readxl)
library(textreadr)
#library(climatol)
library(DT)
library(shinyWidgets)
library(sf)
library(leaflet)
library(mapview)
source("diagwl.R")
library(ggplot2)
library("RColorBrewer")

library(profmem)


dataArea <- read_xlsx("data/DataAreaFile.xlsx", sheet = 1)
areaFile <- read_xlsx("data/DataAreaFile.xlsx", sheet = 2)

# User interface ----
ui <- fluidPage(
  navbarPage("Climate Browser", 
      tabPanel("Getting Started",
        fluidRow(style = "height:400px;", align = "center",
          includeMarkdown("data/getting_started.md")
        )
      ),
      tabPanel("Upload Data",
        fluidRow(style = "height:400px;",
          column(5, align = "center", 
            h2("Choose a dataset below", style="height:80px;"),
            htmlOutput("dataTypeSelector"),
            htmlOutput("areaSelector"),
            htmlOutput("fileSelector")
          ),
          column(2, align = "center",
            h2("or")
          ),
          column(5, align = "center",
            h2("Upload your own data", style="height:80px;"),
            h4("(overrides previous selection)"),
            fileInput("file1", label="", accept = c(".csv", ".xlsx", ".rtf"), 
              buttonLabel="Upload File")     
          )
        ),
        fluidRow(column(6, align="center", offset=3,
          actionBttn("read", "Read File/Continue", color = "success"),
          checkboxInput("table_toggle", "Display table", value = TRUE),
          dataTableOutput("front_table"))
        )
        
        
      ),
        navbarMenu("Tidy Data",
          tabPanel("Select/Filter Data", 
            h4("Select columns by clicking on the table"),
            h4("Select rows with the boxes at the top of each column"),
            h4("Choose whether to accept the new dataset below"),
                                     
            checkboxInput("remove_na_vals", "Remove N/A values \n
                          (Delete all rows in the filtered data that contain missing values)", value = FALSE),
            
            DTOutput("table"),
            h3("Results table:"),
            p("Press the button to use the filtered data for analysis."),
            actionButton("done_tidying", "Use these data"),
            DTOutput("newTable")
          ),
          tabPanel("Pivot Data",
            h4("Pivoting collapses rows into columns (longer) or columns into rows (wider)."),
            actionButton("pivot_longer", "Pivot Longer"),
            # select columns
            textInput('names_to', "Names to"),
            textInput('values_to', "Values to"),
            
            actionButton("pivot_wider", "Pivot Wider"),
            selectInput("names", "Wider - first column name", list("none")),
            selectInput("values", "Wider - second column name", list("none")),
            
            DTOutput("pivotTable")
            
          )
      ),
    
      tabPanel("Diversity Indices",
        h4("This page expects your data to have a column with plot specifiers and a column with species specifiers."),
        h4("Suggested additional columns are latitude/longitude, or anything consistent in each plot."),
        h4("Use Tidy Data>Select/Filter Data to remove columns if needed"),
        selectInput("SRGeo", "Which column has the plot data?", list("none")),
        selectInput("SRSp", "Which column has the species data?", list("none")),
        checkboxGroupInput("SRDesired", "Which indices do you want?", list(
          "Species Richness" = 1, "Shannon-Wiener" = 2,
          #"Fisher Alpha" = 3, 
          "Berger & Parker Dominance" = 4, "Simpson" = 5,
          "Menhinick" = 6, "Margalef" = 7, "McIntosh" = 8 
          #"Brillouin" = 9
          #add more as desired
          )),
        actionButton("SRCalc", "Calculate"),
        DTOutput("SRTable"), #output table
        h4("If the data has previously been tidied to only include the plot and species data columns, the following data should have one row for each plot."),
        h4("Would you like to use this data for graphing and/or statistical tests?"),
        actionButton("SRUseFiltered", "Yes"),
        DTOutput("SRFiltered")
      ),
    
      tabPanel("Graph Data",
        tabsetPanel(
          tabPanel("Walter-Lieth Climate Diagram",
            sidebarPanel(
              uiOutput("stationInfo"),
              uiOutput("stationLat"),
              uiOutput("stationLong"),
              uiOutput("stationElev"),
              #textInput('stationName', 'Name or location of the climatological station'),
              #numericInput('lat', 'Station Latitude', value=1),
              #numericInput('long', 'Station Longitude', value=1),
              #numericInput('alt', "Station Altitude", value=1),
              selectInput("monthCol", "Month column name", list("none")),
              selectInput("yearCol", "Year column name", list("none")),
              selectInput("precCol", "Precipitation column name", list("none")),
              selectInput("maxTempCol", "Maximum temperature column name", list("none")),
              selectInput("minTempCol", "Mininum temperature column name", list("none")),
              prettyRadioButtons(inputId = "hem", label = "Hemisphere", 
                                choiceNames = c("Northern", "Southern"), choiceValues = c("north", "south")),
              prettyRadioButtons(inputId = "temp", label = "Temperature Unit", 
                                choiceNames = c("Celsius", "Fahrenheit"), choiceValues = c("cel", "fahr")),
              prettyRadioButtons(inputId = "prec", label = "Precipitation Unit", 
                                choiceNames = c("Millimeters", "Inches"), choiceValues = c("mm", "inch")),
              actionButton("makePlot", "Make Diagram") #TO DO: CHANGE NAME
            ), 
            mainPanel(
              plotOutput("Walter")
            )
          ),
          
          tabPanel("Geoplotting",
            h1("Map your data"),
            leafletOutput("watershedMap"),
            selectInput("colorCol", "Color data points according to:", list("none")),
            actionButton("mapColorUpdated", "Color points"),
            selectInput("mapLat", "Latitude column:", list("none")),
            selectInput("mapLong", "Longitude column:", list("none")),
            actionButton("mapLatLongUpdated", "lat/long chosen")
          ),
          
          tabPanel("Scatter Plot",
            sidebarPanel(
              selectInput("xAxisScatter", "X Axis", list("none")),
              selectInput("yAxisScatter", "Y Axis", list("none")),
              actionButton("makeScatterPlot", "Make Diagram")
            ),
            mainPanel(
              plotOutput("scatter")
            )
          ),
          
          tabPanel("Violin Plot",
            sidebarPanel(
             selectInput("xAxisViolin", "X Axis", list("none")),
             selectInput("yAxisViolin", "Y Axis", list("none")),
             actionButton("makeViolinPlot", "Make Diagram")
            ),
            mainPanel(
             plotOutput("violin")
            )
          ),
          
          tabPanel("Line Graph",
            sidebarPanel(
             selectInput("xAxisLine", "X Axis", list("none")),
             selectInput("yAxisLine", "Y Axis", list("none")),
             selectInput("groupBy", "Group By", list("none")),
             numericInput('lineSize', 'Line Thickness', value=1),
             selectInput("palette", "Color Palette", 
                         c("Greys", "Set1", "Set2", "Set3", "Pastel1", "Pastel2", "Paired", "Dark2", "Accent")),
             actionButton("makeLineGraph", "Make Diagram")
            ),
            mainPanel(
             plotOutput("line")
            )
          ),
          
          tabPanel("Boxplot",
            sidebarPanel(
              selectInput("xAxisBox", "X Axis", list("none")),
              selectInput("yAxisBox", "Y Axis", list("none")),
              actionButton("makeBoxplot", "Make Diagram")
            ),
            mainPanel(
              plotOutput("box")
            )
          ),
          
          tabPanel("Histogram",
            sidebarPanel(
             selectInput("xAxisHist", "X Axis", list("none")),
             sliderInput("histBins", "Number of bins", min = 2, max = 100, value = 20),
             actionButton("makeHistogram", "Make Diagram"),
             p("Histograms display the distribution of continuous data.")
            ),
            mainPanel(
             plotOutput("histogram")
            )
          )
        #)#,
      )
    ),
    
      navbarMenu("Statistical Tests",
        tabPanel("One-Sample T-Test",
          sidebarPanel(
            selectInput("one_t_test_var","Select Variable to Test:",list("none")),
              numericInput('diff_var', 'Null hypothesis mean (usually left at zero)', value=0),
                actionButton("perform_one_sample", "Perform Test")
          ),
          mainPanel(
            textOutput("one_sample_output")
          )
        ),
        tabPanel("Two-Sample T-Test",
          sidebarPanel(
            selectInput("two_sample_var_one","Select Variable One:",list("none")),
            selectInput("two_sample_var_two","Select Variable Two:",list("none")),
            checkboxInput("paired_or_not", "Paired", value = FALSE),
            checkboxInput("equal_variance", "Equal Variance", value = FALSE),
            actionButton("perform_two_sample", "Perform Test")
          ),
          mainPanel(
            textOutput("two_sample_output")
          )
        )
        ),     
      
      tabPanel("Learn More", 
        includeHTML("data/paper.html")
      )
             
  )
)

# Server logic ----
server <- function(input, output, session) {
  options(DT.options = list(pageLength=10, dom="tp", scrollX=TRUE))
  
  #### Reading in File and Outputting Table
  
  # TO DO: Help the server update data without reloading.
  v = reactiveValues(data = NULL, data_in_use = NULL)
  data_to_use = "raw" #use v$data for graphing by default
  
  updateGraphingOptions = function(data_in_use) { #either filtered_na_rmvd_data() or diversity_data()
    # TO DO: Could save space to save names(data_in_use) first
    # TO DO: What does updateSelectInput do?
    #p1 <- profmem({
    updateSelectInput(session, "monthCol", choices = names(data_in_use))
    updateSelectInput(session, "yearCol", choices = names(data_in_use))
    updateSelectInput(session, "precCol", choices = names(data_in_use))
    updateSelectInput(session, "maxTempCol", choices = names(data_in_use))
    updateSelectInput(session, "minTempCol", choices = names(data_in_use))
    #geospatial
    updateSelectInput(session, "colorCol", choices = names(data_in_use)) 
    updateSelectInput(session, "mapLat", choices = names(data_in_use))
    updateSelectInput(session, "mapLong", choices = names(data_in_use))
    # Violin plot
    updateSelectInput(session, "yAxisViolin", choices = names(data_in_use))
    updateSelectInput(session, "xAxisViolin", choices = names(data_in_use))
    # Scatter plot
    updateSelectInput(session, "xAxisScatter", choices = names(data_in_use))
    updateSelectInput(session, "yAxisScatter", choices = names(data_in_use))
    # Line graph
    updateSelectInput(session, "xAxisLine", choices = names(data_in_use))
    updateSelectInput(session, "yAxisLine", choices = names(data_in_use))
    updateSelectInput(session, "groupBy", choices = c("None", names(data_in_use)))
    # Boxplot
    updateSelectInput(session, "xAxisBox", choices = names(data_in_use))
    updateSelectInput(session, "yAxisBox", choices = names(data_in_use))
    # Histogram
    updateSelectInput(session, "xAxisHist", choices = names(data_in_use))
    updateSelectInput(session, "yAxisHist", choices = names(data_in_use))
    # T-Tests
    updateSelectInput(session, "one_t_test_var", choices = c("None", names(data_in_use)))
    updateSelectInput(session, "two_sample_var_one", choices = c("None", names(data_in_use)))
    updateSelectInput(session, "two_sample_var_two", choices = c("None", names(data_in_use)))
    #})
    #print(p1)
    #print("Update Graphing Options")
  }
  
  
  updateDiversityOptions = function(data_in_use) {
    # Species richness
    updateSelectInput(session, "SRGeo", choices = names(data_in_use))
    updateSelectInput(session, "SRSp", choices = names(data_in_use))
  }
  
  # Selecting the file from preloaded data --------
  output$dataTypeSelector = renderUI({
    selectInput(inputId = "dataType", label = "Data Type:", 
                choices = as.character(unique(dataArea$DataType)))
  })
  
  output$areaSelector = renderUI({
    areaOptions = dataArea[dataArea$DataType == input$dataType, "Area"]
    selectInput(inputId = "area", label = "General Area:", choices = unique(areaOptions))
  })
  
  output$fileSelector = renderUI({
    fileOptions = areaFile[areaFile$Area == input$area, "Station"]
    selectInput(inputId = "dataSource", label = "File:", choices = unique(fileOptions))
  })
  
  
  # Reading in the file ---------
  preLoaded <- FALSE
  
  observeEvent(input$read, {
    #print("READ")
    file = input$file1$datapath
    ext <- tools::file_ext(file)
    
    if (is.null(file)) {
      preLoaded <- TRUE
      fileName = as.character(areaFile[areaFile$Station == input$dataSource, "File"])
      file = paste("data", fileName, sep = "/")
      ext = tools::file_ext(file)
    }
  
    
    req(file)
    #To do: Figure out how to put out a better warning message if they haven't input a file
    #validate(need(ext == "rtf", "Please upload an rtf file"))
    p2 <- profmem({
    if (ext == "xlsx"){
      v$data = read_excel(file)
    }

    else if (ext == "csv"){
      v$data = read_csv(file)
    }
    })
    print(p2)
    print("Load file")
    
    postRead()
  })
  
  # After reading in the file, display it (if the user chooses to)
  # Also update the available data
  postRead = function(){

    output$front_table = renderDT({
      if(input$table_toggle){
        v$data
      }
    })
    v$data_in_use = v$data
    updateGraphingOptions(v$data)
    updateDiversityOptions(v$data)
    updateSelectInput(session, "names", choices = names(v$data))
    updateSelectInput(session, "values", choices = names(v$data))
  }

  
  #### Tidying stuff
  
  #### Select/Filter
  output$table = renderDT(v$data, selection = list(target = "column"), 
                          filter = "top", server = TRUE, options = list(dom="ftp"))
  
  # TO DO: Seems like we shouldn't need three different steps?
  # Esp. b/c all three of these are being saved in memory. That takes up a lot of memory.
  p3 <- profmem({
  new_data = reactive({ select(v$data, sort(input$table_columns_selected)) })
  })
  print(p3)
  print("New data loaded")
  p4 <- profmem({
  filtered_data = reactive({ new_data()[input$table_rows_all,] })
  })
  print(p4)
  print("Filtered data loaded")
  p5 <- profmem({
  filtered_na_rmvd_data = reactive({ 
    if (input$remove_na_vals){
      drop_na(filtered_data())
    }
    else{
      filtered_data()
    }
  })
  })
  print(p5)
  print("Filtered NA Removed Data")
  
  output$newTable = renderDT({
    filtered_na_rmvd_data()
  })
  
  observeEvent(input$done_tidying, {
    updateGraphingOptions(filtered_na_rmvd_data())
    updateDiversityOptions(filtered_na_rmvd_data())
    
    v$data_in_use = filtered_na_rmvd_data()
    data_to_use = "filtered"
  }) # TODO: don't do this, update Options based on page not if done tidying (could skip tidying)
  
  output$tidy_data <- renderUI({
    updateSelectInput(session, "names", choices = names(v$data))
    updateSelectInput(session, "values", choices = names(v$data))
  })
  
  #### End Select/Filter
  
  #### Pivot
  
  output$pivotTable = renderDT(v$data, selection = list(target = "column"), 
                               server = TRUE, options = list(dom="tp"))
  
  #longer:
  observeEvent(input$pivot_longer, {
    v$data = pivot_longer(v$data, input$pivotTable_columns_selected, names_to=input$names_to, values_to=input$values_to)
    
    updateSelectInput(session, "names", choices = names(v$data))
    updateSelectInput(session, "values", choices = names(v$data))
  })
  
  #wider:
  observeEvent(input$pivot_wider, {
    v$data = pivot_wider(v$data, names_from=input$names, values_from=input$values)
    updateSelectInput(session, "names", choices = names(v$data))
    updateSelectInput(session, "values", choices = names(v$data))
  })
  
  
  #### end Tidying stuff
  
  #### Start Species Richness
  
  calcSpeciesRichness = function(data_in, geo, sp) {
    
    data_in %<>%
      group_by_(geo) %>%
      mutate_(sr = lazyeval::interp(~n_distinct(val), val=as.name(sp)))
    return(data_in)
  }
  
  calcShannonWiener = function(data_in, geo, sp) {
    p9 <- profmem({
    data_copy = data_in
    data_in %<>%
      group_by_(as.name(geo), as.name(sp)) %>%
      summarise(count_sw = n()) %>%
      mutate(total = sum(count_sw)) %>%
      summarise(SWIndex = -sum(count_sw / total * log(count_sw / total)))
    data_in = full_join(data_copy, data_in, by=str(geo))
    })
    print(p9)
    print("Diversity index")
    return(data_in)
  }
  
  calcFisherAlpha = function(data_in, geo, sp) {
    
  }
  
  calcBergerParker = function(data_in, geo, sp) {
    data_in %<>%
      group_by_(as.name(geo), as.name(sp)) %>%
      mutate(ni = n()) %>%
      group_by_(as.name(geo)) %>%
      mutate(BergerParker = max(ni) / sum(ni)) %>%
      select(-ni)
    return(data_in)
    
  }
  
  calcSimpson = function(data_in, geo, sp) {
    data_in %<>%
      group_by_(as.name(geo), as.name(sp)) %>%
      mutate(count_sms = n()) %>%
      group_by_(as.name(geo)) %>%
      mutate(simpson = 1 / sum((count_sms / sum(count_sms)) ^ 2)) %>%
      select(-count_sms)
    return(data_in)
  }
  
  calcMenhinick = function(data_in, geo, sp) {
    if ("sr" %in% names(data_in)) {
      data_in %<>%
        group_by_(as.name(geo)) %>%
        mutate(Menhinick = sr / sqrt(n()))
    } 
    else {
      data_in %<>%
        group_by_(as.name(geo)) %>%
        mutate_(sr = lazyeval::interp(~n_distinct(val), val=as.name(sp))) %>%
        mutate(Menhinick = sr / sqrt(n()))
    }
    return(data_in)
  }
  
  calcMargalef = function(data_in, geo, sp) {
    if ("sr" %in% names(data_in)) {
      data_in %<>%
        group_by_(as.name(geo)) %>%
        mutate(Margalef = (sr - 1) / log(n()))
    } 
    else {
      data_in %<>%
        group_by_(as.name(geo)) %>%
        mutate_(sr = lazyeval::interp(~n_distinct(val), val=as.name(sp))) %>%
        mutate(Margalef = (sr - 1) / log(n()))
    }
    return(data_in)
  }
  
  calcMcIntosh = function(data_in, geo, sp) {
    data_in %<>%
      group_by_(as.name(geo)) %>%
      mutate(N = n()) %>%
      group_by_(as.name(geo), as.name(sp)) %>%
      mutate(ni = n()) %>%
      group_by_(as.name(geo)) %>%
      mutate(u = sqrt(sum(ni))) %>%
      mutate(McIntosh = (N - u) / (N - sqrt(N))) %>%
      select(-N, -u, -ni)
    return(data_in)
  }
  
  calcBrillouin = function(data_in, geo, sp) {
    data_in %<>%
      group_by_(as.name(geo), as.name(sp)) %>%
      mutate(b_ni = n()) %>%
      mutate(b_bottom = factorial)
      group_by_(as.name(geo)) %>%
      mutate(Brillouin = 1 / sum(b_ni) * log10(factorial(sum(b_ni))))
  }
  
  #add more below
  
  
  
  # tempSR = eventReactive(input$SRCalc, { 
  #   sr_data = v$data_in_use
  #   #sr_data_copy = sr_data
  #   sr_data %<>%
  #     group_by_(input$SRGeo) %>%
  #     mutate_(sr = lazyeval::interp(~n_distinct(val), val=as.name(input$SRSp)))
  #   
  #   #full_join(sr_data, sr_data_copy, by=str(input$SRGeo))
  #     
  # })
  
  
  # observeEvent(input$SRCalc, {
  #   v$data_in_use = tempSR()
  #   print(tempSR)
  #   print(tempSR())
  #   
  #   updateGraphingOptions(v$data_in_use)
  #   print(v$data_in_use)
  #   print("IELELQI")
  #   data_to_use <<- "diversity"
  # })
  
  observeEvent(input$SRCalc, {
    #print(input$SRDesired)
    if ("1" %in% input$SRDesired) {
      #print("PIE")
      v$data_in_use = calcSpeciesRichness(v$data_in_use, input$SRGeo, input$SRSp)
    }
    if ("2" %in% input$SRDesired) {
      #print("HIE")
      v$data_in_use = calcShannonWiener(v$data_in_use, input$SRGeo, input$SRSp)
    }
    if ("4" %in% input$SRDesired) {
      v$data_in_use = calcBergerParker(v$data_in_use, input$SRGeo, input$SRSp)
    }
    if ("5" %in% input$SRDesired) {
      v$data_in_use = calcSimpson(v$data_in_use, input$SRGeo, input$SRSp)
    }
    if ("6" %in% input$SRDesired) {
      v$data_in_use = calcMenhinick(v$data_in_use, input$SRGeo, input$SRSp)
    }
    if ("7" %in% input$SRDesired) {
      v$data_in_use = calcMargalef(v$data_in_use, input$SRGeo, input$SRSp)
    }
    if ("8" %in% input$SRDesired) {
      v$data_in_use = calcMcIntosh(v$data_in_use, input$SRGeo, input$SRSp)
    }
    if ("9" %in% input$SRDesired) {
      v$data_in_use = calcBrillouin(v$data_in_use, input$SRGeo, input$SRSp)
    }
    
    #add more for different indices
  })
  
  output$SRTable = renderDT(
    v$data_in_use
  )
  
  diversity_filtered = reactive({
    v$data_in_use %>%
      select(-as.name(input$SRSp)) %>%
      unique()
  })
  
  output$SRFiltered = renderDT(
    diversity_filtered()
  )
  
  observeEvent(input$SRUseFiltered, {
    v$data_in_use = diversity_filtered()
    data_to_use <<- "diversity"
    updateGraphingOptions(v$data_in_use)
    #print(v$data_in_use)
  }) 
  
  #### End Species Richness
  
  
  #### Shared Graphing
  
  graphing_data = reactive({
    if (data_to_use == "raw") {
      v$data
    }
    else if (data_to_use == "filtered") {
      filtered_na_rmvd_data()
    }
    else if (data_to_use == "diversity") {
      v$data_in_use   #Maximum sloppiness
    }
  })
  #### End Shared Graphing
  
  
  #### Walter-Lieth stuff---------
  
  # Auto fill based on previous input
  output$stationInfo <- renderUI({
    stationName <- NULL
    if (is.null(input$file1$datapath)){
      stationName <- input$dataSource
    }
    textInput('stationName', 'Name or location of the climatological station', 
              value=stationName)
  })
  
  output$stationLat <- renderUI({
    latitude <- NULL
    if (is.null(input$file1$datapath)){
      latitude <- as.numeric(areaFile[areaFile$Station == input$dataSource, "Latitude"])
    }
    numericInput('lat', 'Station Latitude', value=latitude)
  })
  
  output$stationLong <- renderUI({
    longitude <- NULL
    if (is.null(input$file1$datapath)){
      longitude <- as.numeric(areaFile[areaFile$Station == input$dataSource, "Longitude"])
    }
    numericInput('long', 'Station Longitude', value=longitude)
  })
  
  output$stationElev <- renderUI({
    elevation <- NULL
    if (is.null(input$file1$datapath)){
      elevation <- as.numeric(areaFile[areaFile$Station == input$dataSource, "Elevation"])
    }
    numericInput('elev', 'Station Elevation', value=elevation)
  })
  
  # Make the plot
  observeEvent(input$makePlot, {
    output$Walter = renderPlot({
      data_wl = graphing_data()
      
      
      stationName = input$stationName
      latitude = input$lat
      longitude = input$long
      altitude = input$elev
      monthCol = input$monthCol
      yearCol = input$yearCol
      precCol = input$precCol
      maxTempCol = input$maxTempCol
      minTempCol = input$minTempCol
      
      # Get range of years
      startYear = min(data_wl[[yearCol]])
      endYear = max(data_wl[[yearCol]])
      
      data_wl[[precCol]] = as.numeric(as.character(data_wl[[precCol]]))
      data_wl[[maxTempCol]] = as.numeric(as.character(data_wl[[maxTempCol]]))
      data_wl[[minTempCol]] = as.numeric(as.character(data_wl[[minTempCol]]))
      if (all(typeof(data_wl[[monthCol]]) == "character")) {
        print("month is chars")
        data_wl[[monthCol]] <- factor(data_wl[[monthCol]], levels = month.abb) 
      }
      data_wl = rename(data_wl, Month = monthCol, Year = yearCol, Prec = precCol, Tmax = maxTempCol, Tmin = minTempCol)
      
      print(input$prec)
      if (input$prec == "inches") {
        data_wl = mutate(data_wl, Prec = Prec * 25.4)
      }
      
      if (input$temp == "fahr"){
        data_wl = mutate(data_wl, Tmax = (Tmax - 32) * (5 / 9))
        data_wl = mutate(data_wl, Tmin = (Tmin - 32) * (5 / 9))
      }
      
      if (input$hem == "south"){
        shem_wl = TRUE
      }
      else{
        shem_wl = FALSE
      }

      data_wl = select(data_wl, Year, Month, Prec, Tmin, Tmax) %>%
        group_by(Year, Month) %>%
        summarize(rainfall = sum(Prec, na.rm = TRUE), 
                  meanMax = mean(Tmax, na.rm = TRUE), 
                  meanMin = mean(Tmin, na.rm = TRUE), 
                  minMin = min(Tmin, na.rm = TRUE)) %>%
        group_by(Month) %>%
        summarize(rainfall = mean(rainfall), 
                  meanMax = mean(meanMax), 
                  meanMin = mean(meanMin), 
                  minMin = min(minMin))
      
      data_matrix = data.matrix(data_wl)
      data_matrix = t(data_matrix)
      data_matrix = data_matrix[-1, ]

      # Set the title of the graph
      if (str_length(stationName) > 0){
        stationInfo = paste0(stationName, " [Lat: ", latitude, ", Lon: ", longitude,
                             "] (", altitude, ")")
      }
      else{
        stationInfo = ""
      }

      yearRange = ""
      if (startYear == endYear){
        yearRange = startYear
      }
      else{
        yearRange = paste0(startYear, " to ", endYear)
      }
      
      
      diagwl(data_matrix, est=stationInfo, alt=NA, per=yearRange, margen=c(4, 4, 5, 4), mlab="",
             pcol="#005ac8", tcol="#e81800", pfcol="#79e6e8", sfcol="#09a0d1", shem=shem_wl,
             p3line=FALSE)
      
    },
    width = 700,
    height = 700
    )
  })
  
  #### END WALTER-LIETH
  
  
  
  
  #### Start geospatial
  mapLatCol = NULL
  mapLongCol = NULL
  mapColorCol = NULL
  
  
  redraw_map = function(latCol, longCol, colCol) {
    # It would take some work, but if we could find some way to intuit which columns are Lat and Long,
    # we could keep this code.
    # The means used below is too simplistic and will lead to errors most of the time, 
    # Unless we cheat by editing all the pre-loaded data so the columns are exactly the words below.
    # if (is.null(latCol) & is.null(longCol) & is.null(colCol)) {
    #   output$watershedMap <- renderLeaflet({
    #     vals = reactiveValues(
    #       m = mapview(graphing_data(), xcol = "Longitude", ycol = "Latitude", crs = 4269, grid = FALSE)
    #     )
    #     vals$m@map
    #   })
    # }
    # 
    # else if (is.null(latCol) & is.null(longCol)) {
    #   #output$watershedMap <- renderLeaflet({
    #   #  cvals = reactiveValues(
    #   #    cm = mapview(graphing_data(), xcol = "Longitude", ycol = "Latitude", zcol = colCol, crs = 4269, grid = FALSE)
    #   #  )
    #   #  cvals$cm@map
    #   #})
    # }
    if (is.null(latCol) | is.null(longCol)) {
      output$watershedMap <- renderLeaflet({
        vals = reactiveValues(
          m = mapview()
         )
        vals$m@map
      })
    }
    
    else if (is.null(colCol)) {
      output$watershedMap <- renderLeaflet({
        llvals = reactiveValues(
          llm = mapview(graphing_data(), xcol = longCol, ycol = latCol, crs = 4269, grid = FALSE)
        )
        llvals$llm@map
      })
    }
    
    #if (!is.null(mapLatCol) & !is.null(mapLongCol) & !is.null(mapColorCol)) {
    else{
      output$watershedMap <- renderLeaflet({
        llcvals = reactiveValues(
          llcm = mapview(graphing_data(), xcol = longCol, ycol = latCol, zcol = colCol, crs = 4269, grid = FALSE)
        )
        llcvals$llcm@map
      })
    }
  }
  
  
  redraw_map(mapLatCol, mapLongCol, mapColorCol)
  observeEvent(input$mapColorUpdated, {
    print(graphing_data())
    print(data_to_use)
    mapColorCol <<- input$colorCol
    redraw_map(mapLatCol, mapLongCol, mapColorCol)
  })
  observeEvent(input$mapLatLongUpdated, {
    mapLatCol <<- input$mapLat
    mapLongCol <<- input$mapLong
    redraw_map(mapLatCol, mapLongCol, mapColorCol)
  })
  
  
  
  #### END GEOSPATIAL
  
  #### Scatter Plot

  
  observeEvent(input$makeScatterPlot, {
  
    output$scatter = renderPlot({
      scatter_data = graphing_data()
      
      xAxis = input$xAxisScatter
      yAxis = input$yAxisScatter
      scatter_data[[xAxis]] = as.numeric(scatter_data[[xAxis]])
      scatter_data[[yAxis]] = as.numeric(scatter_data[[yAxis]])
      ggplot(scatter_data) + 
        geom_point(aes_(x = as.name(xAxis), y = as.name(yAxis))) +
        theme_bw()
    }#,
    #width = 500,
    #height = 500
    )
  })

#### End Scatter Plot

#### Violin Plot

  observeEvent(input$makeViolinPlot, {
    violin_data = graphing_data()
    violin_data[[input$xAxisViolin]] = as.factor(violin_data[[input$xAxisViolin]])
    
    output$violin = renderPlot({
      ggplot(data = violin_data, aes_q(x = as.name(input$xAxisViolin), y = as.name(input$yAxisViolin))) +
        geom_violin() +
        theme_bw()
    })
  })


#### End Violin Plot
  
#### Line Plot
  observeEvent(input$makeLineGraph, {
    
    output$line = renderPlot({
      line_data = graphing_data()
      
      xAxis = input$xAxisLine
      yAxis = input$yAxisLine
      if(all(!is.na(as.numeric(line_data[[xAxis]])))){
        line_data[[xAxis]] = as.numeric(line_data[[xAxis]])
      }
      line_data[[yAxis]] = as.numeric(line_data[[yAxis]])
      
      groupBy = input$groupBy
      colorChoice = NULL
      if(groupBy == "None"){
        groupBy = 1
        colorChoice = NULL
      }
      else{
        groupBy = as.name(groupBy)
        colorChoice = as.name(groupBy)
      }
      
      lineSize = input$lineSize
      
      ggplot(line_data) + 
        geom_line(aes_(x = as.name(xAxis), y = as.name(yAxis), group=groupBy, color=colorChoice), 
                  size=lineSize) +
        scale_color_brewer(palette=input$palette) +
        theme_bw()
    }#,
    #width = 500,
    #height = 500
    )
  }) 
  
  #### End Line Plot
  
  #### Boxplot
  
  observeEvent(input$makeBoxplot, {
    box_data = graphing_data()
    box_data[[input$xAxisBox]] = as.factor(box_data[[input$xAxisBox]])
  
    output$box = renderPlot({
      ggplot(data = box_data, aes_q(x = as.name(input$xAxisBox), y = as.name(input$yAxisBox))) +
        geom_boxplot() +
        theme_bw()
    })  
  })
  
  
  #### End Boxplot
  
  #### Histogram
  
  observeEvent(input$makeHistogram, {
    hist_data = graphing_data()
    hist_data[[input$xAxisHist]] = as.double(hist_data[[input$xAxisHist]])
    
    output$histogram = renderPlot({
      ggplot(data = hist_data, aes_q(as.name(input$xAxisHist))) +
        geom_histogram(bins=input$histBins) + #, stat = "count") +
        theme_bw()
    })  
  })
  
  
  #### End Histogram
  
  #### One-Sample T-Test
  observeEvent(input$perform_one_sample, {
    t_test_data = graphing_data()
    t_test_data[[input$one_t_test_var]] = as.double(t_test_data[[input$one_t_test_var]])
    var_num = input$diff_var
    test_col = t_test_data[[input$one_t_test_var]]
    output$one_sample_output = renderPrint({t.test(test_col,mu=var_num)})
  })
  #### End One-Sample T-Test
  
  #### Two-Sample T-Test
  observeEvent(input$perform_two_sample, {
    t_test_data = graphing_data()
    t_test_data[[input$two_sample_var_one]] = as.double(t_test_data[[input$two_sample_var_one]])
    t_test_data[[input$two_sample_var_two]] = as.double(t_test_data[[input$two_sample_var_two]])

    first_col = t_test_data[[input$two_sample_var_one]]
    second_col = t_test_data[[input$two_sample_var_two]]
    output$two_sample_output = renderPrint({t.test(first_col,second_col,paired=input$paired_or_not,var.equal = input$equal_variance)})
  })
  #### End Two-Sample T-Test
}

# Run app ----
#The statement below causes Shiny to enter the debugger when an error occurs. 
#options(shiny.error = )
shinyApp(ui, server)

