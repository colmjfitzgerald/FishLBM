# server function for shiny app
server <- function(input, output, session){
  
  # read catch data file - reactive expression ----
  catchdata_read <- reactive({
    file_in <- input$uploadFile
    ext <- tools::file_ext(file_in$datapath)
    req(file_in)
    
    # data from file
    catch_data <- switch(ext,
                        xlsx = read.xlsx(xlsxFile = file_in$datapath, sheet = 1, check.names = TRUE),
                        xls = read_xls(path = file_in$datapath, sheet = 1),
                        csv = read.csv(file = file_in$datapath, header = TRUE))
  })
  
  
  
  
  # dynamic UI elements (renderUI, insertUI etc.) ----
  
  # length data is the minimum required
  output$lengthSelect <- renderUI({
    catchdata <- catchdata_read()
    choice <-  colnames(catchdata)
    selectInput("lengthCol", "Select length column", choices = choice, multiple = FALSE)
  })
  
  output$checkboxData <- renderUI({
    catchdata <- catchdata_read()
    choice <-  c("sex", "age", "year", "maturity", "gear")
    expr = list(
      checkboxGroupInput("checkboxUserData", "Select data categories", choices = choice, selected = NULL),
      actionButton("submitDataTypes", "Submit categories", icon = icon("table"))
    )
  })
  
  submittedCBData <- 
    eventReactive(input$submitDataTypes,
                {input$checkboxUserData}#, ignoreInit = TRUE
    )
  
  observeEvent(
    input$submitDataTypes,
    {
      catchdata <- catchdata_read()
      choice <-  colnames(catchdata)
      if("sex" %in% submittedCBData()){
        insertUI(
          selector = "#columnSelect",
          where = "beforeEnd",
          ui = selectInput("sexCol", "Select sex column", choices = choice, multiple = FALSE)
        )
      }
      if("gear" %in% submittedCBData()){
        insertUI(
          selector = "#columnSelect",
          where = "beforeEnd",
          ui = selectInput("gearCol", "Select gear column", choices = choice, multiple = FALSE)
        )
      }
      print(paste("all user categories", input$checkboxUserData, sep = " = "))
      print(paste("submittedCheckBoxData", submittedCBData(), sep = "="))
      print(paste0("is.vector submittedCBData? ", is.vector(submittedCBData)))
      print(paste0("gear col query:", input$gearCol))
    }
    # how to use ignoreInit effectively?
  )
  
  
  
  # action button and reaction to data column selection ====
  output$submitColsBtn <- renderUI({
    actionButton("submitCols", "Add categories", icon = icon("table"))
  })
  
  
  catchdataCategorise <- eventReactive(
    input$submitCols, {
      catchdata <- catchdata_read()
      if(!is.null(input$sexCol)){
        if(!is.null(input$gearCol)) {
          catchdata <- catchdata %>%
            select(input$sexCol, input$gearCol, input$lengthCol)
          pg <- ggplot(catchdata) +
            geom_histogram(aes_(x = as.name(input$lengthCol), 
                                fill = as.name(input$sexCol)), 
                           closed = "left", boundary = 0, binwidth = 20) +
            facet_grid(rows = sym(input$gearCol), scales = "free") 
        } else {
          catchdata <- catchdata %>%
            select(input$sexCol, input$lengthCol)
          pg <- ggplot(catchdata) +
            geom_histogram(aes_(x = as.name(input$lengthCol), 
                                fill = as.name(input$sexCol)), 
                           closed = "left", boundary = 0, binwidth = 20)
        }
      }  else{
        if(!is.null(input$gearCol)) {
          catchdata <- catchdata %>%
            select(input$gearCol, input$lengthCol)
          pg <- ggplot(catchdata) +
            geom_histogram(aes_(x = as.name(input$lengthCol)), 
                           closed = "left", boundary = 0, binwidth = 20) + 
            facet_grid(rows = sym(input$gearCol))
        } else {
          catchdata <- catchdata %>%
            select(input$lengthCol)
          pg <- ggplot(catchdata) +
            geom_histogram(aes_(x = as.name(input$lengthCol)))
        }
      }
      list(catchdata, pg)
    }#,
    #ignoreInit = TRUE
  )
  
  
  # choose length-based assessment ====
  observeEvent(
    input$submitCols,
    {
      output$cbLBA <- renderUI({
        # choice <- # dependent on data
        # tagList - see renderUI help
        expr = list(
          checkboxGroupInput("cbLBA", "Select assessment", choices = c("LB-SPR", "LBB", selected = NULL)),
          actionButton("submitLBA", "Submit assessment", icon = icon("chart-line"))
        )
      })
    })
  

  
  # ui elements table and plot objects ----
  
  # configure data ====
  # present "raw" data in tabular form, once we submit dataTypes
  output$catchDataTable <- 
    renderDataTable({
      expr = datatable(catchdataCategorise()[[1]], 
                       options = list(autowidth = TRUE, pageLength = 10, scrollX = TRUE, scrollY = FALSE), # position of options? 
                       fillContainer = TRUE)})
  
  
  # visualise data ====
  output$lengthComposition <- renderPlotly({
    print(paste0("str(input$lengthCol) = ", str(input$lengthCol)))
    print(.data[[input$lengthCol]])
    expr = ggplotly(p = catchdataCategorise()[[2]] + 
                      theme_bw(),
                    height = 800, width = 400)
  })
  
  
  # LB-SPR assessment ====
  # LVB growth ####
  # output$growthParRBtn <- 
  #   eventReactive(
  #     input$submitDataTypes,
  #     {if("age" %in% input$checkboxUserData){
  #       expr = renderUI({
  #         radioButtons(inputId = "growthParOption", 
  #                      label = "LVB growth",
  #                      choices = list("User-specified (point) estimate", 
  #                                     "Frequentist inference", 
  #                                     "Bayesian inference"))
  #       })
  #     } else {
  #       print("here")
  #       expr = renderUI({
  #         radioButtons(inputId = "growthParOption", 
  #                      label = "LVB growth",
  #                      choices = list("User-specified (point) estimate"))
  #         })
  #     }
  #       print(expr)
  #       expr
  #     }
  #   )
  output$growthParRBtn <- 
    renderUI({
      radioButtons(inputId = "growthParOption", 
                   label = "LVB growth",
                   choices = list("User-specified (point) estimate"))
    })
  
  observeEvent(input$submitDataTypes,
               {print(input$checkboxUserData)
                 print(input$growthParOption)}
  )
  
  output$lvbGrowthCurve <- renderPlotly({
    # slider curve
    growthcurve <- data.frame(age = seq(0, input$sliderAgeMax, by = 0.1))
    growthcurve$length_cm <- input$sliderLinf*(1- exp(-input$sliderK*(growthcurve$age-input$slidert0)))
    p <- ggplot() + 
      geom_line(data = growthcurve,
                aes(x = age, y = length_cm), colour = "black", alpha = 0.5, size = 1.5)
  })
  
}
