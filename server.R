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
  
  
  observeEvent(input$uploadFile,
               {updateVarSelectInput(session = getDefaultReactiveDomain(),
                                     "lengthColSelect",
                                     data = catchdata_read(),
                                     selected = ifelse(any(grepl("[length]", colnames(catchdata_read()), ignore.case = TRUE)),
                                                       grep("[length]", colnames(catchdata_read()), ignore.case = TRUE, value = TRUE),
                                                       NULL))})
  
  observeEvent(input$lengthColSelect,
               {
                 print(any(grepl("[length]", colnames(catchdata_read()), ignore.case = TRUE)))
                 print(grep("[length]", colnames(catchdata_read()), ignore.case = TRUE, value = TRUE))
                 print(paste0("str(input$lengthColSelect) = ", str(input$lengthColSelect)))
                 print(.data[[input$lengthColSelect]])
                 updateCheckboxGroupInput(session = getDefaultReactiveDomain(),
                                          inputId = "checkboxCatchData",
                                          choices = setdiff(colnames(catchdata_read()),
                                                            paste0(input$lengthColSelect)))
                                          #choices = colnames(catchdata_read()))
                                          # paste0() converts input$lengthColSelect to string
                                          
               })
  
  
  # dynamic UI elements (renderUI, insertUI etc.) ----
  
  # # length data is the minimum required
  # output$lengthSelect <- renderUI({
  #   catchdata <- catchdata_read()
  #   choice <-  colnames(catchdata)
  #   selectInput("lengthCol", "Select length column", choices = choice, multiple = FALSE)
  # })
  
  # output$checkboxData <- renderUI({
  #   catchdata <- catchdata_read()
  #   choice <-  c("sex", "age", "year", "maturity", "gear")
  #   expr = list(
  #     checkboxGroupInput("checkboxUserData", "Select data categories", choices = choice, selected = NULL),
  #     actionButton("submitDataTypes", "Submit categories", icon = icon("table"))
  #   )
  # })
  
  #submittedCBData <- 
  #  eventReactive(input$submitDataTypes,
  #              {input$checkboxUserData}#, ignoreInit = TRUE
  #  )
  
  # observeEvent(
  #   input$submitDataTypes,
  #   {
  #     catchdata <- catchdata_read()
  #     choice <-  colnames(catchdata)
  #     if("sex" %in% submittedCBData()){
  #       insertUI(
  #         selector = "#columnSelect",
  #         where = "beforeEnd",
  #         ui = selectInput("sexCol", "Select sex column", choices = choice, multiple = FALSE)
  #       )
  #     }
  #     if("gear" %in% submittedCBData()){
  #       insertUI(
  #         selector = "#columnSelect",
  #         where = "beforeEnd",
  #         ui = selectInput("gearCol", "Select gear column", choices = choice, multiple = FALSE)
  #       )
  #     }
  #     print(paste("all user categories", input$checkboxUserData, sep = " = "))
  #     print(paste("submittedCheckBoxData", submittedCBData(), sep = "="))
  #     print(paste0("is.vector submittedCBData? ", is.vector(submittedCBData)))
  #     print(paste0("gear col query:", input$gearCol))
  #   }
  #   # how to use ignoreInit effectively?
  # )
  
  


  
  
  
  # Tabulate data action button and reaction ====
  
  # length data category column selection ####
  output$submitColsBtn <- renderUI({
    actionButton("submitCols", "Input select data", icon = icon("table"))
  })
  
  
  # catchDataCategorise <- eventReactive(
  #   input$submitCols, {
  #     catchdata <- catchdata_read()
  #     if(!is.null(input$sexCol)){
  #       if(!is.null(input$gearCol)) {
  #         catchdata <- catchdata %>%
  #           select(input$sexCol, input$gearCol, input$lengthCol)
  #         pg <- ggplot(catchdata) +
  #           geom_histogram(aes_(x = as.name(input$lengthCol),
  #                               fill = as.name(input$sexCol)),
  #                          closed = "left", boundary = 0, binwidth = 20) +
  #           facet_grid(rows = sym(input$gearCol), scales = "free")
  #       } else {
  #         catchdata <- catchdata %>%
  #           select(input$sexCol, input$lengthCol)
  #         pg <- ggplot(catchdata) +
  #           geom_histogram(aes_(x = as.name(input$lengthCol),
  #                               fill = as.name(input$sexCol)),
  #                          closed = "left", boundary = 0, binwidth = 20)
  #       }
  #     }  else{
  #       if(!is.null(input$gearCol)) {
  #         catchdata <- catchdata %>%
  #           select(input$gearCol, input$lengthCol)
  #         pg <- ggplot(catchdata) +
  #           geom_histogram(aes_(x = as.name(input$lengthCol)),
  #                          closed = "left", boundary = 0, binwidth = 20) +
  #           facet_grid(rows = sym(input$gearCol))
  #       } else {
  #         catchdata <- catchdata %>%
  #           select(input$lengthCol)
  #         pg <- ggplot(catchdata) +
  #           geom_histogram(aes_(x = as.name(input$lengthCol)))
  #       }
  #     }
  #     list(catchdata, pg)
  #   }#,
  #   #ignoreInit = TRUE
  # )
  
  
  # catchdata element for plotting and LBSPR
  catchdata_table <- eventReactive(
    input$submitCols,
    {catchdata_read()[, c(input$checkboxCatchData, paste0(input$lengthColSelect))]}
  )
  
  # catchdata_plot
  catchdata_plot <- eventReactive(
    input$submitCols,
    { 
      whichSexCol <- sapply(input$checkboxCatchData, grepl, "[sex]", ignore.case = TRUE)
      whichGearCol <- sapply(input$checkboxCatchData, grepl, "[gear]", ignore.case = TRUE)
      whichYearCol <- sapply(input$checkboxCatchData, grepl, "[year]", ignore.case = TRUE)
      pg <- ggplot(catchdata_table())
      # if(any(whichSexCol) & !any(whichGearCol) & !any(whichYearCol)){ # sex
      #   
      # } else if(any(whichSexCol) & !any(whichGearCol) & any(whichYearCol)) {  # sex + year
      #   
      # } else if(!any(whichSexCol) & !any(whichGearCol) & any(whichYearCol)){ # year
      #   
      # } else if(any(whichSexCol) & any(whichGearCol) & !any(whichYearCol)) { # sex + gear
      #   
      # } else if(!any(whichSexCol) & any(whichGearCol) & !any(whichYearCol)) { # gear + year
      #   
      # } else if(!any(whichSexCol) & any(whichGearCol) & !any(whichYearCol)) { # gear
      #   
      # } else if(any(whichSexCol) & any(whichGearCol) & any(whichYearCol)) { # sex + gear + year
      #   
      # } else {
      #   
      # }
      gearCol <- input$checkboxCatchData[whichGearCol]
      yearCol <- input$checkboxCatchData[whichYearCol]
      if(any(whichSexCol)){  
        sexCol <- input$checkboxCatchData[whichSexCol]
        if(!any(whichGearCol) & !any(whichYearCol)){
          pg <- pg +
            geom_histogram(aes_(x = input$lengthColSelect, fill = ensym(sexCol)),
                           closed = "left", boundary = 0, bins = 40)
        } else if(any(whichGearCol) & !any(whichYearCol)) {
          pg <- pg +
            geom_histogram(aes_(x = input$lengthColSelect, fill = ensym(sexCol)),
                           closed = "left", boundary = 0, bins = 40) +
            facet_grid(rows = ensym(gearCol), scales = "free")
        } else if(!any(whichGearCol) & any(whichYearCol)) {
          pg <- pg +
            geom_histogram(aes_(x = input$lengthColSelect, fill = ensym(sexCol)),
                           closed = "left", boundary = 0, bins = 40) +
            facet_grid(rows = ensym(yearCol),  scales = "free")
        } else {
          pg <- pg +
            geom_histogram(aes_(x = input$lengthColSelect, fill = ensym(sexCol)),
                           closed = "left", boundary = 0, bins = 40) +
            facet_grid(rows = ensym(gearCol), cols = vars(yearCol), scales = "free")
        }
      } else {
          if(!any(whichGearCol) & !any(whichYearCol)){
            pg <- pg +
              geom_histogram(aes_(x = input$lengthColSelect), fill ="grey50",
                             closed = "left", boundary = 0, bins = 40)
          } else if(any(whichGearCol) & !any(whichYearCol)) {
            pg <- pg +
              geom_histogram(aes_(x = input$lengthColSelect), fill ="grey50",
                             closed = "left", boundary = 0, bins = 40) +
              facet_grid(rows = ensym(gearCol), scales = "free")
          } else if(!any(whichGearCol) & any(whichYearCol)) {
            pg <- pg +
              geom_histogram(aes_(x = input$lengthColSelect), fill ="grey50",
                             closed = "left", boundary = 0, bins = 40) +
              facet_grid(rows = ensym(yearCol), scales = "free")
          } else {
            pg <- pg +
              geom_histogram(aes_(x = input$lengthColSelect), fill ="grey50",
                             closed = "left", boundary = 0, bins = 40) +
              facet_grid(rows = ensym(gearCol), cols = vars(yearCol), scales = "free")
          }
      }
    pg + theme_bw()
    })
  
  # print head of raw catch data
  output$headRawCatchData <- renderPrint({
    expr = print(head(catchdata_read(), n = 20))
  })
  
  
  
  # Filtering data ====  

  output$checkboxFilterData <- renderUI({
    # does filterCols need to be an object to allow filtering??
    dfCL <- catchdata_table()
    filterCols <- setdiff(colnames(dfCL), paste0(input$lengthColSelect))
    checkboxList <- NULL
    for (fcol in filterCols){
      choiceCat <- unique(dfCL[,fcol])
      checkboxList <- append(checkboxList, 
                             list(checkboxGroupInput(inputId = paste0("checkbox",fcol),
                                                label = paste("Select", fcol, "levels", sep = " "), 
                                                choices = choiceCat, selected = choiceCat))
      )
    }
    #tagList(
    checkboxGroupInput(inputId = paste0("checkbox", fcol),
                       label = paste("Select", fcol, "levels", sep = " "), 
                       choices = choiceCat, selected = choiceCat)
    # checkboxGroupInput(inputId = paste0("checkbox",fcol),
    #                    label = paste("Select", fcol, "levels", sep = " "), 
    #                    choices = choiceCat, selected = choiceCat)
    #)
    #print(str(checkboxList))
    tagList(checkboxList)
  })
  
  output$filterBtn <- renderUI({
    actionButton(inputId = "actionFilterData", label = "Filter length records",
                 icon = icon("table"))
  })
  
  # for debugging purposes
  observeEvent(input$actionFilterData,
               {
                 dfCL <- catchdata_table()
                 filterCols <- setdiff(colnames(dfCL), paste0(input$lengthColSelect))
                 fcol <- filterCols[length(filterCols)]
                 print(input[[paste0("checkbox", fcol)]])
                 print(input[[paste0("checkbox", filterCols[1])]])
                 print(input)
                 print(head(catchDataFilter()))
                 #print(get(paste0("input$checkbox", fcol)))
               })
  
  catchDataFilter <- eventReactive(
    input$actionFilterData, {
      catchData <- catchdata_table()
      filterCols <- setdiff(colnames(catchData), paste0(input$lengthColSelect))
      print(filterCols)
      for (fcol in filterCols) {
        print(input[[paste0("checkbox", fcol)]])
        catchData <- catchData %>% 
          filter(!!sym(fcol) %in% input[[paste0("checkbox", fcol)]])
      }
      catchData
    }
  )
  
  
  
  # Length data conversion and column selection ====
  newLengthCol <- eventReactive(
    input$convertLengthUnits,
    {paste0("length_", input$newLengthUnits)}
  )
  
  lengthRecordsScale <- eventReactive(
    input$convertLengthUnits,
    {
      if(input$dataLengthUnits == input$newLengthUnits){
        lengthScale <- 1
      } else {
        lengthScale <- 
          switch(paste0(input$dataLengthUnits, "_to_", input$newLengthUnits),
                 cm_to_m = 0.01,
                 m_to_cm = 100,
                 mm_to_cm = 0.1,
                 cm_to_mm = 10,
                 m_to_mm = 1000,
                 mm_to_m = 0.001,
                 in_to_cm = 2.54,
                 cm_to_in = 1/2.54,
                 in_to_m = 0.0254,
                 m_to_in = 1/0.0254,
                 in_to_mm = 25.4,
                 mm_to_in = 1/25.4)
      }
    }
  )
  
  lengthRecordsConvert <- reactive(
    #   eventReactive(input$convertLengthUnits,
    { 
      lengthScale <- lengthRecordsScale()
      #length_records <- catchdata_table() %>% select(input$lengthColSelect) %>% 
      #  mutate("{newLengthCol()}" := .data[[input$lengthColSelect]]*lengthScale)
      length_records <- catchdata_table()[input$catchDataTable_rows_all,] %>% select(input$lengthColSelect) %>% 
        mutate("{newLengthCol()}" := .data[[input$lengthColSelect]]*lengthScale)
    }
  )
  
  
  # choose length-based assessment ====
  # currently not functional
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

  # numeric inputs for LBSPR
  # default to sliderInput values
  output$numLinf <- renderUI({
    numericInput("Linf", label = NULL, #"Linf", 
                 value = input$sliderLinf)
  })
  
  output$numKlvb <- renderUI({
    numericInput("kLvb", label = NULL, #"K growth", 
                 value = input$sliderK)
  })
  
    
  # tabulate data ====
  # present "raw" data in tabular form, once we submit dataTypes
  # https://rstudio.github.io/DT/shiny.html
  # "The first argument of DT::renderDT() can be either a data object 
  # or a table widget returned by datatable(). The latter form can be 
  # useful when you want to manipulate the widget before rendering it in 
  # Shiny, e.g. you may apply a formatting function to a table widget"
  output$catchDataTable <- 
    renderDataTable(
      expr = datatable(catchdata_table(), 
                       options = list(autowidth = TRUE, pageLength = 10, scrollX = TRUE, scrollY = FALSE,
                                      orderClasses = TRUE), # position of options? 
                       filter = "top", 
                       fillContainer = TRUE)
      )
  
  # visualise data ====
  output$lengthComposition <- renderPlotly({
    expr = ggplotly(p = catchdata_plot())#, height = 800, width = 400)
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
                aes(x = age, y = length_cm), colour = "black", alpha = 0.5, size = 1.5) +
      geom_segment(data = data.frame(length_0 = 0, length_inf = input$sliderLinf, 
                              age_0 = 0, age_max = input$sliderAgeMax),
                   aes(x = age_0, y = length_inf, xend = age_max, yend = length_inf),
                   colour = "red", linetype = 2) + 
      geom_text(data = data.frame(length_0 = 0, length_inf = input$sliderLinf, 
                                  age_0 = 0, age_max = input$sliderAgeMax),
                aes(x = age_0, y = length_inf), label = "Linf", colour = "red",
                nudge_x = 1, nudge_y = -5) + 
      theme_bw()
  })
  
  
  
  # reactive list for biological inputs
  reactiveStockPars <- eventReactive(input$btnStockPars,
                {expr = list(Linf = input$Linf,
                             K = input$kLvb,
                             M = input$M,
                             Walpha = input$Walpha, 
                             Wbeta = input$Wbeta, 
                             FecB = input$FecB, 
                             Steepness = input$Steepness,
                             Mpow = input$Mpow,
                             CVLinf = input$CVLinf,
                             NGTG = input$NGTG, 
                             MaxSD = input$MaxSD,
                             L50 = input$L50, # length at first maturity
                             L95 = input$L95)
                })
  
  
  
  # selectivity curve
  observeEvent(input$btnSelectivity,
               {
                 if(input$specifySelectivity == "Specify"){
#                   insertUI
                   output$selectivityNote <- renderText("User specifies selectivity parameters")
                 } else{
                   output$selectivityNote <- renderText("Estimate selectivity parameters in model fitting process")
                 }
               })
  
  
  
  
  # eventReactive??
  # slideLenBins <- reactive(
  #   {# eventually have a reactive StockPars object
  #     #                    StockPars <- list(MaxSD = input$MaxSD,
  #     #                                      CVLinf = input$CVLinf,
  #     #                                      Linc = input$Linc,
  #     #                                      Linf = input$sliderLinf)
  #     StockPars <- reactiveStockPars()
  #     
  #     SizeBins <- list(Linc = input$Linc, ToSize = NULL)
  #     SizeBins$ToSize <- StockPars$Linf * (1 + StockPars$MaxSD*StockPars$CVLinf)
  #     
  #     LenBins <- seq(from=0, to=SizeBins$ToSize, by=SizeBins$Linc)
  #   }
  # )
  
  # binned length data
  binLengthData <- reactive({
    StockPars <- reactiveStockPars() # eventReactive(input$btnStockPars
    SizeBins <- list(Linc = input$Linc, ToSize = NULL) # input$Linc
    SizeBins$ToSize <- StockPars$Linf * (1 + StockPars$MaxSD*StockPars$CVLinf)
    LenBins <- seq(from=0, to=SizeBins$ToSize, by=SizeBins$Linc)
    LenMids <- seq(from=0.5*SizeBins$Linc, 
                   by=SizeBins$Linc, length.out=(length(LenBins)-1))
    
    # input reactive expressions
    length_records <- lengthRecordsConvert()
    length_col <- newLengthCol()
    
    histogram_length <- hist(length_records[, length_col], plot = FALSE,  
                             breaks = LenBins, right = FALSE)
    # bins and binned counts
    list(SizeBins = SizeBins,
         LenBins = LenBins,
         LenMids = LenMids,
         LenDat = histogram_length$counts)
  }
  )
  
  observeEvent(input$btnStockPars,
               {print(binLengthData())})
  
  
  # change with slider input
  output$plotResponsiveLengthComposition <- 
    renderPlotly({
      expr = ggplotly(
        ggplot(lengthRecordsConvert()) + 
          geom_histogram(mapping = aes_string(x = newLengthCol()), 
                         breaks = binLengthData()$LenBins, # slideLenBins(), 
                         closed = "left", colour = "black", fill = "grey75") + 
          theme_bw())
      })
  

    
  # Fit LBSPR ####
  fitGTGLBSPR <- eventReactive(
    input$fitLBSPR,
    {
      # as.name
      length_records <- lengthRecordsConvert()
      length_col <- newLengthCol()
#      print(head(length_records[, length_col]))
      
      StockPars <- reactiveStockPars()
      StockPars$MK <- StockPars$M/StockPars$K
      
      
      #SizeBins <- list(Linc = input$Linc, ToSize = NULL)
      #SizeBins$ToSize <- StockPars$Linf * (1 + StockPars$MaxSD*StockPars$CVLinf)
      # 
      # 
      # LenBins <- seq(from=0, to=SizeBins$ToSize, by=SizeBins$Linc)
      # LenMids <- seq(from=0.5*SizeBins$Linc, 
      #                by=SizeBins$Linc, length.out=(length(LenBins)-1))
      # 
      # 
      # histogram_length <- 
      #   hist(length_records[, length_col], plot = FALSE,  breaks = LenBins, right = FALSE)
      # # apply knife-edge selection to length composition data
      # LenDat <- histogram_length$counts
      SizeBins <- binLengthData()$SizeBins
      LenBins <- binLengthData()$LenBins
      LenMids <- binLengthData()$LenMids
      LenDat <- binLengthData()$LenDat
      
      
      
      # GTG-LBSPR optimisation
      optGTG <- DoOpt(StockPars, LenDat, SizeBins, "GTG")
      # optGTG$Ests
      # optGTG$PredLen
      
      optFleetPars <- list(SL50 = optGTG$Ests["SL50"], 
                           SL95 = optGTG$Ests["SL95"],
                           FM = optGTG$Ests["FM"])
      # per recruit theory
      prGTG <- GTGLBSPRSim(StockPars, optFleetPars, SizeBins)
      
      # configure outputs
      VulLen2 <- 1.0/(1+exp(-log(19)*(LenMids-optFleetPars$SL50)/(optFleetPars$SL95-optFleetPars$SL50))) # Selectivity-at-Length
      
      # numbers-at-length (midpoints) LBSPR
      NatL_LBSPR <- data.frame(length_mid = prGTG$LenMids,
                               catchFished_at_length = prGTG$LCatchFished/max(prGTG$LCatchFished),
                               catchUnfished_at_length = prGTG$LCatchUnfished/max(prGTG$LCatchUnfished),
                               selectivityF_at_length = VulLen2,
                               popUnfished_at_length = prGTG$LPopUnfished/max(prGTG$LPopUnfished),
                               popFished_at_length = prGTG$LPopFished/max(prGTG$LPopFished)) #standardised??
      
      estModelFit <- data.frame(Parameter = c("FM", "SL50", "SL95"),
                                Description = c("F/M: fishing-natural mortality ratio",
                                                "Length at 50% selectivity",
                                                "Length at 95% selectivity"),
                                Estimate = c(unname(optFleetPars$FM), 
                                             unname(optFleetPars$SL50), 
                                             unname(optFleetPars$SL95)))
      opModelOut <- data.frame(Parameter = c("SPR", "YPR"),
                               Description = c("Spawning Potential Ratio", "Yield-per-recruit"),
                               Estimate = c(prGTG$SPR, prGTG$YPR))
      
      list(NatL_LBSPR = NatL_LBSPR,
           estModelFit = estModelFit,
           opModelOut = opModelOut)
    }
  )
  
  # print text on LBSPR estimating model fit 
  output$textLBSPREstFit <- renderPrint({
    expr = print(fitGTGLBSPR()$estModelFit)
  })
  
  # print text on LBSPR operating model output
  output$textLBSPROpOut <- renderPrint({
    expr = print(fitGTGLBSPR()$opModelOut)
  })
  
  output$visFitLBSPR <- renderPlotly({
    # length data
    length_records <- lengthRecordsConvert()
    length_col <- newLengthCol()
    
    LenBins <- binLengthData()$LenBins
    LenDat <- binLengthData()$LenDat
    maxLenDat <- max(LenDat)
    
    NatL_LBSPR <- fitGTGLBSPR()$NatL_LBSPR
    
    # pivot_longer
    NatL_long <- NatL_LBSPR %>%
      pivot_longer(cols = ends_with("at_length"),
                   names_to = "quantity",
                   names_pattern = "(.*)_at_length",
                   values_to = "numbers-per-recruit")
    
    # create ggplot with data...
    pg <- ggplot(length_records) + 
      geom_histogram(mapping = aes_string(x = length_col), breaks = LenBins, 
                     closed = "left", colour = "black", fill = "grey75")
    
    # ...and fit
    pg <- pg + 
      geom_area(data = NatL_LBSPR,
                mapping = aes(x = length_mid, y = max(LenDat)*catchFished_at_length), 
                fill = "salmon", alpha = 0.5) +
      geom_line(data = NatL_LBSPR,
                mapping = aes(x = length_mid, y = max(LenDat)*selectivityF_at_length), 
                colour = "red", lwd = 1) #+ 
      #scale_y_continuous(sec.axis = sec_axis(~  . /maxLenDat, name = "selectivity",
                                             #breaks = c(0, 1),
                                             #labels = c("0", "1")
      #                                       ) )
    
    
    
    expr = ggplotly(p = pg +  
                      scale_x_continuous(name = length_col) +
                      theme_bw(),
                    height = 400, width = 600)
  })
  
  
  # output$plotOpLBSPR <- renderPlotly({
  #   # operating model output based on estimating model fit
  #   NatL_LBSPR <- fitGTGLBSPR()$NatL_LBSPR
  #   
  #   # pivot_longer
  #   NatL_long <- NatL_LBSPR %>%
  #     pivot_longer(cols = ends_with("at_length"),
  #                  names_to = "quantity",
  #                  names_pattern = "(.*)_at_length",
  #                  values_to = "numbers_per_recruit")
  #   
  #   # ...and fit
  #   pg <- ggplot(NatL_long) +
  #     geom_area(mapping = aes(x = length_mid, y = numbers_per_recruit ), 
  #               fill = "salmon", alpha = 0.5) + 
  #     facet_grid(rows = vars(quantity))
  #   
  #   expr = ggplotly(p = pg + theme_bw(),
  #                   height = 400, width = 600)
  # })
  output$plotPopLBSPR <- renderPlot({
    NatL_LBSPR <- fitGTGLBSPR()$NatL_LBSPR
    LPopUnfished <- NatL_LBSPR$popUnfished_at_length
    LPopFished <- NatL_LBSPR$popFished_at_length
    LenMids <- NatL_LBSPR$length_mid
    print(max(LenMids))
    
    estModelFit <- fitGTGLBSPR()$estModelFit  
    SL50 <- estModelFit$Estimate[estModelFit$Parameter == "SL50"]
    SL95 <- estModelFit$Estimate[estModelFit$Parameter == "SL95"]
    SLmin <- SL50 - (SL95-SL50)
    
    par(mfrow = c(2,1), mgp = c(2,1,0), mar = c(4,3,3,1), cex = 1.15)
    plot(LenMids, LPopFished, col = "grey25", pch = 1, lwd = 1.5,
         xlab = newLengthCol(), ylab = "numbers per recruit",
         main = "entire length range", font.main = 1)
    points(LenMids, LPopUnfished, col = "grey75", pch = 16)
    legend("topright", c("fished", "unfished"), 
           col = c("grey25", "grey75"), 
           pch = c(1, 16))
    
    plot(LenMids, LPopFished, col = "grey25", pch = 1, lwd = 1.5,
         xlab = newLengthCol(), ylab = "numbers per recruit",
         xlim = c(SL50-(SL95-SL50), max(LenMids)),
         ylim = c(0, 1.25*LPopUnfished[which(LenMids-SLmin == min(abs((LenMids-SLmin)))) ]),
         main = "vulnerable length range", font.main = 1)
    points(LenMids, LPopUnfished, col = "grey75", pch = 16)
    legend("topright", c("fished", "unfished"), 
           col = c("grey25", "grey75"), 
           pch = c(1, 16))
    
  })
  
  output$plotCatchLBSPR <- renderPlot({
    NatL_LBSPR <- fitGTGLBSPR()$NatL_LBSPR
    LCatchUnfished <- NatL_LBSPR$catchUnfished_at_length
    LCatchFished <- NatL_LBSPR$catchFished_at_length
    LenMids <- NatL_LBSPR$length_mid
    
    par(mfrow = c(1,1), mgp = c(2,1,0), mar = c(4,3,3,1), cex = 1.15)
    plot(LenMids, LCatchFished, col = "grey25", pch = 1,
         xlab = newLengthCol(), ylab = "numbers per recruit",
         main = "catch-at-length", font.main = 1)
    points(LenMids, LCatchUnfished, col = "grey75", pch = 16)
    legend("topright", c("equilibrium fished", "no previous fishing"), 
           col = c("grey25", "grey75"), 
           pch = c(1, 16))
  })
}