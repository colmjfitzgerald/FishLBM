# server function for shiny app
server <- function(input, output, session){
  
  file_data <- reactive({
    inputFile <- req(input$uploadFile)
    ext <- tools::file_ext(inputFile$datapath)
    if(ext == "xlsx" || ext == "XLSX"){
      sheetNames <- openxlsx::getSheetNames(file = inputFile$datapath)
    } else if (ext == "xls" || ext == "XLS") {
      sheetNames <- unlist(readxl::excel_sheets(path = inputFile$datapath))
    } else {
      sheetNames <- NULL
    }
      
    list(ext = tools::file_ext(inputFile$datapath),
         path = inputFile$datapath,
         sheets = sheetNames)
  })
  
  # read catch data file - reactive expression ----
  catchdata_read <- reactive({
    
    fileExt <- file_data()$ext
    filePath <- file_data()$path
    sheetNames <- file_data()$sheets
    
    # data from file
    if(fileExt == "csv" || fileExt == ".CSV"){
      catch_data <- read.csv(file = filePath, header = TRUE)
    } else if(fileExt == "xlsx" || fileExt == "XLSX"){
      if(requireNamespace("openxlsx", quietly = FALSE)){
        sheetNames <- openxlsx::getSheetNames(file = filePath)
        showModal(
          modalDialog(
            renderUI({selectInput(inputId = "selectWorksheet", label = "Select input", choices = sheetNames, selected = sheetNames[1])}), 
            footer = modalButton("Dismiss")), 
          session)
        catch_data <- openxlsx::read.xlsx(xlsxFile = filePath, sheet = 1, check.names = TRUE)
      }
    } else if(fileExt == "xls" || fileExt == "XLS"){
      if(requireNamespace("readxl", quietly = FALSE)){
        catch_data <- readxl::read_xls(path = filePath, sheet = 1)
#        showModal(modalDialog(footer = modalButton("Dismiss")), 
#                  session)
      }
    } else{
      stop("file extension not supported")
    }
    catch_data
  })
  
  
  # freezeReactiveValue https://mastering-shiny.org/action-dynamic.html?q=freeze#freezing-reactive-inputs
  # This ensures that any reactives or outputs that use the input won’t be updated 
  # until the next full round of invalidation...
  # You might wonder when you should use freezeReactiveValue(): it’s actually good 
  # practice to always use it when you dynamically change an input value. The actual 
  # modification takes some time to flow to the browser then back to Shiny, and in 
  # the interim any reads of the value are at best wasted, and at worst lead to errors. 
  # Use freezeReactiveValue() to tell all downstream calculations that an input value 
  # is stale and they should save their effort until it’s useful.
  
  # update*Input functions
  # the input updater functions send a message to the client, telling it to change 
  # the settings of an input object
  # the messages are collect and sent after all the observers (including outputs) have
  # finished running
  
  # Put freezeReactiveValue() before update*Input() to prevent reactive expressions depending 
  # on a reactive value re-evaluating before being updated.?
  
  # observer help... will automatically re-execute when those dependencies change
  #              ... obseervers are only useful for their side effects
  #              ... reactive expressions use lazy evaluation...wait until they are called by someone else
  #              ... observers use eager evaluations; as soon as their dependencies change, they 
  #              ... schedule themselves to re-execute
  
  #observeEvent(input$uploadFile, #catchdata_read()?
  observe(
    { freezeReactiveValue(input, "lengthColSelect")
      updateVarSelectInput(session = getDefaultReactiveDomain(),
                           "lengthColSelect",
                           data = catchdata_read(),
                           selected = ifelse(any(grepl("length", colnames(catchdata_read()), ignore.case = TRUE)),
                                             grep("length", colnames(catchdata_read()), ignore.case = TRUE, value = TRUE),
                                             NA))
      })
  
  # updates checkboxCatchData after lengthColSelect changes
  observeEvent(input$lengthColSelect,
               { freezeReactiveValue(input, "checkboxCatchData")
                 updateCheckboxGroupInput(session = getDefaultReactiveDomain(),
                                          inputId = "checkboxCatchData",
                                          choices = setdiff(colnames(catchdata_read()),
                                                            paste0(input$lengthColSelect)))
                                          #choices = colnames(catchdata_read()))
                                          # paste0() converts input$lengthColSelect to string
                 # from help file
                 # input updater functions send a message to the client, telling to to change
                 # settings of an input object. The messages are collected and sent after all
                 # the observers (including outputs) have finished running
               })
  

  # dynamic UI elements (renderUI, insertUI etc.) ----
  
  # Tabulate data action button and reaction ====
  
  # length data category column selection ####
  output$btnSelectCols <- renderUI({
    actionButton("selectCols", "Input and plot data", icon = icon("table"))
  })
  
  
  
  # catchdata element for plotting and LBSPR
  catchdata_table <- eventReactive(input$selectCols, 
    { 
      req(input$lengthColSelect)
      checkboxCols <- input$checkboxCatchData
      catchdata <- catchdata_read()[, c(checkboxCols, paste0(input$lengthColSelect))]
      charCols <- which(sapply(catchdata, is.character))
      if(!(length(charCols) == 0 & is.integer(charCols))) {
        catchdata[, charCols] <- sapply(catchdata[, charCols], trimws)

        sexCol <- grep("sex", checkboxCols, ignore.case = TRUE, value = TRUE)
        dateCol <- grep("date", checkboxCols, ignore.case = TRUE, value = TRUE)
        yearCol <- grep("year", checkboxCols, ignore.case = TRUE, value = TRUE)
        if(length(dateCol)==1 & length(yearCol)==0){
          catchdata[, dateCol] <- as.Date(catchdata[, dateCol], tryFormats = c("%d/%m/%Y", "%Y-%m-%d"))
          catchdata$yearDate <- format(catchdata[, dateCol], "%Y")
        }
        # character columns, except date, as factor
        catchdata[, setdiff(charCols, union(dateCol, yearCol))] <- data.frame(lapply(catchdata[, setdiff(charCols, union(dateCol, yearCol)), drop = FALSE], as.factor))

      } else {
        catchdata <- data.frame(catchdata)
        names(catchdata) <- c(checkboxCols, paste0(input$lengthColSelect))
      }
      catchdata
    }
  )

  
  # catchdata_plot
  catchdata_plot <- eventReactive(
    input$selectCols,
    { # reactive "sources" and "conductors"
      #reactive({
      #freezeReactiveValue(input$uploadFile)
      #catchDataCols <- isolate(input$checkboxCatchData)
      #lengthCol <- isolate(input$lengthColSelect)
      req(input$selectCols)
      lengthCol <- input$lengthColSelect
      catchDataCols <- input$checkboxCatchData
      ggdata <- catchdata_table() # eventReactive(input$checkboxCatchData,...)
      
      # identify columns for grid plotting
      whichSexCol <- grepl("sex", catchDataCols, ignore.case = TRUE)
      whichGearCol <- grepl("gear|method", catchDataCols, ignore.case = TRUE)
      whichYearCol <- grepl("year", catchDataCols, ignore.case = TRUE)
      whichSpeciesCol <- grepl("species", catchDataCols, ignore.case = TRUE)
      
      pg <- ggplot(ggdata)

      if(!is.null(catchDataCols)){
      speciesCol <- catchDataCols[whichSpeciesCol]
      gearCol <- catchDataCols[whichGearCol]
      yearCol <- catchDataCols[whichYearCol]
      sexCol <- catchDataCols[whichSexCol]
      # facet by species (rows) if multispecies
      if(any(whichSpeciesCol)){
        # years as col, sex as colours
        if(any(whichYearCol)) {
          # if gear and sex, use sex as colour category
          if(any(whichSexCol)){
            pg <- pg + 
              geom_histogram(aes_(x = lengthCol, fill = ensym(sexCol)),
                             closed = "left", boundary = 0, bins = 40)
          } else if (any(whichGearCol)){
            pg <- pg + 
              geom_histogram(aes_(x = lengthCol, fill = ensym(gearCol)),
                             closed = "left", boundary = 0, bins = 40)
          } else {
            pg <- pg + 
              geom_histogram(aes_(x = lengthCol),
                             closed = "left", boundary = 0, bins = 40)
          }
          if(length(unique(catchdata_table()[, speciesCol])) <= 
             length(unique(catchdata_table()[, yearCol]))) {
            pg <- pg + 
              facet_grid(rows = as.formula(paste0(yearCol, " ~ ", speciesCol)), scales = "free",
                         labeller = label_wrap_gen(10)) #switch = "y", 
          } else {
            pg <- pg + 
              facet_grid(rows = as.formula(paste0(speciesCol, " ~ ", yearCol)), scales = "free",
                                  labeller = label_wrap_gen(10)) #switch = "y",
          }
        } else if(any(whichGearCol)){
          if(any(whichSexCol)){
            pg <- pg + 
              geom_histogram(aes_(x = lengthCol, fill = ensym(sexCol)),
                             closed = "left", boundary = 0, bins = 40)
          } else {
            pg <- pg + 
              geom_histogram(aes_(x = lengthCol),
                             closed = "left", boundary = 0, bins = 40)
          }
          pg <- pg + facet_grid(rows = as.formula(paste0(speciesCol, " ~ ", gearCol)), scales = "free")
        } else if (any(whichSexCol)) {
          # species and sex 
          pg <- pg + 
            geom_histogram(aes_(x = lengthCol, fill = ensym(sexCol)), closed = "left", boundary = 0, bins = 40) + 
            facet_grid(rows = ensym(speciesCol), scales = "free")
          
        } else{
          # species only
          pg <- pg + 
            geom_histogram(aes_(x = lengthCol), closed = "left", boundary = 0, bins = 40) + 
            facet_grid(rows = ensym(speciesCol), scales = "free")
        }
      } else {
        # no species
        if(any(whichSexCol)){ 
          if(!any(whichGearCol) & !any(whichYearCol)){
            pg <- pg +
              geom_histogram(aes_(x = lengthCol, fill = ensym(sexCol)),
                             closed = "left", boundary = 0, bins = 40)
          } else if(any(whichGearCol) & !any(whichYearCol)) {
            pg <- pg +
              geom_histogram(aes_(x = lengthCol, fill = ensym(sexCol)),
                             closed = "left", boundary = 0, bins = 40) +
              facet_grid(rows = ensym(gearCol), scales = "free")
          } else if(!any(whichGearCol) & any(whichYearCol)) {
            pg <- pg +
              geom_histogram(aes_(x = lengthCol, fill = ensym(sexCol)),
                             closed = "left", boundary = 0, bins = 40) +
              facet_grid(rows = ensym(yearCol),  scales = "free")
          } else {
            pg <- pg +
              geom_histogram(aes_(x = lengthCol, fill = ensym(sexCol)),
                             closed = "left", boundary = 0, bins = 40) +
              facet_grid(rows = ensym(gearCol), cols = vars(yearCol), scales = "free")
          }
        } else {
          # no species or sex
          if(!any(whichGearCol) & !any(whichYearCol)){
            pg <- pg +
              geom_histogram(aes_(x = lengthCol), fill ="grey50",
                             closed = "left", boundary = 0, bins = 40)
          } else if(any(whichGearCol) & !any(whichYearCol)) {
            pg <- pg +
              geom_histogram(aes_(x = lengthCol), fill ="grey50",
                             closed = "left", boundary = 0, bins = 40) +
              facet_grid(rows = ensym(gearCol), scales = "free")
          } else if(!any(whichGearCol) & any(whichYearCol)) {
            pg <- pg +
              geom_histogram(aes_(x = lengthCol), fill ="grey50",
                             closed = "left", boundary = 0, bins = 40) +
              facet_grid(rows = ensym(yearCol), scales = "free")
          } else {
            pg <- pg +
              geom_histogram(aes_(x = lengthCol), fill ="grey50",
                             closed = "left", boundary = 0, bins = 40) +
              facet_grid(rows = as.formula(paste0(yearCol, " ~ ", gearCol)), scales = "free")
          }
        }
        
      }
      } else {
        pg <- pg + geom_histogram(aes_(x = lengthCol), fill ="grey50",
                             closed = "left", boundary = 0, bins = 40)
      }
      pg + theme_bw() + theme(text = element_text(size = 12),
                              strip.text.x = element_text(margin = margin(0.125,0.25,0.25,0.25, "cm")))
    })
  
  # print head of raw catch data
#  output$headRawCatchData <- renderPrint({
#    expr = print(describe(catchdata_read())) # head(, n = 10)
#  })
# if(!require(Hmisc)){install.packages("Hmisc", dependencies = TRUE); require(Hmisc)}
  
  output$strRawCatchData <- renderPrint({
    expr = str(catchdata_read(), vec.len = 1)  
  })
  

  # Length data conversion and column selection ====
  # consider changing - overly complex
  newLengthCol <- eventReactive(
    input$convertLengthUnits,
    {paste0("length_", input$newLengthUnits)}
  )
  
  lengthRecordsScale <- eventReactive(
    input$convertLengthUnits,
    {
      if((input$dataLengthUnits == input$newLengthUnits) || 
         input$unitConvertRadioBtn == "Keep units unchanged"){
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
  
  lengthRecordsFilter <- reactive(#   eventReactive(input$convertLengthUnits,
    { 
      lengthScale <- lengthRecordsScale()
      length_records <- catchdata_table()[input$catchDataTable_rows_all,,drop = FALSE] %>% #select(input$lengthColSelect) %>% 
        dplyr::mutate("{newLengthCol()}" := .data[[input$lengthColSelect]]*lengthScale)
      length_records
    }
  )
  
  
  # choose length-based assessment ====
  # # currently not functional
  # observeEvent(
  #   input$selectCols,
  #   {
  #     output$cbLBA <- renderUI({
  #       # choice <- # dependent on data
  #       # tagList - see renderUI help
  #       expr = list(
  #         checkboxGroupInput("cbLBA", "Select assessment", choices = c("LB-SPR", "LBB", selected = NULL)),
  #         actionButton("submitLBA", "Submit assessment", icon = icon("chart-line"))
  #       )
  #     })
  #   })
  

  
  # ui elements table and plot objects ----

  # tabulate data ====
  # present "raw" data in tabular form, once we submit dataTypes
  # https://rstudio.github.io/DT/shiny.html
  # "The first argument of DT::renderDT() can be either a data object 
  # or a table widget returned by datatable(). The latter form can be 
  # useful when you want to manipulate the widget before rendering it in 
  # Shiny, e.g. you may apply a formatting function to a table widget"
  output$catchDataTable <- 
    DT::renderDT(
      expr = DT::datatable(catchdata_table(), 
                       options = list(autowidth = TRUE, pageLength = 10, scrollX = TRUE, scrollY = FALSE,
                                      orderClasses = TRUE), # position of options? 
                       filter = "top", 
                       fillContainer = TRUE)
      )
  
  # visualise data ====
  output$plotSpeciesLengthComposition <- renderPlot({
    # catchdata_plot() eventReactive on input$selectCols
    catchdata_plot()
  })
  
  

  # LVB growth ====
  
  anyAgeData <- reactive({
    ifelse(is.null(input$checkboxCatchData), FALSE, 
           any(grepl("age", input$checkboxCatchData, ignore.case = TRUE)))
  })
  
  maxAge <- reactive({
    if(anyAgeData()){
      max(gatherFishAgeLengthData()$age, na.rm = TRUE)
    } else{
      NA
    }
  })
  
  
  exploreLengthAtAge <- reactive({
    if(anyAgeData()){
      tagList(actionButton(inputId = "btnGoToGrowthPage", "Explore length-at-age fit", 
                           class = NULL))
    } else {
      tagList(actionButton(inputId = "btnGoToGrowthPage", "Explore length composition", 
                           class = NULL))
    }
  })
  
  output$moveToGrowthPage <- renderUI({
    exploreLengthAtAge()
  })
  
  observeEvent(input$btnGoToGrowthPage,
               updateNavbarPage(session, inputId = "lhEstimate", selected = "tabLengthAtAgeFit"))  
  
  # choices for growth parameters
  growthParChoices <- reactive({
    
    growthModel <- growthFrequentistFit()
    
    if(req(growthModel$nls$convInfo$isConv) & !is.null(req(growthModel$nls$convInfo$isConv))) {
      expr = c("User-specified", "Length-at-age fit")
    } else {
      expr = c("User-specified")
    }
  })
  
  
  observe({
    # note growthParChoices calls growthFrequentistFit - eventReactive(input$fitGrowth,)
    updateRadioButtons(session, "growthParOption", choices = growthParChoices(),
                       selected = "User-specified")
  })
  

  # reactive dataframes ####
  # create growth (age-length) dataframe 
  createPlotSliderCurveData <- reactive({
    # slider curve
    growthcurve <- data.frame(age = seq(0, floor(maxAge()*1.25), by = 0.1))
    growthcurve$length_cm <- input$sliderLinf*(1- exp(-input$sliderK*(growthcurve$age-input$slidert0)))
    growthcurve
  })
  

  gatherFishAgeLengthData <- reactive({
    # lengthRecordsFilter() dependence ***
    lengthCol <- newLengthCol()
    filteredLengthRecords <- lengthRecordsFilter()
    checkboxCatchCols <- input$checkboxCatchData
    sexCol <- grep("sex", checkboxCatchCols, ignore.case = TRUE, value = TRUE)
    if(anyAgeData()){
      ageCol <- grep("age", checkboxCatchCols, ignore.case = TRUE, value = TRUE)
      if(is.character(filteredLengthRecords[, ageCol])){
        filteredLengthRecords[, ageCol] <- as.numeric(gsub("\\+", "", filteredLengthRecords[, ageCol])) 
      }
      lengthAge <- data.frame(filteredLengthRecords[, c(lengthCol)], 
                              filteredLengthRecords[, ageCol],
                              filteredLengthRecords[, sexCol])
      colnames(lengthAge) <- c(lengthCol, ageCol, sexCol)
      lengthAgeData <- lengthAge[!is.na(lengthAge[, ageCol]) & !is.na(lengthAge[, lengthCol]),]
      colnames(lengthAgeData) <- c(lengthCol, "age", sexCol)
    } else {
      lengthAgeData <- data.frame(filteredLengthRecords[, c(lengthCol)], 
                                  age = NA,
                                  sex = filteredLengthRecords[, sexCol])
      colnames(lengthAgeData) <- c(lengthCol, "age", sexCol)
      lengthAgeData <- lengthAgeData[!is.na(lengthAgeData[, lengthCol]),]
    }
    lengthAgeData
  })
  
  
  # reactive (or eventReactive) model fitting objects ####
  # fit frequentist model to age-at-length data use slider settings as initial guess
  # if no age data, compute quantiles
  growthFrequentistFit <- eventReactive(input$fitGrowth, { 
    growthData <- gatherFishAgeLengthData()
    if(all(is.na(growthData$age))){
      nls.m <- growthData
      # summary(data.frame()) gives min, 1st quartile, median, mean 3rd quartile, max
    } else {
      nls.m <- nls(length_cm ~ Linf*(1-exp(-K*(age-t0))), 
                   data = growthData, 
                   start = list(Linf=input$sliderLinf, K = input$sliderK, t0=input$slidert0),
                   nls.control(maxiter = 250, tol = 1e-05, minFactor = 1/4096, 
                               printEval = TRUE, warnOnly = TRUE))
      #print(class(nls.m))
      #print(nls.m$convInfo)
    }
    x <- list(nls = nls.m, data = growthData)
  })
  
  # bootstrapped growth parameters confidence intervals using nlsBoot
  growthFrequentistFitBoot <- reactive({ #
    GFF <- growthFrequentistFit()
    growthFitData <- GFF$data
    ageLengthData <- gatherFishAgeLengthData()
    gm <- GFF$nls
    
    if(all(is.na(growthFitData[, "age"])) || !identical(growthFitData, ageLengthData)){
      # NULL return value
      return()
    } else {
      mean_fit <- fitted(gm)
      
      # bootstrap confidence intervals and growth parameter
      nBoot <- 1000
      coef_boot <- data.frame(Linf = rep(NA, nBoot),
                              K = rep(NA, nBoot),
                              t0 = rep(NA, nBoot))
      pred_data <- data.frame(age = seq(0, max(growthFitData[, "age"]), by = 0.1))
      pred_boot <- array(data = NA, dim = c(dim(pred_data)[1], nBoot))
      # can the following be vectorised?
      for (i in 1:nBoot){
        resid_boot <- sample(resid(gm), size = length(resid(gm)), replace = TRUE)
        gm_self_boot <- nls(length_cm ~ Linf*(1-exp(-K*(age-t0))),
                            data = data.frame(age = growthFitData$age,
                                              length_cm = resid_boot + mean_fit),
                            start = list(Linf=40.,K=0.2,t0=0.0),
                            nls.control(maxiter = 250, tol = 1e-05, minFactor = 1/4096, 
                                        printEval = FALSE, warnOnly = TRUE))
        coef_boot[i,] <- gm_self_boot$m$getPars()
        # predict length based on bootstrap regression estimators
        pred_boot[,i] <- predict(gm_self_boot, pred_data)
        # predict appears faster than vectorised  Linf*(1-exp(-K*(t-t0)))?
      }
      
      # confidence intervals
      length_ci_boot <- apply(pred_boot, MARGIN = 1, FUN = quantile, probs = c(0.50, 0.025, 0.975))
      row.names(length_ci_boot) <-  c("length_p_500", "length_p_025", "length_p_975")
      length_ci_boot <- as.data.frame(cbind(pred_data, t(length_ci_boot)))
      
      x <- list(predictions = length_ci_boot,
                parameters = coef_boot)
    }
  })
  
  # create Linf dataframe from growth fit
  createLinfLineData <- reactive({ # question - better way to do this?
    GFF <- growthFrequentistFit()
    fitLinf <- coef(GFF$nls)["Linf"]
    if(is.null(fitLinf)){
      gtgLinf <- NULL
    } else {
      gtgLinf <- expand.grid(age = c(0, floor(maxAge()*1.25)), 
                           length = c(fitLinf), 
                           ci = c("mean", "mean+2SD", "mean-2SD"), stringsAsFactors = FALSE)
      gtgLinf$linetype <- ifelse(gtgLinf$ci == "mean", "1", "2")
    }
    gtgLinf
  })
  
  
  # reactive ggplot elements/geoms #### 
  ggGrowth_CurveALData <- reactive({
    growthcurve <- createPlotSliderCurveData()
    fishAgeLengthData <- gatherFishAgeLengthData()
    lengthCol <- newLengthCol()
    
    p <- ggplot()
    p <- p +
      geom_line(data = growthcurve,
                aes(x = age, y = length_cm), colour = "grey75", alpha = 0.5, size = 1.5)
    
    # age-length or just length data
    if(all(is.na(fishAgeLengthData[, "age"]))){
      if(nrow(fishAgeLengthData) > 400){
        fishAgeLengthDataSubSample <- 
          rbind(fishAgeLengthData[fishAgeLengthData[ ,lengthCol] == min(fishAgeLengthData[ , lengthCol]),],
            fishAgeLengthData[fishAgeLengthData[ ,lengthCol] == max(fishAgeLengthData[ , lengthCol]),],
            fishAgeLengthData[sampleLengthRecords(),])  
      } else {
        fishAgeLengthDataSubSample <- fishAgeLengthData
      }
      p <- p + 
        geom_rug(data = fishAgeLengthDataSubSample, mapping = aes(y = length_cm)) 
      
    } else {
      if("sex" %in% colnames(fishAgeLengthData)){
        p <- p + 
          geom_point(data = fishAgeLengthData, mapping = aes(x = age, y = !!sym(lengthCol), 
                                                             colour = sex), alpha = 0.5)
      } else {
        p <- p + 
          geom_point(data = fishAgeLengthData, mapping = aes(x = age, y = !!sym(lengthCol)), 
                     alpha = 0.5)
      }
    }
    p + theme_bw()
  })
  
  # sample from length data for geom_rug
  sampleLengthRecords <- reactive({
    fishAgeLengthData <- gatherFishAgeLengthData()
    sample(1:nrow(fishAgeLengthData), 398, replace = FALSE,)
  })
  
  
  # length-at-age confidence interval ribbon
  ggGrowthFitCI <- reactive({
    GP <- growthFrequentistFitBoot()[["predictions"]]
    if(is.null(GP)){
      x <- geom_blank()
    } else {
      x <- geom_ribbon(data = GP, 
                  mapping = aes(x = age, ymin = length_p_025, ymax = length_p_975), 
                  fill = "red", alpha = 0.25)
    }
    x
  })
  
  # fitted mean length-at-age line
  ggGrowthFitMean <- reactive({
    GP <- growthFrequentistFitBoot()[["predictions"]]
    if(is.null(GP)){
      x <- geom_blank()
    } else {
      x <- geom_line(data = GP, mapping = aes(x = age, y = length_p_500), colour = "red")
    }
    x
  })
  
  ggLinf <- reactive({
    GFF <- growthFrequentistFit()
    fitGrowthData <- GFF$data
    
    fitLinf <- coef(GFF$nls)["Linf"]
    if(is.null(fitLinf)){
      gtgLinf <- NULL
    } else {
      gtgLinf <- expand.grid(age = c(0, floor(maxAge()*1.25)), 
                             length = c(fitLinf), 
                             ci = c("mean", "mean+2SD", "mean-2SD"), stringsAsFactors = FALSE)
      gtgLinf$linetype <- ifelse(gtgLinf$ci == "mean", "1", "2")
    }
    
    currentGrowthData <- gatherFishAgeLengthData()
    
    #  maximum extent of plot
    if(!is.null(gtgLinf) & identical(fitGrowthData, currentGrowthData)){
      x <- list(geom_line(data = gtgLinf[gtgLinf$ci == "mean",], aes(x = age, y = length),
                          colour = "red", size = 0.75, linetype = 2),
                geom_text(data = gtgLinf[gtgLinf$ci == "mean" & gtgLinf$age == 0, ],
                          aes(x = age, y = length), label = "Linf", colour = "red",
                          nudge_x = 1, nudge_y = -5),
                ylim(c(0, fitLinf*1.05)))
    } else {
      x <- geom_blank()
    }
    x
  })
  
  # # debug observe events
  # observeEvent(input$fitGrowth,
  #              {print(input$checkboxCatchData)
  #               insertUI(selector = "#sectionGrowthFitSummary", where = "beforeBegin", 
  #                        ui = actionButton(inputId = "removeFit", label = "Remove fit"))
  # })
  
  
  # growth outputs ####
  output$lvbGrowthCurve <- plotly::renderPlotly({#expr =
    # consider: req(growthModel$nls$convInfo$isConv) & !is.null(req(growthModel$nls$convInfo$isConv))
    if(anyAgeData()){
    if(input$fitGrowth > 0) {
      plotly::ggplotly(ggGrowth_CurveALData() + ggGrowthFitMean() + ggGrowthFitCI() + ggLinf())
    } else {
      plotly::ggplotly(ggGrowth_CurveALData())
    }
    } else {
      lengthData <- lengthRecordsFilter()
      lengthCol <- newLengthCol()
      maxLength <- max(lengthData[, lengthCol])
      
      p <- ggplot(data = lengthData) +
        geom_histogram(aes(x = length_cm), fill = "grey80", binwidth = 1) +
        geom_vline(xintercept = maxLength, colour = "black", linetype = 2, size = 0.75) +
        geom_vline(xintercept = input$sliderLinf, colour = "red") +
        theme_bw()
      if(any(grepl("year", colnames(lengthData), ignore.case = TRUE))){
        p <- p + 
        facet_wrap(as.formula(paste0(grep("year", colnames(lengthData), ignore.case = TRUE, 
                                          value = TRUE)," ~ .")))
      }
      plotly::ggplotly(p)
    }  
  })
  
  output$growthFitSummary <- renderPrint({
    expr = print(summary(growthFrequentistFit()$nls, correlation = TRUE))
  })
  
  # weight-at-length ####
  anyWeightData <- reactive({
    ifelse(is.null(input$checkboxCatchData), FALSE, any(grepl("weight", input$checkboxCatchData, ignore.case = TRUE)))
  })
  
  gatherFishWeightLengthData <- reactive({
    lengthColChr <- paste0(newLengthCol())
    lengthData <- lengthRecordsFilter()
    sexCol <- grep("sex", input$checkboxCatchData, ignore.case = TRUE, value = TRUE)
    lengthWeightData <- NULL
    if(anyWeightData()){
      weightCol <- grep("weight", input$checkboxCatchData, ignore.case = TRUE, value = TRUE)
      #browser()
      if(is.character(lengthData[, weightCol])){
        lengthData[, weightCol] <- as.numeric(gsub("\\+", "", lengthData[, weightCol])) 
      }
      lengthWeight <- data.frame(lengthData[, c(lengthColChr)],
                                 lengthData[, weightCol],
                                 lengthData[, sexCol])
      colnames(lengthWeight) <- c(lengthColChr, weightCol, sexCol)
      lengthWeightData <- lengthWeight[!is.na(lengthWeight[, weightCol]) & !is.na(lengthWeight[, lengthColChr]),]
      colnames(lengthWeightData) <- c("length", "weight", sexCol)
    } else{
      lengthWeight <- data.frame(lengthData[, c(lengthColChr)], 
                                 lengthData[, sexCol])
      colnames(lengthWeightData) <- c("length", sexCol)
    }
    lengthWeightData
  })
  
  geom_weight_length_data <- reactive({
    if(anyWeightData()){
      p_geom <- geom_point(data = gatherFishWeightLengthData(), 
                           aes(x = length, y = weight)) #fill = sexCol  
    } else {
      p_geom <- geom_blank()
    }
    p_geom
  })
  
  lwFit <- fitWeightLengthServer("wlCurve", gatherFishWeightLengthData)$lwFit
  geom_line_lw_fit <- fitWeightLengthServer("wlCurve", gatherFishWeightLengthData)$geom_line_lw_fit
  
  
  # output length, weight columns from module?
  output$lengthWeightData <- plotly::renderPlotly({
    p <- ggplot() +
      geom_weight_length_data() + 
      geom_line_lw_fit() + 
      theme_bw()# idea: set sexCol to NI
    plotly::ggplotly(p)
  }
  )
  
  
  # LB-SPR assessment ====
  
  
  # natural mortality choices ####
  
  # choices for growth parameters
  natMestChoices <- reactive({
    growthChoices <- c("User-specified" = "user", 
                       "PaulyNLS-T ($M = 4.118K^{0.73}L_{\\infty}^{-0.33}$)" = "pauly", 
                       "Two-parameter K ($M = 0.098 + 1.55K$)" = "twoK")
    if(anyAgeData() & 
       any(!is.na(gatherFishAgeLengthData()[,grep("age", names(gatherFishAgeLengthData()), ignore.case = TRUE, value = TRUE)]))) {
      growthChoices = c(growthChoices, "HoenigNLS ($M = 4.899t_{max}^{-0.916}$)" = "hoenig")
    }
    expr = growthChoices
  })
  
  output$natMortalityRadioBtn <- 
    renderUI({
      withMathJax(
      radioButtons(inputId= "natMortality", label = "Empirical estimators",
                   choices = natMestChoices(), selected = natMestChoices()[1])
      )
    }
    )
  
  # numeric inputs for LBSPR ####
  # default to sliderInput values
  output$numLinf <- renderUI({
    numericInput("Linf", label = NULL, step = 1, #"Linf", 
                 value = ifelse(grepl("fit", input$growthParOption), 
                                round(coef(growthFrequentistFit()$nls)["Linf"], digits = 2),
                                round(input$sliderLinf, digits = 2)))
  })
  
  output$numKlvb <- renderUI({
    numericInput("kLvb", label = NULL, step = 0.01,  #"K growth", 
                 value = ifelse(grepl("fit", input$growthParOption), 
                                round(coef(growthFrequentistFit()$nls)["K"], digits = 2),
                                round(input$sliderK, digits = 2)))
  })
  
  output$numM <- renderUI({
    numericInput("M", label = NULL, value = 0.3, step = 0.05) #"M natural mortality", 
  })
  
  # nb value = initial value
  output$numLm50 <- renderUI({
    numericInput("Lm50", label = NULL, #"Linf", 
                 value = ifelse(grepl("fit", input$growthParOption), 
                                round(coef(growthFrequentistFit()$nls)["Linf"]*0.66, digits = 2),
                                round(input$sliderLinf*0.66, digits = 2)))
  })
  
  output$numLm95 <- renderUI({
    numericInput("Lm95", label = NULL, #"Linf", 
                 value = ifelse(grepl("fit", input$growthParOption), 
                                round(coef(growthFrequentistFit()$nls)["Linf"]*0.75, digits = 2),
                                round(input$sliderLinf*0.75, digits = 2)),
                 min = input$sliderLinf*0.25)
  })
  
  
  # mortality and maturity life history parameter updates
  updateMortality <- reactive({
    #if(is.na(max(gatherFishAgeLengthData()$age, na.rm = FALSE)))
    #if(is.na(tmax)) { NA } else { round(4.899*tmax^-0.916, 3) }
    if(input$natMortality == "user" || is.null(input$natMortality)){
      expr = 0.3
    } else if(input$natMortality == "pauly") {
      expr = round(4.118*input$kLvb^0.73*input$Linf^(-0.33), 3)
    } else if(input$natMortality == "twoK") {
      expr = round(0.098 + 1.55*input$kLvb, 3)        
    } else if(input$natMortality == "hoenig") {
      expr = round(4.899*max(isolate(gatherFishAgeLengthData())$age, na.rm = FALSE)^-0.916, 3) 
    }
  })
  
  updateMaturity <- reactive({
    if(input$maturityPars == "user" || is.null(input$maturityPars)){
      expr = round(0.66*input$Linf,1)
    } else if(input$maturityPars == "bhlhi") {
      expr = round(0.66*input$Linf,1)
    } else if(input$maturityPars == "bf2009") {
      expr = round(exp(-0.1189 + 0.9157*log(max(isolate(gatherFishAgeLengthData())[, newLengthCol()]))),1)
    }
  })
  
  updateMaturity95 <- reactive({
    if(input$maturityPars == "user" || is.null(input$maturityPars)){
      expr = round(0.75*input$Linf,1)
    } else if(input$maturityPars == "bhlhi") {
      expr = round(0.66*input$Linf,1) + 5
    } else if(input$maturityPars == "bf2009") {
      expr = round(exp(-0.1189 + 0.9157*log(max(isolate(gatherFishAgeLengthData())[, newLengthCol()])))+5,1)
    }
  })
  
  
  observe({
    updateNumericInput(session, "M", value = updateMortality())
  })
  observe({
    updateNumericInput(session, "Lm50", value = updateMaturity())
    updateNumericInput(session, "Lm95", value = updateMaturity95())
  })
  
  # weight-at-length coefficients
  observe({
    updateNumericInput(session, "Walpha", value = signif(lwFit()$walpha, 4))
    updateNumericInput(session, "Wbeta", value = signif(lwFit()$wbeta, 4))
  })
  
  
  
  # reactive list for biological inputs
  setLHPars <- #eventReactive(input$btnStockPars,{
    reactive({# common life history parameters
    lhp_list <- list(Linf = input$Linf,     # linf
                     K = input$kLvb,        # vbk 
                     t0 = input$slidert0,   # not used LBSPR
                     M = input$M,           # M
                     Walpha = input$Walpha, # lwa
                     Wbeta = input$Wbeta,   # lwb
                     L50 = input$Lm50,      # length at first maturity, M50
                     L95 = ifelse(!is.null(input$Lm95), input$Lm95, NULL), # M95
                     CVLinf = input$CVLinf  # CVlen
    )  
  })
  

  # technical method parameters ####
  observeEvent(input$lengthBasedAssessmentMethod, 
               {updateTabsetPanel(inputId = "techPars", selected = input$lengthBasedAssessmentMethod)}
  )
  
  # selectivity curve ####

  observe({
    updateRadioButtons(session, inputId = "specifySelectivity",
                       choices = selectParameterSpecification())
  })
  
  # reactive radioButton options..."Asymptotic" default:
  # must match radioButtons(inputId = "specifySelectivity",
  selectParameterSpecification <- reactive({
    #if(input$selectSelectivityCurve == "Logistic" && !is.null(input$selectSelectivityCurve)){ # or input$chooseSelectivityPattern
    if(input$chooseSelectivityPattern == "Asymptotic" && !is.null(input$chooseSelectivityPattern)){
      x <- c("Initial estimate", "Fixed value")
    } else {
      x <- c("Fixed value")
    }
  })

  output$chooseSelectivityCurve <- renderUI({
    selectivityCurveChoices <- sizeSelectivityCurves() 
    selectInput(inputId = "selectSelectivityCurve",
                label = "Size-selectivity curve",
                choices = selectivityCurveChoices,
                selected = selectivityCurveChoices[1])
    })
  
  # chooseSelectivityCurve - reactive options
  sizeSelectivityCurves <- reactive({
    if(input$chooseSelectivityPattern == "Asymptotic" && !is.null(input$chooseSelectivityPattern)){
      x <- c("Logistic", "Knife-edged")  
    } else if(input$chooseSelectivityPattern == "Dome-shaped" && !is.null(input$chooseSelectivityPattern)) {
      if(input$lengthBasedAssessmentMethod == "LB-SPR"){
        x <- c("Normal.loc", "Normal.sca", "logNorm")
      } else if(input$lengthBasedAssessmentMethod == "LIME"){
        x <- "dome (rhs)"
      }
    }
  })

  
  output$gearMeshSizes <- renderUI(
    numericInput(inputId = "selectGearMeshSizes",
                 label = "Number of mesh sizes in gear",
                 value = 1, min = 1, max = 1)
  )

  # reactiveValues - when you read a value from it, the calling reactive expression takes a 
  # reactive dependency on that value, and when you write to it, it notifies any reactive functions 
  # that depend on that value. Note that values taken from the reactiveValues object are reactive, 
  # but the reactiveValues object itself is not.
  #reactValues <- reactiveValues()
  
  #observe({
    # use reactiveValues and change the value of a reactive value in an observer...
    # use that value as a dependency in the render function
  #})
  
  # reactive or reactiveValues?
  gearSelectivityParameters <- reactive({
    x <- list(SL1 = NULL, SL2 = NULL, SLKnife = NULL)
  })
  
  
  # selectivity parameters
  # numeric inputs
  selectivityParsNumInp <- reactive({
    if(input$specifySelectivity == "Fixed value" & !is.null(input$specifySelectivity)){
      if(input$selectSelectivityCurve == "Knife-edged" & !is.null(input$selectSelectivityCurve)){
        tagList(
          numericInput(inputId = "SLKnife", label = "MLL",
                       value = round(input$Linf*0.50, digits = 2),
                       min = 0.0, max = input$Linf),
          actionButton(inputId = "btnFixedFleetPars", "Input selectivity parameters",
                       class = "btn-success"),
          bsTooltip(id = "SLKnife",
                    title = "Minimum Length Limit. Fishery regulation whereby fish smaller than this limit must be returned to the water whereas larger fish may be retained.",
                    placement = "left", trigger = "hover",  options = list(container = "body"))
        )
      } else if(input$selectSelectivityCurve == "Logistic" & !is.null(input$selectSelectivityCurve)) {
        tagList(
          numericInput(inputId = "SL1", label = "Length at 50% selectivity",
                       value = round(input$Linf*0.70, digits = 2)),
          numericInput(inputId = "SL2", label = "Length at 95% selectivity",
                       value = round(input$Linf*0.80, digits = 2)),
          actionButton(inputId = "btnFixedFleetPars", "Input selectivity parameters",
                       class = "btn-success"),
          bsTooltip(id = "SL1",
                    title = "Length at 50% selectivity<br> <em>Length at which 50% of a stock is caught by fishery gear.</em> Replace default <b>SL50 = 0.7 Linf</b> with empirical data or expert knowledge. Influences F/M statistical inference.",
                    placement = "left", trigger = "hover",  options = list(container = "body")),
          bsTooltip(id = "SL2",
                    title = "Length at 95% selectivity<br> <em>Length at which 95% of a stock is caught by fishery gear.</em> Replace default <db>SL50 = 0.8 Linf</b> with empirical data or expert knowledge. Influences F/M statistical inference.",
                    placement = "left", trigger = "hover",  options = list(container = "body"))
        )
      } else if(input$selectSelectivityCurve %in% c("Normal.sca", "Normal.loc") &
                !is.null(input$selectSelectivityCurve)) {
        tagList(
          numericInput(inputId = "SL1", label = "Length at maximum selectivity",
                       value = round(input$Linf*0.70, digits = 2)),
          numericInput(inputId = "SL2", label = "SD or spread of normal selectivity curve",
                       value = round(input$Linf*0.10, digits = 2)),
          numericInput(inputId = "SLKnife", label = "MLL",
                       value = round(input$Linf*0.0, digits = 2),
                       min = 0.0, max = input$Linf),
          actionButton(inputId = "btnFixedFleetPars", "Input selectivity parameters",
                       class = "btn-success")
        )
      } else if(input$selectSelectivityCurve %in% c("logNorm") & !is.null(input$selectSelectivityCurve)) {
        tagList(
          numericInput(inputId = "SL1", label = "Length at maximum selectivity",
                      value = round(input$Linf*0.70, digits = 2), min = 0, max = input$Linf, step = 1),
          numericInput(inputId = "SL2", label = "SD/spread of lognormal selectivity curve",
                      value = round(input$Linf*0.01, digits = 2), min = 1e-3, max = 2, step = 0.01),
          numericInput(inputId = "SLKnife", label = "MLL",
                      value = round(input$Linf*0.0, digits = 2),
                      min = 0.0, max = input$Linf,  step = 1)
        )
      }
    } else if (input$specifySelectivity == "Initial estimate" & !is.null(input$specifySelectivity)) {
      tagList(tags$p("Estimate selectivity parameters in model fitting process"))
    }
  })
  
  # slider controls
  selectivityParsSliderInp <- reactive({
    
    validate(need(input$Linf * (1 + 2*input$CVLinf) > max(lengthRecordsFilter()[,newLengthCol()], na.rm = TRUE),
                  "Increase Linf (or CVLinf) so that Linf of largest GTG: Linf*(1+2*CVLinf) > max fish length!!"))

    lengthInc <-1 # min bin width
    lengthBins <- seq(from=0, to=input$Linf*(1 + 2*input$CVLinf), by = lengthInc)
    lengthMids <- seq(from=0.5*lengthInc, by = lengthInc, length.out=(length(lengthBins)-1))
    
    # Starting guesses fo SL50, SL95, sDelta = SL95 - SL50 , sSD
    LenDat1 <- hist(lengthRecordsFilter()[,newLengthCol()], plot = FALSE, breaks = lengthBins, right = FALSE)
    sSL50 <- lengthMids[which.max(LenDat1$count)]
    sDelta <- log(19) # single parameter logistic curve
    sSL95 <- ifelse(sSL50+sDelta < input$Linf, sSL50+sDelta, sSL50 + 0.1(input$Linf-sSL50)  )
    sSD <- 0.2*sSL50
    
    if(req(input$selectSelectivityCurve) == "Knife-edged"){
      tList <- tagList(
        sliderInput(inputId = "SLKnife", label = "MLL",
                    value = round(input$Linf*0.50, digits = 2),
                    min = 0.0, max = input$Linf, step = 1)
      )
    } else if(req(input$selectSelectivityCurve) == "Logistic") {
      tList <- tagList(
        sliderInput(inputId = "SL1", label = "Length at 50% selectivity",
                    value = round(sSL50, digits = 2), min = 0, max = input$Linf,
                    step = 0.1, round = 3),
        sliderInput(inputId = "SL2", label = "Length at 95% selectivity",
                    value = round(sSL95, digits = 2), min = 0, max = input$Linf,
                    step = 0.1, round = 3)
      )
    } else if(req(input$selectSelectivityCurve) %in% c("Normal.sca", "Normal.loc")){
      tList <- tagList(
        sliderInput(inputId = "SL1", label = "Length with maximum selectivity",
                    value = round(sSL50, digits = 2), min = 0, max = input$Linf, step = 1),
        sliderInput(inputId = "SL2", label = "SD (spread) of dome-shaped selectivity curve",
                    value = round(sSD, digits = 2), min = 0, max = input$Linf, step = 1),
        sliderInput(inputId = "SLKnife", label = "MLL",
                    value = round(input$Linf*0.0, digits = 2),
                    min = 0.0, max = input$Linf,  step = 1)
      )
    } else if(req(input$selectSelectivityCurve) == "dome (rhs)") {
      tList <- tagList(
        sliderInput(inputId = "SL1", label = "Length at 50% selectivity",
                    value = round(sSL50, digits = 2), min = 0, max = input$Linf,
                    step = 0.1, round = 3),
        sliderInput(inputId = "SL2", label = "Length at 95% selectivity",
                    value = round(sSL95, digits = 2), min = 0, max = input$Linf,
                    step = 0.1, round = 3),
        sliderInput(inputId = "SLDome", label = "SD (spread) of dome-shaped selectivity curve",
                    value = sSD, min = 0, max = input$Linf, step = 1, round = 2),
        sliderInput(inputId = "SLKnife", label = "MLL",
                    value = round(input$Linf*0.0, digits = 2),
                    min = 0.0, max = round(input$Linf, 3),  step = 1)
      )
    } else if(req(input$selectSelectivityCurve) == "logNorm") {
      tList <- tagList(
        sliderInput(inputId = "SL1", label = "Length with maximum selectivity",
                    value = round(sSL50, digits = 2), min = 0, max = input$Linf, step = 1),
        sliderInput(inputId = "SL2", label = "Log-normal SD/spread of dome-shaped selectivity curve",
                    value = round(0.50, digits = 2), min = 1e-3, max = 2, step = 0.01),
        sliderInput(inputId = "SLKnife", label = "MLL",
                    value = round(input$Linf*0.0, digits = 2),
                    min = 0.0, max = input$Linf,  step = 1)
      )
    }
    # add action button to submit selectivity parameters
    tagList(tList, 
            actionButton(inputId = "btnFixedFleetPars", 
                         "Input selectivity parameters",
                         class = "btn-success"))
  })
  
  
  # render reactive expression with tagList
  output$selectivityParameters <- renderUI({  
    selectivityParsSliderInp()
    #selectivityParsNumInp()
    #  tags$p("Text here")
    })
  
  # LBSPR fleet parameters
  setFleetPars <- reactive(
    #eventReactive(input$btnFixedFleetPars, 
                  {
                    if(input$lengthBasedAssessmentMethod == "LB-SPR"){
                    if(input$chooseSelectivityPattern == "Dome-shaped"){
                      list(SL1 = input$SL1, SL2 = input$SL2, SLMin = input$SLKnife, 
                        SLmesh = 1, selexCurve = input$selectSelectivityCurve,
                        selexParsEstimate = FALSE)
                    } else {
                      if(input$selectSelectivityCurve == "Logistic") {
                        if(input$specifySelectivity == "Fixed value"){
                          list(SL1 = input$SL1, SL2 = input$SL2, 
                               selexCurve = input$selectSelectivityCurve,
                               selexParsEstimate = FALSE)
                        } else if(input$specifySelectivity == "Initial estimate") {
                          list(selexCurve = input$selectSelectivityCurve,
                               selexParsEstimate = TRUE)  
                        }
                      } else if(input$selectSelectivityCurve == "Knife") {
                        list(SLKnife = input$SLKnife, selexCurve = input$selectSelectivityCurve,
                             selexParsEstimate = FALSE)
                      } 
                    }
                    } else if(input$lengthBasedAssessmentMethod == "LIME"){
                      if(input$chooseSelectivityPattern == "Dome-shaped"){
                        list(S50 = input$SL1, S95 = input$SL2, # hack to get around create_lh_list issues
                             dome_sd = input$SLDome,
                             selexBy = "length",
                             selexCurve = "dome", # create_lh_list, #tolower(input$selectSelectivityCurve),
                             nfleets = 1,
                             est_selex_f = FALSE)
                      } else if(input$chooseSelectivityPattern == "Asymptotic") {
                        list(S50 = input$SL1, S95 = input$SL2,
                             selexBy = "length",
                             selexCurve = tolower(input$selectSelectivityCurve),
                             nfleets = 1,
                             est_selex_f = ifelse(input$specifySelectivity == "Fixed value", FALSE, TRUE))
                      }
                    }
    })

  
  # selectivity curve data frame - reactive or reactiveValues?
  selectionCurves <- reactive({
    # nb handle SLKnife, SLMLL dependent on selectivityCurve value (logistic, knife-edged etc.)?
    dSC <- isolate(data.frame(
      length = seq(min(lengthRecordsFilter()[,newLengthCol()], na.rm = TRUE), input$Linf, length.out = 51),
      size = 1, quantity = "selectivity"))
    SLmesh <- 1
    req(input$SL1, input$SL2)
    if(req(input$selectSelectivityCurve) == "Logistic"){
      dSC$proportion <- 1.0/(1+exp(-log(19)*(dSC$length-input$SL1)/(input$SL2-input$SL1)))
    } else if(req(input$selectSelectivityCurve) == "Normal.loc") {
      dSC$proportion <- exp(-0.5*((dSC$length-((input$SL1)*SLmesh))/(input$SL2))^2)
      dSC$proportion[dSC$length < req(input$SLKnife)] <- 0
    } else if(req(input$selectSelectivityCurve) == "Normal.sca") {
      dSC$proportion <- exp(-0.5*((dSC$length-((input$SL1)*SLmesh))/((input$SL2^0.5)*SLmesh)^2))
      dSC$proportion[dSC$length < req(input$SLKnife)] <- 0
    } else if(req(input$selectSelectivityCurve) == "logNorm") {
      dSC$proportion <- exp(-0.5*((log(dSC$length)-log((input$SL1)*SLmesh))/(input$SL2))^2)
      dSC$proportion[dSC$length < req(input$SLKnife)] <- 0
    } else if(req(input$selectSelectivityCurve) == "dome (rhs)") {
      dSC$proportion <- 1/(1 + exp(-log(19)*(dSC$length - input$SL1)/(input$SL2 - input$SL1)))
      # see create_lh_list.R in LIME https://github.com/merrillrudd/LIME/blob/master/R/create_lh_list.R
      indexSfull <- which(round(dSC$proportion, 2) == 1.00)[1] # select first component
      lengthSfull <- dSC$length[indexSfull]
      right_dome <- indexSfull:(dim(dSC)[2])
      dSC$proportion <- dSC$proportion*exp(-0.5*((dSC$length-lengthSfull)/(req(input$SLDome)))^2)
      dSC$proportion[dSC$length < req(input$SLKnife)] <- 0
    }
    dSC
  })
  
  
  # plot selectivity pattern

  #observeEvent(
  #  input$btnFixedFleetPars, {
      output$plotSelectivityPattern <- plotly::renderPlotly({
        isolate(length_vals <- seq(min(lengthRecordsFilter()[,newLengthCol()], na.rm = TRUE), 
                           input$Linf, length.out = 201))
        if(req(input$specifySelectivity) == "Initial estimate" ){
          gg_line_types <- c(1,2)
          gg_titleplot <- "Maturity and selectivity (initial estimate)"
        } else if(input$specifySelectivity == "Fixed value") {
          gg_line_types <- c(1,1)
          gg_titleplot <- "Maturity and selectivity (fixed)"
        }
        ggdata <- rbind(selectionCurves(),
                        data.frame(length = length_vals, 
                                   proportion = 1.0/(1+exp(-log(19)*(length_vals-input$Lm50)/(input$Lm95-input$Lm50))),
                                   size = 2,
                                   quantity = "maturity")
        )
        expr = plotly::ggplotly(ggplot(ggdata) + 
                          geom_line(aes( x = length, y = proportion, colour = quantity, size = size,
                                         linetype = quantity)) +
                          scale_colour_manual(values = c("red", "black")) +
                          scale_linetype_manual(values = gg_line_types) +
                          scale_size_identity() + 
                          labs(title = gg_titleplot) + # could have reactive?
                          theme_bw())
      })
#    })
      
      # sliders for plot??
      # You can use a simple observeEvent to detect when button pressed, 
      # and then show the hidden sliderInput and plotOutput widgets
      # output$selectivityControls <- renderUI({
      #   
      # })
  

  # eventReactive??
  # slideLenBins <- reactive(
  #   {# eventually have a reactive StockPars object
  #     #                    StockPars <- list(MaxSD = input$MaxSD,
  #     #                                      CVLinf = input$CVLinf,
  #     #                                      Linc = input$Linc,
  #     #                                      Linf = input$sliderLinf)
  #     StockPars <- setLHPars()
  #     
  #     SizeBins <- list(Linc = input$Linc, ToSize = NULL)
  #     SizeBins$ToSize <- StockPars$Linf * (1 + StockPars$MaxSD*StockPars$CVLinf)
  #     
  #     LenBins <- seq(from=0, to=SizeBins$ToSize, by=SizeBins$Linc)
  #   }
  # )
  
  # create and bin length data
  createLengthBins <- reactive({
    # eventReactive(input$btnStockPars
    StockPars <- setLHPars()
    StockPars$MaxSD <- input$MaxSD
    binwidth <- input$Linc  # in fitLIME: binwidth <- isolate(input$Linc)
    
    if(input$lengthBasedAssessmentMethod == "LB-SPR"){
      # SizeBins object for LBSPR
      SizeBins <- list(Linc = binwidth, ToSize = NULL)
      SizeBins$ToSize <- StockPars$Linf * (1 + StockPars$MaxSD*StockPars$CVLinf)
      LenBins <- seq(from=0, to=SizeBins$ToSize, by=SizeBins$Linc)
      LenMids <- seq(from=0.5*SizeBins$Linc, 
                     by=SizeBins$Linc, length.out=(length(LenBins)-1))
    } else if(input$lengthBasedAssessmentMethod == "LIME") {
      ## length bins (LIME)
      # see https://github.com/merrillrudd/LIME/blob/master/R/create_lh_list.R
      mids <- seq((binwidth/2), StockPars$Linf*1.3, by=binwidth) 
      highs <- mids + (binwidth/2)
      lows <- mids - (binwidth)/2
      LenBins <- c(lows[1],highs)
      LenMids <- mids
    }
    
    # bins and mid points
    list_out <- NULL
    if((input$lengthBasedAssessmentMethod == "LB-SPR")){
      list_out <- list(SizeBins = SizeBins) # SizeBins for LB-SPR only
    }
    list_out <- c(list_out, 
                  list(LenBins = LenBins,
                       LenMids = LenMids
                  )
    )
  })
  
  
  # bin length records - may depend on year also
  binLengthData <- reactive({
    # length bins
    lengthBins <- createLengthBins()$LenBins
    lengthMids <- createLengthBins()$LenMids
    
    # length records
    length_records <- lengthRecordsFilter()
    
    # length column
    length_col <- newLengthCol()
    
    validate(
      need(max(lengthBins) > max(length_records[, length_col], na.rm = TRUE),
                                 "Increase Linf (or CVLinf and MaxSD) so maximum length bin > max fish length !!")
    )
    
    # check for year attribute
    year_col <- names(length_records)[grepl("year", names(length_records), ignore.case = TRUE)]
    if(any(grepl("year", names(length_records), ignore.case = TRUE))){
      year_min <- min(length_records[, year_col])
      year_max <- max(length_records[, year_col])
    }

    # vulnerable to fishery 
    #isVulnerable <- (lengthMids >= input$MLL)
    MLL <- max(ifelse(is.null(setFleetPars()$SLMin), NA , setFleetPars()$SLMin), input$MLL, na.rm = TRUE)
    
    # bin length data
    # aggregate all years of data
    if(input$analyseLengthComposition == "all periods"){

      LF <- hist(na.omit(length_records[, length_col]), 
                 plot = FALSE, breaks = lengthBins, right = FALSE)$counts
      LFVul <- hist(na.omit(length_records[length_records[,length_col] >= MLL, length_col]), 
                    plot = FALSE, breaks = lengthBins, right = FALSE)$counts
      
      LF <- matrix(LF, nrow = 1, ncol = length(LF), 
                   dimnames = list("all periods", as.character(lengthBins)[-1]))
      LFVul <- matrix(LFVul, nrow = 1, ncol = length(LFVul), 
                      dimnames = list("all periods", as.character(lengthBins)[-1]))
    } else if(input$analyseLengthComposition == "annual") {

        # "annual" only an option if year column present (length(year_col) > 0)
        # drop any entry with NA years or lengths
        if(input$lengthBasedAssessmentMethod == "LIME"){
          LF <- createAnnualLF(length_records, length_col, year_col, lengthBins, allYears = TRUE)
          LFVul <- createAnnualLF(length_records[length_records[,length_col] >= MLL,], 
                                  length_col, year_col, lengthBins, allYears = TRUE)
        } else if(input$lengthBasedAssessmentMethod == "LB-SPR"){
          LF <- createAnnualLF(length_records, length_col, year_col, lengthBins, allYears = FALSE)
          LFVul <- createAnnualLF(length_records[length_records[,length_col] >= MLL,], 
                                  length_col, year_col, lengthBins, allYears = FALSE)
        }
    }

    # binned counts
    list(LenDat = LF, LenDatVul = LFVul)
  })
  
  # app navigation
  
  # after button clicks
  observeEvent(input$convertLengthUnits,
               updateTabsetPanel(session, inputId = "tabMain", selected = "tabLHP"))  
  
  observeEvent(input$btnFixedFleetPars,
               updateNavbarPage(session, inputId = "methodLBSPR", selected = "tabMethodParameters")
               )

  observeEvent(input$btnLengthComposition,
              updateNavbarPage(session, inputId = "methodLBSPR", selected = "tabSelectivity")
  )
  
  # btnStockPars causes move to next tab
  observeEvent(input$btnStockPars, {
    updateTabsetPanel(session, inputId = "tabMain", selected = "tabLBA")
  })
  
  # visualise length composition by year - optional selection radio button
  # observeEvent(input$selectCols,
  #              { rbChoices <- c("in aggregate")
  #              if(any(grepl("year", input$checkboxCatchData, ignore.case = TRUE))){
  #                if(input$lengthBasedAssessmentMethod == "LB-SPR") {
  #                  rbChoices <- c(rbChoices, 
  #                                 paste0("by ", 
  #                                        grep("year", input$checkboxCatchData, ignore.case = TRUE, value = TRUE))
  #                  )
  #                } else if(input$lengthBasedAssessmentMethod == "LIME"){
  #                  rbChoices <- paste0("by ", grep("year", input$checkboxCatchData, ignore.case = TRUE, value = TRUE))
  #                  print(rbChoices)
  #                }
  #              }
  #              
  #              updateRadioButtons(inputId = "analyseLengthComposition",
  #                                 label = "Visualise...",
  #                                 choices = rbChoices) 
  #              #, selected = "in aggregate")
  #              })
  collateLengthChoice <- reactive({
    req(catchdata_table(), input$lengthColSelect)
    # input$checkboxCatchData may not have "year" but catchdata_table() will
    lengthRecordAttributes <- setdiff(colnames(catchdata_table()), paste0(input$lengthColSelect))
    if(input$lengthBasedAssessmentMethod == "LB-SPR"){
      collationChoice <- "all periods"
      if(any(grepl("year", lengthRecordAttributes, ignore.case = TRUE))){
        collationChoice <- c(collationChoice, "annual")
      } 
    } else if(input$lengthBasedAssessmentMethod == "LIME"){
      if(any(grepl("year", lengthRecordAttributes, ignore.case = TRUE))){
        collationChoice <- "annual"
      } else {
        collationChoice <- "all periods"
      }
    }
    collationChoice
  })
  
  observe({ 
    updateRadioButtons(inputId = "analyseLengthComposition",
                       label = "Assessment - temporal basis",
                       choices = collateLengthChoice())#, selected = defaultChoice)
  })
  
  observe({  
    updateActionButton(inputId = "fitLBA",
                        label = paste0("Apply ", input$lengthBasedAssessmentMethod))
  })
  

  # collate length data
  lengthDataInput <- reactive({
    # length data
    length_records <- lengthRecordsFilter()
    length_col <- newLengthCol()
    length_records$isVulnerable <- length_records[,length_col] >= input$MLL
    
    # is year column present? rename as 'year' for facet_wrap
    year_col <- names(length_records)[grepl("year", names(length_records), ignore.case = TRUE)]
    if(is.null(year_col) | input$analyseLengthComposition == "all periods"){
      length_records <- length_records %>%
        dplyr::select(!!ensym(length_col), isVulnerable) %>%
        dplyr::mutate(year = "all periods")
    } else {
      names(length_records)[grepl("year", names(length_records), ignore.case = TRUE)] <- "year"
    }
    
    list("lengthRecords" = length_records,
         "lengthCol" = length_col)
  })
  
  # plot length composition of filtered data - change with slider input
  # output$plotLengthComposition <- 
  #   plotly::renderPlotly({
  #     ggLengthComp <-  
  #     expr = plotly::ggplotly(ggLengthComp) %>% plotly::layout(legend = list(orientation = "h"))
  #   })
  lengthComposition <- lcPlotServer("lCMLL", lengthDataInput, createLengthBins, reactive(input$Linc), reactive({input$MLL}))

  output$plotLengthCompSelect <- plotly::renderPlotly({
    ggdata <- selectionCurves()
    gg_lty <- ifelse(req(input$specifySelectivity) == "Initial estimate", 2, 1) # if(input$specifySelectivity == "Fixed value") { gg_lty <- 1 }
    
    catchLHist <- max_counts_per_year(lengthData = lengthDataInput()$lengthRecords, lengthCol = lengthDataInput()$lengthCol, 
                        lengthBins = createLengthBins()$LenBins, analyseLengthComposition =input$analyseLengthComposition,
                        statHist = lengthComposition$statHist())

    # scale selectivity curves
    ggyear <- expand.grid(lengthCol = ggdata$length, year = catchLHist$year)
    ggyear$proportion <- rep(ggdata$proportion, dim(catchLHist)[1])
    if(lengthComposition$statHist() == "count"){
      ggyear$scaled_proportion <- as.vector(outer(ggdata$proportion, catchLHist$maxCount))
    } else if(lengthComposition$statHist() == "proportion"){
      ggyear$scaled_proportion <- as.vector(outer(ggdata$proportion, catchLHist$maxProportion))
    } else if(lengthComposition$statHist() == "density"){
      ggyear$scaled_proportion <- as.vector(outer(ggdata$proportion, catchLHist$maxDensity))
    }
    
    pg <- lengthComposition$pg() +
      geom_line(data = ggyear,
                mapping = aes(x = lengthCol, y = scaled_proportion), 
                linetype = gg_lty, colour = "red", size = 1) +
      scale_size_identity()
    expr = ggplotly_config(pg, "plotLengthCompSelectivity")
  })


  # Fit LBA ####
  fitLBSPR <- eventReactive(
    input$fitLBA,
    {
      StockPars <- setLHPars()
      StockPars$MK <- StockPars$M/StockPars$K
      StockPars <- append(StockPars, 
                    list(FecB = input$FecB, 
                         Steepness = input$Steepness,
                         Mpow = input$Mpow,
                         MaxSD = input$MaxSD,
                         NGTG = input$NGTG)) # "technical" parameter?
      
      fixedFleetPars <- NULL
      allFleetPars <- setFleetPars()
      if(input$specifySelectivity == "Fixed value" & !allFleetPars$selexParsEstimate){
        titleFitPlot <- "User-specified selectivity parameters"
        fixedFleetPars <- allFleetPars[names(allFleetPars) != "selexParsEstimate"]
      } else if(input$specifySelectivity == "Initial estimate" & allFleetPars$selexParsEstimate) {
        titleFitPlot <- "User-specified selectivity parameters"
        fixedFleetPars <- allFleetPars[names(allFleetPars) != "selexParsEstimate"]
        initialFleetPars <- list(SL1 = allFleetPars$SL1, SL2 = allFleetPars$SL2)
      }
      fixedFleetPars
      names(fixedFleetPars)[names(fixedFleetPars) == "selexCurve"] <- "selectivityCurve"
      
      titleFitPlot <- "Model-estimated selectivity parameters"
      if(input$specifySelectivity == "Fixed value"){
        titleFitPlot <- "User-specified selectivity parameters"
      }

      # bin length data
      SizeBins <- createLengthBins()$SizeBins
      LenBins <- createLengthBins()$LenBins
      LenMids <- createLengthBins()$LenMids
      LenDat <- binLengthData()$LenDat
      LenDatVul <- binLengthData()$LenDatVul
      years <- rownames(LenDatVul)
      
      NatL_LBSPR <- NULL
      lbsprPars <- NULL
      lbsprStdErrs <- NULL
      mlePars <- NULL
      MLE <- NULL
      sprVar <- NULL
      optimOut <- vector("list", length = length(years))

      for (yearLBSPR in years){
        
        LenDatIn <- LenDatVul[which(rownames(LenDatVul) == yearLBSPR),]  
        
        # GTG-LBSPR optimisation
        optGTG <- optLBSPRDome(StockPars,  fixedFleetPars, LenDatIn, SizeBins, "GTG")
        
        optimOut[[which(years == yearLBSPR)]] <- optGTG$optimOut
        names(optimOut)[which(years == yearLBSPR)] <- paste0("lbspr_",yearLBSPR)
        
        if(input$specifySelectivity == "Fixed value"){
          optFleetPars <- list(FM = optGTG$lbPars[["F/M"]],
                               selectivityCurve = optGTG$fixedFleetPars$selectivityCurve,
                               SL1 = fixedFleetPars$SL1, 
                               SL2 = fixedFleetPars$SL2,
                               SLMin = fixedFleetPars$SLMin,
                               SLmesh = fixedFleetPars$SLmesh)
        } else if(input$specifySelectivity == "Initial estimate") {
          optFleetPars <- list(FM = optGTG$lbPars[["F/M"]], 
                               selectivityCurve = optGTG$fixedFleetPars$selectivityCurve,
                               SL1 = optGTG$lbPars[["SL50"]], 
                               SL2 = optGTG$lbPars[["SL95"]])
        }

        # per recruit theory simulation - called in DoOptDome also
        prGTG <- simLBSPRDome(StockPars, optFleetPars, SizeBins)
        
        # configure outputs
        # ifelse statement depending on selectivity curve
        if(input$selectSelectivityCurve == "Logistic"){
          VulLen2 <- 1.0/(1+exp(-log(19)*(LenMids-optFleetPars$SL1)/(optFleetPars$SL2-optFleetPars$SL1))) # Selectivity-at-Length
        } else if(input$selectSelectivityCurve == "Normal.loc") {
          VulLen2 <- exp(-0.5*((LenMids-((optFleetPars$SL1)*optFleetPars$SLmesh))/(optFleetPars$SL2))^2)
          VulLen2[LenMids < optFleetPars$SLMin] <- 0
        } else if(input$selectSelectivityCurve == "Normal.sca") {
          VulLen2 <- exp(-0.5*((LenMids-((optFleetPars$SL1)*optFleetPars$SLmesh))/(optFleetPars$SLmesh*(optFleetPars$SL2)^0.5)^2))
          VulLen2[LenMids < optFleetPars$SLMin] <- 0
        } else if(input$selectSelectivityCurve == "logNorm") {
          VulLen2 <- exp(-0.5*((log(LenMids)-log((optFleetPars$SL1)*optFleetPars$SLmesh))/(optFleetPars$SL2))^2)
          VulLen2[LenMids < optFleetPars$SLMin] <- 0
        } else if(input$selectSelectivityCurve == "Knife-edged"){
          VulLen2 <- rep(1, length(LenMids))
          VulLen2[LenMids < optFleetPars$SLMin] <- 0
        }

        optVarcov <- optGTG$optVarcov
        # calculate variance in selectivity-at-length
        if(input$specifySelectivity == "Initial estimate") {
          deltaSLF <- varFishingAtLength(optGTG$optimOut$par, optVarcov, optFleetPars, StockPars, LenMids)
          lbsprPars <- rbind(lbsprPars,
                             data.frame(FM = unname(optFleetPars$FM), 
                                        SL50 = unname(optFleetPars$SL1), 
                                        SL95 = unname(optFleetPars$SL2),
                                        SPR = prGTG$SPR, 
                                        row.names = yearLBSPR)
          )
        } else {
          lbsprPars <- rbind(lbsprPars,
                             data.frame(FM = unname(optFleetPars$FM), 
                                        SPR = prGTG$SPR, 
                                        row.names = yearLBSPR))
        }
        
        lbsprStdErrs <- rbind(lbsprStdErrs, optGTG$lbStdErrs)
        
        # numbers-at-length (midpoints) LBSPR
        NatL_LBSPR <- rbind(NatL_LBSPR,
                            data.frame(year = yearLBSPR,
                                       length_mid = prGTG$LenMids,
                                       catchFished_at_length = prGTG$LCatchFished/max(prGTG$LCatchFished),
                                       catchUnfished_at_length = prGTG$LCatchUnfished/max(prGTG$LCatchUnfished),
                                       selectivityF_at_length = VulLen2,
                                       varSelectivityF_at_length = ifelse(input$specifySelectivity == "Initial estimate",
                                                                          deltaSLF$varSLF,
                                                                          NA),
                                       popUnfished_at_length = prGTG$LPopUnfished/max(prGTG$LPopUnfished),
                                       popFished_at_length = prGTG$LPopFished/max(prGTG$LPopFished) #standardised??
                            ))
        
        mlePars <- rbind(mlePars, optGTG$optimOut$par)
        mleDF <- optGTG$MLE
        mleDF$Parameter <- paste(mleDF$Parameter, yearLBSPR, sep = ".")
        MLE <- rbind(mleDF, MLE)
        # Parameter = c("FM", "SL50", "SL95", "SPR"),
        # Description = c("F/M: relative fishing mortality",
        #                 "Length at 50% selectivity",
        #                 "Length at 95% selectivity",
        #                 "Spawning Potential Ratio"),
        
        #        opModelOut <- data.frame(Parameter = c("SPR", "YPR"),
        #                                 Description = c("Spawning Potential Ratio", "Yield-per-recruit"),
        #                                 Estimate = c(prGTG$SPR, prGTG$YPR))
        
        sprVar <- rbind(sprVar, varSPR(optGTG$optimOut$par, optVarcov, optFleetPars, StockPars, SizeBins))
        
      }
      if(input$specifySelectivity == "Fixed value"){
        if(input$selectSelectivityCurve == "Logistic"){
          names(fixedFleetPars)[names(fixedFleetPars) == "SL50"] <- "SL50 (fixed)"
          names(fixedFleetPars)[names(fixedFleetPars) == "SL95"] <- "SL95 (fixed)"
        # } else if(input$selectSelectivityCurve == "Dome-shaped"){
        #   names(lbsprPars)[names(lbsprPars) == "SL50"] <- "SL1"
        #   names(lbsprPars)[names(lbsprPars) == "SL95"] <- "SL2"
        }
      }
      
      mlePars <- data.frame(mlePars)
      if(dim(mlePars)[2] == 3) {
        colnames(mlePars) <- c("log_FM", "log_SL50", "log_SLdelta")
      } else {
        colnames(mlePars) <- c("log_FM")
      }
      mlePars <- cbind(year = years, mlePars)
      
      
      list(NatL_LBSPR = NatL_LBSPR,
           lbsprPars = lbsprPars,
           lbsprStdErrs = lbsprStdErrs,
           optimOut = optimOut,
           mlePars = mlePars,
           MLE = MLE,
           fixedFleetPars = fixedFleetPars,
           sprVar = sprVar
           )
    }
  )
  

  fitLIME <- reactive(
    {
      lhParVals <- isolate(setLHPars()) # only evaluated if fitLIME called
      lhParVals$maturity_input <- "length"
      fleetParVals <- isolate(setFleetPars()) 
      binwidth <- isolate(input$Linc)
      # selectivity parameters fitted or estimated
      titleFitPlot <- "Model-estimated selectivity parameters"
      if(input$specifySelectivity == "Fixed value"){
        titleFitPlot <- "User-specified selectivity parameters"
      }

      # how to handle specifying/estimating SL50/SL95 for logistic
      if(tolower(fleetParVals$selexCurve) == "logistic"){
          S50 <- fleetParVals$S50  # initial estimate of selectivity-at-length 50%
          S95 <- fleetParVals$S95
          dome_sd <- NULL
      } else if(grepl("dome", fleetParVals$selexCurve, ignore.case = TRUE)) {
        cat("no app support for dome-shaped LIME as of yet - see LIME package or LIME shiny app for dome-shaped support")
        # place-holders
        S50 <- fleetParVals$S50
        S95 <- fleetParVals$S95 # avoid S95 NULL issues in create_lh_list
        dome_sd <- fleetParVals$dome_sd
      }

      lh <- LIME::create_lh_list(vbk= lhParVals$K,    # vb growth coefficient
                           linf= lhParVals$Linf,  # vbg Linf
                           t0= input$slidert0,  
                           lwa=lhParVals$Walpha,     # length-weight W = aL^b: a  
                           lwb=lhParVals$Wbeta,     # length-weight W = aL^b: b
                           S50=S50, #50,  # selectivity-at-length 50%
                           S95=S95, #95,  # selectivity-at-length 95%
                           dome_sd = dome_sd,
                           selex_input=fleetParVals$selexBy,# "length"
                           selex_type=tolower(fleetParVals$selexCurve), # fleetParVals$selex_type
                           M50=lhParVals$L50,     # length at 50% maturity
                           maturity_input="length", #lhParVals$maturity_input,
                           M=lhParVals$M,     # natural mortality
                           binwidth=binwidth,
                           CVlen=lhParVals$CVLinf,  # coefficient of variation with length
                           #technical parameters through output$tableTechnicalParameters
                           SigmaR=input$SigmaR, # standard deviation in recruitment process
                           SigmaF=input$SigmaF,
                           SigmaC=input$SigmaC,
                           SigmaI=input$SigmaI,
                           R0=input$R0,
                           qcoef=input$qcoef,
                           start_ages=input$start_ages,
                           theta=input$theta,
                           nseasons=input$nseasons,
                           nfleets=1 # fleetParVals$nfleets
                           ) # omit Frate, Fequil, rho (simulation-only) parameters

      # length records
      length_records <- isolate(lengthRecordsFilter())
      lengthCol <- isolate(newLengthCol())
      yearCol <- names(length_records)[grepl("year", names(length_records), ignore.case = TRUE)]
      lengthBins <- isolate(createLengthBins()$LenBins)
      lengthMids <- isolate(createLengthBins()$LenMids)
      
      # # lengthbins vulnerable to fishery
      # isVulnerable <- (lengthMids >= input$MLL)  # lengthBins[-1] > input$MLL
      # 
      # # years 
      # yearCol <- names(length_records)[grepl("year", names(length_records), ignore.case = TRUE)]
      # 
      # if(any(grepl("year", names(length_records), ignore.case = TRUE))){
      #   year_min <- min(length_records[, yearCol])
      #   year_max <- max(length_records[, yearCol])
      # }
      # 
      # 
      # # collate length data in LIME format #### 
      # # Q should upper limit be 
      # # ---------------> lh$linf*(1 + lh$CVlen)
      # # or ------------> lh$binwidth*ceiling(lh$linf*(1 + lh$CVlen)/lh$binwidth)
      # lrLIME <- length_records[, c(yearCol, lengthCol)] %>% na.omit() %>%
      #   dplyr::mutate(lengthBin = cut(!!ensym(lengthCol), breaks = lengthBins, 
      #                          right = FALSE),
      #          year = factor(!!ensym(yearCol), levels = seq(year_min, year_max, 1)))  %>% # lengthMids??
      #   dplyr::group_by(year, lengthBin) %>%
      #   dplyr::summarise(nFish = n())
      # 
      # 
      # # configure length data for LIME
      # lfLIME <- xtabs(formula = nFish ~ year + lengthBin, data = lrLIME)
      # 
      # # 26/07/2021
      # #lengthBins <- seq(0, lh$binwidth*ceiling(lh$linf*(1 + lh$CVlen)/lh$binwidth), lh$binwidth)
      # #lengthMids <- seq(from=0.5*lh$binwidth, by=lh$binwidth, length.out=length(lengthBins)-1)
      # lfLIME <- lfLIME*outer(rep.int(1, nrow(lfLIME)), isVulnerable)
      lfLIME <- binLengthData()$LenDatVul

      # lfLIME col.name with upper end of length bins or mid-bin??
      colnames(lfLIME) <- as.character(lengthBins)[-1]#as.character(lengthMids)
      
      
      # array LF[,,]
      LF <- array(data = NA, 
                  dim = c(dim(lfLIME),1), 
                  dimnames = list(rownames(lfLIME), colnames(lfLIME), NULL))
      LF[,,1] <- lfLIME
      
      neff_ft <- array(apply(LF,1,"sum"),
                       dim = c(1,dim(LF)[1]), 
                       dimnames = list(NULL,rownames(lfLIME)))
      

      # prepare LIME inputs
      data_all <- list("years"=as.numeric(dplyr::first(rownames(lfLIME))):
                         as.numeric(dplyr::last(rownames(lfLIME))), 
                       "LF"=LF,  
                       "neff_ft"= neff_ft)

      inputs_all <- LIME::create_inputs(lh=lh, input_data=data_all)

      # vals_selex_ft = FALSE
      vals_selex_ft_LIME <- -1
      if(!fleetParVals$est_selex_f){
        vals_selex_ft_LIME <- lh$S_fl
      }
      
      #  run_LIME ####
      start <- Sys.time()
      lc_only <- LIME::run_LIME(modpath=NULL, 
                          input=inputs_all,
                          data_avail="LC",
                          SigRprior = c(input$SigmaR, 0.3),
                          est_selex_f = fleetParVals$est_selex_f,
                          vals_selex_ft = vals_selex_ft_LIME)
      end <- Sys.time() - start
      # outputs
      list(length_data_raw = length_records[, c(yearCol, lengthCol)] %>% na.omit(),
           LF = LF, lc_only = lc_only, lh = lh)
    }
  )
  
  
  # move panel within navBarPage after fit
  observeEvent(input$fitLBA,
               {updateNavbarPage(session, inputId = "methodLBSPR", selected = "tabModelFit")
                 if(input$lengthBasedAssessmentMethod == "LIME") {
                   limeFitFailStatus <-  all(is.na(fitLIME()$lc_only$opt$final_gradient)) & !("Convergence_check" %in% names(fitLIME()$lc_only$opt))
                   showModal(modalDialog(
                     title = "TMBhelper convergence check",
                     ifelse(limeFitFailStatus, 
                            paste("Model not converged:", names(fitLIME()$lc_only$opt), "=", fitLIME()$lc_only$opt$final_gradient, sep = " "), 
                            fitLIME()$lc_only$opt$Convergence_check),
                     easyClose = TRUE,
                     size = "m"), session)
                 }
               })
  
  
  # print text on LBSPR estimating model fit 
  output$tableLBAEstimates <- reactive(
    {
      
    if(input$lengthBasedAssessmentMethod == "LB-SPR"){
      #lbsprPars <- fitLBSPR()$lbsprPars
      mlePars <- fitLBSPR()$mlePars
      # three significant figures
      mlePars[sapply(mlePars, is.numeric)] <- signif(mlePars[sapply(mlePars, is.numeric)], 3)
      #lbsprPars[ ,colnames(lbsprPars)!= "SPR"] %>%
      mlePars %>%
        knitr::kable("html", digits = 3) %>%
        kableExtra::kable_styling("striped", full_width = F, position = "float_left")
    } else if(input$lengthBasedAssessmentMethod == "LIME"){
      limeFit <- fitLIME()$lc_only
      yearsLIME <- row.names(fitLIME()$LF)

      dfLIME <- data.frame(year = c(yearsLIME, rep("all",4)),
                           quantity = c(rep("logF", length(yearsLIME)), "log_S50_f", "log_Sdelta_f", "log_sigma_R", "log_theta"),
                           estimate = NA,
                           std_error = NA)
      if(!all(is.null(limeFit$Report)) & !all(is.na(limeFit$Report))){
        # fishing mortality
        log_fishing <- summary(limeFit$Sdreport)[which(rownames(summary(limeFit$Sdreport))=="lF_y"),]
        
        # recruitment parameter
        log_sigma_R <- summary(limeFit$Sdreport)[which(rownames(summary(limeFit$Sdreport))=="log_sigma_R"),]
        
        # Dirichlet-multinomial parameter
        log_theta <- summary(limeFit$Sdreport)[which(rownames(summary(limeFit$Sdreport))=="log_theta"),]
        
        # length-at-selectivity parameters
        if(input$specifySelectivity == "Initial estimate"){
          log_S50_f <- summary(limeFit$Sdreport)[which(rownames(summary(limeFit$Sdreport))=="log_S50_f"), ]
          log_Sdelta_f <- summary(limeFit$Sdreport)[which(rownames(summary(limeFit$Sdreport))=="log_Sdelta_f"),]
          
          dfLIME <- data.frame(year = c(yearsLIME, rep("all",4)),
                               quantity = c(rep("logF", length(yearsLIME)), "log_S50_f", "log_Sdelta_f", "log_sigma_R", "log_theta"),
                               estimate = c(log_fishing[ ,"Estimate"], log_S50_f["Estimate"], log_Sdelta_f["Estimate"],
                                            log_sigma_R["Estimate"], log_theta["Estimate"]),
                               std_error = c(log_fishing[ ,"Std. Error"], log_S50_f["Std. Error"], log_Sdelta_f["Std. Error"],
                                             log_sigma_R["Std. Error"], log_theta["Std. Error"])
          )
        } else if(input$specifySelectivity == "Fixed value") {
          dfLIME <- data.frame(year = c(yearsLIME, rep("all",2)),
                               quantity = c(rep("logF", length(yearsLIME)), "log_sigma_R", "log_theta"),
                               estimate = c(log_fishing[ ,"Estimate"], log_sigma_R["Estimate"], log_theta["Estimate"]),
                               std_error = c(log_fishing[ ,"Std. Error"], log_sigma_R["Std. Error"], log_theta["Std. Error"])
          )
        }
      }
      dfLIME %>%
         knitr::kable("html", digits = 3) %>%
         kableExtra::kable_styling("striped", full_width = F, position = "float_left")
    }
    }
  )
    
  #renderPrint({
  #  expr = print(fitLBSPR()$lbsprPars)
  #})
  
  # # print text on LBSPR operating model output
  # output$textLBSPROpOut <- renderPrint({
  #   expr = print(fitLBSPR()$opModelOut)
  # })
  
  output$plotLBAModelFit <- plotly::renderPlotly({
    # length data
    length_records <- lengthDataInput()$lengthRecords
    length_col <- lengthDataInput()$lengthCol
    
    LenBins <- createLengthBins()$LenBins
    LenMids <- createLengthBins()$LenMids	
    LenDat <- binLengthData()$LenDatVul # vulnerable to fishery only
    if(input$lengthBasedAssessmentMethod == "LB-SPR"){
      NatL_LBSPR <- fitLBSPR()$NatL_LBSPR
      maxLengthYearVector <- apply(LenDat, 1, "max")
      maxLengthYear <- rep(maxLengthYearVector, each = dim(LenDat)[2])
      NatL_LBSPR$catchFished_at_length_count <- NatL_LBSPR$catchFished_at_length*maxLengthYear
      NatL_LBSPR$selectivityF_at_length_count <- NatL_LBSPR$selectivityF_at_length*maxLengthYear
      if(input$analyseLengthComposition == "annual"){
        NatL_LBSPR$year <- as.integer(NatL_LBSPR$year)
      }
      
      if(input$specifySelectivity == "Fixed value"){
        titleFitPlot <- "Length data, LB-SPR fit and selectivity curve (specified)"
      } else if (input$specifySelectivity == "Initial estimate") {
        titleFitPlot <- "Length data, LB-SPR fit and selectivity curve (estimated)"
      }
      
      # create ggplot with data...
      pg <- ggplot(length_records %>% dplyr::filter(isVulnerable)) + 
        geom_histogram(mapping = aes_string(x = length_col), breaks = LenBins, 
                       closed = "left", colour = "black", fill = "grey75")
      
      # ...and fit
      pg <- pg + 
        geom_area(data = NatL_LBSPR,
                  mapping = aes(x = length_mid, y = catchFished_at_length_count), 
                  fill = "salmon", alpha = 0.5) +
        geom_line(data = NatL_LBSPR,
                  mapping = aes(x = length_mid, y = selectivityF_at_length_count), 
                  colour = "red", lwd = 1) #+ 
      # labs(y = "count", #title = titleFitPlot,
      #      caption = paste("Data from", input$uploadFile[[1]], sep = " "))#+ 
      # scale_y_continuous(sec.axis = sec_axis(~  . /maxLenDat, name = "selectivity",
      # breaks = c(0, 1),
      # labels = c("0", "1")
      #                                        ) )
      
      expr = ggplotly_config(pg +
                        labs(y = "count", name = gsub("_", " (", length_col, ")" )) +
                        facet_wrap(vars(year)) + 
                        theme_bw() + 
                        lengthComposition$themeTweak(), "plotLCModelFit")
    } else if(input$lengthBasedAssessmentMethod == "LIME"){
      limeFit <- fitLIME()
      
      # length data and year attribute
      length_records <- limeFit$length_data_raw
      year_col <- names(length_records)[grepl("year", names(length_records), ignore.case = TRUE)]
      length_records$isVulnerable <- length_records[, length_col] >= input$MLL

      # predictions - code extracted from https://github.com/merrillrudd/LIME/blob/master/R/plot_LCfits.R
      nf <- limeFit$lc_only$input$nfleets
      Tyrs <- limeFit$lc_only$input$years
      
      pg <- ggplot(length_records %>% dplyr::filter(isVulnerable)) + 
        geom_histogram(aes(x = !!ensym(length_col), y = after_stat(density*width), fill = isVulnerable),
                       colour = "black", size = 0.25, breaks = LenBins, closed = "left") +
        scale_fill_manual(name = "observed \n data", values = c("grey75"), breaks = waiver(), guide = NULL) +
        labs(y = "catch proportion") +
        facet_wrap(as.formula(paste0(year_col," ~ .")))
      
      # Plot_LCfits code (modified)
      if(!all(is.null(limeFit$lc_only$Report)) & !all(is.na(limeFit$lc_only$Report))){
        Inputs <- limeFit$lc_only$Inputs
        Report <- limeFit$lc_only$Report
        
        # pred <- Report$plb
        pred <- lapply(1:nf, function(x){
          sub <- matrix(Report$plb[,,x], nrow=length(Tyrs))
          rownames(sub) <- Tyrs
          colnames(sub) <- colnames(Inputs$Data$LF_tlf)
          return(sub)
        })
        
        pred_df <- reshape2::melt(pred)
        names(pred_df) <- c(year_col , length_col, "proportion", "fleet")
        pred_df <- pred_df %>%
          dplyr::group_by(!!ensym(year_col), !!ensym(length_col), proportion, fleet)
        pred_df[[year_col]] <- as.integer(pred_df[[year_col]])
        pred_df[[length_col]] <- as.numeric(pred_df[[length_col]])
        pred_df$proportion <-  as.numeric(pred_df$proportion)
        pred_df$fleet <- factor(pred_df$fleet)
        pred_df2 <- pred_df %>% dplyr::mutate("Type"="Predicted") %>% dplyr::mutate("Model"="LIME")
        # plot_LCfits adaption
        pg <- pg + 
          geom_line(data=pred_df2 %>% dplyr::filter(Type=="Predicted"), 
                    aes(x=!!ensym(length_col), y=proportion, color=Model), alpha = 0.5, lwd=1.2) +
          scale_color_brewer(palette="Set1", direction=-1)
      }

      expr = ggplotly_config(pg + theme_bw() + lengthComposition$themeTweak(), "plotLCModelFit") %>% 
        plotly::layout(autosize = TRUE)
    }
    
  })
  
  

  # output$plotOpLBSPR <- plotly::renderPlotly({
  #   # operating model output based on estimating model fit
  #   NatL_LBSPR <- fitLBSPR()$NatL_LBSPR
  #   
  #   # pivot_longer
  #   NatL_long <- NatL_LBSPR %>%
  #     pivot_longer(cols = dplyr::ends_with("at_length"),
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
  #   expr = plotly::ggplotly(p = pg + theme_bw(),
  #                   height = 400, width = 600)
  # })

  
  # nlminb (optimisation function for matching expected per-recruit length composition to
  # multinomial log likelihood)
  output$textLBAModelFit <- renderPrint({
    if(input$lengthBasedAssessmentMethod == "LB-SPR"){
      expr <- print(fitLBSPR()$optimOut)  
    } else if(input$lengthBasedAssessmentMethod == "LIME") {
      expr <- print(fitLIME()$lc_only$opt)
    }
    
    
  })
  
  
  # table - exploitation parameter summary
  output$tableLBASummary <- reactive({
    # isolate reactive elements that may cause LIME re-evaluation
    specifySelectivity <- isolate(input$specifySelectivity)
    lbaMethod <- isolate(input$lengthBasedAssessmentMethod)
    
    if(specifySelectivity == "Fixed value"){
      tableData <- data.frame(Notation = c("M", "F", "Z", "SPR"),
                              Description = c("Natural mortality", "Fishing mortality", "Total mortality",
                                              "Spawning potential ratio")
      )
    } else {
      tableData <- data.frame(Notation = c("M", "F", "Z", "SL1", "SL2", "SPR"),
                              Description = c("Natural mortality", "Fishing mortality", "Total mortality",
                                              "Length-at-50%-selectivity", "Length-at-95%-selectivity", 
                                              "Spawning potential ratio")
      )
    }

    if(lbaMethod == "LB-SPR"){
      lbsprFit <- fitLBSPR()$lbsprPars
      #fixedFleetPars <- fitLBSPR()$fixedFleetPars
      lbsprStockInput <- setLHPars()
      lbsprGearInput <- setFleetPars()
      yearsLBA <- row.names(lbsprFit)
      M <- lbsprStockInput$M
      FM <- lbsprFit$FM
      SPR <- format(lbsprFit$SPR, digits = 3)
      if(specifySelectivity == "Initial estimate"){
        SL1 <- format(lbsprFit[,"SL50"], digits = 3)
        SL2 <- format(lbsprFit[,"SL95"], digits = 3)
      }
      for (iyear in seq_along(yearsLBA)){
        if(specifySelectivity == "Fixed value"){
          sel_pars <- NULL
        } else {
          sel_pars <- c(SL1[iyear], SL2[iyear])        
        }
        tableData[,paste0(yearsLBA[iyear])] <- 
          c(M, 
            format(M*FM[iyear], digits =3), 
            format(M*(1 + FM[iyear]), digits = 3), 
            sel_pars,
            SPR[iyear])
      }
    } else if(lbaMethod == "LIME") {
      limeFit <- fitLIME()$lc_only
      # limeFit$input$selex_type, limeFit$Report$F_t, yearsLBA = row.names(fitLIME()$LF)
      # ML95/M95 "length" calculated in create_lh_list() if not specified
      yearsLBA <- row.names(fitLIME()$LF) # could we obtain from limeFit??
      nYears <- length(yearsLBA)
      
      if(!all(is.null(limeFit$Report)) & !all(is.na(limeFit$Report))){
        F_t <- signif(limeFit$Report$F_t, 3)
        Z_t <- format(limeFit$Report$F_t + limeFit$input$M, digits = 3)
        SPR_t <- signif(limeFit$Report$SPR_t, 3)
      } else {
        F_t <- rep(NA, nYears)
        Z_t <- rep(NA, nYears)
        SPR_t <- rep(NA, nYears)
      }
      if(specifySelectivity == "Fixed value"){
        tableData[, yearsLBA] = rbind(rep(limeFit$input$M, nYears), F_t, Z_t, SPR_t)
      } else {
        if(!all(is.null(limeFit$Report)) & !all(is.na(limeFit$Report))){
          tableData[, yearsLBA] = rbind(rep(limeFit$input$M, nYears), F_t, Z_t, 
                                        rep(signif(limeFit$Report$S50_f, 3), nYears),
                                        rep(signif(limeFit$Report$S95_f, 3), nYears),
                                        SPR_t)
        } else {
          tableData[, yearsLBA] = rbind(rep(limeFit$input$M, nYears), F_t, Z_t, rep(NA, nYears), rep(NA, nYears), SPR_t)
        }
      }
    }
    
    if(specifySelectivity == "Fixed value"){
        tableData %>% knitr::kable("html") %>% 
          kableExtra::kable_styling("striped", full_width = F, position = "float_left") %>%
          kableExtra::pack_rows("Mortality", 1, 3) %>% 
          kableExtra::pack_rows("Status", 4, 4)
    } else {
        tableData %>% knitr::kable("html") %>% 
          kableExtra::kable_styling("striped", full_width = F, position = "float_left") %>%
          kableExtra::pack_rows("Mortality", 1, 3) %>% 
          kableExtra::pack_rows("Selectivity", 4, 5) %>%
          kableExtra::pack_rows("Status", 6, 6)
    }
    
    
  })  
  
  output$tableStockParameters <- reactive({
    
    # length-at-95%-maturity can be unspecified in LIME
    Lm95 <- as.numeric(ifelse(input$lengthBasedAssessmentMethod == "LB-SPR", input$Lm95, fitLIME()$lc_only$input$ML95))
    tableData <- data.frame(Notation = c("M", "Linf", "K", "Lm50", "Lm95", "SLCurve", "MLL"),
                            Description = c("Natural mortality", "Asymptotic length", "LVB growth constant", 
                                            "Length-at-50%-maturity", "Length-at-95%-maturity", 
                                            "Selectivity-at-length curve", "Minimum length limit"),
                            Value = c(as.numeric(input$M), as.numeric(input$Linf), as.numeric(input$kLvb), 
                                      as.numeric(input$Lm50), signif(Lm95, 3), tolower(input$selectSelectivityCurve),
                                      NA)
                            )
    
    if(input$lengthBasedAssessmentMethod == "LB-SPR"){
      lbsprGearInput <- setFleetPars() # SLMin
      tableData[tableData$Notation == "MLL",]$Value <- format(ifelse(is.null(lbsprGearInput$SLMin), input$MLL, lbsprGearInput$SLMin), 
                                                              digits = 3)
    } else if(input$lengthBasedAssessmentMethod == "LIME") {
      limeFit <- fitLIME()$lc_only
      tableData[tableData$Notation == "MLL",]$Value <- signif(input$MLL, digits = 3)
    }
    
    # if selectivity is also an input to stock assessment
    if(input$specifySelectivity == "Fixed value"){
      if(input$selectSelectivityCurve == "Logistic"){
        descript_select <- c("Length-at-50%-selectivity", "Length-at-95%-selectivity")
      } else {
        descript_select <- c("Length-at-max-selectivity", "Selectivity curve spread (sd)")
      }
      tableDataSelectivity <- data.frame(
        Notation = c("SL1", "SL2"), 
        Description = descript_select,
        Value = c(signif(input$SL1, digits = 3), 
                  signif(input$SL2, digits = 3))
      )
      tableData <- rbind(tableData, tableDataSelectivity)
    }
    
    tableData %>%
      knitr::kable("html", digits = c(3,3,3,3,3, NA, rep(3,dim(tableData)[1] - 6))) %>%
      kableExtra::kable_styling("striped", full_width = F, position = "float_left") %>%
      kableExtra::pack_rows("Mortality", 1, 1) %>%
      kableExtra::pack_rows("Growth", 2, 3) %>%
      kableExtra::pack_rows("Maturity", 4, 5) %>%
      kableExtra::pack_rows("Selectivity", 6, dim(tableData)[1]) 
  })
  
  
  # interpretation panel
  # visual comparison of exploited and unexploited fish populations
  output$plotCatchFishedUnfished <- plotly::renderPlotly({
    
    if(input$lengthBasedAssessmentMethod == "LB-SPR"){
      # data
      length_records <- lengthRecordsFilter()
      length_col <- newLengthCol()
      length_records$isVulnerable <- length_records[, newLengthCol()] >= input$MLL
      
      # lb-spr numbers-at-length
      NatL_LBSPR <- fitLBSPR()$NatL_LBSPR
      
      all_years <- unique(NatL_LBSPR$year)
      figs <- vector(mode = "list", length = length(all_years))
      annotations_ply <- vector(mode = "list", length = length(all_years))
      
      nsubplots <- length(all_years)
      nrows_max <- 3
      ncols_ply <- ceiling(nsubplots/nrows_max)
      nrows_ply <- ceiling(nsubplots/ncols_ply)
      margin_ply <- 0.025
      px <- ifelse(ncols_ply == 1, 1, 1 - (ncols_ply-1.25)*margin_ply)  # horizontal space available for plots
      py <- 1 - (nrows_ply-1)*margin_ply  # vertical space available for plots
      # plotly
      for (i_year in seq_along(all_years)) {
        # trace legends
        showlegendstatus <- ifelse(i_year == 1, TRUE, FALSE)
        
        # annotation positions
        
        
        # extract yearly data
        NLY <- NatL_LBSPR[NatL_LBSPR$year == all_years[i_year], ]
        catchAtLength <- rbind(data.frame(length = NLY$length_mid, 
                                          catch_standardised = NLY$catchFished_at_length, 
                                          exploitation = rep("fished", dim(NLY)[1])),
                               data.frame(length = NLY$length_mid,
                                          catch_standardised = NLY$catchUnfished_at_length, 
                                          exploitation = rep("unfished", dim(NLY)[1])))
        figs[[i_year]] <- 
          plotly::plot_ly(data = catchAtLength, x = ~length, y = ~catch_standardised, color = ~exploitation,
                  type = "scatter", mode = "lines+markers", colors = c('#ff7f0e', '#1f77b4'), 
                  showlegend = showlegendstatus) #%>%plotly::add_trace(x = ~length, y = ~catchnfished_at_length)
        irow <- floor((i_year-1)/ncols_ply)
        icol <- (i_year-1) %% ncols_ply
        
        annotations_ply[[i_year]] <- 
          list(x = margin_ply*1.75 + icol*((px/ncols_ply) + margin_ply), 
               y = (1-1.5*margin_ply) - (irow/nrows_ply), text = all_years[i_year], 
               xref = "paper", yref = "paper", xanchor = "center", yanchor = "bottom", 
               showarrow = FALSE, font = list(size = 16))
        
      }
      
      # add annotations
      pl_y <- plotly::subplot(figs, nrows = nrows_ply, shareX = TRUE, titleY = FALSE, margin = margin_ply) %>%
        plotly::layout(title = "Catch-length composition (standardised)",
               annotations = annotations_ply)
      # # plotly
      # pl_y <- plotly::plot_ly(data = NatL_LBSPR, 
      #                 x = ~ length_mid, y = ~ catchUnfished_at_length, name = "unfished", 
      #                 type = "scatter", mode = "lines+markers", frame = TRUE) %>% 
      #   plotly::add_trace(y = ~ catchFished_at_length, name = "fished", mode = "lines+markers")
      # # adding histogram/bar data difficult in plot_ly
      # #lengthData <- binLengthData()
      # # %>% add_bars(data = lengthData,
      # #           x = ~ LenMids, y = LenDatVul, name = "catch data") %>% 
      # # layout(barmode = "stack", bargap = 0.1)
      # pl_y <- pl_y %>% layout(xaxis = list(title = newLengthCol(), font = "f"),
      #                         yaxis = list(title = "numbers-at-length (standardised)", font = "f"))
      # #title = "Per recruit theory - catch")
    } else if(input$lengthBasedAssessmentMethod == "LIME") {
      pl_y <- plotly::plotly_empty() %>% 
        plotly::config(staticPlot = TRUE)
      
      # LIME - life history and model fit data
      limeFit <- fitLIME()
      lh_fit <- limeFit$lh
      limeFit <- limeFit$lc_only
      
      if(!all(is.null(limeFit$Report)) & !all(is.na(limeFit$Report))){
        Nyears_est <- limeFit$input$Nyears
        SPR_Nyear <- limeFit$Report$SPR_t[Nyears_est]
        
        
        lengthBins <- isolate(createLengthBins()$LenBins)
        lengthMids <- isolate(createLengthBins()$LenMids)
        
        # observation years (would prefer a better way - fleet independent)
        years_o <- which(limeFit$input$neff_ft != 0)
        
        # LIME fit of "probability of being in length bin" for final year
        catchFishedEst <- (limeFit$Report$plb[, ,1])/rowSums(limeFit$Report$plb[, ,1])
        NatL_LIME <- data.frame(length_mid = lengthMids,
                                catchFished =  catchFishedEst[Nyears_est,])
        
        # simulate fishing/no-fishing equilibrium
        # upper limit of 1.3*linf for length_mids/length_bins - more bins than in estimation model
        # model fit selectivity-at-length parameters
        # SigmaR = 0.1 for equilibrium recruitment
        lh_sim <- LIME::create_lh_list(vbk= lh_fit$vbk,    # vb growth coefficient
                                       linf= lh_fit$linf,# vbg Linf
                                       t0= lh_fit$t0,  
                                       lwa= lh_fit$lwa,# length-weight W = aL^b: a  
                                       lwb= lh_fit$lwb, # length-weight W = aL^b: b
                                       S50= limeFit$Report$S50_f,  # selectivity-at-length 50%
                                       S95= limeFit$Report$S95_f,  # selectivity-at-length 95%
                                       dome_sd = lh_fit$dome_sd,
                                       selex_input= lh_fit$selex_input,# "length"
                                       selex_type= lh_fit$selex_type, # "logistic"/"dome"
                                       maturity_input=lh_fit$maturity_input,#lhParVals$maturity_input,
                                       M50= lh_fit$ML50,      # length at 50% maturity
                                       M= lh_fit$M,     # natural mortality
                                       binwidth=lh_fit$binwidth,
                                       CVlen=lh_fit$CVlen,  # coefficient of variation with length
                                       #technical parameters through output$tableTechnicalParameters
                                       SigmaR = 0.01, # effective equilibrium recruitment
                                       SigmaF= 0.01, # effective equilibrium fishing
                                       SigmaC=lh_fit$SigmaC,
                                       SigmaI=lh_fit$SigmaI,
                                       R0=lh_fit$R0,
                                       Frate=lh_fit$Frate,
                                       Fequil=lh_fit$Fequil,
                                       qcoef=lh_fit$qcoef,
                                       start_ages= 0, #not available from lh$
                                       rho=lh_fit$rho,
                                       theta=lh_fit$theta,
                                       nseasons=lh_fit$nseasons,
                                       nfleets=1)
        
        # simulate fishing with sim_pop
        # @param lh list of life history attributes, output of create_lh_list
        # @param comp_sample vector of number of individuals sampled each year (set as 1 for proportions)
        # @param sample_type a character vector specifying if the length comps are sampled from the 'catch' (default) or from the population
        
        limeSimF0 <- LIME::sim_pop(lh_sim, Fdynamics = "None", Rdynamics = "Constant", 
                                   Nyears = 20, Nyears_comp = 1, comp_sample = 200, pool = TRUE,
                                   init_depl = 0.99, seed = 9999, sample_type = "catch",
                                   mgt_type = 'F', fleet_proportions = 1, nareas = 1)
        
        limeSimF <- LIME::sim_pop(lh_sim, Fdynamics = "Constant", Rdynamics = "Constant", 
                                  Nyears = 20, Nyears_comp = 1, comp_sample = 200, pool = TRUE,
                                  init_depl = SPR_Nyear, seed = 9999, sample_type = "catch",
                                  mgt_type = 'F', fleet_proportions = 1, nareas = 1)
        
        Nyears_sim <- limeSimF0$Nyears
        catchUnfished <- limeSimF0$plb[[1]]/rowSums(limeSimF0$plb[[1]])
        catchFished <- limeSimF0$plb[[1]]/rowSums(limeSimF$plb[[1]])
        
        NatL_LIME_Fsim <- data.frame(length_mid = limeSimF0$mids,
                                     catchUnfished = limeSimF0$plb[[1]][Nyears_sim,],
                                     catchFished = limeSimF$plb[[1]][Nyears_sim,])
        
        pl_y <- plotly::plot_ly(data = NatL_LIME, 
                                x = ~ length_mid, y = ~ catchFished, name = "fished - model fit", 
                                type = "scatter", mode = "lines+markers", frame = TRUE) %>%
          plotly::add_trace(data = NatL_LIME_Fsim, x = ~ length_mid, y = ~ catchUnfished, name = "unfished - equilibrium",
                            type = "scatter",mode = "lines+markers") %>% 
          plotly::add_trace(data = NatL_LIME_Fsim, x = ~ length_mid, y = ~ catchFished, name = "fished - equilibrium",
                            type = "scatter",mode = "lines+markers")
        pl_y <- pl_y %>% 
          plotly::layout(xaxis = list(title = newLengthCol(), font = "f"), 
                         yaxis = list(title = "numbers-at-length (standardised)", font = "f"))
      }
    }
    expr <- pl_y
  })
  
  
  
  # fishing estimate data for download and plotting
  fishingEstimates <- reactive({
    if(input$lengthBasedAssessmentMethod == "LIME"){
      limeFit <- fitLIME()$lc_only
      limeLH <- fitLIME()$lh
      
      yearLIME <- limeFit$input$years
      fishingMt <- rep(NA, length(yearLIME))
      Rt <- rep(NA, length(yearLIME))
      SPRt <- rep(NA, length(yearLIME))
      SBt <- rep(NA,length(yearLIME))
      MLt <- rep(NA,length(yearLIME))
      fleet_select <- data.frame(l_mid = limeLH$mids,
                                 S_l = NA)

      if(!all(is.na(limeFit$Sdreport)) & !all(is.null(limeFit$Report))){
      # fishing mortality
      indexFt <- names(limeFit$Sdreport$value) == "lF_t" # or lF_y
      # recruitment
      indexRt <- names(limeFit$Sdreport$value) == "lR_t"
      # SPR
      indexSPRt <- names(limeFit$Sdreport$value) == "SPR_t"
      # SB - spawning biomass
      indexSBt <- names(limeFit$Sdreport$value) == "lSB_t"
      # mean length
      indexMLt <- names(limeFit$Sdreport$value) == "ML_ft_hat"
      
      # selectivity
      indexS50 <- names(limeFit$Sdreport$value) == "S50_f"
      indexS95 <- names(limeFit$Sdreport$value) == "S95_f"
      

      
      stock_status <- rbind(
        data.frame(year = yearLIME,
                   quantity = "Fishing mortality",
                   log = TRUE,
                   mean = limeFit$Sdreport$value[indexFt],
                   sd = limeFit$Sdreport$sd[indexFt]),
        data.frame(year = yearLIME,
                   quantity = "Recruitment",
                   log = TRUE,
                   mean = limeFit$Sdreport$value[indexRt],
                   sd = limeFit$Sdreport$sd[indexRt]),
        data.frame(year = yearLIME,
                   quantity = "Spawning biomass",
                   log = TRUE,
                   mean = limeFit$Sdreport$value[indexSBt],
                   sd = limeFit$Sdreport$sd[indexSBt]),
        data.frame(year = yearLIME,
                   quantity = "Mean length",
                   log = FALSE,
                   mean = limeFit$Sdreport$value[indexMLt],
                   sd = limeFit$Sdreport$sd[indexMLt]),
        data.frame(year = yearLIME,
                   quantity = "SPR",
                   log = FALSE,
                   mean = limeFit$Sdreport$value[indexSPRt],
                   sd = limeFit$Sdreport$sd[indexSPRt])
      )
      
      fleet_select <- data.frame(l_mid = limeLH$mids,
                                 S_l = t(limeFit$Report$S_fl))
      }
      
    } else if(input$lengthBasedAssessmentMethod == "LB-SPR"){  
      lbsprPars <- fitLBSPR()$lbsprPars
      lbsprStdErrs <- fitLBSPR()$lbsprStdErrs
      NatL_LBSPR <- fitLBSPR()$NatL_LBSPR
      sprVars <- fitLBSPR()$sprVar
      StockPars <- setLHPars()
      
      # extract years
      lbsprPars$year <- row.names(lbsprPars)
      row.names(lbsprPars) <- NULL
      
      # absolute fishing mortality
      meanF <- lbsprPars$FM*StockPars$M
      stdErrFM <- lbsprStdErrs[, "F/M"]
      lowerciF <- (lbsprPars$FM - 1.96*stdErrFM)*StockPars$M
      upperciF <- (lbsprPars$FM + 1.96*stdErrFM)*StockPars$M
      dfMort <- data.frame(mortality = c(rep("fishing", length(meanF)), "natural"),
                           mean = c(meanF, StockPars$M),
                           lowerci = c(lowerciF, NA),
                           upperci = c(upperciF, NA),
                           year = c(lbsprPars$year, "all periods"))
      
      # delta method approximation
      meanSPR <- lbsprPars$SPR
      stderrSPR <- sqrt(sprVars)
      lowerciSPR <- ifelse(meanSPR - 1.96*stderrSPR < 0, 0, meanSPR - 1.96*stderrSPR)
      upperciSPR <- ifelse(meanSPR + 1.96*stderrSPR > 1, 1, meanSPR + 1.96*stderrSPR)
      
      # remove as.integer
      stock_status <- rbind(data.frame(year = lbsprPars$year,
                                       quantity = "F",
                                       mean = lbsprPars$FM*StockPars$M,
                                       sderr = stdErrFM,
                                       lowerci = ifelse((lbsprPars$FM - 1.96*stdErrFM)*StockPars$M < 0, 0, (lbsprPars$FM - 1.96*stdErrFM)*StockPars$M),
                                       upperci = (lbsprPars$FM + 1.96*stdErrFM)*StockPars$M),
                            data.frame(year =  lbsprPars$year,
                                       quantity = "SPR",
                                       mean = lbsprPars$SPR,
                                       sderr = stderrSPR,
                                       lowerci = ifelse(meanSPR - 1.96*stderrSPR < 0, 0, meanSPR - 1.96*stderrSPR),
                                       upperci = ifelse(meanSPR + 1.96*stderrSPR > 1, 1, meanSPR + 1.96*stderrSPR)),
                            data.frame(year =  lbsprPars$year,
                                       quantity = "M",
                                       mean = StockPars$M,
                                       sderr = NA,
                                       lowerci = NA,
                                       upperci = NA),
                            data.frame(year =  lbsprPars$year,
                                       quantity = "SPR_RP",
                                       mean = 0.4,
                                       sderr = NA,
                                       lowerci = NA,
                                       upperci = NA))
      
      # selectivity-at-length fishing
      fleet_select <- data.frame(year = NatL_LBSPR$year, 
                                 lengthMid = NatL_LBSPR$length_mid,
                                 meanSL = NatL_LBSPR$selectivityF_at_length)
      
      if(input$specifySelectivity == "Initial estimate" & !is.null(input$specifySelectivity)){
        # calculate variance, derive lower and upper (95%) confidence intervals
        fleet_select$sderrSL = sqrt(NatL_LBSPR$varSelectivityF_at_length)
        fleet_select$lowerciSL <- pmax(fleet_select$meanSL - 1.96*fleet_select$sderrSL, 0.0)
        fleet_select$upperciSL <- pmin(fleet_select$meanSL + 1.96*fleet_select$sderrSL, 1.0)
      }
    }
    list(stock_status = stock_status,  fleet_select = fleet_select)
  })
  
  obsMeanLengthMLL <- reactive({
    length_records <- lengthRecordsFilter()
    length_col <- newLengthCol()
    MLL <- max(ifelse(is.null(setFleetPars()$SLMin), NA , setFleetPars()$SLMin), input$MLL, na.rm = TRUE)
    length_records <- length_records[length_records[,length_col] >= MLL,]
    yearCol <- names(length_records)[grepl("year", names(length_records), ignore.case = TRUE)]
    stats::aggregate(length_records[sapply(length_records, is.numeric)], 
              by = list(year = length_records[, yearCol]), FUN = mean, na.rm = TRUE)
  })

  createPlotLBAestimates <- reactive({
    if(input$lengthBasedAssessmentMethod == "LIME"){
      lc_only <- fitLIME()$lc_only
      lh <- fitLIME()$lh
      if(!all(is.na(lc_only$Report)) & !all(is.null(lc_only$Report))){
        # Ft_uci <- max(lc_only$Report$F_t + 1.96*exp(lc_only$Sdreport$value[which(names(lc_only$Sdreport$value) == "lF_t")]))
        # p <- LIME::plot_output(Inputs=lc_only$Inputs,
        #                        Report=lc_only$Report,
        #                        Sdreport=lc_only$Sdreport,
        #                        lh=lh,
        #                        True=NULL,
        #                        plot=c("Fish","Rec","SPR","ML","SB","Selex"),
        #                        set_ylim=list("Fish"=c(0,Ft_uci*1.1),"SPR"=c(0,1)))
        outLIME <- collate_lime_output(lc_only$input, lc_only$Report, lc_only$Sdreport)
        lime_plots_th <- lapply(outLIME$limeTH, FUN = "gg_lime_time", outLIME$years_o)
        lime_plots_th$limeML <-
          lime_plots_th$limeML + 
          geom_point(data = obsMeanLengthMLL(), aes_string(x = "year", y = newLengthCol())) + 
          geom_line(data = obsMeanLengthMLL(), aes_string(x = "year", y = newLengthCol()), 
                    linewidth = 0.5)
        names(lime_plots_th) <- sub("lime", "plot", names(lime_plots_th))
        lime_plots_sel <- ggplot(outLIME$limeSelexF) +
          geom_line(aes(x = length, y = selectivity), colour = "green4", linewidth = 1) + 
          theme_bw()
        p <- append(lime_plots_th, list("plotSelexF" = lime_plots_sel))
      } else {
        # p <- LIME::plot_output(Inputs=lc_only$Inputs,
        #                        Report=NULL,
        #                        Sdreport=NULL,
        #                        lh=lh,
        #                        True=NULL,
        #                        plot=c("Fish","Rec","SPR","ML","SB","Selex"),
        #                        set_ylim=list("Fish" = c(0,1),"SPR" = c(0,1), "Rec" = c(0,1), 
        #                                      "ML" = c(0,100), "SB" = c(0,1), "Selex" = c(0,1)))
        # p <- ggplot() + 
        #   geom_blank()
        p <- rep(list(ggplot() + geom_blank()), 6)
      }
      
    } else if(input$lengthBasedAssessmentMethod == "LB-SPR"){
      fishingLBSPR <- fishingEstimates()$stock_status
      selectLBSPR <- fishingEstimates()$fleet_select
      
      DM <- fishingLBSPR %>% 
        dplyr::filter(quantity %in% c("F", "M")) %>% 
        dplyr::rename(mortality = quantity)
      DSPR <- fishingLBSPR %>% 
        dplyr::filter(quantity == "SPR")
      
      p <- ggplot_lbspr(DM, DSPR, DSLX = selectLBSPR, input$specifySelectivity, input$newLengthUnits, 
                        add_facet = FALSE)
    }
    p
  })
  
  # output$plotLBAestimate <- renderPlotly({
  #   browser()
  #   ply_gg <- lapply(createPlotLBAestimates(), plotly::ggplotly)
  #   subplot(ply_gg, nrows = 3)
  # })
  # renderPlotly
  output$plotFFit <- plotly::renderPlotly(
    ggplotly_config(createPlotLBAestimates()$plotF, 
                    paste0(tolower(sub("-", "", isolate(input$lengthBasedAssessmentMethod))), "PlotF")))
  output$plotSPRFit <- plotly::renderPlotly(
    ggplotly_config(createPlotLBAestimates()$plotSPR,
                    paste0(tolower(sub("-", "", isolate(input$lengthBasedAssessmentMethod))), "PlotSPR")))
  output$plotSelexFit <- plotly::renderPlotly(
    ggplotly_config(createPlotLBAestimates()$plotSelexF,
                    paste0(tolower(sub("-", "", isolate(input$lengthBasedAssessmentMethod))), "PlotSelexF")))

  # run insertRemovePlotServer code
  insertRemovePlotServer("limeLBSPR", reactive(input$lengthBasedAssessmentMethod), 
                         createPlotLBAestimates, reactive({input$fitLBA}))
  
  output$textFishingEstimateOutput <- renderText({
    if(input$lengthBasedAssessmentMethod == "LIME"){
      paste0("Plot code from https://github.com/merrillrudd/LIME/blob/master/R/plot_output.R")
    } else {
      NULL
    }
  })
  
  output$downloadStockStatusData <- downloadHandler(
    filename = function() {
      paste0(input$lengthBasedAssessmentMethod, "_stock_status_estimates-", Sys.Date(), ".csv")
    },
    content = function(fname) {
      write.csv(fishingEstimates()$stock_status, file = fname, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  output$downloadSelectivityData <- downloadHandler(
    filename = function() {
      paste0(input$lengthBasedAssessmentMethod, "_fleet_selectivity-", Sys.Date(), ".csv")
    },
    content = function(fname) {
      write.csv(fishingEstimates()$fleet_select, file = fname, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  output$downloadFishingPlot <- downloadHandler(
    filename = function() {
      paste0("FishingEstimatesPlot-", Sys.Date(), ".png")
    },
    content = function(fname) {
      grDevices::png(filename = fname, width = 480, height = 600, units = "px", res = 300)
      print(createPlotLBAestimates())
      dev.off()
    },
    contentType = "image/png"
  )

  
  diagnosticData <- reactive({
    if(input$lengthBasedAssessmentMethod == "LIME") {
      # LIME - life history and model fit data
      limeFit <- fitLIME()$lc_only
      #lh_fit <- limeFit$lh
      #limeFit <- limeFit$lc_only
      years_lime <- limeFit$input$years
      
      diagnosticLIME <- as.data.frame(limeFit$opt$diagnostics)
      # fishing mortality by year -  use two digits...for proper ordering of years
      diagnosticLIME$Param[diagnosticLIME$Param == "log_F_ft"] <- sprintf(fmt = "%s.%02d", "log_F_ft", years_lime)
      
      
      # parameter constraints
      parConstraints <- diagnosticLIME %>% dplyr::select(Param, Lower, Upper, final_gradient) %>%
        dplyr::mutate(Lower = ifelse(is.finite(Lower), Lower, -100),
               Upper = ifelse(is.finite(Upper), Upper, 100),
               Domain = "lightgreen")
      
      # parameter estimates
      diagnosticEstimates <- diagnosticLIME %>%
        dplyr::rename(InitialEstimate = starting_value, MaximumLikelihoodEstimate = MLE) %>% 
        tidyr::pivot_longer(cols = dplyr::ends_with("Estimate"), names_to = "Estimate", values_to = "Value", names_pattern = "(.*)Estimate") %>% 
        dplyr::select(Param, Estimate, Value)
      
      # estimate confidence intervals from standard error from covariance matrix
      ciEstimates <- data.frame(Param = diagnosticLIME$Param,
                                MLE = limeFit$Sdreport$par.fixed,
                                LowerCI = limeFit$Sdreport$par.fixed -1.96*sqrt(diag(limeFit$Sdreport$cov.fixed)),
                                UpperCI = limeFit$Sdreport$par.fixed +1.96*sqrt(diag(limeFit$Sdreport$cov.fixed)))
      
    } else if(input$lengthBasedAssessmentMethod == "LB-SPR") {
      fitLBSPR <- fitLBSPR()
      parNames <- t(unname(as.data.frame(strsplit(fitLBSPR$MLE$Parameter, split = ".", fixed = TRUE))))[,1]
      parYears <- t(unname(as.data.frame(strsplit(fitLBSPR$MLE$Parameter, split = ".", fixed = TRUE))))[,2]
      
      # valid data manipulation for "all periods" and "annual" LB-SPR data
      # initial estimates, MLEs, confidence intervals
      parConstraints <- 
        data.frame(Parameter = parNames,
                   Year = parYears,
                   Lower = rep(-Inf, dim(fitLBSPR$MLE)[1]),
                   Upper = rep(0, dim(fitLBSPR$MLE)[1]))
      parConstraints$Upper[grepl("log(F/M)", parConstraints$Parameter, fixed = TRUE)] <- Inf
      parConstraints <- parConstraints %>% 
        dplyr::mutate(Lower = ifelse(is.finite(Lower), Lower, -100),
               Upper = ifelse(is.finite(Upper), Upper, 100),
               Domain = "lightgreen")
      
      # diagnostic data frame
      diagnosticEstimates <- fitLBSPR$MLE # initialise
      diagnosticEstimates$Parameter <- parNames
      diagnosticEstimates$Year <- parYears
      diagnosticEstimates <- diagnosticEstimates %>% 
        dplyr::rename(InitialEstimate = Initial, MaximumLikelihoodEstimate = Estimate) %>%
        tidyr::pivot_longer(cols = dplyr::ends_with("Estimate"), names_to = "Estimate", values_to = "Value", names_pattern = "(.*)Estimate") %>% 
        dplyr::select(Parameter, Year, Estimate, Value)
      
      # confidence intervals - standard error from covariance matrix
      ciEstimates <- data.frame(Parameter = parNames,
                                Year = parYears,
                                MLE = fitLBSPR$MLE$Estimate,
                                LowerCI = fitLBSPR$MLE$Estimate-1.96*fitLBSPR$MLE$`Std. Error`,
                                UpperCI = fitLBSPR$MLE$Estimate +1.96*fitLBSPR$MLE$`Std. Error`)
    }
    list("parConstraints" = parConstraints, 
         "diagnosticEstimates" = diagnosticEstimates,
         "ciEstimates" = ciEstimates)
  })
  
  output$diagnosticParameterFits <- plotly::renderPlotly({
    diagnostics <- diagnosticData()  
    if(input$lengthBasedAssessmentMethod == "LIME") {
      parConstraintsLIME <- diagnostics$parConstraints
      diagnosticEstimatesLIME <- diagnostics$diagnosticEstimates
      ciEstimatesLIME <- diagnostics$ciEstimates
      
      # x-axis range
      x_min <- floor(min(parConstraintsLIME$Lower[parConstraintsLIME$Lower != -100], # exclude "infinite" lower bound
                         diagnosticEstimatesLIME$Value))
      x_max <- ceiling(max(parConstraintsLIME$Upper[parConstraintsLIME$Upper != 100], # exclude "infinite" upper bound 
                           diagnosticEstimatesLIME$Value))
      
      # order of fixed effects is important
      pg <- ggplot() + 
        geom_segment(data = parConstraintsLIME,
                     aes(y = Param, yend = Param, x = Lower, xend = Upper), size = 5, lineend = "butt",
                     colour = "lightgreen") +
        geom_point(data = diagnosticEstimatesLIME,
                   aes(y = Param, x = Value, shape = Estimate), size = 5, colour = "black") +
        geom_errorbarh(data = ciEstimatesLIME, 
                       aes(y = Param, xmin = LowerCI, xmax = UpperCI), height = 0.8) +
        scale_x_continuous(name = "Value") +
        scale_y_discrete(name = "Fixed effect parameters") +
        scale_shape_manual(values = c(1, 16)) + #scale_colour_manual(name = waiver(), values = "lightgreen", breaks = "lightgreen", labels = NULL) +
        coord_cartesian(xlim = c(x_min, x_max)) +
        theme_bw() + 
        theme(axis.text = element_text(size = 12),
              axis.title = element_text(size = 12))
      pl_y <- plotly::ggplotly(p = pg) %>% plotly::highlight("plotly_selected")
    } else if(input$lengthBasedAssessmentMethod == "LB-SPR") {
      parConstraintsLBSPR <- diagnostics$parConstraints
      diagnosticEstimatesLBSPR <- diagnostics$diagnosticEstimates
      ciEstimatesLBSPR <- diagnostics$ciEstimates
      
      parNames <- unique(parConstraintsLBSPR$Parameter)
      pglist <- vector("list", length = length(parNames))
      
      for (iParEst in seq_along(parNames)){
        parCon <- parConstraintsLBSPR[parConstraintsLBSPR$Parameter == parNames[iParEst],]
        dgnstcEst <- diagnosticEstimatesLBSPR[diagnosticEstimatesLBSPR$Parameter == parNames[iParEst],]
        ciEst <- ciEstimatesLBSPR[ciEstimatesLBSPR$Parameter == parNames[iParEst],]
        
        x_min <- floor(min(dgnstcEst$Value, max(ciEst$LowerCI,-100))) # exclude "infinite" lower bound
        x_max <- ceiling(max(dgnstcEst$Value, min(ciEst$UpperCI, 100))) # exclude "infinite" upper bound
        
        if(parNames[[iParEst]] == "log(Sdelta/Linf)"){
          x_max <- 0.0
        }
        
        pgPar <- ggplot() +
          geom_segment(data = parCon, aes(y = Year, yend = Year, x = Lower, xend = Upper), size = 5, lineend = "butt",
                       colour = "lightgreen") +
          geom_point(data = dgnstcEst, aes(y = Year, x = Value, shape = Estimate), size = 5, colour = "black") +
          geom_errorbarh(data = ciEst, aes(y = Year, xmin = LowerCI, xmax = UpperCI), height = 0.5) +
          scale_x_continuous(name = "Parameter value") +
          scale_y_discrete(name = "") + # MLE parameters
          scale_shape_manual(values = c(1, 16)) + 
          facet_grid(cols = vars(Parameter), scales = "free_x") +
          coord_cartesian(xlim = c(x_min, x_max)) +
          theme_bw() + 
          theme(axis.text = element_text(size = 12),
                axis.title = element_text(size = 12),
                legend.position = "none")
        pglist[[iParEst]] <- pgPar
        if(input$analyseLengthComposition == "all periods") {  
          pglist[[iParEst]] <- pgPar +
            theme(axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 4))
        } 
      }

      # convert to plotly and arrange
      if(length(pglist) == 3){
        col_widths <- c(0.31, 0.345, 0.345)
      } else if(length(pglist) == 1){
        col_widths <- 1
      } else {
        errorCondition("LB-SPR diagnostics plot input error")
      }
      pl_y <- plotly::subplot(pglist, nrows = 1, 
                              margin = c(0.05, 0.0, 0.0, 0.0), 
                              widths = col_widths,
                              shareY = TRUE, titleX = TRUE) %>%
        plotly::layout(xaxis = list(title = "estimate"), 
               xaxis2 = list(title = "estimate"), 
               xaxis3 = list(title = "estimate"), font = list(size = 14))
    }
    pl_y
  })
  ##----------
  ## FEEDBACK
  ##----------
  ## code block from DAMARA web-app
  ## see https://archimer.ifremer.fr/doc/00390/50174/50795.pdf for details
  submit.txt <- eventReactive(input$submitfeed, {
    user.name<-tolower(input$user)
    user.name<-gsub("[[:punct:]]|", "", user.name)
    user.name<-gsub("[[:space:]]", "", user.name)
    date.val<-format(Sys.time(), format="%B_%d_%Y_%H_%M_%S")
    feedback.file<-paste(paste(user.name, date.val, sep="_"),".txt", sep="")
    cat(input$exampleTextarea, file=feedback.file)
    paste(feedback.file,"submitted, thank you.")
  })
  ##
  output$outText <- renderText({
    submit.txt()
  })
}

