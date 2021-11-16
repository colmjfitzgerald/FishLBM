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
  
  #observeEvent(input$uploadFile,
  observe(
    { freezeReactiveValue(input, "lengthColSelect")
      updateVarSelectInput(session = getDefaultReactiveDomain(),
                           "lengthColSelect",
                           data = catchdata_read(),
                           selected = ifelse(any(grepl("length", colnames(catchdata_read()), ignore.case = TRUE)),
                                             grep("length", colnames(catchdata_read()), ignore.case = TRUE, value = TRUE),
                                             NULL))
      cat(file = stderr(), "after updateVarSelectInput\n")})
  
  # updates checkboxCatchData after lengthColSelect changes
  observeEvent(input$lengthColSelect,
               { cat(file = stderr(), "before freezeReactiveValue(input, checkboxCatchData)\n")
                 freezeReactiveValue(input, "checkboxCatchData")
                 cat(file = stderr(), "after freezeReactiveValue(input, checkboxCatchData)\n")
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
  
  observe({
    cat(file=stderr(), "selected length column", input$lengthColSelect, "\n")
    cat(file=stderr(), "number of rows selected", length(input$catchDataTable_rows_all), "\n")
  })
  
  observeEvent(input$selectCols,
               { print(paste0("choose length column"))
                 print(input$lengthColSelect)
                 print(paste0("choose attributes"))
                 print(input$checkboxCatchData)
                 print(paste0("species column?"))
                 print(grepl("species", input$checkboxCatchData, ignore.case = TRUE))
               })

    
  # dynamic UI elements (renderUI, insertUI etc.) ----
  
  # Tabulate data action button and reaction ====
  
  # length data category column selection ####
  output$btnSelectCols <- renderUI({
    actionButton("selectCols", "Input and plot data", icon = icon("table"))
  })
  
  
  
  # catchdata element for plotting and LBSPR
  catchdata_table <- eventReactive(input$checkboxCatchData,
    { 
      req(input$lengthColSelect)
      cat(file = stderr(), "catchdata_table: new length col", input$lengthColSelect,"\n")
      checkboxCols <- input$checkboxCatchData
      cat(file = stderr(), "catchdata_table: first $checkboxCatchData", checkboxCols,"\n")
      cat(file = stderr(), "catchdata_table: names(catchdata_read())", names(catchdata_read()),"\n")
      catchdata <- catchdata_read()[, c(checkboxCols, paste0(input$lengthColSelect))]
      charCols <- which(sapply(catchdata, is.character))
      if(!(length(charCols) == 0 & is.integer(charCols))) {
        catchdata[, charCols] <- sapply(catchdata[, charCols], trimws)
      } else {
        catchdata <- data.frame(catchdata)
        names(catchdata) <- c(checkboxCols, paste0(input$lengthColSelect))
      }
      cat(file = stderr(), "catchdata_table: second $checkboxCatchdata", checkboxCols,"\n")
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
      pg + theme_bw() + theme(strip.text.x = element_text(margin = margin(0.125,0.25,0.25,0.25, "cm")))
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
      cat(file = stderr(), paste0("number of rows = ", length(input$catchDataTable_rows_all)),"\n")
      length_records <- catchdata_table()[input$catchDataTable_rows_all,,drop = FALSE] %>% #select(input$lengthColSelect) %>% 
        mutate("{newLengthCol()}" := .data[[input$lengthColSelect]]*lengthScale)
      cat(file = stderr(), "lengthRecordsFilter\n")
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
    renderDT(
      expr = datatable(catchdata_table(), 
                       options = list(autowidth = TRUE, pageLength = 10, scrollX = TRUE, scrollY = FALSE,
                                      orderClasses = TRUE), # position of options? 
                       filter = "top", 
                       fillContainer = TRUE)
      )
  
  # visualise data ====
  output$lengthComposition <- renderPlotly({
    # catchdata_plot() eventReactive on input$selectCols
    expr = ggplotly(p = catchdata_plot())
  })
  
  

  # LVB growth ====
  
  # choices for growth parameters
  growthParChoices <- reactive({
    if("age" %in% input$checkboxCatchData) {
      expr = c("User-specified", "Data fit (frequentist)")
    } else {
      expr = c("User-specified")
    }
      
  })
  
  output$growthParRadioBtn <- renderUI({
                      radioButtons(inputId = "growthParOption",
                                   label = "LVB growth",
                                   choices = growthParChoices())
                    })

  # reactive dataframes ####
  # create growth (age-length) dataframe 
  createPlotSliderCurveData <- reactive({
    # slider curve
    growthcurve <- data.frame(age = seq(0, input$sliderAgeMax, by = 0.1))
    growthcurve$length_cm <- input$sliderLinf*(1- exp(-input$sliderK*(growthcurve$age-input$slidert0)))
    growthcurve
  })
  

  gatherFishAgeLengthData <- reactive({
    # lengthRecordsFilter() dependence ***
    cat(file = stderr(), "gatherFishAgeLengthData\n")
    lengthCol <- newLengthCol()
    filteredLengthRecords <- lengthRecordsFilter()
    checkboxCatchCols <- input$checkboxCatchData
    sexCol <- grep("sex", checkboxCatchCols, ignore.case = TRUE, value = TRUE)
    cat(file = stderr(), "gatherFishAgeLengthData lengthCol, sexCol\n")
    if(any(grepl("age", checkboxCatchCols, ignore.case = TRUE))){
      ageCol <- grep("age", checkboxCatchCols, ignore.case = TRUE, value = TRUE)
      lengthAge <- data.frame(filteredLengthRecords[, c(lengthCol)], 
                              filteredLengthRecords[, ageCol],
                              filteredLengthRecords[, sexCol])
      colnames(lengthAge) <- c(lengthCol, ageCol, sexCol)
      lengthAgeData <- lengthAge[!is.na(lengthAge[, ageCol]) & !is.na(lengthAge[, lengthCol]),]
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
      print(class(nls.m))
    }
    x <- list(model = nls.m, data = growthData)
  })
  
  # bootstrapped growth parameters confidence intervals using nlsBoot
  growthFrequentistFitBoot <- reactive({ #
    GFF <- growthFrequentistFit()
    growthFitData <- GFF$data
    ageLengthData <- gatherFishAgeLengthData()
    gm <- GFF$model
    
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
    fitLinf <- coef(GFF$model)["Linf"]
    if(is.null(fitLinf)){
      gtgLinf <- NULL
    } else {
      gtgLinf <- expand.grid(age = c(0, input$sliderAgeMax), 
                           length = c(fitLinf), 
                           ci = c("mean", "mean+2SD", "mean-2SD"), stringsAsFactors = FALSE)
      gtgLinf$linetype <- ifelse(gtgLinf$ci == "mean", "1", "2")
    }
    gtgLinf
  })
  
  
  # reactive ggplot elements/geoms #### 
  ggGrowth_CurveALData <- reactive({
    cat(file = stderr(), "ggGrowth_CurveALData\n")
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
    cat(file = stderr(), "sampleLengthRecords\n")
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
    
    fitLinf <- coef(GFF$model)["Linf"]
    if(is.null(fitLinf)){
      gtgLinf <- NULL
    } else {
      gtgLinf <- expand.grid(age = c(0, input$sliderAgeMax), 
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
  
  # debug observe events
  observeEvent(input$fitGrowth,
               {print(input$checkboxCatchData)
                 # print(growthParChoices())
                 # print("debug observe event - print growthFrequentistFit(), growthFrequentistFitBoot()")
                 # print(growthFrequentistFit())
                 # print(growthFrequentistFitBoot())
                 # print("are ggplots?")
                 # print(is.ggplot(ggGrowthFitMean()))
                 # print(is.ggplot(ggGrowth_CurveALData()))
                 # print(tags$p("(/^weight/).test(input.checkboxCatchData)"))
                 #insertUI(selector = "#sectionGrowthFitSummary",
                #          where = "beforeBegin",
                #          ui = actionButton(inputId = "removeFit", label = "Remove fit"))
  })
  
  
  # growth outputs ####
  output$lvbGrowthCurve <- renderPlotly({#expr =
    print(input$fitGrowth)
    if(input$fitGrowth > 0) {
      ggplotly(ggGrowth_CurveALData() + ggGrowthFitMean() + ggGrowthFitCI() + ggLinf())    
    } else {
      ggplotly(ggGrowth_CurveALData())
    }
  })
  
  output$growthFitSummary <- renderPrint({
    expr = print(summary(growthFrequentistFit()$model, correlation = TRUE))
  })
  
  
  # LB-SPR assessment ====
  
  
  # natural mortality choices ####
  
  # choices for growth parameters
  natMestChoices <- reactive({
    growthChoices <- c("User-specified" = "user", 
                       "PaulyNLS-T ($M = 4.118K^{0.73}L_{\\infty}^{-0.33}$)" = "pauly", 
                       "Two-parameter K ($M = 0.098 + 1.55K$)" = "twoK")
    if("age" %in% input$checkboxCatchData & 
       any(!is.na(gatherFishAgeLengthData()[,grep("age", names(gatherFishAgeLengthData()), value = TRUE)]))) {
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
  
  output$btnRadioMaturity <- 
    renderUI({
      withMathJax(
        radioButtons(inputId= "maturityPars", label = "Length-at-50%-maturity",
                     choices = c("User-specified" = "user", 
                       "Beverton-Holt LHI ($Lm50 = 0.66 L_\\infty$)" = "bhlhi",
                       "Binohlan, Froese (2009) $Lm50  = e^{-0.119} (L\\max)^{0.916}$" = "bf2009"), 
                       selected = "bhlhi")
      )
    }
    )
  
  
  # numeric inputs for LBSPR ####
  # default to sliderInput values
  output$numLinf <- renderUI({
    numericInput("Linf", label = NULL, #"Linf", 
                 value = ifelse(grepl("fit", input$growthParOption), 
                                round(coef(growthFrequentistFit()$model)["Linf"], digits = 2),
                                round(input$sliderLinf, digits = 2)))
  })
  
  output$numKlvb <- renderUI({
    numericInput("kLvb", label = NULL, #"K growth", 
                 value = ifelse(grepl("fit", input$growthParOption), 
                                round(coef(growthFrequentistFit()$model)["K"], digits = 2),
                                round(input$sliderK, digits = 2)))
  })
  
  output$numM <- renderUI({
    numericInput("M", label = NULL, #"M natural mortality", 
                 value = 0.3)
  })
  
  # nb value = initial value
  output$numLm50 <- renderUI({
    numericInput("Lm50", label = NULL, #"Linf", 
                 value = ifelse(grepl("fit", input$growthParOption), 
                                round(coef(growthFrequentistFit()$model)["Linf"]*0.66, digits = 2),
                                round(input$sliderLinf*0.66, digits = 2)))
  })
  
  output$numLm95 <- renderUI({
    numericInput("Lm95", label = NULL, #"Linf", 
                 value = ifelse(grepl("fit", input$growthParOption), 
                                round(coef(growthFrequentistFit()$model)["Linf"]*0.75, digits = 2),
                                round(input$sliderLinf*0.75, digits = 2)),
                 min = input$sliderLinf*0.25)
  })
  
  
  observe({
    updateNumericInput(session, "M", value = updateMortality())
    updateNumericInput(session, "Lm50", value = updateMaturity()) # update Lm95
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
    if(input$lengthBasedAssessmentMethod == "LB-SPR"){
      lhp_list <- c(lhp_list, 
                    list(FecB = input$FecB, 
                         Steepness = input$Steepness,
                         Mpow = input$Mpow,
                         MaxSD = input$MaxSD,
                         NGTG = input$NGTG)) # "technical" parameter?
    } else if(input$lengthBasedAssessmentMethod == "LIME") {
      lhp_list <- c(lhp_list, list(maturity_input="length"))
    }
    expr = lhp_list
  })
  

  # technical method parameters
  output$boxTechParsLBSPR <- renderUI({
    box(status = "info", width = NULL,
        collapsible = TRUE,
        collapsed = ifelse(is.null(input$lengthBasedAssessmentMethod), TRUE, !(input$lengthBasedAssessmentMethod == "LB-SPR")),
        title = "LB-SPR parameters",
        tagList(
          tags$table(
            tags$tr(tags$td("FecB"),
                    tags$td(numericInput(inputId = "FecB", label = NULL, value = 3))),
            tags$tr(tags$td("Steepness"),
                    tags$td(numericInput(inputId = "Steepness", label = NULL, value = 0.8))),
            tags$tr(tags$td("Mpow"),
                    tags$td(numericInput(inputId = "Mpow", label = NULL, value = 0.0))),
            tags$tr(tags$td("NGTG"),
                    tags$td(numericInput(inputId = "NGTG", label = NULL, value = 17))),
            tags$tr(tags$td("GTG Max SD about Linf"),
                    tags$td(numericInput(inputId = "MaxSD", label = NULL,
                                         value = 2, min = 0, max = 4))),
            tags$tfoot()
          ),
        )
        # uiOutput(outputId = "tableTechnicalParametersLBSPR")
    )
    
  })
  
  output$boxTechParsLIME <- renderUI({
     box(status = "info", width = NULL,
         collapsible = TRUE,
         collapsed = ifelse(is.null(input$lengthBasedAssessmentMethod), TRUE, !(input$lengthBasedAssessmentMethod == "LIME")),
         title = "LIME parameters",
         tagList(
           tags$table(
             tags$tr(tags$td("SigmaR"),
                     tags$td(numericInput(inputId = "SigmaR", label = NULL, value = 0.737))),
             tags$tr(tags$td("SigmaF"),
                     tags$td(numericInput(inputId = "SigmaF", label = NULL, value = 0.2))),
             tags$tr(tags$td("SigmaC"),
                     tags$td(numericInput(inputId = "SigmaC", label = NULL, value = 0.1))),
             tags$tr(tags$td("SigmaI"),
                     tags$td(numericInput(inputId = "SigmaI", label = NULL, value = 0.1))),
             tags$tr(tags$td("R0"),
                     tags$td(numericInput(inputId = "R0", label = NULL, value = 1, min = 0))),
             tags$tr(tags$td("Frate"),
                     tags$td(numericInput(inputId = "Frate", label = NULL, value = 0.1, min = 0))),
             tags$tr(tags$td("Fequil"),
                     tags$td(numericInput(inputId = "Frate", label = NULL, value = 0.25, min = 0))),
             tags$tr(tags$td("qcoef"),
                     tags$td(numericInput(inputId = "qcoef", label = NULL, value = 1e-5, min = 0))),
             tags$tr(tags$td("start_ages"),
                     tags$td(numericInput(inputId = "start_ages", label = NULL, value = 0))),
             tags$tr(tags$td("rho"),
                     tags$td(numericInput(inputId = "rho", label = NULL, value = 0.43, min = 0))),
             tags$tr(tags$td("theta"),
                     tags$td(numericInput(inputId = "theta", label = NULL, value = 10, min = 0))),
             tags$tr(tags$td("nseasons"),
                     tags$td(numericInput(inputId = "nseasons", label = NULL, value = 1, min = 0))),
             tags$tfoot()
           )
         )
    #     uiOutput(outputId = "tableTechnicalParametersLIME")
     )
  })
  
  # selectivity curve ####

  observe({
    print("update radio buttons")    
    updateRadioButtons(session, inputId = "specifySelectivity",
                       choices = selectParameterSpecification())
    print("finished updating")
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
      x <- c("Normal.loc", "Normal.sca", "logNorm")
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
  
  observe({
    print("*** observe ***")
    print(input$chooseSelectivityPattern)
    print(paste0("sizeSelectivityCurves() == ",  sizeSelectivityCurves()))
    print(input$selectSelectivityCurve)
    # we comment out the print statements below because changes in reactive expressions 
    # cause the observe expressions to evaluate. therefore to find out which reactive 
    # expressions change we should examine each input individually and then together 
    # print(paste0("selectSelectivityCurve is NULL: ", is.null(input$selectSelectivityCurve)))
    # print(input$specifySelectivity)
    # print(paste0("specify selectivity choices = ", selectParameterSpecification()))
    # print(length(selectParameterSpecification()))
    # print(paste0("specifySelectivity is NULL: ", is.null(input$specifySelectivity)))
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
    # Starting guesses
    SizeBins <- list(Linc = 1)
    SizeBins$ToSize <- input$Linf * (1 + 2*input$CVLinf)
    lengthBins <- seq(from=0, to=SizeBins$ToSize, by = SizeBins$Linc)
    lengthMids <- seq(from=0.5*SizeBins$Linc, by = 1, length.out=(length(lengthBins)-1))
    
    LenDat1 <- hist(lengthRecordsFilter()[,newLengthCol()], plot = FALSE, breaks = lengthBins, right = FALSE)
    sSL50 <- lengthMids[which.max(LenDat1$count)] # LenMids[which.max(LenDat)]/input$Linf
    sDel <- 0.2*sSL50 # LenMids[which.max(LenDat)]/input$Linf
    sSL95 <- ifelse(sSL50+sDel < input$Linf, sSL50+sDel, sSL50 + 0.1(input$Linf-sSL50)  )
    
    if(req(input$selectSelectivityCurve) == "Knife-edged"){
      tList <- tagList(
        sliderInput(inputId = "SLKnife", label = "MLL",
                    value = round(input$Linf*0.50, digits = 2),
                    min = 0.0, max = input$Linf, step = 1)
      )
    } else if(req(input$selectSelectivityCurve) == "Logistic") {
      tList <- tagList(
        sliderInput(inputId = "SL1", label = "Length at 50% selectivity",
                    value = round(sSL50, digits = 2), min = 0, max = input$Linf, step = round(input$Linf/50)/2),
        sliderInput(inputId = "SL2", label = "Length at 95% selectivity",
                    value = round(sSL95, digits = 2), min = 0, max = input$Linf, step = round(input$Linf/50)/2)
      )
    } else if(req(input$selectSelectivityCurve) %in% c("Normal.sca", "Normal.loc")) {
      tList <- tagList(
        sliderInput(inputId = "SL1", label = "Length with maximum selectivity",
                    value = round(sSL50, digits = 2), min = 0, max = input$Linf, step = 1),
        sliderInput(inputId = "SL2", label = "SD (spread) of dome-shaped selectivity curve",
                    value = round(sDel, digits = 2), min = 0, max = input$Linf, step = 1),
        sliderInput(inputId = "SLKnife", label = "MLL",
                    value = round(input$Linf*0.0, digits = 2),
                    min = 0.0, max = input$Linf,  step = 1)
      )
    } else if(req(input$selectSelectivityCurve) %in% c("logNorm")) {
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
                      if(input$specifySelectivity == "Fixed value"){
                        list(S50 = input$SL1, S95 = input$SL2,
                             selexBy = "length",
                             selexCurve = tolower(input$selectSelectivityCurve),
                             nfleets = 1, # where is the best place for this?
                             est_selex_f = FALSE
                        )
                      } else if(input$specifySelectivity == "Initial estimate") {
                        list(S50 = input$SL1, S95 = input$SL2,
                             selexBy = "length",
                             selexCurve = tolower(input$selectSelectivityCurve),
                             nfleets = 1, # where is the best place for this?
                             est_selex_f = TRUE
                        )
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
    }
    dSC
  })
  
  
  # plot selectivity pattern

  #observeEvent(
  #  input$btnFixedFleetPars, {
      output$plotSelectivityPattern <- renderPlotly({
        print("before isolate in output$plotSelectivityPattern")
        print(min(lengthRecordsFilter()[,newLengthCol()], na.rm = TRUE))
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
        expr = ggplotly(ggplot(ggdata) + 
                          geom_line(aes( x = length, y = proportion, colour = quantity, size = size,
                                         linetype = quantity)) +
                          scale_colour_manual(values = c("red", "black")) +
                          scale_linetype_manual(values = gg_line_types) +
                          scale_size_identity() + 
                          labs(title = gg_titleplot) + # could have reactive?
                          theme_bw())
      })
      
      # sliders for plot??
      # You can use a simple observeEvent to detect when button pressed, 
      # and then show the hidden sliderInput and plotOutput widgets
      # output$selectivityControls <- renderUI({
      #   
      # })
      
#    })
  

  
  
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
    print(str(length_records))
    
    # length column
    length_col <- newLengthCol()
    
    # check for year attribute
    year_col <- names(length_records)[grepl("year", names(length_records), ignore.case = TRUE)]
    if(any(grepl("year", names(length_records), ignore.case = TRUE))){
      year_min <- min(length_records[, year_col])
      year_max <- max(length_records[, year_col])
    }

    # length_col (+ optional year_col): exclude NAs
    length_records <- na.omit(length_records[, c(year_col,length_col), drop = FALSE])
    
    # vulnerable to fishery 
    isVulnerable <- (lengthMids >= input$MLL)
    
    # bin length data
    if(input$lengthBasedAssessmentMethod == "LB-SPR"){
      print("length_records")
      print(dim(length_records))
      print(is.numeric(length_records[, length_col]))
      if(input$analyseLengthComposition == "annual") {
        # "each year" only an option if year column present (length(year_col)>0)
        length_records_by_year <- length_records[, c(year_col, length_col)] %>% na.omit() %>%
          mutate(lengthBin = cut(!!ensym(length_col), breaks = lengthBins, 
                                 right = FALSE),
                 year = factor(!!ensym(year_col)))  %>% # lengthMids??
          group_by(year, lengthBin) %>%
          summarise(nFish = n())
        LF <- xtabs(formula = nFish ~ year + lengthBin, data = length_records_by_year)
        colnames(LF) <- as.character(lengthBins)[-1]
        LFVul <- LF*outer(rep.int(1, nrow(LF)), isVulnerable)
      } else if(input$analyseLengthComposition == "all periods"){
        # aggregate all years - see fitGTGLBSPR for how this approach is incorporated in the assessment 
        # could also be implemented in this reactive and propagated??
        LFhist <- hist(length_records[, length_col], plot = FALSE, breaks = lengthBins, right = FALSE)

        LF <- matrix(LFhist$counts, nrow = 1, ncol = length(LFhist$counts), 
                     dimnames = list("all periods", as.character(lengthBins)[-1]))
        LFVul <- matrix(LFhist$counts*isVulnerable, 
                        nrow = 1, ncol = length(LFhist$counts), 
                        dimnames = list("all periods", as.character(lengthBins)[-1]))
      }
    } else {
      length_records_by_year <- length_records[, c(year_col, length_col)] %>% na.omit() %>%
        mutate(lengthBin = cut(!!ensym(length_col), breaks = lengthBins, 
                               right = FALSE),
               year = factor(!!ensym(year_col), levels = seq(year_min, year_max, 1)))  %>% # lengthMids??
        group_by(year, lengthBin) %>%
        summarise(nFish = n())
      
      # configure length data for LIME
      LFYear <- xtabs(formula = nFish ~ year + lengthBin, data = length_records_by_year)
      colnames(LFYear) <- as.character(lengthBins)[-1] # lfLIME col.name with upper end of length bins or mid-bin??
      LFVulYear <- LFYear*outer(rep.int(1, nrow(LFYear)), isVulnerable)
    }

    # binned counts
    list_out <- NULL
    if((input$lengthBasedAssessmentMethod == "LB-SPR")){
      list_out <- list(LenDat = LF,
                    LenDatVul = LFVul)
    } else if(input$lengthBasedAssessmentMethod == "LIME") {
      list_out <- list(LenDat = LFYear,
                    LenDatVul = LFVulYear)      
    }
    list_out
  })
  
  # app navigation
  
  # after button clicks
  observeEvent(input$convertLengthUnits,
               updateTabsetPanel(session, inputId = "tabMain", selected = "tabLHP"))  
  
  observeEvent(input$btnFixedFleetPars,
               updateTabsetPanel(session, inputId = "tabMain", selected = "tabLBA"))
  
  # btnStockPars causes move to next tab
  observeEvent(input$btnStockPars, {
    print(setLHPars())
    updateTabsetPanel(session, inputId = "tabMain", selected = "tabSelectivity")
  })
  
  # btnTechnicalStockPars 
  observeEvent(input$btnTechnicalStockPars,
               { cat(file = stderr(), "input$btnTechnicalStockPars\n")
                 length_records <- lengthRecordsFilter()[, newLengthCol()]
                 print(length_records[is.na(lengthRecordsFilter()[, newLengthCol()])])
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
    lengthRecordAttributes <- input$checkboxCatchData
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
  
  
  # plot length composition of filtered data - change with slider input
  output$plotResponsiveLengthComposition <- 
    renderPlotly({
      lengthData <- lengthRecordsFilter()
      lengthData$isVulnerable <- lengthData[, newLengthCol()] >= input$MLL
      if(all(lengthData$isVulnerable, na.rm = TRUE)) {
        ggLengthComp <- ggplot(lengthData %>% filter(!is.na(!!sym(newLengthCol())))) +  
          geom_histogram(mapping = aes_string(x = newLengthCol()), fill = "grey80",
                         breaks = createLengthBins()$LenBins, # slideLenBins(),
                         closed = "left", colour = "black") +
          geom_vline(xintercept = input$MLL, colour = "red", linetype = 2, size = 1) +
          theme_bw()
      } else {
        ggLengthComp <- ggplot(lengthData %>% filter(!is.na(!!sym(newLengthCol())))) + 
          geom_histogram(mapping = aes_string(x = newLengthCol(), fill = "isVulnerable"),
                         breaks = createLengthBins()$LenBins, # slideLenBins(),
                         closed = "left", colour = "black") +
          scale_fill_manual(name = "fishery \n vulnerable", breaks = waiver(), values = c("grey20", "grey80")) + 
          geom_vline(xintercept = input$MLL, colour = "red", linetype = 2, size = 1) +
          theme_bw() + 
          theme(legend.position = "bottom")
      }
      print("analyseLengthComposition")
      print(input$analyseLengthComposition)
      if(input$analyseLengthComposition == "annual"){ # was observeEvent or eventReactive 
        print(paste0(grep("year", colnames(lengthRecordsFilter()), ignore.case = TRUE,
                          value = TRUE)," ~ ."))
        ggLengthComp <- ggLengthComp + 
          facet_wrap(as.formula(paste0(grep("year", colnames(lengthRecordsFilter()), ignore.case = TRUE,
                                            value = TRUE)," ~ .")),
                     ncol = 3) 
      }
      expr = ggplotly(ggLengthComp)
    })
  

  # Fit LBA ####
  fitGTGLBSPR <- eventReactive(
    input$fitLBA,
    {
      # as.name
      length_records <- lengthRecordsFilter()
      length_col <- newLengthCol()
      print(head(length_records[, length_col]))
      print(length_records[, length_col])
      
      StockPars <- setLHPars()
      StockPars$MK <- StockPars$M/StockPars$K
      
      fixedFleetPars <- NULL
      allFleetPars <- setFleetPars()
      if(input$specifySelectivity == "Fixed value" & !allFleetPars$selexParsEstimate){
        titleFitPlot <- "User-specified selectivity parameters"
        fixedFleetPars <- allFleetPars[names(allFleetPars) != "selexParsEstimate"]
        cat(paste0("fixedFleetPars = ", fixedFleetPars, "\n")) 
      } else if(input$specifySelectivity == "Initial estimate" & allFleetPars$selexParsEstimate) {
        titleFitPlot <- "User-specified selectivity parameters"
        fixedFleetPars <- allFleetPars[names(allFleetPars) != "selexParsEstimate"]
        initialFleetPars <- list(SL1 = allFleetPars$SL1, SL2 = allFleetPars$SL2)
        cat(paste0("initialFleetPars = ", initialFleetPars, "\n"))
      }
      fixedFleetPars
      names(fixedFleetPars)[names(fixedFleetPars) == "selexCurve"] <- "selectivityCurve"
      
      titleFitPlot <- "Model-estimated selectivity parameters"
      if(input$specifySelectivity == "Fixed value"){
        titleFitPlot <- "User-specified selectivity parameters"
      }

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
      SizeBins <- createLengthBins()$SizeBins
      LenBins <- createLengthBins()$LenBins
      LenMids <- createLengthBins()$LenMids
      LenDat <- binLengthData()$LenDat
      LenDatVul <- binLengthData()$LenDatVul
      years <- rownames(LenDatVul)
      
      NatL_LBSPR <- NULL
      estModelFit <- NULL
      optPars <- NULL
      optimOut <- vector("list", length = length(years))
      
      for (yearLBSPR in years){

      LenDatIn <- LenDatVul[which(rownames(LenDatVul) == yearLBSPR),]  

      # GTG-LBSPR optimisation
      optGTG <- DoOptDome(StockPars,  fixedFleetPars, LenDatIn, SizeBins, "GTG")#, input$selectSelectivityCurve)
      # optGTG$Ests
      # optGTG$PredLen
      # optGTG$opt_par
      optimOut[[which(years == yearLBSPR)]] <- optGTG$optimOut
      names(optimOut)[which(years == yearLBSPR)] <- paste0("lbspr_",yearLBSPR)

      if(input$specifySelectivity == "Fixed value"){
        optFleetPars <- list(FM = optGTG$Ests["FM"],
                             selectivityCurve = optGTG$SelectivityCurve,
                             SL1 = fixedFleetPars$SL1, 
                             SL2 = fixedFleetPars$SL2,
                             SLMin = fixedFleetPars$SLMin,
                             SLmesh = fixedFleetPars$SLmesh)
      } else if(input$specifySelectivity == "Initial estimate") {
        optFleetPars <- list(FM = optGTG$Ests["FM"], 
                             selectivityCurve = optGTG$SelectivityCurve,
                             SL1 = optGTG$Ests["SL1"], 
                             SL2 = optGTG$Ests["SL2"])
      }

      # per recruit theory
      prGTG <- GTGDomeLBSPRSim(StockPars, optFleetPars, SizeBins)#, input$selectSelectivityCurve)

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
      
      # numbers-at-length (midpoints) LBSPR
      NatL_LBSPR <- rbind(NatL_LBSPR,
                      data.frame(year = yearLBSPR,
                                 length_mid = prGTG$LenMids,
                                 catchFished_at_length = prGTG$LCatchFished/max(prGTG$LCatchFished),
                                 catchUnfished_at_length = prGTG$LCatchUnfished/max(prGTG$LCatchUnfished),
                                 selectivityF_at_length = VulLen2,
                                 popUnfished_at_length = prGTG$LPopUnfished/max(prGTG$LPopUnfished),
                                 popFished_at_length = prGTG$LPopFished/max(prGTG$LPopFished) #standardised??
                      ))
      
      print(data.frame(FM = unname(optFleetPars$FM), 
                       SL50 = unname(optFleetPars$SL1), 
                       SL95 = unname(optFleetPars$SL2),
                       SPR = prGTG$SPR))
      estModelFit <- rbind(estModelFit,
                       data.frame(FM = unname(optFleetPars$FM), 
                                  SL50 = unname(optFleetPars$SL1), 
                                  SL95 = unname(optFleetPars$SL2),
                                  SPR = prGTG$SPR, 
                                  row.names = yearLBSPR)
      )
      optPars <- rbind(optPars, optGTG$opt_par)
      # Parameter = c("FM", "SL50", "SL95", "SPR"),
      # Description = c("F/M: relative fishing mortality",
      #                 "Length at 50% selectivity",
      #                 "Length at 95% selectivity",
      #                 "Spawning Potential Ratio"),
      
      #        opModelOut <- data.frame(Parameter = c("SPR", "YPR"),
      #                                 Description = c("Spawning Potential Ratio", "Yield-per-recruit"),
      #                                 Estimate = c(prGTG$SPR, prGTG$YPR))
      }
      if(input$specifySelectivity == "Fixed value"){
        #estModelFit$Source <- c("Model fit", "User-specified", "User-specified", "Model")
        if(input$selectSelectivityCurve == "Logistic"){
          names(estModelFit)[names(estModelFit) == "SL50"] <- "SL50 (fixed)"
          names(estModelFit)[names(estModelFit) == "SL95"] <- "SL95 (fixed)"
        } else if(input$selectSelectivityCurve == "Dome-shaped"){
          names(estModelFit)[names(estModelFit) == "SL50"] <- "SL1"
          names(estModelFit)[names(estModelFit) == "SL95"] <- "SL2"
        }
      }
      
      optPars <- data.frame(optPars)
      if(dim(optPars)[2] == 3) {
        colnames(optPars) <- c("FM", "log_SL50", "log_SLdelta")
      } else {
        colnames(optPars) <- c("FM")
      }
      optPars <- cbind(year = years, optPars)
      
      list(NatL_LBSPR = NatL_LBSPR,
           estModelFit = estModelFit,
           optimOut = optimOut,
           optPars = optPars,
           MLE = optGTG$MLE
           )
    }
  )
  

  fitLIME <- reactive(
    {
      lhParVals <- isolate(setLHPars()) # only evaluated if fitLIME called
      fleetParVals <- isolate(setFleetPars()) 
      binwidth <- isolate(input$Linc)
      print("fleetParVals LIME")
      print(fleetParVals)
      # selectivity parameters fitted or estimated
      titleFitPlot <- "Model-estimated selectivity parameters"
      if(input$specifySelectivity == "Fixed value"){
        titleFitPlot <- "User-specified selectivity parameters"
      }
      
      # how to handle specifying/estimating SL50/SL95 for logistic
      if(tolower(fleetParVals$selexCurve) == "logistic"){
          S50 <- fleetParVals$S50  # initial estimate of selectivity-at-length 50%
          S95 <- fleetParVals$S95
      } else {
        cat("no app support for dome-shaped LIME as of yet - see LIME package or LIME shiny app for dome-shaped support")   
      }
      lh <- create_lh_list(vbk= lhParVals$K,    # vb growth coefficient
                           linf= lhParVals$Linf,  # vbg Linf
                           t0= input$slidert0,  
                           lwa=lhParVals$Walpha,     # length-weight W = aL^b: a  
                           lwb=lhParVals$Wbeta,     # length-weight W = aL^b: b
                           S50=S50, #50,  # selectivity-at-length 50%
                           S95=S95, #95,  # selectivity-at-length 95%
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
                           Frate=input$Frate,
                           Fequil=input$Fequil,
                           qcoef=input$qcoef,
                           start_ages=input$start_ages,
                           rho=input$rho,
                           theta=input$theta,
                           nseasons=input$nseasons,
                           nfleets=1 # fleetParVals$nfleets
                           )
      print(lh)
      
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
      #   mutate(lengthBin = cut(!!ensym(lengthCol), breaks = lengthBins, 
      #                          right = FALSE),
      #          year = factor(!!ensym(yearCol), levels = seq(year_min, year_max, 1)))  %>% # lengthMids??
      #   group_by(year, lengthBin) %>%
      #   summarise(nFish = n())
      # 
      # 
      # # configure length data for LIME
      # lfLIME <- xtabs(formula = nFish ~ year + lengthBin, data = lrLIME)
      # 
      # # 26/07/2021
      # #lengthBins <- seq(0, lh$binwidth*ceiling(lh$linf*(1 + lh$CVlen)/lh$binwidth), lh$binwidth)
      # #lengthMids <- seq(from=0.5*lh$binwidth, by=lh$binwidth, length.out=length(lengthBins)-1)
      # lfLIME <- lfLIME*outer(rep.int(1, nrow(lfLIME)), isVulnerable)
      print("pre-lfLIME")
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
      data_all <- list("years"=as.numeric(first(rownames(lfLIME))):as.numeric(last(rownames(lfLIME))), 
                       "LF"=LF,  
                       "neff_ft"= neff_ft)

      inputs_all <- create_inputs(lh=lh, input_data=data_all)
      
      print(data_all)
      
      #  run_LIME ####
      start <- Sys.time()
      
      lc_only <- run_LIME(modpath=NULL, 
                          input=inputs_all,
                          data_avail="LC", 
                          est_selex_f = fleetParVals$est_selex_f)
      end <- Sys.time() - start
      print(end)
      print(lc_only$input)
      # outputs
      list(length_data_raw = length_records[, c(yearCol, lengthCol)] %>% na.omit(),
           LF = LF, lc_only = lc_only, lh = lh)
    }
  )
  
  
  # move panel within navBarPage after fit
  observeEvent(input$btnTechnicalStockPars,
               updateNavbarPage(session, inputId = "methodLBSPR", selected = "tabLengthComposition"))
  
  observeEvent(input$fitLBA,
               {updateNavbarPage(session, inputId = "methodLBSPR", selected = "tabModelFit")
                 if(input$lengthBasedAssessmentMethod == "LIME") {
                   showModal(modalDialog(
                     title = "TMBhelper convergence check",
                     fitLIME()$lc_only$opt$Convergence_check,
                     easyClose = TRUE,
                     size = "m"), session)
                 }
               })
  
  
  # print text on LBSPR estimating model fit 
  output$tableLBAEstimates <- reactive(
    {
      
    if(input$lengthBasedAssessmentMethod == "LB-SPR"){
      estModelFitLBSPR <- fitGTGLBSPR()$estModelFit
      optParsLBSPR <- fitGTGLBSPR()$optPars
      # three significant figures
      optParsLBSPR[sapply(optParsLBSPR, is.numeric)] <- signif(optParsLBSPR[sapply(optParsLBSPR, is.numeric)], 3)
      #estModelFitLBSPR[ ,colnames(estModelFitLBSPR)!= "SPR"] %>%
      optParsLBSPR %>%
        knitr::kable("html", digits = 3) %>%
        kable_styling("striped", full_width = F, position = "float_left")
    } else if(input$lengthBasedAssessmentMethod == "LIME"){
      fitLIMEout <- fitLIME()$lc_only
      yearsLIME <- row.names(fitLIME()$LF)
      print(yearsLIME)
      
      # fishing mortality
      log_fishing <- summary(fitLIMEout$Sdreport)[which(rownames(summary(fitLIMEout$Sdreport))=="lF_y"),]
      
      # recruitment parameter
      log_sigma_R <- summary(fitLIMEout$Sdreport)[which(rownames(summary(fitLIMEout$Sdreport))=="log_sigma_R"),]
      
      # Dirichlet-multinomial parameter
      log_theta <- summary(fitLIMEout$Sdreport)[which(rownames(summary(fitLIMEout$Sdreport))=="log_theta"),]
      
      # length-at-selectivity parameters
      if(input$specifySelectivity == "Initial estimate"){
        log_S50_f <- summary(fitLIMEout$Sdreport)[which(rownames(summary(fitLIMEout$Sdreport))=="log_S50_f"), ]
        log_Sdelta_f <- summary(fitLIMEout$Sdreport)[which(rownames(summary(fitLIMEout$Sdreport))=="log_Sdelta_f"),]
        
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
      
      dfLIME %>%
         knitr::kable("html", digits = 3) %>%
         kable_styling("striped", full_width = F, position = "float_left")
    }
    }
  )
    
  #renderPrint({
  #  expr = print(fitGTGLBSPR()$estModelFit)
  #})
  
  # # print text on LBSPR operating model output
  # output$textLBSPROpOut <- renderPrint({
  #   expr = print(fitGTGLBSPR()$opModelOut)
  # })
  
  output$plotLBAModelFit <- renderPlotly({
    # length data
    length_records <- lengthRecordsFilter()
    length_col <- newLengthCol()
    length_records$isVulnerable <- length_records[,length_col] >= input$MLL
    year_col <- names(length_records)[grepl("year", names(length_records), ignore.case = TRUE)]
    
    # is year column present?
    if(is.null(year_col) | input$analyseLengthComposition == "all periods"){
      length_records <- length_records %>%
        select(!!ensym(length_col), isVulnerable) %>%
        mutate(year = "all periods")
    } else {
      #rename as year or facetting
      names(length_records)[grepl("year", names(length_records), ignore.case = TRUE)] <- "year"
    }
    
    LenBins <- createLengthBins()$LenBins
    LenMids <- createLengthBins()$LenMids	
    LenDat <- binLengthData()$LenDatVul # vulnerable to fishery only
    if(input$lengthBasedAssessmentMethod == "LB-SPR"){
      NatL_LBSPR <- fitGTGLBSPR()$NatL_LBSPR
      maxLengthYearVector <- apply(LenDat, 1, "max")
      maxLengthYear <- rep(maxLengthYearVector, each = dim(LenDat)[2])
      NatL_LBSPR$catchFished_at_length_count <- NatL_LBSPR$catchFished_at_length*maxLengthYear
      NatL_LBSPR$selectivityF_at_length_count <- NatL_LBSPR$selectivityF_at_length*maxLengthYear
      
      if(input$specifySelectivity == "Fixed value"){
        titleFitPlot <- "Length data, LB-SPR fit and selectivity curve (specified)"
      } else if (input$specifySelectivity == "Initial estimate") {
        titleFitPlot <- "Length data, LB-SPR fit and selectivity curve (estimated)"
      }
      
      
      # create ggplot with data...
      pg <- ggplot(length_records %>% filter(isVulnerable) ) + 
        geom_histogram(mapping = aes_string(x = length_col), breaks = LenBins, 
                       closed = "left", colour = "black", fill = "grey75")
      
      # ...and fit
      pg <- pg + 
        geom_area(data = NatL_LBSPR,
                  mapping = aes(x = length_mid, y = catchFished_at_length_count), 
                  fill = "salmon", alpha = 0.5) +
        geom_line(data = NatL_LBSPR,
                  mapping = aes(x = length_mid, y = selectivityF_at_length_count), 
                  colour = "red", lwd = 1) + 
        labs(title = titleFitPlot,
             caption = paste("Data from", input$uploadFile[[1]], sep = " "))#+ 
      #scale_y_continuous(sec.axis = sec_axis(~  . /maxLenDat, name = "selectivity",
      #breaks = c(0, 1),
      #labels = c("0", "1")
      #                                       ) )
      
      expr = ggplotly(pg +  
                        scale_x_continuous(name = length_col) +
                        facet_wrap(vars(year)) + 
                        theme_bw())
    } else if(input$lengthBasedAssessmentMethod == "LIME"){
      fitLIMEobj <- fitLIME()
      
      # length data and year attribute
      length_records <- fitLIMEobj$length_data_raw
      year_col <- names(length_records)[grepl("year", names(length_records), ignore.case = TRUE)]
      length_records$isVulnerable <- length_records[, length_col] >= input$MLL

      # predictions - code extracted from https://github.com/merrillrudd/LIME/blob/master/R/plot_LCfits.R
      Inputs <- fitLIMEobj$lc_only$Inputs
      Report <- fitLIMEobj$lc_only$Report
      nf <- fitLIMEobj$lc_only$input$nfleets
      Tyrs <- fitLIMEobj$lc_only$input$years
      
      # Plot_LCfits code (modified)
      if(all(is.null(Report))==FALSE){
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
        pred_df[[year_col]] <- factor(pred_df[[year_col]])
        pred_df[[length_col]] <- as.numeric(pred_df[[length_col]])
        pred_df$proportion <-  as.numeric(pred_df$proportion)
        pred_df$fleet <- factor(pred_df$fleet)
      }
      pred_df2 <- pred_df %>% mutate("Type"="Predicted") %>% mutate("Model"="LIME")
      
      # plot_LCfits adaption
      pg <- ggplot(length_records) + 
        geom_histogram(aes_string(x = length_col, y = "..density..", fill = "isVulnerable"), breaks = LenBins, closed = "right") + 
        scale_fill_manual(name = "fishery \n vulnerable", breaks = waiver(), values = c("grey20", "grey80"),
                          guide = NULL) +
        geom_line(data=pred_df2 %>% filter(Type=="Predicted"), 
                  aes(x=!!ensym(length_col), y=proportion, color=Model), lwd=1.2) +
        scale_color_brewer(palette="Set1", direction=-1) + 
        facet_wrap(as.formula(paste0(year_col," ~ .")))
      
      #      pg <- plot_LCfits_cf(Inputs=fitLIMEobj$lc_only$Inputs,
      #                        Report=fitLIMEobj$lc_only$Report, plot_type = "counts")
      
      expr = ggplotly(pg + theme_bw()) %>% layout(autosize = TRUE)
    }
    
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

  
  # nlminb (optimisation function for matching expected per-recruit length composition to
  # multinomial log likelihood)
  output$textLBAModelFit <- renderPrint({
    if(input$lengthBasedAssessmentMethod == "LB-SPR"){
      expr <- print(fitGTGLBSPR()$optimOut)  
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
      lbsprFit <- fitGTGLBSPR()$estModelFit
      lbsprStockInput <- setLHPars()
      lbsprGearInput <- setFleetPars()
      yearsLBA <- row.names(lbsprFit)
      M <- lbsprStockInput$M
      FM <- lbsprFit$FM
      SL1 <- format(lbsprFit[,2], digits = 3)
      SL2 <- format(lbsprFit[,3], digits = 3)
      SPR <- format(lbsprFit$SPR, digits = 3)
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
      lime_data <- fitLIME()$lc_only
      # lime_data$input$selex_type, lime_data$Report$F_t, yearsLBA = row.names(fitLIME()$LF)
      # ML95/M95 "length" calculated in create_lh_list() if not specified
      yearsLBA <- row.names(fitLIME()$LF) # could we obtain from lime_data??
      nYears <- length(yearsLBA)
      if(specifySelectivity == "Fixed value"){
        tableData[, yearsLBA] = rbind(rep(lime_data$input$M, nYears), 
                                      signif(lime_data$Report$F_t, 3),
                                      format(lime_data$Report$F_t + lime_data$input$M, digits = 3),
                                      signif(lime_data$Report$SPR_t, 3))
      } else {
        tableData[, yearsLBA] = rbind(rep(lime_data$input$M, nYears), 
                                      signif(lime_data$Report$F_t, 3),
                                      format(lime_data$Report$F_t + lime_data$input$M, digits = 3),
                                      rep(signif(lime_data$Report$S50_f, 3), nYears),
                                      rep(signif(lime_data$Report$S95_f, 3), nYears),
                                      signif(lime_data$Report$SPR_t, 3))
      }
    }
    
    if(specifySelectivity == "Fixed value"){
        tableData %>% kable("html") %>% 
          kable_styling("striped", full_width = F, position = "float_left") %>%
          pack_rows("Mortality", 1, 3) %>% 
          pack_rows("Status", 4, 4)
    } else {
        tableData %>% kable("html") %>% 
          kable_styling("striped", full_width = F, position = "float_left") %>%
          pack_rows("Mortality", 1, 3) %>% 
          pack_rows("Selectivity", 4, 5) %>%
          pack_rows("Status", 6, 6)
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
      lime_data <- fitLIME()$lc_only
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
      kable("html", digits = c(3,3,3,3,3, NA, rep(3,dim(tableData)[1] - 6))) %>%
      kable_styling("striped", full_width = F, position = "float_left") %>%
      pack_rows("Mortality", 1, 1) %>%
      pack_rows("Growth", 2, 3) %>%
      pack_rows("Maturity", 4, 5) %>%
      pack_rows("Selectivity", 6, dim(tableData)[1]) 
  })
  
  
  # interpretation panel
  # visual comparison of exploited and unexploited fish populations
  output$plotCatchFishedUnfished <- renderPlotly({
    
    if(input$lengthBasedAssessmentMethod == "LB-SPR"){
      # data
      length_records <- lengthRecordsFilter()
      length_col <- newLengthCol()
      length_records$isVulnerable <- length_records[, newLengthCol()] >= input$MLL
      
      # theory
      NatL_LBSPR <- fitGTGLBSPR()$NatL_LBSPR
      
      
      # plotly
      pl_y <- plot_ly(data = NatL_LBSPR, 
                      x = ~ length_mid, y = ~ catchUnfished_at_length, name = "unfished", 
                      type = "scatter", mode = "lines+markers", frame = TRUE) %>% 
        add_trace(y = ~ catchFished_at_length, name = "fished", mode = "lines+markers")
      # adding histogram/bar data difficult in plot_ly
      #lengthData <- binLengthData()
      # %>% add_bars(data = lengthData,
      #           x = ~ LenMids, y = LenDatVul, name = "catch data") %>% 
      # layout(barmode = "stack", bargap = 0.1)
      pl_y <- pl_y %>% layout(xaxis = list(title = newLengthCol(), font = "f"),
                              yaxis = list(title = "numbers-at-length (standardised)", font = "f"))
      #title = "Per recruit theory - catch")
    } else if(input$lengthBasedAssessmentMethod == "LIME") {
      pl_y <- plotly_empty() %>% 
        plotly:: config(staticPlot = TRUE)
      
      # LIME - life history and model fit data
      fitLIMEout <- fitLIME()
      lh_fit <- fitLIMEout$lh
      limeFit <- fitLIMEout$lc_only
      
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
      lh_sim <- create_lh_list(vbk= lh_fit$vbk,    # vb growth coefficient
                     linf= lh_fit$linf,# vbg Linf
                     t0= lh_fit$t0,  
                     lwa= lh_fit$lwa,# length-weight W = aL^b: a  
                     lwb= lh_fit$lwb, # length-weight W = aL^b: b
                     S50= limeFit$Report$S50_f,  # selectivity-at-length 50%
                     S95= limeFit$Report$S95_f,  # selectivity-at-length 95%
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
      
      limeSimF0 <- sim_pop(lh_sim, Fdynamics = "None", Rdynamics = "Constant", 
                           Nyears = 20, Nyears_comp = 1, comp_sample = 200, pool = TRUE,
                           init_depl = 0.99, seed = 9999, sample_type = "catch",
                           mgt_type = 'F', fleet_proportions = 1, nareas = 1)
      
      limeSimF <- sim_pop(lh_sim, Fdynamics = "Constant", Rdynamics = "Constant", 
                           Nyears = 20, Nyears_comp = 1, comp_sample = 200, pool = TRUE,
                           init_depl = SPR_Nyear, seed = 9999, sample_type = "catch",
                           mgt_type = 'F', fleet_proportions = 1, nareas = 1)
      
      Nyears_sim <- limeSimF0$Nyears
      catchUnfished <- limeSimF0$plb[[1]]/rowSums(limeSimF0$plb[[1]])
      catchFished <- limeSimF0$plb[[1]]/rowSums(limeSimF$plb[[1]])
      
      NatL_LIME_Fsim <- data.frame(length_mid = limeSimF0$mids,
                                   catchUnfished = limeSimF0$plb[[1]][Nyears_sim,],
                                   catchFished = limeSimF$plb[[1]][Nyears_sim,])
      
      pl_y <- plot_ly(data = NatL_LIME, 
                      x = ~ length_mid, y = ~ catchFished, name = "fished - model fit", 
                      type = "scatter", mode = "lines+markers", frame = TRUE) %>%
        add_trace(data = NatL_LIME_Fsim, x = ~ length_mid, y = ~ catchUnfished, name = "unfished - sim",
                  type = "scatter",mode = "lines+markers") %>% 
        add_trace(data = NatL_LIME_Fsim, x = ~ length_mid, y = ~ catchFished, name = "fished - sim",
                  type = "scatter",mode = "lines+markers")
      pl_y <- pl_y %>% layout(xaxis = list(title = newLengthCol(), font = "f"),
                              yaxis = list(title = "numbers-at-length (standardised)", font = "f"))
    }
    expr <- pl_y
  })
  

  output$plotPopFishedUnfished <- renderPlotly({
    
    if(input$lengthBasedAssessmentMethod == "LB-SPR"){
      # data
      length_records <- lengthRecordsFilter()
      length_col <- newLengthCol()
      length_records$isVulnerable <- length_records[, newLengthCol()] >= input$MLL
      
      # theory
      NatL_LBSPR <- fitGTGLBSPR()$NatL_LBSPR
      head(NatL_LBSPR)
      
      # plotly
      pl_y <- plotly_empty() %>% 
        plotly:: config(staticPlot = TRUE)
      
    } else if(input$lengthBasedAssessmentMethod == "LIME") {

      # LIME - life history and model fit data
      fitLIMEout <- fitLIME()
      lh_fit <- fitLIMEout$lh
      limeFit <- fitLIMEout$lc_only
      
      Nyears_est <- limeFit$input$Nyears
      SPR_Nyear <- limeFit$Report$SPR_t[Nyears_est]
      
      

      # LIME fit of "probability of being in length bin" for final year
      popFished_est <- limeFit$Report$N_ta %*% limeFit$Report$plba
      NatL_LIME <- data.frame(length_mid = limeFit$input$mids,
                              catchFished =  popFished_est[Nyears_est,])
      
      # simulate fishing/no-fishing equilibrium
      # upper limit of 1.3*linf for length_mids/length_bins - more bins than in estimation model
      # model fit selectivity-at-length parameters
      # SigmaR = 0.1 for equilibrium recruitment
      lh_sim <- create_lh_list(vbk= lh_fit$vbk,    # vb growth coefficient
                               linf= lh_fit$linf,# vbg Linf
                               t0= lh_fit$t0,  
                               lwa= lh_fit$lwa,# length-weight W = aL^b: a  
                               lwb= lh_fit$lwb, # length-weight W = aL^b: b
                               S50= limeFit$Report$S50_f,  # selectivity-at-length 50%
                               S95= limeFit$Report$S95_f,  # selectivity-at-length 95%
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
      
      limeSimF0 <- sim_pop(lh_sim, Fdynamics = "None", Rdynamics = "Constant", 
                           Nyears = 20, Nyears_comp = 1, comp_sample = 200, pool = TRUE,
                           init_depl = 0.99, seed = 9999, sample_type = "population",
                           mgt_type = 'F', fleet_proportions = 1, nareas = 1)
      
      limeSimF <- sim_pop(lh_sim, Fdynamics = "Constant", Rdynamics = "Constant", 
                          Nyears = 20, Nyears_comp = 1, comp_sample = 200, pool = TRUE,
                          init_depl = SPR_Nyear, seed = 9999, sample_type = "population",
                          mgt_type = 'F', fleet_proportions = 1, nareas = 1)
      
      Nyears_sim <- limeSimF0$Nyears
      catchUnfished <- limeSimF0$plb[[1]]/rowSums(limeSimF0$plb[[1]])
      catchFished <- limeSimF0$plb[[1]]/rowSums(limeSimF$plb[[1]])
      
      NatL_LIME_Fsim <- data.frame(length_mid = limeSimF0$mids,
                                   catchUnfished = limeSimF0$plb[[1]][Nyears_sim,],
                                   catchFished = limeSimF$plb[[1]][Nyears_sim,])
      
      pl_y <- plot_ly(data = NatL_LIME, 
                      x = ~ length_mid, y = ~ catchFished, name = "fished - model fit", 
                      type = "scatter", mode = "lines+markers", frame = TRUE) %>%
        add_trace(data = NatL_LIME_Fsim, x = ~ length_mid, y = ~ catchUnfished, name = "unfished - sim",
                  type = "scatter",mode = "lines+markers") %>% 
        add_trace(data = NatL_LIME_Fsim, x = ~ length_mid, y = ~ catchFished, name = "fished - sim",
                  type = "scatter",mode = "lines+markers")
      pl_y <- pl_y %>% layout(xaxis = list(title = newLengthCol(), font = "f"),
                              yaxis = list(title = "numbers-at-length (standardised)", font = "f"))
    }
    expr <- pl_y
  })
  
    
  output$plotFishingEstimateOutput <- renderPlot({
    if(input$lengthBasedAssessmentMethod == "LIME"){
      lc_only <- fitLIME()$lc_only
      lh <- fitLIME()$lh
      p <- plot_output(Inputs=lc_only$Inputs,
                  Report=lc_only$Report,
                  Sdreport=lc_only$Sdreport,
                  lh=lh,
                  True=NULL,
                  plot=c("Fish","Rec","SPR","ML","SB","Selex"),
                  set_ylim=list("Fish"=c(0,mean(lc_only$Report$F_t)*2),"SPR"=c(0,1)))
    } else if(input$lengthBasedAssessmentMethod == "LB-SPR"){
      p <- plot.new()
      estModelFit <- fitGTGLBSPR()$estModelFit
      NatL_LBSPR <- fitGTGLBSPR()$NatL_LBSPR
      StockPars <- setLHPars()
      
      FishM <- estModelFit$FM*StockPars$M # unscaled fishing mortality
      # extract years
      estModelFit$year <- row.names(estModelFit)
      row.names(estModelFit) <- NULL
      
      cat(paste0("estModelFit$year = ", estModelFit$year, "\n"))
      cat(paste0("estModelFit$FM = ", estModelFit$FM, "\n"))
      cat(paste0("estModelFit$SPR= ", estModelFit$SPR, "\n"))
      
      par(mgp = c(3,1,0), mar = c(4,5,2,2)+0.1)
      layout(matrix(c(1,2,3,3),2,2, byrow = TRUE), c(1,1), c(1,1), TRUE)
      if(input$analyseLengthComposition == "all periods"){
        yearsLBA <- estModelFit$year
        
        # fishing mortality
        barplot(FishM, width = 0.6, names.arg = yearsLBA, 
                cex.axis = 2, cex.names = 2, cex.lab = 2, axes = TRUE, axisnames = TRUE,
                ylab = "Fishing mortality", ylim = c(0, 1.5*max(FishM)), space = 0.4)
        abline(h = StockPars$M, col = "red", lty = 2, lwd = 2)
        
        # SPR
        barplot(estModelFit$SPR, width = 0.6, names.arg = yearsLBA, 
                cex.axis = 2, cex.names = 2, cex.lab = 2, axes = TRUE, axisnames = TRUE,
                ylab = "SPR", ylim = c(0,1), space = 0.4)
        abline(h = 0.4, col = "red", lty = 2, lwd = 2)
        
        # selectivity-at-length
        plot(x=1, y=1, type="n", xlim = range(NatL_LBSPR$length_mid), ylim = c(0,1),
             xlab = "length", ylab = "selectivity", col = "black", main = yearsLBA,
             lwd = 2, cex = 2, cex.axis = 2, cex.lab = 2, cex.lab=2 )
        lengthMid_year <- NatL_LBSPR$length_mid
        selectivityF_year <- NatL_LBSPR$selectivityF_at_length
        lines(lengthMid_year, selectivityF_year, lwd = 2)
        points(lengthMid_year, selectivityF_year, lwd = 2, pch = 1)
        
      } else {
        estModelFit$year <- as.integer(estModelFit$year)
        yearsLBA <- estModelFit$year
        
        # fishing mortality
        plot(yearsLBA, FishM, 
             type = "p", lwd = 1.5, pch = 19, cex = 2, cex.axis = 2, cex.lab = 2,
             xlab = "year", ylab = "F/M", ylim = c(0, max(FishM)))
        lines(yearsLBA, FishM, lwd = 1, lty = 1)
        abline(h = StockPars$M, col = "red", lty = 2, lwd = 2)
        
        # SPR
        plot(yearsLBA, estModelFit$SPR, 
             type = "p", lwd = 1.5, pch = 19, cex = 2, cex.axis = 2, cex.lab = 2,
             xlab = "year", ylab = "SPR", ylim = c(0,1))
        lines(yearsLBA, estModelFit$SPR, lwd = 1, lty = 1)
        abline(h = 0.4, col = "red", lty = 2, lwd = 2)

        # selectivity-at-length
        plot(x=1, y=1, type="n", xlim = range(NatL_LBSPR$length_mid), ylim = c(0,1),
             xlab = "length", ylab = "selectivity", col = "black", 
             lwd = 2, cex = 2, cex.axis = 2, cex.lab = 2, cex.lab=2 )
        for (year_plot in yearsLBA){
          lengthMid_year <- NatL_LBSPR$length_mid[NatL_LBSPR$year == year_plot]
          selectivityF_year <- NatL_LBSPR$selectivityF_at_length[NatL_LBSPR$year == year_plot]
          lines(lengthMid_year, selectivityF_year, lwd = 2)
          points(lengthMid_year, selectivityF_year, lwd = 2, pch = 1)
          i_sl50 <- which.min(abs(selectivityF_year-0.5))[1]
          x_plot <- lengthMid_year[i_sl50]
          y_plot <- selectivityF_year[i_sl50]
          print(paste(x_plot, y_plot, sep = ", "))
          text(x_plot, y_plot, labels = as.character(year_plot), cex = 1.5, col = "red")
        }
      }
    }
    p
  })
  
  
  output$plotPopLBSPR <- renderPlot({
    NatL_LBSPR <- fitGTGLBSPR()$NatL_LBSPR
    LPopUnfished <- NatL_LBSPR$popUnfished_at_length
    LPopFished <- NatL_LBSPR$popFished_at_length
    LenMids <- NatL_LBSPR$length_mid
    print(max(LenMids))
    
    estModelFit <- fitGTGLBSPR()$estModelFit  
    SL50 <- estModelFit$Estimate[estModelFit$Parameter == "SL1"]
    SL95 <- estModelFit$Estimate[estModelFit$Parameter == "SL2"]
    SLmin <- SL50 - (SL95-SL50)
    
    par(mfrow = c(2,1), mgp = c(2,1,0), mar = c(4,3,3,1), cex = 1.15)
    plot(LenMids, LPopFished, col = "grey25", pch = 1, lwd = 1.5,
         xlab = newLengthCol(), ylab = "numbers per recruit",
         main = "entire length range", font.main = 1)
    lines(LenMids, LPopFished, col = "grey25", lty = 1, lwd = 1.5)
    points(LenMids, LPopUnfished, col = "grey75", pch = 16, lwd = 1.5)
    lines(LenMids, LPopUnfished, col = "grey75", lty = 1, lwd = 1.5)
    legend("topright", c("fished", "unfished"), 
           col = c("grey25", "grey75"), 
           pch = c(1, 16))
    
    plot(LenMids, LPopFished, col = "grey25", pch = 1, lwd = 1.5,
         xlab = newLengthCol(), ylab = "numbers per recruit",
         xlim = c(SL50-(SL95-SL50), max(LenMids)),
         ylim = c(0, 1.25*LPopUnfished[which(LenMids-SLmin == min(abs((LenMids-SLmin)))) ]),
         main = "vulnerable length range", font.main = 1)
    lines(LenMids, LPopFished, col = "grey25", lty = 1, lwd = 1.5)
    points(LenMids, LPopUnfished, col = "grey75", pch = 16, lwd = 1.5)
    lines(LenMids, LPopUnfished, col = "grey75", lty = 1, lwd = 1.5)
    legend("topright", c("fished", "unfished"), 
           col = c("grey25", "grey75"), 
           pch = c(1, 16))
    
  })
  
  
  output$diagnosticParameterFits <- renderPlotly({
    if(input$lengthBasedAssessmentMethod == "LIME") {
      
      # LIME - life history and model fit data
      fitLIMEout <- fitLIME()
      #lh_fit <- fitLIMEout$lh
      limeFit <- fitLIMEout$lc_only
      years_lime <- limeFit$input$years
      
      diagnosticLIME <- as.data.frame(limeFit$opt$diagnostics)
      # fishing mortality by year -  use two digits...for proper ordering of years
      diagnosticLIME$Param[diagnosticLIME$Param == "log_F_ft"] <- sprintf(fmt = "%s.%02d", "log_F_ft", years_lime)

      
      # parameter constraints
      parConstraintsLIME <- diagnosticLIME %>% select(Param, Lower, Upper, final_gradient) %>%
        mutate(Lower = ifelse(is.finite(Lower), Lower, -100),
               Upper = ifelse(is.finite(Upper), Upper, 100),
               Domain = "lightgreen")
      
      # parameter estimates
      diagnosticEstimatesLIME <- diagnosticLIME %>%
        rename(InitialEstimate = starting_value, MaximumLikelihoodEstimate = MLE) %>% 
        pivot_longer(cols = ends_with("Estimate"), names_to = "Estimate", values_to = "Value", names_pattern = "(.*)Estimate") %>% 
        select(Param, Estimate, Value)
      
      # estimate confidence intervals from standard error from covariance matrix
      ciEstimatesLIME <- data.frame(Param = diagnosticLIME$Param,
                                    MLE = limeFit$Sdreport$par.fixed,
                                    LowerCI = limeFit$Sdreport$par.fixed -1.96*sqrt(diag(limeFit$Sdreport$cov.fixed)),
                                    UpperCI = limeFit$Sdreport$par.fixed +1.96*sqrt(diag(limeFit$Sdreport$cov.fixed)))

      # x-axis range
      x_min <- floor(min(diagnosticLIME$Lower[is.finite(diagnosticLIME$Lower)], 
                         diagnosticLIME$starting_value, diagnosticLIME$starting_value,
                         diagnosticLIME$MLE))
      x_max <- ceiling(max(diagnosticLIME$Upper[is.finite(diagnosticLIME$Upper)], 
                           diagnosticLIME$starting_value,
                           diagnosticLIME$MLE))

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
      
    } else if(input$lengthBasedAssessmentMethod == "LB-SPR" &&
              input$analyseLengthComposition == "all periods") {
      fitLBSPR <- fitGTGLBSPR()
      # initial estimates, MLEs, confidence intervals
      parConstraintsLBSPR <- 
        data.frame(Parameter = fitLBSPR$MLE$Parameter,
                   Lower = rep(-Inf, dim(fitLBSPR$MLE)[1]),
                   Upper = rep(0, dim(fitLBSPR$MLE)[1])) 
      parConstraintsLBSPR$Upper[parConstraintsLBSPR$Parameter == "log(F/M)"] <- Inf   
      parConstraintsLBSPR <- parConstraintsLBSPR %>% 
        mutate(Lower = ifelse(is.finite(Lower), Lower, -100),
                Upper = ifelse(is.finite(Upper), Upper, 100),
                Domain = "lightgreen")
      
      diagnosticEstimatesLBSPR <- fitLBSPR$MLE %>%
        rename(InitialEstimate = Initial, MaximumLikelihoodEstimate = Estimate) %>% 
        pivot_longer(cols = ends_with("Estimate"), names_to = "Estimate", values_to = "Value", names_pattern = "(.*)Estimate") %>% 
        select(Parameter, Estimate, Value)
      
      
      # standard error from covariance matrix
      ciEstimatesLBSPR <- data.frame(Parameter = fitLBSPR$MLE$Parameter,
                                     MLE = fitLBSPR$MLE$Estimate,
                                     LowerCI = fitLBSPR$MLE$Estimate-1.96*fitLBSPR$MLE$`Std. Error`,
                                     UpperCI = fitLBSPR$MLE$Estimate +1.96*fitLBSPR$MLE$`Std. Error`)
      
      # x-axis range
      x_min <- floor(min(fitLBSPR$MLE$Initial, fitLBSPR$MLE$Estimate, ciEstimatesLBSPR$LowerCI))
      x_max <- ceiling(max(fitLBSPR$MLE$Initial, fitLBSPR$MLE$Estimate, ciEstimatesLBSPR$UpperCI))

      pg <- ggplot() +
        geom_segment(data = parConstraintsLBSPR,
                     aes(y = Parameter, yend = Parameter, x = Lower, xend = Upper), size = 5, lineend = "butt",
                     colour = "lightgreen") +
        geom_point(data = diagnosticEstimatesLBSPR,
                   aes(y = Parameter, x = Value, shape = Estimate), size = 5, colour = "black") +
        geom_errorbarh(data = ciEstimatesLBSPR,
                       aes(y = Parameter, xmin = LowerCI, xmax = UpperCI), height = 0.5) +
        scale_x_continuous(name = "Value", breaks = seq(x_min,x_max,1)) +
        scale_y_discrete(name = "MLE parameters") +
        scale_shape_manual(values = c(1, 16)) + #scale_colour_manual(name = waiver(), values = "lightgreen", breaks = "lightgreen", labels = NULL) +
        coord_cartesian(xlim = c(x_min, x_max)) +
        theme_bw() +
        theme(axis.text = element_text(size = 12),
              axis.title = element_text(size = 12))
      pg
      
    }
    ggplotly(p = pg) %>% highlight("plotly_selected")
  })
}