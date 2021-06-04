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
                                     selected = ifelse(any(grepl("length", colnames(catchdata_read()), ignore.case = TRUE)),
                                                       grep("length", colnames(catchdata_read()), ignore.case = TRUE, value = TRUE),
                                                       NULL))})
  
  # updates checkboxCatchData
  observeEvent(input$lengthColSelect,
               {
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
  
  observeEvent(input$selectCols,
               { print(paste0("input select data"))
                 print(input$lengthColSelect)
               })

    
  # dynamic UI elements (renderUI, insertUI etc.) ----
  
  # Tabulate data action button and reaction ====
  
  # length data category column selection ####
  output$btnSelectCols <- renderUI({
    actionButton("selectCols", "Input and plot data", icon = icon("table"))
  })
  
  
  
  # catchdata element for plotting and LBSPR
  catchdata_table <- eventReactive(
    input$selectCols,
    { catchdata <- catchdata_read()[, c(input$checkboxCatchData, paste0(input$lengthColSelect))]
      charCols <- which(sapply(catchdata, is.character))
      catchdata[, charCols] <- sapply(catchdata[, charCols], trimws)
      catchdata
    }
  )
  
  observeEvent(input$selectCols,
               {print(input$checkboxCatchData)
                print(grepl("species", input$checkboxCatchData, ignore.case = TRUE))
                }
               )
  
  # catchdata_plot
  catchdata_plot <- eventReactive(
    input$selectCols,
    { 
      # identify columns for grid plotting
      whichSexCol <- grepl("sex", input$checkboxCatchData, ignore.case = TRUE)
      whichGearCol <- grepl("gear|method", input$checkboxCatchData, ignore.case = TRUE)
      whichYearCol <- grepl("year", input$checkboxCatchData, ignore.case = TRUE)
      whichSpeciesCol <- grepl("species", input$checkboxCatchData, ignore.case = TRUE)
      
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
      speciesCol <- input$checkboxCatchData[whichSpeciesCol]
      gearCol <- input$checkboxCatchData[whichGearCol]
      yearCol <- input$checkboxCatchData[whichYearCol]
      sexCol <- input$checkboxCatchData[whichSexCol]
      # facet by species (rows) if multispecies
      if(any(whichSpeciesCol)){
        # years as col, sex as colours
        if(any(whichYearCol)) {
          # if gear and sex, use sex as colour category
          if(any(whichSexCol)){
            pg <- pg + 
              geom_histogram(aes_(x = input$lengthColSelect, fill = ensym(sexCol)),
                             closed = "left", boundary = 0, bins = 40)
          } else if (any(whichGearCol)){
            pg <- pg + 
              geom_histogram(aes_(x = input$lengthColSelect, fill = ensym(gearCol)),
                             closed = "left", boundary = 0, bins = 40)
          } else {
            pg <- pg + 
              geom_histogram(aes_(x = input$lengthColSelect),
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
              geom_histogram(aes_(x = input$lengthColSelect, fill = ensym(sexCol)),
                             closed = "left", boundary = 0, bins = 40)
          } else {
            pg <- pg + 
              geom_histogram(aes_(x = input$lengthColSelect),
                             closed = "left", boundary = 0, bins = 40)
          }
          pg <- pg + facet_grid(rows = as.formula(paste0(speciesCol, " ~ ", gearCol)), scales = "free")
        } else {
          # species only
          pg <- pg + 
            geom_histogram(aes_(x = input$lengthColSelect), closed = "left", boundary = 0, bins = 40) + 
            facet_grid(rows = ensym(speciesCol), scales = "free")
        }
      } else {
        # no species
        if(any(whichSexCol)){ 
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
          # no species or sex
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
              facet_grid(rows = as.formula(paste0(yearCol, " ~ ", gearCol)), scales = "free")
          }
        }
        
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
  
  lengthRecordsFilter <- reactive(
    #   eventReactive(input$convertLengthUnits,
    { 
      lengthScale <- lengthRecordsScale()
      #length_records <- catchdata_table() %>% select(input$lengthColSelect) %>% 
      #  mutate("{newLengthCol()}" := .data[[input$lengthColSelect]]*lengthScale)
      length_records <- catchdata_table()[input$catchDataTable_rows_all,] %>% #select(input$lengthColSelect) %>% 
        mutate("{newLengthCol()}" := .data[[input$lengthColSelect]]*lengthScale)
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
    lengthCol <- newLengthCol()
    sexCol <- grep("sex", input$checkboxCatchData, ignore.case = TRUE, value = TRUE)
    if(any(grepl("age", input$checkboxCatchData, ignore.case = TRUE))){
      ageCol <- grep("age", input$checkboxCatchData, ignore.case = TRUE, value = TRUE)
      lengthAge <- data.frame(lengthRecordsFilter()[, c(newLengthCol())], 
                              lengthRecordsFilter()[, ageCol],
                              lengthRecordsFilter()[, sexCol])
      colnames(lengthAge) <- c(newLengthCol(), ageCol, sexCol)
      lengthAgeData <- lengthAge[!is.na(lengthAge[, ageCol]) & !is.na(lengthAge[, newLengthCol()]),]
    } else {
      lengthAgeData <- data.frame(lengthRecordsFilter()[, c(newLengthCol())], 
                                  age = NA,
                                  sex = lengthRecordsFilter()[, sexCol])
      colnames(lengthAgeData) <- c(newLengthCol(), "age", sexCol)
      lengthAgeData <- lengthAgeData[!is.na(lengthAgeData[, newLengthCol()]),]
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
                   start = list(Linf=input$sliderLinf, K = input$sliderK, t0=input$slidert0))
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
                            start = list(Linf=40.,K=0.2,t0=0.0))
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
    growthcurve <- createPlotSliderCurveData()
    fishAgeLengthData <- gatherFishAgeLengthData()
    
    p <- ggplot()
    p <- p +
      geom_line(data = growthcurve,
                aes(x = age, y = length_cm), colour = "grey75", alpha = 0.5, size = 1.5)
    
    # age-length or just length data
    if(all(is.na(fishAgeLengthData[, "age"]))){
      if(nrow(fishAgeLengthData) > 400){
        fishAgeLengthDataSubSample <- 
          rbind(fishAgeLengthData[fishAgeLengthData[ ,newLengthCol()] == min(fishAgeLengthData[ , newLengthCol()]),],
            fishAgeLengthData[fishAgeLengthData[ ,newLengthCol()] == max(fishAgeLengthData[ , newLengthCol()]),],
            fishAgeLengthData[sampleLengthRecords(),])  
      } else {
        fishAgeLengthDataSubSample <- fishAgeLengthData
      }
      p <- p + 
        geom_rug(data = fishAgeLengthDataSubSample, mapping = aes(y = length_cm)) 
      
    } else {
      if("sex" %in% colnames(fishAgeLengthData)){
        p <- p + 
          geom_point(data = fishAgeLengthData, mapping = aes(x = age, y = !!sym(newLengthCol()), 
                                                             colour = sex), alpha = 0.5)
      } else {
        p <- p + 
          geom_point(data = fishAgeLengthData, mapping = aes(x = age, y = !!sym(newLengthCol())), 
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
                 print(growthParChoices())
                 print("debug observe event")
                 print(growthFrequentistFit())
                 print(growthFrequentistFitBoot())
                 print("are ggplots?")
                 print(is.ggplot(ggGrowthFitMean()))
                 print(is.ggplot(ggGrowth_CurveALData()))
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
  
  # numeric inputs for LBSPR
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
                             L50 = input$Lm50, # length at first maturity
                             L95 = input$Lm95)
                })
  
  
  # selectivity curve ####
  # insertUI
  # output$selectivityNote <- renderText("User specifies selectivity parameters")
  output$selectivityParameters <- renderUI(
    { 
      if(input$specifySelectivity == "Specify"){
        tagList(
          numericInput(inputId = "SL50", label = "Length at 50% selectivity",
                       value = round(input$Linf*0.70, digits = 2)),
          numericInput(inputId = "SL95", label = "Length at 95% selectivity",
                       value = round(input$Linf*0.80, digits = 2)),
          actionButton(inputId = "btnFixedFleetPars", "Input selectivity parameters",
                       class = "btn-success"),
          bsTooltip(id = "SL50", 
                    title = "Length at 50% selectivity<br> <em>Length at which 50% of a stock is caught by fishery gear.</em> Replace default <b>SL50 = 0.7 Linf</b> with empirical data or expert knowledge. Influences F/M statistical inference.", 
                    placement = "left", trigger = "hover",  options = list(container = "body")),
          bsTooltip(id = "SL95", 
                    title = "Length at 95% selectivity<br> <em>Length at which 95% of a stock is caught by fishery gear.</em> Replace default <db>SL50 = 0.8 Linf</b> with empirical data or expert knowledge. Influences F/M statistical inference.", 
                    placement = "left", trigger = "hover",  options = list(container = "body"))
        )
      } else {
        tagList(renderText("Estimate selectivity parameters in model fitting process"))
      }
    })
  
  reactiveFixedFleetPars <- eventReactive(input$btnFixedFleetPars,
                                          {list(SL50 = input$SL50, SL95 = input$SL95)
                                          })
  
  # plot selectivity pattern provided input$specifySelectivity == "Specify"
  observeEvent(
    input$btnFixedFleetPars, {
      length_vals <- seq(min(lengthRecordsFilter()[,newLengthCol()], na.rm = TRUE), 
                       input$Linf, length.out = 51)

      ggdata <- rbind(data.frame(length = length_vals, 
                          proportion = 1.0/(1+exp(-log(19)*(length_vals-input$SL50)/(input$SL95-input$SL50))),
                          size = 1,
                          quantity = "selectivity"),
                     data.frame(length = length_vals, 
                                proportion = 1.0/(1+exp(-log(19)*(length_vals-input$Lm50)/(input$Lm95-input$Lm50))),
                                size = 2,
                                quantity = "maturity")
                     )
      output$plotSelectivityPattern <- renderPlotly({
       expr = ggplotly(ggplot(ggdata) + 
                         geom_line(aes( x = length, y = proportion, colour = quantity, size = size), ) +
                         scale_colour_manual(values = c("red", "black")) +
                         scale_size_identity() + 
                         labs(title = "User-specified selectivity and maturity") +
                         theme_bw())
     })
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
    
    # input length data, exclude NAs
    length_col <- newLengthCol()
    length_records <- na.omit(lengthRecordsFilter()[, length_col])
    
    histogram_length <- hist(length_records, plot = FALSE, breaks = LenBins, right = FALSE)
    
    # vulnerable 
    isLenBinFished <- (LenMids >= input$MLL)
    
    
    # bins and binned counts
    list(SizeBins = SizeBins,
         LenBins = LenBins,
         LenMids = LenMids,
         LenDat = histogram_length$counts,
         LenDatVul = histogram_length$counts*isLenBinFished)
  }
  )
  
  observeEvent(input$btnStockPars,
               {print("debug observeEvent")
                 print(binLengthData())
                 length_records <- lengthRecordsFilter()[, newLengthCol()]
                 print(length_records[is.na(lengthRecordsFilter()[, newLengthCol()])])})
  
  
  # visualise length composition by year - optional selection radio button
  observeEvent(input$selectCols,
               { rbChoices <- c("in aggregate")
                 if(any(grepl("year", input$checkboxCatchData, ignore.case = TRUE))){
                   rbChoices <- c(rbChoices, 
                                  paste0("by ", 
                                         grep("year", input$checkboxCatchData, ignore.case = TRUE, value = TRUE))
                                  )
                 }
                 updateRadioButtons(inputId = "visualiseLengthComposition",
                                    label = "Visualise...",
                                    choices = rbChoices,
                                    selected = "in aggregate")
               })
  
  
  # plot length composition of filtered data - change with slider input
  output$plotResponsiveLengthComposition <- 
    renderPlotly({
      lengthData <- lengthRecordsFilter()
      lengthData$isVulnerable <- lengthData[, newLengthCol()] >= input$MLL
      if(all(lengthData$isVulnerable, na.rm = TRUE)) {
        ggLengthComp <- ggplot(lengthData %>% filter(!is.na(!!sym(newLengthCol())))) +  
          geom_histogram(mapping = aes_string(x = newLengthCol()), fill = "grey80",
                         breaks = binLengthData()$LenBins, # slideLenBins(),
                         closed = "left", colour = "black") +
          geom_vline(xintercept = input$MLL, colour = "red", linetype = 2, size = 1) +
          theme_bw()
      } else {
        ggLengthComp <- ggplot(lengthData %>% filter(!is.na(!!sym(newLengthCol())))) + 
          geom_histogram(mapping = aes_string(x = newLengthCol(), fill = "isVulnerable"),
                         breaks = binLengthData()$LenBins, # slideLenBins(),
                         closed = "left", colour = "black") +
          scale_fill_manual(name = "fishery \n vulnerable", breaks = waiver(), values = c("grey20", "grey80")) + 
          geom_vline(xintercept = input$MLL, colour = "red", linetype = 2, size = 1) +
          theme_bw() + 
          theme(legend.position = "bottom")
      }
      if(input$visualiseLengthComposition == 
         paste0("by ", grep("year", colnames(lengthRecordsFilter()), ignore.case = TRUE, value = TRUE))
         ){ # was observeEvent or eventReactive 
        print(paste0(grep("year", colnames(lengthRecordsFilter()), ignore.case = TRUE,
                          value = TRUE)," ~ ."))
        ggLengthComp <- ggLengthComp + 
          facet_wrap(as.formula(paste0(grep("year", colnames(lengthRecordsFilter()), ignore.case = TRUE,
                                            value = TRUE)," ~ ."))) 
      }
      expr = ggplotly(ggLengthComp)
    })
  

  # Fit LBSPR ####
  fitGTGLBSPR <- eventReactive(
    input$fitLBSPR,
    {
      # as.name
      length_records <- lengthRecordsFilter()
      length_col <- newLengthCol()
#      print(head(length_records[, length_col]))
      
      StockPars <- reactiveStockPars()
      StockPars$MK <- StockPars$M/StockPars$K
      
      fixedFleetPars <- NULL
      titleFitPlot <- "Model-estimated selectivity parameters"
      if(input$specifySelectivity == "Specify"){
        fixedFleetPars <- reactiveFixedFleetPars()
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
      SizeBins <- binLengthData()$SizeBins
      LenBins <- binLengthData()$LenBins
      LenMids <- binLengthData()$LenMids
      LenDat <- binLengthData()$LenDat
      LenDatVul <- binLengthData()$LenDatVul
      
      
      
      # GTG-LBSPR optimisation
      optGTG <- DoOpt(StockPars,  fixedFleetPars, LenDatVul, SizeBins, "GTG")
      # optGTG$Ests
      # optGTG$PredLen
      # optGTG$nlminbOut
      
      if(input$specifySelectivity == "Specify"){
        optFleetPars <- list(FM = optGTG$Ests["FM"],
                             SL50 = fixedFleetPars$SL50, 
                             SL95 = fixedFleetPars$SL95)
      } else if(input$specifySelectivity == "Estimate") {
        optFleetPars <- list(FM = optGTG$Ests["FM"],
                             SL50 = optGTG$Ests["SL50"], 
                             SL95 = optGTG$Ests["SL95"])
      }
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
      

      estModelFit <- data.frame(Parameter = c("FM", "SL50", "SL95", "SPR"),
                                Description = c("F/M: relative fishing mortality",
                                                "Length at 50% selectivity",
                                                "Length at 95% selectivity",
                                                "Spawning Potential Ratio"),
                                Estimate = c(unname(optFleetPars$FM), 
                                             unname(optFleetPars$SL50), 
                                             unname(optFleetPars$SL95),
                                             prGTG$SPR))
      #        opModelOut <- data.frame(Parameter = c("SPR", "YPR"),
      #                                 Description = c("Spawning Potential Ratio", "Yield-per-recruit"),
      #                                 Estimate = c(prGTG$SPR, prGTG$YPR))      
      if(input$specifySelectivity == "Specify"){
        estModelFit$Source <- c("Model fit", "User-specified", "User-specified", "Model")
      }
      
      list(NatL_LBSPR = NatL_LBSPR,
           estModelFit = estModelFit,
           nlminbOut = optGTG$nlminbOut
           #opModelOut = opModelOut
           )
    }
  )
  
  
  # move panel within navBarPage after fit
  observeEvent(input$fitLBSPR,
               {updateTabsetPanel(session, inputId = "parLBSPR", selected = "modelFit")})
  
  
  # print text on LBSPR estimating model fit 
  output$textLBSPREstFit <- function()
    {
    fitGTGLBSPR()$estModelFit %>%
      knitr::kable("html", digits = 3) %>%
      kable_styling("striped", full_width = F, position = "float_left")
    }
    
  #renderPrint({
  #  expr = print(fitGTGLBSPR()$estModelFit)
  #})
  
  # # print text on LBSPR operating model output
  # output$textLBSPROpOut <- renderPrint({
  #   expr = print(fitGTGLBSPR()$opModelOut)
  # })
  
  output$visFitLBSPR <- renderPlotly({
    # length data
    length_records <- lengthRecordsFilter()
    length_col <- newLengthCol()
    length_records$isVulnerable <- length_records[, newLengthCol()] >= input$MLL
    
    LenBins <- binLengthData()$LenBins
    LenDat <- binLengthData()$LenDatVul # vulnerable to fishery only
    # maxLenDat <- max(LenDat)
    
    NatL_LBSPR <- fitGTGLBSPR()$NatL_LBSPR
    
    # pivot_longer
    NatL_long <- NatL_LBSPR %>%
      pivot_longer(cols = ends_with("at_length"),
                   names_to = "quantity",
                   names_pattern = "(.*)_at_length",
                   values_to = "numbers-per-recruit")
    
    
    if(input$specifySelectivity == "Specify"){
      titleFitPlot <- "Length data, LB-SPR fit and selectivity curve (specified)"
    } else if (input$specifySelectivity == "Estimate") {
      titleFitPlot <- "Length data, LB-SPR fit and selectivity curve (estimated)"
    }
    
    # create ggplot with data...
    pg <- ggplot(length_records %>% filter(isVulnerable) ) + 
      geom_histogram(mapping = aes_string(x = length_col), breaks = LenBins, 
                     closed = "left", colour = "black", fill = "grey75")
    
    # ...and fit
    pg <- pg + 
      geom_area(data = NatL_LBSPR,
                mapping = aes(x = length_mid, y = max(LenDat)*catchFished_at_length), 
                fill = "salmon", alpha = 0.5) +
      geom_line(data = NatL_LBSPR,
                mapping = aes(x = length_mid, y = max(LenDat)*selectivityF_at_length), 
                colour = "red", lwd = 1) + 
      labs(title = titleFitPlot,
           caption = paste("Data from", input$uploadFile, sep = " "))#+ 
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
  output$plotCatchLBSPR <- renderPlotly({
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
    expr <- pl_y
  })
  
  # nlminb (optimisation function for matching expected per-recruit length composition to
  # multinomial log likelihood)
  output$textFitLBSPR <- renderPrint({
    expr <- print(fitGTGLBSPR()$nlminbOut)
    
  })
  
  # stock paramater summary
  output$stockPopParameters <- reactive({
    lbsprFit <- fitGTGLBSPR()$estModelFit
    lbsprInput <- reactiveStockPars()
    FM <- lbsprFit[lbsprFit$Parameter == "FM",]$Estimate
    M <- lbsprInput$M
    tableData <- data.frame(Notation = c("M", "F", "Z", "Linf", "K", "Lm50", "Lm95", "SL50", "SL95", "SPR"),
                            Description = c("Natural mortality", "Fishing mortality", "Total mortality",
                               "Asymptotic length", "LVB growth constant", "Length-at-50%-maturity",
                               "Length-at-95%-maturity", "Spawning potential ratio"),
                            Estimate = c(M, M*FM, M*(1 + FM), lbsprInput$Linf, lbsprInput$K,
                            lbsprInput$L50, lbsprInput$L95, 
                            lbsprFit[lbsprFit$Parameter == "SPR",]$Estimate)
    )
    tableData %>%
      kable("html", digits = 3) %>%
      kable_styling("striped", full_width = F, position = "float_left") %>%
      pack_rows("Mortality", 1, 3) %>%
      pack_rows("Growth", 4, 5) %>%
      pack_rows("Maturity", 6, 7) %>%
      pack_rows("Status", 8, 8)
  })
  
  # visual comparison of exploited and unexploited fish populations
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
  
}