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
  
  # debug
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
  catchdata_table <- eventReactive(
    input$selectCols,
    { catchdata <- catchdata_read()[, c(input$checkboxCatchData, paste0(input$lengthColSelect))]
      charCols <- which(sapply(catchdata, is.character))
      if(!(length(charCols) == 0 & is.integer(charCols))) {
        catchdata[, charCols] <- sapply(catchdata[, charCols], trimws)
      } else {
        catchdata <- data.frame(catchdata)
        names(catchdata) <- c(input$checkboxCatchData, paste0(input$lengthColSelect))
      }
      catchdata
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
      
      if(!is.null(input$checkboxCatchData)){
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
        } else if (any(whichSexCol)) {
          # species and sex 
          pg <- pg + 
            geom_histogram(aes_(x = input$lengthColSelect, fill = ensym(sexCol)), closed = "left", boundary = 0, bins = 40) + 
            facet_grid(rows = ensym(speciesCol), scales = "free")
          
        } else{
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
      } else {
        pg <- pg + geom_histogram(aes_(x = input$lengthColSelect), fill ="grey50",
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
  
  lengthRecordsFilter <- reactive(
    #   eventReactive(input$convertLengthUnits,
    { 
      lengthScale <- lengthRecordsScale()
      #length_records <- catchdata_table() %>% select(input$lengthColSelect) %>% 
      #  mutate("{newLengthCol()}" := .data[[input$lengthColSelect]]*lengthScale)
      length_records <- catchdata_table()[input$catchDataTable_rows_all,,drop = FALSE] %>% #select(input$lengthColSelect) %>% 
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
    renderDT(
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
                 print("debug observe event - print growthFrequentistFit(), growthFrequentistFitBoot()")
                 print(growthFrequentistFit())
                 print(growthFrequentistFitBoot())
                 print("are ggplots?")
                 print(is.ggplot(ggGrowthFitMean()))
                 print(is.ggplot(ggGrowth_CurveALData()))
                 print(tags$p("(/^weight/).test(input.checkboxCatchData)"))
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
  
  
  observeEvent(input$natMortality, {
    if(input$natMortality == "user"){
      updateNumericInput(session, "M", value = 0.3)      
    } else if(input$natMortality == "pauly") {
      updateNumericInput(session, "M", value = round(4.118*input$kLvb^0.73*input$Linf^(-0.33), 3))
    } else if(input$natMortality == "twoK"){
      updateNumericInput(session, "M", value = round(0.098 + 1.55*input$kLvb, 3))
    } else if(input$natMortality == "hoenig") {
      updateNumericInput(session, "M", value = round(4.899*max(gatherFishAgeLengthData()$age, na.rm = FALSE)^-0.916, 3))
    }
      #if(is.na(max(gatherFishAgeLengthData()$age, na.rm = FALSE)))
      #if(is.na(tmax)) { NA } else { round(4.899*tmax^-0.916, 3) }
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
      x <- c("Estimate (model fit)", "Specify (user)")
    } else {
      x <- c("Specify (user)")
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
    if(input$specifySelectivity == "Specify (user)" & !is.null(input$specifySelectivity)){
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
    } else if (input$specifySelectivity == "Estimate (model fit)" & !is.null(input$specifySelectivity)) {
      tagList(tags$p("Estimate selectivity parameters in model fitting process"))
    }
  })
  
  # slider controls
  selectivityParsSliderInp <- reactive({
    if(input$specifySelectivity == "Specify (user)" & !is.null(input$specifySelectivity)){
      
      if(input$selectSelectivityCurve == "Knife-edged" & !is.null(input$selectSelectivityCurve)){
        tList <- tagList(
          sliderInput(inputId = "SLKnife", label = "MLL",
                      value = round(input$Linf*0.50, digits = 2),
                      min = 0.0, max = input$Linf, step = 1)
        )
      } else if(input$selectSelectivityCurve == "Logistic" & !is.null(input$selectSelectivityCurve)) {
        tList <- tagList(
          sliderInput(inputId = "SL1", label = "Length at 50% selectivity",
                      value = round(input$Linf*0.70, digits = 2), min = 0, max = input$Linf, step = round(input$Linf/50)/2),
          sliderInput(inputId = "SL2", label = "Length at 95% selectivity",
                      value = round(input$Linf*0.80, digits = 2), min = 0, max = input$Linf, step = round(input$Linf/50)/2)
        )
      } else if(input$selectSelectivityCurve %in% c("Normal.sca", "Normal.loc") &
                !is.null(input$selectSelectivityCurve)) {
        tList <- tagList(
          sliderInput(inputId = "SL1", label = "Length with maximum selectivity",
                      value = round(input$Linf*0.70, digits = 2), min = 0, max = input$Linf, step = 1),
          sliderInput(inputId = "SL2", label = "SD (spread) of dome-shaped selectivity curve",
                      value = round(input$Linf*0.10, digits = 2), min = 0, max = input$Linf, step = 1),
          sliderInput(inputId = "SLKnife", label = "MLL",
                      value = round(input$Linf*0.0, digits = 2),
                      min = 0.0, max = input$Linf,  step = 1)
        )
      } else if(input$selectSelectivityCurve %in% c("logNorm") & !is.null(input$selectSelectivityCurve)) {
        tList <- tagList(
          sliderInput(inputId = "SL1", label = "Length with maximum selectivity",
                      value = round(input$Linf*0.70, digits = 2), min = 0, max = input$Linf, step = 1),
          sliderInput(inputId = "SL2", label = "Log-normal SD/spread of dome-shaped selectivity curve",
                      value = round(0.50, digits = 2), min = 1e-3, max = 2, step = 0.01),
          sliderInput(inputId = "SLKnife", label = "MLL",
                      value = round(input$Linf*0.0, digits = 2),
                      min = 0.0, max = input$Linf,  step = 1)
        )
      }
    } else if (input$specifySelectivity == "Estimate (model fit)" & !is.null(input$specifySelectivity)) {
      tList <- tagList(tags$p("Estimate selectivity parameters in model fitting process"))
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
                        SLmesh = 1, selexCurve = input$selectSelectivityCurve)
                    } else {
                      if(input$selectSelectivityCurve == "Logistic") {
                        list(selexCurve = input$selectSelectivityCurve)
                      } else if(input$selectSelectivityCurve == "Knife") {
                        list(SLKnife = input$SLKnife, selexCurve = input$selectSelectivityCurve)
                      } 
                    }
                    } else if(input$lengthBasedAssessmentMethod == "LIME"){
                      list(S50 = input$SL1, S95 = input$SL2,
                           selexBy = "length",
                           selexCurve = tolower(input$selectSelectivityCurve),
                           nfleets = 1 # where is the best place for this?
                           )
                    }
    })

  
  # selectivity curve data frame - reactive or reactiveValues?
  selectionCurves <- reactive({
    # nb handle SLKnife, SLMLL dependent on selectivityCurve value (logistic, knife-edged etc.)?
    if(input$specifySelectivity == "Specify (user)" & 
       !(is.null(input$SL1) | is.null(input$SL2)) ){
      isolate(dSC <- data.frame(
        length = seq(min(lengthRecordsFilter()[,newLengthCol()], na.rm = TRUE), input$Linf, length.out = 51),
        size = 1, quantity = "selectivity"))
      SLmesh <- 1
      if(input$selectSelectivityCurve == "Logistic"){
        dSC$proportion <- 1.0/(1+exp(-log(19)*(dSC$length-input$SL1)/(input$SL2-input$SL1)))
      } else if(input$selectSelectivityCurve == "Normal.loc") {
        dSC$proportion <- exp(-0.5*((dSC$length-((input$SL1)*SLmesh))/(input$SL2))^2)
        dSC$proportion[dSC$length < input$SLKnife] <- 0
      } else if(input$selectSelectivityCurve == "Normal.sca") {
        dSC$proportion <- exp(-0.5*((dSC$length-((input$SL1)*SLmesh))/((input$SL2^0.5)*SLmesh)^2))
        dSC$proportion[dSC$length < input$SLKnife] <- 0
      } else if(input$selectSelectivityCurve == "logNorm") {
        dSC$proportion <- exp(-0.5*((log(dSC$length)-log((input$SL1)*SLmesh))/(input$SL2))^2)
        dSC$proportion[dSC$length < input$SLKnife] <- 0
      }
    } else {
      dSC <- NULL
    }
    dSC
  })
  
  
  # plot selectivity pattern provided input$specifySelectivity == "Specify (user)"

  #observeEvent(
  #  input$btnFixedFleetPars, {
      output$plotSelectivityPattern <- renderPlotly({
        print("before isolate")
        print(min(lengthRecordsFilter()[,newLengthCol()], na.rm = TRUE))
        isolate(length_vals <- seq(min(lengthRecordsFilter()[,newLengthCol()], na.rm = TRUE), 
                           input$Linf, length.out = 201))
        
        ggdata <- rbind(selectionCurves(),
                        data.frame(length = length_vals, 
                                   proportion = 1.0/(1+exp(-log(19)*(length_vals-input$Lm50)/(input$Lm95-input$Lm50))),
                                   size = 2,
                                   quantity = "maturity")
        )
        expr = ggplotly(ggplot(ggdata) + 
                          geom_line(aes( x = length, y = proportion, colour = quantity, size = size), ) +
                          scale_colour_manual(values = c("red", "black")) +
                          scale_size_identity() + 
                          labs(title = "User-specified maturity and selectivity") + # could have reactive
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
    
    # check for year attribute
    year_col <- names(length_records)[grepl("year", names(length_records), ignore.case = TRUE)]
    if(any(grepl("year", names(length_records), ignore.case = TRUE))){
      year_min <- min(length_records[, year_col])
      year_max <- max(length_records[, year_col])
    }

    # length_col (+ optional year_col): exclude NAs
    length_col <- newLengthCol()
    length_records <- na.omit(lengthRecordsFilter()[, c(year_col,length_col)])
    
    # vulnerable to fishery 
    isVulnerable <- (lengthMids >= input$MLL)
    
    # bin length data
    if(length(year_col)==0L | input$lengthBasedAssessmentMethod == "LB-SPR"){
      print("length_records")
      print(is.numeric(length_records[, length_col]))
      LF <- hist(length_records[, length_col], plot = FALSE, breaks = lengthBins, right = FALSE)
      print(LF)
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
      list_out <- list(LenDat = LF$counts,
                    LenDatVul = LF$counts*isVulnerable)
    } else if(input$lengthBasedAssessmentMethod == "LIME") {
      list_out <- list(LenDat = LFYear,
                    LenDatVul = LFVulYear)      
    }
    list_out
  })
  
  # app navigation
  # btnStockPars causes move to next tab
  observeEvent(input$btnStockPars, {
    print(setLHPars())
    updateTabsetPanel(session, inputId = "tabMain", selected = "tabSelectivity")
  })
  
  # btnTechnicalStockPars 
  observeEvent(input$btnTechnicalStockPars,
               {length_records <- lengthRecordsFilter()[, newLengthCol()]
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
  #              updateRadioButtons(inputId = "visualiseLengthComposition",
  #                                 label = "Visualise...",
  #                                 choices = rbChoices) 
  #              #, selected = "in aggregate")
  #              })
  collateLengthChoice <- reactive({
    lengthRecordAttributes <- input$checkboxCatchData
    if(input$lengthBasedAssessmentMethod == "LB-SPR"){
      collationChoice <- "all"
      if(any(grepl("year", lengthRecordAttributes, ignore.case = TRUE))){
        collationChoice <- c(collationChoice, 
                             paste0("each ", grep("year", lengthRecordAttributes, ignore.case = TRUE, value = TRUE)))
      } 
    } else if(input$lengthBasedAssessmentMethod == "LIME"){
      if(any(grepl("year", lengthRecordAttributes, ignore.case = TRUE))){
        collationChoice <- paste0("each ", grep("year", lengthRecordAttributes, ignore.case = TRUE, value = TRUE))
      } else {
        collationChoice <- "all"
      }
    }
    collationChoice
  })
  
  observe({ 
    updateRadioButtons(inputId = "visualiseLengthComposition",
                       label = "Assess...",
                       choices = collateLengthChoice())#, selected = defaultChoice)
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
      print("visualiseLengthComposition")
      print(input$visualiseLengthComposition)
      if(input$visualiseLengthComposition == 
         paste0("each ", grep("year", colnames(lengthRecordsFilter()), ignore.case = TRUE, value = TRUE))
         ){ # was observeEvent or eventReactive 
        print(paste0(grep("year", colnames(lengthRecordsFilter()), ignore.case = TRUE,
                          value = TRUE)," ~ ."))
        ggLengthComp <- ggLengthComp + 
          facet_wrap(as.formula(paste0(grep("year", colnames(lengthRecordsFilter()), ignore.case = TRUE,
                                            value = TRUE)," ~ ."))) 
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
      titleFitPlot <- "Model-estimated selectivity parameters"
      if(input$specifySelectivity == "Specify (user)"){
        titleFitPlot <- "User-specified selectivity parameters"
      }
      fixedFleetPars <- setFleetPars()
      names(fixedFleetPars)[names(fixedFleetPars) == "selexCurve"] <- "selectivityCurve"
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
      
      print("GTG LBSPR")
      print(StockPars)
      print(fixedFleetPars)
      print(LenDatVul)
      print(SizeBins)
      
      # GTG-LBSPR optimisation
      optGTG <- DoOptDome(StockPars,  fixedFleetPars, LenDatVul, SizeBins, "GTG")#, input$selectSelectivityCurve)
      # optGTG$Ests
      # optGTG$PredLen
      # optGTG$nlminbOut

      if(input$specifySelectivity == "Specify (user)"){
        optFleetPars <- list(FM = optGTG$Ests["FM"],
                             selectivityCurve = optGTG$SelectivityCurve,
                             SL1 = fixedFleetPars$SL1, 
                             SL2 = fixedFleetPars$SL2,
                             SLMin = fixedFleetPars$SLMin,
                             SLmesh = fixedFleetPars$SLmesh)
      } else if(input$specifySelectivity == "Estimate (model fit)") {
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
                                             unname(optFleetPars$SL1), 
                                             unname(optFleetPars$SL2),
                                             prGTG$SPR))
      #        opModelOut <- data.frame(Parameter = c("SPR", "YPR"),
      #                                 Description = c("Spawning Potential Ratio", "Yield-per-recruit"),
      #                                 Estimate = c(prGTG$SPR, prGTG$YPR))      
      if(input$specifySelectivity == "Specify (user)"){
        estModelFit$Source <- c("Model fit", "User-specified", "User-specified", "Model")
      }
      
      list(NatL_LBSPR = NatL_LBSPR,
           estModelFit = estModelFit,
           nlminbOut = optGTG$nlminbOut
           #opModelOut = opModelOut
           )
    }
  )
  

  fitLIME <- reactive(
    {
      lhParVals <- isolate(setLHPars()) # only evaluated if fitLIME called
      fleetParVals <- isolate(setFleetPars()) 
      binwidth <- isolate(input$Linc)
      print("fleet par vals")
      print(fleetParVals)
      # selectivity parameters fitted or estimated
      titleFitPlot <- "Model-estimated selectivity parameters"
      if(input$specifySelectivity == "Specify (user)"){
        titleFitPlot <- "User-specified selectivity parameters"
      }
      
      # how to handle specifying/estimating SL50/SL95 for logistic
      if(tolower(fleetParVals$selexCurve) == "logistic"){
        if(input$specifySelectivity == "Estimate (model fit)"){
          S50 <- 0.66*lhParVals$Linf  # initial estimate of selectivity-at-length 50%
          S95 <- 0.80*lhParVals$Linf
        } else if(input$specifySelectivity == "Specify (user)") {
          S50=fleetParVals$SL1 # initial estimate of selectivity-at-length 50%
          S95=fleetParVals$SL2
        }
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
                          data_avail="LC")
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
               {updateNavbarPage(session, inputId = "methodLBSPR", selected = "tabModelFit")})
  
  observeEvent(input$btnFixedFleetPars,
               updateTabsetPanel(session, inputId = "tabMain", selected = "tabLBSPR"))
  
  # print text on LBSPR estimating model fit 
  output$tableLBAEstimates <- reactive(
    {
    if(input$lengthBasedAssessmentMethod == "LB-SPR"){
      fitGTGLBSPR()$estModelFit %>%
        knitr::kable("html", digits = 3) %>%
        kable_styling("striped", full_width = F, position = "float_left")
    } else if(input$lengthBasedAssessmentMethod == "LIME"){
      NULL
      # fitLIME()$Report %>%
      #   knitr::kable("html", digits = 3) %>%
      #   kable_styling("striped", full_width = F, position = "float_left")
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
    length_records$isVulnerable <- length_records[, newLengthCol()] >= input$MLL
    
    LenBins <- createLengthBins()$LenBins
    LenDat <- binLengthData()$LenDatVul # vulnerable to fishery only
    # maxLenDat <- max(LenDat)
    if(input$lengthBasedAssessmentMethod == "LB-SPR"){
      NatL_LBSPR <- fitGTGLBSPR()$NatL_LBSPR
      
      # pivot_longer
      NatL_long <- NatL_LBSPR %>%
        pivot_longer(cols = ends_with("at_length"),
                     names_to = "quantity",
                     names_pattern = "(.*)_at_length",
                     values_to = "numbers-per-recruit")
      
      
      if(input$specifySelectivity == "Specify (user)"){
        titleFitPlot <- "Length data, LB-SPR fit and selectivity curve (specified)"
      } else if (input$specifySelectivity == "Estimate (model fit)") {
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
      
      expr = ggplotly(pg +  
                        scale_x_continuous(name = length_col) +
                        theme_bw())
    } else if(input$lengthBasedAssessmentMethod == "LIME"){
      fitLIMEobj <- fitLIME()
      
      # length data and year attribute
      length_records <- fitLIMEobj$length_data_raw
      year_col <- names(length_records)[grepl("year", names(length_records), ignore.case = TRUE)]

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
        geom_histogram(aes_string(x = length_col, y = "..density.."), breaks = LenBins, closed = "right") + 
        geom_line(data=pred_df2 %>% filter(Type=="Predicted"), 
                  aes(x=!!ensym(length_col), y=proportion, color=Model), lwd=1.2) +
        facet_wrap(as.formula(paste0(year_col," ~ ."))) + 
        scale_color_brewer(palette="Set1", direction=-1)
      
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
      expr <- print(fitGTGLBSPR()$nlminbOut)  
    } else if(input$lengthBasedAssessmentMethod == "LIME") {
      expr <- print(fitLIME()$lc_only$Sdreport)
    }
    
    
  })
  
  # stock paramater summary
  output$stockPopParameters <- reactive({
    tableData <- data.frame(Notation = c("M", "F", "Z", "Linf", "K", "Lm50", "Lm95", "SLCurve", "SL1", "SL2", "SLMin", "SPR"),
                            Description = c("Natural mortality", "Fishing mortality", "Total mortality",
                                            "Asymptotic length", "LVB growth constant", 
                                            "Length-at-50%-maturity", "Length-at-95%-maturity", 
                                            "Selectivity-at-length curve", "Selectivity-at-length parameter 1", "Selectivity-at-length parameter 2", 
                                            "Minimum length limit", "Spawning potential ratio"),
                            Estimate = NA
                            )
    if(input$lengthBasedAssessmentMethod == "LB-SPR"){
      lbsprFit <- fitGTGLBSPR()$estModelFit
      lbsprStockInput <- setLHPars()
      lbsprGearInput <- setFleetPars()
      FM <- lbsprFit[lbsprFit$Parameter == "FM",]$Estimate
      M <- lbsprStockInput$M
      tableData$Esimtate <- c(M, M*FM, M*(1 + FM), 
                              lbsprStockInput$Linf, lbsprStockInput$K, lbsprStockInput$L50, lbsprStockInput$L95, 
                              lbsprGearInput$selexCurve,
                              ifelse(is.null(lbsprGearInput$SL1), lbsprFit$Estimate[lbsprFit$Parameter=="SL50"], lbsprGearInput$SL1),
                              ifelse(is.null(lbsprGearInput$SL2), lbsprFit$Estimate[lbsprFit$Parameter=="SL95"], lbsprGearInput$SL2),
                              ifelse(is.null(lbsprGearInput$SLMin), NA, lbsprGearInput$SLMin),  
                              lbsprFit[lbsprFit$Parameter == "SPR",]$Estimate)
    } else if(input$lengthBasedAssessmentMethod == "LIME") {
      lime_data <- fitLIME()$lc_only
      print("lime_data$input$selex_type")
      print(lime_data$input$selex_type)
      tableData$Estimate = c(lime_data$input$M, mean(lime_data$Report$F_t), mean(lime_data$Report$F_t)+lime_data$input$M, 
                             lime_data$input$linf, lime_data$input$vbk, 
                             lime_data$input$ML50, lime_data$input$ML95, 
                             as.character(lime_data$input$selex_type),
                             lime_data$Report$S50_f,lime_data$Report$S95_f,
                             input$MLL, 
                             mean(lime_data$Report$SPR_t))
    }
    tableData %>%
      kable("html", digits = c(3,3,3,1,3,2,2,NA,3,3,3,3)) %>%
      kable_styling("striped", full_width = F, position = "float_left") %>%
      pack_rows("Mortality", 1, 3) %>%
      pack_rows("Growth", 4, 5) %>%
      pack_rows("Maturity", 6, 7) %>%
      pack_rows("Status", 8, 12)
  })
  
  
  # interpretation panel
  # visual comparison of exploited and unexploited fish populations
  output$plotCatchLBSPR <- renderPlotly({
    
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
    }
    expr <- pl_y
  })
  
  
  output$plotLIMEOutput <- renderPlot({
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
  
}