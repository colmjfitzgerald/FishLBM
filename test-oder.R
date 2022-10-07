source("./server.R")

# set variable for use in testServer
oderFilePath <- "C:/Users/cfitzgerald/workspace/length_based_assessment/data/Oder_FishData.csv"
oder_csv <- read.csv(oderFilePath)

# outputs
oder_assessment <- read.csv("C:/Users/cfitzgerald/workspace/length_based_assessment/test/odertest-2022-07-27/stock_status.csv")
oder_assess_mle_logFM <- log(oder_assessment[oder_assessment$quantity == "F", "mean"]/oder_assessment[oder_assessment$quantity == "M", "mean"])

options(warn = 1)

# https://mastering-shiny.org/scaling-testing.html
# testServer() ignores UI. That means inputs don’t get default values, 
# and no JavaScript works. Most importantly this means that you can’t 
# test the update* functions, because they work by sending JavaScript 
# to the browser to simulates user interactions. 
testServer(server, {
  
  # file upload ####
  # specify the uploadFile, unitConvertRadioBtn
  session$setInputs(uploadFile = list(datapath = oderFilePath),
                    unitConvertRadioBtn = "Keep units unchanged")

  # check file input
  cat("file address is :", input$uploadFile$datapath, "\n")
  cat("input file column names are:", colnames(catchdata_read()), "\n")
  cat("expect oder file data is identical:", identical(oder_csv, catchdata_read()), "\n")
  
  
  # select length column and other attributes ####
  # update* functions don't work (see note above testServer() call)
  cat("input lengthColSelect is.null:", is.null(input$lengthColSelect), "\n")
  
  # set lengthColSelect using updateVarSelectInput expression
  session$setInputs(lengthColSelect = ifelse(any(grepl("length", colnames(catchdata_read()), ignore.case = TRUE)),
                                             grep("length", colnames(catchdata_read()), ignore.case = TRUE, value = TRUE),
                                             NULL) )
  cat("input lengthColSelect is:", input$lengthColSelect, "\n")
  
  # checkboxCatchData from updateCheckboxGroupInput expression
  session$setInputs(checkboxCatchData = setdiff(colnames(catchdata_read()),
                                               paste0(input$lengthColSelect)))
  cat("input checkboxCatchData has elements:", input$checkboxCatchData, "\n")
  
  
  # choose columns from length data file
  session$setInputs(checkboxCatchData = c("Year", "Method", "Species"))
  cat("no error yet?\n")
  
  
  # Data upload/overview: click enter data button ####
  # server renderUI
  # actionButton("selectCols", "Input and plot data", icon = icon("table"))
  session$setInputs(selectCols = 1, lengthBasedAssessmentMethod = "LB-SPR") 
  # lengthBasedAssessmentMethod = NULL yields error in collateLengthChoices
  
  cat("***after click selectCols button***\n" )
  cat(file = stderr(), "selected length column: ", input$lengthColSelect, "\n") # output through stderr()
  cat("catchdata_table() columns:", colnames(catchdata_table()), "\n")
  cat("number of catchdata_table() rows:", nrow(catchdata_table()), "\n")
  cat("input checkboxCatchData has elements:", input$checkboxCatchData, "\n")
  cat("input checkboxCatchData species cols: ", grepl("species", input$checkboxCatchData, ignore.case = TRUE), "\n")
  cat("catchdata_plot() is ggplot:", is.ggplot(catchdata_plot()), "\n")
  cat("***\n")
  
  
  # simulate click of convert length units button ####
  # DT filtering - necessary because of reactivity cascade to lengthRecordsFilter
  index_silver_bream <- which(catchdata_table()[, "Species"] == "Silver bream")
  index_trawl <- which(catchdata_table()[, "Method"] == "Trawl")
  
  session$setInputs(convertLengthUnits = 1, 
                     dataLengthUnits = "cm", newLengthUnits = "cm",
                     catchDataTable_rows_all = intersect(index_silver_bream, index_trawl))
  
  # lengthRecordsScale
  cat("lengthRecordsScale() value = ", lengthRecordsScale(), "\n")
  
  # lengthRecordsFilter reactive
  # - lengthRecordsScale()
  # - input$lengthColSelect
  # - input$catchDataTable_rows_all
  
  # filter length records
  cat("lengthRecordsFilter() reactive is.null?", is.null(lengthRecordsFilter()), "\n")
  
  # lengthRecordsFilter
  cat("collateLengthChoices: ", paste(collateLengthChoice(), sep = ","), "\n")
  cat("number of lengthRecordsFilter() rows:", nrow(lengthRecordsFilter()), "\n")
  cat("\n")
  
  
  # growth curve fitting ####
  cat("***\n")
  cat("growth curve fit\n")
  cat("growth fit button value: ", input$fitGrowth, "\n")
  # age data
  cat("anyAgeData?", anyAgeData(), "\n")
  cat("gatherFishAgeData has nrow = ", nrow(gatherFishAgeLengthData()), "\n")
  
  # fit growth ####
  # session$setInputs(fitGrowth = 1)
  # cat("hit growth fit button\n")
  # print(growthParChoices())
  # print("debug observe event - print growthFrequentistFit(), growthFrequentistFitBoot()")
  # print(growthFrequentistFit())
  # print(growthFrequentistFitBoot())
  # print("are ggplots?")
  # print(is.ggplot(ggGrowthFitMean()))
  # print(is.ggplot(ggGrowth_CurveALData()))
  # print(tags$p("(/^weight/).test(input.checkboxCatchData)"))
  
  
  
  # life history parameters ####
  cat("***\n")
  cat("Life History Parameters\n")
  cat("natural mortality choices = ", natMestChoices(), "\n")
  session$setInputs(CVLinf = 0.1, Walpha = 0.00001, Wbeta = 3,
                    Linf = 42, kLvb = 0.175, natMortality = "twoK", slidert0 = -0.01)
  cat("natural mortality model: ", input$natMortality, " gives value =", updateMortality(), "\n")
  
  # assign mortality and length-at-maturity values using update... reactives
  session$setInputs(M = updateMortality(), Lm50 = updateMaturity(), Lm95 = updateMaturity95())
  cat("length at 50% maturity =", input$Lm50, "\n")
  cat("updated mortality =", input$M, "\n")
  

  
  # gear selectivity ####
  cat("***\n")
  cat("Gear selectivity\n")
  session$setInputs(chooseSelectivityPattern = "Asymptotic", 
                    specifySelectivity = "Initial estimate")
  cat("***test\n")
  cat("size selectivity curve options = (", paste(sizeSelectivityCurves(), sep = ","), ")", "\n")
  cat("selectSelectivityCurve = ", input$selectSelectivityCurve, "\n" )
  cat("***test\n")
  
  # recreate opening gear selectivity page
  session$setInputs(selectSelectivityCurve = sizeSelectivityCurves()[1],
                    selectGearMeshSizes = 1,
                    SL1 = input$Linf*0.70, SL2 = input$Linf*0.80, SLKnife = NULL)
  cat("size selectivity curve parameters = (", paste(c(input$SL1, input$SL2), sep = ","), ")", "\n")
  cat("selectSelectivityCurve = ", input$selectSelectivityCurve, "\n" )

  
  # length-based stock assessment ####
  # LB-SPR technical parameters
  session$setInputs(FecB = 3, Steepness = 0.8, Mpow = 0, NGTG = 13, MaxSD = 2,
                    btnTechnicalStockPars = 1)
  cat("LB-SPR technical parameters entered\n" )
  
  # LIME technical parameters
  session$setInputs(
    SigmaR = 0.737, SigmaF = 0.2, SigmaC = 0.1, SigmaI = 0.1, # variance terms for processes (R,F) and obs
    R0 = 1, Frate = 0.1, Fequil = 0.25, qcoef = 1e-5, start_ages = 0, 
    rho = 0.43, nseasons = 1, theta = 10)
  cat("LIME technical parameters entered\n" )

  length_records <- lengthRecordsFilter()[, newLengthCol()]
  cat("NA length records:\n", length_records[is.na(lengthRecordsFilter()[, newLengthCol()])], "\n")

  cat("setFleetPars = ", paste(setFleetPars(), sep = ","), "\n")
    
  # length bin and temporal discretisation
  session$setInputs(analyseLengthComposition = "annual", Linc = 1, MLL = 0)
    
  # check length bins
  cat(createLengthBins()$LenBins, "\n")
  
  # click button and check fit
  session$setInputs(fitLBA = 1)

  # LB-SPR pars
  cat(fitLBSPR()$mlePars$log_FM, "\n")
  cat("sum of squared differences between fitLBSPR and benchmark run", 
      sum((fitLBSPR()$mlePars$log_FM - oder_assess_mle_logFM)^2), "\n")
  
  cat("createPlotLBAestimates reactive is.NULL", is.null(createPlotLBAestimates()), "\n")
  
  cat("************************** end of LB-SPR test *************************\n")
  cat("***********************************************************************\n")
  cat("***********************************************************************\n")
  cat("***********************************************************************\n")

  # LIME run ####
  # fitLIME is **reactive** so we must set all parameters at the same time
  session$setInputs(MLL = 19)
  cat("MLL = ", input$MLL, "\n")
  cat("************************** start LIME test ****************************\n")
  session$setInputs(lengthBasedAssessmentMethod = "LIME")
  cat("slidert0 =", input$slidert0, "\n")
  
  # click button
  cat("click button\n")
  #session$setInputs(fitLBA = 1)
  #session$setInputs(sigmaR = 1.0)
  
  # sequence of events is out of sync perhaps?
  # LIME technical parameters are specified at the same time as LB-SPR
  # so we recreate this in test-oder
  # examine if
})

