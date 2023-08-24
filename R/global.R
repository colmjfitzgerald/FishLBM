# global variables used in server function
utils::globalVariables(c("Estimate", "Initial", "Lower", "LowerCI", "Model", "Param", "Parameter",
                         "Type", "Upper", "UpperCI", "Value", "Year", "age", "catchFished_at_length_count",
                         "final_gradient", "fleet", "isVulnerable", "length_cm", "length_mid", 
                         "length_p_025", "length_p_500", "length_p_975", "mortality", "proportion", 
                         "weight_p_500", "weight", "density", "width",
                         "quantity", "scaled_proportion", "selectivityF_at_length_count", "sex", 
                         "size", "starting_value", "varFishingAtLength", "varSPR", "year",
                         "lci", "uci", "selectivity",
                         "lengthMid", "meanSL", "lowerci", "upperci", "lowerciSL", "upperciSL"))


# weight-length module ####

# ui
fitWeightLengthInput <- function(id, label = "uiWeightLengthFitButton"){
  ns <- NS(id)
  tagList(
    actionButton(ns("fitWeight"), label = "Fit weight-length curve", class = "btn-success"),
    hr(),
    radioButtons(ns("weightLengthError"), label = "Weight-length model",
                 choices = c("Additive error: W = aL^b + e" = "add", 
                             "Multiplicative error: W = (aL^b) x e" = "mult"))
  )
}

fitWeightLengthOutput <- function(id, label = "uiWeightLengthFitSummary"){
  ns <- NS(id)
  verbatimTextOutput(outputId = ns("fitSummary"))
} 

# server
fitWeightLengthServer <- 
  function(id, dataLW) {
    moduleServer(
      id, 
      function(input, output, session){
        stopifnot(is.reactive(dataLW))
        
        lwFit <- eventReactive(input$fitWeight, {
          lwdata <- dataLW()
          if(!any(grepl("weight", colnames(lwdata), ignore.case = TRUE)) || all(is.na(lwdata$weight))){
            lmLW <- NULL # or data lwdata??
            walpha <- NULL
            wbeta <- NULL
          } else if(input$weightLengthError == "mult" ) {
            # fit linear regression after looking at data
            # W = (alpha*L^beta)*epsilon
            # ln(W) = ln(alpha) + beta*ln(L) + ln(epsilon)
            lwdata$logL <- log(lwdata[, "length"])
            lwdata$logW <- log(lwdata[, "weight"])
            lmLW <- lm(logW ~ logL, data = lwdata)
            wsigma <- sigma(lmLW) 
            walpha <- exp(coefficients(lmLW)[1])*exp((wsigma^2)/2)
            wbeta <- coefficients(lmLW)[2]
          } else if(input$weightLengthError == "add"){
            # W = alpha*L^beta + epsilon
            lmLW <- nls(weight ~ walpha*length^wbeta, data = lwdata, start = list(walpha = 0.0001, wbeta = 3))
            walpha <- coefficients(lmLW)[1]
            wbeta <- coefficients(lmLW)[2]
          }
          x <- list(lm = lmLW, data = lwdata, walpha = walpha[[1]], wbeta = wbeta[[1]])
        })
        
        # lwFit() already eventReactive...
        lw_fit_mean <- reactive({
          stopifnot("length" %in% colnames(dataLW()))
          stopifnot("weight" %in% colnames(dataLW()))
          
          length_p <- seq(0, max(dataLW()[, "length"]), length.out = 51)
          weight_p <- (lwFit()$walpha)*length_p^(lwFit()$wbeta)
          
          data.frame(length = length_p, weight_p_500 = weight_p)
        })
        
        # fitted mean weight-at-length line
        geom_line_lw_fit <- reactive({
          if(input$fitWeight > 0){
            LW <- lw_fit_mean()
            if(is.null(LW)){
              x <- geom_blank()
            } else {
              x <- geom_line(data = LW,
                             mapping = aes(x = length, y = weight_p_500), colour = "red")
            }
          } else {
            x <- geom_blank()
          }
          x
        })
        
        
        output$fitSummary <- renderPrint({
          expr = print(summary(lwFit()$lm, correlation = TRUE))
        })
        
        return(list(lwFit = lwFit, geom_line_lw_fit = geom_line_lw_fit))
      } 
    )
  }


# module to add-remove LIME/LB-SPR outputs ####
# basic ui structure into and from which we inset and remove ui objects
insertRemovePlotOutput <- function(id, label = "lengthBasedMethodOutput"){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 4, div(id = ns("outMLFit"))),
      column(width = 4, div(id = ns("outSSBFit"))),
      column(width = 4, div(id = ns("outRFit")))
    )
  )
}

insertRemovePlotServer <- 
  function(id, lbsaMethod, lbsaOutput, btnLbsaFit) {
    moduleServer(
      id, 
      function(input, output, session){
        stopifnot(is.reactive(lbsaMethod), is.reactive(lbsaOutput), is.reactive(btnLbsaFit))
        
        addPlotRows <- reactiveVal(0)
        # insertUI with tags$div(): has the advantage that you can give it an id to make it easier to reference or remove it later on
        ns <- session$ns
        observeEvent(req(lbsaMethod()), 
                     { if(addPlotRows() == 0 && lbsaMethod() == "LIME"){
                       insertUI(selector = paste0("#", ns("outMLFit")),
                                where = "afterBegin",
                                tags$div(id = ns("plotMLFit"), plotly::plotlyOutput(outputId = ns("plotMLFit")))
                       )
                       insertUI(selector = paste0("#", ns("outSSBFit")),
                                where = "afterBegin",
                                tags$div(id = ns("plotSSBFit"), plotly::plotlyOutput(outputId = ns("plotSSBFit")))
                       )
                       insertUI(selector = paste0("#", ns("outRFit")),
                                where = "afterBegin",
                                tags$div(id = ns("plotRecFit"), plotly::plotlyOutput(outputId = ns("plotRecFit")))
                       )
                       addPlotRows(1)
                       } else if (addPlotRows() == 1 && lbsaMethod() == "LB-SPR")
                     { removeUI(selector = paste0("#", ns("plotMLFit")))
                       removeUI(selector = paste0("#", ns("plotSSBFit")))
                       removeUI(selector = paste0("#", ns("plotRecFit")))
                       addPlotRows(0)
                     }
        
        })
        
        observeEvent(req(lbsaMethod()) == "LIME" && btnLbsaFit(),
                     { #browser()
                     output$plotMLFit <- plotly::renderPlotly(
                       ggplotly_config(lbsaOutput()$plotML, 
                                       paste0(tolower(sub("-", "", lbsaMethod())), "PlotML")))
                     output$plotSSBFit <- plotly::renderPlotly(
                       ggplotly_config(lbsaOutput()$plotSB, 
                                       paste0(tolower(sub("-", "", lbsaMethod())), "PlotSB")))
                     output$plotRecFit <- plotly::renderPlotly(
                       ggplotly_config(lbsaOutput()$plotRecruit, 
                                       paste0(tolower(sub("-", "", lbsaMethod())), "PlotRecruit")))
      })
      }
    )  
  }

# module to produce LIME/LB-SPR length compositions ####

# basic ui structure into and from which we inset and remove ui objects
lcPlotTweakInput <- function(id, label = "lengthCompositionInput"){
  ns <- NS(id)
  tagList(
    numericInput(ns("axisTitleFontSize"), label = "Axis label font size", min = 6, max = 16, step = 1, value = 10), #11*relSize
    numericInput(ns("axisFontSize"), label = "Axis text font size", min = 6, max = 16, step = 1, value = 10), # 11*relSize
    numericInput(ns("stripFontSize"), label = "Facet text font size", min = 6, max = 16, step = 1, value = 10), # 11*relSize
    numericInput(ns("panelSpacing"), label = "Facet plot spacing", min = 1, max = 15, step = 1, value = 5),
    selectInput(ns("statHist"), label = "Histogram statistic", choices = c("count"), selected = "count") #"density", "proportion"),
  )
}

lcPlotOutput <- function(id, label = "lengthCompositionOutput"){
  ns <- NS(id)
  plotly::plotlyOutput(outputId = ns("plotLengthComposition"), width = "100%", height = "800px")
}

lcPlotServer <- 
  function(id, lengthDataInput, createLengthBins, Linc, MLL) {
    moduleServer(
      id, 
      function(input, output, session){
        stopifnot(is.reactive(lengthDataInput), is.reactive(createLengthBins), is.reactive(Linc), is.reactive(MLL))
        
        themeTweak <- reactive({theme(axis.text = element_text(size = input$axisFontSize),
                                        axis.title = element_text(size = input$axisTitleFontSize),
                                        strip.text = element_text(size = input$stripFontSize),
                                        panel.spacing = unit(input$panelSpacing, "pt"))
        })
        
        pgLengthComposition <- reactive({
          ggplot_length_composition(lengthDataInput(), createLengthBins()$LenBins, Linc(), MLL(), input$statHist) + 
            themeTweak()
          }) 
          

        output$plotLengthComposition <- renderPlotly({
          gglc <- pgLengthComposition() +
            geom_vline(xintercept = MLL(), colour = "red", linetype = 2, linewidth = 1)
          expr = plotly::ggplotly(gglc) %>% plotly::layout(legend = list(orientation = "h"))
          })

        return(list(pg = pgLengthComposition, 
                    statHist = reactive({input$statHist}),
                    themeTweak = themeTweak ))
      }
    )
  }


# non-reactive functions available to each user session ====

# bin length data
# how to replace the above?
createAnnualLF <- function(length_records, length_col, year_col, lengthBins, allYears = FALSE) {
  length_yearly_all <- na.omit(length_records[, c(year_col,length_col), drop = FALSE])
  # create length bins
  length_yearly_all$lengthBin = cut(length_yearly_all[, length_col], breaks = lengthBins, right = FALSE)
  # select (categorical) year levels
  if (allYears) {
    yearLevels <- seq(min(length_yearly_all[, year_col]), max(length_yearly_all[, year_col]), 1)
  } else {
    yearLevels <- unique(length_yearly_all[, year_col])
  }
  length_yearly_all$year <- factor(length_yearly_all[, year_col], levels = yearLevels)
  # build contingency table of counts at each combination of factor levels
  LF <- table(length_yearly_all$year, length_yearly_all$lengthBin, dnn = c("year", "lengthBin"))
  colnames(LF) <- as.character(lengthBins)[-1]
  LF
}

# length composition plots ####
ggplot_length_composition <- function(lengthDataInput, lengthBins, Linc, MLL, stat_type){
  stopifnot(is.list(lengthDataInput), 
            "lengthRecords" %in% names(lengthDataInput),
            "lengthCol" %in% names(lengthDataInput))
  
  lengthData <- lengthDataInput$lengthRecords
  lengthCol <- lengthDataInput$lengthCol

  binwidth <- Linc
  stat_formula <- stat_type
  if(stat_type == "density"){
    stat_formula <- "count/sum(count)/width"
  } else if(stat_type == "proportion"){
    stat_formula <- "count/sum(count)"
  }
  ggplot(lengthData %>% 
           dplyr::filter(!is.na(!!sym(lengthCol))) %>%
           dplyr::mutate(isVulnerable = factor(isVulnerable, levels = c(TRUE, FALSE)))) + 
    geom_histogram(mapping = aes(x = !!sym(lengthCol), fill = isVulnerable, after_stat(!!parse_expr(stat_formula))), 
                   breaks = lengthBins, closed = "left", colour = "black", binwidth = binwidth) +
    facet_wrap(as.formula(paste0(grep("year", colnames(lengthData), ignore.case = TRUE, value = TRUE)," ~ ."))) +
    scale_fill_manual(name = "fishery \n vulnerable", breaks = waiver(), values = c("grey80", "grey20")) +
    labs(y = stat_type) +
    theme_bw() + 
    theme(legend.position = "bottom")
}

# maximum counts per year
max_counts_per_year <- function(lengthData, lengthCol, lengthBins, analyseLengthComposition, statHist = NULL){
  stopifnot("isVulnerable" %in% colnames(lengthData), "year" %in% colnames(lengthData), is.character(lengthCol))
  # lengthDataInput() reactive ensures lengthData features a year column (value could be "all periods")

  # filter vulnerable fish, categorise (cut) fish by lengthBins, calculate maximum fish per length bin by year
  lengthDataVul <- lengthData %>% dplyr::filter(isVulnerable) 
  lengthDataVul$lengthBin <- cut(lengthDataVul[, lengthCol], breaks = lengthBins, right = FALSE)
  maxCountPerYear <- apply(table(lengthDataVul$year, lengthDataVul$lengthBin), 1, max)
  sumCountPerYear <- apply(table(lengthDataVul$year, lengthDataVul$lengthBin), 1, sum)
  catchLenHist <- data.frame(year = names(maxCountPerYear), 
                             maxCount = maxCountPerYear, 
                             maxProportion = maxCountPerYear/sumCountPerYear,
                             row.names = NULL)
  if(statHist == "density" & !is.null(statHist)){
    catchLenHist <- cbind(catchLenHist, 
                          list(maxDensity = maxCountPerYear/sumCountPerYear/mean(diff(lengthBins)))) # assumes constant lengthBin widths
  }
  if(analyseLengthComposition == "annual"){
    catchLenHist$year <- as.integer(catchLenHist$year)
  }
  catchLenHist
}

# LIME model ####

# collate output
collate_lime_output <- function(inputLIME, report, sdreport){
  
  years <- as.integer(colnames(inputLIME$neff_ft))
  years_i <- 1:(inputLIME$Nyears) 
  years_o <- years_i[which(inputLIME$neff_ft > 0)]
  
  
  # fishing mortality
  Ft_mean <- exp(sdreport$value[grep("lF_t", names(sdreport$value))])
  limeF <- data.frame(year = years,
                      quantity = "fishing mortality",
                      Ft_mean = exp(sdreport$value[grep("lF_t", names(sdreport$value))]),
                      Ft_lci = Ft_mean*exp(-1.96*sdreport$sd[grep("lF_t", names(sdreport$value))]),
                      Ft_uci = Ft_mean*exp(1.96*sdreport$sd[grep("lF_t", names(sdreport$value))])
  )
  
  # SPR
  SPRt_mean <- sdreport$value[grep("SPR_t", names(sdreport$value))]
  limeSPR <- data.frame(year = years,
                        quantity = "SPR",
                        SPRt_mean = sdreport$value[grep("SPR_t", names(sdreport$value))],
                        SPRt_lci = pmax(SPRt_mean-1.96*sdreport$sd[grep("SPR_t", names(sdreport$value))],0),
                        SPRt_uci = pmin(SPRt_mean+1.96*sdreport$sd[grep("SPR_t", names(sdreport$value))],1)
  )
  
  # spawning stock biomass
  rSBt_mean <- exp(sdreport$value[grep("lF_t", names(sdreport$value))])/report$SB0
  limeSB <- data.frame(year = years,
                       quantity = "SSB (relative)",
                       rSBt_mean,
                       rSBt_lci = rSBt_mean*exp(-1.96*sdreport$sd[grep("lSB_t", names(sdreport$value))]),
                       rSBt_uci = rSBt_mean*exp(+1.96*sdreport$sd[grep("lSB_t", names(sdreport$value))])
  )
  
  # recruitment
  limeRecruit <- data.frame(year = years,
                            quantity = "recruitment",
                            R_t_mean = report$R_t,
                            R_t_lCI = exp(log(report$R_t) - 1.96*report$sigma_R),
                            R_t_uCI = exp(log(report$R_t) + 1.96*report$sigma_R ))
  
  # mean length
  limeML <- data.frame(year = years,
                       quantity = "mean length (catch)",
                       ML_mean = sdreport$value[grep("ML_ft_hat",names(sdreport$value))],
                       ML_lCI =sdreport$value[grep("ML_ft_hat",names(sdreport$value))] - 1.96*sdreport$sd[grep("ML_ft_hat",names(sdreport$value))],
                       ML_uCI = sdreport$value[grep("ML_ft_hat",names(sdreport$value))] + 1.96*sdreport$sd[grep("ML_ft_hat",names(sdreport$value))])
  
  # selectivity
  lmin <- min(inputLIME$lows)
  lmax <- max(inputLIME$highs)
  fishlength <- seq(lmin,lmax, inputLIME$binwidth/4)
  limeSelexF <- data.frame(length = fishlength,
                           selectivity = 1/(1+exp(-log(19)*(fishlength - report$S50_f)/(report$S95_f-report$S50_f)))
  )
  
  # collate outputs
  names_replace<- function(limeOut) {
    prefix <- strsplit(names(limeOut)[grepl("(.*)_mean$", names(limeOut))], split = "_mean")[[1]]
    names(limeOut)[grepl(prefix, names(limeOut))] <- c("mean", "lci", "uci")
    limeOut
  }
  limeTH <- list("limeF" = limeF, "limeSPR" = limeSPR, "limeSB" = limeSB, 
                  "limeRecruit" = limeRecruit, "limeML" = limeML)
  limeTH <- lapply(limeTH, FUN = names_replace)
  
  limeI_t <- do.call("rbind", limeTH)
  limeI_t$quantity <- factor(limeI_t$quantity, 
                             levels = c("fishing mortality", "SSB (relative)", "mean length (catch)",
                                        "SPR", "recruitment"))
  limeO_t <- lapply(limeTH, FUN = "[", years_o, 1:5)
  limeO_t <- do.call("rbind", limeO_t)
  limeO_t$quantity <- factor(limeO_t$quantity, 
                             levels = c("fishing mortality", "SSB (relative)", "mean length (catch)",
                                        "SPR", "recruitment"))
  
  return(list("limeTH" = limeTH,
              "limeSelexF" = limeSelexF,
              "limeI_t" = limeI_t,
              "limeO_t" = limeO_t,
              "years_o" = years_o)
  )
}

# ggplot time histories
gg_lime_time <- function(limeTH, i_yrs_obs){
  pg <- ggplot(limeTH) + 
    geom_ribbon(aes(x = year, ymin = lci, ymax = uci), fill = "green4", alpha = 0.2 ) +
    geom_line(aes(x = year, y = mean), colour = "green4", linewidth = 1) + 
    geom_point(data = limeTH[i_yrs_obs,], 
               mapping = aes(x= year ,y = mean), colour = "green4", size = 2) + 
    scale_x_continuous(breaks = limeTH$year) + 
    labs(y = unique(limeTH$quantity)) + 
    expand_limits(y = 0)+
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
  if(unique(limeTH$quantity) == "SPR"){
    pg <- pg + expand_limits(y = c(0,1))
  } else if (unique(limeTH$quantity) == "fishing mortality") {
    pg <- pg + coord_cartesian(ylim = c(0, min(3.0, max(limeTH$uci, na.rm = TRUE))))
  }
  return(pg)
}

# ggplotly config
ggplotly_config <- function(pg, fname){
  if(!is.null(pg) & !is.null(fname)){
    stopifnot(ggplot2::is.ggplot(pg), is.character(fname))
    plotly::ggplotly(pg) %>% 
      plotly::config(toImageButtonOptions = 
                       list(format= 'jpeg', filename= fname, scale= 5)
      )
  } else {
    plotly::ggplotly(ggplot() + geom_blank() + theme_bw()) %>% 
      plotly::config(toImageButtonOptions = 
                       list(format= 'jpeg', filename= fname, scale= 5)
      )
  }
}


# lbspr model ####
ggplot_lbspr <- function(DM, DSPR, DSLX, specifySelectivity, length_units, add_facet = FALSE){
  # mortality
  pgM <- ggplot(DM[DM$mortality == "F",]) + 
    geom_point(aes(x= year, y = mean), colour = "black", size = 2.5) + 
    geom_errorbar(aes(x = year, ymin = lowerci, ymax = upperci), colour = "black", width = 0.25) +
    geom_hline(yintercept = unique(DM$mean[DM$mortality == "M"]), colour = "red", linetype = 2, linewidth = 1) + #linetype = 5
    labs(x = "", y = "fishing mortality F (1/yr)") + 
    lims(y = c(0, 1.25*max(DM$upperci, DM$mean, na.rm = TRUE))) +
    theme_bw()
  
  # SPR
  pgSPR <- ggplot(DSPR) + 
    geom_point(aes(x = year, y = mean), colour = "black", size = 2.5) + #geom_col(aes(x = year, y = mean), fill = "#228B2240", colour = "black", width = 0.6) +
    geom_errorbar(aes(x = year, ymin = lowerci, ymax = upperci ), width = 0.25) +
    geom_hline(yintercept = 0.4, colour = "red", linetype = 2, linewidth = 1) +
    geom_hline(yintercept = 0.3, colour = "red", linetype = 1, linewidth = 1) +
    labs(y = "SPR", x = "") +
    ylim(c(0,1)) +
    theme_bw()
  
  # add_facet?
  if(add_facet){
    pgM <- pgM + facet_wrap(vars(mortality))
    pgSPR <- pgSPR + facet_wrap(vars(quantity))
  }
  
  # selectivity
  pgSelex <- ggplot(DSLX) + 
    geom_point(aes(x = lengthMid, y = meanSL, colour = as.factor(year)), size = 2.5) + 
    geom_line(aes(x = lengthMid, y = meanSL, colour = as.factor(year)), linewidth = 1.25) +
    labs(x = paste0("length (", length_units, ")"), y = "gear selectivity") +
    scale_colour_grey(name = "year") + 
    theme_bw()
  if(specifySelectivity == "Initial estimate" & !is.null(specifySelectivity)){
    pgSelex <- pgSelex + 
      geom_ribbon(aes(x = lengthMid, ymin = lowerciSL, ymax = upperciSL, fill = as.factor(year)),
                  alpha = 0.25) +
      scale_fill_grey(guide = NULL)
  }
  
  return(list("plotF" = pgM, "plotSPR" = pgSPR, "plotSelexF" = pgSelex))
}
