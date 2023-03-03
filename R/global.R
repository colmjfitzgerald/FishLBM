# global variables used in server function
utils::globalVariables(c("Estimate", "Initial", "Lower", "LowerCI", "Model", "Param", "Parameter",
                         "Type", "Upper", "UpperCI", "Value", "Year", "age", "catchFished_at_length_count",
                         "final_gradient", "fleet", "isVulnerable", "length_cm", "length_mid", 
                         "length_p_025", "length_p_500", "length_p_975", "mortality", "proportion", 
                         "weight_p_500", "weight", "density", "width",
                         "quantity", "scaled_proportion", "selectivityF_at_length_count", "sex", 
                         "size", "starting_value", "varFishingAtLength", "varSPR", "year",
                         "lci", "uci", "selectivity"))


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

# ggplot LIME output addition ####
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
                           selectivity = 1/(1+exp(-(fishlength - report$S50_f)/(report$S95_f-report$S50_f)))
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
  }
  return(pg)
}

ggplotly_config <- function(pg, fname){
  stopifnot(ggplot2::is.ggplot(pg), is.character(fname))
  plotly::ggplotly(pg) %>% 
    plotly::config(toImageButtonOptions = 
                     list(format= 'jpeg', filename= fname, scale= 5)
    )
}