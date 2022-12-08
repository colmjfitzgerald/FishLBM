# global variables used in server function
utils::globalVariables(c("Estimate", "Initial", "Lower", "LowerCI", "Model", "Param", "Parameter",
                         "Type", "Upper", "UpperCI", "Value", "Year", "age", "catchFished_at_length_count",
                         "final_gradient", "fleet", "isVulnerable", "length_cm", "length_mid", 
                         "length_p_025", "length_p_500", "length_p_975", "mortality", "proportion", 
                         "weight_p_500", "weight",
                         "quantity", "scaled_proportion", "selectivityF_at_length_count", "sex", 
                         "size", "starting_value", "varFishingAtLength", "varSPR", "year"))


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