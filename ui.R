# ui code - shinydashboard

sidebar <- sidebarPanel(
  width = 3,
  fileInput(inputId = "uploadFile", 
            label = "Upload Data Files",
            multiple = TRUE, accept = c(".csv", ".xls", ".xlsx", ".XLS")
            #, icon = icon("upload"),
  ),
  
  # length column select - paired with updateVarSelectInput in server.R
  varSelectInput(
    inputId = "lengthColSelect",
    label = "Select length column",
    data = NULL ,
    selected = NULL
  ),
  
  #  withTags({
  #    div(class="form-group shiny-input-container",
  #        label(class="control-label", id="test_label", `for`="test"),
  #        select(id = "test", option(value = "test", selected = "test")),
  #        script(type="application/json", 
  #               `data-for`="test", 
  #               `data-nonempty`="">{"plugins":["selectize-plugin-a11y"]}))
  #  })
  #uiOutput(outputId = "checkboxData", width = "100%"),
  checkboxGroupInput(inputId = "checkboxCatchData", 
                     label = "Select attributes",
                     choices = NULL, selected = NULL),
  
  hr(),
  div(id = "columnSelect"),#, h5("Apply data filters")),
  # # species, sex, gear, age, maturity, year
  # fluidRow(
  #   column(width = 6, 
  #          uiOutput(outputId = "speciesSelect", width = "100%")),
  #   column(width = 6,
  #          uiOutput(outputId = "sexSelect", width = "100%")),
  #   column(width = 6, 
  #          uiOutput(outputId = "gearSelect", width = "100%")),
  #   column(width = 6,
  #          uiOutput(outputId = "yearSelect", width = "100%"))
  # ),
  uiOutput(outputId = "btnSelectCols", width = "100%"),
)


body <- mainPanel(
  width = 9,
  #fluidPage(
  tabsetPanel(
    id = "tabMain",
    type = "tabs",
    tabPanel("Data Overview", 
             fluidPage(
               column(width = 3,
                      h5("Raw data"),
                      verbatimTextOutput(outputId = "strRawCatchData")#,
                      #                      verbatimTextOutput(outputId = "headRawCatchData")
               ),
               column(width = 9,
                      h5("Length composition by attribute"),
                      plotlyOutput(outputId = "lengthComposition",
                                   width = "100%",
                                   height = "800px")
               )
             ),
             icon = icon("chart-bar")
    ),
    tabPanel("Filter data", 
             fluidPage(
               fluidRow(
                      h3("Catch data by attribute"),
                      DTOutput(outputId = "catchDataTable", height = "auto")
               ),
               fluidRow(
                 div(id = "unitConversionOption", hr()),
                 column(width = 4,
                        h3("Length unit conversion"),
                        radioButtons(inputId = "unitConvertRadioBtn",
                                     label = "",
                                     choices = c("Keep units unchanged",
                                                 "Convert units")
                                     )
                 ),
                 column(width = 8,
                        conditionalPanel(condition = "input.unitConvertRadioBtn == 'Convert units'",
                          #h4("Convert length units, input data"),
                          selectInput(inputId = "dataLengthUnits", label = "Current length units", 
                                      choices = c("mm", "cm", "m", "in"), selected = "cm"),
                          selectInput(inputId = "newLengthUnits", label = "New length units", 
                                      choices = c("mm", "cm", "m", "in"), selected = "cm")
                          #textInput(inputId = "inputLengthColName", label = "Length column name")
                        )
                 ),
               ),
               fluidRow(
                 actionButton(inputId = "convertLengthUnits", label = "Input filtered data",
                              class = "btn-success")
               )
             ),
             icon = icon("table")
    ),
    tabPanel("Life history estimation", value = "tabLHP",
             navbarPage(title = "Life history",
                        id = "lhEstimate",
                        tabPanel("Life history parameters",
                                 withMathJax(),
                                 tags$div(HTML(
                                   "<script type='text/x-mathjax-config'>
                                 MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$']]}});
                                  </script>")
                                 ),
                                 fluidPage(
                                   fluidRow(
                                     column(width = 6,
                                     box(width = 12, 
                                         title = h4("Growth"), status = "primary",
                                         uiOutput(outputId = "moveToGrowthPage"),
                                         hr(),
                                         radioButtons(inputId = "growthParOption",
                                                      label = "LVB growth parameters",
                                                      choices = "User-specified"),
                                     ),
                                     box(width = 12,
                                         title = h4("Natural mortality"), status = "primary",
                                         uiOutput(outputId= "natMortalityRadioBtn"),
                                     ),
                                     box(width = 12, 
                                         title = h4("Maturity"), status = "primary",
                                         #h3("Maturity"),
                                         uiOutput(outputId= "btnRadioMaturity")
                                     )
                                     ),
                                     column(width = 6,
                                     box(title = h4("Summary"),
                                         status = "primary", width = 12,
                                         tags$table(id = "tableLHP",
                                                    #tags$thead(h3("Summary table")),
                                                    tags$tr(tags$th(class = "parTable", "Parameter"), 
                                                            tags$th(class = "parTable", "Description"), 
                                                            tags$th(class = "parTable", "Value")),
                                                    tags$tr(tags$td(strong("Linf (LVB)"), id = "Linf_label"), 
                                                            tags$td("Asymptotic length"),
                                                            tags$td(#class = "inline",
                                                              uiOutput(outputId = "numLinf"))), #numericInput(inputId = "Linf", label = "Linf", value = input$sliderLinf),
                                                    tags$tr(tags$td(strong("CV for Linf"), id = "CVLinf_label"), 
                                                            tags$td("Coefficient of variation - asymptotic length"),
                                                            tags$td(#class = "inline",
                                                              numericInput(inputId = "CVLinf", label = NULL, value = 0.1,
                                                                           min = 0.001, max = 1, step = 0.001))),
                                                    tags$tr(tags$td(strong("K (LVB)"), id = "K_label"), 
                                                            tags$td("Growth (length-at-age) constant "),
                                                            tags$td(#class = "inline", 
                                                              uiOutput(outputId = "numKlvb"))), #numericInput(inputId = "kLVB", label = "K", value = input$sliderK), 
                                                    tags$tr(tags$td(strong("M"), id = "M_label"), 
                                                            tags$td("Natural mortality rate"),
                                                            tags$td(#class = "inline", 
                                                              uiOutput(outputId = "numM"))),
                                                    tags$tr(tags$td(strong("Lm50"), id = "Lm50_label"), 
                                                            tags$td("Length at 50% maturity"),
                                                            tags$td(#class = "inline",
                                                              uiOutput(outputId = "numLm50"))), 
                                                    tags$tr(tags$td(strong("Lm95"), id = "Lm95_label"), 
                                                            tags$td("Length at 95% maturity"),
                                                            tags$td(#class = "inline",
                                                              uiOutput(outputId = "numLm95"))),
                                                    tags$tr(tags$td(strong("Walpha"), id = "Walpha_label"),
                                                            tags$td("Weight-at-length coefficient a"),
                                                            tags$td(#class = "inline",
                                                              numericInput(inputId = "Walpha", label = NULL, value =  0.00001 ))),
                                                    tags$tr(tags$td(strong("Wbeta"), id = "Wbeta_label"),
                                                            tags$td("Weight-at-length exponent b"),
                                                            tags$td(#class = "inline",
                                                              numericInput(inputId = "Wbeta", label = NULL, value = 3))),
                                                    tags$tfoot()
                                         ),
                                     ),
                                    # enter pars button
                                    actionButton(inputId = "btnStockPars",
                                                 label = "Enter Stock Pars",
                                                 class = "btn-success"),
                                    # tooltips
                                     bsTooltip(id = "M_label", 
                                               title = "In per-recruit theory M/K determines the natural rate of decrease of numbers-at-length in the absence of fishing mortality. M strongly influences estimate of F/M, SPR.", 
                                               placement = "left", trigger = "hover",  options = list(container = "body")),
                                     bsTooltip(id = "K_label",#"numKlvb", 
                                               title = "Populations with greater values of K approach asymptotic length Linf at earlier ages.", 
                                               placement = "left", trigger = "hover",  options = list(container = "body")),
                                     bsTooltip(id = "Linf_label",#"numLinf", 
                                               title = "Length-at-infinity - mean aysmptotic length that fish in a population can reach. In LB-SPR, an overestimate of Linf may cause an overestimate of F/M and similarly for an underestimate...",  
                                               placement = "left", trigger = "hover",  options = list(container = "body")),
                                     bsTooltip(id = "CVLinf_label", # "CVLinf", 
                                               title = "Coefficient of variation around Linf: larger values imply greater scatter of individual fish lengths around the expected population growth trajectory", 
                                               placement = "left", trigger = "hover",  options = list(container = "body")),
                                     bsTooltip(id = "Lm50_label", #"numLm50", 
                                               title = "Default value <b>Lm50 = 0.66 Linf</b> based on Beverton-Holt life history invariants. <br> Replace default where empirical data or expert knowledge is available. <br> Influences SPR calculation.", 
                                               placement = "left", trigger = "hover",  options = list(container = "body")),
                                     bsTooltip(id = "Lm95_label", #"numLm95", 
                                               title = "Default value <b>Lm95 = 0.75 Linf</b> (assumed to be biologically reasonable). <br> Replace default where empirical data or expert knowledge is available. <br> Influences SPR calculation.", 
                                               placement = "left", trigger = "hover",  options = list(container = "body")),
                                     bsTooltip(id = "natMortalityRadioBtn", 
                                               title = "User default: M = 0.3 1/yr. <br> Empirical natural mortality estimators are from Then et al. (2015) doi:10.1093/icesjms/fsu136", 
                                               placement = "bottom", trigger = "hover",  options = list(container = "body"))
                                   )
                                   ),
                                 )
                        ),
                        tabPanel("Growth", value = "tabLengthAtAgeFit",
                                 fluidRow(
                                   column(width = 8,
                                          plotlyOutput(outputId = "lvbGrowthCurve", width = "100%", height = "400px")
                                   ),
                                   column(width = 4, 
                                          h3("von Bertalanffy growth", id = "vbg_header"),
                                          sliderInput(inputId = "sliderLinf", 
                                                      label = "LVB Linf",
                                                      min = 0, max = 150, value = 40,
                                                      step = 1, ticks = TRUE),
                                          sliderInput(inputId = "sliderK", 
                                                      label = "LVB k",
                                                      min = 0.05, max = 1, value = 0.25,
                                                      step = 0.01, ticks = TRUE, round = FALSE),
                                          sliderInput(inputId = "slidert0", 
                                                      label = "LVB t0",
                                                      min = -1.5, max = 1.5, value = -0.01,
                                                      step = 0.05, ticks = TRUE, round = FALSE),
                                          sliderInput(inputId = "sliderAgeMax", 
                                                      label = "Max age",
                                                      min = 6, max = 16, value = 11,
                                                      step = 1, ticks = TRUE, round = FALSE),
                                          actionButton(inputId = "fitGrowth", label = "Fit LVB curve",
                                                       class = "btn-success"),
                                          bsTooltip(id = "fitGrowth", 
                                                    title = "Apply statistical fit if age data is available. Where age, length data are available then slider settings are used as initial estimates of the parameters Linf, K, t0 in nonlinear regression.", 
                                                    placement = "right", trigger = "hover",  options = list(container = "body"))
                                   )
                                 ),
                                 fluidRow(
                                   column(width = 8,
                                          div(hr(), id = "sectionGrowthFitSummary"),
                                          verbatimTextOutput(outputId = "growthFitSummary")
                                   )
                                 ),
                        ),
                        # tabPanel("Maturity",
                        #          fluidRow(
                        #            column(width = 12,
                        #                   NULL)
                        #          )
                        # ),
                        # tags$head(
                        #   tags$style(
                        #     'thead {
                        #        display: table-header-group;
                        #        vertical-align: middle;
                        #        border-color: inherit;
                        #      }
                        # 
                        #      tr:nth-child(1) {
                        #        border: solid thick;
                        #      }
                        # 
                        #      tr:nth-child(2) {
                        #        border: solid thick;
                        #      }
                        # 
                        #      th {
                        #        text-align: center;
                        #      }
                        # 
                        #      td, th {
                        #        outline: none;
                        #      }
                        # 
                        #      table { 
                        #        display: table;
                        #        border-collapse: separate;
                        #        white-space: normal;
                        #        line-height: normal;
                        #        font-family: times-new-roman;
                        #        font-weight: normal;
                        #        font-size: medium;
                        #        font-style: normal;
                        #        color: -internal-quirk-inherit;
                        #        text-align: start;
                        #        border-spacing: 2px;
                        #        border-color: grey;
                        #        font-variant: normal;
                        #        }  '
                        #   )
                        # ),
             )
    ),
    tabPanel("Gear selectivity", value = "tabSelectivity",
             fluidRow(
               column(width = 4,
                      div(id = "specifySelectivityPattern", h4("Fishery selectivity options")),
                      radioButtons(inputId = "chooseSelectivityPattern",
                                   label = "Size-selectivity pattern",
                                   choices = c("Asymptotic", "Dome-shaped"),
                                   selected = "Asymptotic"
                      ),
                      radioButtons(inputId = "specifySelectivity",
                                   label = "Fishery selectivity parameters",
                                   choices = c("Estimate (model fit)", "Specify (user)"),
                                   selected = c("Estimate (model fit)")),
                      uiOutput(outputId = "chooseSelectivityCurve"),
                      #tags$div(id = "meshSizes"),  # for insertUI, removeUI
                      uiOutput(outputId = "gearMeshSizes"),
               ),
               column(width = 8,
                      div(id = "specifySelectivityParameters", 
                          h4("Fishery selectivity parameters")),
                      #textOutput(outputId = "selectivityNote"),
                      uiOutput(outputId = "selectivityParameters")
                      # specify parameters dependent on input
               )
             ),
             div(id = "plotSelectivitySection", hr()),
             fluidRow(
               #                                   column(width = 3,
               #                                          uiOutput(outputId = "selectivityControls")
               #                                   ),
               column(width = 12, # 9
                      plotlyOutput(outputId = "plotSelectivityPattern"))
             )
    ),
    tabPanel("Length-based assessment", value = "tabLBA",
             #         plotlyOutput(outputId = "lengthAge",
             #                      width = "90%"),
             navbarPage(title = "Assessment steps",
                        id = "methodLBSPR",
                        tabPanel("Method parameters", value = "tabMethodParameters",
                                 fluidPage(
                                   fluidRow(
                                     column(width = 6,
                                            #div(id = "lbAssessment", hr()), # "Choose assessment" #  decide on length-based assessment
                                            selectInput(inputId = "lengthBasedAssessmentMethod", label = "Assessment method",
                                                         choices = c("LB-SPR", "LIME"))
                                            #  uiOutput(outputId = "cbLBA", width = "100%"),)
                                     ),
                                     column(width = 6,
                                            actionButton(inputId = "btnTechnicalStockPars", label = "Input parameters",
                                                         class = "btn-success"))
                                   ), 
                                   fluidRow(
                                     column(width = 6,
                                            uiOutput(outputId = "boxTechParsLBSPR")
                                     ),
                                     column(width = 6,
                                            uiOutput(outputId = "boxTechParsLIME")
                                     ),
                                   ) 
                                 )
                        ),
                        tabPanel("Length composition", value = "tabLengthComposition",
                                 fluidPage(
                                   fluidRow(
                                     column(width = 3,
                                            sliderTextInput(inputId = "Linc",
                                                            label = "Length bin width",
                                                            selected = 1,
                                                            choices = c(0.25, 0.5, 1, 2, 4, 5),
                                                            grid = TRUE),
                                            # sliderInput(inputId = "Linc", 
                                            #             label = "Length bin width",
                                            #             min = 0.5, max = 5, value = 1, step = 0.5, 
                                            #             ticks = TRUE),
                                            div(id = "above MVL",hr()),
                                            numericInput(inputId = "MLL",
                                                         label = "Minimum length limit (fishery)",
                                                         value = 0.0,
                                                         min = 0.0),
                                            div(id = "aboveVisualiseRadioButtons", hr()),
                                            radioButtons(inputId = "analyseLengthComposition",
                                                         label = "Assessment - temporal basis",
                                                         choices = c("all periods"), #, "by year"),
                                                         selected = "all periods"
                                            ),
                                            #uiOutput(outputId = "btnPlotLengthComposition")
                                            div(id = 'buttonDiv', hr(), class = 'simpleDiv'),
                                            fluidRow(
                                              actionButton("fitLBA", paste0("Apply LB-SPR"), icon = icon("chart-line"),
                                                           class = "btn-success"),
                                            )
                                     ),
                                     column(width = 9,
                                            plotlyOutput(outputId = "plotResponsiveLengthComposition",
                                                         width = "100%",
                                                         height = "600px")
                                     )
                                   ),
                                 ),
                        ),
                        navbarMenu("Model fit", 
                                   tabPanel("Length composition", value = "tabModelFit",
                                            fluidPage(
                                              fluidRow(
                                                column(width = 12,
                                                       plotlyOutput(outputId = "plotLBAModelFit",
                                                                    width = "100%",
                                                                    height = "600px")
                                                )
                                              )
                                            )
                                   ),
                                   tabPanel("Fishing estimates",
                                            fluidPage(
                                              fluidRow(
                                                column(width = 12,
                                                       plotOutput(outputId = "plotFishingEstimateOutput",
                                                                  width = "100%", height = "600px")
                                                )
                                              )
                                            )
                                   ),
                                   tabPanel("Assessment summary",
                                            fluidRow(
                                              column(width = 8,
                                                     tags$h3("Model estimates"),
                                                     div(style = 'overflow-x: scroll',tableOutput(outputId = "tableLBASummary"))
                                              ),
                                              column(width = 4,
                                                     tags$h3("Model inputs"),
                                                     tableOutput(outputId = "tableStockParameters"))
                                            )
                                   ),
                                   icon = icon("chart-line")
                        ),
                        navbarMenu("Interpretation",
                                   tabPanel("Graphical",
                                            fluidPage(
                                            fluidRow(
                                              column(width = 12,
                                                     plotlyOutput(outputId = "plotCatchFishedUnfished",
                                                                  width = "100%", height = "600px"),
                                              )
                                            )#,
                                            # fluidRow(
                                            # column(width = 12,
                                            #        plotlyOutput(outputId = "plotPopFishedUnfished"),
                                            # )
                                            # )
                                            )
                                   )
                        ),
                        navbarMenu("Diagnostics",
                                   tabPanel("Graphical",
                                            fluidRow(
                                              column(width = 9,
#                                                     h3("Parameter estimates, confidence intervals, bounds"),
                                                     plotlyOutput(outputId = "diagnosticParameterFits",
                                                                  width = "100%",
                                                                  height = "500px"),
                                              ),
                                              column(width = 3,
                                                     box(
                                                       title = "Parameter estimate plot - interpretation",
                                                       width = NULL, # for column-based layouts
                                                       status = "info",
                                                       tags$body(
                                                         tags$p(
                                                           tags$ul(
                                                             tags$li("Maximum likelihood (ML) optimization parameter domains are shaded light green."),
                                                             tags$li("Initial parameter estimates for ML optimization are shown with a black circle."),
                                                             tags$li("ML estimates and 95% confidence intervals are indicated with a large black dot and error bars."),
                                                           ),
                                                           tags$p("Possible model fit issues if:"),
                                                           tags$ul(
                                                             tags$li("any estimates are at parameter domain boundary;"),
                                                             tags$li("confidence intervals around an estimate are very large;"),
                                                             tags$li("final gradient at MLE is very large.")
                                                           ),
                                                         )                                                       ,
                                                         collapsible = TRUE,
                                                         collapsed = FALSE
                                                       )
                                                     )
                                              )
                                            ),
                                            fluidRow(plotOutput(outputId = "plotPITresiduals"))
                                   ),
                                   tabPanel("Optimisation details",
                                            fluidPage(
                                              fluidRow(
                                                column(width = 6,
                                                       tags$h4("Optimisation output"),
                                                       verbatimTextOutput(outputId = "textLBAModelFit")
                                                ),
                                                column(width = 6, 
                                                       tags$h4("Estimated quantities"),
                                                       tableOutput(outputId = "tableLBAEstimates")
                                                )
                                              )
                                            ),
                                   ),
                                   icon = icon("tasks")
                        )
              ),
             icon = icon("list-ui")
    ),
  ##---------------
  ## USER FEEDBACK
  ##---------------
  ## code block from DAMARA web-app
  ## see https://archimer.ifremer.fr/doc/00390/50174/50795.pdf for details
  tabPanel(title = "App feedback",
        box(
          title = "User feedback", status = "primary",
          textInput("user", "User name", "User name"),
          textAreaInput("exampleTextarea", '', width = "100%", height = "300px"),
          actionButton("submitfeed", "Submit", icon("arrow-circle-right")),
          tags$style(type='text/css', '#outText {background-color: white; color: green; border: white}'),
          verbatimTextOutput("outText"),
          solidHeader = TRUE,
          width=8
        )
  )                 
  )
)



ui <- fluidPage(
#  tags$head(
#    tags$link(rel = "stylesheet", type="text/css", href = "table_style.css")
#  ),
  includeCSS("www/table_style.css"),
  titlePanel("Length-based catch data analysis"),
  useShinydashboard(),
  sidebarLayout(
    sidebarPanel = sidebar,
    mainPanel = body,
    fluid = TRUE
  )
)
