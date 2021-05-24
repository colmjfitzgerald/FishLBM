# ui code

# construct the layout and structure in user interface function "ui"
# Q. does this have to be of fluidPage type? 
# A. no, with shinydashboard we can have dashboardPage()


#menuItem("Survey data", 
#         icon = icon("upload"),
#         tabName = "upload"),
#menuItem("Species overview", tabName = "overview"),
#menuItem("Growth analysis", tabName = "growthAnalysis"),
#menuItem("Maturity analysis", tabName = "maturityAnalysis"),
#menuItem("Survey background", 
#         icon = icon("info"), # fontawesome info circle icon
#         href = "http://wfdfish.ie/" # no tabName because it's a link to a separate page
#),
#id = "sbMenu")

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
  
#  div(id = "lbAssessment", hr("Choose assessment")), # decide on length-based assessment
#  uiOutput(outputId = "cbLBA", width = "100%"),

)


body <-   mainPanel(
  width = 9,
  #fluidPage(
  tabsetPanel(
    id = "tabDataPlotAnalysis",
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
                 div(id = "unitConversion", hr()),
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
                          h4("Convert length units, input data"),
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
    tabPanel("Growth",
             fluidRow(
               column(width = 8,
                      plotlyOutput(outputId = "lvbGrowthCurve"),
                      actionButton(inputId = "fitGrowth", label = "Fit LVB curve"),
                      bsTooltip(id = "fitGrowth", 
                                title = "Statistical fit if age data is available. Where age, length data is available then slider settings are used as initial estimates of the parameters Linf, K, t0 in nonlinear regression.", 
                                placement = "right", trigger = "hover",  options = list(container = "body"))
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
                                  min = -1.5, max = 1.5, value = 0.0,
                                  step = 0.05, ticks = TRUE, round = FALSE),
                      sliderInput(inputId = "sliderAgeMax", 
                                  label = "Max age",
                                  min = 6, max = 16, value = 11,
                                  step = 1, ticks = TRUE, round = FALSE)
               )
             ),
             fluidRow(
               column(width = 8,
                      div(hr(), id = "sectionGrowthFitSummary"),
                      verbatimTextOutput(outputId = "growthFitSummary")
               )
             ),
    ),
    tabPanel("LB-SPR", 
             #         plotlyOutput(outputId = "lengthAge",
             #                      width = "90%"),
             navbarPage(title = "GTG LB-SPR",
                        id = "parLBSPR",
                        tabPanel("Stock biological parameters",
                                 fluidPage(
                                   fluidRow(
                                     column(width = 8,
                                            #                                          box(status = "primary", width = NULL,
                                            #                                          ),
                                            tags$table(id = "tableLBSPR",
                                                       tags$thead(h3("LB-SPR stock parameters")),
                                                       tags$tr(tags$th(class = "parTable", "Parameter"), tags$th(class = "parTable", "Value")),
                                                       tags$tr(tags$td(strong("M"), id = "M_label"), 
                                                               tags$td(class = "inline", 
                                                                       numericInput(inputId = "M", label = NULL, value = 0.3))),
                                                       tags$tr(tags$td(strong("K (LVB)"), id = "K_label"), 
                                                               tags$td(class = "inline", 
                                                                       uiOutput(outputId = "numKlvb"))), #numericInput(inputId = "kLVB", label = "K", value = input$sliderK), 
                                                       tags$tr(tags$td(strong("Linf (LVB)"), id = "Linf_label"), 
                                                               tags$td(class = "inline",
                                                                       uiOutput(outputId = "numLinf"))), #numericInput(inputId = "Linf", label = "Linf", value = input$sliderLinf),
                                                       tags$tr(tags$td(strong("CV for Linf"), id = "CVLinf_label"), 
                                                               tags$td(class = "inline",
                                                                       numericInput(inputId = "CVLinf", label = NULL, value = 0.1,
                                                                                    min = 0.001, max = 1, step = 0.001))),
                                                       tags$tr(tags$td(strong("Lm50"), id = "Lm50_label"), 
                                                               tags$td(class = "inline",
                                                                       uiOutput(outputId = "numLm50"))), 
                                                       tags$tr(tags$td(strong("Lm95"), id = "Lm95_label"), 
                                                               tags$td(class = "inline",
                                                                       uiOutput(outputId = "numLm95"))),
                                                       tags$tfoot()
                                            ),
                                     ),
                                     # tooltips
                                     bsTooltip(id = "M_label", 
                                               title = "Natural mortality rate: in per-recruit theory M/K determines the natural rate of decrease of numbers-at-length in the absence of fishing mortality", 
                                               placement = "left", trigger = "hover",  options = list(container = "body")),
                                     bsTooltip(id = "K_label",#"numKlvb", 
                                               title = "Growth constant (Brody growth parameter): populations with greater values of K approach asymptotic length Linf at earlier ages.", 
                                               placement = "left", trigger = "hover",  options = list(container = "body")),
                                     bsTooltip(id = "Linf_label",#"numLinf", 
                                               title = "Length-at-infinity - mean aysmptotic length that fish in a population can reach.",  
                                               placement = "left", trigger = "hover",  options = list(container = "body")),
                                     bsTooltip(id = "CVLinf_label", # "CVLinf", 
                                               title = "Coefficient of variation around Linf: larger values imply greater scatter of individual fish lengths around the expected population growth trajectory", 
                                               placement = "left", trigger = "hover",  options = list(container = "body")),
                                     bsTooltip(id = "Lm50_label", #"numLm50", 
                                               title = "Length at 50% maturity<br> <em>Length at which 50% of a population have reached reproductive maturity.</em> Replace default <b>Lm50 = 0.66 Linf</b> value (based on Beverton-Holt life history invariants) where empirical data or expert knowledge is available. Influences SPR calculation.", 
                                               placement = "left", trigger = "hover",  options = list(container = "body")),
                                     bsTooltip(id = "Lm95_label", #"numLm95", 
                                               title = "Length at 95% maturity<br> <em>Length at which 95% of a population have reached reproductive maturity.</em> Replace default <b>Lm95 = 0.75 Linf</b> value (assumed to be biologically reasonable) where empirical data or expert knowledge is available. Influences SPR calculation.", 
                                               placement = "left", trigger = "hover",  options = list(container = "body")),
                                     column(width = 4,
                                            h3("Growth parameters"),
                                            uiOutput(outputId = "growthParRadioBtn")
                                     )
                                   ), 
                                   fluidRow(
                                      column(width = 8,
                                          box(status = "info", width = NULL,
                                              collapsible = TRUE, collapsed = TRUE,
                                              title = "Other parameters",
                                              tags$table(
                                              tags$tr(tags$td("Walpha"), 
                                                      tags$td(numericInput(inputId = "Walpha", label = NULL, value =  0.00001 ))),
                                              tags$tr(tags$td("Wbeta"), 
                                                      tags$td(numericInput(inputId = "Wbeta", label = NULL, value = 3))),
                                              tags$tr(tags$td("FecB"), 
                                                      tags$td(numericInput(inputId = "FecB", label = NULL, value = 3))
                                              ),
                                              tags$tr(tags$td("Steepness"),
                                                      tags$td(numericInput(inputId = "Steepness", label = NULL, value = 0.8))
                                              ),
                                              tags$tr(tags$td("Mpow"),
                                                      tags$td(numericInput(inputId = "Mpow", label = NULL, value = 0.8))
                                              ),
                                              tags$tr(tags$td("NGTG"),
                                                      tags$td(numericInput(inputId = "NGTG", label = NULL, value = 17))
                                              ),
                                              tags$tr(tags$td("GTG Max SD about Linf"),
                                                      tags$td(numericInput(inputId = "MaxSD", label = NULL, 
                                                                           value = 2, min = 0, max = 4))),
                                              tags$tfoot()
                                              )
                                            ),
                                      ), 
                                      column(width = 4,
                                            # enter pars button
                                            actionButton(inputId = "btnStockPars",
                                                         label = "Enter Stock Pars",
                                                         class = "btn-success")
                                     ),
                                   )
                                )
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
                        ),
                        tabPanel("Selectivity",
                                 fluidRow(
                                   column(width = 4,
                                          div(id = "specifySelectivityPattern", h4("Fishery selectivity options")),
                                          radioButtons(inputId = "chooseSelectivityPattern",
                                                       label = "Fishery selectivity curve",
                                                       choices = c("Asymptotic")#, "Dome-shaped")
                                                       ),
                                          radioButtons(inputId = "specifySelectivity",
                                                       label = "Fishery selectivity parameters",
                                                       choices = c("Estimate",
                                                                   "Specify"))#,
                                          # actionButton(inputId = "btnSelectivity",
                                          #              label = "Input selectivity choices",
                                          #              class = "btn-success")
                                          ),
                                 column(width = 8,
                                        div(id = "specifySelectivity", h4("Fishery selectivity parameters")),
                                        #textOutput(outputId = "selectivityNote"),
                                        uiOutput(outputId = "selectivityParameters")
                                        # specify parameters dependent on input
                                        )
                                 ),
                                 fluidRow(width = 6,
                                          div(id = "plotSelectivitySection", hr()),
                                          plotlyOutput(outputId = "plotSelectivityPattern")
                                          ) 
                        ),
                        tabPanel("Length composition",
                                 fluidPage(
                                   fluidRow(
                                     column(width = 3,
                                          sliderTextInput(inputId = "Linc", 
                                                      label = "Length bin width", #paste0("Length increment"),
                                                      selected = 1,
                                                      choices = c(0.25, 0.5, 1, 2, 4, 5),
                                                      grid = TRUE),
                                                      #min = 0.25, max = 5, value = 1, step = 0.25, 
                                                      #ticks = TRUE),
                                          div(id = "above MVL",hr()),
                                          numericInput(inputId = "MLL",
                                                       label = "Minimum length limit (fishery)",
                                                       value = 0.0,
                                                       min = 0.0),
                                          div(id = "aboveVisualiseRadioButtons", hr()),
                                          radioButtons(inputId = "visualiseLengthComposition",
                                                       label = "Visualise...",
                                                       choices = c("in aggregate", "by year"),
                                                       selected = "in aggregate"
                                                       )
                                     ),
                                     column(width = 9,
                                            plotlyOutput(outputId = "plotResponsiveLengthComposition",
                                                         width = "100%",
                                                         height = "400px")
                                            )
                                     ),
                                 )
                        ),
                        tabPanel("Model fit",
                                 actionButton("fitLBSPR", "Apply GTG-LBSPR", icon = icon("chart-line"),
                                              class = "btn-success"),
                                 fluidPage(
                                   column(width = 4,
                                          div(id = 'resultsDiv', hr(), class = 'simpleDiv'),
                                          verbatimTextOutput(outputId = "textLBSPREstFit")
                                          #verbatimTextOutput(outputId = "textLBSPROpOut")
                                          #DTOutput(outputId = "gtgLBSPREstModel"),
                                          #DTOutput(outputId = "gtgLBSPROpModel"),
                                   ),
                                   column(width = 8,
                                          plotlyOutput(outputId = "visFitLBSPR",
                                                       width = "100%",
                                                       height = "400px")
                                   )
                                 ),
                                 icon = icon("chart-line")
                        ),
                        tabPanel("Diagnostics",
                                 fluidRow(
                                   column(width = 9,
                                          #tags$h3("Population length composition"),
                                          #plotOutput(outputId = "plotPopLBSPR"),
                                          #tags$hr(),
                                          tags$h3("Expected catch-at-length - per recruit theory"),
                                          plotlyOutput(outputId = "plotCatchLBSPR")
                                   ),
                                   column(width = 3, 
                                          verbatimTextOutput(outputId = "textFitLBSPR")),
                                   #plotlyOutput(outputId = "plotOpLBSPR",
                                   #              width = "100%",
                                   #             height = "400px")
                                 )
                        )),
             icon = icon("list-ui")
    ),
    tabPanel("LBB")#, 
    #inline = FALSE))
    #tabPanel("Stock assessment\n - diagnostics", 
    #         fluidRow(width = 12,
    #                  collapsible = TRUE, collapsed = TRUE,
    #                  h4("Bivariate scatterplots of estimated regression parameters"), # from residual bootstrap sampling
    #                  plotOutput(outputId = "pairPlotLVBParameters")),
    #         fluidRow(width = 12, 
    #                  collapsible = TRUE, collapsed = TRUE,
    #                  h4("Histograms - marginal parameter distributions"),
    #                  plotlyOutput(outputId = "histLVBParameters")),
    #         icon = icon("tasks")
    #)
  )
  #  )
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
