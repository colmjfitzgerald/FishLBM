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
               column(width = 4,
                      h5("Raw data"),
                      verbatimTextOutput(outputId = "strRawCatchData"),
                      verbatimTextOutput(outputId = "headRawCatchData")
               ),
               column(width = 8,
                      h5("Length composition by attribute"),
                      plotlyOutput(outputId = "lengthComposition",
                                   width = "100%",
                                   height = "800px")
               )
             ),
             icon = icon("chart-bar")
    ),
    tabPanel("Configure", 
             fluidPage(
               column(width = 8,
                      DTOutput(outputId = "catchDataTable", height = "auto")
               ),
               column(width = 4,
                      h4("Convert length units"),
                      selectInput(inputId = "dataLengthUnits", label = "Current length units", 
                                  choices = c("mm", "cm", "m", "in"), selected = "cm"),
                      selectInput(inputId = "newLengthUnits", label = "New length units", 
                                  choices = c("mm", "cm", "m", "in"), selected = "cm"),
                      actionButton(inputId = "convertLengthUnits", label = "Convert units",
                                   class = "btn-success"),
                      #textInput(inputId = "inputLengthColName", label = "Length column name")
               ),
             ),
             icon = icon("table")
    ),
    tabPanel("Filter (deprecated)", 
             fluidRow(
               h4("Filter length records"),
               uiOutput(outputId = "checkboxFilterData"),
               uiOutput(outputId = "filterBtn")
             )
             #icon = icon("table")
    ),
    tabPanel("LB-SPR", 
             #         plotlyOutput(outputId = "lengthAge",
             #                      width = "90%"),
             navbarPage(title = "GTG LB-SPR",
                        id = "parLBSPR",
                        tabPanel("Stock biological parameters",
                                 # fluidRow(
                                 #   column(width = 6, 
                                 #          h3("Parameter specification"),
                                 #          uiOutput(outputId = "growthParRBtn")
                                 #          #  actionButton(inputId = "fitGrowth", label = "Fit LVB growth curve")
                                 #   )
                                 # ),
                                 fluidPage(
                                   fluidRow(
                                     column(width = 5, 
                                            h3("von Bertalanffy Growth"),
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
                                                        step = 1, ticks = TRUE, round = FALSE),
                                            plotlyOutput(outputId = "lvbGrowthCurve")
                                            # actionButton
                                     ),
                                     column(width = 7,
                                            tags$table(
                                              tags$thead(h3("LB-SPR stock parameters")),
                                              tags$tr(tags$th("Parameter"), tags$th("Value")),
                                              tags$tr(tags$td("M"), 
                                                      tags$td(numericInput(inputId = "M", label = NULL, value = 0.3))),
                                              tags$tr(tags$td("K (LVB)"), 
                                                      tags$td(uiOutput(outputId = "numKlvb"))), #numericInput(inputId = "kLVB", label = "K", value = input$sliderK), 
                                              tags$tr(tags$td("Linf"), 
                                                      tags$td(uiOutput(outputId = "numLinf"))), #numericInput(inputId = "Linf", label = "Linf", value = input$sliderLinf),
                                              tags$tr(tags$td("CV for Linf"), 
                                                      tags$td(numericInput(inputId = "CVLinf", label = NULL, value = 0.1,
                                                                           min = 0.001, max = 1, step = 0.001))),
                                              tags$tr(tags$td("Lm50"), 
                                                      tags$td(uiOutput(outputId = "numLm50"))), 
                                              tags$tr(tags$td("Lm95"), 
                                                      tags$td(uiOutput(outputId = "numLm95"))),
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
                                            ),
                                            # enter pars button
                                            actionButton(inputId = "btnStockPars",
                                                         label = "Enter Stock Pars",
                                                         class = "btn-success")
                                     ),
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
                                 )
                        ),
                        tabPanel("Selectivity",
                                 fluidRow(
                                   column(width = 4,
                                          radioButtons(inputId = "chooseSelectivityPattern",
                                                       label = "Selectivity curve",
                                                       choices = c("Knife-edge",
                                                                   "Asymptotic",
                                                                   "Dome-shaped")),
                                          radioButtons(inputId = "specifySelectivity",
                                                       label = "Selectivity parameters",
                                                       choices = c("Estimate",
                                                                   "Specify")),
                                          actionButton(inputId = "btnSelectivity",
                                                       label = "Input selectivity choices",
                                                       class = "btn-success")
                                          ),
                                 column(width = 8,
                                        div(id = "specifySelectivity", h4("Selectivity parameters")),
                                        textOutput(outputId = "selectivityNote"),
                                        uiOutput(outputId = "specifySelectivityPars")
                                        # specify parameters dependent on input
                                        )
                                 ),
                                 fluidRow(width = 6,
                                          div(id = "plotSelectivitySection", hr()),
                                          plotlyOutput(outputId = "plotSelectivityPattern")
                                          ) 
                        ),
                        tabPanel("Length composition",
                                 fluidRow(
                                   column(width = 6,
                                          sliderInput(inputId = "Linc", 
                                                      label = paste0("Length increment"),
                                                      min = 0, max = 5, value = 1,
                                                      step = 0.5, ticks = TRUE)
                                   ),
                                   column(width = 6,
                                          actionButton(inputId = "analyseByYear",
                                                       label = "Analyse by year",
                                                       class = "btn-success"))
                                 ),
                                 fluidRow(
                                   plotlyOutput(outputId = "plotResponsiveLengthComposition",
                                                width = "100%",
                                                height = "400px")
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
                                   column(width = 6,
                                          tags$h3("Population length composition"),
                                          plotOutput(outputId = "plotPopLBSPR"),
                                          tags$hr(),
                                          tags$h3("Catch length composition"),
                                          plotOutput(outputId = "plotCatchLBSPR")
                                   ),
                                   column(width = 6, 
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
  titlePanel("Length-based catch data analysis"),
  sidebarLayout(
    sidebarPanel = sidebar,
    mainPanel = body,
    fluid = TRUE
  )
)
