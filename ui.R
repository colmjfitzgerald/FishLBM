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
  #  withTags({
  #    div(class="form-group shiny-input-container",
  #        label(class="control-label", id="test_label", `for`="test"),
  #        select(id = "test", option(value = "test", selected = "test")),
  #        script(type="application/json", 
  #               `data-for`="test", 
  #               `data-nonempty`="">{"plugins":["selectize-plugin-a11y"]}))
  #  })
  uiOutput(outputId = "lengthSelect", width = "100%"),
  uiOutput(outputId = "checkboxData", width = "100%"),
  
  div(id = "columnSelect", hr("Select and submit data columns")),
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
  uiOutput(outputId = "submitColsBtn", width = "100%"),
  
  div(id = "lbAssessment", hr("Choose assessment")), # decide on length-based assessment
  uiOutput(outputId = "cbLBA", width = "100%"),
  div(id = "lengthBinWidth") # decide on length bin width
)


body <-   mainPanel(
  width = 9,
  #fluidPage(
  tabsetPanel(
    id = "tabDataPlotAnalysis",
    type = "tabs",
    tabPanel("Configure data", 
             dataTableOutput(outputId = "catchDataTable", height = "auto"),
             icon = icon("table")),
    tabPanel("Visualise", 
             plotlyOutput(outputId = "lengthComposition",
                          width = "100%",
                          height = "400px"),
             icon = icon("chart-bar")
    ),
    tabPanel("LB-SPR assessment", 
             #         plotlyOutput(outputId = "lengthAge",
             #                      width = "90%"),
             navbarPage(title = "Data and parameters",
                        id = "parLBSPR",
                        tabPanel("Length composition data"),
                        tabPanel("Growth",
                                 fluidRow(
                                   column(width = 6, 
                                          h3("Parameter specification"),
                                          uiOutput(outputId = "growthParRBtn")
#                                          actionButton(inputId = "fitGrowth", label = "Fit LVB growth curve")
                                   )
                                 ),
                                 fluidRow(
                                   column(width = 6, 
                                          h3("von Bertalanffy parameters"),
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
                                          # actionButton
                                   ),
                                   column(width = 6,
                                          h3("Growth curve"),
                                          plotlyOutput(outputId = "lvbGrowthCurve"))
                                 )),
                        tabPanel("Maturity"),
                        tabPanel("Fishery"),
                        tabPanel("GTG"),
                        tabPanel("Diagnostics",
                                 verbatimTextOutput(outputId = "textLVBGrowthFit"),
                                 icon = icon("chart-line"))),
             icon = icon("list-ui")
    ),
    tabPanel("LBB assessment"), 
    #inline = FALSE))
    tabPanel("Stock assessment\n - diagnostics", 
             fluidRow(width = 12,
                      collapsible = TRUE, collapsed = TRUE,
                      h4("Bivariate scatterplots of estimated regression parameters"), # from residual bootstrap sampling
                      plotOutput(outputId = "pairPlotLVBParameters")),
             fluidRow(width = 12, 
                      collapsible = TRUE, collapsed = TRUE,
                      h4("Histograms - marginal parameter distributions"),
                      plotlyOutput(outputId = "histLVBParameters")),
             icon = icon("tasks")
    )
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
