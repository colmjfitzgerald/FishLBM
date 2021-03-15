if(!require(shiny)){
  install.packages("shiny", dependencies = TRUE)
  require(shiny)
}
if(!require(openxlsx)){
  install.packages("openxlsx", dependencies = TRUE)
  require(openxlsx)
}
if(!require(plotly)){
  install.packages("plotly", dependencies = TRUE)
  require(plotly)
}
if(!require(dplyr)){
  install.packages("dplyr", dependencies = TRUE)
  require(dplyr)
}
if(!require(tidyr)){
  install.packages("tidyr", dependencies = TRUE)
  require(tidyr)
}
if(!require(DT)){
  install.packages("DT", dependencies = TRUE)
  require(DT)
}


#library(htmltools)

setwd("C:/Users/cfitzgerald/workspace/length_based_assessment/")
source("./ui.R")
source("./server.R")


shinyApp(ui, server)
