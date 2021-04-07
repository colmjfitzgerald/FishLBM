if(!require(shiny)){
  install.packages("shiny", dependencies = TRUE)
  require(shiny)
}
if(!require(openxlsx)){
  install.packages("openxlsx", dependencies = TRUE)
  require(openxlsx)
}
if(!require(ggplot2)){
  install.packages("ggplot2", dependencies = TRUE)
  require(ggplot2)
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
source("C:/Users/cfitzgerald/Documents/R/GTG_LBSPR/GTG_LBSPR_CF.R")
source("./ui.R")
source("./server.R")


shinyApp(ui, server)
