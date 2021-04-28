if(!require(shiny)){install.packages("shiny", dependencies = TRUE); require(shiny)}
if(!require(openxlsx)){install.packages("openxlsx", dependencies = TRUE); require(openxlsx)}
if(!require(ggplot2)){install.packages("ggplot2", dependencies = TRUE); require(ggplot2)}
if(!require(plotly)){install.packages("plotly", dependencies = TRUE); require(plotly)}
if(!require(dplyr)){install.packages("dplyr", dependencies = TRUE); require(dplyr)}
if(!require(tidyr)){install.packages("tidyr", dependencies = TRUE); require(tidyr)}
if(!require(DT)){install.packages("DT", dependencies = TRUE); require(DT)}
if(!require(devtools)){install.packages("devtools", dependencies = TRUE); require(devtools)}

#library(htmltools)
options(shiny.maxRequestSize = 5*1024^2)
#setwd("C:/Users/cfitzgerald/workspace/length_based_assessment/")
source("./GTG_LBSPR_CF.R")
#source_url("https://github.com/AdrianHordyk/GTG_LBSPR/blob/master/GTG_LBSPR.r")
source("./ui.R")
source("./server.R")


shinyApp(ui, server)
