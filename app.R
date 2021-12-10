if(!require(shiny)){install.packages("shiny", dependencies = TRUE); require(shiny)}
if(!require(shinyBS)){install.packages("shinyBS", dependencies = TRUE); require(shinyBS)}
if(!require(shinyWidgets)){install.packages("shinyWidgets", dependencies = TRUE); require(shinyWidgets)}
if(!require(shinydashboard)){install.packages("shinydashboard", dependencies = TRUE); require(shinydashboard)}
if(!require(openxlsx)){install.packages("openxlsx", dependencies = TRUE); require(openxlsx)}
if(!require(plotly)){install.packages("plotly", dependencies = TRUE); require(plotly)}
if(!require(tidyverse)){install.packages("tidyverse", dependencies = TRUE); require(tidyverse)}
if(!require(knitr)){install.packages("knitr", dependencies = TRUE); require(knitr)}
if(!require(kableExtra)){install.packages("kableExtra", dependencies = TRUE); require(kableExtra)}
if(!require(DT)){install.packages("DT", dependencies = TRUE); require(DT)}
if(!require(devtools)){install.packages("devtools", dependencies = TRUE); require(devtools)}
#devtools::install_github("ropensci/plotly", dependencies=TRUE)
devtools::install_github("merrillrudd/LIME", dependencies=TRUE)
require(LIME)

options(shiny.maxRequestSize = 5*1024^2)
# supporting code for Dome-shaped LB-SPR
source_gist(id = "https://gist.github.com/colmjfitzgerald/bb52a9579f870c09d3af6f4fe9ba6013",
            filename = "GTGLBSPR_Dome.R") # dome-shaped GTG-LBSPR
source_gist(id = "https://gist.github.com/colmjfitzgerald/86feef722803dcc6c82b6901c0d3c294",
            filename = "varianceFishingEstimates.R") # varFishingAtLength, varSPR functions

# user feedback code block adapted (slightly) from DAMARA web-app 
# see https://archimer.ifremer.fr/doc/00390/50174/50795.pdf for details
# courtesy of Coilin Minto at Marine Institute, Ireland

source("./ui.R")
source("./server.R")


shinyApp(ui, server)
