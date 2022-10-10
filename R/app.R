library(shiny)
library(shinyBS)
library(shinyWidgets)
library(shinydashboard)
library(openxlsx)
library(plotly)
library(tidyverse)
library(withr)
library(knitr)
library(kableExtra)
library(DT)
library(devtools)
devtools::install_github("merrillrudd/LIME", dependencies=TRUE)
library(LIME)
library(LBSPRDome)

options(shiny.maxRequestSize = 5*1024^2)
# supporting code for Dome-shaped LB-SPR
source_gist(id = "https://gist.github.com/colmjfitzgerald/86feef722803dcc6c82b6901c0d3c294",
            filename = "varianceFishingEstimates.R") # varFishingAtLength, varSPR functions

# user feedback code block adapted (slightly) from DAMARA web-app 
# see https://archimer.ifremer.fr/doc/00390/50174/50795.pdf for details
# courtesy of Coilin Minto at Marine Institute, Ireland

source("./R/ui.R")
source("./R/server.R")


shinyApp(ui, server)
