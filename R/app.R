# user feedback code block adapted (slightly) from DAMARA web-app 
# see https://archimer.ifremer.fr/doc/00390/50174/50795.pdf for details
# courtesy of Coilin Minto at Marine Institute, Ireland

options(shiny.maxRequestSize = 5*1024^2)

myFishApp <- function(){
  shinyApp(ui, server)
}
