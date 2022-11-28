library(shiny)
source("server.R")
source("ui.R")
shinyApp(ui = ui, server = server)

# shiny::runApp("project.R")


# select Od,Liczba,Plec from gus limit 10000
#   
# select Value,Od,GEO from EUROSTAT limit 10000