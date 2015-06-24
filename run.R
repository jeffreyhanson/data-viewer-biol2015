## Run locally
test=function(){shiny::runApp(file.path(Sys.getenv('HOME'), 'github', 'data-viewer-biol2015'))}

## Deploy online to ShinyApps
shinyapps::deployApp("C:/Users/jeff/Documents/GitHub/data-viewer-biol2015")
shinyapps::deployApp("/home/jeff/Github/data-viewer-biol2015")
 
