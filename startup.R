packages <- c("shiny", "plyr", "tools", "xtable", "devtools", "markdown")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

if(!is.element('rCharts', installed.packages()[,1])){
  require(devtools)
  install_github('rCharts', 'ramnathv')
}

shiny::runApp(launch.browser = TRUE)
