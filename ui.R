shinyUI(pageWithSidebar(  
  
  headerPanel(singleton(tags$head(tags$title("Zbrowse")))),
  sidebarPanel(
    includeCSS('www/style.css'),
#    wellPanel(
#      uiOutput("datasets")
#    ),
    #uiOutput("ui_Manage")
    uiOutput("ui_All")
  ),
  mainPanel(
    tagList( # The four core files: 3 JS files and 1 CSS file --
#      singleton(tags$head(tags$script(src='js/highcharts.js',type='text/javascript'))),
      singleton(tags$head(tags$script(src='/datatables/js/jquery.dataTables.js',type='text/javascript'))),
      singleton(tags$head(tags$script(src='/tabletools/js/TableTools.js',type='text/javascript'))),
      singleton(tags$head(tags$script(src='/tabletools/js/ZeroClipboard.js',type='text/javascript'))),
      singleton(tags$head(tags$link(href='/tabletools/css/TableTools.css',rel='stylesheet',type='text/css')))
      #singleton(tags$head(tags$script(src='http://code.highcharts.com/highcharts.js',type='text/javascript')))
    ),    
    #progressInit(),    
    uiOutput("ui_data_tabs")
    #tableOutput('contents')
  )
))
