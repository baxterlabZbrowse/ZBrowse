args <- commandArgs(TRUE)
library(reshape2)
library(plyr)
library(shiny)

#######Fill in path to organism here, then highlight everything and run#########
#phytoPath <- args[1]
#phytoPath <- "~/Downloads/PhytozomeV10 2/Bdistachyon/"
phytoPath <- "~/Downloads/Sitalica/v2.1/"


###Function for easily reading the gff file
gffRead <- function(gffFile, nrows = -1) {
  cat("Reading ", gffFile, ": ", sep="")
  gff = read.table(gffFile, sep="\t", as.is=TRUE, quote="",
                   header=FALSE, comment.char="#", nrows = nrows,
                   colClasses=c("character", "character", "character", "integer",  
                                "integer",
                                "character", "character", "character", "character"))
  colnames(gff) = c("seqname", "source", "feature", "start", "end",
                    "score", "strand", "frame", "attributes")
  cat("found", nrow(gff), "rows with classes:",
      paste(sapply(gff, class), collapse=", "), "\n")
  stopifnot(!any(is.na(gff$start)), !any(is.na(gff$end)))
  return(gff)
}



addOrganism <- function(phytoPath) {
  require(shiny)
  
  #collect some info about this organism
  phytoOrgs <- read.csv(file = "phytozomeOrganismsAvailable.csv",header=TRUE,stringsAsFactors = FALSE,comment.char="")  
  
  #Need to figure out how to determine versioning
  # I think it's usually what is right before annotation in the annotfileloc file name and up to the underscore
  #capture text between underscore and .annotation, otherwise this will return the unedited string
  annotFileList <- list.files(paste0(phytoPath,"/annotation/"))
  annotFileLoc <- grep("annotation_info",annotFileList,value=TRUE)
  if(length(annotFileLoc)>1){stop(paste("Ambiguous file names found for gene annotations info: ",paste(annotFileLoc,collapse = " "),"\nPlease delete one and rerun."))}
  defFileLoc <- grep("defline",annotFileList,value=TRUE)
  orgVersion <- gsub(".*_(.*?)\\.annotation.*","\\1", annotFileLoc)
  #orgName <- strsplit(phytoPath,"/")[[1]][length(strsplit(phytoPath,"/")[[1]])]
  orgName <- strsplit(annotFileLoc,"\\.|_")[[1]][1]
  
  
  if(length(grep(orgName,phytoOrgs$orgLink))==1){
    commonName <- phytoOrgs$Common.Name[grep(orgName,phytoOrgs$orgLink)]
    displayName <- paste0(commonName," (",orgName," ",orgVersion,")")
  }else{
    displayName <- paste0(orgName," ",orgVersion)
  }  
  
  #pull in information about where each Gene falls  
  geneLocFile <- grep(".gene.gff3",annotFileList,value=TRUE)
  if(length(geneLocFile)>1){stop(paste("Ambiguous file names found for gene annotations file: ",paste(geneLocFile,collapse = " "),"\nPlease delete one and rerun."))}
  geneLoc <- gffRead(gzfile(paste0(phytoPath,"/annotation/",geneLocFile)))
  geneLoc <- geneLoc[geneLoc$feature=="gene",]
  
  seqFeatures <- as.data.frame(table(geneLoc$seqname),stringsAsFactors = FALSE)
  seqFeatures <- seqFeatures[order(seqFeatures$Freq,decreasing = TRUE),]  
  colnames(seqFeatures) <- c("Phytozome Name","Number of Genes")
  
  runApp(shinyApp(  
    ui = fluidPage(
      headerPanel("Zbrowse Add Organism"),
      tags$head(
        #tags$style(type="text/css", "label.control-label, .selectize-control.single{ display: inline-block!important; }")
        tags$style(type="text/css", "label{display: inline-block!important; }")
      ),
      sidebarLayout(      
        sidebarPanel(
  #        tags$head(tags$style(type="text/css", ".dataTables_filter {display: none;    }")),
  #        tags$head(tags$style(type="text/css", "select { max-width: 100px; }")),
          helpText("Select the chromosomes you wish to display in the browser from the table below. 
                   Each row selected will display as a separate chromosome in the Zbrowse genome view."),
          dataTableOutput('tbl')
        ),
        mainPanel(          
          tabsetPanel(
            tabPanel("Add Organism",textOutput('rows_out'),htmlOutput("chromRows"),actionButton("actionButtonID","Submit Chromosomes"),tableOutput('allDone'),htmlOutput('allDoneText')),
            tabPanel("Other Phytozome Organisms",tableOutput('showAllPhyto'))
          )
        )
      )
    ), 
    server = function(input, output) {
      
      output$showAllPhyto <- renderTable({
        phytoOrgs$Source <- gsub("\"","",phytoOrgs$Source)
        phytoOrgs$`Organism Name` <- paste0("<a href='",  phytoOrgs$orgLink, "' target='_blank'>",phytoOrgs$Organism,"</a>")
        phytoOrgs[,c("Organism Name","Version","Common.Name","Source")]
      }, sanitize.text.function = function(x) x)
      
      output$tbl <- renderDataTable(        
        seqFeatures,
        options = list(pageLength = -1,bLengthChange=F,searching = FALSE),
        callback = "function(table) {
      table.on('click.dt', 'tr', function() {
        $(this).toggleClass('selected');
        Shiny.onInputChange('rows',
                            table.rows('.selected').indexes().toArray());
        });
       }"
      )
      output$rows_out <- renderText({
        "Below is a list of the chromsomes you have selected. Type the name of the chromosome in the text box (or leave as is). 
         These names have to match the label in the chromosome column of your GWAS file. Click submit and the appropriate annotation
         files will be created and added to Zbrowse. Chromsomes will appear in the browser
         with numeric chromosomes first, followed by alphabetized non-numeric features."
        #paste(c('You selected these rows on the page:', input$rows),collapse = ' ')
      })
      
      output$chromRows <- renderUI({
        #if(input$plotAll == TRUE){return()}
        lapply(input$rows, function(i) {      
          #traits <- sort(unique(values[[input$datasets]][,i]))          
          list(HTML('<br>'),
          textInput(inputId=paste0("ind",i), label=seqFeatures[i+1,"Phytozome Name"],value=gsub("\\D+","",seqFeatures[i+1,"Phytozome Name"])))
        })
      })
      
      output$allDoneText <- renderUI({
        input$actionButtonID
        
        isolate({
          if(length(input$rows)>0){
            htmlText <- paste("<br><h4>The annotation files for",displayName,"have been successfully added to Zbrowse. You can close this tab.</h4>")
            list(HTML(htmlText))
          }
        })
      })

      output$allDone <- renderTable({
        
        #add dependence on button
        input$actionButtonID
        
        #isolate the cached table so it only responds when the button is pressed
        isolate({
          if(length(input$rows)>0){
            withProgress(message = 'Making annotation files', value = 0, {
              chromTrans <- ldply(input$rows,function(x){c(seqFeatures[x+1,"Phytozome Name"],input[[paste0("ind",x)]][1])})
              colnames(chromTrans) <- c("Original Name","Output Name")
              geneLoc <- geneLoc[geneLoc$seqname %in% chromTrans$`Original Name`,]
              geneLoc <- merge(geneLoc,chromTrans,by.x="seqname",by.y="Original Name",all.x=T)
              colnames(geneLoc)[colnames(geneLoc)=="Output Name"] <- "chr"
              
              #bit messy, but do two rounds of colsplit, one splitting on ;Name=, take 2nd column, next splitting on ;, take first part
              geneLoc$name <- colsplit(geneLoc$attributes,";Name=",c("ID","name"))[,2]
              geneLoc$name <- colsplit(geneLoc$name,";",c("name","rest"))[,1]
              

              annotationFile <- read.table(paste0(phytoPath,"/annotation/",annotFileLoc),head=FALSE,stringsAsFactors = FALSE,sep="\t",comment.char="",quote = "")
              
              if(length(defFileLoc)>0){
                defFile <- read.table(paste0(phytoPath,"/annotation/",defFileLoc),stringsAsFactors = FALSE,head=FALSE,sep="\t",comment.char = "",quote="")
              }else{
                #no defFile, just make a dummy data frame
                defFile <- data.frame("name","defLine")
              }
              
              colnames(defFile) <- c("name","defLine")
              incProgress(.3, detail = "Finished Reading Files")
              #I think the first 10 columns are standard, but the best Arab and best Rice are sometimes not present
              if(ncol(annotationFile)==16){
                colnames(annotationFile) <- c("ID","name","transName","protName","PFAM","Panther","KOG","KEGG ec","KEGG Orthology","GO Terms","bestArabHitName","bestArabHitSymbol","bestArabHitDefline","bestRiceHitName","bestRiceHitSymbol","bestRiceHitDefline")
              }else if(ncol(annotationFile)==13){
                #see if column 11 ids look like arabidopsis (starts with AT), if not assume they're rice
                if(length(grep("^AT",annotationFile$V11)) > 1){
                  colnames(annotationFile) <- c("ID","name","transName","protName","PFAM","Panther","KOG","KEGG ec","KEGG Orthology","GO Terms","bestArabHitName","bestArabHitSymbol","bestArabHitDefline")
                  annotationFile$bestRiceHitName <- "Not Present"
                  annotationFile$bestRiceHitSymbol <- "Not Present"
                  annotationFile$bestRiceHitDefline <- "Not Present"
                }else{
                  colnames(annotationFile) <- c("ID","name","transName","protName","PFAM","Panther","KOG","KEGG ec","KEGG Orthology","GO Terms","bestRiceHitName","bestRiceHitSymbol","bestRiceHitDefline")
                  annotationFile$bestArabHitName <- "Not Present"
                  annotationFile$bestArabHitSymbol <- "Not Present"
                  annotationFile$bestArabHitDefline <- "Not Present"                
                }
              }else if(ncol(annotationFile)==10){
                colnames(annotationFile) <- c("ID","name","transName","protName","PFAM","Panther","KOG","KEGG ec","KEGG Orthology","GO Terms")
                annotationFile$bestArabHitName <- "Not Present"
                annotationFile$bestArabHitSymbol <- "Not Present"
                annotationFile$bestArabHitDefline <- "Not Present"                
                annotationFile$bestRiceHitName <- "Not Present"
                annotationFile$bestRiceHitSymbol <- "Not Present"
                annotationFile$bestRiceHitDefline <- "Not Present"              
              }else{
                stop("Too few columns found in annotation file:",annotFileLoc)
              }
              
              #remove rows with duplicate names (e.g. alternate transcripts) from annotation file
              annotationFile <- annotationFile[which(!(duplicated(annotationFile$name))),]
              
              #list of annotation columns to keep when merged with geneLoc
              keepCols <- c("ID","name","transName","PFAM","Panther","KOG","KEGG ec","KEGG Orthology","GO Terms","bestArabHitName","bestArabHitSymbol","bestArabHitDefline","bestRiceHitName","bestRiceHitSymbol","bestRiceHitDefline")
              
              #remove the .1 etc from defFile
              defFile$name <- sub("\\.\\d","",defFile$name,perl = T)
              defFile <- defFile[which(!(duplicated(defFile$name))),]
              
              annotations <- merge(geneLoc[,c("name","chr","start","end","strand")],annotationFile[,keepCols],by = "name",all.x = T,all.y = F)
              annotations <- merge(annotations,defFile,by = "name",all.x=T,all.y=F)
              colnames(annotations)[colnames(annotations) %in% c("chr","start","end")] <- c("chromosome","transcript_start","transcript_end")
              incProgress(.7, detail = "Files Parsed")

              annotFileOut <- paste0("./annotations/",orgName,"_",orgVersion,".csv")
              write.table(annotations,file=annotFileOut,sep=",",row.names = F,col.names = T)
              
              #calculate chromosome sizes
              chrSizes <- paste(daply(annotations, .(chromosome), function(x){paste0(unique(x$chromosome),":",round(max(x$transcript_end)+max(x$transcript_end)*0.003))}),collapse = ",")
              
              #phytoOrgs <- read.csv(file = "phytozomeOrganismsAvailable.csv",header=TRUE,stringsAsFactors = FALSE,comment.char="",quote = "")

              incProgress(1, detail = "Writing Files")                      
              fileConn<-file(paste0("./organisms/",orgName,"_",orgVersion,".txt"))
              writeLines(c(displayName,chrSizes,annotFileOut), fileConn)
              close(fileConn)                        
            })  
            chromTrans            
            #paste(c('You selected these rows on the page:', input$rows),collapse = ' ')
          }
        })
      })  
            
    }#end server
  )
  ,launch.browser=TRUE)
}

addOrganism(phytoPath)
