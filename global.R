#Load GenomicRange packages locally, order here matters
pkgs <- c("BiocGenerics","S4Vectors","IRanges","XVector","GenomeInfoDb","GenomicRanges")
for(p in pkgs) suppressPackageStartupMessages(library(p, quietly=TRUE, character.only=TRUE,lib.loc="./lib/"))

library(shiny)
library(plyr)
library(rCharts)
library(xtable)
library(tools)
#install shinyIncubator like this (gives progress bar):
#install.packages("devtools")
#library(devtools)
#devtools::install_github("shiny-incubator", "rstudio")
#library(shinyIncubator)
#options(shiny.maxRequestSize=-1)

addResourcePath('datatables','www/DataTables/')
addResourcePath('tabletools','www/TableTools/')
addResourcePath('highcharts','www/highcharts/')


#aggTable <- read.table("./www/config/data/sigGWASsnpsCombinedIterations.longhorn.allLoc.csv",sep=",",stringsAsFactors=FALSE,head=TRUE)
#aggTable$negLogP <- -log(aggTable$avgPval,base=10)
#aggTable$modIncProb = aggTable$numIterations/100
#aggTable <- aggTable[order(aggTable$chr,aggTable$bp),]

#Add organisms from files instead of hardcoded arrays
files<-list.files(path="./organisms/")
chrSize<-list()
for(i in 1:length(files)){
  if(tools::file_ext(files[i]) == "txt"){
    filename=""
    filename=paste("./organisms/",files[i],sep="")
    conn=file(filename,open="r")
    data<-readLines(conn)
    
    key<-data[1]
    value<-c(lapply(strsplit(data[2], ","), as.numeric))
    chrSize[key]<-value
    
    close(conn)
  }
}
#List of chromsome lengths for various organisms
#chrSize <- list(Corn=c(301354135,237068873,232140174,241473504,217872852,169174353,176764762,175793759,156750706,150189435),
 #    Soybean=c(55915595,51656713,47781076,49243852,41936504,50722821,44683157,46995532,46843750,50969635,39172790,40113140,44408971,49711204,50939160,37397385,41906774,62308140,50589441,46773167),
  #   Arabidopsis=c(30427671,19698289,23459830,18585056,26975502),
   #  Sorghum=c(73933847,78027413,74539055,68108026,62428788,62294152,64407977,55559831,59722314,61076732))

#much slower way of adding totalBP
#aggTable <- adply(aggTable,1,function(x){data.frame(totalBP=sum(x$bp,chrSize$bp[chrSize$chr %in% if(x$chr==1) 0 else c(1:(x$chr-1))]))})
#cumBP<-c(0,cumsum(chrSize["Corn"][[1]]))
#numeachchr<-aggregate(aggTable$bp,list(aggTable$chr),length)$x
#adjust<-rep(cumBP[1],numeachchr[1])
#for (i in 2:max(unique(aggTable$chr))){
#  adjust<-c(adjust,rep(cumBP[i],numeachchr[i]))
#}
#aggTable$totalBP<-aggTable$bp+adjust

#Add organisms from files instead of hardcoded arrays
annotGeneLoc<-list()
for(i in 1:length(files)){
  if(tools::file_ext(files[i]) == "txt"){
    filename=""
    filename=paste("./organisms/",files[i],sep="")
    conn=file(filename,open="r")
    data<-readLines(conn)
    
    key<-data[1]
    locValue<-read.table(data[3],sep=",",head=TRUE,stringsAsFactors = FALSE)
    annotGeneLoc[key]<-list(locValue)
    
    close(conn)
  }
}
#load annotation data files
#annotGeneLoc <- list(Corn=read.table("./ZmB73_Canonical_Gene_func_annotations.csv",sep=",",head=TRUE,stringsAsFactors = FALSE),
 #                    Soybean=read.delim("./mergedSoyAnnotations.modifiedForBrowser.txt",sep="\t",head=TRUE,stringsAsFactors=FALSE,na.strings="NA",comment.char=""),
  #                   Arabidopsis = read.table("./mergedTAIR10Annotations.modifiedForBrowser.txt",sep="\t",head=TRUE,stringsAsFactors=FALSE,comment.char=""),
   #                  Sorghum = read.table("./SorghumV1.4.with2.1annotations.mergedPhytozomeFiles.csv",sep=",",head=TRUE,stringsAsFactors=FALSE,comment.char=""))

helpPopup <- function(title, content, placement=c('right', 'top', 'left', 'bottom'),
                      trigger=c('click', 'hover', 'focus', 'manual')) {  
  tagList(
    singleton(tags$head(tags$script("$(function() { $(\"[data-toggle='popover']\").popover(); })"))),
    tags$a(href = "#", `data-toggle` = "popover", title = title, `data-content` = content,
           `data-placement` = match.arg(placement, several.ok=TRUE)[1],
           `data-trigger` = match.arg(trigger, several.ok=TRUE)[1], tags$i(class="icon-question-sign"))
  )
}

helpModal <- function(title, link, content) {
  html <- sprintf("<div id='%s' class='modal hide fade in' style='display: none; '>
<div class='modal-header'><a class='close' data-dismiss='modal' href='#'>&times;</a>
<h3>%s</h3>
</div>
<div class='modal-body'>%s</div>
</div>
<a data-toggle='modal' href='#%s' class='icon-question-sign'></a>", link, title, content, link)
  Encoding(html) <- 'UTF-8'
  HTML(html)
}

# binding for a text input that only updates when the return key is pressed
returnTextInput <- function(inputId, label, value = "") {
  tagList(
    singleton(tags$head(tags$script(src = "js/returnTextInputBinding.js"))),
    tags$label(label, `for` = inputId),
    tags$input(id = inputId, type = "text", value = value, class = "returnTextInput")
  )
}

#functions from shiny leaflet example, sets up div columns in page layout
row <- function(...) {
  tags$div(class="row", ...)
}

col <- function(width, ...) {
  tags$div(class=paste0("span", width), ...)
}

#initiate colors for plot data (these 10 are the highchart defaults)
colors <- c('#2f7ed8','#0d233a', '#8bbc21','#910000','#1aadce','#492970','#f28f43','#77a1e5','#c42525','#a6c96a')

#a list of 20 colors from http://stackoverflow.com/questions/470690/how-to-automatically-generate-n-distinct-colors
moreColors <- c(
  '#FFB300', #Vivid Yellow
  '#803E75', #Strong Purple
  '#FF6800', #Vivid Orange
  '#A6BDD7', #Very Light Blue
  '#C10020', #Vivid Red
  '#CEA262', #Grayish Yellow
  '#817066', #Medium Gray
  '#007D34', #Vivid Green
  '#F6768E', #Strong Purplish Pink
  '#00538A', #Strong Blue
  '#FF7A5C', #Strong Yellowish Pink
  '#53377A', #Strong Violet
  '#FF8E00', #Vivid Orange Yellow
  '#B32851', #Strong Purplish Red
  '#F4C800', #Vivid Greenish Yellow
  '#7F180D', #Strong Reddish Brown
  '#93AA00', #Vivid Yellowish Green
  '#593315', #Deep Yellowish Brown
  '#F13A13', #Vivid Reddish Orange
  '#232C16'  #Dark Olive Green
)

allColors <- c(moreColors,colors)

#Steps to adding a new organism
#1) Add organism name to dropbdown list in server.R -> output$organism
#2) Add chromosome sizes to chrSize list in global.R
#3a) Modify annotaion file to have at least these column names:
  #chromosome,transcript_start,tanscript_end
  #additional columns with id,strand, and description are helpful for the display in step 4
#3b) Load annotation file into annotGeneLoc in global.R (if annotations are getting too big, this may need to be handled differenlty)
#4a) Choose a URL that can be built from annotation ID and add to server.R zChart
#4) Change how annotations are displayed in server.R -> output$zChart -> thisChrAnnot

#arabidopsis genome sizes from TAIR10
# Chr1  30427671
# Chr2	19698289
# Chr3	23459830
# Chr4	18585056
# Chr5	26975502
# ChrC	154478
# ChrM	366924
#soybean genome sizes
# ##species http://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?id=3847
# ##genome-build JGI1.01
# Gm01  55915595
# Gm02	51656713
# Gm03	47781076
# Gm04	49243852
# Gm05	41936504
# Gm06	50722821
# Gm07	44683157
# Gm08	46995532
# Gm09	46843750
# Gm10	50969635
# Gm11	39172790
# Gm12	40113140
# Gm13	44408971
# Gm14	49711204
# Gm15	50939160
# Gm16	37397385
# Gm17	41906774
# Gm18	62308140
# Gm19	50589441
# Gm20	46773167

##########################Code for modifying annotaiton files##########################
#load soybean annotation data file (the following code was used to make the description columns short enough to easily display in the browser)
#soyAnnotFile <- read.delim("~/Downloads/mergedSoyAnnotations.txt",sep="\t",head=TRUE,stringsAsFactors = FALSE,na.strings="NA",comment.char="")
#soyAnnotFile$TopUniref100DescriptionSmall <- sub("UniRef100_\\w{6}\\s","\\1",soyAnnotFile$TopUniref100Description,perl=T)
#soyAnnotFile$TopUniref100DescriptionExtraSmall <- (colsplit(soyAnnotFile$TopUniref100DescriptionSmall,split="n=\\d",names=c("keep","rest")))[,1]
#write.table(soyAnnotFile,"mergedSoyAnnotations.modifiedForBrowser.txt",sep="\t",col.names=T,row.names=F)
#soyAnnotFile <- read.delim("../mergedSoyAnnotations.modifiedForBrowser.txt",sep="\t",head=TRUE,stringsAsFactors=FALSE,na.strings="NA",comment.char="")

#load arabidopsis annotation file (this code was used to make columns to display in browser)
#downloaded from ftp://ftp.arabidopsis.org/home/tair/Genes/TAIR10_genome_release/
#removed Computational_description column from annotaiton file
#keep only chromsomes 1:5
#keep only mRNA sequences
# library(reshape) #for colsplit
# gffGenes <- read.table("~/Documents/TAIR10_GFF3_genes.gff",head=FALSE,stringsAsFactors=FALSE,sep="\t")
# gffGenes$chromosome <- sub("Chr","",gffGenes$V1)
# gffGenes <- gffGenes[gffGenes$chromosome %in% 1:5,] #filter chloroplast and mitochondiral genes
# gffGenes <- gffGenes[gffGenes$V3 == "mRNA",]
# gffGenes <- cbind(gffGenes,colsplit(gffGenes$V9,"Name=|;",names=c("ID","Parent","empty","name","Index"))[4])
# colnames(gffGenes)[c(4:5,7)] <- c("transcript_start","transcript_end","strand")
# geneAnnots <- read.delim("~/Documents/TAIR10_functional_descriptions_20130831.txt",head=TRUE,stringsAsFactors=FALSE,sep="\t",comment.char="")
# length(intersect(gffGenes$name,geneAnnots$name))
# dim(gffGenes) #every mRNA name in gffGenes matches a line in geneAnnots
# #merge them
# mergedArabAnnot <- merge(gffGenes,geneAnnots,by="name",all=F,sort=F)
# mergedArabAnnot <- mergedArabAnnot[,c("name","transcript_start","transcript_end","strand","chromosome","short_description","Curator_summary")]
# mergedArabAnnot$Curator_summary[mergedArabAnnot$Curator_summary==""] <- NA
# mergedArabAnnot$short_description[mergedArabAnnot$short_description==""] <- NA
# mergedArabAnnot$Locus <- sub("\\.\\d","",mergedArabAnnot$name)
# 
# #keep only representative gene models
# repGenes <- read.delim("~/Documents/TAIR10_representative_gene_models",stringsAsFactors=FALSE,head=FALSE)
# mergedArabAnnot <- mergedArabAnnot[which(mergedArabAnnot$name %in% repGenes$V1),]
# write.table(mergedArabAnnot,"mergedTAIR10Annotations.modifiedForBrowser.txt",sep="\t",col.names=T,row.names=F,qmethod="d")

