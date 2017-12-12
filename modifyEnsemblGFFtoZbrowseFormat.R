library(reshape2)
library(plyr)
#library(shiny)

#######Fill in path to organism here, then highlight everything and run#########
geneLocFile <- "~/Downloads/Zea_mays.AGPv4.37.chr.gff3.gz"
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

#pull in information about where each Gene falls  
geneLoc <- gffRead(gzfile(geneLocFile))
geneLoc <- geneLoc[geneLoc$feature=="gene",]
geneLoc <- geneLoc[geneLoc$seqname %in% c(1:10),]
colnames(geneLoc)[colnames(geneLoc)=="seqname"] <- "chr"

geneLoc$name <- colsplit(geneLoc$attributes,";gene_id=",c("ID","name"))[,2]
geneLoc$name <- colsplit(geneLoc$name,";",c("name","rest"))[,1]

geneLoc$description <- colsplit(geneLoc$attributes,";description=",c("ID","name"))[,2]
geneLoc$description <- colsplit(geneLoc$description,";",c("name","rest"))[,1]

geneLoc$geneName <- colsplit(geneLoc$attributes,";Name=",c("ID","name"))[,2]
geneLoc$geneName <- colsplit(geneLoc$geneName,";",c("name","rest"))[,1]
geneLoc$geneName <- paste(geneLoc$name,geneLoc$geneName)
geneLoc <- geneLoc[,c("chr","start","end","strand","name","description","geneName")]
colnames(geneLoc)[colnames(geneLoc) %in% c("chr","start","end")] <- c("chromosome","transcript_start","transcript_end")
chrSizes <- paste(daply(geneLoc, .(chromosome), function(x){paste0(unique(x$chromosome),":",round(max(x$transcript_end)+max(x$transcript_end)*0.003))}),collapse = ",")

annotFileOut <- paste0("./annotations/","Zmays","_","AGPv4.Ensembl37",".csv")
write.table(geneLoc,file=annotFileOut,sep=",",row.names = F,col.names = T)

displayName <- "Zea mays AGPv4 Ensembl 37"
fileConn<-file(paste0("./organisms/","Zmays","_","AGPv4.Ensembl37",".txt"))
writeLines(c(displayName,chrSizes,annotFileOut), fileConn)
close(fileConn)                        







