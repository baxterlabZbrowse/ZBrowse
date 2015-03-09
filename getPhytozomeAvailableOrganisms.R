#Scrapes the phytozome release notes page for the table containing available organisms and makes into a dataframe

require(RSelenium)
# RSelenium::startServer() # if needed
remDr <- remoteDriver()
remDr$open()
remDr$setImplicitWaitTimeout(3000)
remDr$navigate("http://phytozome.jgi.doe.gov/pz/portal.html#!releaseNotes")

tableElem <- remDr$findElement(using = "class", "mytable")
phytozomeOrgTable <- readHTMLTable(htmlParse(tableElem$getElementAttribute("outerHTML")[[1]]),header = TRUE)[[1]]
#get rid of factor levels
phytozomeOrgTable[] <- lapply(phytozomeOrgTable, as.character)
phytozomeOrgTable[,1] <- gsub("\n\\s+"," ",phytozomeOrgTable[,1])
phytozomeOrgTable[,4] <- gsub("\n\\s+"," ",phytozomeOrgTable[,4])

doc <- htmlParse(tableElem$getElementAttribute("outerHTML")[[1]])
links <- as.vector(xpathSApply(doc, "//a/@href"))
links <- grep("portal.html",links,value=T)

urlBase <- "http://phytozome.jgi.doe.gov"
links <- paste0(urlBase,links)

phytozomeOrgTable$orgLink <- links

write.table(phytozomeOrgTable,file="phytozomeOrganismsAvailable.csv",sep=",",col.names=TRUE,row.names=FALSE)
