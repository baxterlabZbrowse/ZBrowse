#Zbrowse

`Zbrowse` is an interactive GWAS viewer focused on comparing results across traits and other variables. 

### Requirements

- [R] (http://www.r-project.org/) (tested on R versions > 3.1)
- [R Studio] (http://www.rstudio.com/)

### Download and Install

1. Navigate to http://www.baxterlab.org/	
2. Click "Download ZBrowse".
  - This will download a zipped file to your computer. 

3. Unzip the file and open `interactiveGWASuploadable.Rproj`
  - This will open the project in R Studio.

4. At the R command prompt enter:
`source("startup.R")`
  - This will check for and install any necessary libraries and open `ZBrowse` in your default browser.

###Using the Browser

####Load data

#####Prepare GWAS dataset

Your GWAS dataset is likely already in a comma-separated format. If not, open your dataset in spreadsheet software such as Excel and save as csv. `Zbrowse` is designed for plotting the most significant hits from a GWAS experiment and is therefore limited to plotting less than 5000 markers for a single trait. Most GWAS results files contain results from every marker, so it may be necessary to filter the non-significant markers before uploading to the browser.

#####Load into browser

In the browser, select the '.csv' radio button. Most likely you have a header row in the csv file for variable names, if not, deselect the 'Header' check box. If the data are not comma separated you can also choose a semicolon or tab separated format. Then click 'Choose file' and locate the csv file on your computer.

####Select Appropriate Columns

The manage tab gives a look at the first ten rows of your data and provides selection boxes to tell the plotting software which columns to use for graphing. Select appropriate columns, then click the 'Whole Genome View' tab to take a look at your data. Note that traits may be a composition of multiple columns (e.g. a trait and a location), so multiple columns can be selected in that box. 

####Browser Usage

Once you've selected your data in the manage tab, you can view the browser plot and select your combinations of traits from the boxes on the left. The genome view tab provides an overview of significant SNPs across the genome. Clicking any point will switch the view to the chromosome tab. The chromosome tab provides a look at just the chromosome you clicked as well as a zoomed in look at the region directly surrounding your SNP of interest. The maximum number of data-points that can be viewed for a single trait is 5000. If you attempt to plot more than this number the software will simply display an empty plot. Try narrowing down your dataset and re-uploading the file.

