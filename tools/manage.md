Load your data into memory, select appropriate columns, and produce an interactive plot.

#### Load data

The best way to get data in and out of the GWAS browser (and R) is to use the R-data format (.rda). These are binary files that can be stored compactly and that can be read into R quickly. Choose the .rda radio button and click 'Choose file' to locate the file you want to load on your computer.

Loading data from Excel can be achieved in two ways. First, you can save data from excel to a csv format and then choose the .csv radio button. Most likely you will have a header row in the csv file for variable names. If the data are not comma separated you can choose a semicolon or tab separated format. Then click 'Choose file' and locate the csv file you created in Excel on your computer. 

#### Select Appropriate Columns

The manage tab gives a look at the first ten rows of your data and provides selection boxes to tell the plotting software which columns to use for graphing. Select appropriate columns, then click the Whole Genome View tab to take a look at your data. Note that traits may be a composition of multiple columns (e.g. a trait and a location), so multiple columns can be selected in that box.

#### Browser Usage

Once you've selected your data in the manage tab, you can view the browser plot and select your combinations of traits from the boxes on the left. The genome view tab provides an overview of significant SNPs across the genome.  Clicking any point will switch the view to the chromosome tab.  The chromosome tab provides a look at just the chromsome you clicked as well as a zoomed in look at the region directly surrounding your SNP of interest.
The maximum number of datapoints that can be viewed for a single trait is 5000. If you attempt to plot more than this number the software will simply display an empty plot. Try narrowing down your dataset and reuploading the file.
