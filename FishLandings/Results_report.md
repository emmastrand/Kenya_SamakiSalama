# Results Report

Results written by Emma Strand based on FishLandings survey data from Kenya_SamakiSalama project.

Scripts used:  
- Quality Control: [QC.md](https://github.com/emmastrand/Kenya_SamakiSalama/blob/main/FishLandings/scripts/QC.md)    
- Catch per unit effort and length/maturity analysis: [CUE-maturity-length-analysis.md](https://github.com/emmastrand/Kenya_SamakiSalama/blob/main/FishLandings/scripts/CUE-maturity-length-analysis.md)  

### Summary on quality control (QC.md)

I've written a [protocol](https://github.com/emmastrand/Kenya_SamakiSalama/blob/main/FishLandings/scripts/QC.md#-protocol-to-run-this-with-a-future-xlsx-file) within the QC R markdown file for future personnel to run the quality control script. Each section requires personnel to read through the output (i.e. output list of fishermen enumerators) and double check that list is as expected. The scientific name section will take the longest.

I used the file `Fishlandings-data_clay June_updated-IW.xlsx` as input for the quality control R markdown.

**Recommendations for project moving forward:**    
1.) *Require* all data entry to be done with the drop down menu instead of manual entry. During quality control, I found many spelling errors such as 10 different spellings for 1 genus or 1 species name. See `gsub` functions within the QC R markdown file for the mistakes I found and had to correct in R.  

The output from the QC.md script is the file `Fishlandings-cleaned-clay-June_updated-IW.xlsx`.

### Total catch and grams per unit effort between modified and unmodified traps
