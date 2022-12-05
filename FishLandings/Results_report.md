# Results Report

Results written by Emma Strand based on FishLandings survey data from Kenya_SamakiSalama project.

Google slides for figure deck: https://docs.google.com/presentation/d/1-w_XW8_gXJf1OlE-m-AOzcJTGmf5Ofwe0_jLv_Ql7VM/edit#slide=id.p

Scripts used:  
- Quality Control: [QC.md](https://github.com/emmastrand/Kenya_SamakiSalama/blob/main/FishLandings/scripts/QC.md)    
- Catch per unit effort and length/maturity analysis: [CPUE-maturity-length-analysis.md](https://github.com/emmastrand/Kenya_SamakiSalama/blob/main/FishLandings/scripts/CPUE-maturity-length-analysis.md)      
- Relative abundance: [Relative-abundance-analysis.md]()  

### Summary on quality control (QC.md)

I've written a [protocol](https://github.com/emmastrand/Kenya_SamakiSalama/blob/main/FishLandings/scripts/QC.md#-protocol-to-run-this-with-a-future-xlsx-file) within the QC R markdown file for future personnel to run the quality control script. Each section requires personnel to read through the output (i.e. output list of fishermen enumerators) and double check that list is as expected. The scientific name section will take the longest.

I used the file `Fishlandings-data- CC-JM-Clay-IW updated 04-09-2022.xlsx` as input for the quality control R markdown.


### CPUE for Experimental control issue 

2 surveys: 'catch composition' and 'fishing operation':

![](https://github.com/emmastrand/Kenya_SamakiSalama/blob/main/FishLandings/Screen%20Shot%202022-12-05%20at%201.53.52%20PM.png?raw=true)

![](https://github.com/emmastrand/Kenya_SamakiSalama/blob/main/FishLandings/Screen%20Shot%202022-12-05%20at%201.54.50%20PM.png?raw=true)

Sum of number of fish per survey / total traps per suvey  = CPUE 

So this 