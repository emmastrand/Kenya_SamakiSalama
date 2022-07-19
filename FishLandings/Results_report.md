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

## Total catch and grams per unit effort between modified and unmodified traps (CUE...Rmd)

### Total catch per trap

*Total catch per trap for modified traps was significantly higher (p < 0.0001). This result does not seem to be influenced by any one particular fisherman or landing site (see CUE.Rmd for figures).*

![](https://github.com/emmastrand/Kenya_SamakiSalama/raw/main/FishLandings/scripts/CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-4-1.png)

```
t.test(catch_per_trap~trap_type, data = modified_trap_df, var.equal = FALSE)

##
##  Welch Two Sample t-test
##
## data:  catch_per_trap by trap_type
## t = 10.041, df = 1903.5, p-value < 2.2e-16
## alternative hypothesis: true difference in means between group MODIFIED and group UNMODIFIED is not equal to 0
## 95 percent confidence interval:
##  25.27546 37.54562
## sample estimates:
##   mean in group MODIFIED mean in group UNMODIFIED
##                 60.88043                 29.46989
```

But if you break this down by year and month - 2021 numbers seem too low? Look at scales. Subsetting to 2022 isn't likely to change the stat above but we might want to do that for the paper.

![](https://github.com/emmastrand/Kenya_SamakiSalama/raw/main/FishLandings/scripts/CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-4-4.png).

### Grams per trap

*Gramps per trap set was not significantly different (p=0.3201) between modified and unmodified traps. This result also does not appear to be influended by one particular fisherman or landing site (see CUE.Rmd for figures).*

![](https://github.com/emmastrand/Kenya_SamakiSalama/raw/main/FishLandings/scripts/CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-5-1.png)

```
t.test(grams_per_trap~trap_type, data = modified_trap_df, var.equal = FALSE)

##
##  Welch Two Sample t-test
##
## data:  grams_per_trap by trap_type
## t = -0.99452, df = 1939.3, p-value = 0.3201
## alternative hypothesis: true difference in means between group MODIFIED and group UNMODIFIED is not equal to 0
## 95 percent confidence interval:
##  -0.06561682  0.02145999
## sample estimates:
##   mean in group MODIFIED mean in group UNMODIFIED
##                0.8929056                0.9149840
```

Same issue as total catch per trap set - see above 2021 note.

![](https://github.com/emmastrand/Kenya_SamakiSalama/raw/main/FishLandings/scripts/CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-5-4.png)

### The relationship between total catch and grams per trap

*No stats yet but my thinking is - Modified traps have a higher number of fish caught per trap, but not significantly higher weight per trap. The two traps behave differently - in modified traps, if the total catch per trap is higher, then the grams per trap is higher but that isn't true for modified traps. Higher numbers doesn't necessarily mean higher weight for umodified traps.*

![](https://github.com/emmastrand/Kenya_SamakiSalama/raw/main/FishLandings/scripts/CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-6-1.png)

## Top species caught in modified and unmodified traps



![](https://github.com/emmastrand/Kenya_SamakiSalama/raw/main/FishLandings/scripts/CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-13-1.png)
