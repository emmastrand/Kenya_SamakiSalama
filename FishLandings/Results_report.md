# Results Report

Results written by Emma Strand based on FishLandings survey data from Kenya_SamakiSalama project.

Scripts used:  
- Quality Control: [QC.md](https://github.com/emmastrand/Kenya_SamakiSalama/blob/main/FishLandings/scripts/QC.md)    
- Catch per unit effort and length/maturity analysis: [CUE-maturity-length-analysis.md](https://github.com/emmastrand/Kenya_SamakiSalama/blob/main/FishLandings/scripts/CUE-maturity-length-analysis.md)      
- Relative abundance: [Relative-abundance-analysis.md]()  

### Summary on quality control (QC.md)

I've written a [protocol](https://github.com/emmastrand/Kenya_SamakiSalama/blob/main/FishLandings/scripts/QC.md#-protocol-to-run-this-with-a-future-xlsx-file) within the QC R markdown file for future personnel to run the quality control script. Each section requires personnel to read through the output (i.e. output list of fishermen enumerators) and double check that list is as expected. The scientific name section will take the longest.

I used the file `Fishlandings-data- CC-JM-Clay-IW combined 7-28-2022.xlsx` as input for the quality control R markdown.

**Recommendations for project moving forward:**    
1.) *Require* all data entry to be done with the drop down menu instead of manual entry. During quality control, I found many spelling errors such as 10 different spellings for 1 genus or 1 species name. See `gsub` functions within the QC R markdown file for the mistakes I found and had to correct in R.  

The output from the QC.md script is the file `cleaned-Fishlandings-data- CC-JM-Clay-IW combined 7-28-2022.xlsx`.

## Total catch and grams per unit effort between modified and unmodified traps (CUE...Rmd)

Goal: Total catch per unit effort between modified and traditional traps. It would be great to see this as grams captured per trap set.

### Total catch per trap

*Total catch per trap for modified traps was significantly higher (p < 0.0001). This result does not seem to be influenced by any one particular fisherman or landing site (see CUE.Rmd for figures).*

![](https://github.com/emmastrand/Kenya_SamakiSalama/raw/main/FishLandings/scripts/CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-4-1.png)

```
t.test(catch_per_trap~trap_type, data = modified_trap_df, var.equal = FALSE)


	Welch Two Sample t-test

data:  catch_per_trap by trap_type
t = 16.389, df = 1484, p-value < 2.2e-16
alternative hypothesis: true difference in means between group MODIFIED and group UNMODIFIED is not equal to 0
95 percent confidence interval:
 44.18302 56.19691
sample estimates:
  mean in group MODIFIED mean in group UNMODIFIED
                69.91612                 19.72616
```

![](https://github.com/emmastrand/Kenya_SamakiSalama/raw/main/FishLandings/scripts/CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-4-4.png).

### Grams per trap

*Gramps per trap set was significantly higher in modified traps (p < 0.0001). This result also does not appear to be influended by one particular fisherman or landing site (see CUE.Rmd for figures).*

![](https://github.com/emmastrand/Kenya_SamakiSalama/raw/main/FishLandings/scripts/CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-5-1.png)

```
t.test(grams_per_trap~trap_type, data = modified_trap_df, var.equal = FALSE)

Welch Two Sample t-test

data:  grams_per_trap by trap_type
t = 4.2895, df = 2182.7, p-value = 1.869e-05
alternative hypothesis: true difference in means between group MODIFIED and group UNMODIFIED is not equal to 0
95 percent confidence interval:
0.0403919 0.1084280
sample estimates:
mean in group MODIFIED mean in group UNMODIFIED
             0.8647085                0.7902985
```

![](https://github.com/emmastrand/Kenya_SamakiSalama/raw/main/FishLandings/scripts/CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-5-4.png)

### The relationship between total catch and grams per trap

*No stats yet but my thinking is - Modified traps have a higher number of fish caught per trap, but not significantly higher weight per trap. The two traps behave differently - in modified traps, if the total catch per trap is higher, then the grams per trap is higher but that isn't true for modified traps. Higher numbers doesn't necessarily mean higher weight for umodified traps.*

![](https://github.com/emmastrand/Kenya_SamakiSalama/raw/main/FishLandings/scripts/CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-6-1.png)

## Top species caught in modified and unmodified traps

Goal: Species catch per unit effort between modified and traditional traps. Take the top 3-5 species and run #1 for them separately.

[Link to calculating top species caught code](https://github.com/emmastrand/Kenya_SamakiSalama/blob/main/FishLandings/scripts/CUE-maturity-length-analysis.md#-calculate-top-species-caught)

[Link to comparing top spp between traps code](https://github.com/emmastrand/Kenya_SamakiSalama/blob/main/FishLandings/scripts/CUE-maturity-length-analysis.md#-top-species-stats-per-trap)

![](https://github.com/emmastrand/Kenya_SamakiSalama/raw/main/FishLandings/scripts/CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-13-1.png)

## Catch per unit effort for top species by maturity

Goal: Total mature fish catch per unit effort between modified and traditional traps. This will have to be for the top 3-5 species separately. Go to Fishbase and find the length at first maturity for that particular species, then assign each fish a “mature” or “immature” status in the data and calculate.

[Link to this portion of the script](https://github.com/emmastrand/Kenya_SamakiSalama/blob/main/FishLandings/scripts/CUE-maturity-length-analysis.md#-catch-per-unit-effort-for-top-species-by-maturity)

![](https://github.com/emmastrand/Kenya_SamakiSalama/raw/main/FishLandings/scripts/CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-16-2.png)

Insert statistics from this -- running into an error in R code currently, will update this ASAP.

## Length frequency plots of top species

Goal: Length frequency of top 3-5 species in modified versus traditional (different colors) with Lmat etc. indicators pulled from Fishbase.

[Link to this portion of the script](https://github.com/emmastrand/Kenya_SamakiSalama/blob/main/FishLandings/scripts/CUE-maturity-length-analysis.md#-length-frequency-plots-of-top-species)

Dotted line = Lm range (i.e. Lm = 11.5; Lm range = 11-15). Our length data is in bins so I could only plot this as a bin too.

![](https://github.com/emmastrand/Kenya_SamakiSalama/raw/main/FishLandings/scripts/CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-19-1.png)

## Catch and length data of mature fish

Goal: Average length of catch versus length at first maturity (Lmat). Take the difference for each fish in the data against its length at first maturity and then calculate a weighted value for modified versus traditional traps where a value above 0 represents a fish above Lmat and a value below represents a fish below Lmat.

See notes on the code here: https://github.com/emmastrand/Kenya_SamakiSalama/blob/main/FishLandings/scripts/CUE-maturity-length-analysis.md#-catch-and-length-data-of-mature-fish. I'm not sure if this is the best way to calculate this?

![](https://github.com/emmastrand/Kenya_SamakiSalama/raw/main/FishLandings/scripts/CUE-maturity-length-analysis_files/figure-gfm/unnamed-chunk-18-1.png)

## Relative abundance plots

In progress.. The output isn't what I expect (should be more than 3 on the y axis?) so trying to troubleshoot this.

[Code](https://github.com/emmastrand/Kenya_SamakiSalama/blob/main/FishLandings/scripts/Relative-abundance-analysis.md#relative-abundance-analysis-of-fishing-landings-dataset)

![](https://github.com/emmastrand/Kenya_SamakiSalama/raw/main/FishLandings/scripts/Relative-abundance-analysis_files/figure-gfm/unnamed-chunk-5-1.png)
