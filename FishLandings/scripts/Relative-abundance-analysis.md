Relative abundance analysis of Fishing Landings dataset
================
Author: Emma Strand; <emma_strand@uri.edu>

## Aims and Results

**1. Quantify and visualize relative abundance for each modified and
unmodified trap using phyloseq.**

**2. Alpha and beta diversity for fish species between modified and
unmodified traps. Does using modified traps affect the diversity of fish
caught?**

## Contents

-   [**Reading in datafiles**](#data)

## <a name="data"></a> **Reading in datafiles**

``` r
library(plyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(lubridate)
library(Hmisc)
library(writexl)
library(naniar)
library(Rmisc)
library(phyloseq)
library(stats)
library(lme4)
library(car)
library(vegan)
library(funrar) #make relative fxn 
```

## Read in the data frame that is the output of the QC script and create matrix for phyloseq input.

``` r
# read in excel file
df<- read_excel("data/Fishlandings-cleaned-21052022-May.xlsx") %>% #read in excel file 
  unite(survey_id, Operation_date, fisher_id, sep = "-", remove = FALSE) # creating a new variable called survey id

data_matrix <- df %>% # modified original 
  select(survey_id, scientific_name, number_of_fish) %>% group_by(survey_id, scientific_name) %>% # selecting only desired columns for matrix
  mutate(number_corrected = sum(number_of_fish)) %>% ungroup() %>% # calculating total number of that fish per survey id
  select(-number_of_fish) %>% distinct() %>% # removing original count column and taking out repetitive rows 
  spread(scientific_name, number_corrected) %>% # creating matrix from fish name and number columns
  column_to_rownames(var="survey_id") # moving survey id column to row names 

data_matrix[is.na(data_matrix)] <- 0 #changing NAs to zeros 

meta <- df %>% select(survey_id, trap_type, landing_site) %>% distinct()
```

# Relative abundance

## Making relative abundance dataframe

``` r
otu <- data.matrix(data_matrix) # turning into data matrix 
rel <- make_relative(otu) # making relative abundance 
rel <- as.data.frame(rel) # turning this back into a dataframe 
```

## Combining back with metadata

``` r
rel_meta <- tibble::rownames_to_column(rel, "survey_id")
rel_meta <- merge(meta, rel_meta, by = "survey_id")
rel_meta <- rel_meta %>% filter(!is.na(trap_type)) %>%
  gather(key=scientific_name, value = relabund, 4:154) %>%
  subset(trap_type == "MODIFIED" | trap_type == "UNMODIFIED") %>%
  filter(!is.na(relabund)) %>%
  filter(relabund > 0.4)
```

## Plotting

``` r
rel_meta %>%
  ggplot(., aes(x = landing_site, y = reorder(scientific_name,relabund))) + 
  geom_tile(aes(fill = relabund), color = "grey") +
  ggtitle("Relative abundance per survey ID") +
  scale_fill_distiller(palette = "Blues", direction=100, labels = scales::label_percent(scale=100)) + 
  theme_classic() +
  theme(axis.text.x.bottom = element_text(angle = 90),
        axis.text.y = element_text(colour = 'black', size = 8, face = 'italic')) + 
  facet_grid(~trap_type, scales = "free",) +
  labs(x="Landing Site", fill="Relative Abundance", y="Scientific name") 
```

![](Relative-abundance-analysis_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## PCA on relative abundance

``` r
# pca_rel <- scale(rel)
# meta
# 
# adonis2(pca_rel ~ trap_type*landing_site, data = meta, method='eu')
# 
# pca.results <- prcomp(pca_rel, retx=TRUE) # principal components analysis on sym data
# summary(pca.relabundance.results)
```
