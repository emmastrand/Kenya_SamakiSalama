# Fish Landings dataset
Data quality control and analysis for the Fish Landings data within the Kenya_SamakiSalama project.

Written and analyzed by: Emma Strand; University of Rhode Island  
PI: Austin Humphries; University of Rhode Island 

## Data 

- Raw Data = `Fishlandings-data- CC-JM-Clay-IW combined 7-28-2022`: excel spreadsheet collected from Fish Landings folder on Box    
- Cleaned Data = `cleaned-Fishlandings-data- CC-JM-Clay-IW combined 7-28-2022`: output from `QC.Rmd` 

## Scripts 

All scripts created within a R project: `FishLandings`. 

- `QC.Rmd`: reads in Fishing Landings raw dataframe and outputs a cleaned data file   
- `Figures.Rmd`: figures produced from the above datasets  
- `Statistics.Rmd`: statistical analyses on the above datasets  

## Metadata 

Raw data file: `Fishlandings-data_21052022-May_update-cleaning in progress.xlsx`.  

### fishing_operation tab 

 Each row is a fishing excursion (operation).

- `Date`: Date of the fishing trip. Correct format = DD-MM-YYYY 
- `enumerator`: Fisherman's name  
- `landing_site`: 
- `BMU`: 
- `fisher_id`: Fisherman's ID number
- `fisher_phone`: Fisherman's phone number 
- `household_id`: Fisherman's household ID number 
- `trap_type`: Type of trap used for that fishing excursion (operation)
- `total_traps_collected`: Total number of traps for that fishing excursion (operation)
- `date_set_dd_mm_yyyy`: 
- `time_set_24hh:mm`:
- `date_collected_dd_mm_yyyy`:
- `time_collected_24hh:mm`:
- `time_in_water (effort)`:
- `total_weight_kg`: Total weight (kg) of catch for that fishing excursion (operation)
- `take_home_weight_kg`:
- `total_value_KES`:
- `take_home_value_KES`:
- `No. of fishers in crew`: Total number of fishermen in that crew 
- `general_notes`: Notes section for the data collection team

### catch_composition tab 

Each row is a fish species from a fishing excursion. There will be multiple lines per excursion. 

- `Date`: Date of fishing excursion (will match Date column from fishing_operation tab)
- `fisher_id`: Fisherman's ID number (will match the fisher_id column from fishing_operation tab)
- `Kiswahili_name`: Swahili name for that fish species 
- `SPECIES`: Genus and species name for that fish. This is a drop down menu  
- `length_cm`: Length in cm of that fish. This is binned by 5 cm and is a drop-down menu
- `gear type`: Type of gear used to catch that fish 
- `number_of_fish`: Number of fish for that species in that size category from that fishing excursion
- `destination`: Where that group of fish is being transported to
- `notes_picture`: Notes section for the data collection team. And space for an image if they are not able to identify the fish. 

### validation_lists tab

Drop down menu options for SPECIES, destination, gear type, legnth_cm, and Swahili name for the fish. 

### metadata tab

Information from the field team on data collection and data upload notes. 
