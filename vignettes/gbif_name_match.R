## ------------------------------------------------------------------------
library(inborutils)

## ------------------------------------------------------------------------
knitr::kable(species_example)

## ----warning=FALSE-------------------------------------------------------
my_data_update <- gbif_species_name_match(species_example, 
                                          name_col = "scientificName")

## ------------------------------------------------------------------------
knitr::kable(my_data_update)

## ----message=FALSE, warning=FALSE----------------------------------------
gbif_terms_to_use <- c('scientificName', 'order')
my_data_update <- gbif_species_name_match(species_example, 
                                              name_col = "scientificName" , 
                                              gbif_terms = gbif_terms_to_use)

## ------------------------------------------------------------------------
knitr::kable(my_data_update)

