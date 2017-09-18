Introduction
------------

Working with different partners/institutes/researchers results in a diversity of taxonomic names to define species. This hardens comparison amongst datasets, as in many occasions, aggregation is aimed for or filtering on specific species. By translating all species names to a common taxonomic backbone (ensuring unique ID's for each species name), this can be done. This function supports matching with the GBIF taxonomic backbone.

Aim
---

This small utility provides the functionality to add the species information from the GBIF backbone to any data table (`data.frame`) by requesting this information via the GBIF API. For each match, the corresponding accepted name is looked for. Nevertheless there will always be errors and control is still essential!

Functionality
-------------

The `request_species_information` function extends the matching function provided by [Rgbif](https://github.com/ropensci/rgbif) to be compatible with a `data.frame` data structure.

Consider the example file `example_species`:

``` r
my_data <- read.csv("../data/example_species.csv")
knitr::kable(my_data)
```

| scientificName            | kingdom  | euConcernStatus     |
|:--------------------------|:---------|:--------------------|
| Alopochen aegyptiaca      | Animalia | under consideration |
| Cotoneaster ganghobaensis | Plantae  |                     |
| Cotoneaster hylmoei       | Plantae  |                     |

To add the species information, using the `scientificName` column, and the default fields:

``` r
my_data_update <- request_species_information(my_data)
```

    ## [1] "All column names present"

``` r
knitr::kable(my_data_update)
```

|  usageKey| scientificName                                | rank    | status   | family   | scientificName1           | kingdom  | euConcernStatus     |
|---------:|:----------------------------------------------|:--------|:---------|:---------|:--------------------------|:---------|:--------------------|
|   2498252| Alopochen aegyptiaca (Linnaeus, 1766)         | SPECIES | ACCEPTED | Anatidae | Alopochen aegyptiaca      | Animalia | under consideration |
|   3025989| Cotoneaster ganghobaensis J. Fryer & B. Hylmö | SPECIES | SYNONYM  | Rosaceae | Cotoneaster ganghobaensis | Plantae  |                     |
|   3025758| Cotoneaster hylmoei K.E. Flinck & J. Fryer    | SPECIES | SYNONYM  | Rosaceae | Cotoneaster hylmoei       | Plantae  |                     |

When not satisfied by the default fields provided (`usageKey`, `scientificName`, `rank`, `status`, `family`), you can alter these by the `gbif_terms` argument, for example:

``` r
gbif_terms_to_use <- c('scientificName', 'order')
my_data_update <- request_species_information(my_data, gbif_terms = gbif_terms_to_use)
```

    ## [1] "All column names present"

``` r
knitr::kable(my_data_update)
```

| scientificName                                | order        | scientificName1           | kingdom  | euConcernStatus     |
|:----------------------------------------------|:-------------|:--------------------------|:---------|:--------------------|
| Alopochen aegyptiaca (Linnaeus, 1766)         | Anseriformes | Alopochen aegyptiaca      | Animalia | under consideration |
| Cotoneaster ganghobaensis J. Fryer & B. Hylmö | Rosales      | Cotoneaster ganghobaensis | Plantae  |                     |
| Cotoneaster hylmoei K.E. Flinck & J. Fryer    | Rosales      | Cotoneaster hylmoei       | Plantae  |                     |
