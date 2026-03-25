# Wrapper function to pull all species catch and biological data from the NWFSC data wharehouse.

Wrapper function to pull all species catch and biological data from the
NWFSC data wharehouse.

## Usage

``` r
pull_wcgbts(
  dir = dir,
  species,
  survey = "NWFSC.Combo",
  load = TRUE,
  convert = TRUE,
  verbose = FALSE
)
```

## Arguments

- dir:

  Directory location to save the cleaned data frame

- species:

  A list of species names created by the get_species_list function

- survey:

  A character entry from one of the following options that specifies
  which survey to pull the data for:

  - Triennial,

  - AFSC.Slope,

  - NWFSC.Combo,

  - NWFSC.Slope,

  - NWFSC.Shelf

- load:

  Option to load in existing Rdata files with the catch and biological
  data.

- convert:

  A logical as to whether to convert the data column names to the old
  names as currently used in the nwfscSurvey processing code

- verbose:

  A logical value specifying whether or not to print out message
  statements to the screen while using this function. The default is to
  print all messages, i.e., `verbose = TRUE`.

## Author

Chantel Wetzel
