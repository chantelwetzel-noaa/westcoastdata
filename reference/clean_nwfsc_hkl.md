# Function to remove all unneeded species data and to format column names as needed for the NWFS HKL data.

Function to remove all unneeded species data and to format column names
as needed for the NWFS HKL data.

## Usage

``` r
clean_nwfsc_hkl(dir = here::here("data-processed"), data, species)
```

## Arguments

- dir:

  Directory location to save the cleaned data frame

- data:

  Data frame of NWFSC HKL data

- species:

  A list of species names created by the get_species_list function

## Author

Chantel Wetzel
