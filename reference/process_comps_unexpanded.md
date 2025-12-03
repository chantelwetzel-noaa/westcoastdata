# Creates a matrix of length or age composition data WITHOUT expansion

Creates a matrix of length or age composition data WITHOUT expansion

## Usage

``` r
process_comps_unexpanded(
  dir = NULL,
  datL,
  lgthBins = 1,
  sex = lifecycle::deprecated(),
  partition = 0,
  fleet = "Enter Fleet",
  ageErr = "NA",
  agelow = -1,
  agehigh = -1,
  month = "Enter Month",
  two_sex_model = TRUE,
  printfolder = "forSS",
  verbose = TRUE
)
```

## Arguments

- dir:

  directory this is where the output files will be saved

- datL:

  the read in length comps by the PullBio.fn function

- lgthBins:

  A vector on length bins or age bins to group the data for Stock
  Synthsis

- sex:

  Deprecated with nwfscSurvey 2.2.1 (February 2023). The function will
  now output female/male and unsexed compositions if present in the data
  frame datL

- partition:

  partition for Stock Synthesis

- fleet:

  fleet number

- ageErr:

  Number of ageing error matrix for Stock Synthesis

- agelow:

  age bin for Stock Synthesis (default value of -1)

- agehigh:

  age bin for Stock Synthesis (default value of -1)

- month:

  month the samples were collected used by Stock Synthesis to determine
  the length/age estimate to compare to.

- two_sex_model:

  Default TRUE. If TRUE and unsexed composition data are present the
  unsexed comps will be output in the format needed for a two-sex model
  in Stock Synthesis.

- printfolder:

  folder where the length comps will be saved

- verbose:

  A logical value specifying whether or not to print out message
  statements to the screen while using this function. The default is to
  print all messages, i.e., `verbose = TRUE`.

## Author

Chantel Wetzel
