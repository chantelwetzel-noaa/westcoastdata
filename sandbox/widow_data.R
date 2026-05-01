library(pacfintools)
library(ggplot2)
load(
  "C:/Users/chantel.wetzel/Documents/github/prioritization/westcoastdata/data-raw/2026/PacFIN.PTRL--LCOD--YTRK--RDBD--DSRK--WDOW--BCAC--BLCK--SSPN--LSPN.bds.14.Apr.2026.RData"
)
load(
  "C:/Assessments/pacfin/westcoastdata/2026-04-30/PacFIN.PTRL--LCOD--YTRK--RDBD--DSRK--WDOW--BCAC--BLCK--SSPN--LSPN.bds.30.Apr.2026.RData"
)

Pdata <- cleanPacFIN(
  Pdata = bds.pacfin |>
    dplyr::filter(SAMPLE_YEAR >= 2000),
  clean = TRUE,
  verbose = TRUE
)

widow = Pdata |>
  dplyr::filter(
    PACFIN_SPECIES_COMMON_NAME == "WIDOW ROCKFISH",
    #AGENCY_CODE == "C",
    year >= 2019,
    fleet == "TWL",
    !is.na(Age)
  ) |>
  dplyr::mutate(
    gear_type = dplyr::case_when(
      PACFIN_GEAR_NAME %in% c("CP-MTRAWL", "MID-TRAWL") ~ "midwater trawl",
      .default = "trawl"
    )
  )

widow_all_ca = Pdata |>
  dplyr::filter(
    PACFIN_SPECIES_COMMON_NAME == "WIDOW ROCKFISH",
    AGENCY_CODE == "C",
    year >= 2019,
    !is.na(Age)
  )
widow_all_ca |>
  dplyr::group_by(geargroup) |>
  dplyr::summarise(
    n = sum(!is.na(Age))
  )


# From 2019+ there are 29 ages from HKL gear, 1 age from TLS gear, and 1438 ages from TWL gear.
# CA: Fort Bragg (478), Eureka (812), and Moss Landing (151)
# OR: Astoria (1481), Coos Bay (43), Brookings (7), and Newport (849)
# WA: Westport (3835), Neah Bay (33), and Bellingham (298)

#   CA   OR   WA
# 1438 2380 4174
#        CA   OR   WA
# 2019    3  400 1030
# 2020   83  300  532
# 2021  261  298  750
# 2022  686  508  683
# 2023  314  474  619
# 2024   91  400  560

library(ggplot2)
ggplot(widow |> dplyr::mutate(Count = 1), aes(x = Age, y = Count)) +
  geom_bar(stat = 'identity') +
  theme_bw() +
  facet_grid(c("state", "gear_type"), scales = "free_y")

ggplot(widow, aes(x = Age, fill = state)) +
  geom_density(alpha = 0.40) +
  theme_bw() +
  facet_grid("gear_type")

ggplot(
  widow |> dplyr::filter(year %in% 2021:2023),
  aes(x = Age, fill = state)
) +
  geom_density(alpha = 0.40) +
  theme_bw() +
  facet_grid(c("year", "gear_type"))

ggplot(widow, aes(x = as.factor(state), y = Age)) +
  geom_boxplot() +
  theme_bw() +
  facet_grid("gear_type")

ggplot(
  widow |> dplyr::filter(year %in% 2021:2023),
  aes(x = as.factor(state), y = Age)
) +
  geom_boxplot() +
  theme_bw() +
  facet_grid("year")

aggregate(Age ~ state + gear_type, widow, quantile)
#  state Age.0% Age.25% Age.50% Age.75% Age.100%
#     CA      5       9      13      17       37
#     OR      3       7       8      14       39
#     WA      1       6       7      12       37
aggregate(Age ~ state, widow |> dplyr::filter(year %in% 2021:2023), quantile)
#  state Age.0% Age.25% Age.50% Age.75% Age.100%
#     CA      5       9      12      16       37
#     OR      4       6       7      13       39
#     WA      1       6       6       9       37

aggregate(Age ~ state, widow, mean)
