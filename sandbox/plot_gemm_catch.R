devtools::load_all()
short_species_list <- get_short_species_list()
all_catch <- nwfscSurvey::pull_gemm()

rec <- c(
  "Washington Recreational",
  "Oregon Recreational",
  "California Recreational"
)
pot <- c(
  "CS - Pot",
  "CS EM - Pot",
  "LE Sablefish - Pot",
  "LE Fixed Gear DTL - Pot",
  "OA Fixed Gear - Pot"
)
hkl <- c(
  "CS - Hook & Line",
  "Directed P Halibut",
  "Incidental",
  "LE CA Halibut",
  "LE Fixed Gear DTL - Hook & Line",
  "LE Sablefish - Hook & Line",
  "Nearshore",
  "OA CA Halibut",
  "OA Fixed Gear - Hook & Line"
)
trawl <- c(
  "At-Sea Hake CP",
  "At-Sea Hake MSCV",
  "Midwater Hake",
  "Midwater Hake EM",
  "Shoreside Hake",
  "Tribal At-Sea Hake",
  "Combined LE & OA CA Halibut",
  "CS - Bottom and Midwater Trawl",
  "CS - Bottom Trawl",
  "CS EM - Bottom Trawl",
  "Limited Entry Trawl",
  "Midwater Rockfish",
  "Midwater Rockfish EM",
  "Pink Shrimp",
  "Research",
  "Tribal Shoreside"
)

catch <- all_catch |>
  dplyr::mutate(common_name = tolower(species)) |>
  dplyr::filter(
    common_name %in% c(short_species_list[, "name"], "bocaccio rockfish")
  ) |>
  dplyr::mutate(
    common_name = dplyr::case_when(
      common_name == "yellowtail rockfish" &
        grouping == "Minor shelf rockfish (South of 40°10' N. lat.)" ~
        "yellowtail rockfish south",
      common_name == "yellowtail rockfish" &
        grouping != "Minor shelf rockfish (South of 40°10' N. lat.)" ~
        "yellowtail rockfish north",
      common_name == "lingcod" &
        grouping == "Lingcod (South of 40°10' N. lat.)" ~
        "lingcod south",
      common_name == "lingcod" &
        grouping == "Lingcod (North of 40°10' N. lat.)" ~
        "lingcod north",
      common_name == "black rockfish" &
        grouping == "Black rockfish (Washington)" ~
        "black rockfish - washington",
      common_name == "black rockfish" &
        grouping == "Black/blue/deacon rockfish (Oregon)" ~
        "black rockfish - oregon",
      .default = common_name
    )
  ) |>
  dplyr::filter(
    !grouping %in%
      c(
        "Black rockfish (California)",
        "Black rockfish (Coast)",
        "Lingcod (South of 42° N. lat.)",
        "Yellowtail rockfish (Coast)",
        "Black rockfish (North of 46°16' N. lat.)",
        "Black rockfish (South of 46°16' N. lat.)",
        "Lingcod (South of 42° N. lat.)",
        "Lingcod (North of 42° N. lat.)"
      ),
    sector != "Research"
  ) |>
  dplyr::mutate(
    Gear = dplyr::case_when(
      sector %in% c(hkl, pot) ~ "Commercial fixed-gear",
      sector %in% trawl ~ "Commercial trawl-gear",
      sector %in% rec ~ "Recreational",
      .default = "unknown"
    )
  ) |>
  dplyr::group_by(common_name)

fill_vals <- c(
  "Commercial trawl-gear" = "#56B4E9",
  "Commercial fixed-gear" = "#E69F00",
  "Recreational" = "#009E73"
)
species_to_plot <- unique(catch$common_name)
for (a in species_to_plot) {
  p <- ggplot2::ggplot(
    catch |> dplyr::filter(common_name == a),
    ggplot2::aes(
      x = year,
      y = total_discard_with_mort_rates_applied_and_landings_mt,
      fill = Gear
    )
  ) +
    ggplot2::geom_bar(stat = "identity") +
    #nmfspalette::scale_fill_nmfs(palette = "waves", reverse = TRUE) +
    ggplot2::scale_fill_manual(
      values = fill_vals,
      breaks = names(fill_vals),
      drop = FALSE
    ) +
    #ggplot2::scale_fill_viridis_d(begin = 0, end = 0.5) +
    ggplot2::theme_bw() +
    ggplot2::xlab("Year") +
    ggplot2::ylab("Catch (mt)")
  if (a == "bocaccio rockfish") {
    a = "bocaccio"
  }
  ggplot2::ggsave(
    plot = p,
    filename = here::here("plots", "catches", paste0(a, ".png")),
    height = 5,
    width = 7
  )
}


mod_catch <- catch |>
  dplyr::filter(
    common_name %in% c("black rockfish - washington", "black rockfish - oregon")
  ) |>
  dplyr::mutate(
    Area = dplyr::case_when(
      common_name == "black rockfish - washington" ~ "Washington",
      .default = "Oregon"
    ),
    common_name = "black rockfish"
  )
p <- ggplot2::ggplot(
  mod_catch,
  ggplot2::aes(
    x = year,
    y = total_discard_with_mort_rates_applied_and_landings_mt,
    fill = Gear
  )
) +
  ggplot2::geom_bar(stat = "identity") +
  #nmfspalette::scale_fill_nmfs(palette = "waves", reverse = TRUE) +
  ggplot2::scale_fill_manual(
    values = fill_vals,
    breaks = names(fill_vals),
    drop = FALSE
  ) +
  #ggplot2::scale_fill_viridis_d(begin = 0, end = 0.5) +
  ggplot2::theme_bw() +
  ggplot2::xlab("Year") +
  ggplot2::ylab("Catch (mt)") +
  ggplot2::facet_grid("Area")

ggplot2::ggsave(
  plot = p,
  filename = here::here("plots", "catches", "black rockfish.png"),
  height = 7,
  width = 7
)
