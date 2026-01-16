#' Plot NWFSC WCGBTS composition data
#'
#'
#' @param dir add definition
#' @param wcgbt_catch_filtered The filtered and cleaned data returned from [clean_wcgbt_catch()]
#' @param wcgbt_bio_filtered The filtered and cleaned data returned from [clean_wcgbt_bio()]
#'
#' @author Chantel Wetzel
#' @export
#'
plot_wcgbt_comps <- function(
  dir = here::here("plots", "wcgbts_comps"),
  wcgbt_catch_filtered,
  wcgbt_bio_filtered,
  verbose = TRUE
) {
  nwfscSurvey::check_dir(dir = dir)
  # Check frequency of observations
  obs_rate <- wcgbt_catch_filtered |>
    dplyr::group_by(Common_name) |>
    dplyr::summarise(
      n = sum(positive_tow)
    ) |>
    dplyr::filter(n > 300) |>
    dplyr::ungroup()
  # Determine the species to process and plot
  bio_filterred <- wcgbt_bio_filtered |>
    dplyr::filter(Common_name %in% obs_rate$Common_name)
  species_to_plot <- unique(bio_filterred[, "Common_name"])
  # Check that each species is in the catch data
  catch_species <- unique(wcgbt_catch_filtered[, "Common_name"])
  missing <- species_to_plot[!species_to_plot %in% catch_species]
  if (length(missing) > 0) {
    cli::cli_inform(glue::glue(
      "The following species are in the biological data but not in 
                    the catch data: {missing}"
    ))
    #wcgbt_catch_filtered <- wcgbt_catch_filtered |>
    #  dplyr::filter(Common_name != missing)
  }

  # Create dataframe with information about the youngest ages the survey
  # observes by species and the number of observations
  age_species <- wcgbt_bio_filtered |>
    dplyr::filter(!is.na(Age)) |>
    dplyr::group_by(Common_name) |>
    dplyr::summarize(
      age_10 = quantile(Age, 0.10, na.rm = TRUE),
      age_20 = quantile(Age, 0.20, na.rm = TRUE),
      n10 = sum(Age <= age_10),
      n20 = sum(Age <= age_20)
    ) |>
    dplyr::filter(age_20 <= 5) |>
    dplyr::filter(n20 >= 500)

  test <- species_to_plot[which(species_to_plot != "big skate")]
  for (sp in test) {
    catch <- wcgbt_catch_filtered[wcgbt_catch_filtered$Common_name == sp, ]
    if (length(unique(catch[, "Trawl_id"])) != dim(catch)[1]) {
      catch <- nwfscSurvey::combine_tows(data = catch)
    }
    bio <- wcgbt_bio_filtered[wcgbt_bio_filtered$Common_name == sp, ]

    if (sp %in% c("yellowtail rockfish north", "yellowtail rockfish south")) {
      catch$Common_name <- "yellowtail rockfish"
      bio$Common_name <- "yellowtail rockfish"
    }
    if (sp %in% c("lingcod north", "lingcod south")) {
      catch$Common_name <- "lingcod"
      bio$Common_name <- "lingcod"
    }
    # Create a generic strata
    strata <- nwfscSurvey::CreateStrataDF.fn(
      names = c(
        "shallow_wa",
        "shallow_or",
        "shallow_ca",
        "deep_wa",
        "deep_or",
        "deep_ca"
      ),
      depths.shallow = c(55, 55, 55, 183, 183, 183),
      depths.deep = c(183, 183, 183, 549, 549, 549),
      lats.south = c(46.0, 42.0, 32.0, 46.0, 42.0, 32.0),
      lats.north = c(49.0, 46.0, 42.0, 49.0, 46.0, 42.0)
    )

    if (
      sp %in%
        c(
          "Dover sole",
          "longspine thornyhead",
          "shortspine thornyhead",
          "sablefish",
          "longnose skate"
        )
    ) {
      strata <- nwfscSurvey::CreateStrataDF.fn(
        names = c(
          "shallow_wa",
          "shallow_or",
          "shallow_ca",
          "medium_wa",
          "medium_or",
          "medium_ca",
          "deep_wa",
          "deep_or",
          "deep_ca"
        ),
        depths.shallow = c(55, 55, 55, 183, 183, 183, 549, 549, 549),
        depths.deep = c(183, 183, 183, 549, 549, 549, 1280, 1280, 1280),
        lats.south = c(46.0, 42.0, 32.0, 46.0, 42.0, 32.0, 46.0, 42.0, 32.0),
        lats.north = c(49.0, 46.0, 42.0, 49.0, 46.0, 42.0, 49.0, 46.0, 42.0)
      )
    }

    if (
      sp %in%
        c(
          "splitnose rockfish",
          "darkblotched rockfish",
          "aurora rockfish",
          "rex sole"
        )
    ) {
      strata <- nwfscSurvey::CreateStrataDF.fn(
        names = c(
          "shallow_wa",
          "shallow_or",
          "shallow_ca",
          "medium_wa",
          "medium_or",
          "medium_ca",
          "deep_wa",
          "deep_or",
          "deep_ca"
        ),
        depths.shallow = c(55, 55, 55, 183, 183, 183, 549, 549, 549),
        depths.deep = c(183, 183, 183, 549, 549, 549, 700, 700, 700),
        lats.south = c(46.0, 42.0, 32.0, 46.0, 42.0, 32.0, 46.0, 42.0, 32.0),
        lats.north = c(49.0, 46.0, 42.0, 49.0, 46.0, 42.0, 49.0, 46.0, 42.0)
      )
    }

    ## Calculate the observations by length and age
    if (length(bio$Length_cm) > 0) {
      ind <- !is.na(bio$Length_cm)
      min_len <- ifelse(
        floor(min(bio$Length_cm[ind])) > 10,
        floor(min(bio$Length_cm[ind])),
        10
      )
      max_len <- floor(max(bio$Length_cm[ind]))
      bin_size <- ifelse(max_len - min_len > 60, 4, 2)
      len_bins <- seq(min_len, max_len - 2 * bin_size, bin_size)
      bio$Sex = "U"

      # Calculate and plot the length-frequencies based on the default strata
      lfs <- nwfscSurvey::get_expanded_comps(
        bio_data = bio,
        catch_data = catch,
        comp_bins = len_bins,
        strata = strata,
        comp_column_name = "length_cm",
        output = "full_expansion_ss3_format",
        two_sex_comps = FALSE,
        input_n_method = "tows"
      )

      nwfscSurvey::plot_comps(
        dir = dir,
        add_0_ylim = FALSE,
        add_save_name = sp,
        data = lfs,
        plot = 1
      )

      if (sp %in% age_species$Common_name) {
        age <- as.numeric(age_species[age_species$Common_name == sp, "age_20"])
        max_age_len <- quantile(bio[which(bio$Age == age), "Length_cm"], 0.75)

        find <- ifelse(
          max_age_len %in% len_bins,
          which(len_bins == max_age_len),
          max(which(len_bins < max_age_len)) + 1
        )

        cols_to_keep <-
          c(
            1:which(colnames(lfs$unsexed) == paste0("u", len_bins[find])),
            which(colnames(lfs$unsexed) == paste0("u", len_bins[1])):which(
              colnames(lfs$unsexed) == paste0("u", len_bins[find])
            )
          )

        nwfscSurvey::plot_comps(
          dir = dir,
          add_0_ylim = FALSE,
          add_save_name = paste0(sp, "_young_fish_age_", age),
          data = lfs$unsexed[, cols_to_keep],
          plot = 2
        )
      }
    } # lengths loop
  } # species loop
}
