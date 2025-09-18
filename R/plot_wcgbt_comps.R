#' Plot NWFSC WCGBTS composition data
#'
#'
#' @param dir add definition
#' @param wcgbt_catch add definition
#' @param wcgbt_bio add definition
#'
#' @author Chantel Wetzel
#' @export
#'
plot_wcgbt_comps <- function(
  dir = here::here(),
  wcgbt_catch,
  wcgbt_bio
) {
  species <- nwfscSurvey::GetSpp.fn(species = nwfscSurvey::GetSppDefault.fn())
  species[
    which(species$common == "rougheye rockfish"),
    "common"
  ] <- "rougheye and blackspotted rockfish"

  species_to_plot <- c(
    unique(species[
      which(species[, "common"] %in% wcgbt_bio$Common_name),
      "common"
    ]),
    "yellowtail rockfish south",
    "yellowtail rockfish north"
  )

  age_species <- wcgbt_bio |>
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

  for (sp in species_to_plot) {
    catch <- wcgbt_catch[wcgbt_catch$Common_name == sp, ]
    bio <- wcgbt_bio[wcgbt_bio$Common_name == sp, ]

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
          "sablefish"
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

    #biomass <- nwfscSurvey::Biomass.fn(
    #	dat = catch,
    #	strat.df = strata,
    #	outputMedian = TRUE)

    #nwfscSurvey::PlotBio.fn(
    #	dir = file.path(dir, "plots"),
    #	dat = biomass,
    #	main = sp
    #)

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
      len.bins <- seq(min_len, max_len - 2 * bin_size, bin_size)

      bio$Sex = "U"
      if (
        sp %in%
          c("blue and deacon rockfish", "rougheye and blackspotted rockfish")
      ) {
        catch <- catch |>
          dplyr::distinct(Trawl_id, .keep_all = TRUE)
        catch <- as.data.frame(catch)
      }

      # Calculate and plot the length-frequencies based on the default strata
      lfs <- nwfscSurvey::SurveyLFs.fn(
        datL = bio,
        datTows = catch,
        strat.df = strata,
        lgthBins = len.bins,
        sex = 0,
        verbose = FALSE
      )

      plot_comps(
        dir = dir,
        add_0_ylim = FALSE,
        add_save_name = sp,
        data = lfs,
        plot = 1
      )

      #if(sum(0:4 %in% bio$Age) >  3 ){
      if (sp %in% age_species$Common_name) {
        age <- as.numeric(age_species[age_species$Common_name == sp, "age_20"])
        max_age_len <- quantile(bio[which(bio$Age == age), "Length_cm"], 0.75)

        find <- ifelse(
          max_age_len %in% len.bins,
          which(len.bins == max_age_len),
          max(which(len.bins < max_age_len)) + 1
        )

        cols_to_keep <-
          c(
            1:which(colnames(lfs) == paste0("U", len.bins[find])),
            which(colnames(lfs) == paste0("U", len.bins[1], ".1")):which(
              colnames(lfs) == paste0("U", len.bins[find], ".1")
            )
          )

        plot_comps(
          dir = dir,
          add_0_ylim = FALSE,
          add_save_name = paste0(sp, "_young_fish_age_", age),
          data = lfs[, cols_to_keep],
          plot = 2
        )
      }
    } # lengths loop
  } # species loop
}
