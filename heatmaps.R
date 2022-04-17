rm(list = ls())
library(magrittr)

ph1_data <-
  googlesheets4::read_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1pS3TBai5-lu-xYGRImVZ-AZ250Q8lvLwJ7c0hw2EEA4/edit?usp=sharing",
    sheet = "data"
  ) %>%
  dplyr::rename_with(stringr::str_to_lower) %>%
  janitor::clean_names() %>%
  dplyr::mutate(
    age = 2021L - yob
  ) %>%
  dplyr::mutate(
    vaccine_flublok = (vaccine == "Flublok"),
    vaccine_flucelvax = (vaccine == "Flucelvax"),
    vaccine_fluzone = (vaccine == "Fluzone"),
    vacc_status = (vacc_status == "Y"),
    male = (gender == "Male")
  ) %>%
  dplyr::mutate(dplyr::across(
    .cols = where(is.logical),
    .fns = as.numeric
  )) %>%
  dplyr::select(
    -donor,
    -year,
    -race,
    -yob,
    -tidyselect::contains("delta"),
    -tidyselect::contains("affinity"),
    -tidyselect::contains("ny")
  ) %>%
  dplyr::select(
    tidyselect::contains("_fc_"),
    tidyselect::contains("d0"),
    tidyselect::starts_with("vaccine_"),
    vacc_status,
    male,
    age
  ) %>%
  dplyr::rename(
    `Vaccine status` = vacc_status,
  ) %>%
  dplyr::rename_with(
    .cols = tidyselect::everything(),
    .fn = function(.x){
      .x %>%
        stringr::str_remove_all(pattern = "vaccine_") %>%
        stringr::str_replace_all(pattern = "_", replacement = " ") %>%
        stringr::str_to_sentence() %>%
        stringr::str_replace_all(pattern = "Ph1", replacement = "pH1") %>%
        stringr::str_replace_all(pattern = "fc", replacement = "FC") %>%
        stringr::str_replace_all(pattern = "ifng", replacement = "IFNg") %>%
        stringr::str_replace_all(pattern = "ifng", replacement = "IFNg") %>%
        stringr::str_replace_all(pattern = "elisa", replacement = "ELISA") %>%
        stringr::str_replace_all(pattern = "elispot", replacement = "ELISpot")
    }
  )

# ph1_data %>%
#   dplyr::select(where(is.numeric)) %>%
#   cor(use = "complete.obs") %>%
#   ggcorrplot::ggcorrplot(lab = TRUE) %>%
#   ggplot2::ggsave(
#     filename = "output/heatmap.png",
#     height = 12, width = 12, units = "in"
#   )

ph1_data %>%
  dplyr::select(where(is.numeric)) %>%
  cor(use = "complete.obs") %>%
  ggcorrplot::ggcorrplot(lab = FALSE) %>%
  ggplot2::ggsave(
    filename = "output/heatmap_no_affinity.png",
    height = 10, width = 10, units = "in"
  )