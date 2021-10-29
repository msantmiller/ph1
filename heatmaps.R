rm(list = ls())
library(magrittr)

ph1_data <-
  googlesheets4::read_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1pS3TBai5-lu-xYGRImVZ-AZ250Q8lvLwJ7c0hw2EEA4/edit?usp=sharing",
    sheet = "data"
  ) %>%
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
    -tidyselect::contains("delta")
  )

ph1_data %>%
  dplyr::select(where(is.numeric)) %>%
  cor(use = "complete.obs") %>%
  ggcorrplot::ggcorrplot(lab = TRUE) %>%
  ggplot2::ggsave(
    filename = "output/heatmap.png",
    height = 12, width = 12, units = "in"
  )

ph1_data %>%
  dplyr::select(where(is.numeric)) %>%
  dplyr::select(-tidyselect::contains("affinity")) %>%
  cor(use = "complete.obs") %>%
  ggcorrplot::ggcorrplot(lab = TRUE) %>%
  ggplot2::ggsave(
    filename = "output/heatmap_no_affinity.png",
    height = 10, width = 10, units = "in"
  )