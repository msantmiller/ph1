base::rm(list = base::ls())
library(magrittr)

# Load data---------------------------------------------------------------------
get_clean_ph1_data <- function(input_sheet){
  googlesheets4::read_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1pS3TBai5-lu-xYGRImVZ-AZ250Q8lvLwJ7c0hw2EEA4",
    sheet = input_sheet
  ) %>%
    dplyr::rename_all(.funs = stringr::str_to_lower) %>%
    janitor::clean_names()
}
ph1_data_raw <- get_clean_ph1_data(input_sheet = "data")
ph1_definitions <-
  get_clean_ph1_data(input_sheet = "variable_definitions") %>%
  dplyr::filter(status == "Active")

# Create variable subsets-------------------------------------------------------
ph1_variables <- tibble::lst()
for(class_i in c("outcome","predictor")){
  for(status_i in c("clean","raw")){
    ph1_variables[[base::paste(class_i, status_i, sep = "_")]] <-
      ph1_definitions %>%
      dplyr::filter(class == stringr::str_to_sentence(class_i)) %>%
      purrr::pluck(base::paste(status_i, "name", sep = "_"))
  }
}
for(type_i in c("categorical","numeric")){
  for(status_i in c("clean","raw")){
    ph1_variables[[base::paste(type_i, status_i, sep = "_")]] <- 
      ph1_definitions %>%
      dplyr::filter(type == stringr::str_to_sentence(type_i)) %>%
      purrr::pluck(base::paste(status_i, "name", sep = "_"))
    ph1_variables[[base::paste("predictor", type_i, status_i, sep = "_")]] <- 
      ph1_definitions %>%
      dplyr::filter(class == "Predictor", type == stringr::str_to_sentence(type_i)) %>%
      purrr::pluck(base::paste(status_i, "name", sep = "_"))
  }
}
for(status_i in c("clean","raw")){
  ph1_variables[[base::paste("log", status_i, sep = "_")]] <-
    ph1_definitions %>%
    dplyr::filter(transformation == "Log") %>%
    purrr::pluck(base::paste(status_i, "name", sep = "_"))
}
# Check distributions of categorical variables----------------------------------
for(variable_i in ph1_variables[["categorical_raw"]]){
  tally_i <-
    ph1_data_raw %>%
    dplyr::group_by_at(.vars = variable_i) %>%
    dplyr::tally()
  print(variable_i)
  print(tally_i)
  next_i <- base::readline(prompt = "Next?")
}

# Check distributions of numeric variables--------------------------------------
for(variable_i in c(ph1_variables[["numeric_raw"]])){
  plot_i <-
    ggplot2::ggplot(data = ph1_data_raw) +
    ggplot2::geom_density(
      mapping = ggplot2::aes_string(
        x = variable_i,
      ),
      fill = "lightblue",
      color = "blue"
    )
  print(variable_i)
  print(plot_i)
  next_i <- base::readline(prompt = "Next?")
}

# Clean data--------------------------------------------------------------------
ph1_data <-
  ph1_data_raw %>% # Clean categorical data
  dplyr::mutate(
    prev_season_vacc_status = dplyr::case_when(
      prev_season_vacc_status == "UKNOWN" | 
        prev_season_vacc_status == "UNKNOWN" |
        prev_season_vacc_status == "UnkNOWN" ~ NA_character_,
      T ~ prev_season_vacc_status
    )
  ) %>% dplyr::mutate_at(
    .vars = ph1_variables[["categorical_raw"]],
    .funs = base::as.factor
  ) %>% # Clean numeric data
  dplyr::mutate_at(
    .vars = ph1_variables[["log_raw"]],
    .funs = tibble::lst(log = ~dplyr::case_when(
      . == 0 ~ 0,
      T ~ base::log(.)
    ))
  ) %>%
  dplyr::mutate(quantity_minus_efficacy = ph1_fc_tot_igg_log - x1_kd_fc_ph1_ha1_log) %>%
  dplyr::mutate(ph1_delta_tot_igg = ph1_d14_tot_igg - ph1_d0_tot_igg) %>%
  dplyr::mutate(one_over_ph1_d0_tot_igg = 1/ph1_d14_tot_igg)

# Check to see if outcomes are correlated with one another----------------------
stats::cor(
  ph1_data %>%
    dplyr::select(tidyselect::all_of(ph1_variables[["outcome_clean"]])) %>%
    dplyr::filter_all(.vars_predicate = ~base::is.na(.) == F)
)

ph1_pca <- stats::prcomp(
  x = ph1_data %>%
    dplyr::select(tidyselect::all_of(ph1_variables[["outcome_clean"]])) %>%
    dplyr::filter_all(.vars_predicate = ~base::is.na(.) == F),
  scale. = T)
base::summary(ph1_pca)

# Check to see if categorical predictors are correlated with one another--------
ph1_mca <- FactoMineR::MCA(
  X = ph1_data %>%
    dplyr::select(tidyselect::all_of(ph1_variables[["predictor_categorical_clean"]])),
  graph = F
)
base::summary(ph1_mca)

# Check to see if numeric predictors are correlated with one another------------
stats::cor(
  ph1_data %>%
    dplyr::select(tidyselect::all_of(ph1_variables[["predictor_numeric_clean"]])) %>%
    dplyr::filter_all(.vars_predicate = ~base::is.na(.) == F)
)
ph1_pca <- stats::prcomp(
  x = ph1_data %>%
    dplyr::select(tidyselect::all_of(ph1_variables[["predictor_numeric_clean"]])) %>%
    dplyr::filter_all(.vars_predicate = ~base::is.na(.) == F),
  scale. = T)
base::summary(ph1_pca)

# Create outcome variable/predictor scatter plots-------------------------------
for(outcome_i in ph1_variables[["outcome_clean"]]){
  for(predictor_i in ph1_variables[["predictor_clean"]]){
    if(ph1_definitions %>% dplyr::filter(clean_name == predictor_i) %>% purrr::pluck("type") == "Numeric"){
      plot_i <-
        ggplot2::ggplot(data = ph1_data) +
        ggplot2::geom_point(
          mapping = ggplot2::aes_string(
            x = predictor_i,
            y = outcome_i
          ),
          color = "blue"
        )
    }
    if(ph1_definitions %>% dplyr::filter(clean_name == predictor_i) %>% purrr::pluck("type") == "Categorical"){
      plot_i <-
        ggplot2::ggplot(data = ph1_data) +
        ggplot2::geom_density(
          mapping = ggplot2::aes_string(
            x = outcome_i,
            fill = predictor_i
          ),
          alpha = 0.4
        )
    }
    print(paste(outcome_i,"vs.",predictor_i))
    print(plot_i)
    next_i <- base::readline(prompt = "Next?")
  }
}
for(outcome_i in c(ph1_variables[["outcome_clean"]],"quantity_minus_efficacy")){
  model_i <- 
    stats::lm(
      formula = as.formula(
        base::paste(outcome_i,"~",base::paste(ph1_variables[["predictor_clean"]], collapse = " + "))
      ),
      data = ph1_data %>%
        dplyr::select(outcome_i, tidyselect::all_of(ph1_variables[["predictor_clean"]]))
    )
  print(base::summary(model_i))
  print(outcome_i)
  next_i <- base::readline(prompt = "Next?")
}
# Observation: D0 T-Cells are negatively correlated with fold change.
# Is there a ceiling, or is that some of the D0 T-cells actually inhibit the response?

model <- stats::lm(
  formula = x1_kd_fc_ph1_ha1 ~ 
    x1_kd_d0_ph1_ha1_log + ph1_d0_elispot_log +
    x1_kd_d0_ph1_ha1_log*ph1_d0_elispot_log,
  data = ph1_data
)
model <- stats::lm(
  formula = ph1_d14_tot_igg ~ 
    ph1_d0_tot_igg,
  data = ph1_data
)
model <- stats::lm(
  formula = ph1_d14_tot_igg ~ 
    x1_ph1_d0_tot_igg,
  data = ph1_data
)
base::summary(model)
ph1_new_data <- 
  tidyr::expand_grid(
    x1_kd_d0_ph1_ha1_log = base::seq(
      from = 0,
      to = 6,
      by = 0.1
    ),
    ph1_d0_elispot_log = base::seq(
      from = 0,
      to = 10,
      by = 0.1
    )
  )
model_predictions <-
  dplyr::bind_cols(
    ph1_new_data,
    x1_kd_fc_ph1_ha1_log_pred = stats::predict(
      object = model,
      newdata = ph1_new_data
    )
  )

plotly::plot_ly() %>%
  plotly::add_trace(data = model_predictions,
                    x = ~x1_kd_d0_ph1_ha1_log,
                    y = ~ph1_d0_elispot_log,
                    z = ~x1_kd_fc_ph1_ha1_log_pred,
                    type = "mesh3d")

plotly::plot_ly(data = ph1_data,
                x = ~x1_kd_d0_ph1_ha1_log,
                y = ~ph1_d0_elispot_log,
                z = ~x1_kd_fc_ph1_ha1_log,
                marker = list(color = ~x1_kd_d0_ph1_ha1_log, colorscale = c('#683531','#FFE1A1'),
                              showscale = TRUE)) %>%
  plotly::add_markers()
 
# We have more types of data for years 1 and 2. More detailed data. I don't
# know if that will help. If you have missing categories, do you have to
# analyze those 40 subjects that have all the parameters, or can you analyze
# all the data?

# Estimate BART models and identify important variables-------------------------
for(outcome_i in ph1_variables[["outcome_clean"]]){
  ph1_bm_i <-
    bartMachine::bartMachine(
      X = ph1_data %>%
        dplyr::filter_at(.vars = outcome_i,
                         .vars_predicate = ~is.na(.) == F) %>%
        dplyr::select(tidyselect::all_of(ph1_variables[["predictor_clean"]])) %>%
        base::as.data.frame(),
      y = ph1_data %>%
        dplyr::filter_at(.vars = outcome_i,
                         .vars_predicate = ~is.na(.) == F) %>%
        purrr::pluck(outcome_i),
      use_missing_data = T
    )
  ph1_bm_varimport_object_i <-
    ph1_bm_i[[outcome_i]] %>%
    bartMachine::investigate_var_importance(plot = F)
  ph1_bm_varimport_i <-
    ph1_bm_varimport_object_i %>%
    purrr::pluck("avg_var_props") %>%
    base::as.data.frame() %>%
    tibble::rownames_to_column(var =  "variable") %>%
    tibble::as.tibble() %>%
    dplyr::rename(inclusion_proportion = ".") %>%
    dplyr::bind_cols(
      ph1_bm_varimport_i %>%
        purrr::pluck("sd_var_props") %>%
        tibble::as_tibble() %>%
        dplyr::rename(inclusion_proportion_sd = value)
    )
  base::print(ph1_bm_varimport_i)
  next_i <- base::readline(prompt = "Next?")
}

# Graveyard---------------------------------------------------------------------

# ggplot2::ggplot(data = ph1_data) +
#   ggplot2::geom_point(
#     mapping = ggplot2::aes(
#       x = ph1_d0_elispot,
#       y = ph1_fc_tot_igg,
#     )
#   )
# ggplot2::ggplot(data = ph1_data) +
#   ggplot2::geom_density(
#     mapping = ggplot2::aes(
#       x = ph1_fc_tot_igg,
#     ),
#     fill = "lightblue",
#     color = "blue"
#   )
# 
# data_ph1_cleannum <-
#   data_ph1 %>%
#   dplyr::select_if(.predicate = is.numeric) %>%
#   dplyr::select(-birth_year) %>%
#   dplyr::filter_all(.vars_predicate = ~is.na(.) == F) %>%
#   dplyr::mutate_all(.funs = scale)
# 
# data_ph1_clustered <-
#   data_ph1_cleannum %>%
#   dplyr::mutate(
#     cluster_dbscan = base::as.factor(
#       dbscan::dbscan(
#         x = data_ph1 %>%
#           dplyr::select_if(.predicate = is.numeric) %>%
#           dplyr::filter_all(.vars_predicate = ~is.na(.) == F),
#         eps = 1000,
#         minPts = 5
#       )[["cluster"]]
#     ),
#     cluster_kmeans = base::as.factor(
#       stats::kmeans(
#         x = data_ph1 %>%
#           dplyr::select_if(.predicate = is.numeric) %>%
#           dplyr::filter_all(.vars_predicate = ~is.na(.) == F),
#         centers = 4
#       )[["cluster"]]
#     )
#   ) %>%
#   dplyr::bind_cols(
#     stats::prcomp(
#       x = data_ph1_cleannum,
#       scale. = T)[["x"]] %>%
#       tibble::as_tibble() %>%
#       dplyr::select(PC1, PC2) %>%
#       dplyr::rename_all(.funs = stringr::str_to_lower)
#   )
# 
# ggplot2::ggplot(data = data_ph1_clustered) +
#   ggplot2::geom_point(
#     mapping = ggplot2::aes(
#       x = pc1,
#       y = pc2,
#       color = cluster_dbscan
#     )
#   )
# 
# 
# # Graveyard
# # factoextra::fviz_cluster(
# #   cluster_kmeans,
# #   data = data_ph1 %>%
# #     dplyr::select_if(.predicate = is.numeric) %>%
# #     dplyr::filter_all(.vars_predicate = ~is.na(.) == F),
# #   palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
# #   geom = "point",
# #   ellipse.type = "convex", 
# #   ggtheme = ggplot2::theme_bw()
# # )
# # 
# model_mars <- earth::earth(
#   formula = ph1_fc_tot_igg ~ .,
#   data = ph1_data %>%
#     dplyr::select(ph1_fc_tot_igg, tidyselect::all_of(ph1_predictors)) %>%
#     dplyr::filter_all(.vars_predicate = ~is.na(.) == F)
# )
# 
# summary(model_mars) %>% .$coefficients %>% head(10)
# 
# vip::vip(model_mars) + ggplot2::ggtitle("GCV")

