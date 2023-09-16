# POC for ED department at Patria



# 1.0.0 -------------------------------------------------------------------

library(sumots2)
library(tidyverse)


# 1.1.0 Data ----
file_path   <- "00_data/REPORTE INGRESO DE PACIENTES 21_22_23.xlsx"
sheet_names <- readxl::excel_sheets(file_path)


data_tbl <- map(sheet_names, ~ readxl::read_excel(file_path, sheet = .x)) %>% 
  bind_rows() %>% 
  set_names("id", "date")



# 2.0.0 Wrangling ---------------------------------------------------------

data_prepared_tbl <- data_tbl %>%
  mutate(
    date = str_replace(date, " AM", "AM"),
    date = str_replace(date, " PM", "PM"),
    date = dmy_hm(date),
    date = floor_date(date, "hour")
    ) %>% 
  group_by(date) %>% 
  summarise(outcome = n_distinct(id)) %>% 
  pad_by_time(.date_var = date, .pad_value = 0) %>% 
  mutate(id = "id")




# 3.0.0 Modelling ---------------------------------------------------------
ensemble_ls <- list(
  "ensemble_1" = c("tbats", "snaive"),
  "ensemble_2" = c("deepar", "stl_ets"),
  "ensemble_3" = c("deepar", "tbats"),
  "ensemble_4" = c("deepar", "tbats", "snaive", "stl_ets")
)

fc_ls <- sumo_forecast(
  raw_data               = data_prepared_tbl %>% filter(date >= "2022-01-01"),
  frequency              = "hourly",
  horizon                = 168,
  transformation         = "log1p",
  anomaly                = FALSE,
  use_covid              = TRUE,
  lags                   = 168,
  lags_seasonal          = 168,
  ml_models              = c("xgboost", "catboost", "lightgbm"),
  method                 = c("recursive", "seasonal"),
  univar_models          = c("arima", "ets", "thief", "tbats", "stl_arima", "stl_ets", "prophet", "naive"),
  slice_limit            = 5,
  skip                   = 84,
  azure_container        = "dev",
  client_name            = "patria",
  stability_measure      = "sd",
  n_models_for_stability = 2,
  suffix                 = "168h",
  ceiling                = TRUE,
  ensemble               = ensemble_ls,
  parallel = FALSE
  )


fc_ls$resample_forecasts %>% 
  filter(resample == "resample_1") %>% 
  plot_modeltime_forecast(.facet_ncol = 3)
