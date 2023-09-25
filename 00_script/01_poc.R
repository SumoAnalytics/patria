# POC for ED department at Patria



# 1.0.0 -------------------------------------------------------------------

library(sumots2)
library(tidyverse)
library(timetk)


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

# By shift
shift_data_tbl <- data_prepared_tbl %>% 
  mutate(
    hour = hour(date),
    shift = 
      case_when(hour %in% c(8:15) ~ "morning",
                hour %in% c(0:7)  ~ "night",
                TRUE ~ "afternoon"),
    date_day = date(date)
  ) %>% 
  group_by(shift, date_day) %>% 
  mutate(min_date = min(date)) %>% 
  group_by(min_date) %>% 
  summarise(outcome = sum(outcome)) %>% 
  mutate(id = "id") %>% 
  rename("date" = "min_date")


# Daily
data_daily_prepared_tbl <- data_prepared_tbl %>% 
  mutate(date = date(floor_date(date, "day"))) %>% 
  group_by(date) %>% 
  summarise(outcome = sum(outcome)) %>% 
  mutate(id = "id")


# 3.0.0 Modelling ---------------------------------------------------------

# 3.1.0 Hourly data ----
ensemble_ls <- list(
  "ensemble_1" = c("deepar", "prophet"),
  "ensemble_2" = c("deepar", "stl_ets"),
  "ensemble_3" = c("deepar", "tbats"),
  "ensemble_4" = c("deepar", "tbats", "stl_ets", "prophet"),
  "ensemble_5" = c("deepar", "prophet", "tbats")
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
  ml_models              = NULL, #c("catboost", "lightgbm", "xgboost", "cubist", "ranger"),
  method                 = c("recursive"),
  univar_models          = c("tbats", "stl_ets", "prophet"),
  deep_learning_models   = "deepar",
  slice_limit            = 5,
  skip                   = 168,
  azure_container        = "dev",
  client_name            = "patria",
  stability_measure      = "sd",
  n_models_for_stability = 1,
  suffix                 = "168h",
  ceiling                = TRUE,
  ensemble               = ensemble_ls,
  parallel               = FALSE,
  azure_key              = Sys.getenv("AZURE_DEV"),
  reuse_data_prepared = TRUE,
  reuse_ml_resamples = TRUE,
  reuse_univar_resamples = TRUE
  )


fc_ls$resample_forecasts %>% 
  #group_by(resample) %>% 
  filter(resample == "resample_1") %>% 
  plot_modeltime_forecast(.facet_ncol = 3)


fc_ls$best_per_id


fc_ls$resample_accuracy %>%
  group_by(name) %>% 
  summarise(
    mean_rmse = mean(rmse),
    mean_mae = mean(mae),
    mean_mase = mean(mase)
  ) %>% 
  arrange(mean_rmse)


fc_ls$final_forecast %>% 
  plot_modeltime_forecast()


# Accuracy by sum

accuracy_per_shift_tbl <- fc_ls$resample_forecasts %>%
  mutate(
    hour = hour(.index),
    shift = 
      case_when(hour > 8 & hour <= 16 ~ "morning",
                hour > 0 & hour <= 8  ~ "night",
                TRUE ~ "day")
  ) %>% 
  select(-c(.model_id, .key, id)) %>% 
  pivot_wider(names_from = .model_desc, values_from = .value) %>% 
  drop_na() %>% 
  pivot_longer(cols = -(.index:ACTUAL)) %>% 
  group_by(name, shift) %>% 
  summarise(
    forecast = sum(value),
    actual = sum(ACTUAL)
  ) %>% 
  mutate(accuracy = 1 - abs(forecast - actual) / actual)

accuracy_per_shift_tbl %>% 
  group_by(name) %>% 
  summarise(
    mean_accuracy = mean(accuracy)
  ) %>% 
  arrange(mean_accuracy)


accuracy_per_shift_tbl %>% 
  filter(name == "ensemble-deepar-stl_ets")


# 3.2.0 Hourly by shift ---------------------------------------------------
ensemble_ls <- list(
  "ensemble_1" = c("tbats", "naive"),
  "ensemble_2" = c("deepar", "stl_ets"),
  "ensemble_3" = c("deepar", "tbats"),
  "ensemble_4" = c("deepar", "tbats", "naive", "stl_ets"),
  "ensemble_5" = c("seasonal_lightgbm_default", "prophet", "naive")
)

fc_ls <- sumo_forecast(
  raw_data               = shift_data_tbl,
  frequency              = "hourly",
  horizon                = 21,
  transformation         = "log1p",
  anomaly                = FALSE,
  use_covid              = TRUE,
  lags                   = 21,
  lags_seasonal          = 21,
  ml_models              = c("catboost", "lightgbm", "xgboost", "cubist", "ranger"),
  method                 = c("recursive", "seasonal"),
  univar_models          = c("arima", "ets", "thief", "tbats", "stl_arima", "stl_ets", "prophet", "naive"),
  slice_limit            = 5,
  skip                   = 21,
  deep_learning_models  = NULL,
  azure_container        = "dev",
  client_name            = "patria",
  stability_measure      = "sd",
  n_models_for_stability = 1,
  suffix                 = "shift8h",
  ceiling                = TRUE,
  ensemble               = NULL,
  parallel               = FALSE,
  azure_key              = Sys.getenv("AZURE_DEV")
)


fc_ls$best_per_id


fc_ls$resample_forecasts %>% 
  filter(resample == "resample_5") %>% 
  plot_modeltime_forecast()



# test
test_data_tbl <- tail(shift_data_tbl, 21)
train_data_tbl <- shift_data_tbl %>% filter(!date %in% test_data_tbl$date)

stl_arima <- exp_smoothing() %>% 
  set_engine("ets") %>% 
  fit(outcome ~ date, data = train_data_tbl) %>% 
  modeltime_table()

stl_arima %>% 
  modeltime_forecast(
    new_data = test_data_tbl,
    actual_data = shift_data_tbl
  ) %>% 
  plot_modeltime_forecast()


ds_mtbl <- deep_state(mode = "regression", id = "id", freq = "D", prediction_length = 21, lookback_length = 63) %>% 
  set_engine("gluonts_deepstate") %>% 
  fit(outcome ~ ., data = train_data_tbl) %>% 
  modeltime_table()

ds_mtbl %>% 
  modeltime_forecast(
    new_data = test_data_tbl,
    actual_data = shift_data_tbl
  ) %>% 
  plot_modeltime_forecast()


# 3.3.0 Daily data --------------------------------------------------------

ensemble_ls <- list(
  "ensemble_1" = c("tbats", "naive"),
  "ensemble_2" = c("deepar", "stl_ets"),
  "ensemble_3" = c("deepar", "tbats"),
  "ensemble_4" = c("deepar", "tbats", "naive", "stl_ets"),
  "ensemble_5" = c("seasonal_lightgbm_default", "prophet", "naive")
)

fc_daily_ls <- sumo_forecast(
  raw_data               = data_daily_prepared_tbl,
  frequency              = "daily",
  horizon                = 30,
  transformation         = "log1p",
  anomaly                = FALSE,
  use_covid              = TRUE,
  lags                   = 30,
  lags_seasonal          = 30,
  country                = "Colombia",
  ml_models              = c("catboost", "lightgbm", "xgboost", "cubist", "ranger"),
  method                 = c("recursive", "seasonal"),
  univar_models          = c("arima", "ets", "thief", "tbats", "stl_arima", "stl_ets", "prophet", "naive"),
  slice_limit            = 5,
  skip                   = 15,
  azure_container        = "dev",
  client_name            = "patria",
  stability_measure      = "sd",
  n_models_for_stability = 1,
  suffix                 = "30d",
  ceiling                = TRUE,
  ensemble               = ensemble_ls,
  parallel               = FALSE,
  azure_key              = Sys.getenv("AZURE_DEV")
)

fc_daily_ls$resample_forecasts %>% 
  filter(resample == "resample_5") %>% 
  plot_modeltime_forecast()




# 4.0.0 NHITS & TIMEGPT ---------------------------------------------------

# 4.1.0 TIMEGPT ----
library(httr)
library(tidyverse)
library(jsonlite)
library(tidymodels)


get_timegpt_fc <- function(data, horizon, freq, finetune_steps = 0) {
  
  # prepare data
  splits <- data_prepared_tbl %>% filter(date >= "2023-01-01") %>% timetk::time_series_split(date_var = date, assess = horizon, cumulative = TRUE, slice = 1)
  
  train_tbl <- splits %>% training()
  
  # Convert to correct format
  formatted_date <- format(train_tbl$date, "%Y-%m-%d %H:%M:%S")
  formatted_data <- toJSON(setNames(object = as.list(train_tbl$outcome), nm = formatted_date))
  
  # Set other parameters
  fh <- horizon
  freq <- freq
  
  # Create final data list
  data_ls <- list(
    fh = fh,
    y = fromJSON(formatted_data),
    freq = freq,
    clean_ex_first = FALSE,
    finetune_steps = finetune_steps
  )
  
  json_payload <- toJSON(data_ls, auto_unbox = TRUE)
  
  url      <- "https://dashboard.nixtla.io/api/timegpt"
  encode   <- "json"
  response <- VERB("POST", url, body = json_payload, add_headers('authorization' = 'Bearer vKv5YIogD1f6LbL7EJq0gHsNPtkHM1SRngj1zgyzQPeZf6Kwj67XjClHS93qU8sH1hK0agLXkFDOje4ZkBEfikzRJ60VMkYgtZ4ClJs8uM5Vmp1XY6bU3eHggYurXTZO4zlypMXQNhG4bydWTFhPpgPxld8ml5qzqTTZugWjc7mueUHbuNnBJJREDrzYB8p5tmKXojuDPdibYU6FqGNXJQHeFT2y0zU2ZxVQ547Z3w0o61g68Sk4nna72Uspgipf'), content_type("application/json"), accept("application/json"), encode = encode)
  
  fc_content_ls <- content(response, "text")
  
  # Prepare forecast data
  tibble(
    date    = fromJSON(fc_content_ls)$data$timestamp,
    outcome = fromJSON(fc_content_ls)$data$value,
    what    = "forecast"
  )
  
}


timegpt_fc_tbl <- get_timegpt_fc(
  data    = shift_data_tbl,
  horizon = 21,
  freq = "H",
  finetune_steps = 0
  )

fc_full_tbl <- shift_data_tbl %>% 
  select(-id) %>% 
  mutate(what = "historical") %>% 
  bind_rows(timegpt_fc_tbl %>% 
              mutate(date = ymd_hms(date)))


p <- fc_full_tbl %>% 
  ggplot(aes(x = date, y = outcome, col = what)) +
  geom_line()

plotly::ggplotly(p)


# 4.2.0 NHITS -------------------------------------------------------------

library(tidyverse)
library(tidymodels)
source("00_script/nixtla_fcns.R")

splits <- shift_data_tbl %>% 
  rename("ds" = "date") %>% 
  rename("y" = "outcome") %>% 
  select(-id) %>% 
  mutate(unique_id = 1) %>% 
  time_series_split(date_var = ds, assess = 21, cumulative = TRUE, slice = 1)

train_df <- training(splits)
test_df  <- testing(splits)

horizon <- length(test_df$ds)
input_size <- as.integer(2*horizon)


# model parameterization  
models <- c(NHITS(input_size= input_size, h=horizon, max_steps=50))

# stage models
nf_set <- neural_model_setup(models = models, frequency = "H")  

# fit models   
nf_fit <- neural_model_fit(model_setup = nf_set, df = train_df)

# make predictions
nf_preds <- neural_model_predict(model_setup = nf_set, model_fit = nf_fit)

p <- data_prepared_tbl %>% 
  rename("ds" = "date") %>% 
  rename("y" = "outcome") %>%
  select(-id) %>% 
  mutate(unique_id = 1) %>% 
  left_join(nf_preds) %>% 
  pivot_longer(-c(ds, unique_id), names_to = "model", values_to = "value") |>
  ggplot() + 
  geom_line(aes(x = ds, y = value, color = model))

plotly::ggplotly(p)
