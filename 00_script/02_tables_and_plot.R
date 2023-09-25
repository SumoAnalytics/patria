# Plot for POC


# 1.0.0 SETUP -------------------------------------------------------------

library(tidyverse)
library(AzureStor)
library(modeltime)

con <- sumots2::sumo_connect_azure("dev")

list_storage_files(con$container_dev_fc_files) %>% 
  filter(str_detect(name, "patria"),
         str_detect(name, "best"),
         str_detect(name, "30d"))


fc_resample_tbl <- storage_read_csv(
  con$container_dev_fc_files,
  "patria/2023/09/19/tscv-forecast_168h.csv"
  )

fc_accuracy_tbl <- storage_read_csv(
  con$container_dev_fc_files,
  "patria/2023/09/18/tscv-accuracy_168h.csv"
)



# 2.0.0 Accuracy by shift -------------------------------------------------

fc_resample_tbl %>% 
  mutate(
    hour = hour(.index),
    shift = 
      case_when(hour %in% c(8:15) ~ "day",
                hour %in% c(0:7)  ~ "night",
                TRUE ~ "afternoon")
  ) %>% 
  select(-c(.model_id, .key, id))





#
# scrap -------------------------------------------------------------------


accuracy_by_shift_tbl <- fc_resample_tbl %>% 
  mutate(
    hour = hour(.index),
    shift = 
      case_when(hour %in% c(8:15) ~ "day",
                hour %in% c(0:7)  ~ "night",
                TRUE ~ "afternoon")
  ) %>% 
  select(-c(.model_id, .key, id)) %>% 
  pivot_wider(names_from = .model_desc, values_from = .value) %>% 
  drop_na() %>% 
  pivot_longer(cols = -(.index:ACTUAL)) %>% 
  group_by(name, shift, resample) %>% 
  summarise(
    forecast = sum(value),
    actual = sum(ACTUAL)
  ) %>% 
  mutate(accuracy = 1 - abs(forecast - actual) / actual)


accuracy_by_shift_tbl %>% 
  group_by(name) %>% 
  summarise(
    mean_accuracy = mean(accuracy)
  ) %>% 
  arrange(mean_accuracy)  


# Best model by shift
n_rsamples <- n_distinct(fc_resample_tbl$resample)

fc_shift_list <- list()

for (i in 1:n_rsamples) {
  
  resampe_use <- paste0("resample_", i)
  
  fc_shift_tbl <- fc_resample_tbl %>% 
    filter(.model_desc %in% c("ACTUAL", "ensemble-deepar-prophet")) %>% 
    mutate(
      hour = hour(.index),
      shift = 
        case_when(hour %in% c(8:15) ~ "day",
                  hour %in% c(0:7)  ~ "night",
                  TRUE ~ "afternoon"),
      date = date(.index)
    ) %>% 
    filter(resample == resampe_use) %>% 
    select(-resample) %>% 
    distinct() %>% 
    group_by(.model_desc, shift, date) %>% 
    mutate(min_index = min(.index)) %>% 
    group_by(.model_desc, min_index, shift) %>% 
    summarise(
      .value = round(sum(.value)),
      n_obs = n()
    ) %>% 
    ungroup() %>% 
    filter(n_obs == 8)
  
  
  fc_shift_tbl %>% 
    pivot_wider(names_from = .model_desc, values_from = .value) %>% 
    drop_na() %>% 
    select(-n_obs) %>% 
    set_names("date", "shift", "actual", "forecast") %>% 
    mutate(difference = actual - forecast,
           accuracy = 1 - abs(actual - forecast) / actual) -> fc_shift_list[[i]]
  
  
}

fc_shift_list %>% 
  writexl::write_xlsx("01_output/forecast_per_shift_non_overlapping.xlsx")

#
# plot ----
plot_data_tbl <- fc_resample_tbl %>% 
  filter(.model_desc %in% c("ACTUAL", "ensemble-deepar-tbats")) %>% 
  mutate(
    hour = hour(.index),
    shift = 
      case_when(hour %in% c(8:15) ~ "day",
                hour %in% c(0:7)  ~ "night",
                TRUE ~ "afternoon"),
    date = date(.index)
  ) %>% 
  filter(resample == "resample_1") %>% 
  select(-resample) %>% 
  distinct() %>% 
  group_by(.model_desc, shift, date) %>% 
  mutate(min_index = min(.index)) %>% 
  group_by(.model_desc, min_index, shift) %>% 
  summarise(.value = round(sum(.value))) %>% 
  ungroup()


p <- plot_data_tbl %>% 
  ggplot(aes(x = min_index, y = .value, col = .model_desc)) + 
  geom_line()


plotly::ggplotly(p)


fc_resample_tbl %>% 
  filter(.model_desc %in% c("ACTUAL", "ensemble-deepar-tbats", "prophet")) %>% 
  group_by(resample) %>% 
  plot_modeltime_forecast(.facet_ncol = 2)




# daily -------------------------------------------------------------------

storage_read_csv(
  con$container_dev_fc_files,
  "patria/2023/09/18/best_model_per_id_30d.csv"
)


fc_daily_resample_tbl <- storage_read_csv(
  con$container_dev_fc_files,
  "patria/2023/09/18/tscv-forecast_30d.csv"
)


fc_daily_resample_tbl %>% 
  filter(.model_desc %in% c("ACTUAL", "seasonal_xgboost_default")) %>% 
  group_by(resample) %>% 
  plot_modeltime_forecast(.facet_ncol = 2)




# shift -------------------------------------------------------------------


fc_resample_tbl <- storage_read_csv(
  con$container_dev_fc_files,
  "patria/2023/09/19/tscv-forecast_shift8h.csv"
)

fc_resample_tbl %>% 
  select(-c(.model_id, .key, id)) %>% 
  pivot_wider(names_from = .model_desc, values_from = .value) %>% 
  drop_na() %>% 
  pivot_longer(cols = -c(.index, resample, ACTUAL)) %>% 
  group_by(.index, name) %>% 
  summarise(
    actual = sum(ACTUAL),
    value = sum(value)
  ) %>% 
  mutate(diff = actual - value,
         accuracy = 1 - abs(actual - value) / actual) %>% 
  
  filter(name == "stl_arima") %>% 
  pull(accuracy) %>% 
  mean()
