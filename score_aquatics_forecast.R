score_aquatics_forecast <- function(output_file, remote_folder,forecast_file) {
  library(neon4cast)
  library(tidyverse)
  
  
  faasr_get_file(remote_folder = "data",remote_file = forecast_file,local_folder = "data",local_file = forecast_file)
  forecast <- read_csv(file.path("data",forecast_file))
  target <- read_csv("https://data.ecoforecast.org/neon4cast-targets/aquatics/aquatics-targets.csv.gz")
  
  scores <- score(forecast, target)
  
  summary_scores <- scores %>% 
    group_by(variable) %>% 
    summarise(
      mean_crps = mean(crps, na.rm = TRUE),
      mean_logs = mean(logs, na.rm = TRUE)
    )
  
  write_csv(summary_scores, file.path("data",output_file))
  faasr_put_file(local_file=file.path("data",output_file), 
                 remote_folder=remote_folder, 
                 remote_file=output_file)
  
  log_msg <- paste0("Generated forecast successfully")
  faasr_log(log_msg)
  
}