generate_aquatics_forecast <- function(output_file,remote_folder) {
 
  library(neon4cast)
  library(tidyverse)
  library(fable)
  library(tsibble)
  
  target <- read_csv("https://data.ecoforecast.org/neon4cast-targets/aquatics/aquatics-targets.csv.gz")
  
  aquatic <- target %>% 
    pivot_wider(names_from = "variable", values_from = "observation") %>%
    as_tsibble(index = datetime, key = site_id)
  
  blinded_aquatic <- aquatic %>%
    filter(datetime < max(datetime) - 35) %>% 
    fill_gaps()
  
  oxygen_fc <- blinded_aquatic %>%
    model(benchmark_rw = RW(oxygen)) %>%
    forecast(h = "35 days") %>%
    efi_format()
  
  temperature_fc <- blinded_aquatic  %>% 
    model(benchmark_rw = RW(temperature)) %>%
    forecast(h = "35 days") %>%
    efi_format_ensemble()
  
  forecast <- bind_rows(oxygen_fc, temperature_fc)
  
  write_csv(forecast, file.path("/data", output_file))
  faasr_put_file(local_file=file.path("/data", output_file), 
                 remote_folder=remote_folder, 
                 remote_file=output_file)
  
  log_msg <- paste0("Generated forecast successfully")
  faasr_log(log_msg)
  
}