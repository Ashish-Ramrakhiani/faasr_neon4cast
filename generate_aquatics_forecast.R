generate_aquatics_forecast <- function(output_file, remote_folder) {
  library(tsibble)
  library(tidyverse)
  library(neon4cast)

  
  # Read and process data
  target <- read_csv("https://data.ecoforecast.org/neon4cast-targets/aquatics/aquatics-targets.csv.gz")
  
  aquatic <- target %>% 
    pivot_wider(names_from = "variable", values_from = "observation") %>%
    as_tsibble(index = datetime, key = site_id)
  
  blinded_aquatic <- aquatic %>%
    filter(datetime < max(datetime) - 35) %>% 
    fill_gaps()
  
  # Function to create random walk forecast
  create_rw_forecast <- function(data, variable, h = 35, ensemble_members = 1) {
    sites <- unique(data$site_id)
    forecast_dates <- seq(max(data$datetime) + lubridate::days(1), 
                          by = "day", length.out = h)
    
    result <- list()
    for(site in sites) {
      site_data <- data %>% filter(site_id == site)
      last_value <- site_data[[variable]][nrow(site_data)]
      if(is.na(last_value)) next
      
      # Create random walk forecast
      set.seed(123) # for reproducibility
      for(i in 1:ensemble_members) {
        forecast_values <- numeric(h)
        forecast_values[1] <- last_value + rnorm(1, 0, sd(diff(site_data[[variable]], na.rm = TRUE)))
        for(j in 2:h) {
          forecast_values[j] <- forecast_values[j-1] + 
            rnorm(1, 0, sd(diff(site_data[[variable]], na.rm = TRUE)))
        }
        
        result[[length(result) + 1]] <- tibble(
          datetime = forecast_dates,
          site_id = site,
          variable = variable,
          ensemble = i,
          prediction = forecast_values
        )
      }
    }
    
    bind_rows(result)
  }
  
  # Generate forecasts
  oxygen_fc <- create_rw_forecast(blinded_aquatic, "oxygen")
  temperature_fc <- create_rw_forecast(blinded_aquatic, "temperature", ensemble_members = 30)
  
  # Combine forecasts
  forecast <- bind_rows(oxygen_fc, temperature_fc)
  
  # Write and upload results
  write_csv(forecast, file.path(output_file))
  faasr_put_file(local_file=file.path(output_file), 
                 remote_folder=remote_folder, 
                 remote_file=output_file)
  
  log_msg <- paste0("Generated forecast successfully")
  faasr_log(log_msg)
}