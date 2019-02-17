
#### Client Means ####

# Parse the client mean histograms into a single data frame
source('data_prep_helpers.R')
client_means <- build_client_means.df(list.files('data/client_means/', pattern = 'part*', 
                                                 full.names = TRUE))
# Add a Date Field
library(lubridate)
client_means$date <- as_date(as.character(client_means$app_build_id))

# Save off for later consumption
save(client_means, file = 'data/live/client_means.RData')