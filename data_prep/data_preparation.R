
source('data_prep_helpers.R')

#### RelDS Quantiles ####
print("Processing: RelDS Quantiles")

# Parse the RelDS files into single data frame
df <- build.df(list.files('../data/relds_quantiles/', 
                          full.names = TRUE, 
                          pattern = 'part*'))
# Save off for later consumption
save(df, file = '../data/live/df.RData')

#### Client Means ####
print("Processing: Client Means")

# Parse the client mean histograms into a single data frame
client_means <- build_client_means.df(list.files('../data/client_means/', 
                                                 pattern = 'part*', 
                                                 full.names = TRUE))
# Add a Date Field
library(lubridate)
client_means$date <- as_date(as.character(client_means$app_build_id))

# Save off for later consumption
save(client_means, file = '../data/live/client_means.RData')

#### Probe Histograms #### 
probe_hists <- build_probe_hists.df(list.files('../data/probe_hists', 
                                               pattern='*.csv', 
                                               full.names = TRUE))

save(probe_hists, file = '../data/live/probe_hists.RData')

