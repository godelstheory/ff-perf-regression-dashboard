
source('data_prep_helpers.R')

dir.path <- '/home/cdowhygelund/ShinyApps/ff-perf-regression-dashboard/data'
#### RelDS Quantiles ####
print("Processing: RelDS Quantiles")

# Parse the RelDS files into single data frame
df <- build.df(list.files(file.path(dir.path, 'relds_quantiles'), 
                          full.names = TRUE, 
                          pattern = 'part*'))
# Save off for later consumption
save(df, file = file.path(dir.path, 'live', 'df.RData'))

#### Client Means ####
print("Processing: Client Means")

# Parse the client mean histograms into a single data frame
client_means <- build_client_means.df(list.files(file.path(dir.path, 'client_means'), 
                                                 pattern = 'part*', 
                                                 full.names = TRUE))
# Add a Date Field
library(lubridate)
client_means$date <- as_date(as.character(client_means$app_build_id))

# Save off for later consumption
save(client_means, file = file.path(dir.path, 'live', 'client_means.RData'))

#### Probe Histograms #### 
print("Processing: Probe Histograms")

probe_hists <- build_probe_hists.df(list.files(file.path(dir.path, 'probe_hists'), 
                                               pattern='*.csv', 
                                               full.names = TRUE))

# add a cumulative sum
probe_hists <- probe_hists %>% 
  group_by_at(vars(app_build_id, probe)) %>%
  arrange(app_build_id, probe, metric) %>%
  mutate(cum.sum = cumsum(measure))

<<<<<<< HEAD
save(probe_hists, file = file.path(dir.path, 'live', 'probe_hists.RData'))
=======
save(probe_hists, file = file.path(dir.path, 'live', 'probe_hists.RData')
>>>>>>> 509320e5b7a836c3e99d80962bb2e9cccd15725a

