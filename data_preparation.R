library(dplyr)

build.df <- function() {
  df <- NULL
  for (file_name in list.files('data', full.names = TRUE, pattern = 'part*')) {
    df <- rbind(df, read.csv(file_name))
  }
  
  df$creation_date <- ymd(df$creation_date)
  df$app_build_id <- ymd(df$app_build_id)
  return(clean.df(df))
}

# Return most recent process data for a build: contains most pings for comparison
clean.df <- function(df) {
  df_clean <- df %>% 
    group_by(app_build_id, probe) %>% 
    filter(creation_date == max(creation_date)) %>% 
    arrange(desc(app_build_id))
  return(df_clean)
}
