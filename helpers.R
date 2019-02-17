library(dplyr)
library(purrr)

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

build_client_means.df <- function(files){
  df <- NULL
  for (file in files) {
    df <- bind_rows(df, read.csv(file, header = TRUE, stringsAsFactors =  FALSE))
    df <- df %>% 
      group_by(app_build_id) %>% 
      filter(creation_date==max(creation_date)) %>% 
      arrange(desc(app_build_id))
  }
  return(df)
}

initialize <- function(){
  df <- build.df()
  load('data/live/client_means.RData')
  return(df)
}

calc_quantile <- function(prob = c(0.05, 0.25, 0.5, 0.75, 0.95)){
  p_names <- map_chr(prob, ~paste0(.x*100, "%"))
  p_funs <- map(prob, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
    set_names(nm = p_names)
  return(p_funs)
}