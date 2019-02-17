library(dplyr)
library(purrr)
# source('data_prep/data_prep_helpers.R')

initialize <- function(){
  # df <- build.df()

  # return(df)
}

calc_quantile <- function(prob = c(0.05, 0.25, 0.5, 0.75, 0.95)){
  p_names <- map_chr(prob, ~paste0(.x*100, "%"))
  p_funs <- map(prob, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
    set_names(nm = p_names)
  return(p_funs)
}