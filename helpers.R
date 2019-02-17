library(dplyr)
library(purrr)

calc_quantile <- function(prob = c(0.05, 0.25, 0.5, 0.75, 0.95)){
  p_names <- map_chr(prob, ~paste0(.x*100, "%"))
  p_funs <- map(prob, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
    set_names(nm = p_names)
  return(p_funs)
}

# FIXME: Generalize (used specifically for ridge/CDF plots in this app)
probe_hist.munge <- function(df, input, ranges){
  p_hist <- df %>%
    filter(app_build_id %in% input$app_build & probe==input$hist_ridge_probe)
  
  if(!is.null(ranges$x)){
    p_hist <- p_hist %>%
      filter(metric >= ranges$x[1] & metric <= ranges$x[2])
    # recalculate cum.sum column based upon filtered measurements
    if (input$cdf_recompute){
      p_hist <- p_hist %>%
        arrange(app_build_id, probe, metric) %>%
        mutate(cum.sum = cumsum(measure)/sum(measure))
    }
  }
  return(p_hist)
}