require(ggplot2)

plot.crt <- function(df, input, probe_map, th = 0.10){
  probe <- names(probe_map)[probe_map == input$probe]
  probe_df <- df[df$probe==input$probe, ]
  probe_df$thresh <- as.factor(probe_df$relds_95 >= th)
  
  cbPalette <- c('TRUE' = "red", 'FALSE' = "black")
  
  p <- ggplot(probe_df, aes(x=app_build_id, y=relds_5, color=thresh, shape=thresh)) + 
    geom_point(aes(size=probe_df$num_profiles)) +
    geom_errorbar(aes(ymin=relds_25, ymax=relds_95), width=.1) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    scale_colour_manual(values=cbPalette) +
    scale_shape_manual(values = c('TRUE' = 17, 'FALSE' = 19)) +
    geom_hline(yintercept=th, linetype="dashed", color = "red") +
    # theme(legend.position="none", panel.background = element_blank(), 
    #       axis.line = element_line(colour = 'black'))+
    theme_bw() + 
    scale_size(range = c(1, 9)) + 
    guides(size=guide_legend(title='# of Profiles', thresh=FALSE)) +
    labs(x = 'Nightly Build', y = 'Median RelDS') +
    ggtitle(probe)
  return(p)
}

get.page_load_map <- function(){
    probe_map <- list(
      'TIME_TO_DOM_COMPLETE_MS' = 'histogram_content_time_to_dom_complete_ms',
      'TIME_TO_DOM_CONTENT_LOADED_END_MS' =  'histogram_content_time_to_dom_content_loaded_end_ms',
      'TIME_TO_LOAD_EVENT_END_MS' = 'histogram_content_time_to_load_event_end_ms',
      'TIME_TO_NON_BLANK_PAINT_MS' = 'histogram_content_time_to_non_blank_paint_ms',
      'TIME_TO_NON_BLANK_PAINT_MS_PARENT' = 'histogram_parent_time_to_non_blank_paint_ms',
      'FX_PAGE_LOAD_MS_2' = 'histogram_content_fx_page_load_ms_2',
      'FX_PAGE_LOAD_MS_2_PARENT' = 'histogram_parent_fx_page_load_ms_2'
    )
    return(probe_map)
}

plot.client_means.boxplot <- function(df, input, ranges){
  p <- ggplot(df, aes(x=date, y=get(input$client_mean_probe), group=interaction(date))) +  
    geom_boxplot(outlier.alpha = 0.1)
  return(p)
}

plot.client_means.violin <- function(df, input){
  p <- ggplot(df, aes(x=date, y=get(input$client_mean_probe), group=interaction(date))) +  
    geom_violin(outlier.alpha = 0.1, draw_quantiles = c(0.25, 0.5, 0.75))
  return(p)
}
  
plot.client_means <- function(df, input, ranges){
  x_lims <- if (!is.null(ranges$x[1])) as_date(ranges$x) else ranges$x
  if (input$dist_type == 'violin'){
    p <- plot.client_means.violin(df, input)
  }
  else (
    p <- plot.client_means.boxplot(df, input)
  )
  p <- p + 
    scale_x_date(limits = x_lims) +
    theme_bw() +
    labs(x = 'Nightly Build', y = 'Client Mean') +
    ggtitle(input$client_mean_probe)
  
  if (input$yaxis_log10){
    p <- p + 
      scale_y_continuous(limits = ranges$y, trans = 'log10') +
      annotation_logticks(sides = 'blr')
  }
  else {
    p <- p + scale_y_continuous(limits = ranges$y)
  }
  if (input$fit_lm){
    p <- p + geom_smooth(aes(group=1), method='lm')
  }
  return(p)
}

plot.probe_hist <- function(df, input, probe_map, ranges){
  p_hist <- df %>%
    filter(app_build_id %in% input$app_build & probe==input$hist_ridge_probe) %>%
    select('app_build_id', 'measure', 'metric')
    # mutate(metric_val = as.numeric(metric))
  
  if(!is.null(ranges$x)){
    p_hist <- p_hist %>%
      filter(metric >= ranges$x[1] & metric <= ranges$x[2])
  }
  p <- ggplot(p_hist, aes(metric, as.character(app_build_id), height=measure)) + #, color=app_build_id )) +  
    # geom_point() 
    geom_density_ridges(
      stat = 'identity',
      scale = 1) + 
    theme_ridges()
  return(p)
}


