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
    geom_hline(yintercept=th, linetype="dashed", color = "red") +
    theme(legend.position="none", panel.background = element_blank(), axis.line = element_line(colour = 'black'))+
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


