require(ggplot2)
source('helpers.R')

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
  probe <- names(probe_map)[probe_map == input$hist_ridge_probe]
  p_hist <- probe_hist.munge(df, input, ranges)
  
  # ridges plot
  p2 <- ggplot(p_hist, aes(metric, as.character(app_build_id), height=measure, fill=cum.sum)) + #, color=app_build_id )) +  
    # geom_point() 
    geom_density_ridges_gradient(
      stat = 'identity',
      scale = input$ridge_scale) + 
    labs(x = probe, y = 'App Build') +
    viridis::scale_fill_viridis(name = "CDF", option = "C") +
    theme_ridges()
  
  return(p2)
}

plot.probe_cdf <- function(df, input, probe_map, ranges){
  probe <- names(probe_map)[probe_map == input$hist_ridge_probe]
  p_hist <- probe_hist.munge(df, input, ranges)
  
  # CDF
  p1 <-
    ggplot(p_hist,
           aes(metric, cum.sum, color = as.character(app_build_id))) +
    geom_line() +
    labs(x = probe, y = 'Cumulative Distribution') +
    guides(color = guide_legend(title = "App Build")) +
    theme(legend.position = c(0.8, 0.2), panel.background = element_rect(fill = NA),
          panel.grid.major = element_line(colour = "grey90"),
          panel.grid.minor = element_line(colour = "grey90")
    )
  
  return(p1)
}


