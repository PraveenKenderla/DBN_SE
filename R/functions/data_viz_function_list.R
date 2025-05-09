

#pacman::p_load(dplyr, ggplot2)

#Plot Making function----
plot_making <- function(df,
                        x_var,
                        y_var,
                        filter_var = NULL,
                        color_variable,
                        facet_var = NULL,
                        title_name = NULL) {
  #1. check if filter var is provided if not don't filter
  if (!is.null({{filter_var}})) {
    df <- df %>% filter({{filter_var}})
  }
  
  #2. make3 a plot now!
  plot <- df %>%
    ggplot(aes({{x_var}}, {{y_var}})) +
    geom_point(
      aes(color = {{color_variable}}),
      position = position_jitter(),
      alpha = 0.3,
      size = 2, shape = 16
    ) +
    geom_smooth(aes(color = {{color_variable}}), method = lm, se = F)
  
  #3. Facet Variables;
  facet_var <- enquo(facet_var) # this fun enquo is from r_lang!
  if (!quo_is_null(facet_var)) { ## this fn quo_is_null is from r_lang
    plot <- plot +
      facet_wrap2(
        vars(!!facet_var),
        scales = "fixed",
        nrow = 3,
        ncol = 5,
        axes = "all",
        remove_labels = "y",
        labeller = labeller(.default = toupper)
      )
  }
  #4. Adding color x and y axis values!
  plot <- plot +
    scale_x_continuous(breaks = seq(4, 12, 1)) +
    scale_y_continuous(breaks = seq(0, 1, 0.5)) +
    scale_color_manual(values = c("#241D1D", "#8C510A", "#4A7B6D"),
                       labels = function(x) {
                         x <- as.character(x)
                         paste0(toupper(substr(x,1,1)), 
                                tolower(substr(x,2, nchar(x)))
                         )
                       }
    )
  #5. Adding plot name eh!
  if (!is.null({{title_name}})) {
    plot <- plot + 
      labs(title_name = title_name)
  }
  
  #6. Adding labs info given its same for all items:
  plot <- plot + labs(y = "Essentialism Scores", 
                      x = "Children's Age in Years", 
                      color = "Social Category")
  
  return(plot)
  complete = TRUE
}

#Theme function----
plot_theme <- function(base_family = "Roboto") {
  theme(
    
    #title
    plot.title = element_text(
      size = 18,
      face = "bold",
      hjust = 0.5,
      color = "#7d5b39",
      margin = margin(b = 10, t = 7, unit = "pt")
    ),
    
    ## Facet strip customization!
    strip.text = element_text(
      color = "grey2",
      size = 10,
      face = "bold"
    ),
    strip.background = element_rect(
      fill = "#F7F7F7",
      color = "black",
      linewidth = 1
    ),
    
    ## Panel: also works on facet!
    panel.border = element_rect(color = "black", linewidth = 1,
                                fill = NA),
    panel.background = element_rect(fill = "grey95"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    ##
    plot.background = element_rect(color = NA,
                                   fill = "grey95"),
    
    ## axis text:
    axis.text = element_text(size = 10, 
                             color = "black",
                             face = "bold",),
    
    ## axis titles:
    axis.title.x = element_text(margin = margin(t=10, b = 5, unit = "pt"),
                                size = 14,face = "bold"),
    axis.title.y = element_text(margin = margin(r=7, l = 7, unit = "pt"),
                                size = 14, face = "bold"), 
    axis.ticks.y = element_line(linewidth = 0.7, color = "black"),
    axis.ticks.length.y = unit(0.15, "cm"),
    
    ##Legend Customization:
    legend.background = element_rect(fill = "grey95", color = "black",
                                     linewidth = 0.5),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold")
  ) 
  
}
