#
# DOUBLE ----------------------------------------------------------------------


#Function to get individual trajectory of an id

get.individual.trajectory.pair <- function(track.dyn, id.1, id.2, big = 12, small = 10){
  ind.df.1 <- track.dyn %>% 
    filter(tax.ids == id.1) %>% 
    #mutate(type = ifelse(penalized, "Penalized", ifelse(audited, "Audited", '')))
    select(c(t, tax.ids, hideable.reported, audited, penalized))
  
  ind.df.2 <- NULL
  if(length(id.2) > 1) {
    ind.df.2 <- track.dyn %>% 
      filter(tax.ids %in% id.2) %>% 
      group_by(t) %>% 
      summarise(hideable.reported = mean(hideable.reported)) %>% 
      mutate(tax.ids = 'nn', penalized = F, audited = F) %>% 
      arrange(t, tax.ids, hideable.reported, audited, penalized)
    
    plot.df <- rbind(ind.df.1, ind.df.2) %>%
      mutate(tax.ids = factor(tax.ids, levels = c(id.1, 'nn'), ordered = TRUE))
    
  } else {
    ind.df.2 <- track.dyn %>% 
      filter(tax.ids == id.2) %>% 
      #mutate(type = ifelse(penalized, "Penalized", ifelse(audited, "Audited", '')))
      select(c(t, tax.ids, hideable.reported, audited, penalized))
    plot.df <- bind_rows(ind.df.1, ind.df.2) %>%
      mutate(tax.ids = factor(tax.ids, levels = c(id.1, id.2), ordered = TRUE))
  }
  
  
  ind.plot <- ggplot(plot.df) +
    geom_line(aes(x = t, y = hideable.reported, group = tax.ids, colour = tax.ids), size = 1) + 
    xlab("Time") +
    ylab("Percentage of Hideable Income Reported") + 
    scale_color_brewer("Tax ID", type = "qual") + 
    theme_bw() +
    theme(panel.border = element_blank(),
          axis.text.x=element_text(size = small), 
          axis.text.y=element_text(size = small),
          strip.text=element_text(size = small),
          axis.title.x = element_text(size = big),
          axis.title.y = element_text(size = big),
          legend.text = element_text(size = small),
          plot.title = element_text(size = big + 1, hjust = 0.5))
  
  # annotations
  # plot.text <- plot.df %>%
  #   rowwise() %>%
  #   filter(penalized | audited) %>%
  #   select(tax.ids, t, hideable.reported, penalized, audited) %>%
  #   mutate(separator = ifelse(penalized & audited, "/", "")) %>%
  #   mutate(penalized = ifelse(penalized, "P", "")) %>%
  #   mutate(audited = ifelse(audited, "A", "")) %>%
  #   mutate(lbl = paste(penalized, audited, sep = separator))
  # 
  # if (nrow(plot.text) > 0)
  # {
  #   ind.plot <- ind.plot +
  #     geom_point(data = plot.text, aes(x = t, y = hideable.reported, color = tax.ids), show.legend = FALSE) + 
  #     geom_segment(data = plot.text, aes(x = t, y = hideable.reported, xend = t), yend = 0) + 
  #     geom_label(data = plot.text, aes(x = t, y = 0, label = lbl), size = 2, nudge_y = -1)
  # }
  # 
  # ind.plot
  return(ind.plot)          
}

#
# SINGLE ----------------------------------------------------------------------


#Function to get individual trajectory of an id

get.individual.trajectory <- function(track.dyn, id, big = 12, small = 10){
  ind.df <- track.dyn %>% group_by(tax.ids) %>% filter(tax.ids == id) %>% 
    mutate(type = ifelse(penalized, "Penalized", ifelse(audited, "Audited", '')))
  
  facet.label <- paste("Tax ID:", id, "| Hideable Income =", 
                       round(ind.df[1, 'perc.hideable.income'], 2), "% |", 
                       ifelse(ind.df$self.employed[1], "Self Employed", "Not Self Employed"))
  
  ind.plot <- ind.df %>%  
    ggplot() +  geom_line(aes(x=t, y=hideable.reported, group= tax.ids, colour = tax.ids),size=1.5) +
    #facet_wrap(~tax.ids, ncol = 1) +
    xlab("Time") +
    ylab("Percentage of Hideable Income Reported") + 
    theme_bw() +
    theme(legend.title = element_blank(),
          legend.position="none",
          panel.border = element_blank(),
          axis.text.x=element_text(size=small), 
          axis.text.y=element_text(size=small ) ,
          strip.text=element_text( size=small ) ,
          axis.title.x = element_text( size=big ) ,
          axis.title.y = element_text( size=big ),
          legend.text = element_text( size = small),
          plot.title = element_text(size = big + 1, hjust = 0.5)) + 
    ggtitle(label = facet.label)
  
  tmp <- ind.df[ind.df$type != '', ]
  if(dim(tmp)[1] > 0) {
    ind.plot <- ind.plot + geom_vline(data = tmp, mapping = aes(xintercept = t, linetype = type))
    ind.plot <- ind.plot + geom_text(data = tmp, check_overlap = T, 
                                     mapping = aes(x=t, y=0, label=type), 
                                     size=5, angle=90, vjust=-0.4, hjust=0)
  }
  
  return(ind.plot)          
}
