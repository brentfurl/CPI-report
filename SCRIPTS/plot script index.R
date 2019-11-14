file <- paste0("~/Dropbox/Clients/Ergon/Ergon business units/", business_unit, "/")
for (d in 1:length(demo_vars)) {
  
#1. SAMPLE OVERVIEW PLOT
   ##### data manipulation ###########
   
   dem <- eval(demo_vars[d])
   b <- df %>% group_by(get(dem)) 
   b <- fct_count(b[[eval(dem)]])
   colnames(b) <- c(eval(dem), "count")
   levels(b[[1]]) <- str_wrap(b[[1]], width = 27)
   b <- b %>% filter(get(dem) != "NA") 
   
   #################### plot ########################### 
   
   p <- ggplot(data = b, aes(x = reorder(eval(b[[1]]), desc(eval(b[[1]]))), y = count)) + geom_col(fill = "#9E2A2F") + 
     coord_flip() + theme_classic() + 
     sample_overview_theme +
     geom_text(aes(label = paste0(round(eval(b$count/sum(b$count)*100), 1),"%")), hjust = -.3, size = 5, fontface = "bold") +
     scale_y_continuous(expand = expand_scale(mult = c(.02, .18)))
   
   plot <- ggdraw(p) + draw_label(paste0(dem, ": COUNT"), fontfamily = "Gotham-Bold", size = 38, x = .03, , y = .92, hjust = 0) +
     draw_line(
       x = c(.03, .97),
       y = c(.87, .87),
       colour = ("#9E2A2F"), size = 2.2
     )
   
    ggsave(paste0(file,"sample overview plots/",dem, " - COUNT",".pdf"), 
           plot = plot, width = 14, height = 8.5)
   

########### LOOP BY 4 QUADRANTS (QUADRANT AND DIMENSION LEVEL PLOTS) ######################  
   
   for (q in 1:4) {
    
    colors <- quadrant_colors[[q]][1]
    Quadrant <- quadrants[[q]][1]
   # filepathway <- filepath
    #Demo <- demo_vars[d]
   
    
########################## PLOT BEGIN   ####################################
    long_dimension  <- df %>% gather(Dimension, Dim_score, c(eval(quadrants[[q]][[2]][1]), eval(quadrants[[q]][[3]][1]), eval(quadrants[[q]][[4]][1]))) 
    set <- long_dimension %>%
      mutate(Dimension = factor(Dimension, levels = c(eval(quadrants[[q]][[2]][1]), eval(quadrants[[q]][[3]][1]), eval(quadrants[[q]][[4]][1])), 
                                labels = c(eval(quadrants[[q]][[2]][2]),eval(quadrants[[q]][[3]][2]), eval(quadrants[[q]][[4]][2])))) %>%
      group_by(get(demo_vars[d]), Dimension)
    set <- summarise(set, mean(Dim_score, na.rm = TRUE)) 
    colnames(set) <- c(eval(dem), "Dimension", "Means")
    set <- set %>%
      filter(get(dem) != "NA")
    
   ## QUADRANT DEMO PLOTS 
    
    p <- ggplot(data = set, aes(get(dem), Means, fill = get(dem))) + 
      geom_col() +  
      theme_cowplot() + 
      coord_cartesian(ylim = c(0, 100)) + 
      scale_y_continuous(breaks = c(0, 50, 100)) +
      facet_grid(Dimension ~ .  , switch = "y") +
      scale_fill_manual(values = demo_colors[[1]][[2]]) +
      geom_text(aes(label = round(Means, 0)), vjust = -.5, size = 5, family = "Gotham-Medium") +
      guides(fill=guide_legend()) +
      dem_quad_theme 
    
    plot <- ggdraw(p) + draw_label(paste0(eval(dem), ": ", eval(Quadrant)), fontfamily = "Gotham-Bold", size = 38, x = .03, , y = .92, hjust = 0) +
      draw_line(
        x = c(.03, .97),
        y = c(.87, .87),
        colour = ("#9E2A2F"), size = 2.2
      )
    
    ggsave(paste0(file, "by demo quad and dim plots/",eval(dem), "/", eval(dem), "_", eval(Quadrant),".pdf"), 
           plot = plot, width = 14, height = 8.5)
  
  ############################################ PLOT END #############################
   
    
    for (i in 2:4) {
      ### run the dim level plot  
      
         # q <- 1
         # d <- 1
         # i <- 2
      
      level_nums <- 1:5
      
      if (i == 2) {
        item_range <- eval(q * 15 - 14):eval(q * 15 - 10)
        item_labels <- all_items[item_range]
        #item_labels <- all_items[eval(q * 15 - 14):eval(q * 15 - 10)]
        cpi_vars <- all_cpi_vars[eval(q * 15 - 14):eval(q * 15 - 10)]}
      if (i == 3) {
        item_range <- eval(q * 15 - 9):eval(q * 15 - 5)
        item_labels <- all_items[item_range]
        #items_labels <- all_items[eval(q * 15 - 9):eval(q * 15 - 5)]
        cpi_vars <- all_cpi_vars[eval(q * 15 - 9):eval(q * 15 - 5)]}
      if (i == 4) {
        item_range <- eval(q * 15 - 4):eval(q * 15)
        item_labels <- all_items[item_range]
        #items_labels <- all_items[eval(q * 15 - 4):eval(q * 15)]
        cpi_vars <- all_cpi_vars[eval(q * 15 - 4):eval(q * 15)]}
      
      #dimension <- quadrants[[q]][[i]][1]
      
  ########################################## PLOT BEGIN ###########################
      
      
    item_labels_wrapped <- str_wrap(item_labels, width = 30) # items_wrapped
      
      
      ## do this next and try to get a Adaptability values one to work
      long_dim  <- df %>% gather(dim_items, Items, cpi_vars)  # long_dim, dim_items, Items, 
      #long_dim <- long_dim %>% select(ResponseId, Role:Gender, All, dim_items, Items)
      long_dim$dim_items  <- parse_number(long_dim$dim_items)
      long_dim <- long_dim %>% mutate(dim_items = factor(dim_items, levels = level_nums, labels = item_labels_wrapped))
      
      
      # do this 12 times for each dimension and then repeat for Tenure Generation.  Make code consistent enough to just copy and paste Tenure and Generation.
      long_dim <- long_dim %>%
        group_by(get(demo_vars[d]),dim_items)
      dim_Means <- summarise(long_dim, mean(Items, na.rm = TRUE))
      colnames(dim_Means) <- c(eval(demo_vars[d]), "Items", "Means")
      dim_Means <- dim_Means %>%
        filter(get(demo_vars[d]) != "NA")
    
      #### DIMENSION DEMO PLOTS Q 1-4
        
      p <- ggplot(data = dim_Means, aes(x = reorder(eval(dim_Means[[1]]), desc(eval(dim_Means[[1]]))), y = Means, fill = eval(dim_Means[[1]]))) +     #, fill = eval(demo_vars[d]), alpha=5)) + 
        geom_col()+ coord_flip(ylim = c(0, 100)) + 
        guides(alpha=FALSE) + 
        theme_cowplot()   + 
        facet_grid(Items ~ ., switch = "y") + 
        geom_text(aes(label = round(Means, 0)), hjust = -.3, size = 4, family = "Gotham-Medium") + 
        scale_fill_manual(values = demo_colors[[1]][[2]]) +
        demo_dimension_theme
        
      plot <- ggdraw(p) + 
        draw_label(paste0(eval(demo_vars[d]), ": ", eval(quadrants[[q]][[i]][2])), 
            fontfamily = "Gotham-Bold", size = 38, x = .03, , y = .92, hjust = 0) +
        draw_line(
          x = c(.03, .97),
          y = c(.87, .87),
          colour = ("#9E2A2F"), size = 2.2
        )
      plot
      ggsave(paste0(file,"by demo quad and dim plots/",eval(demo_vars[d]), "/",eval(demo_vars[d]), "- ", eval(quadrants[[q]][[i]][2]),".pdf"), 
             plot = plot, width = 14, height = 8.5)
      
    }
    }# closes the dimension loop   
    #} # closes IF q == 1:4
      # makeplot_item_level <- function{CREATE}
      # plot_item_level <- function(makeplot_item_level)....applies current 'items' and 'dimension' name
 for (q in 5) {
   
     for (i in 2:3) 
     {
       
         # d<-4
         #  q <- 5
         #  i <- 3   #hjust for generation sat and effort on this one was manually put to .8 instead of -.2
       
       if (i == 2) {
         item_range <- eval(q * 15 - 14):eval(q * 15 - 10)
         item_labels <- all_items[item_range]
         #item_labels <- all_items[eval(q * 15 - 14):eval(q * 15 - 10)]
         cpi_vars <- all_cpi_vars[eval(q * 15 - 14):eval(q * 15 - 10)]
         level_nums <- 1:5
         geom_text_size <- 4
         y_range <- c(0, 100)}
       if (i == 3) {
         item_range <- eval(q * 15 - 9):eval(q * 15 - 8)
         item_labels <- all_items[item_range]
         #items_labels <- all_items[eval(q * 15 - 9):eval(q * 15 - 5)]
         cpi_vars <- all_cpi_vars[eval(q * 15 - 9):eval(q * 15 - 8)]
         level_nums <- 1:2
         geom_text_size <- 5
         y_range <- c(0, 102)}
       
       
       
       #dimension <- quadrants[[q]][[i]][1]
       
       ########################################## PLOT BEGIN ###########################
       
       
       #item_labels = all_items[16:20]  # # this is items from above
       item_labels_wrapped <- str_wrap(item_labels, width = 30) # items_wrapped
       
       
       
       
       ## do this next and try to get a Adaptability values one to work
       long_dim  <- df %>% gather(dim_items, Items, cpi_vars)  # long_dim, dim_items, Items, 
       #long_dim <- long_dim %>% select(ResponseId, Role:Gender, All, dim_items, Items)
       long_dim$dim_items  <- parse_number(long_dim$dim_items)
       long_dim <- long_dim %>% mutate(dim_items = factor(dim_items, levels = level_nums, labels = item_labels_wrapped))
       
       long_dim <- long_dim %>%
         group_by(get(demo_vars[d]),dim_items)
       dim_Means <- summarise(long_dim, mean(Items, na.rm = TRUE))
       colnames(dim_Means) <- c(eval(demo_vars[d]), "Items", "Means")
       dim_Means <- dim_Means %>%
         filter(get(demo_vars[d]) != "NA")
       
       #### POS/SAT_EFFORT DEMO PLOTS Q 5
       
       p <- ggplot(data = dim_Means, aes(x = reorder(eval(dim_Means[[1]]), desc(eval(dim_Means[[1]]))), y = Means, fill = eval(dim_Means[[1]]))) +     #, fill = eval(demo_vars[d]), alpha=5)) + 
         geom_col()+ coord_flip(ylim = y_range) + 
         guides(alpha=FALSE) + theme_cowplot() +
         facet_grid(Items ~ ., switch = "y") +
         geom_text(aes(label = round(Means, 0)), hjust = -.3, size = eval(geom_text_size), family = "Gotham-Medium") + 
         scale_fill_manual(values = demo_colors[[1]][[2]]) +
         demo_dimension_theme
       
       plot <- ggdraw(p) + 
         draw_label(paste0(eval(demo_vars[d]), ": ", eval(quadrants[[q]][[i]][2])), 
                    fontfamily = "Gotham-Bold", size = 38, x = .03, , y = .92, hjust = 0) +
         draw_line(
           x = c(.03, .97),
           y = c(.87, .87),
           colour = ("#9E2A2F"), size = 2.2)
         
       
       ggsave(paste0(file,"by demo quad and dim plots/",eval(demo_vars[d]), "/", eval(demo_vars[d]), "- ", eval(quadrants[[q]][[i]][2]),".pdf"), 
              plot = plot, width = 14, height = 8.5)
     } # closes dimension loop 
 }    # closes IF q ==5 
      
    
} #closes demo loop













#2. DIMESNIONS ALL
for (q in 1:4) 
{
  
  #q<-1
  
  colors <- quadrant_colors[[q]][1]
  Quadrant <- quadrants[[q]][1]
  #filepathway <- filepath
  
  long_dimension  <- df %>% gather(Dimension, Dim_score, c(eval(quadrants[[q]][[2]][1]), eval(quadrants[[q]][[3]][1]), eval(quadrants[[q]][[4]][1]))) # actually, this should be 3 dimensions.  Do this graph over 4 times changing this part.  
  set <- long_dimension %>%
    mutate(Dimension = factor(Dimension, levels = c(eval(quadrants[[q]][[2]][1]), eval(quadrants[[q]][[3]][1]), eval(quadrants[[q]][[4]][1])), 
                              labels = c(eval(quadrants[[q]][[2]][2]),eval(quadrants[[q]][[3]][2]), eval(quadrants[[q]][[4]][2])))) %>%
    group_by(Dimension)
  set <- summarise(set, mean(Dim_score, na.rm = TRUE)) 
  colnames(set) <- c("Dimension", "Means")
  
  #### QUADRANTS ALL PLOTS Q 1-4
  
  p <- ggplot(data = set, aes(Dimension, Means)) + geom_col(fill = eval(quadrant_colors[q])) + coord_flip(ylim = c(0, 100)) +  xlim(rev(levels(set$Dimension))) + 
    theme_classic() +
    geom_text(aes(label = round(Means, 0)), hjust = -.3, size = 5, family = "Gotham-Medium") + 
    scale_fill_manual(values = quadrant_colors[q]) +
    all_quadrant_theme
  
  plot <- ggdraw(p) + 
    draw_label(paste0("ALL: ", eval(quadrants[[q]][[1]])), 
               fontfamily = "Gotham-Bold", size = 38, x = .03, , y = .92, hjust = 0) +
    draw_line(
      x = c(.03, .97),
      y = c(.87, .87),
      colour = ("#9E2A2F"), size = 2.2)
   
  ggsave(paste0(file,"by all dimensions plots/All - Quadrant - ", eval(quadrants[[q]][[1]]),".pdf"), 
         plot = plot, width = 14, height = 8.5)
  
  
  for (i in 2:4) 
  {
    ### run the dim level plot  
  
    #df <- df
  
    # q <- 1
    # i <- 4
    
    
    
    if (i == 2) {
      item_range <- eval(q * 15 - 14):eval(q * 15 - 10)
      item_labels <- all_items[item_range]
      #item_labels <- all_items[eval(q * 15 - 14):eval(q * 15 - 10)]
      cpi_vars <- all_cpi_vars[eval(q * 15 - 14):eval(q * 15 - 10)]
      level_nums <- 1:5}
    if (i == 3) {
      item_range <- eval(q * 15 - 9):eval(q * 15 - 5)
      item_labels <- all_items[item_range]
      #items_labels <- all_items[eval(q * 15 - 9):eval(q * 15 - 5)]
      cpi_vars <- all_cpi_vars[eval(q * 15 - 9):eval(q * 15 - 5)]
      level_nums <- 1:5}
    if (i == 4) {
      item_range <- eval(q * 15 - 4):eval(q * 15)
      item_labels <- all_items[item_range]
      #items_labels <- all_items[eval(q * 15 - 4):eval(q * 15)]
      cpi_vars <- all_cpi_vars[eval(q * 15 - 4):eval(q * 15)]
      level_nums <- 1:5}
  
    
  #dimension <- quadrants[[q]][[i]][1]
  
  ########################################## PLOT BEGIN ###########################
  
  
    #item_labels = all_items[16:20]  # # this is items from above
    item_labels_wrapped <- str_wrap(item_labels, width = 30) # items_wrapped
    
    
    ## do this next and try to get a Adaptability values one to work
    long_dim  <- df %>% gather(dim_items, Items, cpi_vars)  # long_dim, dim_items, Items, 
    #long_dim <- long_dim %>% select(ResponseId, Role:Gender, All, dim_items, Items)
    long_dim$dim_items  <- parse_number(long_dim$dim_items)
    long_dim <- long_dim %>% mutate(dim_items = factor(dim_items, levels = level_nums, labels = item_labels_wrapped))
    
    
    # do this 12 times for each dimension and then repeat for Tenure Generation.  Make code consistent enough to just copy and paste Tenure and Generation.
    long_dim <- long_dim %>%
      group_by(dim_items)
    dim_Means <- summarise(long_dim, mean(Items, na.rm = TRUE))
    colnames(dim_Means) <- c("Items", "Means")
    
    p <- ggplot(data = dim_Means, aes(Items, Means)) + 
      geom_col(fill = eval(quadrant_colors[q])) + 
      coord_flip(ylim = c(0, 100)) +  
      xlim(rev(levels(dim_Means$Items))) + 
      theme_classic() +
      geom_text(aes(label = round(Means, 0)), hjust = -.3, size = 5, family = "Gotham-Medium") + 
      scale_fill_manual(values = demo_colors[[1]][[2]]) +
      all_dimension_theme
    
plot <- ggdraw(p) + 
  draw_label(paste0("ALL: ", eval(quadrants[[q]][[i]][2])), 
             fontfamily = "Gotham-Bold", size = 38, x = .03, , y = .92, hjust = 0) +
  draw_line(
    x = c(.03, .97),
    y = c(.87, .87),
    colour = ("#9E2A2F"), size = 2.2)  
    
    
    ggsave(paste0(file,"by all dimensions plots/All - Dimension - ", eval(quadrants[[q]][[i]][2]),".pdf"), 
           plot = plot, width = 14, height = 8.5)

  }
}
   
   for (q in 5){
     for (i in 2:3) 
     {
       ### run the dim level plot  
       
       #df <- df
       
         # q <- 5
         # i <- 3
       
       
       
       if (i == 2) {
         item_range <- eval(q * 15 - 14):eval(q * 15 - 10)
         item_labels <- all_items[item_range]
         #item_labels <- all_items[eval(q * 15 - 14):eval(q * 15 - 10)]
         cpi_vars <- all_cpi_vars[eval(q * 15 - 14):eval(q * 15 - 10)]
         level_nums <- 1:5}
       if (i == 3) {
         item_range <- eval(q * 15 - 9):eval(q * 15 - 8)
         item_labels <- all_items[item_range]
         #items_labels <- all_items[eval(q * 15 - 9):eval(q * 15 - 5)]
         cpi_vars <- all_cpi_vars[eval(q * 15 - 9):eval(q * 15 - 8)]
         level_nums <- 1:2}
       
       
       
       #dimension <- quadrants[[q]][[i]][1]
       
       ########################################## PLOT BEGIN ###########################
       
       
       #item_labels = all_items[16:20]  # # this is items from above
       item_labels_wrapped <- str_wrap(item_labels, width = 30) # items_wrapped
       
       
       ## do this next and try to get a Adaptability values one to work
       long_dim  <- df %>% gather(dim_items, Items, cpi_vars) # %>%
       #   select(dim_items, Items, cpi_vars)# long_dim, dim_items, Items, 
       # long_dim  <- df %>% gather(Items, cpi_vars)
       #long_dim <- long_dim %>% select(ResponseId, Role:Gender, All, dim_items, Items)
       long_dim$dim_items  <- parse_number(long_dim$dim_items)
       long_dim <- long_dim %>% mutate(dim_items = factor(dim_items, levels = level_nums, labels = item_labels_wrapped))
       
       
       # do this 12 times for each dimension and then repeat for Tenure Generation.  Make code consistent enough to just copy and paste Tenure and Generation.
       long_dim <- long_dim %>%
         group_by(dim_items)
       dim_Means <- summarise(long_dim, mean(Items, na.rm=TRUE))
       colnames(dim_Means) <- c("Items", "Means")
       
       p <- ggplot(data = dim_Means, aes(Items, Means)) + 
         geom_col(fill = eval(quadrant_colors[q])) + 
         coord_flip(ylim = c(0, 100)) +  
         xlim(rev(levels(dim_Means$Items))) + 
         theme_classic() +
         geom_text(aes(label = round(Means, 0)), hjust = -.3, size = 5, family = "Gotham-Medium") + 
         scale_fill_manual(values = demo_colors[[1]][[2]]) +
         all_dimension_theme
       
       plot <- ggdraw(p) + 
         draw_label(paste0("ALL: ", eval(quadrants[[q]][[i]][2])), 
                    fontfamily = "Gotham-Bold", size = 38, x = .03, , y = .92, hjust = 0) +
         draw_line(
           x = c(.03, .97),
           y = c(.87, .87),
           colour = ("#9E2A2F"), size = 2.2)  
       
       ggsave(paste0(file,"by all dimensions plots/All - Dimension - ", eval(quadrants[[q]][[i]][2]),".pdf"), 
              plot = plot, width = 14, height = 8.5)
       
     }
   }







