

plot_dend_cluster <- function(patientID){
  colour_code <- read.csv("/Users/mahedi/Documents/UCL/scripts/R/PM_paper/post-mortem_paper/CASCADE_organ_colour_codes.csv", row.names = 1)
  load(paste0("/Users/mahedi/Desktop/test/latest_500kb_run_30Nov21/Smetsplsm_A500000/Models__",
              patientID,
              "__metsplsm__Copies__fullX__A500000__X500000.Rdata"))
  my_tree <- lTrees$tree_aut %>% as.dendrogram()
  old_labels <- labels(my_tree)
  

  col_legend <- colour_code[old_labels,] %>% 
    dplyr::select(Organ, Organ.Color) %>% 
    distinct() %>% 
    dplyr::select(Organ.Color) %>% 
    unlist(use.names = F)
  
  organ_legend <- colour_code[old_labels,] %>% 
    dplyr::select(Organ, Organ.Color) %>% 
    distinct() %>% 
    dplyr::select(Organ) %>% 
    unlist(use.names = F)
  
  labels(my_tree) <- colour_code[old_labels,]$Fig.num

  
  my_tree %>% 
    assign_values_to_leaves_nodePar(nodePar = "pch", value = c(rep(21,length(labels(my_tree)))))%>% 
    assign_values_to_leaves_nodePar(nodePar = "cex", value = 2) %>%
    assign_values_to_leaves_nodePar(nodePar = "bg", value = colour_code[old_labels,]$Organ.Color) %>%
    plot(yaxt="n") 
  my_tree %>% 
    rect.dendrogram(k=colour_code[old_labels,]$k %>% max(), border = 8, lty = 2, lwd = 2)
  legend("topleft",legend = organ_legend, fill=col_legend)
  
}


for(i in c("CA34","CA36","CA76","CA83")){
  plot_dend_cluster(i)
}
  
  
  
  
  
  