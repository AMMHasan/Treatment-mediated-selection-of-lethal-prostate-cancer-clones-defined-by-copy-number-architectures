library(dendextend)
library(circlize)
library(ComplexHeatmap)
library(dplyr)



plot_dend_cluster <- function(patientID){
  colour_code <- read.csv("/Users/mahedi/Documents/UCL/scripts/R/PM_paper/post-mortem_paper/CASCADE_organ_colour_codes.csv", row.names = 1)
  load(paste0("/Users/mahedi/Desktop/test/latest_500kb_run_30Nov21/HC_LP_similarity_in_SCRATCH_tree/Models__",
              patientID,
              "__metsonly__Copies__fullX__A500000__X500000.Rdata"))
  
  my_tree <- lTrees$tree_aut %>% as.dendrogram()
  
  old_labels <- labels(my_tree)
  
  labels(my_tree) <- colour_code[labels(my_tree),]$Fig.num
  
  col_legend <- colour_code[old_labels,] %>% 
    dplyr::select(Organ, Organ.Color) %>% 
    distinct() %>% 
    pull(Organ.Color)
  
  organ_legend <- colour_code[old_labels,] %>% 
    dplyr::select(Organ, Organ.Color) %>% 
    distinct() %>% 
    pull(Organ) 
  
  my_tree %>% 
    assign_values_to_leaves_nodePar(nodePar = "pch", value = colour_code[old_labels,]$pch) %>% 
    assign_values_to_leaves_nodePar(nodePar = "cex", value = 2) %>%
    assign_values_to_leaves_nodePar(nodePar = "bg", value = colour_code[old_labels,]$Organ.Color) %>% 
    assign_values_to_leaves_nodePar(nodePar = "col", value = colour_code[old_labels,]$col %>% tidyr::replace_na("NA")) %>%
    plot(yaxt="n", main=patientID) 
  
  my_tree %>% 
    rect.dendrogram(k=colour_code[old_labels,]$k %>% max(), border = 8, lty = 2, lwd = 2)
  
  legend("topleft",legend = organ_legend, fill=col_legend)
  
  
}

for(i in c("CA27","CA34","CA35","CA36","CA43","CA63","CA76","CA79","CA83")){
  plot_dend_cluster(i)
}

