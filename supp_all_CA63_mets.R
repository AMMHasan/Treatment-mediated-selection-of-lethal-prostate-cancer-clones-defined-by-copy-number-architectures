library(dendextend)
library(circlize)
library(ComplexHeatmap)
library(dplyr)



colour_code <- read.csv("/Users/mahedi/Documents/UCL/scripts/R/PM_paper/post-mortem_paper/CASCADE_organ_colour_codes.csv", row.names = 1)
load("/Users/mahedi/Desktop/test/latest_500kb_run_30Nov21/Smetsonly__A500000//Models__CA63__metsonly__Copies__fullX__A500000__X500000.Rdata")
my_tree <- lTrees$tree_aut %>% as.dendrogram()
col_revised <- colour_code[labels(my_tree),]$Organ.Color
col_legend <- colour_code[labels(my_tree),] %>% 
  dplyr::select(Organ, Organ.Color) %>% 
  distinct() %>% 
  dplyr::select(Organ.Color) %>% 
  unlist(use.names = F)

organ_legend <- colour_code[labels(my_tree),] %>% 
  dplyr::select(Organ, Organ.Color) %>% 
  distinct() %>% 
  dplyr::select(Organ) %>% 
  unlist(use.names = F)
labels(my_tree) <- colour_code[labels(my_tree),]$Fig.num


my_tree %>% 
  assign_values_to_leaves_nodePar(nodePar = "pch", value = c(rep(21,length(labels(my_tree)))))%>% 
  assign_values_to_leaves_nodePar(nodePar = "cex", value = 2) %>%
  assign_values_to_leaves_nodePar(nodePar = "bg", value = col_revised)%>%
  plot(yaxt="n") 
my_tree %>% 
  rect.dendrogram(k=3, border = 8, lty = 2, lwd = 2)
legend("topleft",legend = organ_legend, fill=col_legend)

