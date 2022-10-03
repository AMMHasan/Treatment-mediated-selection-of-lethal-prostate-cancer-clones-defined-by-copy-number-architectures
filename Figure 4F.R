library(dplyr)
library(ggpubr)
library(dndscv)
library(purrr)


mutations <- read.table("/Users/mahedi/Documents/UCL/Prostate cancer/CASCADE/SNV2/dnds/clonal/all_clonal_mutations_CASCADE_for_dndscv.txt", header = T, row.names = 1)
dndsout = dndscv(mutations)

mutation_position_tib <- dndsout$annotmuts %>% 
  filter((impact != "Synonymous")) %>% 
  group_by(sampleID) %>% 
  transmute(chr_pos = paste0(chr,"_", pos)) %>% 
  summarise(across(.cols=everything(), list)) %>% 
  mutate(cm_number=map_int(chr_pos,~ .x %>% length)) %>% 
  tidyr::separate(col=sampleID, into=c("patientID", "sample"), sep="_",remove = F) %>% 
  select(-sample)


percent_common <- list()

for(i in mutation_position_tib$patientID %>% unlist(use.names = F) %>% unique){
  sub_tib <- mutation_position_tib %>% 
    filter(patientID == i)
  for(j in seq(combn(sub_tib$sampleID,2, simplify = T) %>% length()/2)){
    element <- combn(sub_tib$sampleID,2, simplify = T)[,j]
    names <- paste(element[1],element[2],collapse = ".")
    value <- intersect(mutation_position_tib %>% 
                         filter(sampleID %in%  element[1]) %>% 
                         select(chr_pos) %>% 
                         unlist(use.names = F),
                       mutation_position_tib %>% 
                         filter(sampleID %in%  element[2]) %>% 
                         select(chr_pos) %>% 
                         unlist(use.names = F)) %>% length() * 100 / min(
                           mutation_position_tib %>% 
                             filter(sampleID %in%  element[1]) %>% 
                             select(cm_number),
                           mutation_position_tib %>% 
                             filter(sampleID %in%  element[2]) %>% 
                             select(cm_number)
                         ) 
    percent_common[[names]] <- value
  }
}

df <- read.csv("/Users/mahedi/Documents/UCL/Prostate cancer/CASCADE/SNV2/dnds/clonal/correlation_distance.csv", row.names = 1)

df_stat <- do.call(rbind, percent_common) %>% 
  as.data.frame %>%  
  select(percent_common_nonsilent_mutations=V1) %>% 
  merge(.,df, by="row.names", all=T) %>% 
  select(-Row.names)


p <- ggboxplot(df_stat, x="within_same_cluster", y="percent_common_nonsilent_mutations", fill="within_same_cluster") +
  xlab("Within same cluster?") + 
  ylab("% common clonal non-silent mutations") +
  labs(fill = "Within same cluster?")

p + stat_compare_means(method = "t.test")


##################### alternative version: v3 ###########################



library(dplyr)
library(purrr)
library(tidyr)
library(dndscv)
library(ggplot2)
library(ggpubr)

mutations <- read.table("/Users/mahedi/Documents/UCL/Prostate cancer/CASCADE/SNV2/dnds/clonal/all_clonal_mutations_CASCADE_for_dndscv.txt", header = T, row.names = 1)
dndsout = dndscv(mutations)


mutation_position_tib <- dndsout$annotmuts %>%
  filter((impact != "Synonymous")) %>% 
  group_by(sampleID) %>% 
  transmute(chr_pos = paste0(chr,"_", pos)) %>% 
  summarise(across(.cols=everything(), list)) %>% 
  mutate(cm_number=map_int(.x = chr_pos, .f =~ {.x %>% length})) %>% 
  separate(col=sampleID, into=c("patientID", "sample"), sep="_",remove = F) %>% 
  select(-sample) %>% 
  group_by(patientID) %>% 
  nest()


func_pct_common = function(sample_tibble){
  res_list <- map(.x = combn(sample_tibble$sampleID,2, simplify = F), 
                  .f = ~ { (sample_tibble %>% 
                              filter(sampleID %in% .x) %>% 
                              pull(chr_pos) %>% 
                              Reduce(intersect, .) %>% 
                              length)/
                      (sample_tibble %>%
                         filter(sampleID %in% .x) %>% 
                         pull(cm_number) %>% 
                         Reduce(min,.)) * 100
                  }
  )
  names(res_list) = map(.x = combn(sample_tibble$sampleID,2, simplify = F), .f = ~ paste(.x, collapse=" "))
  
  return(do.call(rbind, res_list) %>% data.frame() %>% rename(., pct_common=.)) 
  
}

percent_common = map(.x = mutation_position_tib %>% pull(data), 
                     .f =func_pct_common) %>% 
  do.call(rbind,.) 


df <- read.csv("/Users/mahedi/Documents/UCL/Prostate cancer/CASCADE/SNV2/dnds/clonal/correlation_distance.csv", row.names = 1)

df_stat <- percent_common %>% 
  as.data.frame %>%  
  merge(.,df, by="row.names", all=T) %>% 
  select(-Row.names)

p <- ggboxplot(df_stat, x="within_same_cluster", y="pct_common", fill="within_same_cluster") +
  xlab("Within same cluster?") + 
  ylab("% common clonal non-silent mutations") +
  labs(fill = "Within same cluster?")

p + stat_compare_means(method = "t.test")


