library(ggplot2)
library(ComplexHeatmap)
library(circlize)
library(tidyr)
library(reshape2)
library(dplyr)

load("/Users/mahedi/Desktop/test/latest_500kb_run_30Nov21/Smetsonly__A500000/Models__CA27__metsonly__Copies__fullX__A500000__X500000.Rdata")


# finding out prostate biopsy specific TPs

df <- lTrees$TPs0_aut %>% as.data.frame()

prostate_specific_TPs <- df %>%
  dplyr::select(CA27_221, CA27_222, H670001) %>% 
  mutate(rSums = rowSums(.)) %>% 
  filter(rSums > 0 ) %>% 
  rownames()


df2 <- df[prostate_specific_TPs,]

# creating list for three categories of samples with <20%, >80% and in-between shared TPs 
prop_list <- list()

for(i in df %>% colnames()){
  TPs_met_rows <- df2[df2[,i] > 0,] %>% rownames() 
  TPs_met_nrows <- df2[df2[,i] > 0,] %>% nrow()
  TPs_met_ncols <- df2[df2[,i] > 0,] %>% ncol()
  
  TPs_shared_in_lessthan_20_pct_mets <- df2[TPs_met_rows,] %>% 
    mutate(rSums = rowSums(.)) %>% 
    filter(rSums < (TPs_met_ncols*20)/100) %>% 
    nrow()
  TPs_shared_in_20to80_pct_mets <- df2[TPs_met_rows,] %>% 
    mutate(rSums = rowSums(.)) %>% 
    filter(rSums >= (TPs_met_ncols*20)/100 & rSums <= (TPs_met_ncols*80)/100) %>% 
    nrow()
  
  TPs_shared_in_morethan_80_pct_mets <- df2[TPs_met_rows,] %>% 
    mutate(rSums = rowSums(.)) %>% 
    filter(rSums > (TPs_met_ncols*80)/100) %>% 
    nrow()
  
  
  prop_list[[i]] <- c("shared TPs < 20% "=TPs_shared_in_lessthan_20_pct_mets,
                      "shared TPs between 20% and 80% "=TPs_shared_in_20to80_pct_mets,
                      "shared TPs > 80%"=TPs_shared_in_morethan_80_pct_mets)
}

# df for plotting with some extra metadata

df_plot <- (do.call(rbind,prop_list) %>% as.data.frame())[c("CA27_221",
                                                            "CA27_222",
                                                            "H670001",
                                                            "CA27_229",
                                                            "CA27_216",
                                                            "CA27_217",
                                                            "CA27_218",
                                                            "H950005",
                                                            "CA27_226hc",
                                                            "CA27_227",
                                                            "CA27_214",
                                                            "CA27_215",
                                                            "CA27_231",
                                                            "CA27_220",
                                                            "H670002"),]  %>% 
  mutate(Breakpoint=c(rep("No AR inv BP",2),rep("AR inv BP",13)) %>% 
           factor(levels = c("No AR inv BP", "AR inv BP"))
         ) %>% 
  mutate(sampleID=c(1,2,4,6:17) %>% 
           factor(levels = c(1,2,4,6:17))
         )

df_plot%>% 
  reshape2::melt() %>% 
  ggplot(.) + 
  geom_bar(aes(x=sampleID, y=value, fill=variable), stat = "identity", colour="black", width = .8) + 
  facet_grid(.~Breakpoint,scales = "free", space = "free") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=0.5),
        panel.spacing = unit(0.5, "cm")) +
  xlab("CA27") +
  ylab("Number of Transition points") +
  labs(fill = "") +
  scale_fill_manual(values = c("#e28743", "#76b5c5", "#154c79"))



