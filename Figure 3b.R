library(dplyr)

bp_df <- read.csv("/Users/mahedi/Documents/UCL/scripts/R/PM_paper/post-mortem_paper/Break_point_CA34.csv")
bp_df %>% mutate(AF=var_read_number/(var_read_number+ref_read_number),
                 adjusted_AF = AF/TC,
                 u=adjusted_AF*((TC*T_CCN)+((1-TC)*N_CCN)),
                 m=case_when(
                   u >= 1 ~ abs(u),
                   u < 1 ~ 1),
                 CCF=u/m,
                 comment=case_when(
                   CCF == 1 ~ "Clonal",
                   CCF == 0 ~ "NA",
                   CCF < 1 ~ "Subclonal")
                 )







