
df_isotopes <- read.csv("/Users/charlotteward/Documents/minnow_isotopes/R/data/final data/2022/NC_2022_twosource.csv", header = T)


library(tidyverse)
##function to calculate trophic position & coupling -- two-source mixing model, coupling bound b/w 0-1

##Calculating TP & dN using bound %LC b/w 0-1 --------

si_mixing_model_pred_bound <- function(x) {
  iso_data <- x
  
  cr_baseline <- iso_data %>%
    filter(organism %in% c("odonate", "mayfly", "mussel")) %>%
    filter(location == "creek")
  cr_mean_dC <- mean(cr_baseline$d_13C)
  cr_mean_dN <- mean(cr_baseline$d_15N)
  
  lk_baseline <- iso_data %>%
    filter(organism %in% c("odonate", "mayfly", "mussel")) %>%
    filter(location == "lake")
  lk_mean_dC <- mean(lk_baseline$d_13C)
  lk_mean_dN <- mean(lk_baseline$d_15N)
  
  df_pred <- iso_data %>%
    filter(organism %in% c("bluntnose minnow", "common shiner", "creek chub", "golden shiner", "northern redbelly dace", "pearl dace"))
  
  for(i in 1:nrow(df_pred)){
    lake_coupling <- ((df_pred$d_13C - cr_mean_dC)/(lk_mean_dC - cr_mean_dC))
    lake_coupling[lake_coupling > 1] <- 0.999
    lake_coupling[lake_coupling < 0] <- 0.001
    TP_pred <- 2 + (((lake_coupling * ((df_pred$d_15N - lk_mean_dN)/3.4))) + ((1 - lake_coupling) * ((df_pred$d_15N - cr_mean_dN)/3.4)))
    dN_pred <- df_pred$d_15N - (((1 - lake_coupling) * cr_mean_dN) + (lake_coupling * lk_mean_dN))
    mixing_model <- cbind(lake_coupling, TP_pred, dN_pred)
  }
  
  mixing_model <- as.data.frame(mixing_model)
  mixing_model$organism <- df_pred$organism
  mixing_model$ID <- df_pred$ID
  mixing_model$location <- df_pred$location
  mixing_model$tissue <- df_pred$tissue
  mixing_model$month <- df_pred$month
  
  mixing_model
}

tp_lake_coupling <- si_mixing_model_pred_bound(df_isotopes)

head(tp_lake_coupling)


# plotting proportions
library(ggplot2)

## summary 

data_summary <- tp_lake_coupling %>%
  mutate(month_group = case_when(
    month %in% c("august", "october") ~ "august",
    TRUE ~ as.character(month)
  )) %>%
  group_by(month_group, tissue, location) %>%
  summarize(
    mean_lake_C = mean(lake_coupling),
    sd_lake_C = sd(lake_coupling)
  )



# Filter the data for may samples
data_may <- tp_lake_coupling %>%
  filter(month == "may")
  
ggplot(data_may, aes(x=location, y=lake_coupling, fill=tissue)) + 
  labs(y= "Lake Energy Use", x = "location") +
  geom_boxplot()+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line =  element_line(size = 0.5, linetype = "solid",
                                  colour = "black"),
        panel.background = element_blank()
  )+
  scale_fill_manual("Legend", values = c("liver" = "plum4", "muscle" = "slategray3"))

may_anova<- aov(lake_coupling ~ location * tissue, data = data_may)

summary(may_anova)

may_posthoc <- TukeyHSD(may_anova)
may_posthoc

# Filter the data for august & october samples
data_aug <- tp_lake_coupling %>%
  filter(month == "august")

ggplot(data_aug, aes(x=location, y=lake_coupling, fill=tissue)) + 
  labs(y= "Lake Energy Use", x = "location") +
  geom_boxplot()+
  theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line =  element_line(size = 0.5, linetype = "solid",
                                    colour = "black"),
          panel.background = element_blank()
  )+
  scale_fill_manual("Legend", values = c("liver" = "plum4", "muscle" = "slategray3"))

aug_anova<- aov(lake_coupling ~ location * tissue, data = data_aug)

summary(aug_anova)

aug_posthoc <- TukeyHSD(aug_anova)

aug_posthoc

