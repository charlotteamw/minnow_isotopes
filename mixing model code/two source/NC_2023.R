
library(tidyverse)

##Calculating TP & dN using bound %LC b/w 0-1 --------
isotope_data <- read.csv("/Users/charlotteward/Documents/minnow_isotopes/data/final data/2023/NC_2023.csv", header = T)


################## MAY ####################

##function to calculate trophic position & coupling -- two-source mixing model, coupling bound b/w 0-1

### Choose which was to use the data - first option: creek & int combined; second option: int removed 
#isotope_data <- isotope_data %>%
  #mutate(location = ifelse(location == "int", "creek", location))

isotope_data <- isotope_data %>%
  filter(location != "int")

si_mixing_model_pred_bound_may <- function(x) {
  iso_data <- x
  
  cr_baseline <- iso_data %>%
    filter(organism %in% c("mayfly", "stonefly", "mussel")) %>%
    filter(location == "creek", month=="may")
  cr_mean_dC <- mean(cr_baseline$d13C_kilj)
  cr_mean_dN <- mean(cr_baseline$d15N)
  
  lk_baseline <- iso_data %>%
    filter(organism %in% c("mayfly", "stonefly", "mussel")) %>%
    filter(location == "lake", month=="may")
  lk_mean_dC <- mean(lk_baseline$d13C_kilj)
  lk_mean_dN <- mean(lk_baseline$d15N)
  
  df_pred <- iso_data %>%
    filter(organism %in% c("common shiner", "golden shiner", "creek chub", "bluntnose minnow", "northern redbelly dace", "pearl dace", "yellow perch", "smallmouth bass"))
  
  for(i in 1:nrow(df_pred)){
    lake_coupling <- ((df_pred$d13C_kilj - cr_mean_dC)/(lk_mean_dC - cr_mean_dC))
    lake_coupling[lake_coupling > 1] <- 0.999
    lake_coupling[lake_coupling < 0] <- 0.001
    TP_pred <- 2 + (((lake_coupling * ((df_pred$d15N - lk_mean_dN)/3.4))) + ((1 - lake_coupling) * ((df_pred$cal.d15N - cr_mean_dN)/3.4)))
    dN_pred <- df_pred$d15N - (((1 - lake_coupling) * cr_mean_dN) + (lake_coupling * lk_mean_dN))
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

may_tp_lake_coupling <- si_mixing_model_pred_bound_may(isotope_data)

head(may_tp_lake_coupling)


# plotting proportions
library(ggplot2)

# Filter the data for may samples
data_may <- may_tp_lake_coupling %>%
  filter(month == "may") %>%
  filter(tissue != "N/A")
  
ggplot(data_may, aes(x=location, y=lake_coupling, fill=tissue)) + 
  labs(y = "Lake Energy Use", x = "location") +
  geom_boxplot()+
  ggtitle("All Cyprinids May") +  
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line =  element_line(size = 0.5, linetype = "solid",
                                  colour = "black"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size = 18),
        legend.text = element_text(size = 14)
  )+
  scale_fill_manual("Legend", values = c("liver" = "plum4", "muscle" = "slategray3"))

may_anova<- aov(lake_coupling ~ location * tissue, data = data_may)

summary(may_anova)

may_posthoc <- TukeyHSD(may_anova)
may_posthoc


##### Golden Shiner May #####
data_gs_may <- data_may %>%
  filter(organism=="golden shiner")

ggplot(data_gs_may, aes(x=location, y=lake_coupling, fill=tissue)) + 
  labs(y= "Lake Energy Use", x = "location") +
  geom_boxplot()+
  ggtitle("Golden Shiner May") +  
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line =  element_line(size = 0.5, linetype = "solid",
                                  colour = "black"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size = 18),
        legend.text = element_text(size = 14)
  )+
  scale_fill_manual("Legend", values = c("liver" = "plum4", "muscle" = "slategray3"))


#### Common Shiner May ####
data_cs_may <- data_may %>%
  filter(organism=="common shiner")

ggplot(data_cs_may, aes(x=location, y=lake_coupling, fill=tissue)) + 
  labs(y= "Lake Energy Use", x = "location") +
  geom_boxplot()+
  ggtitle("Common Shiner May") +  
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line =  element_line(size = 0.5, linetype = "solid",
                                  colour = "black"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size = 18),
        legend.text = element_text(size = 14)
  )+
  scale_fill_manual("Legend", values = c("liver" = "plum4", "muscle" = "slategray3"))


##### Creek Chub May #####
data_cc_may <- data_may %>%
  filter(organism=="creek chub")

ggplot(data_cc_may, aes(x=location, y=lake_coupling, fill=tissue)) + 
  labs(y= "Lake Energy Use", x = "location") +
  geom_boxplot()+
  ggtitle("Creek Chub May") +  
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line =  element_line(size = 0.5, linetype = "solid",
                                  colour = "black"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size = 18),
        legend.text = element_text(size = 14)
  )+
  scale_fill_manual("Legend", values = c("liver" = "plum4", "muscle" = "slategray3"))


##### Bluntnose Minnow May #####
data_bnm_may <- data_may %>%
  filter(organism=="bluntnose minnow")

ggplot(data_bnm_may, aes(x=location, y=lake_coupling, fill=tissue)) + 
  labs(y= "Lake Energy Use", x = "location") +
  geom_boxplot()+
  ggtitle("BNM May") +  
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line =  element_line(size = 0.5, linetype = "solid",
                                  colour = "black"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size = 18),
        legend.text = element_text(size = 14)
  )+
  scale_fill_manual("Legend", values = c("liver" = "plum4", "muscle" = "slategray3"))


##### Pearl Dace May #####
data_pd_may <- data_may %>%
  filter(organism=="pearl dace")

ggplot(data_pd_may, aes(x=location, y=lake_coupling, fill=tissue)) + 
  labs(y= "Lake Energy Use", x = "location") +
  geom_boxplot()+
  ggtitle("Pearl Dace May") +  
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line =  element_line(size = 0.5, linetype = "solid",
                                  colour = "black"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size = 18),
        legend.text = element_text(size = 14)
  )+
  scale_fill_manual("Legend", values = c("liver" = "plum4", "muscle" = "slategray3"))


##### Northern Redbelly Dace May #####
data_nrb_may <- data_may %>%
  filter(organism=="northern redbelly dace")

ggplot(data_nrb_may, aes(x=location, y=lake_coupling, fill=tissue)) + 
  labs(y= "Lake Energy Use", x = "location") +
  geom_boxplot()+
  ggtitle("NRB May") +  
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line =  element_line(size = 0.5, linetype = "solid",
                                  colour = "black"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size = 18),
        legend.text = element_text(size = 14)
  )+
  scale_fill_manual("Legend", values = c("liver" = "plum4", "muscle" = "slategray3"))


##### Yellow Perch May #####
data_yp_may <- data_may %>%
  filter(organism=="yellow perch")

ggplot(data_yp_may, aes(x=location, y=lake_coupling, fill=tissue)) + 
  labs(y= "Lake Energy Use", x = "location") +
  geom_boxplot()+
  ggtitle("Yellow Perch May") +  
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line =  element_line(size = 0.5, linetype = "solid",
                                  colour = "black"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size = 18),
        legend.text = element_text(size = 14)
  )+
  scale_fill_manual("Legend", values = c("liver" = "plum4", "muscle" = "slategray3"))

##### SMB May #####
data_smb_may <- data_may %>%
  filter(organism=="smallmouth bass")

ggplot(data_smb_may, aes(x=location, y=lake_coupling, fill=tissue)) + 
  labs(y= "Lake Energy Use", x = "location") +
  geom_boxplot()+
  ggtitle("SMB May") +  
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line =  element_line(size = 0.5, linetype = "solid",
                                  colour = "black"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size = 18),
        legend.text = element_text(size = 14)
  )+
  scale_fill_manual("Legend", values = c("liver" = "plum4", "muscle" = "slategray3"))


################## AUGUST ####################


si_mixing_model_pred_bound_aug <- function(x) {
  iso_data <- x
  
  cr_baseline <- iso_data %>%
    filter(organism %in% c("mayfly", "stonefly", "mussel")) %>%
    filter(location == "creek", month=="august")
  cr_mean_dC <- mean(cr_baseline$d13C_kilj)
  cr_mean_dN <- mean(cr_baseline$d15N)
  
  lk_baseline <- iso_data %>%
    filter(organism %in% c("mayfly", "stonefly", "mussel")) %>%
    filter(location == "lake", month=="august")
  lk_mean_dC <- mean(lk_baseline$d13C_kilj)
  lk_mean_dN <- mean(lk_baseline$d15N)
  
  df_pred <- iso_data %>%
    filter(organism %in% c("common shiner", "golden shiner", "creek chub", "bluntnose minnow", "northern redbelly dace", "pearl dace", "yellow perch", "smallmouth bass"))
  
  for(i in 1:nrow(df_pred)){
    lake_coupling <- ((df_pred$d13C_kilj - cr_mean_dC)/(lk_mean_dC - cr_mean_dC))
    lake_coupling[lake_coupling > 1] <- 0.999
    lake_coupling[lake_coupling < 0] <- 0.001
    TP_pred <- 2 + (((lake_coupling * ((df_pred$d15N - lk_mean_dN)/3.4))) + ((1 - lake_coupling) * ((df_pred$cal.d15N - cr_mean_dN)/3.4)))
    dN_pred <- df_pred$d15N - (((1 - lake_coupling) * cr_mean_dN) + (lake_coupling * lk_mean_dN))
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

aug_tp_lake_coupling <- si_mixing_model_pred_bound_aug(isotope_data)

head(may_tp_lake_coupling)


# Filter the data for august samples
data_aug <- aug_tp_lake_coupling %>%
  filter(month == "august") %>%
  filter(tissue != "N/A")

ggplot(data_aug, aes(x=location, y=lake_coupling, fill=tissue)) + 
  labs(y= "Lake Energy Use", x = "location") +
  geom_boxplot()+
  ggtitle("All Cyprinids Aug") +  
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line =  element_line(size = 0.5, linetype = "solid",
                                  colour = "black"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size = 18),
        legend.text = element_text(size = 14)
  )+
  scale_fill_manual("Legend", values = c("liver" = "plum4", "muscle" = "slategray3"))

aug_anova<- aov(lake_coupling ~ location * tissue, data = data_aug)

summary(aug_anova)

aug_posthoc <- TukeyHSD(aug_anova)

aug_posthoc


##### Golden Shiner Aug #####
data_gs_aug <- data_aug %>%
  filter(organism=="golden shiner")

ggplot(data_gs_aug, aes(x=location, y=lake_coupling, fill=tissue)) + 
  labs(y= "Lake Energy Use", x = "location") +
  geom_boxplot()+
  ggtitle("Golden Shiner Aug") +  
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line =  element_line(size = 0.5, linetype = "solid",
                                  colour = "black"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size = 18),
        legend.text = element_text(size = 14)
  )+
  scale_fill_manual("Legend", values = c("liver" = "plum4", "muscle" = "slategray3"))


#### Common Shiner Aug ####
data_cs_aug <- data_aug %>%
  filter(organism=="common shiner")

ggplot(data_cs_aug, aes(x=location, y=lake_coupling, fill=tissue)) + 
  labs(y= "Lake Energy Use", x = "location") +
  geom_boxplot()+
  ggtitle("Common Shiner Aug") +  
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line =  element_line(size = 0.5, linetype = "solid",
                                  colour = "black"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size = 18),
        legend.text = element_text(size = 14)
  )+
  scale_fill_manual("Legend", values = c("liver" = "plum4", "muscle" = "slategray3"))


##### Creek Chub Aug #####
data_cc_aug <- data_aug %>%
  filter(organism=="creek chub")

ggplot(data_cc_aug, aes(x=location, y=lake_coupling, fill=tissue)) + 
  labs(y= "Lake Energy Use", x = "location") +
  geom_boxplot()+
  ggtitle("Creek Chub Aug") +  
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line =  element_line(size = 0.5, linetype = "solid",
                                  colour = "black"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size = 18),
        legend.text = element_text(size = 14)
  )+
  scale_fill_manual("Legend", values = c("liver" = "plum4", "muscle" = "slategray3"))


##### Bluntnose Minnow Aug #####
data_bnm_aug <- data_aug %>%
  filter(organism=="bluntnose minnow")

ggplot(data_bnm_aug, aes(x=location, y=lake_coupling, fill=tissue)) + 
  labs(y= "Lake Energy Use", x = "location") +
  geom_boxplot()+
  ggtitle("BNM Aug") +  
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line =  element_line(size = 0.5, linetype = "solid",
                                  colour = "black"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size = 18),
        legend.text = element_text(size = 14)
  )+
  scale_fill_manual("Legend", values = c("liver" = "plum4", "muscle" = "slategray3"))


##### Pearl Dace Aug #####
data_pd_aug <- data_aug %>%
  filter(organism=="pearl dace")

ggplot(data_pd_aug, aes(x=location, y=lake_coupling, fill=tissue)) + 
  labs(y= "Lake Energy Use", x = "location") +
  geom_boxplot()+
  ggtitle("Pearl Dace Aug") +  
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line =  element_line(size = 0.5, linetype = "solid",
                                  colour = "black"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size = 18),
        legend.text = element_text(size = 14)
  )+
  scale_fill_manual("Legend", values = c("liver" = "plum4", "muscle" = "slategray3"))


##### Northern Redbelly Dace Aug #####
data_nrb_aug <- data_aug %>%
  filter(organism=="northern redbelly dace")

ggplot(data_nrb_aug, aes(x=location, y=lake_coupling, fill=tissue)) + 
  labs(y= "Lake Energy Use", x = "location") +
  geom_boxplot()+
  ggtitle("NRB Aug") +  
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line =  element_line(size = 0.5, linetype = "solid",
                                  colour = "black"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size = 18),
        legend.text = element_text(size = 14)
  )+
  scale_fill_manual("Legend", values = c("liver" = "plum4", "muscle" = "slategray3"))


##### Yellow Perch Aug #####
data_yp_aug <- data_aug %>%
  filter(organism=="yellow perch")

ggplot(data_yp_aug, aes(x=location, y=lake_coupling, fill=tissue)) + 
  labs(y= "Lake Energy Use", x = "location") +
  geom_boxplot()+
  ggtitle("Yellow Perch Aug") +  
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line =  element_line(size = 0.5, linetype = "solid",
                                  colour = "black"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size = 18),
        legend.text = element_text(size = 14)
  )+
  scale_fill_manual("Legend", values = c("liver" = "plum4", "muscle" = "slategray3"))

##### SMB Aug #####
data_smb_aug <- data_aug %>%
  filter(organism=="smallmouth bass")

ggplot(data_smb_aug, aes(x=location, y=lake_coupling, fill=tissue)) + 
  labs(y= "Lake Energy Use", x = "location") +
  geom_boxplot()+
  ggtitle("SMB Aug") +  
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line =  element_line(size = 0.5, linetype = "solid",
                                  colour = "black"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size = 18),
        legend.text = element_text(size = 14)
  )+
  scale_fill_manual("Legend", values = c("liver" = "plum4", "muscle" = "slategray3"))



################## OCTOBER ####################


si_mixing_model_pred_bound_oct <- function(x) {
  iso_data <- x
  
  cr_baseline <- iso_data %>%
    filter(organism %in% c("mayfly", "stonefly", "mussel")) %>%
    filter(location == "creek", month=="october")
  cr_mean_dC <- mean(cr_baseline$d13C_kilj)
  cr_mean_dN <- mean(cr_baseline$d15N)
  
  lk_baseline <- iso_data %>%
    filter(organism %in% c("mayfly", "stonefly", "mussel")) %>%
    filter(location == "lake", month=="october")
  lk_mean_dC <- mean(lk_baseline$d13C_kilj)
  lk_mean_dN <- mean(lk_baseline$d15N)
  
  df_pred <- iso_data %>%
    filter(organism %in% c("common shiner", "golden shiner", "creek chub", "bluntnose minnow", "northern redbelly dace", "pearl dace", "yellow perch", "smallmouth bass"))
  
  for(i in 1:nrow(df_pred)){
    lake_coupling <- ((df_pred$d13C_kilj - cr_mean_dC)/(lk_mean_dC - cr_mean_dC))
    lake_coupling[lake_coupling > 1] <- 0.999
    lake_coupling[lake_coupling < 0] <- 0.001
    TP_pred <- 2 + (((lake_coupling * ((df_pred$d15N - lk_mean_dN)/3.4))) + ((1 - lake_coupling) * ((df_pred$cal.d15N - cr_mean_dN)/3.4)))
    dN_pred <- df_pred$d15N - (((1 - lake_coupling) * cr_mean_dN) + (lake_coupling * lk_mean_dN))
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

oct_tp_lake_coupling <- si_mixing_model_pred_bound_oct(isotope_data)

head(may_tp_lake_coupling)


# Filter the data for october samples
data_oct <- oct_tp_lake_coupling %>%
  filter(month == "october") %>%
  filter(tissue != "N/A")

ggplot(data_oct, aes(x=location, y=lake_coupling, fill=tissue)) + 
  labs(y= "Lake Energy Use", x = "location") +
  geom_boxplot()+
  ggtitle("All Cyprinids oct") +  
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line =  element_line(size = 0.5, linetype = "solid",
                                  colour = "black"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size = 18),
        legend.text = element_text(size = 14)
  )+
  scale_fill_manual("Legend", values = c("liver" = "plum4", "muscle" = "slategray3"))

oct_anova<- aov(lake_coupling ~ location * tissue, data = data_oct)

summary(oct_anova)

oct_posthoc <- TukeyHSD(oct_anova)

oct_posthoc


##### Golden Shiner oct #####
data_gs_oct <- data_oct %>%
  filter(organism=="golden shiner")

ggplot(data_gs_oct, aes(x=location, y=lake_coupling, fill=tissue)) + 
  labs(y= "Lake Energy Use", x = "location") +
  geom_boxplot()+
  ggtitle("Golden Shiner oct") +  
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line =  element_line(size = 0.5, linetype = "solid",
                                  colour = "black"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size = 18),
        legend.text = element_text(size = 14)
  )+
  scale_fill_manual("Legend", values = c("liver" = "plum4", "muscle" = "slategray3"))


#### Common Shiner oct ####
data_cs_oct <- data_oct %>%
  filter(organism=="common shiner")

ggplot(data_cs_oct, aes(x=location, y=lake_coupling, fill=tissue)) + 
  labs(y= "Lake Energy Use", x = "location") +
  geom_boxplot()+
  ggtitle("Common Shiner oct") +  
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line =  element_line(size = 0.5, linetype = "solid",
                                  colour = "black"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size = 18),
        legend.text = element_text(size = 14)
  )+
  scale_fill_manual("Legend", values = c("liver" = "plum4", "muscle" = "slategray3"))


##### Creek Chub oct #####
data_cc_oct <- data_oct %>%
  filter(organism=="creek chub")

ggplot(data_cc_oct, aes(x=location, y=lake_coupling, fill=tissue)) + 
  labs(y= "Lake Energy Use", x = "location") +
  geom_boxplot()+
  ggtitle("Creek Chub oct") +  
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line =  element_line(size = 0.5, linetype = "solid",
                                  colour = "black"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size = 18),
        legend.text = element_text(size = 14)
  )+
  scale_fill_manual("Legend", values = c("liver" = "plum4", "muscle" = "slategray3"))


##### Bluntnose Minnow oct #####
data_bnm_oct <- data_oct %>%
  filter(organism=="bluntnose minnow")

ggplot(data_bnm_oct, aes(x=location, y=lake_coupling, fill=tissue)) + 
  labs(y= "Lake Energy Use", x = "location") +
  geom_boxplot()+
  ggtitle("BNM oct") +  
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line =  element_line(size = 0.5, linetype = "solid",
                                  colour = "black"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size = 18),
        legend.text = element_text(size = 14)
  )+
  scale_fill_manual("Legend", values = c("liver" = "plum4", "muscle" = "slategray3"))


##### Pearl Dace oct #####
data_pd_oct <- data_oct %>%
  filter(organism=="pearl dace")

ggplot(data_pd_oct, aes(x=location, y=lake_coupling, fill=tissue)) + 
  labs(y= "Lake Energy Use", x = "location") +
  geom_boxplot()+
  ggtitle("Pearl Dace oct") +  
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line =  element_line(size = 0.5, linetype = "solid",
                                  colour = "black"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size = 18),
        legend.text = element_text(size = 14)
  )+
  scale_fill_manual("Legend", values = c("liver" = "plum4", "muscle" = "slategray3"))


##### Northern Redbelly Dace oct #####
data_nrb_oct <- data_oct %>%
  filter(organism=="northern redbelly dace")

ggplot(data_nrb_oct, aes(x=location, y=lake_coupling, fill=tissue)) + 
  labs(y= "Lake Energy Use", x = "location") +
  geom_boxplot()+
  ggtitle("NRB oct") +  
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line =  element_line(size = 0.5, linetype = "solid",
                                  colour = "black"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size = 18),
        legend.text = element_text(size = 14)
  )+
  scale_fill_manual("Legend", values = c("liver" = "plum4", "muscle" = "slategray3"))


##### Yellow Perch oct #####
data_yp_oct <- data_oct %>%
  filter(organism=="yellow perch")

ggplot(data_yp_oct, aes(x=location, y=lake_coupling, fill=tissue)) + 
  labs(y= "Lake Energy Use", x = "location") +
  geom_boxplot()+
  ggtitle("Yellow Perch oct") +  
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line =  element_line(size = 0.5, linetype = "solid",
                                  colour = "black"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size = 18),
        legend.text = element_text(size = 14)
  )+
  scale_fill_manual("Legend", values = c("liver" = "plum4", "muscle" = "slategray3"))

##### SMB oct #####
data_smb_oct <- data_oct %>%
  filter(organism=="smallmouth bass")

ggplot(data_smb_oct, aes(x=location, y=lake_coupling, fill=tissue)) + 
  labs(y= "Lake Energy Use", x = "location") +
  geom_boxplot()+
  ggtitle("SMB May") +  
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line =  element_line(size = 0.5, linetype = "solid",
                                  colour = "black"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size = 18),
        legend.text = element_text(size = 14)
  )+
  scale_fill_manual("Legend", values = c("liver" = "plum4", "muscle" = "slategray3"))

## GS plot
# Combine the data frames for May, August, and October
combined_data_gs <- bind_rows(data_gs_may, data_gs_aug, data_gs_oct)

plot_data_for_location <- function(data, chosen_location) {
  # Ensure 'month' is a factor with levels in the correct order
  data$month <- factor(data$month, levels = c("may", "august", "october"))
  
  data %>%
    filter(.data$location == chosen_location) %>%
    ggplot(aes(x = month, y = lake_coupling, fill = tissue)) +
    geom_boxplot(alpha = 0.4, outlier.shape = NA, position = position_dodge(width = 0.75)) + 
    geom_jitter(position = position_dodge(width = 0.75), aes(color = tissue), size = 1.5) + 
    scale_fill_manual(values = c("liver" = "plum4", "muscle" = "slategray3")) +
    scale_color_manual(values = c("liver" = "plum4", "muscle" = "slategray3")) +
    labs(y = "Lake Energy Use", x = "Month", title = paste("Energy Use in", chosen_location)) +
    theme_minimal() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"),
          panel.background = element_blank(),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          plot.title = element_text(size = 16),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          legend.position = "bottom")
}

# Call the function for 'lake' and 'creek' locations separately
# Plot for "lake" GS
plot_data_for_location(combined_data_gs, "lake")

# Plot for "creek"
plot_data_for_location(combined_data_gs, "creek")

######### YP plot ######
# Combine the data frames for May, August, and October
combined_data_yp <- bind_rows(data_yp_may, data_yp_aug, data_yp_oct)

# Call the function for 'lake' and 'creek' locations separately
# Plot for "lake"
plot_data_for_location(combined_data_yp, "lake")

# Plot for "creek"
plot_data_for_location(combined_data_yp, "creek")
