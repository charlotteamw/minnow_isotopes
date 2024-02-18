
isotope_data <- read.csv("/Users/charlotteward/Documents/Field Study Files/Isotope_analysis/isotope_analysis_2022/Stable Isotope Mixing Models/Isotope data/2023_isotopes_all.csv", header = T)

library(ggplot2)
library(dplyr)

isotope_data$organism_group <- ifelse(isotope_data$organism %in% c("mayfly", "stonefly", "caddisfly"), "EPT", isotope_data$organism)

isotope_data_base_aug <- isotope_data %>%
  filter(organism_group %in% c("EPT", "mussel")) %>%
  filter(! month %in% c("may")) %>%
  group_by(organism_group, location) %>%
  summarise(
    mean_d13C = mean(d13C_post, na.rm = TRUE),
    sd_d13C = sd(d13C_kilj, na.rm = TRUE),
    mean_d15N = mean(d15N, na.rm = TRUE),
    sd_d15N = sd(d15N, na.rm = TRUE)
  )



aug_data_fish_muscle <- isotope_data %>%
  filter(! (organism == "mussel"|
            organism == "mayfly"|
            organism == "beetle"|
              organism == "caterpillar"|
              organism == "stonefly"|
              organism == "slug"|
              organism == "millipede"|
              organism == "caddisfly"|
              organism == "megaloptera"|
              organism == "odonate"|
              organism == "pill bug"|
              month == "may"|
              tissue == "liver"|
              tissue == "N/A"))

aug_data_fish_liver <- isotope_data %>%
  filter(! (organism == "mussel"|
              organism == "mayfly"|
              organism == "beetle"|
              organism == "caterpillar"|
              organism == "stonefly"|
              organism == "slug"|
              organism == "millipede"|
              organism == "caddisfly"|
              organism == "megaloptera"|
              organism == "odonate"|
              organism == "pill bug"|
              month == "may"|
              tissue == "muscle"|
              tissue == "N/A"))

#Muscle Tissue Biplot

ggplot() + 
  geom_point(data = isotope_data_base_aug, aes(x = mean_d13C, y = mean_d15N, color = location, shape = organism_group), size = 3) +
  geom_errorbar(data = isotope_data_base_aug, aes(x = mean_d13C, color = location, ymin = mean_d15N - sd_d15N, ymax = mean_d15N + sd_d15N), width = 0.1) +
  geom_errorbarh(data = isotope_data_base_aug, aes(y = mean_d15N, color = location, xmin = mean_d13C - sd_d13C, xmax = mean_d13C + sd_d13C), height = 0.1) +
  geom_point(data = aug_data_fish_muscle, aes(x = d13C_kilj, y = d15N, color = location, shape = organism), size = 3) +
  xlim(-36, -20) +
  ylim(0, 10) +
  theme_classic()

#Liver Tissue Biplot
ggplot() + 
  geom_point(data = isotope_data_base_aug, aes(x = mean_d13C, y = mean_d15N, color = location, shape = organism_group), size = 3) +
  geom_errorbar(data = isotope_data_base_aug, aes(x = mean_d13C, color = location, ymin = mean_d15N - sd_d15N, ymax = mean_d15N + sd_d15N), width = 0.1) +
  geom_errorbarh(data = isotope_data_base_aug, aes(y = mean_d15N, color = location, xmin = mean_d13C - sd_d13C, xmax = mean_d13C + sd_d13C), height = 0.1) +
  geom_point(data = aug_data_fish_liver, aes(x = d13C_kilj, y = d15N, color = location, shape = organism), size = 3) +
  xlim(-36, -20) +
  ylim(0, 10) +
  theme_classic()

##### Golden Shiner #####

aug_data_gs_muscle <- aug_data_fish_muscle %>%
  filter(organism == "golden shiner")

ggplot() + 
  geom_point(data = isotope_data_base_aug, aes(x = mean_d13C, y = mean_d15N, color = location, shape = organism_group), size = 3) +
  geom_errorbar(data = isotope_data_base_aug, aes(x = mean_d13C, color = location, ymin = mean_d15N - sd_d15N, ymax = mean_d15N + sd_d15N), width = 0.1) +
  geom_errorbar(data = isotope_data_base_aug, aes(y = mean_d15N, color = location, xmin = mean_d13C - sd_d13C, xmax = mean_d13C + sd_d13C), width = 0.1) +
  geom_point(data = aug_data_gs_muscle, aes(x = d13C_kilj, y = d15N, color = location, shape = organism), size = 3) +
  xlim(-36, -20) +
  ylim(0, 10) +
  theme_classic()+
  labs(title = "Golden Shiner Muscle")

aug_data_gs_liver <- aug_data_fish_liver %>%
  filter(organism == "golden shiner")

ggplot() + 
  geom_point(data = isotope_data_base_aug, aes(x = mean_d13C, y = mean_d15N, color = location, shape = organism_group), size = 3) +
  geom_errorbar(data = isotope_data_base_aug, aes(x = mean_d13C, color = location, ymin = mean_d15N - sd_d15N, ymax = mean_d15N + sd_d15N), width = 0.1) +
  geom_errorbar(data = isotope_data_base_aug, aes(y = mean_d15N, color = location, xmin = mean_d13C - sd_d13C, xmax = mean_d13C + sd_d13C), width = 0.1) +
  geom_point(data = aug_data_gs_liver, aes(x = d13C_kilj, y = d15N, color = location, shape = organism), size = 3) +
  xlim(-36, -20) +
  ylim(0, 10) +
  theme_classic()+
  labs(title = "Golden Shiner Liver")

##### Common Shiner #####

aug_data_cs_muscle <- aug_data_fish_muscle %>%
  filter(organism == "common shiner")

ggplot() + 
  geom_point(data = isotope_data_base_aug, aes(x = mean_d13C, y = mean_d15N, color = location, shape = organism_group), size = 3) +
  geom_errorbar(data = isotope_data_base_aug, aes(x = mean_d13C, color = location, ymin = mean_d15N - sd_d15N, ymax = mean_d15N + sd_d15N), width = 0.1) +
  geom_errorbar(data = isotope_data_base_aug, aes(y = mean_d15N, color = location, xmin = mean_d13C - sd_d13C, xmax = mean_d13C + sd_d13C), width = 0.1) +
  geom_point(data = aug_data_cs_muscle, aes(x = d13C_kilj, y = d15N, color = location, shape = organism), size = 3) +
  xlim(-36, -20) +
  ylim(0, 10) +
  theme_classic()+
  labs(title = "Common Shiner Muscle")

aug_data_cs_liver <- aug_data_fish_liver %>%
  filter(organism == "common shiner")

ggplot() + 
  geom_point(data = isotope_data_base_aug, aes(x = mean_d13C, y = mean_d15N, color = location, shape = organism_group), size = 3) +
  geom_errorbar(data = isotope_data_base_aug, aes(x = mean_d13C, color = location, ymin = mean_d15N - sd_d15N, ymax = mean_d15N + sd_d15N), width = 0.1) +
  geom_errorbar(data = isotope_data_base_aug, aes(y = mean_d15N, color = location, xmin = mean_d13C - sd_d13C, xmax = mean_d13C + sd_d13C), width = 0.1) +
  geom_point(data = aug_data_cs_liver, aes(x = d13C_kilj, y = d15N, color = location, shape = organism), size = 3) +
  xlim(-36, -20) +
  ylim(0, 10) +
  theme_classic()+
  labs(title = "Common Shiner Liver")

##### Creek Chub #####

aug_data_cc_muscle <- aug_data_fish_muscle %>%
  filter(organism == "creek chub")

ggplot() + 
  geom_point(data = isotope_data_base_aug, aes(x = mean_d13C, y = mean_d15N, color = location, shape = organism_group), size = 3) +
  geom_errorbar(data = isotope_data_base_aug, aes(x = mean_d13C, color = location, ymin = mean_d15N - sd_d15N, ymax = mean_d15N + sd_d15N), width = 0.1) +
  geom_errorbar(data = isotope_data_base_aug, aes(y = mean_d15N, color = location, xmin = mean_d13C - sd_d13C, xmax = mean_d13C + sd_d13C), width = 0.1) +
  geom_point(data = aug_data_cc_muscle, aes(x = d13C_kilj, y = d15N, color = location, shape = organism), size = 3) +
  xlim(-36, -20) +
  ylim(0, 10) +
  theme_classic()+
  labs(title = "Creek Chub Muscle")

aug_data_cc_liver <- aug_data_fish_liver %>%
  filter(organism == "creek chub")

ggplot() + 
  geom_point(data = isotope_data_base_aug, aes(x = mean_d13C, y = mean_d15N, color = location, shape = organism_group), size = 3) +
  geom_errorbar(data = isotope_data_base_aug, aes(x = mean_d13C, color = location, ymin = mean_d15N - sd_d15N, ymax = mean_d15N + sd_d15N), width = 0.1) +
  geom_errorbar(data = isotope_data_base_aug, aes(y = mean_d15N, color = location, xmin = mean_d13C - sd_d13C, xmax = mean_d13C + sd_d13C), width = 0.1) +
  geom_point(data = aug_data_cc_liver, aes(x = d13C_kilj, y = d15N, color = location, shape = organism), size = 3) +
  xlim(-36, -20) +
  ylim(0, 10) +
  theme_classic()+
  labs(title = "Creek Chub Liver")


##### Bluntnose Minnow #####

aug_data_bnm_muscle <- aug_data_fish_muscle %>%
  filter(organism == "bluntnose minnow")

ggplot() + 
  geom_point(data = isotope_data_base_aug, aes(x = mean_d13C, y = mean_d15N, color = location, shape = organism_group), size = 3) +
  geom_errorbar(data = isotope_data_base_aug, aes(x = mean_d13C, color = location, ymin = mean_d15N - sd_d15N, ymax = mean_d15N + sd_d15N), width = 0.1) +
  geom_errorbarh(data = isotope_data_base_aug, aes(y = mean_d15N, color = location, xmin = mean_d13C - sd_d13C, xmax = mean_d13C + sd_d13C), height = 0.1) +
  geom_point(data = aug_data_bnm_muscle, aes(x = d13C_kilj, y = d15N, color = location, shape = organism), size = 3) +
  xlim(-36, -20) +
  ylim(0, 10) +
  theme_classic()+
  labs(title = "BNM Muscle")

aug_data_bnm_liver <- aug_data_fish_liver %>%
  filter(organism == "bluntnose minnow")

ggplot() + 
  geom_point(data = isotope_data_base_aug, aes(x = mean_d13C, y = mean_d15N, color = location, shape = organism_group), size = 3) +
  geom_errorbar(data = isotope_data_base_aug, aes(x = mean_d13C, color = location, ymin = mean_d15N - sd_d15N, ymax = mean_d15N + sd_d15N), width = 0.1) +
  geom_errorbarh(data = isotope_data_base_aug, aes(y = mean_d15N, color = location, xmin = mean_d13C - sd_d13C, xmax = mean_d13C + sd_d13C), height = 0.1) +
  geom_point(data = aug_data_bnm_liver, aes(x = d13C_kilj, y = d15N, color = location, shape = organism), size = 3) +
  xlim(-36, -20) +
  ylim(0, 10) +
  theme_classic()+
  labs(title = "BNM Liver")


##### Pearl Dace #####
aug_data_pd_muscle <- aug_data_fish_muscle %>%
  filter(organism == "pearl dace")

ggplot() + 
  geom_point(data = isotope_data_base_aug, aes(x = mean_d13C, y = mean_d15N, color = location, shape = organism_group), size = 3) +
  geom_errorbar(data = isotope_data_base_aug, aes(x = mean_d13C, color = location, ymin = mean_d15N - sd_d15N, ymax = mean_d15N + sd_d15N), width = 0.1) +
  geom_errorbarh(data = isotope_data_base_aug, aes(y = mean_d15N, color = location, xmin = mean_d13C - sd_d13C, xmax = mean_d13C + sd_d13C), height = 0.1) +
  geom_point(data = aug_data_pd_muscle, aes(x = d13C_kilj, y = d15N, color = location, shape = organism), size = 3) +
  xlim(-36, -20) +
  ylim(0, 10) +
  theme_classic()+
  labs(title = "PD muscle")

aug_data_pd_liver <- aug_data_fish_liver %>%
  filter(organism == "pearl dace")

ggplot() + 
  geom_point(data = isotope_data_base_aug, aes(x = mean_d13C, y = mean_d15N, color = location, shape = organism_group), size = 3) +
  geom_errorbar(data = isotope_data_base_aug, aes(x = mean_d13C, color = location, ymin = mean_d15N - sd_d15N, ymax = mean_d15N + sd_d15N), width = 0.1) +
  geom_errorbarh(data = isotope_data_base_aug, aes(y = mean_d15N, color = location, xmin = mean_d13C - sd_d13C, xmax = mean_d13C + sd_d13C), height = 0.1) +
  geom_point(data = aug_data_pd_liver, aes(x = d13C_kilj, y = d15N, color = location, shape = organism), size = 3) +
  xlim(-36, -20) +
  ylim(0, 10) +
  theme_classic()+
  labs(title = "PD liver")

##### Northern Redbelly Dace #####

aug_data_nrb_muscle <- aug_data_fish_muscle %>%
  filter(organism == "northern redbelly dace")

ggplot() + 
  geom_point(data = isotope_data_base_aug, aes(x = mean_d13C, y = mean_d15N, color = location, shape = organism_group), size = 3) +
  geom_errorbar(data = isotope_data_base_aug, aes(x = mean_d13C, color = location, ymin = mean_d15N - sd_d15N, ymax = mean_d15N + sd_d15N), width = 0.1) +
  geom_errorbarh(data = isotope_data_base_aug, aes(y = mean_d15N, color = location, xmin = mean_d13C - sd_d13C, xmax = mean_d13C + sd_d13C), height = 0.1) +
  geom_point(data = aug_data_nrb_muscle, aes(x = d13C_kilj, y = d15N, color = location, shape = organism), size = 3) +
  xlim(-36, -20) +
  ylim(0, 10) +
  theme_classic()+
  labs(title = "NRB muscle")

aug_data_nrb_liver <- aug_data_fish_liver %>%
  filter(organism == "northern redbelly dace")

ggplot() + 
  geom_point(data = isotope_data_base_aug, aes(x = mean_d13C, y = mean_d15N, color = location, shape = organism_group), size = 3) +
  geom_errorbar(data = isotope_data_base_aug, aes(x = mean_d13C, color = location, ymin = mean_d15N - sd_d15N, ymax = mean_d15N + sd_d15N), width = 0.1) +
  geom_errorbarh(data = isotope_data_base_aug, aes(y = mean_d15N, color = location, xmin = mean_d13C - sd_d13C, xmax = mean_d13C + sd_d13C), height = 0.1) +
  geom_point(data = aug_data_nrb_liver, aes(x = d13C_kilj, y = d15N, color = location, shape = organism), size = 3) +
  xlim(-36, -20) +
  ylim(0, 10) +
  theme_classic()+
  labs(title = "NRB liver")

##### Yellow Perch #####
aug_data_yp_muscle <- aug_data_fish_muscle %>%
  filter(organism == "yellow perch")

ggplot() + 
  geom_point(data = isotope_data_base_aug, aes(x = mean_d13C, y = mean_d15N, color = location, shape = organism_group), size = 3) +
  geom_errorbar(data = isotope_data_base_aug, aes(x = mean_d13C, color = location, ymin = mean_d15N - sd_d15N, ymax = mean_d15N + sd_d15N), width = 0.1) +
  geom_errorbarh(data = isotope_data_base_aug, aes(y = mean_d15N, color = location, xmin = mean_d13C - sd_d13C, xmax = mean_d13C + sd_d13C), height = 0.1) +
  geom_point(data = aug_data_yp_muscle, aes(x = d13C_kilj, y = d15N, color = location, shape = organism), size = 3) +
  xlim(-36, -20) +
  ylim(0, 10) +
  theme_classic()+
  labs(title = "Yellow Perch muscle")

aug_data_yp_liver <- aug_data_fish_liver %>%
  filter(organism == "yellow perch")

ggplot() + 
  geom_point(data = isotope_data_base_aug, aes(x = mean_d13C, y = mean_d15N, color = location, shape = organism_group), size = 3) +
  geom_errorbar(data = isotope_data_base_aug, aes(x = mean_d13C, color = location, ymin = mean_d15N - sd_d15N, ymax = mean_d15N + sd_d15N), width = 0.1) +
  geom_errorbarh(data = isotope_data_base_aug, aes(y = mean_d15N, color = location, xmin = mean_d13C - sd_d13C, xmax = mean_d13C + sd_d13C), height = 0.1) +
  geom_point(data = aug_data_yp_liver, aes(x = d13C_kilj, y = d15N, color = location, shape = organism), size = 3) +
  xlim(-36, -20) +
  ylim(0, 10) +
  theme_classic()+
  labs(title = "Yellow Perch Liver")


##### Smallmouth bass #####
aug_data_smb_muscle <- aug_data_fish_muscle %>%
  filter(organism == "smallmouth bass")

ggplot() + 
  geom_point(data = isotope_data_base_aug, aes(x = mean_d13C, y = mean_d15N, color = location, shape = organism_group), size = 3) +
  geom_errorbar(data = isotope_data_base_aug, aes(x = mean_d13C, color = location, ymin = mean_d15N - sd_d15N, ymax = mean_d15N + sd_d15N), width = 0.1) +
  geom_errorbarh(data = isotope_data_base_aug, aes(y = mean_d15N, color = location, xmin = mean_d13C - sd_d13C, xmax = mean_d13C + sd_d13C), height = 0.1) +
  geom_point(data = aug_data_smb_muscle, aes(x = d13C_kilj, y = d15N, color = location, shape = organism), size = 3) +
  xlim(-36, -20) +
  ylim(0, 10) +
  theme_classic()+
  labs(title = "SMB muscle")

aug_data_smb_liver <- aug_data_fish_liver %>%
  filter(organism == "smallmouth bass")

ggplot() + 
  geom_point(data = isotope_data_base_aug, aes(x = mean_d13C, y = mean_d15N, color = location, shape = organism_group), size = 3) +
  geom_errorbar(data = isotope_data_base_aug, aes(x = mean_d13C, color = location, ymin = mean_d15N - sd_d15N, ymax = mean_d15N + sd_d15N), width = 0.1) +
  geom_errorbarh(data = isotope_data_base_aug, aes(y = mean_d15N, color = location, xmin = mean_d13C - sd_d13C, xmax = mean_d13C + sd_d13C), height = 0.1) +
  geom_point(data = aug_data_smb_liver, aes(x = d13C_kilj, y = d15N, color = location, shape = organism), size = 3) +
  xlim(-36, -20) +
  ylim(0, 10) +
  theme_classic()+
  labs(title = "SMB Liver")


