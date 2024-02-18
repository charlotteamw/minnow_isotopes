isotope_data <- read.csv("/Users/charlotteward/Documents/Field Study Files/Isotope_analysis/isotope_analysis_2022/Stable Isotope Mixing Models/Isotope data/all_data_2022.csv", header = T)

library(ggplot2)
library(dplyr)

isotope_data$organism_group <- ifelse(isotope_data$organism %in% c("odonate"), "EPT",
                                      ifelse(isotope_data$organism %in% c("caterpillar"), "terra", isotope_data$organism))

isotope_data_base <- isotope_data %>%
  filter(organism_group %in% c("EPT",  "mussel", "leaves")) %>%
  group_by(organism_group, location) %>%
  summarise(
    mean_d13C = mean(d_13C, na.rm = TRUE),
    sd_d13C = sd(d_13C, na.rm = TRUE), 
    mean_d2H = mean(d_2H, na.rm = TRUE),
    sd_d2H = sd(d_2H, na.rm = TRUE),
    .groups = "drop" # This line ensures that the grouping is dropped after summarising in dplyr version 1.0.0 and later
  )



may_data_fish_muscle <- isotope_data %>%
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
              month == "august"|
              tissue == "liver"|
              tissue == "N/A"))

may_data_fish_liver <- isotope_data %>%
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
              month == "august"|
              tissue == "muscle"|
              tissue == "N/A"))


### GS
may_data_gs_muscle <- may_data_fish_muscle %>%
  filter(organism == "golden shiner")

ggplot() + 
  geom_point(data = isotope_data_base, aes(x = mean_d2H, y = mean_d13C, color = location, shape = organism_group), size = 3) +
  geom_errorbar(data = isotope_data_base, aes(x = mean_d2H, ymin = mean_d13C - sd_d13C, ymax = mean_d13C + sd_d13C, color = location), width = 0.1) +
  geom_errorbarh(data = isotope_data_base, aes(y = mean_d13C, xmin = mean_d2H - sd_d2H, xmax = mean_d2H + sd_d2H, color = location), height = 0.1) +
  geom_point(data = may_data_gs_muscle, aes(x = d_2H, y = d_13C, color = location, shape = organism), size = 3) +
  xlim(-150, -100) +
  ylim(-35, -21) +
  theme_classic() +
  labs(title = "Golden Shiner Muscle")

may_data_gs_liver <- may_data_fish_liver %>%
  filter(organism == "golden shiner")

ggplot() + 
  geom_point(data = isotope_data_base, aes(x = mean_d2H, y = mean_d13C, color = location, shape = organism_group), size = 3) +
  geom_errorbar(data = isotope_data_base, aes(x = mean_d2H, ymin = mean_d13C - sd_d13C, ymax = mean_d13C + sd_d13C, color = location), width = 0.1) +
  geom_errorbarh(data = isotope_data_base, aes(y = mean_d13C, xmin = mean_d2H - sd_d2H, xmax = mean_d2H + sd_d2H, color = location), height = 0.1) +
  geom_point(data = may_data_gs_liver, aes(x = d_2H, y = d_13C, color = location, shape = organism), size = 3) +
  xlim(-150, -100) +
  ylim(-35, -21) +
  theme_classic() +
  labs(title = "Golden Shiner Liver")



### CC
may_data_cc_muscle <- may_data_fish_muscle %>%
  filter(organism == "creek chub")

ggplot() + 
  geom_point(data = isotope_data_base, aes(x = mean_d2H, y = mean_d13C, color = location, shape = organism_group), size = 3) +
  geom_errorbar(data = isotope_data_base, aes(x = mean_d2H, ymin = mean_d13C - sd_d13C, ymax = mean_d13C + sd_d13C, color = location), width = 0.1) +
  geom_errorbarh(data = isotope_data_base, aes(y = mean_d13C, xmin = mean_d2H - sd_d2H, xmax = mean_d2H + sd_d2H, color = location), height = 0.1) +
  geom_point(data = may_data_cc_muscle, aes(x = d_2H, y = d_13C, color = location, shape = organism), size = 3) +
  xlim(-150, -80) +
  ylim(-35, -21) +
  theme_classic()+
  labs(title = "creek chub muscle")

may_data_cc_liver <- may_data_fish_liver %>%
  filter(organism == "creek chub")

ggplot() + 
  geom_point(data = isotope_data_base, aes(x = mean_d2H, y = mean_d13C, color = location, shape = organism_group), size = 3) +
  geom_errorbar(data = isotope_data_base, aes(x = mean_d2H, ymin = mean_d13C - sd_d13C, ymax = mean_d13C + sd_d13C, color = location), width = 0.1) +
  geom_errorbarh(data = isotope_data_base, aes(y = mean_d13C, xmin = mean_d2H - sd_d2H, xmax = mean_d2H + sd_d2H, color = location), height = 0.1) +
  geom_point(data = may_data_cc_liver, aes(x = d_2H, y = d_13C, color = location, shape = organism), size = 3) +
  xlim(-160, -100) +
  ylim(-35, -21) +
  theme_classic()+
  labs(title = "Creek Chub Liver")

### CS
may_data_cs_muscle <- may_data_fish_muscle %>%
  filter(organism == "common shiner")

ggplot() + 
  geom_point(data = isotope_data_base, aes(x = mean_d2H, y = mean_d13C, color = location, shape = organism_group), size = 3) +
  geom_errorbar(data = isotope_data_base, aes(x = mean_d2H, ymin = mean_d13C - sd_d13C, ymax = mean_d13C + sd_d13C, color = location), width = 0.1) +
  geom_errorbarh(data = isotope_data_base, aes(y = mean_d13C, xmin = mean_d2H - sd_d2H, xmax = mean_d2H + sd_d2H, color = location), height = 0.1) +
  geom_point(data = may_data_cs_muscle, aes(x = d_2H, y = d_13C, color = location, shape = organism), size = 3) +
  xlim(-150, -80) +
  ylim(-35, -21) +
  theme_classic()+
  labs(title = "Common Shiner Muscle")

may_data_cs_liver <- may_data_fish_liver %>%
  filter(organism == "common shiner")

ggplot() + 
  geom_point(data = isotope_data_base, aes(x = mean_d2H, y = mean_d13C, color = location, shape = organism_group), size = 3) +
  geom_errorbar(data = isotope_data_base, aes(x = mean_d2H, ymin = mean_d13C - sd_d13C, ymax = mean_d13C + sd_d13C, color = location), width = 0.1) +
  geom_errorbarh(data = isotope_data_base, aes(y = mean_d13C, xmin = mean_d2H - sd_d2H, xmax = mean_d2H + sd_d2H, color = location), height = 0.1) +
  geom_point(data = may_data_cs_liver, aes(x = d_2H, y = d_13C, color = location, shape = organism), size = 3) +
  xlim(-160, -100) +
  ylim(-35, -21) +
  theme_classic()+
  labs(title = "Common Shiner Liver")

