
library(simmr)
library(readxl)
library(tidyverse)

# Specify the file path to your Excel file
path <- "/Users/charlotteward/Documents/minnow_isotopes/R/data/final data/NC_2022_simmr.xlsx"

# Read the Excel file
ope_costello <- lapply(excel_sheets(path), read_excel, path = path)

targets <- ope_costello[[1]]
sources <-ope_costello[[2]]
TEFs <- ope_costello[[3]]

may_targets <- subset(targets, month == "may") 
may_sources <- subset(sources, month == "may")
may_TEFs <- subset(TEFs, month == "may")

may_simmr <- simmr_load(
  mixtures = may_targets[, 1:2],
  source_names = may_sources$Sources,
  source_means = may_sources[, 4:5],
  source_sds = may_sources[, 6:7],
  correction_means = may_TEFs[, 4:5],
  correction_sds = may_TEFs[, 6:7],
  group = as.factor(paste( may_targets$location, may_targets$tissue))
)

group_names <- levels(may_simmr$group)
print(group_names)

## Model for May samples
may_simmr_out <- simmr_mcmc(may_simmr,   mcmc_control = list(iter = 5000, burn = 1000, thin = 10, n.chain = 4)
)
summary(may_simmr_out)
summary(may_simmr_out, type = "diagnostics")
post_pred <- posterior_predictive(may_simmr_out)
plot(may_simmr_out, type = "density")
plot(may_simmr_out, type = "matrix")


post_pred

prior_viz(may_simmr_out)
summary(may_simmr_out, type = "statistics")


# Combine lake sources
may_simmr_com_source <- combine_sources(may_simmr_out,
                                        to_combine = c( "mussel_lk_may", "mayfly_lk_may"),
                                        new_source_name = "may_lk_inverts"
)

# Combine creek sources
may_simmr_com_source1 <- combine_sources(may_simmr_com_source,
                                         to_combine = c("mussel_cr_may", "mayfly_cr_may"),
                                         new_source_name = "may_cr_inverts"
)

## Creek Caught Fish
#muscle
plot(may_simmr_out$input, group = c(2))
plot(may_simmr_out$input, group = c(4))

#proportions

compare_sources(may_simmr_com_source1,
                source_names = c(
                  "may_cr_inverts",
                  "may_lk_inverts"
                ), group = 1
)

plot(may_simmr_out,
     type = "boxplot",
     title = "simmr output: creek caught fish muscle", 
     group = 2
)
plot(may_simmr_com_source1,
     type = "boxplot",
     title = "simmr output: lake caught fish muscle", 
     group = 4
)

#liver
plot(may_simmr_com_source1$input, group = c(1))
plot(may_simmr_com_source1$input, group = c(3))

#proportions
plot(may_simmr_com_source1,
     type = "boxplot",
     title = "simmr output: creek caught fish liver", 
     group = 1
)
plot(may_simmr_com_source1,
     type = "boxplot",
     title = "simmr output: lake caught fish liver", 
     group = 4
)
