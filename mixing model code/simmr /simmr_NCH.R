library(simmr)
library(readxl)

# Specify the file path to your Excel file
path <- "/Users/charlotteward/Documents/minnow_isotopes/R/data/final data/NCH_2022_simmr.xlsx"

# Read the Excel file
ope_costello <- lapply(excel_sheets(path), read_excel, path = path)

targets <- ope_costello[[1]]
sources <-ope_costello[[2]]
TEFs <- ope_costello[[3]]

may_targets <- subset(targets, month == "may") 

may_ope_simmr <- simmr_load(
  mixtures = may_targets[, 1:3],
  source_names = sources$Sources,
  source_means = sources[, c(3,5,7)],
  source_sds = sources[, c(4,6,8)],
  correction_means = TEFs[, c(3,5,7)],
  correction_sds = TEFs[, c(4,6,8)],
  group = as.factor(paste( may_targets$location,  may_targets$tissue, may_targets$month))
)

group_names <- levels(may_ope_simmr$group)


# Print the group names
print(group_names)

#### only run if needing to rerun model
may_ope_simmr_out <- simmr_mcmc(may_ope_simmr,   mcmc_control = list(iter = 10000, burn = 1000, thin = 10, n.chain = 4)
)                            

summary(may_ope_simmr_out)
summary(may_ope_simmr_out, type = "diagnostics")



## May Bi-plots
#Common Shiner
#muscle
plot(may_ope_simmr_out$input, group = 1, tracers = c(1, 3))
plot(may_ope_simmr_out$input, group = 2, tracers = c(1,3))

compare_sources(may_ope_simmr_out,
                source_names = c("odonate_creek", "odonate_lake"), group = 1
)

compare_sources(may_ope_simmr_out,
                source_names = c("odonate_creek", "odonate_lake"), group = 2
)


compare_sources(ope_simmr_out,
                source_names = c("odonate_cr_may", "odonate_lk_may"), group = 1
)
#liver
plot(simmr_ope_sources_combine4$input, group = 1)

compare_sources(simmr_ope_sources_combine4,
                source_names = c("may_cr_inverts", "may_lk_inverts"), group = 1
)
# Combine first set of sources
simmr_ope_sources_combine1 <- combine_sources(ope_simmr_out,
                                              to_combine = c("odonate_lk_aug", "mussel_lk_aug", "mayfly_lk_aug"),
                                              new_source_name = "august_lk_inverts"
)

# Combine second set of sources
simmr_ope_sources_combine2 <- combine_sources(simmr_ope_sources_combine1,
                                              to_combine = c("odonate_lk_may", "mussel_lk_may", "mayfly_lk_may"),
                                              new_source_name = "may_lk_inverts"
)

# Combine third set of sources
simmr_ope_sources_combine3 <- combine_sources(simmr_ope_sources_combine2,
                                              to_combine = c("odonate_cr_may", "mussel_cr_may", "mayfly_cr_may"),
                                              new_source_name = "may_cr_inverts"
)

# Combine fourth set of sources
simmr_ope_sources_combine4 <- combine_sources(simmr_ope_sources_combine3,
                                              to_combine = c("odonate_cr_aug", "mussel_cr_aug", "mayfly_cr_aug"),
                                              new_source_name = "august_cr_inverts"
)

## May
#Common Shiner
#muscle
plot(simmr_ope_sources_combine4$input, group = 3)

compare_sources(simmr_ope_sources_combine4,
                source_names = c("may_cr_inverts", "may_lk_inverts"), group = 3
)
#liver
plot(simmr_ope_sources_combine4$input, group = 1)

compare_sources(simmr_ope_sources_combine4,
                source_names = c("may_cr_inverts", "may_lk_inverts"), group = 1
)

#Creek Chub
plot(simmr_ope_sources_combine4$input, group = 4)
plot(simmr_ope_sources_combine4$input, group = 5)

#Golden Shiner
plot(simmr_ope_sources_combine4$input, group = 7)
plot(simmr_ope_sources_combine4$input, group = 8)

#Pearl Dace
plot(simmr_ope_sources_combine4$input, group = 12)
plot(simmr_ope_sources_combine4$input, group = 15)

#BNM
plot(simmr_ope_sources_combine4$input, group = 23)
plot(simmr_ope_sources_combine4$input, group = 25)

#SMB
plot(simmr_ope_sources_combine4$input, group = 40)
plot(simmr_ope_sources_combine4$input, group = 42)


## August
#Common Shiner
plot(simmr_ope_sources_combine4$input, group = 2)
plot(simmr_ope_sources_combine4$input, group = 3)
