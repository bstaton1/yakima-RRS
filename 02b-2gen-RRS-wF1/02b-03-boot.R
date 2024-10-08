
## SETUP WORKSPACE FOR THIS ANALYSIS ##
# -> sets output directory           ##
# -> loads data                      ##
# -> loads any existing output files ##
source("02b-2gen-RRS-wF1/02b-00-setup.R")

# set bootstrap dimensions; use the args object if it exists
if (exists("the_args")) {
  nboot = the_args$nboot 
  ncpu = the_args$ncpu 
} else {
  nboot = 1000
  ncpu = max(min(parallel::detectCores() - 2, 10), 1)
}

# run the bootstrap
boot_preds = bootstrap(best_model, dat, nboot = nboot, ncpu = ncpu, extra_vars = c("origin", "life_stage", "day", "length"))

# save the output
saveRDS(boot_preds, file = file.path(out_dir, "boot_preds.rds"))
