
## SETUP WORKSPACE FOR THIS ANALYSIS ##
# -> sets output directory           ##
# -> loads data                      ##
# -> loads any existing output files ##
source("04-acc-site-RRS/04-00-setup.R")

# set bootstrap dimensions; use the args object if it exists
if (exists("the_args")) {
  nboot = the_args$nboot 
  ncpu = the_args$ncpu 
} else {
  nboot = 500
  ncpu = max(min(parallel::detectCores() - 2, 5), 1)
}

# run the bootstrap
boot_preds = bootstrap(best_model, dat, nboot = nboot, ncpu = ncpu)

# save the output
saveRDS(boot_preds, file = file.path(out_dir, "boot_preds.rds"))
