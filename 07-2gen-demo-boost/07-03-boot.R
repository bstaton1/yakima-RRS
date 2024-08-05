
## SETUP WORKSPACE FOR THIS ANALYSIS ##
# -> sets output directory           ##
# -> loads data                      ##
# -> loads any existing output files ##
source("07-2gen-demo-boost/07-00-setup.R")

# set bootstrap dimensions; use the args object if it exists
if (exists("the_args")) {
  nboot = the_args$nboot 
  ncpu = the_args$ncpu 
} else {
  nboot = 1000
  ncpu = max(min(parallel::detectCores() - 2, 8), 1)
}

# run the bootstrap
boot_preds = bootstrap(best_model, dat, nboot = nboot, ncpu = ncpu)

# save the output
saveRDS(boot_preds, file = file.path(out_dir, "boot_preds.rds"))
