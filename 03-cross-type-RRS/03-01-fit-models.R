
## SETUP WORKSPACE FOR THIS ANALYSIS ##
# -> sets output directory           ##
# -> loads data                      ##
# -> loads any existing output files ##
source("03-cross-type-RRS/03-00-setup.R")

formulae = list(
  ~ 1,
  ~ cross_type * year,
  ~ cross_type * year + jacks_in_cross,
  ~ cross_type * year + Pa_length + Ma_length,
  ~ cross_type * year + jacks_in_cross + Pa_length + Ma_length,
  ~ cross_type * year + Pa_day + Ma_day,
  
  ~ year,
  ~ year + jacks_in_cross,
  ~ year + Pa_length + Ma_length,
  ~ year + jacks_in_cross + Pa_length + Ma_length,
  ~ year + Pa_day + Ma_day
)

# fit all these models
starttime = Sys.time()
fits = lapply(1:length(formulae), function(f) {
  if (f == 1) cat("\n")
  cat("\r                                                                                                               ")
  cat("\r    ~ ", as.character(formulae[[f]])[2], " (GLM ", f, " of ", length(formulae), ")", sep = "")
  fit = fit_model(
    cond_form = formulae[[f]], 
    zi_form = ~0,
    family = glmmTMB::truncated_nbinom2)
  if (f == length(formulae)) cat("\n")
  return(fit)
})
Sys.time() - starttime

# make the AIC table
AIC_tab = AIC_table(fits)

# extract the best model
best_model = find_best_model(fits)

# print the AIC table
AIC_kable(AIC_tab, best_model, has_zi = FALSE, markdown = TRUE) 

# save objects
saveRDS(best_model, file.path(out_dir, "best_model.rds"))
saveRDS(fits, file.path(out_dir, "fits.rds"))
write.csv(AIC_tab, file.path(out_dir, "AIC_table.csv"), row.names = FALSE)
