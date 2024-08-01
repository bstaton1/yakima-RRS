
## SETUP WORKSPACE FOR THIS ANALYSIS ##
# -> sets output directory           ##
# -> loads data                      ##
# -> loads any existing output files ##
source("05-ancestry-RRS/05-00-setup.R")

# build the formulae for candidate models
formulae = list(
  ~ 1,
  ~ ancestry,
  ~ year,
  ~ sex,
  ~ ancestry * year * sex,
  ~ ancestry * year * sex + life_stage,
  ~ ancestry * year * sex + length,
  ~ ancestry * year * sex + length * life_stage,
  ~ ancestry * year * sex + day,
  ~ ancestry * year * sex + day + I(day^2),
  ~ ancestry * year * sex + length * life_stage + day + I(day^2),
  ~ ancestry * year * sex + length * life_stage + day * year + I(day^2),
  ~ ancestry * year * sex + length * life_stage + day + I(day^2) * year,
  ~ ancestry * year * sex + length * life_stage + day * year + I(day^2) * year,
  ~ year * sex,
  ~ year * sex + life_stage,
  ~ year * sex + length,
  ~ year * sex + length * life_stage,
  ~ year * sex + day,
  ~ year * sex + day + I(day^2),
  ~ year * sex + length * life_stage + day + I(day^2),
  ~ year * sex + length * life_stage + day * year + I(day^2),
  ~ year * sex + length * life_stage + day + I(day^2) * year,
  ~ year * sex + length * life_stage + day * year + I(day^2) * year
)

# fit all these models
starttime = Sys.time()
fits = lapply(1:length(formulae), function(f) {
  if (f == 1) cat("\n")
  cat("\r                                                                                                               ")
  cat("\r    ~ ", as.character(formulae[[f]])[2], " (GLM ", f, " of ", length(formulae), ")", sep = "")
  fit = fit_model(
    cond_form = formulae[[f]], 
    zi_form = formulae[[f]],
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
AIC_kable(AIC_tab, best_model, markdown = TRUE) 

# save objects
saveRDS(best_model, file.path(out_dir, "best_model.rds"))
saveRDS(fits, file.path(out_dir, "fits.rds"))
write.csv(AIC_tab, file.path(out_dir, "AIC_table.csv"), row.names = FALSE)
