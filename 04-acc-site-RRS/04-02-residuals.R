
## SETUP WORKSPACE FOR THIS ANALYSIS ##
# -> sets output directory           ##
# -> loads data                      ##
# -> loads any existing output files ##
source("04-acc-site-RRS/04-00-setup.R")

# format the data set for plotting residuals
resid_dat = prep_resid_df(
  dat, best_model, 
  cat_vars = c("year", "acc_site", "sex", "life_stage"),
  cont_vars = c("day_raw", "length_raw")
)

# get the names of the variables to plot residuals against
vars = colnames(resid_dat)
vars = vars[!vars %in% c("y_var", "QSR")]

# open a PDF graphics device
pdf(file.path(out_dir, "residuals.pdf"), width = 7, height = 4)

# loop through variables, creating the corresponding plot each time
junk = sapply(vars, resid_plot)

# close the device and display if in interactive session
dev.off(); if (interactive()) file.show(file.path(out_dir, "residuals.pdf"))
