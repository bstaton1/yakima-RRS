
## SETUP WORKSPACE FOR THIS ANALYSIS ##
# -> sets output directory           ##
# -> loads data                      ##
# -> loads any existing output files ##
source("03-cross-type-RRS/03-00-setup.R")

# format the data set for plotting residuals
resid_dat = prep_resid_df(
  dat, best_model, 
  cat_vars = c("year", "cross_type", "jacks_in_cross"), 
  cont_vars = NULL
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
