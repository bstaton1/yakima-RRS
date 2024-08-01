
## SETUP WORKSPACE FOR THIS ANALYSIS ##
# -> sets output directory           ##
# -> loads data                      ##
# -> loads any existing output files ##
source("03-cross-type-RRS/03-00-setup.R")

# resolution
ppi = 600

##### RS VS. CATEGORICAL #####

# print a progress message
cat("\n    Creating Figure: RS vs. Categorical Variables")

# features common across all plots
dcast_formula = cross_type ~ year
resp_ylim = c(0, 2.5)
counter <<- 0

# open the graphics device
png(file.path(out_dir, "RS-comparisons.png"), width = 6 * ppi, height = 3 * ppi, res = ppi)

# graphical parameters
my_par(mfcol = c(1,2), oma = c(1,2,1.5,0), cex.axis = 0.7)

# male is jack column
compare_RS_plot(boot_preds, keep_jacks_in_cross = 1, RS_type = "resp", dcast_formula = dcast_formula, legend = FALSE, ylim = resp_ylim)
mtext(side = 3, line = 1.5, "Male is Jack", font = 2)
mtext(side = 2, line = 1.5, text = RS_name("resp", grand = is_grand, unit = "Spawning Pair"), cex = 0.9)

# male is adult column
compare_RS_plot(boot_preds, keep_jacks_in_cross = 0, RS_type = "resp", dcast_formula = dcast_formula, legend = TRUE, ylim = resp_ylim, cex.legend = 0.6)
mtext(side = 3, line = 1.5, "Male is Adult", font = 2)

# x-axis label
mtext(side = 1, outer = TRUE, line = 0, text = "Spawn Year", cex = 0.8)

# close the device
dev.off(); if (interactive()) file.show(file.path(out_dir, "RS-comparisons.png"))

##### RRS #####

# print a progress message
cat("\n    Creating Figure: RRS vs. Categorical Variables")

# set the numerator/denominator for each ratio
common_args = list(
  boot_preds = boot_preds,
  keep_jacks_in_cross = 0,
  denominator = c(keep_cross_type = "NORxNOR")
)

# calculate ratios
RRS_summ = rbind(
  do.call(summarize_RRS, c(common_args, list(numerator = c(keep_cross_type = "HORxHOR")))),
  do.call(summarize_RRS, c(common_args, list(numerator = c(keep_cross_type = "HORxNOR")))),
  do.call(summarize_RRS, c(common_args, list(numerator = c(keep_cross_type = "NORxHOR"))))
)

# simplify group title
RRS_summ$group = RRS_summ$numerator

# create the plot figure
ylim = c(0.5,1.5)
counter <<- 0
png(file.path(out_dir, "RRS-comparisons.png"), width = 5 * ppi, height = 4 * ppi, res = ppi)
my_par(cex.axis = 0.9, oma = c(1.1,1.5,0,0), xaxs = "i", yaxs = "i")
compare_RRS_plot(RRS_summ, RS_type = "resp", ylim = ylim, legend = TRUE, label_panel = FALSE, cex.legend = 0.9)
mtext(side = 2, line = 1.35, text = "Progeny/Successful Spawning Pair", cex = 1)
mtext(side = 1, outer = TRUE, line = 0.1, text = "Spawn Year", cex = 1)
dev.off(); if (interactive()) file.show(file.path(out_dir, "RRS-comparisons.png"))
