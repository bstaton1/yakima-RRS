
# location to store output files
if (!exists("fig_dir")) fig_dir = "ms-content"
if (!dir.exists(fig_dir)) dir.create(fig_dir)
ppi = 600

# the objects to protect when clearing the workspace
if (exists("protect")) {
  protect = c("ppi", "fig_dir", protect)
} else {
  protect = c("protect", "ppi", "fig_dir")
}

cat("\n  Creating Manuscript Figures")

##### FIGURE 4: 1GEN & 2GEN RRS #####

# print a progress message
cat("\n    Creating Figure 4: 1gen & 2gen RRS")
 
# load the information
protect = c(protect, "boot_preds_1gen", "boot_preds_2gen")
source("01-1gen-RRS/01-00-setup.R"); boot_preds_1gen = boot_preds
source("02a-2gen-RRS/02a-00-setup.R"); boot_preds_2gen = boot_preds

# set the numerator/denominator for each ratio
common_args = list(
  numerator = c(keep_origin = "HOR"),
  denominator = c(keep_origin = "NOR")
)

# calculate ratios: 1gen analysis
RRS_summ_1gen = rbind(
  do.call(summarize_RRS, c(common_args, list(boot_preds = boot_preds_1gen, keep_sex = "M", keep_life_stage = "Jack"))),
  do.call(summarize_RRS, c(common_args, list(boot_preds = boot_preds_1gen, keep_sex = "M", keep_life_stage = "Adult"))),
  do.call(summarize_RRS, c(common_args, list(boot_preds = boot_preds_1gen, keep_sex = "F", keep_life_stage = "Adult")))
)

# calculate ratios: 2gen analysis
RRS_summ_2gen = rbind(
  do.call(summarize_RRS, c(common_args, list(boot_preds = boot_preds_2gen, keep_sex = "M", keep_life_stage = "Jack"))),
  do.call(summarize_RRS, c(common_args, list(boot_preds = boot_preds_2gen, keep_sex = "M", keep_life_stage = "Adult"))),
  do.call(summarize_RRS, c(common_args, list(boot_preds = boot_preds_2gen, keep_sex = "F", keep_life_stage = "Adult")))
)

# simplify group title
RRS_summ_1gen$group = sapply(RRS_summ_1gen$group, function(x) switch(x, "M-Jack" = "Jack", "M-Adult" = "Male", "F-Adult" = "Female"))
RRS_summ_2gen$group = sapply(RRS_summ_2gen$group, function(x) switch(x, "M-Jack" = "Jack", "M-Adult" = "Male", "F-Adult" = "Female"))

# create the figure
ylim = c(0,2)
counter <<- 0

png(file.path(fig_dir, "Fig-4.png"), width = 5.5 * ppi, height = 6 * ppi, res = ppi)
my_par(mfcol = c(3,2), oma = c(2,2,1.5,2.5), cex.axis = 1, xaxs = "i", yaxs = "i")
compare_RRS_plot(RRS_summ_1gen, RS_type = "cond", ylim = ylim, legend = TRUE)
mtext(side = 2, line = 1.5, text = RS_name("cond", grand = FALSE), cex = 0.9)
mtext(side = 3, line = 1.5, "One Generation", font = 2)
compare_RRS_plot(RRS_summ_1gen, RS_type = "nzprb", ylim = ylim)
mtext(side = 2, line = 1.5, text = RS_name("nzprb", grand = FALSE), cex = 0.9)
compare_RRS_plot(RRS_summ_1gen, RS_type = "resp", ylim = ylim)
mtext(side = 2, line = 1.5, text = RS_name("resp", grand = FALSE), cex = 0.9)
mtext(side = 1, outer = TRUE, line = 0.75, text = "Spawn Year", cex = 0.9)

compare_RRS_plot(RRS_summ_2gen, RS_type = "cond", ylim = ylim, yaxis_side = 2)
mtext(side = 3, line = 1.5, "Two Generations", font = 2)
mtext(side = 4, line = 1.75, text = "Grand-Progeny/\nSuccessful Spawner", cex = 0.9)
compare_RRS_plot(RRS_summ_2gen, RS_type = "nzprb", ylim = ylim, yaxis_side = 2)
mtext(side = 4, line = 0.75, text = RS_name("nzprb", grand = TRUE), cex = 0.9)
compare_RRS_plot(RRS_summ_2gen, RS_type = "resp", ylim = ylim, yaxis_side = 2)
mtext(side = 4, line = 0.75, text = RS_name("resp", grand = TRUE), cex = 0.9)

# close the device
junk = dev.off(); if (interactive()) file.show(file.path(fig_dir, "Fig-4.png"))

# reset the variables to protect
protect = protect[-which(protect %in% c("boot_preds_1gen", "boot_preds_2gen"))]


##### FIGURE 6: CROSS TYPE RRS #####

# print a progress message

# prep the workspace for this analysis
source("05-ancestry-RRS/05-00-setup.R")

# set the numerator/denominator for each ratio
common_args = list(
  boot_preds = boot_preds,
  denominator = c(keep_ancestry = "NORxNOR")
)

# calculate ratios: males
RRS_summ_M = rbind(
  do.call(summarize_RRS, c(common_args, list(keep_sex = "M", numerator = c(keep_ancestry = "HORxHOR")))),
  do.call(summarize_RRS, c(common_args, list(keep_sex = "M", numerator = c(keep_ancestry = "HORxNOR")))),
  do.call(summarize_RRS, c(common_args, list(keep_sex = "M", numerator = c(keep_ancestry = "NORxHOR"))))
)

# calculate ratios: females
RRS_summ_F = rbind(
  do.call(summarize_RRS, c(common_args, list(keep_sex = "F", numerator = c(keep_ancestry = "HORxHOR")))),
  do.call(summarize_RRS, c(common_args, list(keep_sex = "F", numerator = c(keep_ancestry = "HORxNOR")))),
  do.call(summarize_RRS, c(common_args, list(keep_sex = "F", numerator = c(keep_ancestry = "NORxHOR"))))
)

# simplify group title
RRS_summ_M$group = RRS_summ_M$numerator
RRS_summ_F$group = RRS_summ_F$numerator

# create the plot figure
ylim = c(0,3)
counter <<- 0
png(file.path(fig_dir, "Fig-7.png"), width = 5 * ppi, height = 6 * ppi, res = ppi)
my_par(mfcol = c(3,2), oma = c(2,2,1.5,0), cex.axis = 1, xaxs = "i", yaxs = "i")
compare_RRS_plot(RRS_summ_M, RS_type = "cond", ylim = ylim, legend = TRUE)
mtext(side = 2, line = 1.5, text = RS_name("cond", grand = is_grand), cex = 0.9)
mtext(side = 3, line = 1.5, "Male Spawners", font = 2)
compare_RRS_plot(RRS_summ_M, RS_type = "nzprb", ylim = ylim)
mtext(side = 2, line = 1.5, text = RS_name("nzprb", grand = is_grand), cex = 0.9)
compare_RRS_plot(RRS_summ_M, RS_type = "resp", ylim = ylim)
mtext(side = 2, line = 1.5, text = RS_name("resp", grand = is_grand), cex = 0.9)
mtext(side = 1, outer = TRUE, line = 0.75, text = "Spawn Year", cex = 0.9)

compare_RRS_plot(RRS_summ_F, RS_type = "cond", ylim = ylim)
mtext(side = 3, line = 1.5, "Female Spawners", font = 2)
compare_RRS_plot(RRS_summ_F, RS_type = "nzprb", ylim = ylim)
compare_RRS_plot(RRS_summ_F, RS_type = "resp", ylim = ylim)

# close the device
junk = dev.off(); if (interactive()) file.show(file.path(fig_dir, "Fig-7.png"))

##### FIGURE 8: 1GEN & 2GEN DEMO BOOST #####

# print a progress message
cat("\n    Creating Figure 8: 1gen & 2gen demo boost")

# load the information
protect = c(protect, "boot_preds_1gen", "boot_preds_2gen")
source("06-1gen-demo-boost/06-00-setup.R"); boot_preds_1gen = boot_preds
source("07-2gen-demo-boost/07-00-setup.R"); boot_preds_2gen = boot_preds

# set the numerator/denominator for each ratio
common_args = list(
  numerator = c(keep_disposition = "Broodstock"),
  denominator = c(keep_disposition = "Natural")
)

# calculate ratios: 1gen analysis
RRS_summ_1gen = rbind(
  do.call(summarize_RRS, c(common_args, list(boot_preds = boot_preds_1gen, keep_sex = "M", keep_life_stage = "Jack"))),
  do.call(summarize_RRS, c(common_args, list(boot_preds = boot_preds_1gen, keep_sex = "M", keep_life_stage = "Adult"))),
  do.call(summarize_RRS, c(common_args, list(boot_preds = boot_preds_1gen, keep_sex = "F", keep_life_stage = "Adult")))
)

# calculate ratios: 2gen analysis
RRS_summ_2gen = rbind(
  do.call(summarize_RRS, c(common_args, list(boot_preds = boot_preds_2gen, keep_sex = "M", keep_life_stage = "Jack"))),
  do.call(summarize_RRS, c(common_args, list(boot_preds = boot_preds_2gen, keep_sex = "M", keep_life_stage = "Adult"))),
  do.call(summarize_RRS, c(common_args, list(boot_preds = boot_preds_2gen, keep_sex = "F", keep_life_stage = "Adult")))
)

# simplify group title
RRS_summ_1gen$group = sapply(RRS_summ_1gen$group, function(x) switch(x, "M-Jack" = "Jack", "M-Adult" = "Male", "F-Adult" = "Female"))
RRS_summ_2gen$group = sapply(RRS_summ_2gen$group, function(x) switch(x, "M-Jack" = "Jack", "M-Adult" = "Male", "F-Adult" = "Female"))

# create the plot figure
ylim = c(0,30)
counter <<- 0

png(file.path(fig_dir, "Fig-8.png"), width = 5.5 * ppi, height = 6 * ppi, res = ppi)
my_par(mfcol = c(3,2), oma = c(2,2,1.5,2.5), cex.axis = 1, xaxs = "i", yaxs = "i")
compare_RRS_plot(RRS_summ_1gen, RS_type = "cond", ylim = ylim, legend = TRUE)
mtext(side = 2, line = 1.5, text = RS_name("cond", grand = FALSE), cex = 0.9)
mtext(side = 3, line = 1.5, "One Generation", font = 2)
compare_RRS_plot(RRS_summ_1gen, RS_type = "nzprb", ylim = ylim)
mtext(side = 2, line = 1.5, text = RS_name("nzprb", grand = FALSE), cex = 0.9)
compare_RRS_plot(RRS_summ_1gen, RS_type = "resp", ylim = ylim)
mtext(side = 2, line = 1.5, text = RS_name("resp", grand = FALSE), cex = 0.9)
mtext(side = 1, outer = TRUE, line = 0.75, text = "Spawn Year", cex = 0.9)

compare_RRS_plot(RRS_summ_2gen, RS_type = "cond", ylim = ylim, yaxis_side = 2)
mtext(side = 3, line = 1.5, "Two Generations", font = 2)
mtext(side = 4, line = 1.75, text = "Grand-Progeny/\nSuccessful Spawner", cex = 0.9)
compare_RRS_plot(RRS_summ_2gen, RS_type = "nzprb", ylim = ylim, yaxis_side = 2)
mtext(side = 4, line = 0.75, text = RS_name("nzprb", grand = TRUE), cex = 0.9)
compare_RRS_plot(RRS_summ_2gen, RS_type = "resp", ylim = ylim, yaxis_side = 2)
mtext(side = 4, line = 0.75, text = RS_name("resp", grand = TRUE), cex = 0.9)

# close the device
junk = dev.off(); if (interactive()) file.show(file.path(fig_dir, "Fig-8.png"))

# reset the variables to protect
protect = protect[-which(protect %in% c("boot_preds_1gen", "boot_preds_2gen"))]

# print an empty line when complete
cat("\n")
