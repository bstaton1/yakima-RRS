
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

##### FIGURE 5: EXAMPLE CONTINUOUS RELATIONSHIPS #####

# print a progress message
cat("\n    Creating Figure 5: Example Continuous Relationships")

# load the information
source("01-1gen-RRS/01-00-setup.R")

# the specific settings for the example to be shown
the_year = 2007
the_sex = "F"
the_life_stage = "Adult"

# initiate a panel counter; enables automating (a), (b), etc. labels
counter = 0

png(file.path(fig_dir, "Fig-5.png"), width = 7 * ppi, height = 4.5 * ppi, res = ppi)
par(mfrow = c(2,3), mar = c(2.75,2,1.25,0.5), tcl = -0.1, mgp = c(2,0.1,0), lend = "square", ljoin = "mitre", oma = c(0,1,0,0))

for (x_var in c("day_raw", "length_raw")) {
  for (RS_type in c("cond", "nzprb", "resp")) {
    # set the plotting function and xlim to use based on the pred_var type (day or length)
    if (x_var == "day_raw") xlim = range(dat$day_raw[dat$sex == the_sex & dat$year == the_year])
    if (x_var == "length_raw") xlim = range(dat$length_raw[dat$sex == the_sex & dat$year == the_year])

    # set the ylimit
    if (RS_type == "nzprb") ylim = c(0,1) else ylim = c(0,9) #ylim = c(0,max(dat$y_var[dat$sex == s & dat$year == y]))

    # only draw the legend on a specific panel
    if (RS_type == "resp" & x_var == "day_raw") legend_loc = "topright" else legend_loc = NULL

    # create the plot
    RS_v_x_plot(
      dat = dat, boot_preds = boot_preds,
      keep_sex = the_sex, keep_life_stage = the_life_stage, keep_year = the_year,
      x = x_var, RS_type = RS_type, bin_width = set_bin_width(x_var),
      groups = list(keep_origin = "NOR", keep_origin = "HOR"), legend_loc = legend_loc,
      xlim = xlim, ylim = ylim, title = " ", include_letter = FALSE
    )

    counter = counter + 1
    mtext(side = 3, line = 0, text = paste0("(", letters[counter], ") ", RS_name(RS_type, grand = FALSE)), font = 1, adj = 0, cex = 0.8)

    if (x_var == "day_raw" & RS_type == "nzprb") mtext(side = 1, line = 1.5, "Return Day", cex = 0.9)
    if (x_var == "length_raw" & RS_type == "nzprb") mtext(side = 1, line = 1.5, "Spawner Length (mm)", cex = 0.9)
  }
}
mtext(side = 2, outer = TRUE, line = -0.5, "Reproductive Success Measure")

# close the device
junk = dev.off(); if (interactive()) file.show(file.path(fig_dir, "Fig-5.png"))

##### FIGURE 6: CROSS TYPE RRS #####

# print a progress message
cat("\n    Creating Figure 6: Cross Type RRS")

# load the information
source("03-cross-type-RRS/03-00-setup.R")

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
png(file.path(fig_dir, "Fig-6.png"), width = 5 * ppi, height = 4 * ppi, res = ppi)
my_par(cex.axis = 0.9, oma = c(1.1,1.5,0,0), xaxs = "i", yaxs = "i")
compare_RRS_plot(RRS_summ, RS_type = "resp", ylim = ylim, legend = TRUE, label_panel = FALSE, cex.legend = 0.9)
mtext(side = 2, line = 1.35, text = "Progeny/Successful Spawning Pair", cex = 1)
mtext(side = 1, outer = TRUE, line = 0.1, text = "Spawn Year", cex = 1)

# close the device
junk = dev.off(); if (interactive()) file.show(file.path(fig_dir, "Fig-6.png"))

##### FIGURE 7: ANCESTRY TYPE RRS #####

# print a progress message
cat("\n    Creating Figure 7: Ancestry RRS")

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
