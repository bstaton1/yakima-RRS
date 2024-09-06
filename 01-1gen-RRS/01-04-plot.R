
## SETUP WORKSPACE FOR THIS ANALYSIS ##
# -> sets output directory           ##
# -> loads data                      ##
# -> loads any existing output files ##
source("01-1gen-RRS/01-00-setup.R")

# resolution
ppi = 600

##### RS VS. CONTINUOUS #####

# print a progress message
cat("\n    Creating Figures: RS vs. Continuous Variables\n")

# directory
fig_dir = file.path(out_dir, "RS-v-continuous-figs")
if (!dir.exists(fig_dir)) dir.create(fig_dir)

# file name, for replacement with looping variables
base_file = "RS-v-PREDVAR_RSTYPE_LIFESTAGE_SEX.png"

# the different values each looping variable can take on
x_vars = c("length_raw", "day_raw")
sexes = c("M", "F")
life_stages = c("Jack", "Adult")
years = levels(dat$year)
RS_types = c("nzprb", "cond", "resp")

# xaxis label depends on the predictor variable type
xlabs = c(
  "day_raw" = "Return Date",
  "length_raw" = "Fork Length (mm)"
)

# examine what the yaxis limits should be
out = NULL
for (x_var in x_vars) {
  for (year in years) {
    for (sex in sexes) {
      for (life_stage in life_stages) {
        out = c(out, obs_RS_by_x(dat, x_var, keep_year = year, keep_sex = sex, keep_life_stage = life_stage, bin_width = set_bin_width(x_var))$cond)
      }
    }
  }
}

# quantile(out, 0.99, na.rm = TRUE)
# max(out, na.rm = TRUE)
y_max_cond_resp = 9

for (RS_type in RS_types) {
  if (RS_type == "nzprb") ylim = c(0,1) else ylim = c(0,y_max_cond_resp)
  for (x_var in x_vars) {

    for (sex in sexes) {
      for (life_stage in life_stages) {

        # set xaxis limits depending on variable, and by life stage if xaxis is length
        if (x_var == "length_raw") {
          xlim = range(dat$length_raw[dat$life_stage == life_stage])
        } else {
          xlim = NULL
        }

        # skip female jacks
        if (sex == "F" & life_stage == "Jack") next()
        # create the file name using correct identifiers
        file = base_file |>
          stringr::str_replace("PREDVAR", stringr::str_remove(x_var, "_raw$")) |>
          stringr::str_replace("RSTYPE", RS_type) |>
          stringr::str_replace("LIFESTAGE", life_stage) |>
          stringr::str_replace("SEX", sex)
        cat("      ", file, "\n", sep = "")

        # open a graphics device
        png(file.path(fig_dir, file), width = 7.2 * ppi, height = 5 * ppi, res = ppi)

        my_par(mfrow = c(3,4))

        # set the counter: for iterating through the panel letter labels (a), (b), etc.
        counter <<- 0

        legend_loc = "topleft"

        for (year in years) {

          # only draw the legend on the first panel
          if (year != years[1]) legend_loc = NULL

          # create the plot
          RS_v_x_plot(
            dat = dat, boot_preds = boot_preds,
            keep_sex = sex, keep_life_stage = life_stage, keep_year = year,
            x = x_var, RS_type = RS_type, bin_width = set_bin_width(x_var),
            groups = list(keep_origin = "NOR", keep_origin = "HOR"), legend_loc = legend_loc,
            xlim = xlim, ylim = ylim, title = year, include_letter = TRUE
          )
        }

        # draw the "label" panel
        plot(1,1, type = "n", axes = FALSE, ann = FALSE, xlim = c(0,1), ylim = c(0,1))
        text(x = 0.0, y = 0.95, labels = paste0("Life Stage: ", life_stage), pos = 4, cex = 1.5)
        text(x = 0.0, y = 0.75, labels = paste0("Sex: ", sex), pos = 4, cex = 1.5)

        # add main axes labels
        mtext(side = 1, outer = TRUE, line = 0.75, text = xlabs[x_var])
        mtext(side = 2, outer = TRUE, line = 0.5, text = RS_name(RS_type, grand = is_grand))

        # close the device
        dev.off()
        if (interactive()) file.show(file.path(fig_dir, file))
      }
    }
  }
}

##### RS VS. CATEGORICAL #####

# print a progress message
cat("    Creating Figure: RS vs. Categorical Variables")

# features common across all plots
dcast_formula = origin ~ year
cond_ylim = c(0, 6)
nzprb_ylim = c(0,0.9)
resp_ylim = c(0, 5)
counter <<- 0

# open the graphics device
png(file.path(out_dir, "RS-comparisons.png"), width = 7.5 * ppi, height = 6 * ppi, res = ppi)

# graphical parameters
my_par(mfcol = c(3,3), oma = c(2,2,1.5,0))

# jack column
compare_RS_plot(boot_preds, keep_sex = "M", keep_life_stage = "Jack", RS_type = "cond", dcast_formula = dcast_formula, legend = TRUE, ylim = cond_ylim)
mtext(side = 2, line = 1.5, text = RS_name("cond", grand = is_grand), cex = 0.9)
mtext(side = 3, line = 1.5, "Jack Spawners", font = 2)
compare_RS_plot(boot_preds, keep_sex = "M", keep_life_stage = "Jack", RS_type = "nzprb", dcast_formula = dcast_formula, ylim = nzprb_ylim)
mtext(side = 2, line = 1.5, text = RS_name("nzprb", grand = is_grand), cex = 0.9)
compare_RS_plot(boot_preds, keep_sex = "M", keep_life_stage = "Jack", RS_type = "resp", dcast_formula = dcast_formula, ylim = resp_ylim)
mtext(side = 2, line = 1.5, text = RS_name("resp", grand = is_grand), cex = 0.9)

# male column
compare_RS_plot(boot_preds, keep_sex = "M", keep_life_stage = "Adult", RS_type = "cond", dcast_formula = dcast_formula, ylim = cond_ylim)
mtext(side = 3, line = 1.5, "Male Spawners", font = 2)
compare_RS_plot(boot_preds, keep_sex = "M", keep_life_stage = "Adult", RS_type = "nzprb", dcast_formula = dcast_formula, ylim = nzprb_ylim)
compare_RS_plot(boot_preds, keep_sex = "M", keep_life_stage = "Adult", RS_type = "resp", dcast_formula = dcast_formula, ylim = resp_ylim)

# female column
compare_RS_plot(boot_preds, keep_sex = "F", keep_life_stage = "Adult", RS_type = "cond", dcast_formula = dcast_formula, ylim = cond_ylim)
mtext(side = 3, line = 1.5, "Female Spawners", font = 2)
compare_RS_plot(boot_preds, keep_sex = "F", keep_life_stage = "Adult", RS_type = "nzprb", dcast_formula = dcast_formula, ylim = nzprb_ylim)
compare_RS_plot(boot_preds, keep_sex = "F", keep_life_stage = "Adult", RS_type = "resp", dcast_formula = dcast_formula, ylim = resp_ylim)

# x-axis label
mtext(side = 1, outer = TRUE, line = 0.8, text = "Spawn Year", cex = 0.8)

# close the device
dev.off(); if (interactive()) file.show(file.path(out_dir, "RS-comparisons.png"))

##### RRS PLOTS #####

# print a progress message
cat("\n    Creating Figure: RRS vs. Categorical Variables")

# set the numerator/denominator for each ratio
common_args = list(
  boot_preds = boot_preds,
  numerator = c(keep_origin = "HOR"),
  denominator = c(keep_origin = "NOR")
)

# calculate ratios
RRS_summ = rbind(
  do.call(summarize_RRS, c(common_args, list(keep_sex = "M", keep_life_stage = "Jack"))),
  do.call(summarize_RRS, c(common_args, list(keep_sex = "M", keep_life_stage = "Adult"))),
  do.call(summarize_RRS, c(common_args, list(keep_sex = "F", keep_life_stage = "Adult")))
)

# simplify group title
RRS_summ$group = sapply(RRS_summ$group, function(x) switch(x, "M-Jack" = "Jack", "M-Adult" = "Male", "F-Adult" = "Female"))

# create the plot figure
ylim = c(0,2)
counter <<- 0
png(file.path(out_dir, "RRS-comparisons.png"), width = 3.5 * ppi, height = 7.5 * ppi, res = ppi)
my_par(mfcol = c(3,1), cex.axis = 1, xaxs = "i", yaxs = "i")
compare_RRS_plot(RRS_summ, RS_type = "cond", ylim = ylim, legend = TRUE)
mtext(side = 2, line = 1.5, text = RS_name("cond", grand = is_grand, is_RRS = TRUE), cex = 0.9)
compare_RRS_plot(RRS_summ, RS_type = "nzprb", ylim = ylim)
mtext(side = 2, line = 1.5, text = RS_name("nzprb", grand = is_grand, is_RRS = TRUE), cex = 0.9)
compare_RRS_plot(RRS_summ, RS_type = "resp", ylim = ylim)
mtext(side = 2, line = 1.5, text = RS_name("resp", grand = is_grand, is_RRS = TRUE), cex = 0.9)
mtext(side = 1, outer = TRUE, line = 0.75, text = "Spawn Year", cex = 0.9)
dev.off(); if (interactive()) file.show(file.path(out_dir, "RRS-comparisons.png"))
