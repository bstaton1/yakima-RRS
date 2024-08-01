
# clear the workspace
if (exists("protect")) rm(list = setdiff(ls(), protect)) else rm(list = ls(all = TRUE))

# load needed functions
source("common-functions.R")

# set the output directory
fig_dir = "08-attr-vs-origin"
if (!dir.exists(fig_dir)) dir.create(fig_dir)

# build the data set for this analysis
dat = build_dataset(rrs_type = "single_gen")

# function to perform 2-way ANOVA and calculate
# multiple comparisons between origins by year (w/tukey post-hoc)
compare_attributes = function(response, dat_fit) {
  
  # build the formula to fit the model based on the provided response variable
  ftext = "RESPONSE ~ origin * year"
  f = as.formula(stringr::str_replace(ftext, "RESPONSE", response))
  
  # fit the mode
  fit = lm(f, data = dat_fit)
  
  # get "model fitted" values: for post-hoc test  
  means = emmeans::emmeans(fit, spec = as.formula(paste(as.character(formula(fit))[c(1,3)], collapse = " ")))
  
  # perform tukey post hoc multiple comparisons
  tukey = pairs(means, by = "year")
  
  # get confidence intervals for differences
  ci = confint(tukey)
  
  # coerce to data frame and merge
  tukey_df = as.data.frame(tukey)
  ci_df = as.data.frame(ci)
  ests = merge(tukey_df, ci_df[,c("year", "contrast", "lower.CL", "upper.CL")], by = c("year", "contrast"))
  
  # return the output
  list(fit = fit, ests = ests)
}

# function to create barplot showing estimates, CI's, and stars for significance
compare_attributes_plot = function(ests, main, ylab) {
  
  # get the yaxis limits
  lim = max(abs(ests[,c("lower.CL", "upper.CL")])) * 1.1 * c(-1,1)
  
  # make the barplot
  mp = barplot(ests$estimate, ylim = lim, border = NA,
               ylab = ylab, xlab = "")
  
  # add title
  mtext(side = 3, line = 0, adj = 0, text = main, cex = 0.9)
  
  # get dimensions
  usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])
  
  # draw error bars
  segments(mp, ests$lower.CL, mp, ests$upper.CL)
  
  # draw axes & connectors
  segments(usr[1], usr[3], usr[2], usr[3], xpd = TRUE)
  segments(usr[1], usr[3], usr[1], usr[4], xpd = TRUE)
  axis(side = 1, at = mp, labels = substr(ests$year, 3, 4))
  
  # draw reference line
  abline(h = 0, lty = 2)
  
  # draw significance
  base = pmax(ests$upper.CL, 0)
  text(x = mp, y = base + ydiff * 0.025, labels = ifelse(ests$p.value < 0.05, "*", ""))
}

# perform the analysis comparing return day by origin and year (all spawners, no stratification)
day_compare = compare_attributes("day_raw", dat_fit = dat)

# perform the analysis comparing length by origin and year (stratefied by sex/life stage type)
length_compare_male = compare_attributes("length_raw", dat_fit = subset(dat, life_stage == "Adult" & sex == "M"))
length_compare_female = compare_attributes("length_raw", dat_fit = subset(dat, life_stage == "Adult" & sex == "F"))
length_compare_jack = compare_attributes("length_raw", dat_fit = subset(dat, life_stage == "Jack" & sex == "M"))

# create the plot
ppi = 600
png(file.path(fig_dir, "attr-vs-origin.png"), width = 6.5 * ppi, height = 5 * ppi, res = ppi)
my_par(mfrow = c(2,2), mar = c(2,3.5,1,0.5), oma = c(0.75,0.5,0.5,0))
compare_attributes_plot(day_compare$ests, main = "(a) Day: All Spawners", ylab = "Difference in Mean Return Day\n(HOR - NOR)")
compare_attributes_plot(length_compare_jack$ests, main = "(b) Length: Jack Spawners", ylab = "Difference in Mean Length\n(HOR - NOR)")
compare_attributes_plot(length_compare_male$ests, main = "(c) Length: Male Spawners", ylab = "Difference in Mean Length\n(HOR - NOR)")
compare_attributes_plot(length_compare_female$ests, main = "(d) Length: Female Spawners", ylab = "Difference in Mean Length\n(HOR - NOR)")
mtext(side = 1, outer = TRUE, line = -0.5, "Spawn Year")
dev.off(); if (interactive()) file.show(file.path(fig_dir, "attr-vs-origin.png"))
