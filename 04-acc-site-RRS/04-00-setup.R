
# clear the workspace
if (exists("protect")) rm(list = setdiff(ls(), protect)) else rm(list = ls(all = TRUE))

# set the random seed -- for completely reproducible results
set.seed(1234)

# load needed functions
source("common-functions.R")

# set the output directory & create it if doesn't exist
the_dir = "04-acc-site-RRS"
if (exists("the_args")) subdir = the_args$subdir else subdir = "output"
out_dir = file.path(the_dir, subdir)
if (!dir.exists(out_dir)) dir.create(out_dir)
is_grand = FALSE

# build the data set for this analysis
dat = build_dataset(rrs_type = "acc_site")

# load all fitted models (if file exists)
inFile = file.path(out_dir, "fits.rds")
if (file.exists(inFile)) fits = readRDS(inFile)

# load best model (if file exists)
inFile = file.path(out_dir, "best_model.rds")
if (file.exists(inFile)) best_model = readRDS(inFile)

# load bootstrapped model-predicted RS values (if file exists)
inFile = file.path(out_dir, "boot_preds.rds")
if (file.exists(inFile)) boot_preds = readRDS(inFile)

# load AIC table (if file exists)
inFile = file.path(out_dir, "AIC_table.csv")
if (file.exists(inFile)) AIC_tab = read.csv(inFile)
rm(inFile)
