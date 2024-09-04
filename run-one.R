
# SCRIPT TO RUN ALL CODE FOR ONE GLM ANAYSIS
# INTENDED TO BE CALLED VIA COMMAND LINE
# Rscript run-one.R 01-1gen-RRS to run the 1st GLM analysis with all defaults

# start a time for the entire script
stt_all = Sys.time()

# load the arg parser package
library(argparser)

# set up the argument parser
parser = arg_parser("Fit One of the GLM Analyses", hide.opts = TRUE) |>
  
  add_argument("analysis_dir", "Directory containing the analysis to run",
               type = "character", default = NULL) |> 
  
  add_argument("--nboot", "Number of bootstrap iterations (default can take between 10mins-5hrs depending on the analysis and computer specs.",
               type = "numeric", default = 1000) |> 

  add_argument("--ncpu", "Number of CPU cores to use in bootstrap parallel processing, will use `r max(parallel::detectCores() - 2, 1) if this is not available",
               type = "numeric", default = 10) |> 
  
  add_argument("--fit", "Fit the models?", type = "logical", default = TRUE) |> 
  add_argument("--resid", "Create residual plots?", type = "logical", default = TRUE) |> 
  add_argument("--boot", "Run the bootstrap?", type = "logical", default = TRUE) |> 
  add_argument("--plot", "Create the plots?", type = "logical", default = TRUE) |> 
  add_argument("--subdir", "Sub-directory name to use for output", type = "character", default = "output") 

# parse the arguments
the_args = parse_args(parser)

# remove unnecessary arguments (not sure why these are ever included?)
the_args = the_args[!(names(the_args) %in% c("", "help"))]

# determine how many cores are available for parallel processing
n_cores = parallel::detectCores()

# if the requested amount leaves fewer than 2 free cores, 
# set ncpu to have 2 left free
if (the_args$ncpu > (n_cores - 2)) {
  the_args$ncpu = max(n_cores - 2, 1)
}

# create a basic function for printing a section break
dashes = function(n = 80) {paste(c("|", rep("-", n), "|"), collapse = "")}

# print a nice startup message
cat(dashes(), "\n", sep = "")
cat("  -> Executing analysis **", the_args$analysis_dir, "**\n", sep = "")
cat("  -> With **", the_args$nboot, "** bootstrap iterations\n", sep = "")
cat("  -> On **", the_args$ncpu, "** processor cores in parallel\n", sep = "")
cat("  -> Saving output in **", file.path(the_args$analysis_dir, the_args$subdir), "**\n", sep = "")
cat("  -> Started at: ", format(stt_all), "\n", sep = "")
cat(dashes(), "\n", sep = "")

# set objects to protect so they are not removed by scripts later
protect <<- c("protect", "the_args", "dashes", "stt", "stt_all", "my_source")

# a function to source a script
# * prints nice messages about the file name and elapsed times
# * includes option to not run/not print
my_source <<- function(the_file, run, desc, silent = FALSE) {
  base_text = "  DO_DONT DESC: "
  text = base_text |> 
    stringr::str_replace("DO_DONT", ifelse(run, "Running", "Skipping")) |> 
    stringr::str_replace("DESC", desc) |> 
    stringr::str_pad(width = 22, side = "right")
  
  if (!silent) cat(text, the_file, ifelse(run, " ", "\n"), sep = "")
  if (run) {
    stt = Sys.time()
    source(the_file)
    spt = Sys.time()
    elapsed = format(round(spt - stt, 0))
    if (!silent) cat("\n  Elapsed: ", elapsed, "\n", sep = "")
  }
}

# execute the model fitting script if instructed
file.path(the_args$analysis_dir, list.files(the_args$analysis_dir, pattern = "fit-models")) |> 
  my_source(run = the_args$fit, desc = "GLM fits")
cat(dashes(), "\n", sep = "")

# execute the residual diagnostic plotting script if instructed
file.path(the_args$analysis_dir, list.files(the_args$analysis_dir, pattern = "residuals")) |> 
  my_source(run = the_args$resid, desc = "residuals")
cat(dashes(), "\n", sep = "")

# execute the bootstrap script if instructed
file.path(the_args$analysis_dir, list.files(the_args$analysis_dir, pattern = "boot")) |> 
  my_source(run = the_args$boot, desc = "bootstrap")
cat(dashes(), "\n", sep = "")

# execute the plotting script if instructed
file.path(the_args$analysis_dir, list.files(the_args$analysis_dir, pattern = "plot")) |> 
  my_source(run = the_args$plot, desc = "plots")
cat(dashes(), "\n", sep = "")

# print a done message
elapsed = format(round(Sys.time() - stt_all, 0))
cat("  Analysis Complete.\n")
cat("  Elapsed: ", elapsed, "\n", sep = "")
cat(dashes(), "\n", sep = "")
