
rm(list = ls(all = TRUE))

##### SETTINGS #####

# SELECT THE ANALYSES TO RUN
# comment out any you do not wish to run
analyses_to_run = c(
  "01",     # 1gen-RRS
  "02a",    # 2gen-RRS
  "02b",    # 2gen-RRS-wF1
  "03",     # cross-type-RRS
  "04",     # acc-site-RRS
  "05",     # ancestry-type
  "06",     # 1gen-demo-boost
  "07",     # 2gen-demo-boost
  "08"      # attr-v-origin
)

# or uncomment this line to skip model fitting/bootstrap
# analyses_to_run = "none"

# SELECT ANALYSIS-LEVEL SETTINGS
nboot = 1000        # number of bootstrap iterations
ncpu = 8            # number of CPUs to use for parallel processing
subdir = "output"   # the subdirectory to place output in each analysis directory

# SELECT OUTPUT TO CREATE
do_MS_figs = TRUE
do_MS_tabs = TRUE
do_MS_supp = TRUE

##### SCRIPT CALLS (NO NEED TO EDIT) #####

### CALL SCRIPTS TO PERFORM EACH ANALYSIS ###

# set the working directory to the project directory
# even if the project isn't active in RStudio session
setwd(rprojroot::find_rstudio_root_file())

# objects to protect (i.e., don't remove at start of each analysis)
protect = c("protect", "analyses_to_run", "all_analyses", "a", "do_MS_figs", "do_MS_tabs", "do_MS_supp", "the_command", "subdir", "the_args", "ms_output_dir")

# define the names of all analyses
all_analyses = c(
  "01"  = "01-1gen-RRS",
  "02a" = "02a-2gen-RRS",
  "02b" = "02b-2gen-RRS-wF1",
  "03"  = "03-cross-type-RRS",
  "04"  = "04-acc-site-RRS",
  "05"  = "05-ancestry-RRS",
  "06"  = "06-1gen-demo-boost",
  "07"  = "07-2gen-demo-boost",
  "08"  = "08-attr-vs-origin/08-fit-plot.R"
)

# swap in the analysis-level settings
the_command = "Rscript run-one.R ANALYSIS --nboot NBOOT --ncpu NCPU --subdir SUBDIR" |> 
  stringr::str_replace("NBOOT", as.character(nboot)) |> 
  stringr::str_replace("NCPU", as.character(ncpu)) |> 
  stringr::str_replace("SUBDIR", subdir)

# loop through each analysis
for (a in names(all_analyses)) {
  
  # if found in requested analyses
  if (a %in% analyses_to_run) {
    
    # if the analysis is not 08, call system() on Rscript
    if (a != "08") {
      # pipe the global command to swap the placeholder for the specific analysis and to call it
      the_command |> 
        stringr::str_replace("ANALYSIS", all_analyses[a]) |> 
        system()
    }
    
    # the whole analysis for 08 is contained in a single R script
    if (a == "08") source(all_analyses[a])
  }
}

### CALL SCRIPTS TO BUILD MANUSCRIPT CONTENT ###
the_args = list(); the_args$subdir = subdir

# set the name of the output subdirectory for manuscript content
ms_output_dir = file.path("ms-content", subdir)

# create the manuscript figure content if requested
if (do_MS_figs) {
  fig_dir = ms_output_dir
  source("ms-content/ms-figs.R")
}

# knit the output tables if requested
if (do_MS_tabs) {
  cat("    Rendering Rmarkdown Output: ms-content/ms-tables.Rmd\n")
  rmarkdown::render("ms-content/ms-tables.Rmd", output_dir = ms_output_dir, quiet = TRUE, params = list(output_subdir = subdir), envir = new.env())
}

# knit the supplemental material if requested
if (do_MS_supp) {
  cat("    Rendering Rmarkdown Output: ms-content/supplement/yakima-RRS-supplement.Rmd\n")
  rmarkdown::render("ms-content/supplement/yakima-RRS-supplement.Rmd", output_dir = ms_output_dir, quiet = TRUE,
                    params = list(output_subdir = subdir), envir = new.env())
}

# print a done message
cat("\n\nDone with all calculations.")
