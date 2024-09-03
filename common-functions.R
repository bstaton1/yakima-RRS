
# set the default data file to be used
default_inFile = "YakimaParentageInputFile2024_V4.xlsx"

#### DATA PREPARATION FUNCTIONS ####

### standardize_colnames(): STANDARDIZE COLUMN NAMES ###
# not vectorized, use in *apply

standardize_colnames = function(x) {
  switch(
    x,
    "id" = "id",
    "sex" = "sex",
    "year" = "year",
    "origin" = "origin",
    "length" = "length",
    "date" = "date",
    "day" = "day",
    "disposition" = "disposition",
    "life_stage" = "life_stage",
    "total_adult_offs" = "total_offspring",
    "age-3 offspring" = "age3_offspring",
    "age-4 offspring" = "age4_offspring",
    "age-5 offspring" = "age5_offspring",
    "Total_Adult_Grand_Offs" = "total_grand_offspring",
    "Acclimation site Color" = "acc_site_color",
    "Parentage Broodyear" = "acc_site_brood_year",
    "Recoded Acc Site" = "acc_site",
    "Pa_Ma" = "cross_id",
    "No.Offspring" = "total_offspring",
    "Parental_CrossType" = "cross_type",
    "Parental_Spawn_Location" = "spawn_location",
    "Parental_SampleYear" = "year",
    "Pa_Ma_life_stage" = "Pa_Ma_life_stage",
    "Pa_Disposition" = "Pa_disposition",
    "Pa_Origin" = "Pa_origin",
    "Pa_Date" = "Pa_date",
    "Pa_Location" = "Pa_location",
    "Pa_ForkLength" = "Pa_length",
    "Ma_Disposition" = "Ma_disposition",
    "Ma_Origin" = "Ma_origin",
    "Ma_Date" = "Ma_date",
    "Ma_Location" = "Ma_location",
    "Ma_ForkLength" = "Ma_length",
    NA
  )
}

### standardize_origin(): STANDARDIZE ORIGIN NAMES ###

standardize_origin = function(x) {
  stringr::str_replace_all(x, "SH", "HOR") |>
    stringr::str_replace_all("WN", "NOR")
}

### format_dataset(): PERFORM BASIC DATA FORMATTING ###
# - loads the xlsx data file
# - standardizes column headers, factor levels
# - drops missing values
# - keeps only relevant records and variables for a given RRS analysis

format_dataset = function(rrs_type, inFile = default_inFile) {
  
  # return informative error if analysis type is not accepted
  accepted_types = c("single_gen", "multi_gen", "cross_type", "acc_site", "ancestry", "single_gen_demo_boost", "multi_gen_demo_boost")
  if (!rrs_type %in% accepted_types) {
    stop ("rrs_type ", "('", rrs_type, "') not accepted. Accepted options are:\n  ",
          knitr::combine_words(accepted_types, and = "or", before = "'"))
  }
  
  # determine some basic aspects of the data type to build
  is_multi_gen = stringr::str_detect(rrs_type, "multi_gen")
  is_single_gen = !is_multi_gen
  is_demo_boost = stringr::str_detect(rrs_type, "demo_boost")
  
  # step 0: load the data file
  # sheet to extract depends on whether it is the cross_type analysis or something else
  # warnings are about NA values in the Total_Adult_Grand_Offs;
  # these can be ignored because those rows will be filtered out later if column is used
  dat = suppressWarnings({
    readxl::read_excel(inFile, sheet = ifelse(rrs_type == "cross_type", "Yakima Assign Per Cross", "YakimaAssignmentsCorrectedGrand"))
  })
  
  # step 1: improve column names
  colnames(dat) = sapply(colnames(dat), standardize_colnames)
  
  # step 2a: standardize origin/cross types
  origin_vars = colnames(dat)[stringr::str_detect(colnames(dat), "origin|cross_type")]
  new_origins = do.call(data.frame, lapply(origin_vars, function(col) standardize_origin(unlist(dat[,col]))))
  colnames(new_origins) = origin_vars
  dat[,origin_vars] = new_origins
  
  # step 2b: filter to only origins of interest
  #  - acc_site analyses only deal with HOR spawners
  #  - demo_boost analyses only deal with NOR spawners (either taken for broodstock or spawning in wild)
  #  - everything else compares RS by origin, so need both origins
  if (rrs_type == "acc_site") {
    keep_origins = c("HOR")
  } else {
    if (is_demo_boost) {
      keep_origins = c("NOR")
    } else {
      keep_origins = c("HOR", "NOR")
    }
  }
  if (rrs_type == "cross_type") {
    dat = subset(dat, Pa_origin %in% keep_origins & Ma_origin %in% keep_origins)
  } else {
    dat = subset(dat, origin %in% keep_origins)
  }
  
  # step 3: filter years to keep
  #  - if performing multi-generation analysis, keep only earliest years to ensure all F2 returns have been seen
  #  - more years possible for single-generation analyses because only need to wait 5 years to complete cohort
  if (is_multi_gen) {
    keep_years = 2007:2011
  } else {
    keep_years = 2007:2016
  }
  dat = subset(dat, year %in% keep_years)
  
  # step 4: reassign all female "jacks" to female adults
  # models will treat all females the same, regardless of life stage
  if (rrs_type == "cross_type") {
    dat$Pa_life_stage = stringr::str_extract(dat$Pa_Ma_life_stage, "^[:alpha:]+_") |> 
      stringr::str_remove("_")
    dat$Ma_life_stage = stringr::str_extract(dat$Pa_Ma_life_stage, "_[:alpha:]+$") |> 
      stringr::str_remove("_")
    dat$Ma_life_stage[dat$Ma_life_stage == "Jack"] = "Adult"
  } else {
    dat$life_stage[dat$sex == "F" & dat$life_stage == "Jack"] = "Adult"
  }
  
  # step 5: filter life stages to keep
  keep_life_stages = c("Adult", "Jack")
  if (rrs_type == "cross_type") {
    dat = subset(dat, Pa_life_stage %in% keep_life_stages & Ma_life_stage %in% keep_life_stages)
    dat$jacks_in_cross = colSums(apply(dat[,c("Pa_life_stage", "Ma_life_stage")], 1, function(x) x == "Jack"))
    dat$jacks_in_cross = factor(dat$jacks_in_cross, levels = c(0,1))
  } else {
    dat = subset(dat, life_stage %in% keep_life_stages)
  }
  
  # step 6: filter disposition types to keep
  #  - demographic boost calculations compare progeny per naturally spawning fish to progeny per broodstock fish -- need both
  #  - all other analyses compare wild spawning fish, so only need natural
  if (is_demo_boost) {
    keep_dispositions = c("Natural", "Broodstock")
  } else {
    keep_dispositions = "Natural"
  }
  if (rrs_type == "cross_type") {
    dat = subset(dat, Pa_disposition %in% keep_dispositions & Ma_disposition %in% keep_dispositions)
  } else {
    dat = subset(dat, disposition %in% keep_dispositions)
  }
  
  # step 7: handle IDs; formatting only needed for cross_type data sets
  if (rrs_type == "cross_type") {
    dat$Pa_id = stringr::str_extract(dat$cross_id, "^.+\\:") |> 
      stringr::str_remove("\\:")
    dat$Ma_id = stringr::str_extract(dat$cross_id, "\\:.+$") |> 
      stringr::str_remove("\\:")
  }
  
  # step 8: standardize length data
  if (rrs_type == "cross_type") {
    dat$Pa_length_raw = dat$Pa_length
    dat$Ma_length_raw = dat$Ma_length
    dat$Pa_length = scale(dat$Pa_length)
    dat$Ma_length = scale(dat$Ma_length)
  } else {
    dat$length_raw = dat$length
    dat$length = scale(dat$length)
  }
  
  # step 9: standardize day of return data
  if (rrs_type == "cross_type") {
    dat$Pa_day = lubridate::yday(dat$Pa_date)
    dat$Ma_day = lubridate::yday(dat$Ma_date)
    fday = min(dat$Pa_day, dat$Ma_day)
    dat$Pa_day = dat$Pa_day - fday
    dat$Ma_day = dat$Ma_day - fday
    dat$Pa_day_raw = dat$Pa_day + fday
    dat$Ma_day_raw = dat$Ma_day + fday
  } else {
    dat$day = lubridate::yday(dat$date)
    fday = min(dat$day)
    dat$day = dat$day - fday
    dat$day_raw = dat$day + fday
  }
  
  # step 10: decide which meta data columns to keep
  if (rrs_type == "cross_type") {
    keep_vars = c("year", "cross_type", "Pa_id", "Ma_id", "Pa_origin", "Ma_origin", "Pa_life_stage", "Ma_life_stage", "jacks_in_cross", "Pa_length", "Ma_length", "Pa_day", "Ma_day", "Pa_day_raw", "Ma_day_raw", "Pa_length_raw", "Ma_length_raw")
  } else {
    keep_vars = c("year", "id", "origin", "sex", "length", "day", "life_stage", "length_raw", "day_raw")
    if (is_demo_boost) keep_vars = c(keep_vars, "disposition")
    if (rrs_type == "acc_site") {
      keep_vars = c(keep_vars, "acc_site")
      dat$acc_site = stringr::str_remove(dat$acc_site, "Creek|Flat")
      keep_acc_sites = c("Easton", "Clark", "Jack")
      dat = subset(dat, acc_site %in% keep_acc_sites & year >= 2012)
    }
  }
  
  # step 11: decide which offspring variable to keep
  if (is_single_gen) {
    keep_vars = c(keep_vars, "total_offspring")
  } else {
    keep_vars = c(keep_vars, "total_grand_offspring")
  }
  dat = dat[,keep_vars]
  colnames(dat)[ncol(dat)] = "y_var"  # call this variable "y_var"
  
  # ensure this variable is an integer
  dat$y_var = suppressWarnings(as.integer(dat$y_var))
  dat = dat[!is.na(dat$y_var),]
  dat$y_is_zero = dat$y_var == 0
  
  # step 12: coercions
  dat$year = factor(dat$year, levels = sort(unique(dat$year)))
  dat = as.data.frame(dat)
  
  # step 13: return the result
  return(dat)
}

### build_dataset(): BUILD DATA SET FOR SPECIFIC ANALYSIS ###

build_dataset = function(rrs_type, use_F1 = FALSE, inFile = default_inFile) {
  
  # determine whether this is a multigenerational data set
  is_multi_gen = stringr::str_detect(rrs_type, "multi_gen")
  
  # determine whether this is a demographic boost data set
  is_demo_boost = stringr::str_detect(rrs_type, "demo_boost")
  
  # notify user F1 = TRUE will be ignored if single gen
  if (use_F1 & !is_multi_gen) {
    warning("'use_F1' only applies to multi-generational analyses; argument ignored.")
  }
  
  # if this is a multigen analysis and F1 is needed, special prep needed.
  # must first get F1 progeny per spawner, then F2 progeny per spawner, and merge
  if (use_F1 & is_multi_gen) {
    
    # get the number of F1 progeny produced per spawner
    dat1 = format_dataset(rrs_type = paste0("single_gen", ifelse(is_demo_boost, "_demo_boost", "")), inFile = inFile)
    dat1 = dat1[,c("id","y_var")]; colnames(dat1)[2] = "F1"
    
    # get the number of F2 progeny produced per spawner
    dat2 = format_dataset(rrs_type = paste0("multi_gen", ifelse(is_demo_boost, "_demo_boost", "")), inFile = inFile)
    
    # combine them
    dat = merge(dat1, dat2, by = "id"); rm(dat1, dat2)
    dat$F1_is_zero = dat$F1 == 0
    
  } else {
    # if this is the ancestry analysis, special data prep needed.
    # must first get F1 progeny per spawner, then add the attributes of the parents of each spawner
    if (rrs_type == "ancestry") {
      dat = format_dataset(rrs_type = "single_gen") |>
        merge_parent_info(keep_unk_parent_id = FALSE, keep_unk_parent_origin = FALSE) |>
        subset(origin == "NOR" & year %in% 2012:2016) |> 
        flip_parent_order("ancestry")
    } else {
      # otherwise, no special data preparations needed beyond basic data formatting/filtering
      dat = format_dataset(rrs_type = rrs_type, inFile = inFile)
      
      # if the analysis uses cross types, flip the order of the parents in the cross (for females first)
      if (rrs_type == "cross_type") dat = flip_parent_order(dat, "cross_type")
    }
  }
  
  # remove any unused factor levels
  dat = droplevels(dat)
  
  # reset row names
  rownames(dat) = NULL
  
  # return the output
  return(dat)
}

### merge_parent_info(): ADD THE PARENT META DATA FOR EACH SPAWNER IN A DATA SET ###

merge_parent_info = function(dat, inFile = default_inFile, keep_unk_parent_id = FALSE, keep_unk_parent_origin = FALSE) {
  df = suppressWarnings({
    readxl::read_excel(inFile, sheet = "YakimaAssignmentsCorrectedGrand")
  })
  
  # step 1: improve column names
  colnames(df) = sapply(colnames(df), standardize_colnames)
  
  # step 2: standardize origin/cross types
  origin_vars = colnames(df)[stringr::str_detect(colnames(df), "origin|cross_type")]
  new_origins = do.call(data.frame, lapply(origin_vars, function(col) standardize_origin(unlist(df[,col]))))
  colnames(new_origins) = origin_vars
  df[,origin_vars] = new_origins
  df$origin[df$origin == "NA"] = NA
  
  # step 3: force females to be adults
  df$life_stage[df$sex == "F"] = "Adult"
  
  # step 4: split apart parent ids
  df$Pa_id = stringr::str_extract(df$cross_id, "^.+\\:") |> stringr::str_remove("\\:$")
  df$Pa_id[df$Pa_id == "NA"] = NA
  df$Ma_id = stringr::str_extract(df$cross_id, "\\:.+$") |> stringr::str_remove("^\\:")
  df$Ma_id[df$Ma_id == "NA"] = NA
  
  # step 5: build two dataframes: all potential male and female parents
  Pa_attr = df[df$sex == "M",c("id", "year", "life_stage", "origin", "day", "length", "total_offspring")]
  colnames(Pa_attr)[ncol(Pa_attr)] = "F1"; colnames(Pa_attr) = paste0("Pa_", colnames(Pa_attr))
  Ma_attr = df[df$sex == "F",c("id", "year", "life_stage", "origin", "day", "length", "total_offspring")]
  colnames(Ma_attr)[ncol(Ma_attr)] = "F1"; colnames(Ma_attr) = paste0("Ma_", colnames(Ma_attr))
  
  # step 6: merge parent meta data to progeny ids
  df = df[,c("id", "Pa_id", "Ma_id")]
  df = merge(df, Pa_attr, by = "Pa_id", all.x = TRUE, sort = FALSE) |> 
    merge(Ma_attr, by = "Ma_id", all.x = TRUE, sort = FALSE)
  df = df[,c("id", "Pa_id", "Ma_id", "Pa_year", "Ma_year", "Pa_origin", "Ma_origin", "Pa_day", "Ma_day", "Pa_length", "Ma_length", "Pa_F1", "Ma_F1", "Pa_life_stage", "Ma_life_stage")]
  
  # step 7: merge parent meta data to progeny data
  out = merge(dat, df, by = "id", all.x = TRUE)
  
  # step 8: remove fish with unknown parent ID if requested
  if (!keep_unk_parent_id) out = subset(out, !is.na(Pa_id) == "Unk" & !is.na(Ma_id))
  
  # step 9: remove fish with unknown parent origin if requested
  out$Pa_origin[is.na(out$Pa_origin)] = "Unk"
  out$Ma_origin[is.na(out$Ma_origin)] = "Unk"
  if (!keep_unk_parent_origin) out = subset(out, Pa_origin != "Unk" & Ma_origin != "Unk")
  
  # step 10: create ancestry variable
  out$ancestry = paste0(out$Pa_origin, "x", out$Ma_origin)
  
  # return output
  return(out)
}

### flip_parent_order(): REVERSE ORDER OF MALE AND FEMALE PARENT IN CROSS VARIABLES ###
# E.G., if formatted as "NORxHOR" currently output will be formatted "HORxNOR"

flip_parent_order = function(dat, var) {
  
  # stop if var isn't one of "cross_type" or "ancestry"
  if (!var %in% c("cross_type", "ancestry")) stop ("var must be one of 'cross_type' or 'ancestry'")
  
  # extract the correct variable, or return error if var isn't found in dat
  if (any(c("cross_type", "ancestry") %in% colnames(dat))) {
    original = dat[,var]
  } else {
    stop ("neither of 'cross_type' or 'ancestry' found in colnames of dat")
  }
  
  # extract the first and last origin types in the variable
  first = stringr::str_extract(original, "^...")
  last = stringr::str_extract(original, "...$")
  
  # reverse the order and write over the old column
  dat[,var] = paste0(last, "x", first)
  
  # return the data set with flipped order
  dat
}

### unscale(): BACK-TRANSFORM A VARIABLE THAT HAS BEEN CENTERED AND SCALED ### 

unscale = function(x) {
  as.numeric(x * attr(x, "scaled:scale") + attr(x, "scaled:center"))
}

### odds(): CONVERT FROM PROBABILITY TO ODDS SCALE ###

odds = function(x) x/(1 - x)

### day2date(): CONVERT DOY TO DATE FORMAT, ACCORDING TO YEAR ###

day2date = function(day, year) {
  lubridate::as_date(floor(day) - 1, origin = paste0(as.character(year), "-01-01"))
}

##### MODEL FITTING FUNCTIONS #####

### count_***_coefs(): COUNT THE NUMBER OF COEFFICIENTS IN A FITTED SUBMODEL ###

count_cond_coefs = function(fit) if (class(fit) == "glmmTMB") length(glmmTMB::fixef(fit)$cond) else NA
count_zi_coefs = function(fit) if (class(fit) == "glmmTMB") length(glmmTMB::fixef(fit)$zi) else NA

### format_formula(): OBTAIN THE FORMULAS USED IN A FITTED MODEL OBJECT ###

format_formula = function(fit, as_formula = FALSE) {
  out = list(
    cond = as.character(fit$modelInfo$allForm$formula)[3],
    zi = as.character(fit$modelInfo$allForm$ziformula)[2],
    disp = as.character(fit$modelInfo$allForm$dispformula)[2]
  )
  
  if (as_formula) {
    out = lapply(out, function(f) as.formula(paste(c("~", f), collapse = " ")))
  }
  
  return(out)
}

### fit_model(): FIT A GLM ###
# some commonly used defaults are pre-set
# errors/warnings returned don't crash code
# and the specific error/warning text will be returned, along with the model formulas

fit_model = function(cond_form, zi_form = ~0, disp_form = ~1, data = dat, family = glmmTMB::truncated_nbinom2, ...) {
  
  # handle the conditional formula; allow fitting regardless of whether formula provided as:
  # ~ x_vars
  # y_var ~ x_vars
  char_cond_form = as.character(cond_form)
  if (char_cond_form[2] == "y_var") char_cond_corm = char_cond_form[-2]
  cond_form = as.formula(paste(c("y_var", char_cond_form), collapse = " "))
  
  tryCatch(expr = {
    suppressMessages({
      glmmTMB::glmmTMB(
        cond_form,
        ziformula = zi_form, dispformula = disp_form,
        data = data, family = family,
        control = glmmTMB::glmmTMBControl(optCtrl = list(iter.max = 1000, eval.max = 1000)), ...)
    })
  },
  error = function(e) {
    list(
      modelInfo = list(
        allForm = list(
          formula = cond_form,
          ziformula = zi_form,
          dispformula = disp_form
        )), 
      issue = as.character(e))
  }
  ,
  warning = function(w) {
    list(
      modelInfo = list(
        allForm = list(
          formula = cond_form,
          ziformula = zi_form,
          dispformula = disp_form
        )), 
      issue = as.character(w))
  })
}

### AIC_table(): CREATE AN AIC TABLE FROM A LIST OF FITTED GLMS ###

AIC_table = function(fits, include_zi = TRUE) {
  
  AIC_tab = data.frame(
    # get the conditional formula for each model
    cond_form = sapply(fits, function(fit) format_formula(fit)$cond),
    
    # get the zero formula for each model
    zi_form = sapply(fits, function(fit) format_formula(fit)$zi),
    
    # count the number of conditional submodel coefficients in each model
    n_cond_coefs = sapply(fits, count_cond_coefs),
    
    # count the number of zero submodel coefficients in each model
    n_zi_coefs = sapply(fits, count_zi_coefs),
    
    # get the AIC for each model, but return NA if model didn't fit without error/warning
    AIC = sapply(fits, function(x) {if (class(x) == "glmmTMB") AIC(x) else NA})
  ); rownames(AIC_tab) = NULL
  
  # calculate deltaAIC
  AIC_tab$delta = round(AIC_tab$AIC - min(AIC_tab$AIC, na.rm = TRUE), 2)
  
  # drop zero submodel info if instructed
  if (include_zi) {
    output = AIC_tab
  } else {
    output = AIC_tab[,-which(colnames(AIC_tab) %in% c("zi_form", "n_zi_coefs"))]
  }
  
  # return the table (data.frame)
  return(output)
}

### find_best_model(): IDENTIFY THE BEST MODEL IN A LIST OF FITTED GLMS ###
# i.e., that with the fewest parameters within 2 AIC units of lowest AIC model

find_best_model = function(fits) {
  
  # which models returned errors/warnings?
  bad_fits = which(unlist(lapply(fits, class)) != "glmmTMB")
  
  # remove these from consideration as being the best model
  if (length(bad_fits) > 0) {
    fits = fits[-bad_fits]
  }
  
  # obtain AIC scores
  AIC_scores = unlist(lapply(fits, AIC))
  
  # get delta scores
  delta_scores = AIC_scores - min(AIC_scores)
  
  # count the number of parameters per model
  n_params = unlist(lapply(fits, function(fit) count_cond_coefs(fit) + count_zi_coefs(fit)))
  
  # assign models temporary ids
  all_ids = letters[1:length(AIC_scores)]
  names(delta_scores) = names(n_params) = all_ids
  
  # which model ids have delta scores less than or equal to 2?
  ids_lt2 = names(which(delta_scores <= 2))
  
  # of these, which has the fewest parameters?
  id_best = names(which.min(n_params[ids_lt2]))
  
  # return the best model
  fits[[which(all_ids == id_best)]]
}

### make_seq(): CREATE A NUMERIC SEQUENCE THAT MIMICS A VARIABLE ###
# used by make_pred_data() to create sequences of continuous variables to predict at
# x: a vector of numbers for which to build a mimic sequence
# name: the variable name
# n: number of elements in output sequence
#   mean of x will be inserted in the proper place for a total of n+1 elements
# example: make_seq(rnorm(100), name = "variable", n = 5)

make_seq = function(x, name, n = 10) {
  xmin = min(x); xmax = max(x); xmean = mean(x)
  xseq = seq(xmin, xmax, length = n)
  x_out = c(xseq[xseq < xmean], xmean, xseq[xseq > xmean])
  x_out = data.frame(x = x_out, is_mean = x_out == xmean)
  colnames(x_out) = c(name, paste0("is_mean_", name))
  x_out
}

### make_pred_data(): CREATE A PREDICTION DATA SET ###
# includes all combinations of categorical variables
# crossed with sequences of all continuous variables grouped by their respective categorical variable
# only variables in the `fit` object are included by default, name additional variables in `extra_vars`

make_pred_data = function(fit, dat, extra_vars = NULL) {
  
  # extract data used to fit model & remove unneeded variables
  fit_dat = fit$frame
  fit_dat = fit_dat[,-which(colnames(fit_dat) == "y_var")]
  if (!is.data.frame(fit_dat)) fit_dat = data.frame(fit_dat); colnames(fit_dat) = colnames(fit$frame)[colnames(fit$frame) != "y_var"]
  if (any(colnames(fit_dat) == "I(day^2)")) fit_dat = fit_dat[,-which(colnames(fit_dat) == "I(day^2)")]
  
  # add any extra variables requested that aren't already in fit_dat
  if (!is.null(extra_vars)) {
    extra_vars = extra_vars[!extra_vars %in% colnames(fit_dat)]
    original_vars = colnames(fit_dat)
    fit_dat = cbind(fit_dat, dat[,extra_vars])
    colnames(fit_dat) = c(original_vars, extra_vars)
  }
  
  # which variables are factors?
  factors = sapply(fit_dat, function(x) !is.numeric(x))
  
  # what are the levels of the factors?
  if (!is.data.frame(fit_dat[,factors])) {
    factor_levels = list(as.character(unique(fit_dat[,factors])))
    names(factor_levels) = names(factors[factors])
  } else {
    factor_levels = lapply(fit_dat[,factors], function(x) levels(as.factor(x)))
  }
  
  # which variables are continuous?
  continuous = !factors
  
  # create all combinations of factor levels
  combos = do.call(expand.grid, factor_levels)
  colnames(combos) = names(factor_levels)
  vars = colnames(combos)
  
  # remove any instances of Jack Females
  if (all(c("sex", "life_stage") %in% colnames(combos))) combos = combos[-which(combos$sex == "F" & combos$life_stage == "Jack"),]
  
  # if there are any continuos variables, loop through each combo and create sequences from the smallest to largest value for that combo
  # ensures no extrapolation
  if (any(continuous)) {
    
    # loop through combos
    pred_list = lapply(1:nrow(combos), function(i) {
      # subset data set for only records meeting this combo
      tmp = NULL
      for (j in 1:ncol(combos)) {
        if (j == 1) use = dat else use = tmp
        tmp = use[use[,vars[j]] == combos[i,j],]
      }
      
      # create the day sequence, if day is in model
      if ("day" %in% colnames(fit_dat)) {
        day_seq = make_seq(tmp$day, "day")
        day_seq$day_raw = day_seq$day + min(dat$day_raw)
        day_seq = cbind(
          do.call(rbind, replicate(nrow(day_seq), combos[i,], simplify = FALSE)),
          day_seq
        )
        out_seq = day_seq
      } else {
        out_seq = NULL
      }
      
      # create the length sequence, if length is in model
      if ("length" %in% colnames(fit_dat)) {
        length_seq = make_seq(tmp$length, "length")
        length_seq$length_raw = length_seq$length * sd(dat$length_raw) + mean(dat$length_raw)
        length_seq = cbind(
          do.call(rbind, replicate(nrow(length_seq), combos[i,], simplify = FALSE)),
          length_seq
        )
        if (is.null(out_seq)) out_seq = length_seq else out_seq = merge(out_seq, length_seq)
      }
      
      # create the F1 sequence, if F1 is in model
      if ("F1" %in% colnames(fit_dat)) {
        F1_seq = make_seq(tmp$F1, "F1")
        F1_seq = cbind(
          do.call(rbind, replicate(nrow(F1_seq), combos[i,], simplify = FALSE)),
          F1_seq
        )
        if (!is.null(out_seq)) out_seq = merge(out_seq, F1_seq) else out_seq = F1_seq
      }
      
      out_seq
    })
    pred_data = do.call(rbind, pred_list)
  } else {
    pred_data = combos
  }
  
  # remove any vestigial rownames
  rownames(pred_data) = NULL
  
  # return the output
  return(pred_data)
}

### predict_model(): PREDICT FROM A FITTED GLM GIVEN A DATA SET ###
# returns predictions of the response, 1 - prZero, and conditional

predict_model = function(fit, pred_data, ...) {
  
  # build the prediction data set
  id.vars = colnames(pred_data)
  
  # obtain expected values at each combo of prediction covariates
  # fit will be NULL if a model fitting error/warning was returned
  if (class(fit) == "glmmTMB") {
    pred_data$cond = predict(fit, pred_data, type = "conditional", ...)
    pred_data$resp = predict(fit, pred_data, type = "response", ...)
    pred_data$nzprb = 1 - predict(fit, pred_data, type = "zprob", ...)
  } else {
    pred_data$cond = NA
    pred_data$resp = NA
    pred_data$nzprb = NA
  }
  
  # convert to long format across prediction types
  pred_data = reshape2::melt(pred_data, id.vars = id.vars)
  
  # split up identifiers and numerical output
  out = list(
    id = pred_data[,-which(colnames(pred_data) == "value")],
    out = pred_data$value
  )
  
  # return the output
  return(out)
}

### simulate_model(): SIMULATE A DATA SET FROM A FITTED GLM ###
# uses same covariate data, but simulates a new response vector 
# following model assumptions and estimated parameters

simulate_model = function(fit) {
  
  # simulate data from the model
  newresp = unlist(simulate(fit), use.names = FALSE)
  
  # extract the original data used to fit the model
  newdat = fit$frame
  
  # replace the original response with the simulated response
  newdat$y_var = newresp
  
  # return the new data set
  return(newdat)
}

### bootstrap(): PERFORM A PARAMETRIC BOOTSTRAP FOR A FITTED GLM ###

bootstrap = function(fit, dat, nboot = 500, ncpu = max(parallel::detectCores() - 2, 1), extra_vars = NULL) {
  
  # any object passed to the cluster must be assigned to the global environment (i.e., <<- )
  # otherwise the cluster won't be able to find it
  
  # make a list with the model to be bootstrapped as the element
  boot_list <<- replicate(nboot, fit, simplify = FALSE)
  
  # extract the model formulae from the fitted object
  formulae <<- format_formula(best_model, as_formula = TRUE)
  
  # create the prediction data set for this model
  # contains all combinations of factors, as well as sequences of continuous variables at ranges of each combo
  # also contains mean continuous values of each combo
  pred_data <<- make_pred_data(fit = best_model, dat = dat, extra_vars = extra_vars)
  
  # assign functions created elsewhere to global objects
  # otherwise cluster won't be able to find them
  fit_model <<- fit_model
  simulate_model <<- simulate_model
  predict_model <<- predict_model
  
  # start a timer
  # starttime = Sys.time()
  
  # initialize a cluster for parallel computing
  my_cluster = snow::makeSOCKcluster(ncpu)
  
  # send the packages to the cluster, and suppress the output from being printed to console
  invisible({
    snow::clusterEvalQ(my_cluster, {library("glmmTMB")})
  })
  
  # send the necessary objects to the cluster
  snow::clusterExport(my_cluster, c("dat", "boot_list", "formulae", "fit_model", "simulate_model", "pred_data", "predict_model"))
  
  # run the bootstrap
  boot_out = snow::parLapply(my_cluster, x = boot_list, fun = function(fit) {
    
    # refit model to simulated data
    refit = fit_model(
      cond_form = formulae$cond,
      zi_form = formulae$zi,
      disp_form = formulae$disp,
      family = fit$modelInfo$family,
      data = simulate_model(fit)
    )
    
    # obtain model-predicted values from the refitted model
    preds = predict_model(fit = refit, pred_data = pred_data)
    
    # return just the numerical predictions, the covariate values will be added later
    preds$out
  })
  
  # stop the cluster and timer
  snow::stopCluster(my_cluster)
  # stoptime = Sys.time()
  # cat("\nElapsed:", format(stoptime - starttime, digits = 2))
  
  # get the prediction values for the original model
  original_preds = predict_model(best_model, pred_data)
  
  # combine bootstrap prediction values output as columns
  boot_preds = do.call(cbind, boot_out)
  
  # add on the original predicted values to the bootstrapped predicted values
  boot_preds = cbind(original_preds$out, boot_preds)
  
  # add iteration number identifiers
  colnames(boot_preds) = paste0("iter_", 0:nboot)
  boot_preds = as.data.frame(boot_preds)
  
  # add the predictor variable values
  boot_preds = cbind(original_preds$id, boot_preds)
  
  # convert to long format
  boot_preds = reshape2::melt(boot_preds, id.vars = colnames(original_preds$id), value.name = "value", variable.name = "iter")
  
  # count how many iterations failed (i.e., returned error or warning upon calling fit_model())
  cat("\n    Iters Failed:", sum(is.na(boot_preds[!duplicated(boot_preds$iter),"value"])))
  
  # return output
  return(boot_preds)
}

##### SUMMARIZATION FUNCTIONS #####

### my_filter(): FLEXIBLY FILTER A DATA.FRAME SPECIFIC TO THIS ANALYSIS ###

# the data frame may be a data set, model-predicted values, bootstrapped output, etc.
# all categorical variables that could be filtered by are NULL by default -- if present and NULL, will include all levels of that variable
# otherwise, pass the levels you wish to keep to the keep_ argument.
# for continuous variables, set TRUE to use the mean value (for prediction/boostrap output only), FALSE to return all values
# for bootstrapped output, set use_only_iter_0 = TRUE to include only the fit to the original data set
# for bootstrapped output, set drop_iter_0 = TRUE to include only fits to simulated data sets

# for example, to keep only male adult records (all years, all origins) from the dataset called `dat`,
# my_filter(dat, keep_life_stage = "Adult", keep_sex = "M")

my_filter = function(x, keep_year = NULL, keep_origin = NULL, keep_sex = NULL, keep_life_stage = NULL, keep_jacks_in_cross = NULL,
                     keep_cross_type = NULL, keep_acc_site = NULL, keep_disposition = NULL, keep_ancestry = NULL, 
                     use_mean_day = TRUE, use_mean_length = TRUE, use_mean_F1 = TRUE, use_only_iter_0 = FALSE, drop_iter_0 = FALSE) {
  
  # what are the variables in the data frame?
  vars = colnames(x)
  
  # handle years
  if ("year" %in% vars) {
    if (is.null(keep_year)) keep_year = unique(x$year)
    x = subset(x, year %in% keep_year)
  } 
  
  # handle origins
  if ("origin" %in% vars) {
    if (is.null(keep_origin)) keep_origin = unique(x$origin)
    x = subset(x, origin %in% keep_origin)
  }
  
  # handle sexes
  if ("sex" %in% vars) {
    if (is.null(keep_sex)) keep_sex = unique(x$sex)
    x = subset(x, sex %in% keep_sex)
  }
  
  # handle life stages
  if ("life_stage" %in% vars) {
    if (is.null(keep_life_stage)) keep_life_stage = unique(x$life_stage)
    x = subset(x, life_stage %in% keep_life_stage)
  }
  
  # handle jacks in cross
  if ("jacks_in_cross" %in% vars) {
    if (is.null(keep_jacks_in_cross)) keep_jacks_in_cross = unique(x$jacks_in_cross)
    x = subset(x, jacks_in_cross %in% keep_jacks_in_cross)
  }
  
  # handle jacks in cross
  if ("cross_type" %in% vars) {
    if (is.null(keep_cross_type)) keep_cross_type = unique(x$cross_type)
    x = subset(x, cross_type %in% keep_cross_type)
  }
  
  # handle acclimation sites
  if ("acc_site" %in% vars) {
    if (is.null(keep_acc_site)) keep_acc_site = unique(x$acc_site)
    x = subset(x, acc_site %in% keep_acc_site)
  }
  
  # handle disposition types
  if ("disposition" %in% vars) {
    if (is.null(keep_disposition)) keep_disposition = unique(x$disposition)
    x = subset(x, disposition %in% keep_disposition)
  }
  
  # handle parental cross types
  if ("ancestry" %in% vars) {
    if (is.null(keep_ancestry)) keep_ancestry = unique(x$ancestry)
    x = subset(x, ancestry %in% keep_ancestry)
  }
  
  # handle whether to return only the mean value for continuous predictors
  if ("is_mean_day" %in% vars & use_mean_day) x = subset(x, is_mean_day)
  if ("is_mean_length" %in% vars & use_mean_length) x = subset(x, is_mean_length)
  if ("is_mean_F1" %in% vars & use_mean_F1) x = subset(x, is_mean_F1)
  
  # handle whether to use only iter_0 or exclude iter_0
  if ("iter" %in% vars) {
    if (use_only_iter_0) x = subset(x, iter == "iter_0")
    if (drop_iter_0) x = subset(x, iter != "iter_0")
  }
  
  # return the data frame
  return(x)
}

### summarize_RS(): SUMMARIZE REPRODUCTIVE SUCCESS ESTIMATES FOLLOWING BOOTSTRAP ###
# boot_preds: bootstrap output
# ...: keep_ arguments passed to my_filter()
# bootstrapped output will be summarized for each unique combination of variables

summarize_RS = function(boot_preds, ...) {
  
  # format the arguments passed to my_filter
  args = list(...) |> unlist()
  args = args[stringr::str_detect(names(args), "^keep")]
  group = paste(args, collapse = "-")
  
  # data filtering/grouping
  x = boot_preds |> 
    
    # keep only relevant rows
    my_filter(...) |> 
    
    # group by all variables present, except for "iter" and "value"
    dplyr::group_by_at(dplyr::vars(-iter, -value)) 
  
  # summarize bootstrap iters
  boot_summ = x |> 
    
    # keep only bootstrap iters
    my_filter(drop_iter_0 = TRUE, ...) |> 
    
    # summarize all iterations
    dplyr::summarize(lwr = quantile(value, 0.025, na.rm = TRUE),
                     upr = quantile(value, 0.975, na.rm = TRUE),
                     .groups = "drop"
    )
  
  # summarize point ests
  orig_summ = x |> 
    
    # keep only bootstrap iters
    my_filter(use_only_iter_0 = TRUE, ...) |> 
    
    # summarize all iterations
    dplyr::summarize(mean = value, .groups = "drop")
  
  # extract the variables
  orig_vars = boot_preds |> 
    dplyr::filter(iter == "iter_0" & variable == "cond") |> 
    dplyr::select(-iter, -value, -variable) |> 
    dplyr::mutate(group = group)
  
  # combine and return
  merge(orig_vars, orig_summ, sort = FALSE) |> 
    merge(boot_summ, sort = FALSE)
  
}

### summarize_RRS(): SUMMARIZE RELATIVE REPRODUCTIVE SUCCESS ESTIMATES FOLLOWING BOOTSTRAP ###
# boot_preds: bootstrap output
# numerator: named list of keep_ arguments to supply to my_filter(), used in specifying the attributes of the group (those not supplied to ...) in the numerator of the RRS ratio
# denominator: named list of keep_ arguments to supply to my_filter(), used in specifying the attributes of the group (those not supplied to ...) in the denominator of the RRS ratio
# ...: keep_ arguments passed to my_filter(), apply to both numerator and denominator
# bootstrapped output will be summarized for each unique combination of variables

summarize_RRS = function(boot_preds, numerator = c(keep_origin = "HOR"), denominator = c(keep_origin = "NOR"), ...) {
  
  # format the arguments passed to my_filter
  args = list(...) |> unlist()
  args = args[stringr::str_detect(names(args), "^keep")]
  group = paste(args, collapse = "-")
  
  x = boot_preds |> 
    
    # keep only relevant rows
    my_filter(...) |> 
    
    # create an "RS" variable -- converts to odds if is a probability
    dplyr::mutate(RS = ifelse(variable == "nzprb", odds(value), value))
  
  # extract/format the values for the numerator class
  x_numerator = do.call(my_filter, c(list(x = x), as.list(numerator))) |>
    dplyr::select(year, variable, iter, RS) |>
    dplyr::rename(RS_numerator = RS)

  # extract/format the values for the numerator class
  x_denominator = do.call(my_filter, c(list(x = x), as.list(denominator))) |>
    dplyr::select(year, variable, iter, RS) |>
    dplyr::rename(RS_denominator = RS)

  # combine them and calculate RRS
  x_combined = merge(x_numerator, x_denominator, by = c("year", "variable", "iter"), sort = FALSE) |>
    dplyr::mutate(RRS = RS_numerator/RS_denominator) |>
    dplyr::select(year, variable, iter, RRS)

  # summarize bootstrapped values
  boot_summ = x_combined |>
    my_filter(drop_iter_0 = TRUE) |>
    dplyr::group_by(year, variable) |>
    dplyr::summarize(lwr = quantile(RRS, 0.025, na.rm = TRUE), upr = quantile(RRS, 0.975, na.rm = TRUE), .groups = "drop")

  # summarize original fitted values
  orig_summ = x_combined |>
    my_filter(use_only_iter_0 = TRUE) |>
    dplyr::group_by(year, variable) |>
    dplyr::summarize(mean = RRS, .groups = "drop")

  # combine and return
  merge(orig_summ, boot_summ, sort = FALSE) |>
    dplyr::mutate(group = group, numerator = numerator) |>
    dplyr::select(numerator, group, year, variable, mean, lwr, upr)
}

### format_day_bins(): CREATE RETURN DAY BINS WITH PRETTIER NAMES ###
# takes the output of cut() when applied to numerical day variable
# and returns the bin names in terms of dates
# only used for plotting

format_day_bins = function(day_bins, year = 2007) {
  
  old_bins = levels(day_bins)
  
  # format the first day of each bin
  first = old_bins |> 
    stringr::str_extract("[:digit:]+,") |> 
    stringr::str_remove(",") |> 
    as.numeric() |> 
    day2date(year = year)
  first = paste0(lubridate::month(first), "/", lubridate::day(first))
  
  # format the last day of each bin
  last = old_bins |> 
    stringr::str_extract(",[:digit:]+") |> 
    stringr::str_remove(",") |> 
    as.numeric() |> 
    day2date(year = year)
  last = paste0(lubridate::month(last), "/", lubridate::day(last))
  
  # combine
  new_bins = paste0(
    stringr::str_extract(old_bins, "^."),
    first, ",", last, 
    stringr::str_extract(old_bins, ".$")
  )
  
  names(new_bins) = old_bins
  new_day_bins = unname(new_bins[day_bins])
  factor(new_day_bins, levels = new_bins)
}

### set_bin_width(): ASSIGN A BIN WIDTH DEPENDING ON A VARIABLE ###
# allows changing this just one place; only used for plotting

set_bin_width = function(x_var) {
  switch(x_var, "F1" = 1, "day_raw" = 5, "length_raw" = 10)
}

### obs_RS_by_x(): CALCULATE AVERAGE RS BASED ON A CATEGORIZED CONTINOUS VARIABLE ###
# dat: data set containing records of reproductive output
# x: name of the continuous covariate to build levels for and summarize at
# xmin: the smallest value of the first bin
# xmax: the largest value of the last bin
# bin_width: the distance between bin breakpoints
# ...: keep_ arguments passed to my_filter(), will summarize all records satisfying returned by my_filter(dat, ...)
# only used for plotting

obs_RS_by_x = function(dat, x, xmin = NULL, xmax = NULL, bin_width, ...) {
  
  # retain only the relevant records (e.g., filter out specific years, origin, life stages)
  dat_sub = my_filter(dat, ...)

  # set low-end of smallest bin, if not provided; force a cap at zero
  if (is.null(xmin)) {
    xmin = round(min(dat[,x]), -1) - bin_width
    xmin = ifelse(xmin < 0, 0, xmin)
  }
  
  # set high-end of largest bin, if not provided
  if (is.null(xmax)) {
    xmax = round(max(dat[,x]), -1) + bin_width
  }
  
  # assign the breakpoints between bins; assign bin to each record, and calculate bin midpoint
  breaks = seq(xmin, xmax, by = bin_width)
  dat_sub$xcat = cut(dat_sub[,x], breaks = breaks)
  xcat_mids = bin_width/2 + xmin + (bin_width * 0:(length(levels(dat_sub$xcat))-1))
  
  # calculate the RS summaries (conditional, response, and prob not zero) and sample size by bin
  cond = with(subset(dat_sub, !y_is_zero), tapply(y_var, xcat, mean))
  resp = with(dat_sub, tapply(y_var, xcat, mean))
  nzprb = with(dat_sub, tapply(!y_is_zero, xcat, mean))
  n = with(dat_sub, tapply(y_var, xcat, length))
  
  # construct data frame to return
  data.frame(
    bin_name = factor(levels(dat_sub$xcat), levels = levels(dat_sub$xcat)),
    bin_mid = xcat_mids,
    cond = unname(cond),
    resp = unname(resp),
    nzprb = unname(nzprb),
    n = unname(n)
  )
}

##### OUTPUT PLOTTING FUNCTIONS #####

### RS_name(): AUTO-SELECT THE NAME OF THE RS TYPE BEING DISPLAYED ###
# for flexibly building axis/plot titles
RS_name = function(RS_type = NULL, grand = FALSE, unit = "Spawner") {
  if (is.null(RS_type)) {
    name = " "
  } else {
    name = switch(RS_type, "cond" = "Progeny/Successful UNIT", "resp" = "Progeny/UNIT", "nzprb" = "Pr(Successful)")
    name = ifelse(grand, stringr::str_replace(name, "Progeny", "Grand-Progeny"), name)
    name = stringr::str_replace(name, "UNIT", unit)
  }
  return(name)
}

### my_par(): CUSTOM DEFAULT GRAPHICS PARAMETERS FOR THIS ANALYSIS ###
# any default already set can be overwritten following standard par() usage
# any graphics parameter not given a default can be passed to ...

my_par = function(mar = c(1,1,1.5,0.5), mgp = c(1.5,0.2,0), tcl = -0.1, cex.axis = 0.8, oma = c(2,2,0,0), lend = "square", ljoin = "mitre", ...) {
  do.call(par, c(as.list(environment()), list(...)))
}

### RS_v_x_plot(): FLEXIBLY PLOT FITTED CONTINOUS RELATIONSHIP OVER SUMMARIZED DATA ###

RS_v_x_plot = function(dat, boot_preds, x, RS_type, xmin = NULL, xmax = NULL, bin_width, groups, xlim = NULL, ylim = NULL, min_nobs = 10, legend_loc = "topright", title = NULL, include_letter = TRUE, ...) {
  
  # set arguments
  dot_args = list(...)
  dat_summ_args = list(dat = dat, x = x, xmin = xmin, xmax = xmax, bin_width = bin_width)
  use_mean_args = list(use_mean_length = x != "length_raw", use_mean_day = x != "day_raw", use_mean_F1 = x != "F1")
  
  # summarize bootstrap output
  RS_summ = lapply(1:length(groups), function(g) do.call(summarize_RS, c(list(boot_preds = boot_preds), dot_args, use_mean_args, groups[g])))
  RS_summ = lapply(RS_summ, function(g) subset(g, variable == RS_type))
  
  # summarize individual spawner records, binned to provide some level of aggregation
  dat_summ = lapply(1:length(groups), function(g) do.call(obs_RS_by_x, c(dot_args, dat_summ_args, groups[g])))
  names(RS_summ) = names(dat_summ) = unname(unlist(groups))
  
  # add a date column if plotting the return day variable
  if (x == "day_raw") {
    RS_summ = lapply(RS_summ, function(x) cbind(x, date = day2date(x$day_raw, year = unique(x$year))))
    dat_summ = lapply(dat_summ, function(x) cbind(x, date = day2date(floor(x$bin_mid), year = unique(RS_summ[[1]]$year))))
  } 
  
  x_var = ifelse(x == "day_raw", "date", x)
  
  RS_summ_all = do.call(rbind, RS_summ)
  dat_summ_all = do.call(rbind, dat_summ)
  
  # set axis limits
  if (is.null(xlim)) xlim = range(RS_summ_all[,x_var], dat_summ_all[,ifelse(x == "day_raw", "date", "bin_mid")]) else if (x == "day_raw") xlim = day2date(xlim, unique(RS_summ[[1]]$year))
  if (is.null(ylim)) ylim = range(RS_summ_all[,c("lwr","upr")], dat_summ_all[,RS_type], na.rm = TRUE)
  ylim
  if (RS_type != "nzprb" & ylim[2] < 2) ylim[2] = 2
  if (RS_type != "nzprb" & ylim[1] > 0) ylim[1] = 0
  
  cols = c("tomato", "royalblue", "orange", "forestgreen")
  
  # empty plot
  plot(RS_summ[[1]][,"mean"] ~ RS_summ[[1]][,x_var], type = "n", xlim = xlim, ylim = ylim, xlab = "", ylab = "")
  
  lapply(1:length(groups), function(g) {
    
    polygon(
      x = c(RS_summ[[g]][,x_var], rev(RS_summ[[g]][,x_var])),
      y = c(RS_summ[[g]]$lwr, rev(RS_summ[[g]]$upr)), 
      col = scales::alpha(cols[g], 0.35), border = NA
    )
    
    lines(RS_summ[[g]][,"mean"] ~ RS_summ[[g]][,x_var], col = cols[g])
    points(dat_summ[[g]][,RS_type] ~ dat_summ[[g]][,ifelse(x == "day_raw", "date", "bin_mid")], col = cols[g],
           pch = 21, bg = ifelse(dat_summ[[g]]$n >= min_nobs, scales::alpha(cols[g], 0.35), "white"))
  })
  
  if (!is.null(legend_loc)) {
    legend(legend_loc, legend = unlist(groups),
           col = cols[1:length(groups)],
           pt.bg = scales::alpha(cols[1:length(groups)], 0.35),
           pch = 22, pt.cex = 2, cex = 0.8, bty = "n"
    )
  }
  
  # title
  if (include_letter) {
    counter <<- counter + 1
    title = paste0("(", letters[counter], ") ", ifelse(is.null(title), "", title))
  } 
  mtext(side = 3, line = 0.125, text = title, font = 1, adj = 0, cex = 0.8)
  
}

### compare_RS_plot(): FLEXIBLY PLOT RS ESTIMATES ###

compare_RS_plot = function(boot_preds, RS_type, ylim = NULL, legend = FALSE, dcast_formula, cex.legend = 1, ...) {
  
  # summarize the bootstrap output
  RS_summ = summarize_RS(boot_preds, ...) |> 
    dplyr::filter(variable == RS_type)
  
  # format means
  mn = RS_summ |> 
    reshape2::dcast(dcast_formula, value.var = "mean")
  
  # get group names
  grps = mn[,1]
  
  # finish formatting means
  mn = mn |> 
    dplyr::select(-1) |> 
    as.matrix()
  
  # format lwrs
  lwr = RS_summ |> 
    reshape2::dcast(dcast_formula, value.var = "lwr") |> 
    dplyr::select(-1) |> 
    as.matrix()
  
  # format uprs
  upr = RS_summ |> 
    reshape2::dcast(dcast_formula, value.var = "upr") |> 
    dplyr::select(-1) |> 
    as.matrix()
  
  # set the y-axis limit
  if (is.null(ylim)) ylim = c(0, max(upr))
  
  # set the colors
  col_bank = c("grey30", "grey50", "grey60", "grey70", "grey90")
  if (length(grps) == 2) keep_cols = c(2,5)
  if (length(grps) == 3) keep_cols = c(1,3,5)
  if (length(grps) == 4) keep_cols = c(1,2,4,5)
  bar_cols = col_bank[keep_cols]
  
  # create the barplot
  mp = barplot(mn, beside = TRUE, ylim = ylim, names.arg = rep("", ncol(mn)), col = bar_cols)  
  
  # draw uncertainty estimates
  segments(mp, lwr, mp, upr)
  
  # draw axes
  usr = par("usr")
  segments(usr[1], usr[3], usr[2], usr[3], xpd = TRUE)
  axis(side = 1, at = colMeans(mp), labels = substr(colnames(mn), 3, 4))
  
  # draw the legend if requested
  if (legend) {
    legend("topright", legend = grps, pch = 22, col = "black", pt.bg = bar_cols, bty = "n", pt.cex = 2, cex = cex.legend)
  }
  
  # draw the panel label
  counter <<- counter + 1
  mtext(side = 3, adj = 0, line = 0, text = paste0("(", letters[counter], ")"), cex = 0.8)
}

### compare_RRS_plot(): FLEXIBLY PLOT RRS ESTIMATES ###

compare_RRS_plot = function(RRS_summ, RS_type = NULL, ylim = NULL, legend = FALSE, yaxis_side = 2, label_panel = TRUE, cex.legend = 1) {
  
  # set levels based on the order they are found in the dataset
  RRS_summ$group = factor(RRS_summ$group, levels = unique(RRS_summ$group))
  
  # extract RRS estimates for this RS_type
  if (!is.null(RS_type)) {
    x = RRS_summ |> 
      dplyr::filter(variable == RS_type)
  } else {
    x = RRS_summ
  }
  
  # format means
  mn = x |> 
    reshape2::dcast(group ~ year, value.var = "mean") 
  
  # get group names
  grps = as.character(mn$group)
  
  # finish formatting means
  mn = mn |> 
    dplyr::select(-group) |> 
    as.matrix()
  
  # format lwrs
  lwr = x |> 
    reshape2::dcast(group ~ year, value.var = "lwr") |> 
    dplyr::select(-group) |> 
    as.matrix()
  
  # format uprs
  upr = x |> 
    reshape2::dcast(group ~ year, value.var = "upr") |> 
    dplyr::select(-group) |> 
    as.matrix()
  
  # set the yaxis limits such that 1 is in the center if not provided
  if (is.null(ylim)) {
    lwr_dist = 1 - min(lwr)
    upr_dist = 1 - max(upr)
    max_dist = max(abs(lwr_dist), abs(lwr_dist))
    ylim = 1 + (max_dist * c(-1,1))
  } 
  
  # set colors based on number of groups
  symbols = c(21, 24, 22, 25, 23)
  
  n_grps = length(grps)
  if (n_grps > 5) stop ("Do not try to display more than 5 RRS comparisons per year")
  
  # create the empty plot
  mp = barplot(mn, beside = TRUE, ylim = ylim, names.arg = rep("", ncol(mn)),
               col = NA, border = NA, xlim = c(0.5, ncol(mn) * nrow(mn) + ncol(mn) + 0.5), yaxt = "n")  
  
  # add year separators
  abline(v = (mp[n_grps,1:(ncol(mp)-1)] +  mp[1,2:(ncol(mp))])/2, col = "grey", lty = 3)
  
  # draw error bars
  segments(mp, lwr, mp, upr, col = "black")
  
  # draw point estimates
  points(mp, mn, pch = symbols[1:n_grps], col = "white", bg = "black", cex = 1.5)
  
  # draw axes
  usr = par("usr")
  segments(usr[1], usr[3], usr[2], usr[3], xpd = TRUE)
  if (yaxis_side == 2) {
    segments(usr[1], usr[3], usr[1], usr[4], xpd = TRUE)
  } else {
    segments(usr[2], usr[3], usr[2], usr[4], xpd = TRUE)
  }
  axis(side = 1, at = colMeans(mp), labels = substr(as.character(unique(RRS_summ$year)), 3, 4))
  axis(side = yaxis_side)
  
  # draw reference line
  abline(h = 1, lty = 2)
  
  # add legend if requested
  if (legend) {
    legend("topright", ncol = 1, legend = grps, pch = symbols[1:n_grps], col = "black", pt.bg = "black",
           box.col = "white", x.intersp = 0.75,
           pt.cex = 1.2, cex = cex.legend)
  }
  
  # draw the panel label
  if (label_panel) {
    counter <<- counter + 1
    mtext(side = 3, adj = 0, line = 0, text = paste0("(", letters[counter], ")"), cex = 0.8)
  }
}

##### RMARKDOWN OUTPUT FUNCTIONS #####

### percentize(): CONVERT A PROPORTION TO A PERCENTAGE FOR NICE PRINTING
# copied verbatim from bstaton1/msdown

percentize = function(x, digits = 0, escape = FALSE) {
  
  # create a formatted percent, assuming x is a proportion (i.e., x = 1 = 100%)
  out = paste0(round(x * 100, digits = digits), ifelse(escape, "\\%", "%"))
  
  # test if a value rounded to 0 needs to actually be reported as <1
  zero_test = ifelse(escape, "0\\%", "0%")
  lt1_replace = ifelse(escape, "<1\\%", "<1%")
  
  # if x > 0 but was rounded to 0, replace with <1%
  ifelse(out == zero_test & x > 0, lt1_replace, out)
  
}

### desc_table(): CREATE A NICE TABLE TO SUMMARIZE KEY ASPECTS OF AN ANALYSIS ###

desc_table = function(y_var, main_x_var, other_x_vars, RRS, years, data_rules = "None", footnote_text = NULL) {
  
  out = data.frame(
    x1 = c(
      "Response Variable",
      "Primary Predictor",
      "Other Variables Assessed",
      "Ratio Expressed As",
      "Spawn Years Included",
      "Notable Data Exclusions"
    ),
    x2 = c(y_var, main_x_var, other_x_vars, RRS, paste(range(years), collapse = "--"), data_rules)
  ) |> 
    kableExtra::kbl(caption = "Summary of the critical aspects of this analysis.", col.names = c("Aspect", "Description"), escape = FALSE) |> 
    kableExtra::kable_styling(full_width = FALSE, bootstrap_options = c("condensed", "striped")) |> 
    kableExtra::column_spec(1, bold = TRUE, width = "200px") |> 
    kableExtra::column_spec(2, width = "450px")
  
  if (!is.null(footnote_text)) {
    out = out |> 
      kableExtra::footnote(
        alphabet = footnote_text, alphabet_title = "Data Exclusion Notes", title_format = c("italic", "underline"), escape = FALSE
      )
  }
  
  return(out)
}

### model_RS_RRS_kable(): CREATE A NICE TABLE REPORTING MODEL-BASED RS AND RRS ESTIMATES

model_RS_RRS_kable = function(boot_preds, digits = 2, denominator = c("keep_origin" = "NOR"), numerator = c("keep_origin" = "HOR"), dcast_formula = year ~ variable + origin, is_grand, RS_types = c("nzprb", "cond", "resp"), unit = "Spawner", ...) {
  
  dot_args = list(...)
  
  ### MODEL OUTPUT FILTERING/PROCESSING ###
  
  # summarize RS
  RS_summs = do.call(summarize_RS, c(list(boot_preds = boot_preds), dot_args))
  
  # summarize RRS
  RRS_summs = lapply(1:length(numerator), function(i) {
    do.call(summarize_RRS, c(list(boot_preds = boot_preds, numerator = numerator[i], denominator = denominator[i]), dot_args))
  }); RRS_summs = do.call(rbind, RRS_summs)
  
  # RRS_summs = summarize_RRS(boot_preds, numerator = numerator, denominator = denominator, ...)
  
  # function to combine mean, lwr, upr columns into one cell
  format_cells = function(df, digits) {
    empty = 'MEAN<br><small>(LWR-UPR)</small>'
    stringr::str_replace(empty, "MEAN", as.character(round(df$mean, digits = digits))) |>
      stringr::str_replace("LWR", as.character(round(df$lwr, digits = digits))) |>
      stringr::str_replace("UPR", as.character(round(df$upr, digits = digits)))
  }
  
  # apply it
  RS_summs$cell = format_cells(RS_summs, digits = digits)
  RRS_summs$cell = format_cells(RRS_summs, digits = digits)
  RRS_summs$sig = ifelse(RRS_summs$lwr < 1 & RRS_summs$upr > 1, FALSE, TRUE)
  
  # what is the name of the variable being compared?
  var = unique(stringr::str_remove(names(numerator), "^keep_"))
  
  # extract needed info for RS and RRS
  RS_df = RS_summs[,c("year", var, "cell", "variable")]
  RRS_df = RRS_summs
  RRS_df$empty = paste0("RRS-", RRS_df$numerator)
  names(RRS_df)[ncol(RRS_df)] = var
  RRS_df = RRS_df[,c("year", var, "cell", "variable", "sig")]
  
  # combine and reshape
  df = rbind(cbind(RS_df, sig = TRUE), RRS_df)
  df$variable = factor(df$variable, levels = c("nzprb", "cond", "resp"))
  df$cell = ifelse(!df$sig, kableExtra::cell_spec(df$cell, color = "lightgray", escape = FALSE), df$cell)
  tab = reshape2::dcast(df, formula = dcast_formula, value.var = "cell")
  
  # keep only RS types of interest
  tab = tab[,stringr::str_detect(colnames(tab), paste0(c("year", RS_types), collapse = "|"))]
  
  ### KABLE MAKING ###
  
  colnames(tab) = stringr::str_remove(colnames(tab), "^[:alpha:]+_")
  colnames(tab)[1] = " "
  
  n_rs_per_type = length(unique(RS_df[,var]))
  n_rrs_per_type = n_rs_per_type - 1
  n_per_type = n_rs_per_type + n_rrs_per_type

  RS_type_headers = c(" " = 1, "nzprb" = n_per_type, "cond" = n_per_type, "resp" = n_per_type)
  RS_type_headers = RS_type_headers[c(1, which(names(RS_type_headers) %in% RS_types))]
  
  RRS_type_headers = c(" " = 1, rep(c("RS" = n_rs_per_type, "RRS" = n_rrs_per_type), length(RS_types)))
  
  names(RS_type_headers)[2:length(RS_type_headers)] = sapply(names(RS_type_headers)[2:length(RS_type_headers)], RS_name, grand = is_grand, unit = unit)
  
  is_RRS_column = stringr::str_detect(colnames(tab), "^RRS-")
  colnames(tab) = stringr::str_remove(colnames(tab), "^RRS-")
  
  has_border = cumsum(RS_type_headers)[-1]
  
  out = tab |>
    knitr::kable(format = "html", escape = FALSE, align = "c") |>
    kableExtra::kable_styling(full_width = FALSE, bootstrap_options = c("condensed", "striped")) |> 
    kableExtra::column_spec(1, bold = TRUE) |> 
    kableExtra::add_header_above(RRS_type_headers, bold = TRUE) |>
    kableExtra::add_header_above(RS_type_headers, bold = TRUE) |> 
    kableExtra::column_spec(which(is_RRS_column), bold = TRUE)
    
  
  if (length(has_border) > 1) {
    out = kableExtra::column_spec(out, has_border, border_right = TRUE)
  } 
  
  return(out)
}

### AIC_kable(): PRINTS A NICE AIC TABLE FOR RMD REPORTS ###

AIC_kable = function(AIC_tab, best_model, has_zi = TRUE, caption = NULL, markdown = FALSE) {
  
  # assign which model is the best model
  AIC_tab$is_best = AIC_tab$cond_form == format_formula(best_model)$cond & AIC_tab$zi_form == format_formula(best_model)$zi 
  
  # calculate the total number of parameters
  # +1 includes the dispersion parameter
  AIC_tab$n_params = AIC_tab$n_cond_coefs + AIC_tab$n_zi_coefs + 1
  
  # sort models by increasing delta scores
  AIC_tab = AIC_tab[order(AIC_tab$delta),]
  
  first_col_header = ifelse(has_zi, " <small>(Identical for Conditional and Zero Submodels)</small>", " <small>(No Zero Submodel)</small>")
  
  if (markdown) {
    first_col_header = stringr::str_remove(first_col_header, stringr::fixed("<small>")) |> 
      stringr::str_remove(stringr::fixed("</small>"))
  }
  
  if (!is.null(caption)) {
    caption = ifelse(any(is.na(AIC_tab$delta)), paste0(caption, " Models with no AIC values failed to converge."), caption)
  }
  
  # produce the kable
  out = AIC_tab[,c("cond_form", "n_params", "delta")] |> 
    kableExtra::kbl(format = ifelse(markdown, "markdown", "html"), format.args = list(big.mark = ","), digits = 0,
                    col.names = c(paste0("Covariates in Model", first_col_header), "Parameters", ifelse(markdown, "deltaAIC", "$\\Delta$AIC")),
                    caption = caption,
                    row.names = FALSE, escape = FALSE)
  
  if (!markdown) {
    out |> 
      kableExtra::kable_styling(full_width = FALSE, bootstrap_options = c("condensed", "striped")) |> 
      kableExtra::column_spec(1, monospace = TRUE, width = "500px") |> 
      kableExtra::row_spec(which(AIC_tab$is_best), bold = TRUE)
  } else {
    c("    ---------", "    AIC TABLE", "    ---------", sapply(as.character(out), function(y) paste(c("    ", y), collapse = ""))) |> 
      cat(sep = "\n")  
  }
}

### coef_kable(): PRINTS A NICE COEFFICIENT TABLE FOR RMD REPORTS ###

coef_kable = function(model, type) {
  
  tab = as.data.frame(summary(model)$coef[[type]])
  tab[,"Pr(>|z|)"] = ifelse(tab[,"Pr(>|z|)"] < 0.001, "<0.001", as.character(round(tab[,"Pr(>|z|)"], 3)))
  tab[,c("Estimate", "Std. Error", "z value")] = round(tab[,c("Estimate", "Std. Error", "z value")], 2)
  
  caption = "Coefficient estimates from the SUBMODEL submodel." |> 
    stringr::str_replace("SUBMODEL", ifelse(type == "cond", "conditional", "zero"))
  
  kableExtra::kbl(tab, caption = caption) |> 
    kableExtra::kable_styling(full_width = FALSE, bootstrap_options = c("condensed", "striped")) |> 
    kableExtra::column_spec(1, monospace = TRUE, bold = TRUE)
}

##### RESIDUAL DIAGNOSTICS FUNCTIONS #####

### prep_resid_df(): PREPARE A DATA SET FOR DISPLAYING RESIDUALS FROM A FITTED GLM ###

prep_resid_df = function(dat, best_model, cat_vars = NULL, cont_vars = NULL) {
  
  # select only needed variables
  keep_cols = c("y_var")
  if (!is.null(cat_vars)) keep_cols = c(keep_cols, cat_vars)
  if (!is.null(cont_vars)) keep_cols = c(keep_cols, cont_vars)
  resid_dat = dat[,keep_cols]
  
  # add DHARMa residuals
  resids = DHARMa::simulateResiduals(best_model)
  resid_dat$QSR = resids$scaledResiduals

  # get model predicted value for each fish and their rank
  resid_dat$y_hat = predict(best_model, type = "response")
  resid_dat$y_hat_rank = rank(resid_dat$y_hat)
  
  # add y_hat bins
  breaks = seq(0, max(resid_dat$y_hat), length = 11)
  resid_dat$y_hat_bin = cut(resid_dat$y_hat, breaks = breaks, labels = FALSE)
  
  # add y_hat rank bins
  breaks = seq(1, max(resid_dat$y_hat_rank), length = 11)
  resid_dat$y_hat_rank_bin = cut(resid_dat$y_hat_rank, breaks = breaks, labels = FALSE)
  
  # add binned versions  continuous variables
  if (!is.null(cont_vars)) {
    # add length bins if requested
    if ("length_raw" %in% cont_vars) {
      breaks = seq(min(resid_dat$length_raw), max(resid_dat$length_raw), length = 11)
      resid_dat$length_bin = cut(resid_dat$length_raw, breaks = breaks, dig.lab = 4)
    }
    
    # add day bins if requested
    if ("day_raw" %in% cont_vars) {
      breaks = seq(min(resid_dat$day_raw), max(resid_dat$day_raw), length = 11)
      resid_dat$day_bin = cut(resid_dat$day_raw, breaks = breaks) |> 
        format_day_bins()
      # need to use the same year for all data points so they are plotted together
      resid_dat$day = day2date(resid_dat$day_raw, year = 2007)
    }
    
    # add length bins if requested
    if ("F1" %in% cont_vars) {
      breaks = seq(min(resid_dat$F1), max(resid_dat$F1), length = 11)
      resid_dat$F1_bin = cut(resid_dat$F1, breaks = breaks, labels = FALSE, include.lowest = TRUE)
    }
  }
  
  # handle categorical variables
  if (!is.null(cat_vars)) {
    # clean categorical variable names
    # years
    if ("year" %in% cat_vars) resid_dat$year = substr(as.character(resid_dat$year), 3, 4)
    
    # life stages/sexes
    if (all(c("sex", "life_stage") %in% cat_vars)) {
      resid_dat$sex_life_stage = ifelse(resid_dat$sex == "M", ifelse(resid_dat$life_stage == "Adult", "Male", "Jack"), "Female")
      resid_dat = resid_dat[,-which(colnames(resid_dat) %in% c("sex", "life_stage"))]
      cat_vars = c(cat_vars[-which(cat_vars %in% c("sex", "life_stage"))], "sex_life_stage")
    } else {
      if ("life_stage" %in% cat_vars) resid_dat$life_stage = ifelse(resid_dat$life_stage == "Jack", "Jk", "Ad")
      if ("sex" %in% cat_vars) resid_dat$sex = ifelse(resid_dat$sex == "M", "Male", "Female")
    }
    
    # cross type
    if ("cross_type" %in% cat_vars) {
      resid_dat$cross_type = stringr::str_remove_all(resid_dat$cross_type, "OR")
    }
    
    # ancestry type
    if ("ancestry" %in% cat_vars) {
      resid_dat$ancestry = stringr::str_remove_all(resid_dat$ancestry, "OR")
    }
    
    # disposition type
    if ("disposition" %in% cat_vars) {
      resid_dat$disposition = ifelse(resid_dat$disposition == "Broodstock", "Brood", "Natrl")
    }
    
    # function to make n-way combos of categorical covariates
    make_nway_vars = function(dat, cat_vars, n) {
      
      # create all n-way interactions among categorical variables
      vars = do.call(expand.grid, lapply(1:n, function(x) cat_vars))
      
      # exclude combos with the same variable in all positions
      vars = vars[apply(vars, 1, function(x) length(unique(x)) == n),]
      
      # combine variable names using ":" and remove any duplicated ones
      vars = unlist(lapply(1:nrow(vars), function(i) paste(sort(as.character(unlist(vars[i,]))), collapse = ":")))
      vars = vars[!duplicated(vars)]
      
      # combine the variable values
      vals = lapply(vars, function(v) {
        vals = dat[,stringr::str_split(v, ":")[[1]]]
        vals = apply(vals, 1, paste, collapse = "-")
        out = data.frame(vals); colnames(out) = v
        out
      })
      
      # combine and return
      do.call(cbind, vals)
    }
    
    # add the n-way combo variables
    if (length(cat_vars) > 1) resid_dat = cbind(resid_dat, make_nway_vars(resid_dat, cat_vars, 2))
    if (length(cat_vars) > 2) resid_dat = cbind(resid_dat, make_nway_vars(resid_dat, cat_vars, 3))
    if (length(cat_vars) > 3) resid_dat = cbind(resid_dat, make_nway_vars(resid_dat, cat_vars, 4))
  }
  
  # combine variables to return
  ordered_vars = c("y_var", "QSR", "y_hat", "y_hat_bin", "y_hat_rank", "y_hat_rank_bin")
  remaining_vars = colnames(resid_dat)[!colnames(resid_dat) %in% ordered_vars]
  
  # return the output
  return(resid_dat[,c(ordered_vars, remaining_vars)])
}

### resid_par(): SET GRAPHICS PARAMETERS FOR RESIDUAL DIAGNOSTIC FIGURES ###

resid_par = function() {
  par(mar = c(5,2,1.5,1.5), xaxs = "i", mgp = c(1,0.15,0), tcl = -0.1, cex.axis = 0.65, lend = "square", ljoin = "mitre")
}

### resid_scatterplot(): CREATE A SCATTERPLOT OF RESIDUALS VS. A CONTINUOUS VARIABLE ###

resid_scatterplot = function(var) {
  # build the formula for the plot call
  text_formula = "QSR ~ VAR" |> 
    stringr::str_replace("VAR", var)
  formula = as.formula(text_formula)
  
  # create the plot
  plot(formula, data = resid_dat, pch = 16, cex = 0.5, col = scales::alpha("grey", 0.2),
       xlab = "", ylab = "Quantile Standardized Residual", las = 2)
  
  # draw reference lines
  abline(h = c(0.25, 0.5, 0.75), col = "royalblue", lty = 2, lwd = c(1,2,1))
  
  # add a plot title
  mtext(side = 3, line = 0.2, adj = 0, cex = 1.1, font = 2, text = stringr::str_remove(text_formula, "_raw"))
}

### resid_boxplot(): CREATE A BOX-AND-WISKER PLOT OF RESIDUALS VS. A CATEGORICAL VARIABLE ###

resid_boxplot = function(var) {
  
  # build the formula for the plot call
  text_formula = "QSR ~ VAR" |> 
    stringr::str_replace("VAR", var)
  formula = as.formula(text_formula)
  
  # create the plot
  boxplot(formula, data = resid_dat, las = 2,
          xlab = "", ylab = "Quantile Standardized Residual",
          fillcol = "grey80", boxcol = "grey30", medcol = "grey30", whiskcol = "grey30", whisklty = 1, staplewex = 0,
          yaxt = "n", ylim = c(0,1.15), outpch = 16, outcex = 0.5, outcol = scales::alpha("grey30", 0.5))
  
  # draw y-axis
  usr = par("usr"); xdiff = diff(usr[1:2])
  axis(side = 2, at = seq(0, 1, 0.2), labels = seq(0, 1, 0.2), las = 2)
  
  # draw sample sizes
  n = aggregate(formula, data = resid_dat, FUN = length)
  text(x = (1:nrow(n)) - xdiff * 0.0125, y = 1.025, labels = prettyNum(n$QSR, big.mark = ","), pos = 4, srt = 90, cex = 0.6, col = "grey30")
  
  # draw reference lines
  abline(h = c(0.25, 0.5, 0.75), col = "royalblue", lty = 2, lwd = c(1,2,1))
  
  # add the plot title
  mtext(side = 3, line = 0.2, adj = 0, cex = 1.1, font = 2, text = stringr::str_remove(text_formula, "_raw"))
}

### resid_plot(): AUTOMATICALLY SELECT AND CREATE THE CORRECT RESIDUAL PLOT TYPE DEPENDING ON THE VARIABLE ###

resid_plot = function(var) {
  # banks of all continuous and categorical variables
  all_cont_vars = c("length_raw", "day_raw", "F1",
                    "y_hat", "y_hat_rank")
  all_cat_vars = c("year", "origin", "sex_life_stage", "acc_site", "cross_type", "jacks_in_cross", "disposition", "ancestry",
                   "y_hat_bin", "y_hat_rank_bin", "length_bin", "day_bin", "F1_bin")
  
  # use the appropriate plot type
  resid_par()
  if (var %in% all_cont_vars) resid_scatterplot(var)
  if (var %in% all_cat_vars | stringr::str_detect(var, "\\:")) resid_boxplot(var)
}
