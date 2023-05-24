

#' O2 machine factors
#'
#' @return tibble of factors
#' @export
o2_corr_factors <- function() {
  
  tibble::tribble(
    ~device, ~meas_type, ~from_seq, ~to_seq,     ~factor, ~file_ext,
    "EMIL",      "bas",         1,    8227,           1,    ".log",
    "EMIL",      "bas",      8228,   1e+07,           1,    ".csv",
    "EMIL",      "mic",         1,    8227,           1,    ".log",
    "EMIL",      "mic",      8228,   1e+07,           1,    ".csv",
    "KLAUS",      "bas",         1,    6629, 0.976062491,    ".log",
    "KLAUS",      "bas",      6630,   1e+07,           1,    ".csv",
    "KLAUS",      "mic",         1,    6629, 0.987520843,    ".log",
    "KLAUS",      "mic",      6630,   1e+07,           1,    ".csv",
    "KARL",      "bas",         1,    4260, 1.454178379,    ".txt",
    "KARL",      "bas",      4261,   1e+07,           1,    ".csv",
    "KARL",      "mic",         1,    4260, 1.564204231,    ".txt",
    "KARL",      "mic",      4261,   1e+07,           1,    ".csv",
    "DIETER",      "bas",         1,    5190, 1.490347018,    ".txt",
    "DIETER",      "bas",      5191,   1e+07,           1,    ".csv",
    "DIETER",      "mic",         1,    5190, 1.545151676,    ".txt",
    "DIETER",      "mic",      5191,   1e+07,           1,    ".csv"
  )
  
  
}


#' Correction factors
#' 
#' Returns the  correction factor from list
#' 
#' @param dev Device
#' @param seq Sequence
#' @param meas Measurements type. "bas" or "mic"
#'
#' @return a number
#' @export
o2_corr_fct <- function(dev, seq, meas) {
  
  df <- o2_corr_factors() %>% dplyr::filter(device == dev,
                                            meas_type == meas)
  
  right_row <- which(slider::slide_lgl(df, ~ dplyr::between(as.numeric(seq), .x$from_seq, .x$to_seq)))
  
  stopifnot(length(right_row) == 1)
  
  df %>% dplyr::slice(right_row) %>% dplyr::pull(factor)
  
}


#' Process O2 table
#'
#' The function looks for the raw data files in the same directory
#'
#' @param w_file 
#' @param plot 
#'
#' @return tibble
#' @export
o2_process_from_table1 <- function(w_file, raw_files = "from_wei", combine_names = "_", plot = FALSE) {
  #w_file <- weighing_files[3]
  # w_file <- path
  # raw_files = "from_wei"; combine_names = "_"; plot = FALSE
  # w_file <- files[1]
  
  message("Processing ", basename(w_file))
  
  w_file0 <- w_file
  w_dir <- dirname(w_file)
  
  #how many worksheets
  # TODO add reading method for different templates
  # 
  if (length(readxl::excel_sheets(w_file)) > 1){
    
    message(basename(w_file), " had too many worksheets. It was skipped.")
    
    return(NULL)
  } 
  
  #read weighing sheet
  suppressMessages(
    weights_check <- readxl::read_xlsx(w_file, sheet = 1, col_names = FALSE, na = c("NA", ""), col_types = "text")
  )
  
  # starting row
  st_row <- which(weights_check[,1] == "idSequence")
  
  # fix n/a
  as.num_na <- function(x) {
    
    x %>% stringr::str_replace("n/a", NA_character_) %>% as.numeric
    
  }
  
  weights0 <- readxl::read_xlsx(w_file, col_types = "text", skip = st_row - 1) %>% 
    dplyr::slice(-1) %>% 
    dplyr::mutate(across(c(`Container empty [g]`:`Container + Sample dry weight [g]`,
                           `Glucose / sample [mg]`, `Water added / sample [ml]`, starts_with("BAS_st")), as.num_na))
  
  # purrr::map_dfc(~parse_guess(.x)) #better to specify, but could guess using this
  
  # fiddle selection + column names
  weights <- weights0 %>% 
    dplyr::mutate(weight_file = w_file0,
                  bas_file = weights_check[3,3],
                  cmic_file = weights_check[4,3]) %>% 
    dplyr::select(idSequence,
                  channel = Channel,
                  device = Device,
                  name1 = `Sample name 1`, 
                  name2 = `Sample name 2`,
                  name3 = `Sample name 3`,
                  weight_file,
                  bas_file,
                  cmic_file,
                  wei_cont_empty = `Container empty [g]`,
                  wei_sample_fresh = `Sample fresh weight [g]`,
                  wei_cont_plus_samp_dry = `Container + Sample dry weight [g]`,
                  soil_type = `Soil type`,
                  glucose = `Glucose / sample [mg]`,
                  water_added = `Water added / sample [ml]`,
                  date_sampling = `Date sampling`,
                  comment = Comments,
                  bas_start = BAS_start,
                  bas_stop = BAS_stop) %>% 
    dplyr::mutate(name_c = combine_name(name1, name2, name3, combine_names),
                  .after = name3)
  
  
  # Dry weight calculation, equivalent to first tab of Excel file
  weights <- weights %>%
    dplyr::mutate(
      # Container + sample fresh weight [g]
      wei_cont_plus_samp_fresh = wei_cont_empty + wei_sample_fresh,
      # Weighted sample dry weight [g]
      wei_samp_dry = wei_cont_plus_samp_dry - wei_cont_empty,
      # H2O in sample [g]
      h2o_samp = wei_sample_fresh - wei_samp_dry,
      # H2O of fresh weight [%]
      h2o_perc = h2o_samp*100 / wei_sample_fresh
    )
  
  
  # Machine measurement files -----------------------------------------------
  
  # raw_files extracted from weighing sheet, in same dir or provided as list
  
  # find raw file paths
  if (raw_files == "from_wei") {
    
    raw_paths <- extract_raw_path1(file = w_file)
    
    bas_path <- file.path(w_dir, raw_paths[1])
    
    cmic_path <- file.path(w_dir, raw_paths[2])
    
    if (is.na(raw_paths[1]) || !file.exists(bas_path)) {
      warning(basename(bas_path), " was not found. BAS cannot be calculated.")
      bas_path <- NA
    }
    
    if (is.na(raw_paths[2]) || !file.exists(cmic_path)) {
      warning(basename(cmic_path), " was not found. CMIC cannot be calculated.")
      cmic_path <- NA
    }
    
    
  } else if (raw_files == "matching_names") {
    
    bas_path <- list.files(w_dir, stringr::str_replace(basename(w_file), "w_", "bas_") %>% stringr::str_remove(".xlsx"))
    if (!assertthat::is.string(bas_path)) {
      warning(basename(bas_path), " was not found. BAS cannot be calculated.")
      bas_path <- NA
      
    }
    
    cmic_path <- list.files(w_dir, stringr::str_replace(basename(w_file), "w_", "mic_") %>% stringr::str_remove(".xlsx"))
    if (!assertthat::is.string(cmic_path)) {
      warning(basename(cmic_path), " was not found. CMIC cannot be calculated.")
      cmic_path <- NA
    }
    
  } else {
    
    stop("Method for 'raw_files' not yet implemented.")
    
  }
  
  # read raw files  
  
  bas_raw0 <- o2_read_machine_file(bas_path)
  cmic_raw0 <- o2_read_machine_file(cmic_path)
  
  # dates
  date_bas <- as.Date(bas_raw0[1, "Date", drop=T], format = "%d.%m.%y")
  date_cmic <- as.Date(cmic_raw0[1, "Date", drop=T], format = "%d.%m.%y")
  
  
  # Process measurements as in Excel sheet ----------------------------------
  
  weights2 <- weights %>% dplyr::mutate(bas_raw = bas_raw0 %>% dplyr::select(-c(1,2)) %>% purrr::map(~.x),
                                        cmic_raw = cmic_raw0 %>% dplyr::select(-c(1,2)) %>% purrr::map(~.x),
                                        date_bas_meas = date_bas,
                                        date_cmic_meas = date_cmic)
  
  
  # add all colums from O2 measurements
  weights2 <- weights2 %>% o2_calc_all(plot = plot)
  
  
  # Remove empty rows
  to_rm <- weights2 %>% dplyr::select(name1, name2, name3, wei_cont_empty, wei_sample_fresh, wei_cont_plus_samp_dry) %>%
    slider::slide_lgl(~all(is.na(.x)))
  
  weights2 %>% dplyr::filter(!to_rm)
  
  
}


#' Process all
#' 
#' Wrapper function to run all analyses with defaults
#'
#' @param files 
#' @param raw_files 
#' @param combine_names 
#' @param plot 
#'
#' @return
#' @export
o2_process_all <- function(files, raw_files = "from_wei", combine_names = "_", plot = FALSE) {
  
  purrr::map_dfr(files, o2_process_from_table1, raw_files = raw_files, combine_names = combine_names, plot = plot)
  
}


#' Combine names
#'
#' @param n1 First name
#' @param n2 Second name
#' @param n3 Third name
#' @param symbol Used to paste names together
#'
#' @return character
#' @export
combine_name <- function(n1, n2, n3, symbol = "_") {
  
  make_name <- function(names) {
    
    if (all(is.na(names))) {
      return(NA_character_)
    }
    
    cnam <- NULL
    
    if (!is.na(names[1])) {
      cnam <- names[1]
    }
    
    if (!all(is.na(names[2:3]))) {
      
      cnam <- paste0(cnam, symbol)
      
      if (!is.na(names[2])) {
        cnam <- paste0(cnam, names[2])
      }
      
      if (!is.na(names[3])) {
        
        cnam <- paste0(cnam, symbol, names[3])
        
      }
      
    }
    
    cnam
  }
  
  list(n1, n2, n3) %>% 
    purrr::transpose() %>%
    purrr::simplify_all() %>%
    purrr::map_chr(make_name)
  
}




#' Calculate all
#'
#' @param data dataset
#' @param plot logical
#'
#' @return tibble
#' @export
o2_calc_all <- function(data, plot = FALSE) {
  data %>% o2_bas(plot = plot) %>% o2_cmic(plot = plot) %>%
    dplyr::mutate(qo2 = basal / cmic) %>% 
    o2_mgrowth(plot = plot)
}



# assumes columns wei_samp_dry, bas_raw(list), bas_start, bas_stop
#' Calculate Basal
#'
#' @param data dataset
#' @param plot logical
#' @param only_sets logical if TRUE will only recalculate basal measurements for entries that have bas_set value
#'
#' @return tibble
#' @export
o2_bas <- function(data, plot = FALSE, only_sets = FALSE) {
  # data <- weights2
  
  # subset: weird implementation but works
  # only recompute for non-na bas_set
  
  only_sets <- only_sets && "bas_set" %in% names(data)
  
  if (only_sets) {
    
    # FIX name_c might not be unique, machine x seq has to be unique, should use that instead!
    
    nam_aft <- which(names(data) == "name_c")
    
    name_cs <- data %>% dplyr::select(name_c)
    others <- data %>% dplyr::filter(is.na(data$bas_set))
    
    data <- data %>% dplyr::filter(!is.na(data$bas_set))
    
  }
  
  
  # add machine correction factor
  data <- data %>% dplyr::mutate(bas_corfct = purrr::map2_dbl(device, idSequence, ~ o2_corr_fct(.x, .y, meas = "bas")),
                                 bas_raw_st = purrr::map2(bas_raw, bas_corfct,  ~ .x * .y))
  
  
  # multiply by ratio already
  data2 <- data %>% dplyr::mutate(bas_diff = purrr::map2(bas_raw_st, wei_samp_dry, ~ (calc_hour_diffs(.x) * .83) / .y * 0.7))
  
  
  # take mean for chosen period
  # or specified values if given in bas_set
  calc_bas_mean <- function(data) {
    
    sel_diffs <- get_diffs(data)
    
    mean(sel_diffs, na.rm = TRUE)
    
  }
  
  # coefficient of variation
  calc_bas_cova <- function(data) {
    
    sel_diffs <- get_diffs(data)
    
    bas_mean <- mean(sel_diffs, na.rm = TRUE)
    
    bas_sd <- sd(sel_diffs, na.rm = TRUE)
    
    bas_sd/bas_mean
    
  }
  
  get_diffs <- function(data) {
    
    #select
    sel_times <- if ("bas_set" %in% names(data) && !is.na(data$bas_set)) {
      data$bas_set[[1]]
      
    } else {
      
      if (is.na(data$bas_start)) {
        
        # TODO add generic message at the right position. Not for each row.
        # message("bas_start is not specified. Default value of 10 is used.")
        data$bas_start <- 10
        
      }
      
      if (is.na(data$bas_stop)) {
        
        # message("bas_stop is not specified. Default value of 20 is used.")
        data$bas_stop <- 20
        
      }
      
      data$bas_start:data$bas_stop
      
    }
    
    data$bas_diff[[1]][sel_times]
    
  }
  
  
  # DEBUGGING
  # calc_bas_mean(data = data2[15,])
  # slider::slide2_dbl(data2, 1:nrow(data2), ~{print(.y); calc_bas_mean(.x)})
  
  
  data2 <- data2 %>% dplyr::mutate(basal = slider::slide_dbl(., calc_bas_mean),
                                   bas_cova = slider::slide_dbl(., calc_bas_cova))
  
  data2 <- data2 %>% dplyr::mutate(pulse_bas_max = purrr::map_dbl(bas_raw, ~calc_hour_diffs(.x)[2:length(.x)] %>% max(na.rm = TRUE)),
                                   pulse_bas_test = dplyr::if_else(pulse_bas_max > 599, "ERROR PULSE", "OK"))
  
  # put back together
  
  if (only_sets) {
    
    data2 <- left_join(name_cs, bind_rows(others, data2), by = "name_c")
    data2 <- data2 %>% dplyr::relocate("name_c", .after = dplyr::all_of(nam_aft))
    
  }
  
  
  if (plot) {
    
    data2 <- data2 %>% dplyr::mutate(bas_plot = slider::slide(., plot_1bas))
    
  }
  
  data2
  
}


#' Calculate Cmic
#'
#' @param data dataset
#' @param plot logical
#' @param only_sets logical if TRUE will only recalculate basal measurements for entries that have bas_set value
#'
#' @return tibble
#' @export
o2_cmic <- function(data, plot = FALSE, only_sets = FALSE) {
  # data <- summary
  
  only_sets <- only_sets && "cmic_set" %in% names(data)
  
  if (only_sets) {
    
    nam_aft <- which(names(data) == "name_c")
    
    name_cs <- data %>% dplyr::select(name_c)
    others <- data %>% dplyr::filter(is.na(data$cmic_set))
    
    data <- data %>% dplyr::filter(!is.na(data$cmic_set))
    
  }
  
  
  # add machine correction factor
  data <- data %>% dplyr::mutate(cmic_corfct = purrr::map2_dbl(device, idSequence, ~ o2_corr_fct(.x, .y, meas = "mic")),
                                 cmic_raw_st = purrr::map2(cmic_raw, cmic_corfct,  ~ .x * .y))
  
  # NOTE mutiplied by ratio
  data2 <- data %>% dplyr::mutate(cmic_diff = purrr::map2(cmic_raw_st, wei_samp_dry, ~(calc_hour_diffs(.x) * .83) / .y))
  
  
  # for 1 row, get 3 values for cmic calc
  get_cmic_vals <- function(data) {
    
    data$cmic_diff[[1]][data$cmic_times[[1]]] * 0.7 * 38
    
  }
  
  calc_cmic_mean <- function(data) {
    
    mean(get_cmic_vals(data))
    
  }
  
  # for 1 row
  calc_cmic_cova <- function(data) {
    
    vals <- get_cmic_vals(data)
    
    sd(vals) / mean(vals)
    
  }
  
  
  # find 3 times for cmic calc
  find_cmic_times <- function(data) {
    
    if ("cmic_set" %in% names(data) && !is.na(data$cmic_set)) {
      
      data$cmic_set[[1]]
      
    } else {
      
      diffs <- data$cmic_diff[[1]]
      
      if(all(is.na(diffs))) return(NA)
      
      # calc moving average of 3 points for 3-5 to 8-10, take min
      mirr_means <- purrr::map_dbl(3:8, ~mean(diffs[.x:(.x+2)]))
      
      whmin <- which.min(mirr_means) + 2
      
      whmin:(whmin+2)
      
    }
    
  }
  
  data2 <- data2 %>% dplyr::mutate(cmic_times = slider::slide(., find_cmic_times)) %>% 
    dplyr::mutate(cmic = slider::slide_dbl(., calc_cmic_mean),
                  cmic_cova = slider::slide_dbl(., calc_cmic_cova))
  
  
  # pulse test
  data2 <- data2 %>% dplyr::mutate(pulse_cmic_max = purrr::map_dbl(cmic_raw, ~calc_hour_diffs(.x)[2:20] %>% max(na.rm = TRUE)),
                                   pulse_cmic_test = dplyr::if_else(pulse_cmic_max > 599, "ERROR PULSE", "OK"))
  
  if (only_sets) {
    
    data2 <- left_join(name_cs, dplyr::bind_rows(others, data2), by = "name_c")
    data2 <- data2 %>% dplyr::relocate("name_c", .after = dplyr::all_of(nam_aft))
    
  }
  
  if (plot) {
    
    data2 <- data2 %>% dplyr::mutate(cmic_plot = slider::slide(., plot_1bas))
    
  }
  
  data2  
  
}


# microbial growth
# slope of the log values CMIC hours 5 to 24?


# for one row
#' Prepare table for microbial growth calculation
#'
#' @param data dataset
#'
#' @return tibble
#' @export
sel_mgrowth_df <- function(data) {
  
  # print(data$name_c)
  
  cmic_df0 <- dplyr::tibble(mirr_d = data$cmic_diff[[1]],
                     time = seq_along(mirr_d),
                     cmic_d = mirr_d * 0.7 * 38,
                     incr = cmic_d - dplyr::lag(cmic_d),
                     incr2 = incr - dplyr::lag(incr),
                     cmic_log = log10(cmic_d)) %>% 
    dplyr::filter(time < 20, time >3)
  
  cmic_df <- cmic_df0
  
  
  # start at mirr
  cmic_df <- cmic_df %>% dplyr::filter(time >= min(data$cmic_times[[1]]))
  
  
  # remove decreasing tail values
  # last increasing value
  if (all(cmic_df$incr <= 0)) return(NA)
  
  until <- max(which(cmic_df$incr > 0))
  
  cmic_df <- cmic_df %>% dplyr::slice(seq_len(until))
  
  if(nrow(cmic_df) < 3) return(NA)
  
  
  # remove decreasing head values
  # first increasing value
  from <- min(which(cmic_df$incr > 0))
  
  if (from >= 3) {
    cmic_df <- cmic_df %>% dplyr::slice(-seq_len(from-2))
    
  }
  
  
  # stop if many decreasing values
  ch_incr <- cmic_df$incr >= 0
  
  # decreasing 3 times in a row
  fx3 <- slider::slide_lgl(ch_incr, ~ all(!.x),
                           .after = 2) %>%
    {.[1:(length(.) - 2)]}
  
  if (any(fx3)) {
    
    first_fx3 <- min(which(fx3))
    cmic_df <- cmic_df %>% dplyr::slice(seq_len(first_fx3))
    
  }
  
  
  # mod <- lm(cmic_log ~ time, cmic_df)
  # with(cmic_df, qplot(time, incr))
  
  
  # take until second increment = 0 (inflection point)
  # meaning the increment stopped increasing
  
  mod_incr <- lm(data = cmic_df, incr2 ~ time)
  coefs <- mod_incr$coefficients
  
  
  # remove retardation period
  if (coefs[2] < 0) {
    
    # 0 - b / a = x
    zero <- - coefs[1] / coefs[2]
    
    if (zero > 12) {
      
      cmic_df <- cmic_df %>% dplyr::filter(time <= zero)
      
    } else {
      # remove tail
      cmic_df <- cmic_df %>% dplyr::filter(time <= 12)
      
    }
  }
  
  # TODO return all values, with mgrow selection indicated
  
  # return cmic_df table
  if(nrow(cmic_df) < 3) {
    return(NA)
    
  } else {
    cmic_df
    
  }

}


#' Calculate Microbial growth
#'
#' @param data dataset
#' @param plot logical
#' @param only_sets logical if TRUE will only recalculate basal measurements for entries that have bas_set value
#'
#' @return tibble
#' @export
o2_mgrowth <- function(data, plot = FALSE) {
  # data <- summary[2,]
  # TODO get mirr times from cmic calculated to define first values used

  data2 <- data %>% dplyr::mutate(mgrow_df = slider::slide(., sel_mgrowth_df))
  
  calc_mgrowth <- function(data) {
    
    if (is.atomic(data) && (length(data) == 1) && is.na(data)) {
      
      NA
      
    } else {
      coef(lm(cmic_log ~ time, data = data))[2]
      
    }
    
  }

  data2 <- data2 %>% dplyr::mutate(mgrowth = purrr::map_dbl(mgrow_df, calc_mgrowth))
  
  if (plot) {
    
    data2 <- data2 %>% dplyr::mutate(mgrow_plot = slider::slide(., plot_1mgrowth))
    
  }
  
  data2
  
}






