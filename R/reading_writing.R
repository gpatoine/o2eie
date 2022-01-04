



#' Read Machine File
#'
#' 
#'
#' @param path to the file
#'
#' @return
#' @export
o2_read_machine_file <- function(path) {
  # path <- bas_path
  
  # no path provided
  if (is.na(path)) {
    # files_all$BAS_file[files_all$weighing_sheet == basename(w_file)] <- "Error: Missing path"
    stop("missing path")
  }
  
  # wrong file format
  if (!tools::file_ext(path) %in% c("txt", "csv")) {
    # files_all$BAS_path[files_all$weighing_sheet == basename(w_file)] <- "Error: Wrong file extension"
    stop("wrong file extension")
  }
  
  # file doesn't exist
  if (!file.exists(path)) {
    # files_all$BAS_file[files_all$weighing_sheet == basename(w_file)] <- "Error: Missing file"
    stop("missing file")
  }
  
  o2_ext <- tools::file_ext(path)
  
  
  if (o2_ext == "txt") {
    
    raw_data0 <- readr::read_delim(path, delim = ";", col_types = cols(Time = col_character()))
    raw_data <- raw_data0[,-c(33:34)]
    
  } else if (o2_ext == "csv") {
    
    suppressMessages(
      raw_data0 <- readr::read_csv(path, col_names = FALSE, na = c(NA, ""))
    )
    
    # issue with some files
    if (length(raw_data0) == 1) {
      raw_data0 <- read.csv(path, header = F, sep = ';', skip = 7 ,na = c(NA, ""), as.is = T)
      names(raw_data0) <- raw_data0[1,]
      raw_data <- raw_data0[2:(nrow(raw_data0)-2),]
      raw_data <- raw_data %>% dplyr::mutate(across(starts_with("S"), as.numeric))
      
    } else {
      names(raw_data0) <- raw_data0[8,]
      raw_data <- raw_data0[9:(nrow(raw_data0)-2),]
    }
  }
  
  raw_data
  
}


#' Calculate hourly differences
#'
#' Uses the difference between every fourth value, similar to calculations 
#' using the Excel template.
#' 
#'
#' @param x 
#'
#' @return
#' @export
calc_hour_diffs <- function(x) {
  
  max_ind <- floor((length(x))/4)
  
  ind <- seq_len(max_ind)*4
  
  vals <- x[ind]
  
  vals <- c(0, vals)
  
  (vals - lag(vals))[-1]
  
}


#' Reformat weighing sheet
#'
#' Changed the weighing sheet format to match the template used
#'
#' @param df 
#' @param info 
#' @param template 
#'
#' @return
#' @export
reformat_weish <- function(df, info = NULL, template = "iSBio"){
  library(readxl)
  
  if (template == "iSBio") {
    suppressMessages(
      
      # TODO add template file (in inst) and update script
      template <- read_xlsx("C:/Users/gp63dyte/Documents/Projects_local/isbio_r/rawdata/o2_autofill/3_TEMPLATES/w_template.xlsx",
                            sheet = "Data", col_names = FALSE))
  }
  
  #fill defaults
  template[3:4,6] <- "Alfred Lochner"
  template[8:37, 10] <- "FS"
  
  template[8:37, c(1:4, 7:9)] <- df[3:32, 1:7]
  
  #add blocks to sample names
  titles <- df[2,]
  lowtitles <- tolower(titles)
  if ("block" %in% lowtitles) {
    template[8:37, 5] <- df[3:32, which(lowtitles == "block"), drop = T]
  }
  
  #add pID to remarks
  if ("personal id" %in% lowtitles) {
    template[8:37, 14] <- paste0("pID=", df[3:32, which(lowtitles == "personal id"), drop = T])
  }
  
  # remarks
  if ("remarks" %in% lowtitles) {
    template[8:37, 14] <- paste0(df[3:32, which(lowtitles == "remarks"), drop = T])
  }
  
  #H2O
  if ("H2O/sample" %in% titles) {
    template[8:37, 12] <- df[3:32, which(titles == "H2O/sample"), drop = T]
  } 
  
  #Glucose
  if ("glc/sample" %in% lowtitles) {
    template[8:37, 11] <- df[3:32, which(lowtitles == "glc/sample"), drop = T]
  }
  
  if(!is.null(info)){ 
    #bas + cmic files
    template[3,3] <- info$bas_new
    template[4,3] <- info$cmic_new
  }
  
  template
}


write_xl_format <- function(df, path){
  #TODO write directly to excel file to keep formatting
  #loadworkbook and writeData
  library(openxlsx)
  
  write.xlsx(df, path, col.names = FALSE)
  
  #format excel document
  weights_xl <- loadWorkbook(path, xlsxFile = NULL)
  
  #merge cells
  mergeCells(weights_xl, sheet = 1, cols = 3:5, rows = 3)
  mergeCells(weights_xl, sheet = 1, cols = 6:11, rows = 3)
  mergeCells(weights_xl, sheet = 1, cols = 12:13, rows = 3)
  
  mergeCells(weights_xl, sheet = 1, cols = 3:5, rows = 4)
  mergeCells(weights_xl, sheet = 1, cols = 6:11, rows = 4)
  mergeCells(weights_xl, sheet = 1, cols = 12:13, rows = 4)
  
  saveWorkbook(weights_xl, path, overwrite = TRUE)
}


#' Title
#'
#' @param file 
#'
#' @return
#' @export
#'
#' @examples
o2_meas_time <- function(file) {
  
  df <- file %>% o2_read_machine_file
  
  difftime(lubridate::dmy_hms(paste(df$Date[nrow(df)-1], df$Time[nrow(df)-1])),
           lubridate::dmy_hms(paste(df$Date[1], df$Time[1])),
           units = "hours") %>% as.numeric
  
  
}

o2_meas_times <- function(files) purrr::map_dbl(files, o2_meas_time)




#' Title
#'
#' @param file 
#'
#' @return
#' @export
o2_raw_date <- function(file){
  
  file %>% o2_read_machine_file %>% pull(Date) %>% .[1] %>% lubridate::dmy() %>% as.character
  
}

o2_raw_dates <- function(files) purrr::map_chr(files, o2_raw_date)



#' Check weighing sheet format
#' 
#' Either all in one ws or split on two
#'
#' @param path 
#'
#' @return
check_wei_format <- function(path) {
  
  ns <- readxl::excel_sheets(path)
  
  if (length(ns) == 1) {
    
    suppressMessages(
    w_sheet <- readxl::read_xlsx(path, col_names = FALSE, col_types = "text", na = c("NA", ""))
    )
    
    assertthat::assert_that(isTRUE(all.equal(w_sheet[1, 1:3] %>% unlist, c("STARTDATE", "TYPE", "FILENAME"), check.names = FALSE)) &
      isTRUE(all.equal((w_sheet[6:7, 1] %>% unlist), c("idSequence", NA), check.names = FALSE)),
      msg = "Unexpected weighing sheet format. Please refer to the documentation.")
    
    return("together")
    
  } else if (length(ns) == 2) {
    
    assertthat::assert_that(all(readxl::excel_sheets(path) == c("info", "data")),
                            msg = "The weighing sheet tabs are mislabelled. Please refer to the documentation.")
    
    return("split")
    
  } else {
    
    stop("Wrong number of tabs in the weighing_sheets.")
    
  }
  
}


extract_raw_path1 <- function(file) {
  
  frmt <- check_wei_format(file)
  
  if (frmt == "together") {
    
    suppressMessages(
    readxl::read_xlsx(file, col_names = FALSE, col_types = "text", na = c("NA", ""))[3:4, 3] %>% unlist %>% unname
    )
    
  } else if (frmt == "split") {
    
    readxl::read_xlsx(file, sheet = "info", col_types = "text", na = c("NA", "")) %>%
      # TODO ..2 generates note when building package. Check for better way
      filter(...1 %in% c("bas_file", "mic_file")) %>% pull(..2)

  }
  
}

extract_raw_path <- function(files) {
  
  assertthat::assert_that(is.character(path))
  
  purrr::map(files, extract_raw_path1)
  
}


