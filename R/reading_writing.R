#' Read Machine File
#' 
#' @param path to the file
#'
#' @return tibble
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
#' @param x numeric
#'
#' @return values
#' @export
calc_hour_diffs <- function(x) {
  
  max_ind <- floor((length(x))/4)
  
  ind <- seq_len(max_ind)*4
  
  vals <- x[ind]
  
  vals <- c(0, vals)
  
  (vals - lag(vals))[-1]
  
}



# automatic weighing sheet formatting -------------------------------------

#' Reformat weighing sheet
#'
#' Updated based on current format used in 2022-09
#'
#' @param file path
#'
#' @return invisible(file)
#' @export
reformat_weighing_sheet <- function(file) {
  
  # check each tab
  
  tabs <- readxl::excel_sheets(file)
  
  # .x <- tabs[1]
  walk(tabs, ~{
    
    message("Processing worksheet ", .x)
    
    wtab <- suppressMessages(
      readxl::read_xlsx(file, sheet = .x, col_names = F, col_types = "text", )
    )
    
    # in some case entries far down
    checkmate::assert(nrow(wtab) == 32)
    
    # remove empty column
    wtab <- wtab[!map_lgl(wtab, ~all(is.na(.x)))]
    
    create_weighing_sheet(wtab, basename(file), dirname(file))
    
  })
  
  invisible(file)
  
}



#' Check if the format fits expectations
#'
#' Currently fairly conservative, would need to be adapted to fit more funky files
#'
#' @param ws data.frame
#'
#' @return logical
#' @export
check_w_format <- function(ws) {
  # ws <- ws_all[[3]]
  
  
  row1 <- c("laufende Nr.", "Kanal / Pott Nr.", NA, "Bezeichnung Probe", 
            NA, NA, "Glasgefäß leer [g]", "Einwaage Frischgewicht [g]", "Einwaage Trockengewicht [g]", 
            NA, NA, NA, NA, NA)
  
  row2 <- c(NA, NA, "Anlage", "Sample name 1", "Sample name 2", "Sample name 3", 
            "Container empty [g]", "Sample fresh weight [g]", "Sample dry weight [g]", 
            "glucose / sample [mg]", "water added / sample [ml]", "date of measurement", 
            "date of sampling", "remarks")
  
  identical(as.character(ws[1,]), row1) & identical(as.character(ws[2,]), row2)
  
}


#' Check if the data is empty
#'
#' @param ws data.frame containing weighing data
#'
#' @return logical
#' @export
check_empty <- function(ws) {
  
  needed_cols <- ws[2,] %in% c("Sample name 1", "Sample name 2", "Sample name 3", "Container empty [g]", 
                               "Sample fresh weight [g]", "Sample dry weight [g]")
  
  # typically cols 4:9
  # which(needed_cols)
  
  all(is.na(unlist(ws[3:32, needed_cols])))
  
}




#' Create weighing sheet from dataframe
#' 
#' To be used to reformat weighing sheets.
#'
#' @param data data.frame
#' @param input_name character file name
#' @param output_dir path
#'
#' @return TRUE if file creation worked, NULL if the data is empty
#' @export
create_weighing_sheet <- function(data, input_name, output_dir) {
  # data <- ws
  
  checkmate::assert(check_w_format(data))
  
  if (check_empty(data)) {
    message("  Data is empty, no file was created")
    return(invisible(NULL))
    
  }
  
  # data rows
  dr <- nrow(data) %>% {(.-29):.}
  
  d_nams <- dplyr::coalesce(purrr::flatten_chr(data[2,]), purrr::flatten_chr(data[1,]))
  
  checkmate::assert(!any(duplicated(d_nams)))
  
  data_df <- data[dr,] %>% purrr::set_names(d_nams)
  
  # names(data_df) # %>% dput
  
  
  # weighing sheet template
  templ_path <- system.file("template", "w_template.xlsx", package = "o2eie")
  
  r_templ <- suppressMessages(
    readxl::read_xlsx(templ_path, col_names = F, col_types = "text")
  )
  
  header <- r_templ[3:4,] %>% purrr::set_names(r_templ[1,])
  
  content <- r_templ[8:nrow(r_templ),] %>% purrr::set_names(r_templ[6,])
  
  
  ## fill header
  
  # RMS files need to be added manually
  
  # date
  date1 <- data_df$`date of measurement` %>% {.[!. == "n/a"]} %>% unique
  checkmate::assert(length(date1) == 1)
  header$STARTDATE[1] <- openxlsx::convertToDate(date1) %>% as.character
  
  
  ## fill content
  
  # machine
  mach <- data_df$Anlage %>% {.[!. == "n/a"]} %>% unique
  checkmate::assert(length(mach) == 1)
  content$Device <- mach
  
  # Pot number should be 1:30
  checkmate::assert(identical(data_df$`Kanal / Pott Nr.`, as.character(1:30)))
  
  # copy many columns
  # names(content) %>% dput
  
  match_names <- tibble::tribble(
    ~templ_name,                    ~ws_name,
    "idSequence",              "laufende Nr.",
    "Sample name 1",             "Sample name 1",
    "Sample name 2",             "Sample name 2",
    "Sample name 3",             "Sample name 3",
    "Container empty [g]",       "Container empty [g]",
    "Sample fresh weight [g]",   "Sample fresh weight [g]",
    "Container + Sample dry weight [g]",     "Sample dry weight [g]",
    "Glucose / sample [mg]",     "glucose / sample [mg]",
    "Water added / sample [ml]", "water added / sample [ml]",
    "Date sampling",          "date of sampling",
    "Comments",                   "remarks"
  )
  
  # TODO if needed, could subset df to existing columns
  content[match_names$templ_name] <- data_df[match_names$ws_name]
  
  # replace n/a all columns
  content <- purrr::map_dfc(content, ~if_else(.x == "n/a", NA_character_, .x))
  
  # fix date
  content <- content %>% dplyr::mutate(`Date sampling` = openxlsx::convertToDate(`Date sampling`))
  
  # check if dry weights include glass container
  dw <- as.numeric(content$`Container + Sample dry weight [g]`)
  cw <- as.numeric(content$`Container empty [g]`)
  
  if(all(is.na(dw) | (dw < cw))) {
    content <- content %>% dplyr::mutate(`Container + Sample dry weight [g]` = as.character(
      as.numeric(`Container + Sample dry weight [g]`) + as.numeric(`Container empty [g]`)
    ))
    
  } else if (!(all(is.na(dw) | (dw > cw)))) {
    stop("Unclear if dry weights are with or without containers. Please check.")
    
  }
  
  
  # columns to numeric
  # content %>% names %>% dput
  
  content <- content %>% dplyr::mutate(dplyr::across(c(idSequence, `Container empty [g]`, `Sample fresh weight [g]`,
                                                       `Container + Sample dry weight [g]`, `Glucose / sample [mg]`,
                                                       `Water added / sample [ml]`), as.numeric))
  
  ## write to workbook and save
  wb_templ <- openxlsx::loadWorkbook(templ_path)
  
  # write header and content
  openxlsx::writeData(wb_templ, "Data", header, startCol = 1, startRow = 3, colNames = FALSE)
  openxlsx::writeData(wb_templ, "Data", content, startCol = 1, startRow = 8, colNames = FALSE)
  
  # openXL(wb_templ)
  
  
  # file name
  # w_machine_originalfile
  output_name <- paste0("w_", mach, "_", input_name %>% str_remove("^(pre_)?w_"))
  file_created <- openxlsx::saveWorkbook(wb_templ, file.path(output_dir, output_name), 
                                         returnValue = TRUE, overwrite = TRUE)
  
  invisible(file_created)
  
}



#' Reformat weighing sheet
#'
#' Project specific, not needed
#' NOTE: is template needed?
#' Changed the weighing sheet format to match the template used
#'
#' @param df data
#' @param info ...
#' @param template file
#'
#' @return
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


#' Title
#'
#' NOTE needs reimplementation
#' Write Excel file and format
#'
#' @param df data
#' @param path path
#'
#' @return NULL
write_xl_format <- function(df, path){
  #TODO write directly to excel file to keep formatting
  #loadworkbook and writeData
  # library(openxlsx)

  openxlsx::write.xlsx(df, path, col.names = FALSE)

  #format excel document
  weights_xl <- openxlsx::loadWorkbook(path, xlsxFile = NULL)

  #merge cells
  openxlsx::mergeCells(weights_xl, sheet = 1, cols = 3:5, rows = 3)
  openxlsx::mergeCells(weights_xl, sheet = 1, cols = 6:11, rows = 3)
  openxlsx::mergeCells(weights_xl, sheet = 1, cols = 12:13, rows = 3)

  openxlsx::mergeCells(weights_xl, sheet = 1, cols = 3:5, rows = 4)
  openxlsx::mergeCells(weights_xl, sheet = 1, cols = 6:11, rows = 4)
  openxlsx::mergeCells(weights_xl, sheet = 1, cols = 12:13, rows = 4)

  openxlsx::saveWorkbook(weights_xl, path, overwrite = TRUE)
}


#' Measurement times
#'
#' @param file path
#'
#' @return values 
#' @export
o2_meas_time <- function(file) {
  
  df <- file %>% o2_read_machine_file
  
  difftime(lubridate::dmy_hms(paste(df$Date[nrow(df)-1], df$Time[nrow(df)-1])),
           lubridate::dmy_hms(paste(df$Date[1], df$Time[1])),
           units = "hours") %>% as.numeric
}


#' Measurement times
#'
#' @param file path
#'
#' @return values 
#' @export
o2_meas_times <- function(files) {
  purrr::map_dbl(files, o2_meas_time)

  }


#' Raw dates
#'
#' @param file path
#'
#' @return date
#' @export
o2_raw_date <- function(file){
  
  file %>% o2_read_machine_file %>% pull(Date) %>% .[1] %>% lubridate::dmy() %>% as.character
  
}


#' Raw dates
#'
#' @param file path
#'
#' @return date
#' @export
o2_raw_dates <- function(files) {
  purrr::map_chr(files, o2_raw_date)
  
}


#' Check weighing sheet format
#' 
#' Either all in one ws (see w_template.xlsx) or split on two. If is two, the worksheets need to be labelled "info" and "data".
#' 
#'
#' @param path 
#'
#' @return character indicating format used
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


#' Extract raw path
#'
#' @param file path
#'
#' @return character
#' @export
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


#' Extract raw path
#'
#' @param file path
#'
#' @return character
#' @export
extract_raw_path <- function(files) {
  
  assertthat::assert_that(is.character(path))
  
  purrr::map(files, extract_raw_path1)
  
}


