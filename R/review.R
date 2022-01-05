


#' Get set times
#'
#' @param id character
#' @param tib tibble with updated times to use
#'
#' @return times
#' @export
get_set_times <- function(id, tib) {
  
  if(id %in% tib[,1, drop = T]) {
    
    # dplyr::across(1, . == id)
    tib %>% dplyr::filter(name_c == id) %>% dplyr::pull(times) %>% purrr::chuck(1)
    
  } else {
    NA
  }
  
}


#' Set basal times manually
#' 
#' Adjust the calculation period for soil basal respiration manually. 
#' The column "times" in tib should be a vector of full hours to select
#' 
#' @param data dataset
#' @param tib tibble with columns "name_c" and "times".
#'
#' @note Currently only works with merged names (column name_c).
#' Duplicated sample names are not handled.
#'
#' @return
#' @export
set_bas_times <- function(data, tib) {
  
  # TODO add method to work with device*seq

  # error if many entries selected
  unique_name <- purrr::map_lgl(tib[,1] %>% unlist, ~ sum(.x == data$name_c) <= 1)
  assertthat::assert_that(all(unique_name))
  
  data <- data %>% dplyr::mutate(bas_set = purrr::map(name_c, get_set_times, tib = tib))
  
  data
  
}


#' Set cmic times
#'
#' @param data dataset
#' @param tib tibble
#'
#' @return tibble
#' @export
set_cmic_times <- function(data, tib) {

  # error if many entries selected
  duplicated_name <- purrr::map_lgl(tib[,1], ~ sum(.x == data$name_c) <= 1)
  assertthat::assert_that(all(!duplicated_name))
    
  data <- data %>% dplyr::mutate(cmic_set = purrr::map(name_c, get_set_times, tib = tib))
  
  data
  
}


# TODO detect remeasurements
