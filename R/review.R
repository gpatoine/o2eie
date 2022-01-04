




get_set_times <- function(id, tib) {
  
  if(id %in% tib[1]) {
    trib %>% filter(across(1, . == id)) %>% pull(times) %>% chuck(1)
  } else {
    NA
  }
  
}


set_bas_times <- function(data, tib) {
  
  # TODO add method to work with device*seq
  
  # error if many entries selected
  duplicated_name <- map_lgl(tib[,1], ~ sum(.x == data$name_c) <= 1)
  assertthat::assert_that(all(!duplicated_name))
  
  data <- data %>% mutate(bas_set = map(name_c, get_set_times, tib = tib))
  
}


set_cmic_times <- function(data, tib) {
  
  data <- data %>% mutate(cmic_set = map(name_c, get_set_times, tib = tib))
  
}




# detect remeasurements



# remove remeasurements
