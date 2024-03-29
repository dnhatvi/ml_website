files_source <- file.path("../../../../[02] Ads Optimization/FUNCTIONS",
                          list.files("../../../../[02] Ads Optimization/FUNCTIONS"))

r_files <- files_source %>% 
  map_chr(~ str_extract(., "^.+\\.R"))

r_files <- r_files[!is.na(r_files)]

purrr::walk(r_files, source)
