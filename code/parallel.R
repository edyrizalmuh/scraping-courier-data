library(tidyverse)
library(foreach)
library(doParallel)
business_name = "pos"
raw_dir = paste0("data/raw/",business_name,"/")

Sys.sleep(5)
(cl <- (detectCores() - 1) %>%  makeCluster) %>% registerDoParallel
Sys.sleep(5)
clusterEvalQ(cl, {
  library(RSelenium)
  library(netstat)
  library(tidyverse)
  business_name = "pos"
  raw_dir = paste0("data/raw/",business_name,"/")
  
  source("code/is_list.R")
  source("code/extract_data.R")
  source("code/scroll.R")
  source("code/extract_search_url.R")
  source("code/main.R")
  
  rs_obj =
    rsDriver(
      browser = "chrome",
      chromever = "114.0.5735.90",
      port = free_port()
    )
  remDr = rs_obj$client
  
  daftar_kota = 
    read_csv("data/clean/table_city_tidy.csv") %>% 
    pull(kabupaten_kota) %>% 
    str_to_lower()
  daftar_kota = daftar_kota
  main_url = "http://maps.google.com/"
  kabkota_done = list.files(raw_dir) %>% str_extract(".+(?=.csv)")
  kabkota_remaining = daftar_kota[!daftar_kota %in% kabkota_done]
  if (length(kabkota_done) == 0){
    visited_urls = c()
    write.csv(visited_urls, paste0(raw_dir,"visited_urls.csv"), row.names = FALSE)
  }
})

daftar_kota = 
  read_csv("data/clean/table_city_tidy.csv") %>% 
  pull(kabupaten_kota) %>% 
  str_to_lower()
daftar_kota = daftar_kota
main_url = "http://maps.google.com/"

kabkota_done = list.files(raw_dir) %>% str_extract(".+(?=.csv)")
kabkota_remaining = daftar_kota[!daftar_kota %in% kabkota_done]
if (length(kabkota_done) == 0){
  visited_urls = c()
  write.csv(visited_urls, paste0(raw_dir,"visited_urls.csv"), row.names = FALSE)
}

complete = NULL
while (is.null(complete)){
  kabkota_done = list.files(raw_dir) %>% str_extract(".+(?=.csv)")
  kabkota_remaining = daftar_kota[!daftar_kota %in% kabkota_done]
  complete =
    tryCatch({
      parSapply(
        cl, 
        kabkota_remaining, 
        function(x) 
          main(
            client = remDr, 
            business_name = business_name, 
            # aliases = "ninjaxpress",
            city = x, 
            dir = raw_dir
          )
      )
    }, error = function(e){
      return(NULL)
    })
}

