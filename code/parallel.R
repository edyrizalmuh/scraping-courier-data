library(tidyverse)
library(foreach)
library(doParallel)
business_name = "jne"
raw_dir = paste0("data/raw/",business_name,"/")

(cl <- (detectCores() - 1) %>%  makeCluster) %>% registerDoParallel
clusterEvalQ(cl, {
  library(RSelenium)
  library(netstat)
  library(tidyverse)
  
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
  main_url = "http://maps.google.com/"
  kabkota_done = list.files(raw_dir) %>% str_extract(".+(?=.csv)")
  kabkota_remaining = daftar_kota[!daftar_kota %in% kabkota_done]
})

daftar_kota = 
  read_csv("data/clean/table_city_tidy.csv") %>% 
  pull(kabupaten_kota) %>% 
  str_to_lower()
main_url = "http://maps.google.com/"
kabkota_done = list.files(raw_dir) %>% str_extract(".+(?=.csv)")
kabkota_remaining = daftar_kota[!daftar_kota %in% kabkota_done]

res = parSapply(
  cl, 
  kabkota_remaining, 
  function(x) main(
    client = remDr, 
    business_name = business_name, 
    aliases = "j&t", 
    city = x, 
    dir = raw_dir
  )
)
