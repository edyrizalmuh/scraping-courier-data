library(RSelenium)
library(netstat)
library(tidyverse)
library(foreach)
library(doParallel)

business_name = "ninja"
raw_dir = paste0("data/raw/",business_name,"/")

source("code/is_list.R")
source("code/extract_data.R")
source("code/scroll.R")
source("code/extract_search_url.R")
source("code/main.R")

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

rs_obj =
  rsDriver(
    browser = "chrome",
    chromever = "114.0.5735.90",
    port = 14414L
  )
remDr = rs_obj$client

# kabkota_done = 
pb = progress_estimated(length(kabkota_remaining))
res = 
  for (kabkota in kabkota_remaining){
    main(
      client = remDr, 
      business_name = business_name, 
      # aliases = "j&t", 
      city = kabkota, 
      dir = raw_dir
    )
    pb$tick()$print()
  }
