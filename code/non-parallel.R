library(RSelenium)
library(netstat)
library(tidyverse)
library(foreach)
library(doParallel)

source("code/is_list.R")
source("code/extract_data.R")
source("code/scroll.R")
source("code/extract_search_url.R")
source("code/main.R")

daftar_kota = 
  read_csv("data/clean/table_city_tidy.csv") %>% 
  pull(kabupaten_kota) %>% 
  str_to_lower()
main_url = "http://maps.google.com/"


rs_obj =
  rsDriver(
    browser = "chrome",
    chromever = "114.0.5735.90",
    port = free_port()
  )
remDr = rs_obj$client

# kabkota_done = 
kabkota_done = list.files("data/raw/JNT/") %>% str_extract(".+(?=.csv)")
kabkota_remaining = daftar_kota[!daftar_kota %in% kabkota_done]
pb = progress_estimated(length(kabkota_remaining))

res = 
  for (kabkota in kabkota_remaining){
    main(
      client = remDr, 
      business_name = "jnt", 
      aliases = "j&t",
      city = kabkota
    )
    pb$tick()$print()
  }
