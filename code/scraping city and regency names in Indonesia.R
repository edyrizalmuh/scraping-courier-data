
# Data scraping -----------------------------------------------------------

# install.packages("rvest")
# install.packages("janitor")

library(rvest) # for scraping
library(tidyverse) # for data wrangling
library(janitor) # for data cleaning
link = "https://id.wikipedia.org/wiki/Daftar_kabupaten_dan_kota_di_Indonesia"
page = read_html(link)

# extact all tables
table_all = 
  page %>% 
  html_elements("table.sortable") %>%
  html_table(dec = ",")
saveRDS(table_all, "data/raw/all cities and regencies.rds") # table_all

# the first table is the table of all provinces in Indonesia
table_prov = table_all[[1]] %>% clean_names()
prov_names = head(table_prov$provinsi, nrow(table_prov)-1)

# the remaining tables are the tables of cities and regencies in each province
table_city = table_all[-1]

# Data cleaning -----------------------------------------------------

# numbers of columns of each province table are different from each other
num_of_col = lapply(1:length(table_city), function(x) ncol(table_city[[x]])) %>% as.numeric()
idx_messy = which(num_of_col > 10)

# Extract unique column names from all tables
all_column_names <- unique(unlist(lapply(table_city, colnames)))

# Create an empty matrix with rows representing tables and columns representing unique column names
result_matrix <- matrix(0, nrow = length(table_city), ncol = length(all_column_names))
colnames(result_matrix) <- all_column_names

# Fill the matrix with 1 if the column name is present in the respective table
for (i in 1:length(table_city)) {
  col_indices <- match(colnames(table_city[[i]]), all_column_names)
  result_matrix[i, col_indices] <- 1
}

# count how many cities/regencies have the corresponding column names
result_tibble =
  result_matrix %>%
  t() %>%
  as_tibble() %>%
  mutate(col_names = rownames(t(result_matrix))) %>%
  relocate(col_names)
colnames(result_tibble) = colnames(result_tibble) %>% str_replace_all("V", "")
result_tibble
result_tibble %>% 
  rowwise() %>%
  mutate(total = sum(c_across(where(is.numeric)))) %>%
  select(col_names, total) %>% 
  arrange(col_names)


# 5th city: header becomes the first row
table_city[[5]] = table_all[[6]][-1,]
colnames(table_city[[5]]) = table_all[[6]] %>% slice(1) %>% as.character()

# 8th city: misplace first row
temp1 = table_all[[9]][1,11:20]
temp2 = table_all[[9]][-1,1:10]
colnames(temp1) = colnames(temp2)
temp1$No. = as.character(temp1$No.)
temp1$`Jumlah penduduk (2020)[12]` = as.character(temp1$`Jumlah penduduk (2020)[12]`)
temp1$Kecamatan = as.character(temp1$Kecamatan)
table_city[[8]] = bind_rows(temp2, temp1)

# 11th city: similar to the 8th city, but more messy
temp1 = table_all[[12]][1,11:19]
temp2 = table_all[[12]][-1,1:10] # 12th table in table_all, the 1st table is the province table

colnames(temp1) = colnames(table_all[[12]][2:10])
temp1 = 
  temp1 %>% 
  mutate('No.' = '1') %>%
  relocate('No.')

temp1$No. = as.character(temp1$No.)
temp1$`Jumlah penduduk (2015)[16]` = as.character(temp1$`Jumlah penduduk (2015)[16]`)
temp1$Kecamatan = as.character(temp1$Kecamatan)
temp1$Kelurahan = as.character(temp1$Kelurahan) 
temp1$`Luas wilayah(km²)[15]` = as.character(temp1$`Luas wilayah(km²)[15]`)
table_city[[11]] = bind_rows(temp1, temp2)

# 37th city: 2 new columns (Ref. and a blank column)
table_city[[37]] = table_all[[38]][,1:10]

# function for cleaning column names
clean_name_table = function(x){
  temp =
    x %>%
    select(-any_of(c("No.", "Ref.", "IPM\n(2020)", "Lambang", "Lambang alt", "Peta lokasi")))
    
  names(temp) =
    names(temp) %>%
    str_replace("Kabupaten(/Kota|/kota)?( administrasi\\[14])?", "kabupaten_kota") %>%
    str_replace("Ibu kota(\\[\\d+\\])?|Pusat pemerintahan", "ibu_kota") %>%
    str_replace("Bupati(/Walikota|/wali kota)?( administrasi)?", "bupati_wali_kota") %>%
    str_replace("[Ll]uas [Ww]ilayah(?:\\s*\\([^\\)]+\\))?(?:\\[\\d+\\])?", "luas_wilayah_diff_sources_and_years") %>%
    str_replace("Jumlah [pP]enduduk(?: \\([^\\)]+\\))?(?:\\[[^\\]]+\\])*(?:\\[[^\\]]+\\])?", "jumlah_penduduk_diff_sources_and_years") %>%
    str_replace("Ibu kota(\\[\\d+\\])?", "ibu_kota") %>%
    str_replace("Kecamatan|Distrik|Kapanewon/kemantren", "kecamatan") %>%
    str_replace("Gampong|Kelurahan(?:\\/desa|\\/kalurahan|\\/kampung)?", "kelurahan_desa")
  
  temp = 
    temp %>%
    mutate(
      kabupaten_kota = as.character(kabupaten_kota),
      ibu_kota = as.character(ibu_kota),
      bupati_wali_kota = as.character(bupati_wali_kota),
      luas_wilayah_diff_sources_and_years = str_replace_all(luas_wilayah_diff_sources_and_years, "\\.", ""),
      luas_wilayah_diff_sources_and_years = str_replace_all(luas_wilayah_diff_sources_and_years, ",", "."),
      luas_wilayah_diff_sources_and_years = str_replace_all(luas_wilayah_diff_sources_and_years, " ", ""),
      luas_wilayah_diff_sources_and_years = as.numeric(luas_wilayah_diff_sources_and_years),
      jumlah_penduduk_diff_sources_and_years = str_replace_all(jumlah_penduduk_diff_sources_and_years, "\\.", ""),
      jumlah_penduduk_diff_sources_and_years = str_replace_all(jumlah_penduduk_diff_sources_and_years, " ", ""),
      jumlah_penduduk_diff_sources_and_years = str_replace_all(jumlah_penduduk_diff_sources_and_years, " ", ""),
      jumlah_penduduk_diff_sources_and_years = as.numeric(jumlah_penduduk_diff_sources_and_years),
      ibu_kota = as.character(ibu_kota),
      kecamatan = as.integer(kecamatan),
      kelurahan_desa = as.character(kelurahan_desa)
    )
  return(temp)
}

table_city_clean_colnames = tibble()
prov_names = head(table_prov$provinsi, nrow(table_prov)-1)
for (i in 1:length(table_city)){
  temp = 
    clean_name_table(table_city[[i]]) %>%
    mutate(provinsi = prov_names[i]) %>%
    relocate(provinsi)
  table_city_clean_colnames = bind_rows(table_city_clean_colnames, temp)
  rm(temp)
}

table_city_clean_colnames

# filter rows with na
table_city_clean_colnames %>% 
  filter_all(any_vars(is.na(.)))

table_prov = table_prov %>% select(-no)

# write.csv(table_city_clean_colnames, "data/raw/list of cities and regencies.csv", row.names = FALSE)
# write.csv(table_prov, "data/raw/list of provinces.csv", row.names = FALSE)

table_city_tidy =
  table_city_clean_colnames %>%
  mutate(
    kabkota = str_extract(kabupaten_kota, "Kabupaten|Kota"),
    kabkota = ifelse(is.na(kabkota), "Kabupaten", kabkota),
    kabupaten_kota = str_remove(kabupaten_kota, "Kabupaten Administrasi |Kabupaten |Kota Administrasi |Kota "),
    ibu_kota = ifelse(ibu_kota == "-", kabupaten_kota, ibu_kota),
    kelurahan = str_extract(kelurahan_desa, "[0-9	]+(?=/)"),
    kelurahan = ifelse(is.na(kelurahan), 0, kelurahan) %>% as.integer(),
    desa = str_extract(kelurahan_desa, "(?<=/)[0-9	]+"),
    desa = ifelse(is.na(desa), 0, desa) %>% as.integer(),
    total_kelurahan_desa = kelurahan+desa,
    total_kelurahan_desa = 
      ifelse(total_kelurahan_desa==0, kelurahan_desa, total_kelurahan_desa) %>%
      as.integer()
  ) %>%
  select(-c("kelurahan", "desa", "kelurahan_desa")) %>%
  relocate(provinsi, kabupaten_kota, kabkota) 

table_city_tidy %>% View()
write.csv(table_city_tidy, "data/clean/table_city_tidy.csv", row.names = FALSE)

table_prov_tidy =
  table_prov %>% 
  select(-no) %>% 
  filter(wilayah != "Jumlah")
write.csv(table_prov_tidy, "data/clean/table_prov_tidy.csv", row.names = FALSE)
