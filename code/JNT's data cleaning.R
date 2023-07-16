library(tidyverse)
files = list.files("data/raw/JNT")
raw_data = tibble()
i = 0
for (file in files){
  temp = read_csv(paste0("data/raw/JNT/", file)) %>% suppressMessages()
  raw_data = bind_rows(raw_data, temp)
  rm(temp)
  i = i + 1
  cat(paste("\r", i, " of ", length(files)))
}
raw_data %>% View()
