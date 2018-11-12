library(fs)
library(tidyverse)

# download upshot elections data

download.file(url = "https://goo.gl/ZRCBda",
              destfile = "master.zip",
              quiet = TRUE,
              mode = "wb")

unzip("master.zip")

file_delete("master.zip")

# read context data

election_context <- read_csv("https://raw.githubusercontent.com/MEDSL/2018-elections/master/election-context-2018.csv")

# read all the upshot data identified by source

file_names <- dir_ls("2018-live-poll-results-master/data/")
all <- map_dfr(file_names, read_csv, .id  = "source")

# get just house data

house <- all %>%
  filter(grepl("sen", source) == FALSE, grepl("gov", source) == FALSE) 

