library(fs)
library(tidyverse)
library(janitor)

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

total_house_data <- all %>%
  filter(grepl("sen", source) == FALSE, grepl("gov", source) == FALSE) 

# extract just the district from list of file names
districts <- str_sub(file_names, start= -10, end = -7)
# duplicated gives true false list on if there's a duplicate
dupes_list <- duplicated(districts)
# prepare a list to place duplicated districts in
dupes_data <- list()

# prepare counters
i <- 1
j <- 1
# for each district
for(i in 1:length(districts)) {
  # if theres a duplicate
  if(dupes_list[[i]] == TRUE) {
    # add it to the list of dupes
    dupes_data[[j]] <- districts[[i]]
    # add 1 to list counter so next duplicate goes in the next spot on the list
    j <- j + 1
  }
  # go to the next district
  i <- i + 1
}

# get the last poll for duplicated districts

dupes <- total_house_data %>%
  mutate(district = str_sub(source, start= -10, end = -7)) %>%
  filter(district %in% dupes_data) %>%
  select(district, response, final_weight, source) %>%
  mutate(poll = str_sub(source, start= -5, end = -5))

# change 2 to 1 because in pa, first poll was in second wave (no other second waves)
dupes$poll[dupes$poll == "2" | dupes$poll == "1"] <- "Poll 1"
# change 3 to 2 to represent that its the 2nd poll in that district
dupes$poll[dupes$poll == "3"] <- "Poll 2"

dupes <- dupes %>%
  filter(response == "Rep" | response == "Dem", poll == "Poll 2") %>%
  select(district, response, final_weight)

# get nonduplicated districts

no_dupes <- total_house_data %>%
  mutate(district = str_sub(source, start= -10, end = -7)) %>%
  filter(! district %in% dupes_data) %>%
  select(district, response, final_weight)

# combine duplicated house polls with nonduplicated house polls then also with governor and senate polls
pre <- merge(dupes, no_dupes, all = TRUE)

# calculate the rep adv for all the polls
pre <- pre %>%
  # calculate adv
  group_by(district) %>%
  mutate(total = sum(final_weight)) %>%
  group_by(response, district, total) %>%
  tally(wt = final_weight) %>%
  spread(response, n) %>%
  group_by(district) %>%
  mutate(rep_adv = (Rep - Dem) / total) %>%
  # make district pretty
  select(district, rep_adv) %>%
  ungroup() %>%
  mutate(district = toupper(district)) %>%
  mutate(district = paste(district = substr(district, 1, 2), "-", substr(district, 3, nchar(district)), sep = ""))

remove_zero <- function(district_code) {
  district_code <- gsub("-0", "-", district_code)
  return(district_code)
}

pre$district <- lapply(pre$district, remove_zero)

# district changed into unknown type for some reason, change back to character

pre <- pre %>%
  mutate(district = as.character(district))

# get the results data

results_data <- read_csv("mt_2_results.csv")

# change it to match formatting of other data

# match senate and gov
results_data$district[results_data$district == "Senate"] <- "SEN"
results_data$district[results_data$district == "Governor"] <- "GOV"

# match district variable

results <- results_data %>%
  mutate(district = paste(0, district, sep = "")) %>%
  select(state, district, dem_votes, rep_votes)

results <- results %>%
  unite("district", state, district, sep = "-", remove = FALSE)

results$district <- lapply(results$district, remove_zero)

# district changed into unknown type for some reason, change back to character

results <- results %>%
  mutate(district = as.character(district))

# join the poll data and results data

combo <- left_join(pre, results, by = "district")

# calculate the actual rep adv

combo <- combo %>%
  mutate(actual = (rep_votes - dem_votes) / (rep_votes + dem_votes))

# calculate error margin

error <- combo %>%
  mutate(error = actual - rep_adv) %>%
  select(district, state, rep_adv, actual, error) %>%
  adorn_pct_formatting()

# now, we mess with the context data to prepare it for the app.



