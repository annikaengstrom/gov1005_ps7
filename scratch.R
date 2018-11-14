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

sen_gov_data <- all %>%
  filter(grepl("sen", source) == TRUE | grepl("gov", source) == TRUE) %>%
  mutate(district = str_sub(source, start= -11, end = -7))

# combine duplicated house polls with nonduplicated house polls then also with governor and senate polls
pre <- merge(dupes, no_dupes, all = TRUE)
pre <- merge(pre, sen_gov_data, all = TRUE)

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
  select(district, state, rep_adv, actual, error)

election_data <- error

# now, we mess with the context data to prepare it for the app.
# the context data is in terms of the state, no district available (county doesn't 
# necessary map on to district) ... so let's just group everything by state.

election_context <- election_context %>%
  group_by(state) %>%
  summarise(
    trump16 = sum(trump16),
    clinton16 = sum(clinton16),
    otherpres16 = sum(otherpres16),
    romney12 = sum(romney12), 
    obama12 = sum(obama12),
    otherpres12 = sum(otherpres12),
    white_pct = mean(white_pct) / 100,
    black_pct = mean(black_pct) / 100,
    hispanic_pct = mean(hispanic_pct) / 100,
    nonwhite_pct = mean(nonwhite_pct) / 100,
    foreignborn_pct = mean(foreignborn_pct) / 100,
    female_pct = mean(female_pct) / 100,
    age29andunder_pct = mean(age29andunder_pct) / 100,
    age65andolder_pct = mean(age65andolder_pct) / 100
  ) %>%
  mutate(pres16_total = trump16 + clinton16 + otherpres16, 
         pres12_total = romney12 + obama12 + otherpres12,
         trump_pct = trump16 / pres16_total, 
         clinton_pct = clinton16 / pres16_total, 
         otherpres16_pct = otherpres16 / pres16_total,
         obama_pct = obama12 / pres12_total,
         romney_pct = romney12 / pres12_total,
         otherpres12_pct = otherpres12 / pres12_total
  ) %>%
  select(state, trump_pct, clinton_pct, otherpres16_pct, obama_pct, romney_pct, otherpres12_pct, white_pct, black_pct,
         hispanic_pct, nonwhite_pct, foreignborn_pct, female_pct, age29andunder_pct, age65andolder_pct)

to_abb <- function(x) {
  i <- 1
  for(i in 1:length(state.name)) {
    if(x == state.name[i]) {
      x <- state.abb[i]
      break
    }
    i + 1
  }
  return(x)
}

election_context$state <- lapply(election_context$state, to_abb)

election_context <- election_context %>%
  mutate(state = as.character(state))

result_context <- left_join(election_data, election_context, by = "state")
         




