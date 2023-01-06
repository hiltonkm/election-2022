# GOAL: Scrape 2022 election data from wikipedia

# Libraries --------------------------------------------------------------------
rm(list=ls())

library(tidyverse)
library(rvest)

# Globals ----------------------------------------------------------------------
wd <- 'C:/Users/kathe/OneDrive/Documents/Data Projects/GIT/election-2022/'
input <- paste0(wd, 'input/')
output <- paste0(wd, 'output/')
temp <- paste0(wd, 'temp/')
dir.create(temp)
spreadsheet <- 'https://docs.google.com/spreadsheets/d/1s20QjqiKOAjPICCiyRkFRh4DF-TCBDnKt4iUH-mI_so/edit#gid=0'

# source: https://www.census.gov/library/reference/code-lists/ansi.html
states <- googlesheets4::range_read("https://docs.google.com/spreadsheets/d/1Bxj2-d7R3npf-u2ghIji6sSc0PXk358UcRC7-9BbQS8/edit#gid=0",
                                    sheet="Sheet1")
# pattern <- paste0("(", paste0(states$stname, collapse="|"), ")") # regex pattern of all state names

# Load house data --------------------------------------------------------------

link <- read_html("https://en.wikipedia.org/wiki/2022_United_States_House_of_Representatives_elections")

link %>%
  html_elements("body") %>%
  html_text()

tables <- link %>%
  html_elements("table") %>%
  html_table()
tables

house_voting <- data.frame()
house_non_voting <- data.frame()
for (i in 13:67) {
  print(i)
  df <- as.data.frame(tables[i])
  if (length(colnames(df)) == 7) {
    colnames(df) <- c("district", "pvi_2022",
                      "incumbent","incumbent_party", "incumbent_first_elected", "incumbent_status",
                      "candidates")
    house_voting <- bind_rows(house_voting, df)
  }
  if (length(colnames(df)) == 6) {
    colnames(df) <- c("district",
                      "incumbent","incumbent_party", "incumbent_first_elected",
                      "results","candidates")
    house_non_voting <- bind_rows(house_non_voting, df)
  }
}
rm(df, tables, link, i)

## Note: pvi_2022 = "The Cook Partisan Voting Index, abbreviated Cook PVI, CPVI, or PVI,
## is a measurement of how strongly a United States congressional district or U.S. state
## leans toward the Democratic or Republican Party, compared to the nation as a whole,
## based on how that district or state voted in the previous two presidential elections

# Clean data (house voting) ----------------------------------------------------
raw <- house_voting
house_voting <- raw %>%
  filter(district != "Location") %>%
  separate(col=candidates, into = c("candidate_1","candidate_2","candidate_3",
                                    "candidate_4","candidate_5","candidate_6",
                                    "candidate_7","candidate_8","candidate_9",
                                    "candidate_10"),
           sep="\n") %>%
  pivot_longer(cols=7:16, names_to = "candidate_number", values_to="candidate_info") %>%
  filter(!is.na(candidate_info))

house_voting$district <- trimws(house_voting$district)
house_voting$candidate_won <- ifelse(str_detect(house_voting$candidate_info,".Y "), 1,0)
house_voting$candidate_info <- trimws(gsub(".Y ", "", house_voting$candidate_info))
house_voting$candidate_party <- str_extract(str_extract(house_voting$candidate_info, "\\(.+\\)"),"\\w+")
house_voting$candidate_margin <- str_extract(house_voting$candidate_info, "\\d+\\.?\\d+?")
house_voting$candidate_name <- str_extract(house_voting$candidate_info, "\\w+ \\w+")
house_voting <- house_voting %>%
  filter(!is.na(candidate_party)) %>%
  select(district, incumbent, incumbent_party, incumbent_first_elected, incumbent_status,
         candidate_name, candidate_party, candidate_margin,candidate_won, pvi_2022)
house_voting$STATE_NAME <- str_extract(house_voting$district, "\\w+ ?\\w*")
nrow(anti_join(house_voting, states, by="STATE_NAME")) == 0
house_voting <- house_voting %>%
  left_join(., states, by="STATE_NAME")

house_voting <- relocate(house_voting,STATE_NAME, STATE, STUSAB, STATENS)
house_voting$voting <- 1
# Clean data (house non voting) ------------------------------------------------

raw2 <- house_non_voting

house_non_voting <- raw2 %>%
  filter(district != "District") %>%
  separate(col=candidates, into = c("candidate_1","candidate_2","candidate_3", "candidate_4"),
           sep="\n") %>%
  pivot_longer(cols=6:9, names_to = "candidate_number", values_to="candidate_info") %>%
  filter(!is.na(candidate_info))

house_non_voting$district <- trimws(house_non_voting$district)
house_non_voting$candidate_won <- ifelse(str_detect(house_non_voting$candidate_info,".Y "), 1,0)
house_non_voting$candidate_info <- trimws(gsub(".Y ", "", house_non_voting$candidate_info))
house_non_voting$candidate_party <- str_extract(str_extract(house_non_voting$candidate_info, "\\(.+\\)"),"\\w+")
house_non_voting$candidate_margin <- str_extract(house_non_voting$candidate_info, "\\d+\\.?\\d+?")
house_non_voting$candidate_margin <- ifelse(str_detect(house_non_voting$candidate_info,"\\[\\d+\\]"), NA,house_non_voting$candidate_margin)
house_non_voting$candidate_name <- str_extract(house_non_voting$candidate_info, "\\w+ \\w+")
house_non_voting <- house_non_voting %>%
  select(district, incumbent, incumbent_party, incumbent_first_elected, incumbent_status=results,
         candidate_name, candidate_party, candidate_margin,candidate_won)
house_non_voting$STATE_NAME <- trimws(gsub(".at.large", "", house_non_voting$district))
house_non_voting$STATE_NAME

nrow(anti_join(house_non_voting, states, by="STATE_NAME")) == 0
house_non_voting <- house_non_voting %>%
  left_join(., states, by="STATE_NAME")

house_non_voting <- relocate(house_non_voting,STATE_NAME, STATE, STUSAB, STATENS)
house_non_voting$voting <- 0
# House data bind --------------------------------------------------------------------

house_races <- bind_rows(house_voting, house_non_voting)
house_races <- rename(house_races, STATE_FIPS=STATE)
googlesheets4::range_write(spreadsheet, house_races, sheet="house")

# Load senate data --------------------------------------------------------------

link <- read_html("https://en.wikipedia.org/wiki/2022_United_States_Senate_elections")

tables <- link %>%
  html_elements("table") %>%
  html_table()
link %>%
  html_elements("table") %>%
  html_text() %>%
  str_detect("Alabama")
## Note: we know the first row needs to be Alabama, using that to narrow down the
## tables to sort through to find the one we want (manually go through tables)
## that are TRUE, we find it's table 17
View(as.data.frame(tables[17]))

senate <- as.data.frame(tables[17])
colnames(senate) <- c("state",
                      "incumbent","incumbent_party", "incumbent_electoral_history",
                      "status","candidates")
senate <- senate[2:nrow(senate),]

# Clean data (senate) ----------------------------------------------------------
raw3 <- senate
senate <- raw3 %>%
  separate(col=candidates, into = c("candidate_1","candidate_2","candidate_3",
                                    "candidate_4","candidate_5","candidate_6",
                                    "candidate_7","candidate_8","candidate_9",
                                    "candidate_10", "candidate_11", "candidate_12",
                                    "candidate_13", "candidate_14", "candidate_15"),
           sep="\n") %>%
  pivot_longer(cols=6:20, names_to = "candidate_number", values_to="candidate_info") %>%
  filter(!is.na(candidate_info))

senate$candidate_won <- ifelse(str_detect(senate$candidate_info,".Y "), 1,0)
senate$candidate_info <- trimws(gsub(".Y ", "", senate$candidate_info))
senate$candidate_party <- str_extract(str_extract(senate$candidate_info, "\\(.+\\)"),"\\w+")
senate$candidate_margin <- str_extract(senate$candidate_info, "\\d+\\.?\\d+?")
senate$candidate_name <- str_extract(senate$candidate_info, "\\w+ \\w+")
senate <- senate %>%
  filter(!is.na(candidate_party)) %>%
  select(STATE_NAME=state, incumbent, incumbent_party, incumbent_electoral_history, status,
         candidate_name, candidate_party, candidate_margin,candidate_won)
nrow(anti_join(senate, states, by="STATE_NAME")) == 0
senate <- senate %>%
  left_join(., states, by="STATE_NAME")

senate <- relocate(senate,STATE_NAME, STATE, STUSAB, STATENS)

## Note: still need to clean incumbent_electoral_history and status
senate <- rename(senate, STATE_FIPS=STATE)
googlesheets4::range_write(spreadsheet, senate, sheet="senate")
