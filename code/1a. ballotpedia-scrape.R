# GOAL: Scrape data from ballotpedia

## Note: using this to get more comfortable with web scraping in R
## Source: https://www.dataquest.io/blog/web-scraping-in-r-rvest/
## Ballotpedia: https://ballotpedia.org/United_States_Senate_elections,_2022

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

# 50 states with fips (source: https://gist.github.com/dantonnoriega/bf1acd2290e15b91e6710b6fd3be0a53)
states <- read.csv(paste0(input, 'us-state-ansi-fips.csv'), stringsAsFactors = FALSE)

# US Senate 2022 ---------------------------------------------------------------

link <- rvest::read_html("https://ballotpedia.org/Election_results,_2022:_U.S._Congress")

# Finding where the table is of senate election results (using the table name)
link %>%
  html_elements("table") %>%
  html_text() %>%
  str_detect("2022 Senate election results")

# Pulling table data
senate_races <- as.data.frame(html_text(html_elements(link, "table"))[2])
colnames(senate_races) <- 'x'
senate_races$x <- gsub("\n", "line_break", senate_races$x, fixed = TRUE)
senate_races$x <- gsub("\\[\\d\\]", "line_break", senate_races$x)
senate_races <- senate_races %>%
  separate(., x,  as.character(c(1:300)),sep="line_break") %>%
  pivot_longer(cols=1:300) # makes the table 1 long data set (every observation is a cell of the table)
senate_races <- senate_races %>%
  filter(value != "",
         value != "2022 Senate election results",
         !is.na(value)) %>%
  mutate(value = trimws(value)) %>%
  select(-name) ## filtering out empty vales and cleaning out white space
column_names <- senate_races[1:6,1] ## finding column names
colnames(column_names) <- "col"
senate_races <- senate_races[7:nrow(senate_races),]
senate_races <- cbind(senate_races, column_names)

# Finding each new line (using fact that each line is a state)
pattern <- paste0("(", paste0(states$stname, collapse="|"), ")") # regex pattern of all state names
senate_races$new_line <- ifelse(grepl(pattern, senate_races$value),senate_races$value,NA)
senate_races <- fill(senate_races,new_line, .direction = "down") # creates a variable that is the state name that the cell references

# Wide version of table (now should be usable!)
senate_races <- senate_races %>%
  pivot_wider(id_cols=new_line, names_from = col, values_from=value) %>%
  select(-new_line)
colnames(senate_races)
colnames(senate_races) <- c("state", "incumbent", "seat_open", "winner", "flip", "margin")

# US House 2022 ----------------------------------------------------------------
# Using same process as above for house data

## OPEN SEATS
link <- rvest::read_html("https://ballotpedia.org/United_States_Congress_elections,_2022")

# Finding where the table is of senate election results (using the table name)
link %>%
  html_elements("table") %>%
  html_text() %>%
  str_detect("Open seats in the U.S. House going into the 2022 general election")

# Pulling table data
house_races <- as.data.frame(html_text(html_elements(link, "table"))[19])
colnames(house_races) <- 'x'
house_races$x
house_races$x <- gsub("\n", "line_break", house_races$x, fixed = TRUE)
house_races <- house_races %>%
  separate(., x,  as.character(c(1:237)),sep="line_break") %>%
  pivot_longer(cols=1:237)
house_races <- house_races %>%
  filter(value != "",
         value != "Open seats in the U.S. House going into the 2022 general election") %>%
  mutate(value = trimws(value)) %>%
  select(-name)

column_names <- house_races[1:4,1]
colnames(column_names) <- "col"
house_races <- house_races[5:nrow(house_races),]
house_races <- cbind(house_races, column_names)

# Finding each new line
house_races$new_line <- ifelse(grepl(pattern, house_races$value),house_races$value,NA)
house_races <- house_races %>%
  mutate(new_line=ifelse(col != "Seat", NA, new_line)) %>%
  fill(new_line, .direction = "down")

# Wide version of table (now should be usable!)
house_races <- house_races %>%
  pivot_wider(id_cols=new_line, names_from = col, values_from=value) %>%
  select(-new_line)

## Note: if want all house elections wikipedia might be the most
## streamlined place? Doesn't seem to be a single chart on ballotpedia
## that has them all (but does have lots of intresting cuts)
## wikipedia page: https://en.wikipedia.org/wiki/2022_United_States_House_of_Representatives_elections#Results_summary


