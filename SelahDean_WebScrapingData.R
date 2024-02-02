#Selah Dean 
#Project - What Contributes to a Major League Baseball Team’s Wins in a Season?
#Webscraping data

library(rvest)
library(tidyverse)

years <- c(c(2000:2019), c(2021:2023))

batting_urls <-  list()
for (i in 1:length(years)) {
  url = paste0("https://www.baseball-reference.com/leagues/majors/", years[i], "-standard-batting.shtml")
  batting_urls[[i]] = url
}
batting_urls <- batting_urls %>% unlist() %>% as.data.frame() %>%
  mutate(year = years) %>% rename(url = ".")

pitching_urls <-  list()
for (i in 1:length(years)) {
  url = paste0("https://www.baseball-reference.com/leagues/majors/", years[i], "-standard-pitching.shtml")
  pitching_urls[[i]] = url
}
pitching_urls <- pitching_urls %>% unlist() %>% as.data.frame() %>%
  mutate(year = years) %>% rename(url = ".")

fielding_urls <-  list()
for (i in 1:length(years)) {
  url = paste0("https://www.baseball-reference.com/leagues/majors/", years[i], "-standard-fielding.shtml")
  fielding_urls[[i]] = url
}
fielding_urls <- fielding_urls %>% unlist() %>% as.data.frame() %>%
  mutate(year = years) %>% rename(url = ".")

standing_urls <-  list()
for (i in 1:length(years)) {
  url = paste0("https://www.baseball-reference.com/leagues/majors/", years[i], "-standings.shtml")
  standing_urls[[i]] = url
}
standing_urls <- standing_urls %>% unlist() %>% as.data.frame() %>%
  mutate(year = years) %>% rename(url = ".")


scrape_data <- function(url, year) {
  df <- url %>% read_html() %>% html_nodes("table") %>%
      html_table(trim=T) %>% .[[1]] %>%
      mutate(Year = year) %>% slice(1:30)
  df
}

scrape_standing_data <- function(url, year){
  df <- url %>% read_html() %>% html_nodes("table") %>%
    html_table(trim=T) %>% bind_rows() %>%
    mutate(Year = year)
  df
}

batting <- 1:nrow(batting_urls) %>% map_df(function(x) scrape_data(batting_urls$url[x],batting_urls$year[x]))
pitching <- 1:nrow(pitching_urls) %>% map_df(function(x) scrape_data(pitching_urls$url[x],pitching_urls$year[x]))
fielding <- 1:nrow(fielding_urls) %>% map_df(function(x) scrape_data(fielding_urls$url[x],fielding_urls$year[x]))
standing <- 1:nrow(standing_urls) %>% map_df(function(x) scrape_standing_data(standing_urls$url[x],standing_urls$year[x]))

write.csv(batting,file='batting.csv', row.names=FALSE)
write.csv(pitching,file='pitching.csv', row.names=FALSE)
write.csv(fielding,file='fielding.csv', row.names=FALSE)
write.csv(standing,file='standing.csv', row.names=FALSE)
