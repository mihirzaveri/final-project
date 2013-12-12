get_state_url <- function(stateAbbr) {
  
  first_part <- "https://www.govtrack.us/congress/members/"
  url <- paste(first_part, stateAbbr, sep = "")
  url <- htmlParse(url)
  return(url)
  
}

setwd("/home/mihir/Documents/grad_school/dataviz-fall-2013/final-project/working/scraping")

state_url <- "https://www.govtrack.us/congress/members/CA"
page <- htmlParse(state_url)
