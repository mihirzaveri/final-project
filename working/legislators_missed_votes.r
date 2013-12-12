setwd("Documents/grad_school/dataviz-fall-2013/final-project/working/")
options(stringsAsFactors = FALSE)
legislator_ids <- read.csv("legislators-current.csv")

govtrack_ID <- legislator_ids[,c(1,2,23)]

govtrack_ID$state <- as.character(legislator_ids$state)

brown_sherrod <- read.csv("legislator_csvs/400050.csv")
brown_sherrod_2013 <- subset(brown_sherrod, brown_sherrod$session == 2013)
sum(brown_sherrod_2013$missed_votes)

links<- dir("mihir2")
data <- NULL

load_data <- function(id) {
     thing <- paste("mihir2/",id,sep="")
     return(data)
 }

for (i in links) {
	df <- read.csv(load_data(i))
	df$file <- i
	data <- rbind(data, df)
}

house_data <- subset(data, data$chamber == "h")
senate_data <- subset(data, data$chamber == "s")

house_2013 <- subset(house_data, house_data$session == 2013)

id_split <- strsplit(house_2013$file, "[.]")
get_first_element <- function(element) {
	element[1]
}
house_2013$id_only <- sapply(id_split, get_first_element)

house_2013_totals <- aggregate(house_2013$missed_votes, by = list(house_2013$id_only), sum)
names(house_2013_totals) <- c("id", "missed_votes")

h2013_totals_ordered <- house_2013_totals[order(house_2013_totals$missed_votes, decreasing = T),]

na_totals <- subset(h2013_totals_ordered, is.na(h2013_totals_ordered$last_name) == TRUE)

nonfactor_h2013totals <- h2013_totals_ordered[,1:4]
rownames(nonfactor_h2013totals) <- 1:436

nonfactor_h2013totals$last_name[6] <- "Young"
nonfactor_h2013totals$first_name[6] <- "Bill"
nonfactor_h2013totals$last_name[78] <- "Bonner"
nonfactor_h2013totals$first_name[78] <- "Jo"
nonfactor_h2013totals$last_name[200] <- "Emerson"
nonfactor_h2013totals$first_name[200] <- "Jo Ann"

final_2013_data <- nonfactor_h2013totals

#swing_last_names <- read.csv("swing_last_names.csv")
#swing_last_names <- as.vector(swing_last_names)
#final_2013_data[,"swing"] <- NA
#steve king
#final_2013_data$swing[153] <- TRUE

#swing district data from Daily Kos

swing_districts <- read.csv("swing_legislators - Sheet1.csv", header = FALSE)

names(swing_districts) <- c("district", "legislator", "party","obama12","romney12","obama08","mccain08")

swing_districts$diff12 <- swing_districts$obama12 - swing_districts$romney12

swing_actual <- subset(swing_districts, (swing_districts$diff12 > -5) & (swing_districts$diff12 < 5))

swing_actual <- swing_actual[,c(2,8)]
swing_actual$leg_nonfactor <- as.character(swing_actual$legislator)

swing_split <- strsplit(swing_actual$leg_nonfactor,",")
library(plyr)
swing_split_names <- ldply(swing_split)
names(swing_split_names) <- c("last", "first")
swing_actual$last <- swing_split_names$last
swing_actual$first <- swing_split_names$first

final_2013_working <- final_2013_data
final_2013_working$combined_name <- paste(final_2013_working$last_name, ","," ", final_2013_working$first_name, sep = "")

#swing-join section

#default is all legislators are not from swing, then we will mark the swing with TRUEs in the swing column

final_2013_working$swing <- FALSE

#where do the swing legislators show up in the missed vote data?

match_order <- match(swing_actual$leg_nonfactor, final_2013_working$combined_name)

# FIRST: tackle the ones that the matching worked
match_order_nona <- match_order[!is.na(match_order)]
#how many are there (to find how many times looping over)
length(match_order_nona)

for (i in 1:38){
     final_2013_working[match_order_nona[i],"swing"] <- TRUE
}

# why does the above for loop work when final_2013_working$swing[match_order_nona,] <- TRUE not work

#SECOND: find the ones that did not work in matching

match(swing_actual$leg_nonfactor, final_2013_working$combined_name)

#indexes of NA values in match_order

non_matches <- which(is.na(match_order))
swing_actual[non_matches,]

#indexes of the non matches in final_2013_working, have to manually check these: 189, 234, 59, 303, 430, 352, 431, 419, 291, 136

subset(final_2013_working, final_2013_working$last_name == "McKeon") #checks out
subset(final_2013_working, final_2013_working$last_name == "Royce") #checks out
subset(final_2013_working, final_2013_working$last_name == "Rogers") #two mike rogers, first mike rogers (400342) is correct
subset(final_2013_working, final_2013_working$last_name == "Walz") #checks out
subset(final_2013_working, final_2013_working$last_name == "Heck") #checks out
subset(final_2013_working, final_2013_working$last_name == "Bishop") #checks out
subset(final_2013_working, final_2013_working$last_name == "Meehan") #checks out
subset(final_2013_working, final_2013_working$last_name == "Dent") #checks out
subset(final_2013_working, final_2013_working$last_name == "Rigell") #checks out
subset(final_2013_working, final_2013_working$last_name == "Forbes") #checks out

final_2013_working$swing[non_match_index] <- TRUE

colors <- rep("yellow", 436)
swing_numbers <- which(final_2013_working$swing == TRUE)
colors[swing_numbers] <- "cyan"
barplot(final_2013_working$missed_votes, col = colors)

#break it out by state

final_2013_working$id <- as.numeric(final_2013_working$id)
id_data$govtrack_id <- as.numeric(id_data$govtrack_id)

id_match_numbers <- match(final_2013_working$id, id_data$govtrack_id)
final_2013_working$state <- id_data$state[id_match_numbers]

#may or may not need to do the following, may have failed to load properly from earlier

final_2013_working$last_name <- as.character(final_2013_working$last_name)
final_2013_working$first_name <- as.character(final_2013_working$first_name)

final_2013_working$last_name[78] <- "Bonner"
final_2013_working$first_name[78] <- "Jo"
final_2013_working$last_name[200] <- "Emerson"
final_2013_working$first_name[200] <- "Jo Ann"
final_2013_working$last_name[205] <- "Alexander"
final_2013_working$first_name[205] <- "Rodney"

#fixing the NAs, Bill Young who died
final_2013_working$state[6] <- "FL"
#jo bonner, AL 1st
final_2013_working$state[78] <- "AL"
#jo ann emerson, MO 8th
final_2013_working$state[200] <- "MO"
#rodney alexander 
[[code here]]


#trying out a function to port over the no match differences
differences <- function(last_names, indexes, data){
  for (i in 1:9){
    diff <- subset(swing_actual, swing_actual$last == last_names[i])[,"diff12"]
    data[indexes[i],"diff12"] <- diff
  }
}

#failing at function, manually inserting difference values for no matches

> subset(swing_actual, swing_actual$diff12 == "McKeon")
[1] legislator    diff12        leg_nonfactor last          first        
<0 rows> (or 0-length row.names)
> subset(swing_actual, swing_actual$diff12 == "Royce")
[1] legislator    diff12        leg_nonfactor last          first        
<0 rows> (or 0-length row.names)
> subset(swing_actual, swing_actual$last == "McKeon")
legislator diff12 leg_nonfactor   last first
46 McKeon, Buck   -1.9  McKeon, Buck McKeon  Buck
> subset(swing_actual, swing_actual$last == "Royce")
legislator diff12 leg_nonfactor  last first
60  Royce, Ed   -3.7     Royce, Ed Royce    Ed
> subset(swing_actual, swing_actual$last == "Rogers")
legislator diff12   leg_nonfactor   last    first
206 Rogers, Mike J.   -3.1 Rogers, Mike J. Rogers  Mike J.
> subset(swing_actual, swing_actual$last == "Walz")
legislator diff12 leg_nonfactor last first
213  Walz, Tim    1.4     Walz, Tim Walz   Tim
> subset(swing_actual, swing_actual$last == "Heck")
legislator diff12 leg_nonfactor last first
270  Heck, Joe    0.8     Heck, Joe Heck   Joe
> subset(swing_actual, swing_actual$last == "Bishop")
legislator diff12 leg_nonfactor   last first
272 Bishop, Tim    0.5   Bishop, Tim Bishop   Tim
> subset(swing_actual, swing_actual$last == "Meehan")
legislator diff12 leg_nonfactor   last first
331 Meehan, Pat   -1.9   Meehan, Pat Meehan   Pat
> subset(swing_actual, swing_actual$last == "Dent")
legislator diff12 leg_nonfactor last    first
339 Dent, Charlie   -2.9 Dent, Charlie Dent  Charlie
> subset(swing_actual, swing_actual$last == "Rigell")
legislator diff12 leg_nonfactor   last  first
403 Rigell, Scott    1.5 Rigell, Scott Rigell  Scott
> subset(swing_actual, swing_actual$last == "Forbes")
legislator diff12 leg_nonfactor   last  first
405 Forbes, Randy   -1.3 Forbes, Randy Forbes  Randy

diff_numbers <- c(-1.9, -3.7, -3.1, 1.4, .8, .5, -1.9, -2.9, 1.5, -1.3)
final_nonmatch_index <- c(189, 234, 59, 303, 430, 352, 431, 419, 291, 136)
final_2013_working[final_nonmatch_index, ]$diff12 <- diff_numbers

library(ggplot2)
ggplot(final_2013_working, aes(x=abs(final_2013_working$diff12), y=final_2013_working$missed_votes)) + geom_point(shape=1) + geom_smooth(method=lm)

#BEGINNING 2012 DATA IMPORT

for(i in files){
  df <- read.csv(i)
  df$id <- i
  data <- rbind(data, df)
}

read the csv, and rbind it to the previous data

#get a list of all the people so I know I have all of them, in this case using WaPo's list
url <- "http://projects.washingtonpost.com/congress/112/house/members/"
library(XML)
wapo <- readHTMLTable(url)
table <- as.data.frame(wapo[[6]])

data$chamber_nf <- as.character(data$chamber)
house_data <- subset
house_2012 <- subset(house_data, house_data$session == 2012)

#get id by itself in it's own column
id_split <- strsplit(house_2012$id, "[.]")
get_first_element <- function(element) {
     element[1]
 }
house_2012$id_only <- sapply(id_split, get_first_element)

house_2012totals <- aggregate(house_2012$missed_votes, by = list(house_2012$id_only), sum)
names(house_2012totals) <- c("id", "missed_votes")
h2012_totals_ordered <- house_2012totals[order(house_2012totals$missed_votes, decreasing = T),]

#old legislators
legislator_ids <- read.csv("../legislators-historic.csv")
govtrack_id <- legislator_ids[,c(1,2,4,5,6,7,23)]

h2012_totals_ordered$id <- as.numeric(h2012_totals_ordered$id)
govtrack_id$govtrack_id <- as.numeric(govtrack_id$govtrack_id)

match_numbers_historic <- match(h2012_totals_ordered$id, govtrack_id$govtrack_id)

h2012_totals_ordered[,c("last_name", "first_name", "gender", "state", "party")] <- govtrack_id[match_numbers_historic, c("last_name", "first_name","gender","state","party")]

#legislators still in congress
leg_current_id <- read.csv("../legislators-current.csv")
govtrack_current_id <- leg_current_id[,c(1,2,4,5,6,7,23)]

match_numbers_current <- match(h2012_totals_ordered$id, govtrack_current_id$govtrack_id)
nona_match_current <- match_numbers_current[!is.na(match_numbers_current)]

current_legs_temp[,c("last_name", "first_name","gender","state","party")] <- govtrack_current_id[nona_match_current, c("last_name", "first_name","gender","state","party")]

#ALL FOLLOWING DIDN'T WORK

if the id of the first matches the id of the second, put the columns in the other one

for(i in h2012_totals_ordered$id) {
  if(is.na(h2012_totals_ordered$last_name[i]) == TRUE){
    h2012_totals_ordered$last_name[i] <- subset(current_legs_temp, current_legs_temp$id == h2012_totals_ordered$id[i])[,"last_name"]
  }
  else{
  }
}

combine_legs <- function(i){
  if(is.na(h2012_totals_ordered$last_name[i]) == TRUE){
    h2012_totals_ordered$last_name[i] <- subset(current_legs_temp, current_legs_temp$id == h2012_totals_ordered$id[i])[,"last_name"]
  }
  else{
  }
}

#UNTIL HERE
test_data <- rbind(h2012_totals_ordered, current_legs_temp)
test_data_ordered <- test_data[order(test_data$last_name),]
test_data_ordered[1:440,]


