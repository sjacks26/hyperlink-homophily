####################################################################################
#
# Project: Echo Chambers
# Auths: Sam, Jeff
# Date(ish): Dec 2017
#
# This is the fifth script in our data process.
#
# This script reads in a json file that contains URLs to be combined. It searches for similar URLs taht we want to treat as 
# equivalent (e.g., blogs.cnn.com and politics.cnn.com), then transforms them to an equivalent string (e.g., cnn.com).
# 
# This script aggregates up to candidate domain counts. 
# It creates a new 3-column file (Candidate, Domain, Count) for each platform that we use for analysis.
#
# To create the within party analysis file, need to remove messages from after May 31, 2016
####################################################################################

output_file_name <- "./FB_within_20180103.csv"

library(jsonlite)
# The next object contains the list of URLs to be combined:
urls <- fromJSON("./urls_to_combine_Dec14.json", flatten = TRUE)

# The next few lines contain all the usernames of the candidates we are interested in
FB_dems <- c("Bernie Sanders", "Jim Webb", "Hillary Clinton", "Martin O'Malley", "Lincoln Chafee")
TW_dems <- c("BernieSanders", "MartinOMalley", "HillaryClinton", "JimWebbUSA", "LincolnChafee")
FB_reps <- c("Bobby Jindal", "Carly Fiorina", "Marco Rubio", "Jeb Bush", "Ted Cruz", "Chris Christie", "Scott Walker", 
             "John Kasich", "Rick Perry", "Rand Paul", "Mike Huckabee", "Dr. Ben Carson", "George E. Pataki", 
             "Lindsey Graham", "Rick Santorum", "Donald J. Trump", "Jim Gilmore")
TW_reps <- c("BobbyJindal", "carlyforamerica", "ChrisChristie", "GovernorPataki", "GovernorPerry", "gov_gilmore",
             "GovMikeHuckabee", "JebBush", "JohnKasich", "LindseyGrahamSC", "marcorubio", "RandPaul", "RealBenCarson",
             "realDonaldTrump", "RickSantorum", "ScottWalker", "tedcruz",
             "Gov. Bobby Jindal", "CARLY for America", "Gov. Mike Huckabee", "Senator Rand Paul", "Ben Carson", "George Pataki", "Donald Trump")
dems <- c(FB_dems, TW_dems)
reps <- c(FB_reps, TW_reps)

# The next block reads in data and does some basic pre-processing.
#  "dat" is the primary object the script uses, so the line that assigns "dat" will change when we want to process different data
#  At the end of this block, "dat" contains only messages with at least 1 url
# old_fb <- read.csv("/home/rstudio/urls_network_analysis/data/Facebook/FB_domains_date_restricted.csv", quote = "\"", sep = "," , header = TRUE, stringsAsFactors = FALSE)
new_fb <- read.csv("./FB_w_types.csv", quote = "\"", sep = "," , header = TRUE, stringsAsFactors = FALSE)
# old_tw <- read.csv("/home/rstudio/urls_network_analysis/Replication_files/ICA/TW_cand_domains_date_restricted_Oct27.csv", quote = "\"", sep = "," , header = TRUE, stringsAsFactors = FALSE)
# colnames(old_tw) <- c("tweet_id", "Candidate_name", "created_at", "url_count", "urls", "expanded_urls", "domains", "time_to_parse_domaints", "shortened_links_present", "mentions", "hashtags", "followers_count", "message")
# new_tw <- read.csv("/home/rstudio/urls_network_analysis/data/Twitter/domains_fromcand_urls_SQL_Dec5_date_restrictedDec18.csv", quote = "\"", sep = "," , header = TRUE, stringsAsFactors = FALSE)
even_newer_tw <- read.csv("./domains_fromTW_domains_post_API_Dec28.csv", quote = "\"", sep = "," , header = TRUE, stringsAsFactors = FALSE)
## These commented out lines are for changing the screen names in the old TW data (which used actual screen names, rather than candidate names) to make it easier to compare old and new data
# bad_names <- unique(old_tw$Candidate_name)
# good_names <- unique(new_tw$Candidate_name)
# for (t in 1:length(bad_names)) {
#   old_tw$Candidate_name <- gsub(bad_names[t], good_names[t], old_tw$Candidate_name)
# }
dat <- even_newer_tw
# If we're using FB data, dom_source is always SQL. TW data is merged between SQL and Mongo, so dom_source should be API (unless it's old TW data).
dom_source <- "API" # API or SQL
platform <- "Twitter" # Twitter or Facebook
analysis <- "Within" # Within or Across
if (platform == "Facebook") {
  colnames(dat)[9] <- "url_count"
  dat <- subset(dat, url_count > 0)
  dat$created_at <- as.POSIXct(strptime(dat$created_at, "%Y-%m-%d %H:%M:%S"))
} else {
  dat <- subset(dat, API_url_count > 0)
  dat$created_at <- as.POSIXct(strptime(dat$created_at, "%Y-%m-%d"))
}

# The next block takes the data and adds a party variable based on candidate names. It then removes any messages from candidates not identified in our lists of candidate names for each party.
dat$party <- "-"
dat$party[dat$Candidate_name %in% reps] <- "R"
dat$party[dat$Candidate_name %in% dems] <- "D"
dat <- subset(dat, party != "-")

# The next if statement removes messages sent after the GOP nomination was effectively decided.
if (analysis == "Within") {
  dat <- subset(dat, created_at < as.POSIXct(strptime("2016-05-31", "%Y-%m-%d")))
}

# The next block takes the data, and for each message that has more than 1 url, it creates duplicates (based on the number of urls) of that
#  row so that we have 1 url per row of the dataframe. At the end of this block, "dat" has 1 row per url per message.
if (dom_source == "API") {
  multiple_urls <- which(dat$API_url_count > 1)
} else {
  multiple_urls <- which(dat$url_count > 1)
}  
new_dat <- dat[1,]

for (i in 1:length(multiple_urls)) {
  message_df <- tmp.df <- dat[multiple_urls[i], ]
  if (dom_source == "API") {
    domain_list <- unlist(strsplit(dat$API_domains[multiple_urls[i]], split = "\\|"))
  } else {
    domain_list <- unlist(strsplit(dat$domains[multiple_urls[i]], split = "\\|"))
  }
  for (j in 2:length(domain_list)) {
    message_df <- rbind(message_df, tmp.df)
  }
  if (dom_source == "API") {
    message_df$API_domains <- domain_list
  } else {
    message_df$domains <- domain_list
  }
  new_dat <- rbind(new_dat, message_df)
}
new_dat <- new_dat[-1, ]
dat <- dat[-multiple_urls, ]
dat <- rbind(dat, new_dat)

if (dom_source == "API") {
  dat$API_domains <- tolower(gsub("^www.", "", dat$API_domains, ignore.case = TRUE))
} else {
  dat$domains <- tolower(gsub("^www.", "", dat$domains, ignore.case = TRUE))
}

# The next block takes the list of urls to be combined, searches for them in the domains column, and replaces the matches with what each match should be replaced with according to the urls object
if (dom_source == "API") {
  dat$good_domains <- dat$API_domains
  for (i in 1:length(urls)) {
    index <- unlist(sapply(X = urls[[i]], FUN = function (X) { grep(X, dat$API_domains)}, simplify = "array"))
    dat$good_domains[index] <- names(urls[i])
  }
} else {
  dat$good_domains <- dat$domains
  for (i in 1:length(urls)) {
    index <- unlist(sapply(X = urls[[i]], FUN = function (X) { grep(X, dat$domains)}, simplify = "array"))
    dat$good_domains[index] <- names(urls[i])
  }
}

# The next block of lines creates a new dataframe with 3 columns: Candidate, domain, number of times a candidate links to that domain
dyads_df <- data.frame(Candidate = character(), Domain = character(), Count = numeric())
candidates <- unique(dat$Candidate_name)
for (i in 1:length(candidates)) {
  candidate_index <- which(dat$Candidate_name == candidates[i])
  domain_table <- table(dat$good_domains[candidate_index])
  candidate_df <- data.frame(Candidate = rep(candidates[i], length(domain_table)), Domain = names(domain_table), Count = as.numeric(domain_table))
  dyads_df <- rbind(dyads_df, candidate_df)
}

write.csv(dyads_df, file = output_file_name , row.names = FALSE)
