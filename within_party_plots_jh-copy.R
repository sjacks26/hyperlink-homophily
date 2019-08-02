####################################################################################
#
# Project: Echo Chambers
# Auths: Sam, Jeff
# Date(ish): October 2017 for ICA Nov 1 deadline  -- UPDATED AUG 2019
#
# Script does some data cleaning and network plots of within Republican party.
# First makes a network plot, tries bipartitie and denogram
#  - switch from FB to Twit by toggling what gets loaded into dat
#
####################################################################################

library(readr)
library(igraph)
library(RColorBrewer)
display.brewer.all()

# main stage republicans on both FB and Twitter
repubs <- c("JebBush", "Jeb Bush"                   #
            , "Dr. Ben Carson", "Ben Carson"        #
            , "ChrisChristie", "Chris Christie"     #
            , "tedcruz", "Ted Cruz"                 #   
            , "Carly Fiorina", "CARLY for America"
            , "Mike Huckabee", "Gov. Mike Huckabee" #
            , "JohnKasich", "John Kasich"           #
            , "RandPaul", "Rand Paul"               #
            , "marcorubio", "Marco Rubio"           #
            , "Donald Trump", "Donald J. Trump"     #
            , "Scott Walker", "GovWalker"           #
)

col.vec <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11)
col.set <- c(brewer.pal(11, "Paired"), "darkblue")[col.vec]
col.set <- c("brown1", "darkgreen", "darkgoldenrod2", "darkorange4", "cornflowerblue", "darkorange", "darkmagenta", "chartreuse3", "deeppink", "purple1", "yellow2")[col.vec]
repubs.cols <- data.frame(repubs = repubs, col = col.set, stringsAsFactors = FALSE)

dat.fb <- read.csv("./FB_within_20180103.csv", quote = "\"", sep = "," , header = TRUE, stringsAsFactors = FALSE)
dat.tw <- read.csv("./TW_within_20180103.csv", quote = "\"", sep = "," , header = TRUE, stringsAsFactors = FALSE)

platform.text <- "Facebook"
dat <- dat.fb

# platform.text <- "Twitter"
# dat <- dat.tw


nonweighted <- FALSE
platform.vec <- c("Facebook", "Twitter")
for (platform in 1:2) { 
  
  # platform <- 1 # testing code
  platform.text <- platform.vec[platform]
  
  if (platform.text == "Facebook") {
    dat <- dat.fb
  } else {
    dat <- new.tw
  }

  dat$party <- "-"
  # dat$party[dat$Candidate %in% dems] <- "D"
  dat$party[dat$Candidate %in% repubs] <- "R"
  
  
  R.dat <- subset(dat, party == "R")
  R.cands <- names(table(R.dat$Candidate))
  
  ignore_domains <- c("bencarson.com", "berniesanders.com", "bobbyjindal.com", "carlyfiorina.com",
                      "chafee2016.com", "christiefornj.com", "donaldjtrump.com", "hillaryclinton.com", "jeb2016.com",
                      "johnkasich.com", "lindseygraham.com", "marcorubio.com", "martinomalley.com", "mikehuckabee.com",
                      "randpaul.com", "rickperry.org", "ricksantorum.com", "scottwalker.com", "tedcruz.org"
                      , "shortened", "content_hosting_analytics_shopping_crowdfunding")
  
  additional_domains_to_ignore <- c("chrischristie.com", "bencarsonbook.com", "candycarsonbook.com", "cruzcommander.com", "fixourborder.com", "goodtobeaclinton.com",
                                    "maketrumpdebateagain.com")
  
  Nov15_domains_to_ignore <- c("173.63.141.16:8005", "â\u0080¦", "t.â\u0080¦", "tâ\u0080¦", "t.câ\u0080¦", "whycarly.com",
                               "googletrends.github.io", "trends.google.com", "amazon.com", "google.com", "m.tmi.me",
                               "spark.adobe.com")
  
  ignore_domains <- c(ignore_domains, additional_domains_to_ignore, Nov15_domains_to_ignore)
  
  linked.list <- data.frame(from = R.dat$Candidate, to = R.dat$Domain, weight = R.dat$Count, stringsAsFactors = FALSE)
  
  zap <- which(linked.list$to %in% ignore_domains)
  linked.list <- linked.list[-zap, ]
  # linked.list <- subset(linked.list, from != "CARLY for America")
  
  ##################
  # Descriptives
  #################
  
  cand_link_counts <- cbind(sort(table(linked.list$from)))
  
  length(unique(linked.list$to))
  
  #linked.list$weight
  
  #table(table(linked.list$to))
  
  #linked.list$from[linked.list$to == "foxnews.com"]
  #linked.list$weight[linked.list$to == "foxnews.com"]
  
  #length(unique(linked.list$from))
  
  #table(linked.list$to)
  
  #cbind(sort(table(linked.list$to), decreasing = TRUE))
  
  # The next line gives the top 9 domains by the number of candidates who link to that domain
  domain_cand_count <- sort(table(linked.list$to), decreasing = TRUE)[1:9]
  
  tmp.dom.df <- aggregate(linked.list$weight, list(domain = linked.list$to), sum)
  tmp.cand.df <- aggregate(linked.list$weight, list(domain = linked.list$from), sum)
  # o <- order(tmp.dom.df$x, decreasing = TRUE)
  domain_link_count <- tmp.dom.df[tmp.dom.df$domain %in% names(domain_cand_count), ]
  
  
  #######################
  #
  # Main plots
  #
  #######################
  g <- graph_from_data_frame(linked.list)
  
  #E(g)$weight <- 1
  g <- simplify(g, edge.attr.comb="sum")
  V(g)$type <- TRUE
  V(g)$type[V(g)$name %in% linked.list$to] <- FALSE
  V(g)$degree <- degree(g)
  V(g)$shape <- "square"
  V(g)$shape[V(g)$type] <- "circle"
  V(g)$size <- 0 # 5 + (10 * degree(g)/max(degree(g)))
  tmp.size <- round(.1 + (3 * E(g)$weight/max(E(g)$weight)), 1)
  #plot(tmp.size)
  E(g)$width <- tmp.size
  V(g)$frame.color <- NA
  
  E(g)$color <- "gray"
  for (i in 1:22) {
    
    my.rgb <- col2rgb(repubs.cols$col[i])
    new.color <- rgb(my.rgb[1], my.rgb[2], my.rgb[3], alpha = 80, maxColorValue = 255)
    
    E(g)[.inc(V(g)$name == repubs.cols$repubs[i])]$color <- new.color 
  }
  E(g)[.to(V(g)$degree > 1)]$color <- rgb(0, 0, 0, .6)
  
  if (nonweighted) {
    E(g)$width <- 2
    E(g)[.to(V(g)$degree > 1)]$width <- .25
  }
  
  V(g)$label.color <- "black"
  for (i in 1:22) {
    # i <- 1
    index <- which(V(g)$name == repubs.cols$repubs[i])
    if (length(index) > 0) {
      my.neighbors <- neighbors(g, V(g)$name[index])$name
      V(g)$label.color[V(g)$name %in% my.neighbors] <- repubs.cols$col[i]
    }
  }
  V(g)$label.color[V(g)$degree > 1] <- "black"

  l <- layout_as_bipartite(g)
  V(g)$x <- l[,2]
  V(g)$y <- l[,1]
  
  V(g)$label.cex <- .3
  V(g)$label.cex[V(g)$type] <- 1
  
  edge.index <- which(E(g)$weight > 5)
  v.index <- get.edges(g, edge.index)
  
  fb.edge.df <- data.frame(from = V(g)$name[v.index[, 1]], to = V(g)$name[v.index[, 2]], weight = E(g)$weight[edge.index])
  o <- order(fb.edge.df$weight, decreasing = TRUE)
  fb.edge.df <- fb.edge.df[o, ]
  
  if (platform.text == "Facebook") {
    par(mar = c(0, 4, 2, 2))
    par(mfrow = c(1,2))
  }
  plot.igraph(g, edge.arrow.size=0, edge.arrow.width=0, asp = 0)
  mtext(text = platform.text, side = 3, line = 0, adj = 0)
  
  edge.index <- which(E(g)$weight > 5)
  v.index <- get.edges(g, edge.index)
  
  print(V(g)$name[v.index[, 2]])
  print(V(g)$name[v.index[, 1]])
  
  
  
  
}

















stop()

tmp <- bipartite_projection(g, type = V(g)$type)
g2 <- tmp[[2]]
size <- E(g2)$weight
size <- size/max(size)
plot(size)

size <- size * 10
E(g2)$width <- size

plot(g2)

g2.mat <- as.matrix(as_adjacency_matrix(g2, attr = "weight"))
image(g2.mat)

stop()

##############################################
##############################################
##
## GOP chunks
##
##############################################
##############################################

library(readr)
library(igraph)
library(RColorBrewer)

# main stage republicans on both FB and Twitter
repubs <- c("JebBush", "Jeb Bush"                   #
            , "RealBenCarson", "Dr. Ben Carson"     #
            , "ChrisChristie", "Chris Christie"     #
            , "tedcruz", "Ted Cruz"                 #   
            , "Carly Fiorina", "CARLY for America"
            , "Mike Huckabee", "Gov. Mike Huckabee" #
            , "JohnKasich", "John Kasich"           #
            , "RandPaul", "Senator Rand Paul"       #
            , "marcorubio", "Marco Rubio"           #
            , "realDonaldTrump", "Donald J. Trump"  #
            , "Scott Walker", "GovWalker"           #
)

col.vec <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11)
col.set <- c(brewer.pal(11, "Set3"), "darkblue")[col.vec]
repubs.cols <- data.frame(repubs = repubs, col = col.set, stringsAsFactors = FALSE)



tw_GOP_1 <- read_csv("/home/rstudio/urls_network_analysis/data/dyads/within_party/GOP_chunks/dyads/GOP_primary_dyads1.csv")
tw_GOP_2 <- read_csv("/home/rstudio/urls_network_analysis/data/dyads/within_party/GOP_chunks/dyads/GOP_primary_dyads2.csv")
tw_GOP_3 <- read_csv("/home/rstudio/urls_network_analysis/data/dyads/within_party/GOP_chunks/dyads/GOP_primary_dyads3.csv")
tw_GOP_4 <- read_csv("/home/rstudio/urls_network_analysis/data/dyads/within_party/GOP_chunks/dyads/GOP_primary_dyads4.csv")
tw_GOP_5 <- read_csv("/home/rstudio/urls_network_analysis/data/dyads/within_party/GOP_chunks/dyads/GOP_primary_dyads5.csv")
tw_GOP_6 <- read_csv("/home/rstudio/urls_network_analysis/data/dyads/within_party/GOP_chunks/dyads/GOP_primary_dyads6.csv")

fb_GOP_1 <- read_csv("/home/rstudio/urls_network_analysis/data/dyads/within_party/GOP_chunks/dyads/GOP_primary_dyads_fb1.csv")
fb_GOP_2 <- read_csv("/home/rstudio/urls_network_analysis/data/dyads/within_party/GOP_chunks/dyads/GOP_primary_dyads_fb2.csv")
fb_GOP_3 <- read_csv("/home/rstudio/urls_network_analysis/data/dyads/within_party/GOP_chunks/dyads/GOP_primary_dyads_fb3.csv")
fb_GOP_4 <- read_csv("/home/rstudio/urls_network_analysis/data/dyads/within_party/GOP_chunks/dyads/GOP_primary_dyads_fb4.csv")
fb_GOP_5 <- read_csv("/home/rstudio/urls_network_analysis/data/dyads/within_party/GOP_chunks/dyads/GOP_primary_dyads_fb5.csv")
fb_GOP_6 <- read_csv("/home/rstudio/urls_network_analysis/data/dyads/within_party/GOP_chunks/dyads/GOP_primary_dyads_fb6.csv")

# end1 <- as.POSIXct(strptime("9/21/2015", "%m/%d/%Y"))
# end2 <- as.POSIXct(strptime("2/10/2016", "%m/%d/%Y"))
# end3 <- as.POSIXct(strptime("2/20/2016", "%m/%d/%Y"))
# end4 <- as.POSIXct(strptime("3/1/2016", "%m/%d/%Y"))
# end5 <- as.POSIXct(strptime("3/15/2016", "%m/%d/%Y"))
# end6 <- as.POSIXct(strptime("5/4/2016", "%m/%d/%Y"))

tw_GOP_chunks <- list(tw_GOP_1, tw_GOP_2, tw_GOP_3, tw_GOP_4, tw_GOP_5, tw_GOP_6)
fb_GOP_chunks <- list(fb_GOP_1, fb_GOP_2, fb_GOP_3, fb_GOP_4, fb_GOP_5, fb_GOP_6)
par(mar = c(0, 4, 2, 2))
par(mfrow = c(2,3))
for (p in 1:length(fb_GOP_chunks)) {
  dat <- fb_GOP_chunks[[p]]
  
  platform.text <- "Facebook"
  # dat <- dat.fb
  
  # platform.text <- "Twitter"
  # dat <- dat.tw
  
  dat$party <- "-"
  dat$party[dat$Candidate %in% repubs] <- "R"
  
  
  R.dat <- subset(dat, party == "R")
  R.cands <- names(table(R.dat$Candidate))
  
  ignore_domains <- c("bencarson.com", "berniesanders.com", "bobbyjindal.com", "carlyfiorina.com",
                      "chafee2016.com", "christiefornj.com", "donaldjtrump.com", "hillaryclinton.com", "jeb2016.com",
                      "johnkasich.com", "lindseygraham.com", "marcorubio.com", "martinomalley.com", "mikehuckabee.com",
                      "randpaul.com", "rickperry.org", "ricksantorum.com", "scottwalker.com", "tedcruz.org"
                      , "shortened", "content_hosting_analytics_shopping_crowdfunding")
  
  additional_domains_to_ignore <- c("chrischristie.com", "bencarsonbook.com", "candycarsonbook.com", "cruzcommander.com", "fixourborder.com", "goodtobeaclinton.com",
                                    "maketrumpdebateagain.com")
  
  Nov15_domains_to_ignore <- c("173.63.141.16:8005", "â\u0080¦", "t.â\u0080¦", "tâ\u0080¦", "t.câ\u0080¦", "whycarly.com",
                               "googletrends.github.io", "trends.google.com", "amazon.com", "google.com", "m.tmi.me",
                               "spark.adobe.com")
  
  ignore_domains <- c(ignore_domains, additional_domains_to_ignore, Nov15_domains_to_ignore)
  
  linked.list <- data.frame(from = R.dat$Candidate, to = R.dat$Domain, weight = R.dat$Count, stringsAsFactors = FALSE)
  
  zap <- which(linked.list$to %in% ignore_domains)
  linked.list <- linked.list[-zap, ]
  
  ##################
  # Descriptives
  #################
  
  cand_link_counts <- cbind(sort(table(linked.list$from)))
  
  #length(unique(linked.list$to))
  
  #linked.list$weight
  
  #table(table(linked.list$to))
  
  #linked.list$from[linked.list$to == "foxnews.com"]
  #linked.list$weight[linked.list$to == "foxnews.com"]
  
  #length(unique(linked.list$from))
  
  #table(linked.list$to)
  
  #cbind(sort(table(linked.list$to), decreasing = TRUE))
  
  # The next line gives the top 11 domains by the number of candidates who link to that domain
  #domain_cand_count <- sort(table(linked.list$to), decreasing = TRUE)[1:10]
  
  #tmp.dom.df <- aggregate(linked.list$weight, list(domain = linked.list$to), sum)
  #o <- order(tmp.dom.df$x, decreasing = TRUE)
  #domain_link_count <- tmp.dom.df[tmp.dom.df$domain %in% names(domain_cand_count), ]
  
  
  #######################
  #
  # Main plots
  #
  #######################
  g <- graph_from_data_frame(linked.list)
  
  g <- simplify(g, edge.attr.comb="sum")
  V(g)$type <- TRUE
  V(g)$type[V(g)$name %in% linked.list$to] <- FALSE
  V(g)$degree <- degree(g)
  V(g)$shape <- "square"
  V(g)$shape[V(g)$type] <- "circle"
  V(g)$size <- 0 # 5 + (10 * degree(g)/max(degree(g)))
  E(g)$weight <- .2
  V(g)$frame.color <- NA
  
  E(g)$color <- "gray"
  for (i in 1:22) {
    
    my.rgb <- col2rgb(repubs.cols$col[i])
    new.color <- rgb(my.rgb[1], my.rgb[2], my.rgb[3], alpha = 80, maxColorValue = 255)
    
    E(g)[.inc(V(g)$name == repubs.cols$repubs[i])]$color <- new.color 
  }
  E(g)[.to(V(g)$degree > 1)]$color <- rgb(0, 0, 0, .6)
  
  
  V(g)$label.color <- "black"
  for (i in 1:22) {
    # i <- 1
    index <- which(V(g)$name == repubs.cols$repubs[i])
    if (length(index) > 0) {
      my.neighbors <- neighbors(g, V(g)$name[index])$name
      V(g)$label.color[V(g)$name %in% my.neighbors] <- repubs.cols$col[i]
    }
  }
  V(g)$label.color[V(g)$degree > 1] <- "black"
  
  
  
  
  l <- layout_as_bipartite(g)
  V(g)$x <- l[,2]
  V(g)$y <- l[,1]
  
  V(g)$label.cex <- .3
  V(g)$label.cex[V(g)$type] <- 1
  plot.igraph(g, edge.arrow.size=0, edge.arrow.width=0, asp = 0)
  mtext(text = platform.text, side = 3, line = 0, adj = 0)
  
}

