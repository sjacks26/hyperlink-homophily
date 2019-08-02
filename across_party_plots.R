####################################################################################
#
# Project: Echo Chambers
# Auths: Sam, Jeff
# Date(ish): Dec 2017 -- UPDATED AUG 2019
#
# This is the sixth script in our data process. It is the primary analysis script
# for across-party analysis.
#
# Script does some data cleaning and creates across party network plots.
# Makes the bipartite by party 3 column plot in red, blue and purple
#  - switch from FB to TW by toggling what gets loaded into dat
#
####################################################################################

library(readr)
library(igraph)

twit.dems <- c("BernieSanders", "SenSanders", "GovernorOMalley", "MartinOMalley", "HillaryClinton", "JimWebbUSA", "LincolnChafee") # "JoeBiden", "lessig"
twit.reps <- c("BobbyJindal", "Gov. Bobby Jindal", "CarlyFiorina", "carlyforamerica", "ChrisChristie", "GovChristie", "GovernorPataki",
               "GovernorPerry", "gov_gilmore", "GovMikeHuckabee", "GovWalker", "GrahamBlog", "JebBush", "JohnKasich", "LindseyGrahamSC", 
               "marcorubio", "MarkForAmerica", "RandPaul", "RealBenCarson", "realDonaldTrump", "RickSantorum", "ScottWalker", "SenTedCruz", 
               "TeamMarco ", "TeamRickPerry", "tedcruz", "CARLY for America", "Gov. Mike Huckabee", "Senator Rand Paul", "Ben Carson", "George Pataki", "Donald Trump")

repubs <- c("Ted Cruz", "Rick Santorum", "Scott Walker", "Mike Huckabee", "Carly Fiorina",
            "Marco Rubio", "Rand Paul", "John Kasich", "Chris Christie", "Dr. Ben Carson",
            "George E. Pataki", "Rick Perry", "Donald J. Trump", "Jeb Bush", "Lindsey Graham",
            "Bobby Jindal", "Jim Gilmore", twit.reps)

dems <- c("Hillary Clinton", "Bernie Sanders", "Martin O'Malley", "Jim Webb", "Lincoln Chafee", twit.dems)

dat.fb <- read.csv("./FB_main_20180103.csv", quote = "\"", sep = "," , header = TRUE, stringsAsFactors = FALSE)
# dat.tw <- read.csv("/home/rstudio/urls_network_analysis/data/dyads/TW_main_Dec18.csv", quote = "\"", sep = "," , header = TRUE, stringsAsFactors = FALSE)
new.tw <- read.csv("./TW_main_20180103.csv", quote = "\"", sep = "," , header = TRUE, stringsAsFactors = FALSE)



do.cut.off <- FALSE
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
  dat$party[dat$Candidate %in% dems] <- "D"
  dat$party[dat$Candidate %in% repubs] <- "R"

  # dat <- subset(dat, Count > 1)
  
  # Count needs to be abs value if we want a non-bipartite network
  dat$Count[dat$party == "R"] <- (-1 * dat$Count[dat$party == "R"])
  
  # dat2 removes the candidates' own  websites and other non-useful domains.
  ignore_domains <- c("bencarson.com", "berniesanders.com", "bobbyjindal.com", "carlyfiorina.com", "content_hosting_analytics_shopping_crowdfunding", "shortened",
                      "chafee2016.com", "christiefornj.com", "donaldjtrump.com", "hillaryclinton.com", "jeb2016.com", "georgepataki.com", 
                      "johnkasich.com", "lindseygraham.com", "marcorubio.com", "martinomalley.com", "mikehuckabee.com", "mikehuckabeee.com", 
                      "randpaul.com", "rickperry.org", "ricksantorum.com", "scottwalker.com", "tedcruz.org", "jameswebb.com", "junk") # "webb2016.com", "citizencarly.com", "cruzcrowd.com"
  additional_domains_to_ignore <- c("chrischristie.com", "bencarsonbook.com", "candycarsonbook.com", "cruzcommander.com", "fixourborder.com", "goodtobeaclinton.com",
                                    "maketrumpdebateagain.com")
  Nov15_domains_to_ignore <- c("173.63.141.16:8005", "â\u0080¦", "t.â\u0080¦", "tâ\u0080¦", "t.câ\u0080¦", "whycarly.com",
                               "googletrends.github.io", "trends.google.com", "amazon.com", "google.com", "m.tmi.me",
                               "spark.adobe.com")
  Dec13_to_ignore <- c("will.i.am", "twcamp^serp", "twgr^tweet", "tiny.cc", "surveymonkey.com", "")
  
  ignore_domains <- c(ignore_domains, additional_domains_to_ignore, Nov15_domains_to_ignore, Dec13_to_ignore)
  
  zap2 <- which(dat$Domain %in% ignore_domains)
  
  if (length(zap2) > 0) {
    dat2 <- dat[-zap2, ]
  } else {
    dat2 <- dat
  }
  
  # dat2 <- subset(dat2, abs(Count) > 1)
  if (exists("df", inherits = FALSE)) { rm(df) }
  df <- data.frame(tapply(dat2$Count, list(dat2$Domain, dat2$party), sum))
  
  # do cut off
  if (do.cut.off & (platform.text == "Twitter")) {
    # ok, so we might want to use some sort of cut off to make the visual artifiact more useable as an 
    # analytic devices. First, lets think about if we should use the same or different 
    # cuttoffs for the different parties. 
    # link ratio:
    sum(df$D, na.rm = TRUE)/sum(df$R, na.rm = TRUE) # about 5 to 1
    #candidate ratio:
    5/17 # about 3 to 1
    
    # here is where we do the cutoff
    zap1 <- which(df$D == 1 | is.na(df$D))
    zap2 <- which(abs(df$R) < 3 | is.na(df$R))
    zap3 <- intersect(zap1, zap2)
    df <- tmp1 <- df[-zap3,]
    #View(df)
    # tmp2 <- subset(df, (D + abs(R) > 1), ) # This is another attempt to do the subsetting I do in the previous couple of lines. It throws out all rows with an NA in D or R.
  }
  
  df$color <- ""
  df$color[df$D != "NA"] <- "blue"
  df$color[df$R != "NA"] <- "red"
  df$color[df$D != "NA" & df$R != "NA"] <- 'purple'
  
  df$d.lwd <- round(.2 + (3 * df$D/max(df$D, na.rm = TRUE)), 1)
  #plot(df$d.lwd)
  df$r.lwd <- round(.2 + (3 * abs(df$R)/max(abs(df$R), na.rm = TRUE)), 1)
  #plot(df$r.lwd)
  
  
  
  
  D.tmp <- df$D
  R.tmp <- df$R
  D.tmp[is.na(D.tmp)] <- .00001
  R.tmp[is.na(R.tmp)] <- .00001
  
  # Smaller values mean a domain is more GOP
  df$link_diff <- D.tmp/ abs(R.tmp)
  
  df1 <- df[order(-df$link_diff),]
  
  #View(df1)
  
  if (platform.text == "Facebook") { par(mfrow = c(1,2)) }
  
  par(mar = c(0,0,0,0))
  
  plot(c(-1,1), c(1,dim(df1)[1]), type = "n", xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")
  text(x = rep(0, dim(df1)[1]), y = 1:dim(df1)[1], labels = rownames(df1), cex = .5, col = df1$color)
  
  text(x = -1, y = dim(df1)[1]/2, labels = "Democrats", adj = 0)
  for (i in 1:length(df1$D)) {
    if (!is.na(df1$D[i])) {
      lines(x = c(-.9, -.05), y = c(dim(df1)[1]/2, i), col = "blue", lwd = df1$d.lwd[i])
    }
  }
  
  text(x = 1, y = dim(df1)[1]/2, labels = "Republicans", adj = 1)
  for (i in 1:length(df1$R)) {
    if (!is.na(df1$R[i])) {
      lines(x = c(.88, .05), y = c(dim(df1)[1]/2, i), col = "red", lwd = df1$r.lwd[i])
    }
  }
  
  
  mtext(text = platform.text, side = 3, line = -2, adj = 0.2)

  domains_by_party <- table(df$color)
  
  rep_domains <- subset(df[df$color == "red", ])
  dem_domains <- subset(df[df$color == "blue", ])
  dualies <- subset(df[df$color == "purple", ])
  dualies$combined <- dualies$D + abs(dualies$R)
  
  if (platform.text == "Twitter") {
    TW.df <- df1
    TW.agg.data <- table(df1$color)
    TW.link.totals <- data.frame(dems = sum(df1$D[df1$color == "blue"]), cross = sum(df1$D[df1$color == "purple"]) + abs(sum(df1$R[df1$color == "purple"])), reps = abs(sum(df1$R[df1$color == "red"])))
    tw_dems <- dem_domains
    tw_reps <- rep_domains
    tw_duals <- dualies
    tw_kendall_cor <- cor.test(R.tmp, D.tmp, method = "kendall", use = "pairwise")
    
  } else {
    FB.df <- df1
    FB.agg.data <- table(df1$color)
    FB.link.totals <- data.frame(dems = sum(df1$D[df1$color == "blue"]), cross = sum(df1$D[df1$color == "purple"]) + abs(sum(df1$R[df1$color == "purple"])), reps = abs(sum(df1$R[df1$color == "red"])))
    fb_dems <- dem_domains
    fb_reps <- rep_domains
    fb_duals <- dualies
    fb_kendall_cor <- cor.test(R.tmp, D.tmp, method = "kendall", use = "pairwise")
  }
}



bar.plot.dat <- data.frame(color = names(TW.agg.data), Facebook = as.numeric(FB.agg.data), Twitter = as.numeric(TW.agg.data), stringsAsFactors = FALSE)
par(mfrow = c(1,2), mar = c(2,3,2,1))
barplot(as.matrix(bar.plot.dat[,2:3]), beside = TRUE, col = bar.plot.dat[,1], border = "white", las = 1, density = 40, angle = c(30,-45,60)
        , legend.text = c("Democrat", "Both", "Republican")
        , args.legend=list(x = "topleft", bty = "n", border = NA))
mtext(text = "a)", side = 3, line = .5, adj = 0, cex = .8)

df.link.totals <- FB.link.totals
df.link.totals[2,] <- TW.link.totals
row.names(df.link.totals) <- c("Facebook", "Twitter")

barplot(t(as.matrix(df.link.totals)), beside = TRUE, col = bar.plot.dat[,1], border = "white", las = 1, density = 40, angle = c(30,-45,60)
        , legend.text = c("Democrat", "Both", "Republican")
        , args.legend=list(x = "topleft", bty = "n", border = NA))
mtext(text = "b)", side = 3, line = .5, adj = 0, cex = .8)

stop()

####################################################################################
#
# The next section performs statistical tests. We report kendall, not spearman
#
####################################################################################

R_D <- cbind(abs(R.tmp), D.tmp)
#tw_kendall_cor <- cor.test(R.tmp, D.tmp, method = "kendall", use = "pairwise")
#tw_spearman_cor <- cor.test(R.tmp, D.tmp, method = "spearm", use = "pairwise")
fb_kendall_cor <- cor.test(R.tmp, D.tmp, method = "kendall", use = "pairwise")
#fb_spearman_cor <- cor.test(R.tmp, D.tmp, method = "spearm", use = "pairwise")

####################################################################################
#
# The next section generates descriptive statistics
#
####################################################################################

domains_by_party <- table(df$color)

reps <- subset(df[df$color == "red", ])
dems <- subset(df[df$color == "blue", ])
dualies <- subset(df[df$color == "purple", ])
dualies$combined <- dualies$D + abs(dualies$R)
View(dems)

# tw_dems <- dems
# tw_reps <- reps
# tw_duals <- dualies
fb_dems <- dems
fb_reps <- reps
fb_duals <- dualies

rownames(tw_dems)[which(rownames(tw_dems) %in% rownames(fb_dems))]
rownames(tw_dems)[-which(rownames(tw_dems) %in% rownames(fb_dems))]
rownames(fb_dems)[-which(rownames(fb_dems) %in% rownames(tw_dems))]

rownames(tw_reps)[which(rownames(tw_reps) %in% rownames(fb_reps))]
rownames(tw_reps)[-which(rownames(tw_reps) %in% rownames(fb_reps))]
rownames(fb_reps)[-which(rownames(fb_reps) %in% rownames(tw_reps))]

# The next two rows generate the mean number of sites that candidates from each party link to
mean(table(dat2$Candidate[dat2$party == "D"]))
mean(table(dat2$Candidate[dat2$party == "R"]))

####################################################################################
#
# end
#
####################################################################################


