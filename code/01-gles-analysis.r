# MEASURING POLITICAL KNOWLEDGE IN WEB-BASED SURVEYS
# SIMON MUNZERT, PETER SELB (UNIVERSITY OF KONSTANZ)
# DATA: GLES ONLINE TRACKING, T23 (ZA5723)
# AVAILABLE AT: http://info1.gesis.org/dbksearch19/SDESC2.asp?no=5723&db=d

rm(list=ls(all=TRUE))

#load packages
library(foreign)
library(ggplot2)
library(scales)
library(plyr)
library(dplyr)
library(xtable)
library(mokken)
library(car)
library(Deducer)
library(reshape2)
library(gmodels)
library(RColorBrewer)
library(stringr)
library(Hmisc)
library(memisc)
library(ordinal)

# import additional functions
source("summarySE.r")
source("setFactorOrder.r")

# import data
gles <- read.dta("data/ZA5723_v1-0-0.dta", convert.factors = TRUE)


### data manipulation -------------------------------------------
source("02-gles-prep.r")

  # use gles data with latency-based corrections for speeding
  source("03-gles-latency.r") 
  gles <- merge(gles, glesWide, by = "lfdn", all.y = T)
  gles <- gles[,as.logical(1-(str_detect(names(gles), "\\.x$")))]
  names(gles) <- str_replace_all(names(gles), "\\.y$", "")
  gles$ktotal_red <-  rowSums(glesWide[,c("k1", "k2", "k3", "k4", "k6", "k7", "k8")], na.rm = TRUE)




                   

### Mokken analysis -------------------------------------------

# how do the scales work that we can construct from each of the item sets? 
# is one of these scales superior?

# distribution of response patterns
returnPatternTable <- function(know) {
	print(order(colSums(know), decreasing = T))
	know <- know[,order(colSums(know), decreasing = T)]
	idealpatterns <- matrix( c(
		c(0,0,0,0,0,0,0,0),
		c(1,0,0,0,0,0,0,0),	
		c(1,1,0,0,0,0,0,0),
		c(1,1,1,0,0,0,0,0),
		c(1,1,1,1,0,0,0,0),
		c(1,1,1,1,1,0,0,0),
		c(1,1,1,1,1,1,0,0),
		c(1,1,1,1,1,1,1,0),
		c(1,1,1,1,1,1,1,1)),	
	  nrow = 9, byrow=T)

	patterns <- matrix(NA, ncol = 9, nrow = nrow(know))
	for (i in 1:nrow(know)) {
	patterns[i,1] <- identical(as.numeric(know[i,]), y = idealpatterns[1,])
	patterns[i,2] <- identical(as.numeric(know[i,]), y = idealpatterns[2,])
	patterns[i,3] <- identical(as.numeric(know[i,]), y = idealpatterns[3,])
	patterns[i,4] <- identical(as.numeric(know[i,]), y = idealpatterns[4,])
	patterns[i,5] <- identical(as.numeric(know[i,]), y = idealpatterns[5,])
	patterns[i,6] <- identical(as.numeric(know[i,]), y = idealpatterns[6,])
	patterns[i,7] <- identical(as.numeric(know[i,]), y = idealpatterns[7,])
	patterns[i,8] <- identical(as.numeric(know[i,]), y = idealpatterns[8,])
	patterns[i,9] <- identical(as.numeric(know[i,]), y = idealpatterns[9,])
	}

	ideal_pattern <- c(str_c(as.character(idealpatterns[1,]), collapse=""), str_c(as.character(idealpatterns[2,]), collapse=""), str_c(as.character(idealpatterns[3,]), collapse=""), str_c(as.character(idealpatterns[4,]), collapse=""), str_c(as.character(idealpatterns[5,]), collapse=""), str_c(as.character(idealpatterns[6,]), collapse=""), str_c(as.character(idealpatterns[7,]), collapse=""), str_c(as.character(idealpatterns[8,]), collapse=""), str_c(as.character(idealpatterns[9,]), collapse=""))
	pattern.table = data.frame(idealPattern = ideal_pattern, absFreq = apply(patterns, 2, sum), relFreq = apply(patterns, 2, sum)/nrow(patterns), score = 0:8)
	return(pattern.table)
}

# full sample
know <- gles[,names(gles) %in% c("k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8")]
(pattern.table <- returnPatternTable(know = know))
(absFreq.sum <- sum(pattern.table$absFreq))
(relFreq.sum <- sum(pattern.table$relFreq))

# verbal items
knowver <- gles[gles$treatment=="verbal",names(gles) %in% c("k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8")]
rownames(knowver) <- NULL
(pattern.table <- returnPatternTable(know = knowver))
(absFreq.sum <- sum(pattern.table$absFreq))
(relFreq.sum <- sum(pattern.table$relFreq))

# visual items
knowvis <- gles[gles$treatment=="visual",names(gles) %in% c("k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8")]
rownames(knowvis) <- NULL
(pattern.table <- returnPatternTable(know = knowvis))
(absFreq.sum <- sum(pattern.table$absFreq))
(relFreq.sum <- sum(pattern.table$relFreq))

# k1 "Pres GER"
# k2 "Pres USA"
# k3 "Pres RUS"
# k4 "Pres FRA"
# k5 "ForM GER"
# k6 "IntM GER"
# k7 "UN GS"
# k8 "FamM GER"

# scalability coefficients for both scales
coefH(knowver)
coefZ(knowver) # test statistics
coefH(knowvis)
coefZ(knowvis) # test statistics


# Guttman errors for each respondent
check.errors(knowver)

# test on latent monotonicity
Nknowver <- nrow(knowver)
Nknowvis <- nrow(knowvis)
monoKnowver <- check.monotonicity(knowver, minvi = .03, minsize = Nknowver/10)
monoKnowvis <- check.monotonicity(knowvis, minvi = .03, minsize = Nknowvis/10)
summary(monoKnowver)
summary(monoKnowvis)

# test on nonintersection with restscore method
interKnowver <- check.restscore(knowver)
interKnowvis <- check.restscore(knowvis)
summary(interKnowver)
summary(interKnowvis)

# bottom-up item selection
scaleKnowver <- search.normal(knowver)
scaleKnowvis <- search.normal(knowvis)

# recalculate scalability coefficients with dropped item GERForM
coefH(knowver[,c(1:4,6:8)])
coefH(knowvis[,c(1:4,6:8)])





### comparison with traditional measures of political knowledge --------------

# recode knowledge score
gles$ktotal_red <- as.factor(gles$ktotal_red)
gles$ktotal_rednum <- as.numeric(gles$ktotal_red)

# recode additional knowledge items
gles$k_votes <- ifelse(gles$t102 == "Zweitstimme", 1, 0)
gles$k_5perc <- ifelse(gles$t112 == "5 (*)", 1, 0)
gles$k_unemp <- ifelse(gles$t133 >= 2.8 & gles$t133 <= 3.2, 1, 0) # field period 21.02.2014 ? 07.03.2014, ~ 3 Mio. http://de.statista.com/statistik/daten/studie/1319/umfrage/aktuelle-arbeitslosenzahl-in-deutschland-monatsdurchschnittswerte/
table(gles$k_votes)
table(gles$k_5perc)
table(gles$k_unemp)

# build index variable
gles$k_alternative <- gles$k_votes + gles$k_5perc + gles$k_unemp # alternative knowledge index

# create response latency variables
gles$k_votes_rl <- gles$zt102
gles$k_5perc_rl <- gles$zt112
gles$k_unemp_rl <- gles$zt133
gles$rl_votes <- gles$k_votes_rl
gles$rl_5perc <- gles$k_5perc_rl
gles$rl_unemp <- gles$k_unemp_rl

# create index of variable names
knames_trad <- c("k_votes", "k_5perc", "k_unemp")
rlnames_trad <- c("rl_votes", "rl_5perc", "rl_unemp")
itemnames <- c("Second vote", "5% threshold", "Unemployment rate")


# bar chart of traditional knowledge items
pdf(file = "figures/correctSharesTradItems.pdf", width = 4.5, height = 4, family="URWTimes")
glesmelt <- melt(gles, measure.vars = knames_trad)
glesmelt.sum <- summarySE(glesmelt, measurevar = "value", groupvars = c("variable"))
levels(glesmelt.sum$variable) <- itemnames
glesmelt.sum$variable <- setFactorOrder(glesmelt.sum$variable, levels(glesmelt.sum$variable)[c(2,1,3)]) # resort items
gg <- ggplot(glesmelt.sum, aes(x = variable, y = value))
gg + geom_bar(stat="summary", position = "dodge", fun.y="mean", color="black", fill = rgb(.4,.4,.4)) + facet_grid(. ~variable, scale = "free_x") + geom_errorbar(aes(ymin=value-ci, ymax=value+ci), width=.2, position=position_dodge(.9), color="black")+ scale_x_discrete(breaks=NULL)  + scale_y_continuous(breaks = seq(0, 1, .1)) + ylab("Share of correct answers") + xlab(NULL)
dev.off()  

# bar chart of total scores
pdf(file = "figures/correctTotalsTradItems.pdf", width = 8, height = 6, family="URWTimes")
gg <- ggplot(gles, aes(factor(k_alternative)))
gg + geom_bar(aes(y = (..count..)*100/sum(..count..)), position = "dodge", color="black",  fill = rgb(.4,.4,.4)) + ylab("Percent") + xlab("Number of correct answers")
dev.off()

# correlations with visual scale
cor(gles$ktotal_rednum, gles$k_alternative, method = "spearman")
cor(gles$ktotal_rednum[gles$treatment=="visual"], gles$k_alternative[gles$treatment=="visual"], method = "spearman")
cor(gles$ktotal_rednum[gles$treatment=="verbal"], gles$k_alternative[gles$treatment=="verbal"], method = "spearman")



# get latencies alternative knowledge measures

# trim latencies to 5 minutes max
trimLatencies <- function(x) { ifelse(x > 300, 300, x)}
apply(gles[,rlnames_trad], 2, summary)
gles[,rlnames_trad] <- apply(gles[,rlnames_trad], 2, trimLatencies)
apply(gles[,rlnames_trad], 2, summary)

# boxplots of latencies, by correct/wrong answer
pdf(file = "figures/latencyTradItems.pdf", width = 10, height = 6, family="URWTimes")
glesmelt <- melt(gles, measure.vars = rlnames_trad)
glesmelt$answer <- NA
glesmelt$answer <- ifelse(glesmelt$variable=="rl_votes", glesmelt$k_votes, glesmelt$answer)
glesmelt$answer <- ifelse(glesmelt$variable=="rl_5perc", glesmelt$k_5perc, glesmelt$answer)
glesmelt$answer <- ifelse(glesmelt$variable=="rl_unemp", glesmelt$k_unemp, glesmelt$answer)
glesmelt$answer <- as.factor(ifelse(glesmelt$answer==1, "right", "wrong"))
levels(glesmelt$variable) <- itemnames
glesmelt$variable <- setFactorOrder(glesmelt$variable, levels(glesmelt$variable)[c(2, 1, 3)]) # resort items
gg <- ggplot(glesmelt, aes(x=answer, y = value)) + geom_boxplot(outlier.size=2, outlier.shape=19, outlier.colour= alpha("black", 1/2), fill=c(rgb(.6,.6,.6),rgb(.8,.8,.8))) + facet_grid(. ~ variable) + ylab("Response Latency (seconds)") + xlab(NULL) + scale_y_continuous(breaks = seq(0, 300, 30))
gg  
dev.off()  



# model response latency as a function of person and question effects
glesLong <- reshape(gles, idvar = "lfdn", varying = list(knames_trad, rlnames_trad), v.names=c("k", "rl"), direction = "long", timevar = "item")
glesLong$logrl <- log(glesLong$rl + 1) # log response latency
glesLong$invalidRL <- ifelse(glesLong$rl >= 300, 1, 0) # mark invalid response latencies
table(glesLong$invalidRL)

# compute model
summary(rlModel2 <- lme(logrl ~ k  + as.factor(item) + rltotal, random=~1|lfdn, data = glesLong)) # no invalidRL term for suspiciously large RLs

# extract residuals
glesLong$rlResids <- resid(rlModel2)

# examine suspicious observations
glesLong$susp <- glesLong$rlResids > quantile(glesLong$rlResids, .95)
table(glesLong$susp)

prop.table(table(glesLong$susp, glesLong$k))

summary(rlModelSusp <- glmer(susp ~ polinterest + (1|lfdn), family = "binomial", data = glesLong))

prop.table(table(glesLong$susp, glesLong$treatment))


# correct suspicious observations
length(glesLong$k[glesLong$rlResids > quantile(glesLong$rlResids, .98)])
glesLong$k[glesLong$rlResids > quantile(glesLong$rlResids, .98)] <- 0

# convert to wide format again
varnames2 <- c("lfdn", "k_alternative", "rltotal")
glesWide <- reshape(glesLong, idvar = varnames2, direction = "wide", timevar = "item", sep="")
glesWide$k_alternative <-  rowSums(glesWide[,knames], na.rm = TRUE) 





### assessment of criterion validity (see Clifford and Jerit, 2015, p.8) -----

# Pearson correlations
gles$polinterest <- recode(as.character(gles$t5), "'keine Angabe'=NA;'sehr stark'=5;'stark'=4;'mittelmaessig'=3;'weniger stark'=2;'ueberhaupt nicht'=1")
cor(gles$ktotal[gles$treatment=="visual"], gles$polinterest[gles$treatment=="visual"], use = "pairwise.complete.obs")
cor(gles$ktotal[gles$treatment=="verbal"], gles$polinterest[gles$treatment=="verbal"], use = "pairwise.complete.obs")
cor(gles$k_alternative, gles$polinterest, use = "pairwise.complete.obs")

# Kendall correlations
cor(gles$ktotal[gles$treatment=="visual"], gles$polinterest[gles$treatment=="visual"], use = "pairwise.complete.obs", method = "kendall")
cor(gles$ktotal[gles$treatment=="verbal"], gles$polinterest[gles$treatment=="verbal"], use = "pairwise.complete.obs", method = "kendall")
cor(gles$k_alternative, gles$polinterest, use = "pairwise.complete.obs", method = "kendall")

# Spearman correlations
cor(gles$ktotal[gles$treatment=="visual"], gles$polinterest[gles$treatment=="visual"], use = "pairwise.complete.obs", method = "spearman")
cor(gles$ktotal[gles$treatment=="verbal"], gles$polinterest[gles$treatment=="verbal"], use = "pairwise.complete.obs", method = "spearman")
cor(gles$k_alternative, gles$polinterest, use = "pairwise.complete.obs", method = "spearman")




### assessing determinants of verbal and visual political knowledge ----------

# recode additional variables
gles$tvinfo <- ifelse(gles$t36 == "Fernsehen", 1, 0)
gles$inetinfo <- ifelse(gles$t36 == "Internet", 1, 0)
gles$radioprintinfo <- ifelse(gles$t36 == "Radio" | gles$t36 == "Zeitung", 1, 0)

gles$inetuse <- ifelse(gles$t40 != "keine Angabe" & gles$t40 != "gar nicht", 1, 0)
gles$inc_cat <- recode.variables(as.numeric(gles$t70) , "1 -> NA;2:6 -> 'below 1000 EUR';7:9 -> 'between 1000 and 1999 EUR';10:11 -> 'between 2000 and 2999 EUR'; 12:16 -> '3000 EUR and more';")[,1]
gles$inc_cat <- factor(gles$inc_cat, levels = c("below 1000 EUR", "between 1000 and 1999 EUR", "between 2000 and 2999 EUR", "3000 EUR and more"))
gles$edu <- factor(gles$edu, levels = c("low", "mid", "high"))

# recode knowledge score
gles$ktotal_red <- as.factor(gles$ktotal_red)
gles$ktotal_rednum <- as.numeric(gles$ktotal_red)
gles$k_alternative_red <- as.factor(gles$k_alternative)

# fit ordinal probit model on knowledge
summary(oprob <- clm(ktotal_red ~ treatment*male + treatment*edu + treatment*age_cat + treatment*inc_cat +  treatment*tvinfo + treatment*polinterest, data = gles, link = "probit"))

# examine fit
fit <- unlist(predict(oprob, type = "class"))
table(fit)
table(fit, gles$ktotal_red[-as.numeric(oprob$na.action)])
prop.table(table(fit==gles$ktotal_red[-as.numeric(oprob$na.action)]))

