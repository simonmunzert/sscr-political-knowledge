# MEASURING POLITICAL KNOWLEDGE IN WEB-BASED SURVEYS
# SIMON MUNZERT, PETER SELB (UNIVERSITY OF KONSTANZ)
# DATA: GLES ONLINE TRACKING, T23 (ZA5723)
# AVAILABLE AT: http://info1.gesis.org/dbksearch19/SDESC2.asp?no=5723&db=d


### Response latency analysis -------------------------------------------

library(lme4)
library(nlme) # to account for heteroskedasticity

# response latencies 
rlnames <- c("rl1", "rl2", "rl3", "rl4", "rl5", "rl6", "rl7", "rl8")
gles$rl0 <- rowSums(cbind(gles$zt277, gles$zt289), na.rm = TRUE)
gles$rl1 <- rowSums(cbind(gles$zt278, gles$zt290), na.rm = TRUE)
gles$rl2 <- rowSums(cbind(gles$zt279, gles$zt291), na.rm = TRUE)
gles$rl3 <- rowSums(cbind(gles$zt280, gles$zt292), na.rm = TRUE)
gles$rl4 <- rowSums(cbind(gles$zt281, gles$zt293), na.rm = TRUE)
gles$rl5 <- rowSums(cbind(gles$zt282, gles$zt294), na.rm = TRUE)
gles$rl6 <- rowSums(cbind(gles$zt283, gles$zt295), na.rm = TRUE)
gles$rl7 <- rowSums(cbind(gles$zt284, gles$zt296), na.rm = TRUE)
gles$rl8 <- rowSums(cbind(gles$zt285, gles$zt297), na.rm = TRUE)

# trim latencies to 3 minutes max
trimLatencies <- function(x) { ifelse(x > 300, 300, x)}
apply(gles[,rlnames], 2, summary)
gles[,rlnames] <- apply(gles[,rlnames], 2, trimLatencies)
apply(gles[,rlnames], 2, summary)

# overall response latency
gles$rltotal <- rowSums(gles[,rlnames], na.rm = TRUE)

# speeders are excluded from analysis, because they give no substantive answers. it is inappropriate to only code them "wrong answer" for two reasons. first, we cannot know if the person actually did not know the correct solution or just refused to solve the task properly. second, if speeders are coded wrong but stay part of the sample, the homogeneity of the scale is artificially increased, because the pattern "all answers wrong" is consistent with the battery (and not automatically excluded from analysis as in parametric Rasch scaling).

# generate speeder flag variable on knowledge items
gles$knspeeder <- gles$rltotal < quantile(gles$rltotal, .05)
table(gles$ktotal, gles$knspeeder) # too many who got a substantive amount of answers right
gles$knspeeder <- gles$rltotal < quantile(gles$rltotal, .02)
table(gles$ktotal, gles$knspeeder)
gles[gles$ktotal>2 & gles$knspeeder==1,rlnames]

# exclude speeders
table(gles$knspeeder)
gles <- gles[gles$knspeeder!=1,]

# prepare long data format for cheating detection
gles$polinterest <- recode(as.character(gles$t5), "'keine Angabe'=NA;'sehr stark'=5;'stark'=4;'mittelmaessig'=3;'weniger stark'=2;'ueberhaupt nicht'=1")
gles$polinterest[is.na(gles$polinterest)] <- 1


varnames <- c("lfdn", knames, rlnames, "treatment", "ktotal", "rltotal", "polinterest", "age_cat", "edu")
glesRed <- gles[,varnames]



# model response latency as a function of person and question effects
glesLong <- reshape(glesRed, idvar = "lfdn", varying = list(knames, rlnames), v.names=c("k", "rl"), direction = "long", timevar = "item")
glesLong$logrl <- log(glesLong$rl + 1) # log response latency
glesLong$invalidRL <- ifelse(glesLong$rl >= 300, 1, 0) # mark invalid response latencies
table(glesLong$invalidRL)

summary(rlModel2 <- lme(logrl ~ treatment + k  + as.factor(item) + rltotal, random=~1|lfdn, weights = varIdent(form = ~ 1|treatment), data = glesLong)) 

# extract residuals
glesLong$rlResids <- resid(rlModel2)

# examine suspicious observations
glesLong$susp <- glesLong$rlResids > quantile(glesLong$rlResids, .98)
table(glesLong$susp)
summary(rlModelSusp <- glmer(susp ~ treatment + polinterest + age_cat + edu + (1|lfdn), family = "binomial", data = glesLong))

prop.table(table(glesLong$susp, glesLong$treatment))


# correct suspicious observations
length(glesLong$k[glesLong$rlResids > quantile(glesLong$rlResids, .98)])
glesLong$k[glesLong$rlResids > quantile(glesLong$rlResids, .98)] <- 0

# convert to wide format again
varnames2 <- c("lfdn", "treatment", "ktotal", "rltotal")
glesWide <- reshape(glesLong, idvar = varnames2, direction = "wide", timevar = "item", sep="")
glesWide$ktotal <-  rowSums(glesWide[,knames], na.rm = TRUE) # correct ktotal statistic


