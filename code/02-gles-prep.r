# MEASURING POLITICAL KNOWLEDGE IN WEB-BASED SURVEYS
# SIMON MUNZERT, PETER SELB (UNIVERSITY OF KONSTANZ)
# DATA: GLES ONLINE TRACKING, T23 (ZA5723)
# AVAILABLE AT: http://info1.gesis.org/dbksearch19/SDESC2.asp?no=5723&db=d


### data preparation -------------------------------------------

# treatment group
gles$treatment <- ifelse(gles$split1 == "Gruppe 1", "visual", "verbal")

# respondent characteristics
gles$age <- 2014 - gles$t2
gles$age_cat <- recode(gles$age, "18:29='18-29';30:44='30-44';45:59='45-59';60:100='60+'")
gles$male <- ifelse(gles$t1 == "maennlich", "male", "female")
gles$education <- as.numeric(gles$t3) 
gles$edu <- recode.variables(gles$education , "3:6 -> 'low';7:8 -> 'mid';11 -> 'low';else -> 'high';")[,1]
gles$sphone <-  ifelse(gles$smartphone == "ja", "yes", "no")

# knowledge items (experiment)
itemnames <- c("Pres GER", "Pres USA", "Pres RUS", "Pres FRA", "ForM GER", "IntM GER", "UN GS", "FamM GER")
knames <- c("k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8")

knowledgeAnswer <- function(dat) {
dat$xnum <- ifelse(dat$x == "richtig", 1, ifelse(dat$x == "falsch", 0, NA))
dat$ynum <- ifelse(dat$y == "richtig", 1, ifelse(dat$y == "falsch", 0, NA))
dat$answer <- rowSums(cbind(dat$xnum, dat$ynum), na.rm=T)
return(dat$answer)
}

gles$k0 <- knowledgeAnswer(data.frame(x = gles$t277b, y = gles$t289b)) #"Chancellor GER, Imgs"
gles$k1 <- knowledgeAnswer(data.frame(x = gles$t278b, y = gles$t290b)) #"President GER, Imgs"
gles$k2 <- knowledgeAnswer(data.frame(x = gles$t279b, y = gles$t291b)) #"President US, Imgs"
gles$k3 <- knowledgeAnswer(data.frame(x = gles$t280b, y = gles$t292b)) #"President RUS, Imgs"
gles$k4 <- knowledgeAnswer(data.frame(x = gles$t281b, y = gles$t293b)) #"President FRA, Imgs"
gles$k5 <- knowledgeAnswer(data.frame(x = gles$t282b, y = gles$t294b)) #"Foreign Minister GER, Imgs"
gles$k6 <- knowledgeAnswer(data.frame(x = gles$t283b, y = gles$t295b)) #"Interior Minister GER, Imgs"
gles$k7 <- knowledgeAnswer(data.frame(x = gles$t284b, y = gles$t296b)) #"UN General Secretary, Imgs"
gles$k8 <- knowledgeAnswer(data.frame(x = gles$t285b, y = gles$t297b)) #"Family Minister GER, Imgs"

# overall score
gles$ktotal <-  rowSums(gles[,knames], na.rm = TRUE)
by(gles$ktotal, gles$treatment, summary)

