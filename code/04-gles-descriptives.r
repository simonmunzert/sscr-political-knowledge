# MEASURING POLITICAL KNOWLEDGE IN WEB-BASED SURVEYS
# SIMON MUNZERT, PETER SELB (UNIVERSITY OF KONSTANZ)
# DATA: GLES ONLINE TRACKING, T23 (ZA5723)
# AVAILABLE AT: http://info1.gesis.org/dbksearch19/SDESC2.asp?no=5723&db=d

#load packages
library(foreign)
library(ggplot2)
library(scales)
library(dplyr)
library(xtable)
library(mokken)
library(car)
library(Deducer)
library(reshape2)
library(gmodels)
library(RColorBrewer)
library(stringr)
library(plyr)
library(Hmisc)

# import additional functions
source("summarySE.r")
source("setFactorOrder.r")

# import data
gles <- read.dta("data/ZA5723_v1-0-0.dta", convert.factors = TRUE)


### data manipulation -------------------------------------------
source("gles-prep.r")
source("gles-latency.r") # use gles data with latency-based corrections for speeding
gles <- glesWide # use gles data with latency-based corrections for cheating


### descriptive statistics -------------------------------------------

### comparison of visual and verbal group on a set of items (used for summary table to demonstrate that the samples are balanced on some observables)

## respondent characteristics 
# sex
sex.tab <- with(gles, CrossTable(male, treatment, digits=3, prop.r=F, prop.c=T, prop.t=F, prop.chisq=F, chisq=T, format="SAS"))

# age
age.tab <- with(gles, CrossTable(age_cat, treatment, digits=3, prop.r=F, prop.c=T, prop.t=F, prop.chisq=F, chisq=T, format="SAS"))

# education
edu.tab <- with(gles, CrossTable(edu, treatment, digits=3, prop.r=F, prop.c=T, prop.t=F, prop.chisq=F, chisq=T, format="SAS"))

# smartphone usage
sphone.tab <- with(gles, CrossTable(sphone, treatment, digits=3, prop.r=F, prop.c=T, prop.t=F, prop.chisq=F, chisq=T, format="SAS"))

## performance on knowledge items, by treatment
with(gles, CrossTable(k0, treatment, digits=3, prop.r=F, prop.c=T, prop.t=F, prop.chisq=F, chisq=T, format="SAS"))
with(gles, CrossTable(k1, treatment, digits=3, prop.r=F, prop.c=T, prop.t=F, prop.chisq=F, chisq=T, format="SAS"))
with(gles, CrossTable(k2, treatment, digits=3, prop.r=F, prop.c=T, prop.t=F, prop.chisq=F, chisq=T, format="SAS"))
with(gles, CrossTable(k3, treatment, digits=3, prop.r=F, prop.c=T, prop.t=F, prop.chisq=F, chisq=T, format="SAS"))
with(gles, CrossTable(k4, treatment, digits=3, prop.r=F, prop.c=T, prop.t=F, prop.chisq=F, chisq=T, format="SAS"))
with(gles, CrossTable(k5, treatment, digits=3, prop.r=F, prop.c=T, prop.t=F, prop.chisq=F, chisq=T, format="SAS"))
with(gles, CrossTable(k6, treatment, digits=3, prop.r=F, prop.c=T, prop.t=F, prop.chisq=F, chisq=T, format="SAS"))
with(gles, CrossTable(k7, treatment, digits=3, prop.r=F, prop.c=T, prop.t=F, prop.chisq=F, chisq=T, format="SAS"))
with(gles, CrossTable(k8, treatment, digits=3, prop.r=F, prop.c=T, prop.t=F, prop.chisq=F, chisq=T, format="SAS"))

# overall knowledge level
by(gles$ktotal, gles$treatment, summary)


### which answer combinations are most prevalent?

# function to figure out which combinations were chosen on the knowledge items
whichCombinations <- function(var1, var2, var3, var4) {
combination <- ifelse(var1 == "genannt" & var2 == "genannt", "1and2",
				ifelse(var1 == "genannt" & var3 == "genannt", "1and3",
				ifelse(var1 == "genannt" & var4 == "genannt", "1and4",
				ifelse(var2 == "genannt" & var3 == "genannt", "2and3",				
				ifelse(var2 == "genannt" & var4 == "genannt", "2and4",				
				ifelse(var3 == "genannt" & var4 == "genannt", "3and4",	"Split/DK/NA")
			   )))))
return(combination)
}
# Westerwelle, Lindner, Friedrich, Steinmeier
table(with(gles, whichCombinations(t282aa, t282ab, t282ac, t282ad))) # ForM GER, visual
table(with(gles, whichCombinations(t294aa, t294ab, t294ac, t294ad))) # ForM GER, verbal

# Medwedew, Lawrow, Janukowytsch, Putin
table(with(gles, whichCombinations(t280aa, t280ab, t280ac, t280ad))) # Pres RUS, visual
table(with(gles, whichCombinations(t292aa, t292ab, t292ac, t292ad))) # Pres RUS, verbal



### graphics -------------------------------------------

# bar charts of knowledge items
pdf(file = "figures/correctShares.pdf", width = 10, height = 6, family="URWTimes")
glesmelt <- melt(gles, measure.vars = knames)
glesmelt.sum <- summarySE(glesmelt, measurevar = "value", groupvars = c("variable", "treatment"))
levels(glesmelt.sum$variable) <- itemnames
glesmelt.sum$variable <- setFactorOrder(glesmelt.sum$variable, levels(glesmelt.sum$variable)[c(2,1,4,3,5,7,6,8)]) # resort items
gg <- ggplot(glesmelt.sum, aes(x = treatment, y = value))
gg + geom_bar(stat="summary", position = "dodge", fun.y="mean", fill=c(rgb(.6,.6,.6),rgb(.8,.8,.8)), color="black") + 
geom_errorbar(aes(ymin=value-ci, ymax=value+ci), width=.2, position=position_dodge(.9), color="black") + facet_grid(. ~variable) + scale_y_continuous(breaks = seq(0, 1, .1)) + ylab("Share of correct answers") + xlab(NULL)
dev.off()  
 
# boxplots of latencies
pdf(file = "figures/latencies.pdf", width = 10, height = 6, family="URWTimes")
glesmelt <- melt(gles, measure.vars = rlnames)  
levels(glesmelt$variable) <- itemnames
glesmelt$variable <- setFactorOrder(glesmelt$variable, levels(glesmelt$variable)[c(2,1,4,3,5,7,6,8)]) # resort items
gg <- ggplot(glesmelt, aes(x=treatment, y = value)) + geom_boxplot(outlier.size=2, outlier.shape=19, outlier.colour= alpha("black", 1/2), fill=c(rgb(.6,.6,.6),rgb(.8,.8,.8))) + facet_grid(. ~variable) + ylab("Response Latency (seconds)") + xlab(NULL) + scale_y_continuous(breaks = seq(0, 300, 30))
gg  
dev.off()  

# overall sum of latencies
pdf(file = "figures/latencyOverall.pdf", width = 6, height = 6, family="URWTimes")
gg <- ggplot(gles, aes(treatment, rltotal))
gg + geom_boxplot(outlier.size=2, outlier.shape=19, outlier.colour= alpha("black", 1/2)) + stat_boxplot(geom ='errorbar') + xlab("Treatment") + ylab("Response latency, total (seconds)")  + ylim(0, 1200)
dev.off()

# boxplots of latencies, by correct/wrong answer
pdf(file = "figures/latencyVerbal.pdf", width = 10, height = 6, family="URWTimes")
glesmelt <- melt(gles, measure.vars = rlnames)
glesmelt$answer <- NA
glesmelt$answer <- ifelse(glesmelt$variable=="rl1", glesmelt$k1, glesmelt$answer)
glesmelt$answer <- ifelse(glesmelt$variable=="rl2", glesmelt$k2, glesmelt$answer)
glesmelt$answer <- ifelse(glesmelt$variable=="rl3", glesmelt$k3, glesmelt$answer)
glesmelt$answer <- ifelse(glesmelt$variable=="rl4", glesmelt$k4, glesmelt$answer)
glesmelt$answer <- ifelse(glesmelt$variable=="rl5", glesmelt$k5, glesmelt$answer)
glesmelt$answer <- ifelse(glesmelt$variable=="rl6", glesmelt$k6, glesmelt$answer)
glesmelt$answer <- ifelse(glesmelt$variable=="rl7", glesmelt$k7, glesmelt$answer)
glesmelt$answer <- ifelse(glesmelt$variable=="rl8", glesmelt$k8, glesmelt$answer)
glesmelt$answer <- as.factor(ifelse(glesmelt$answer==1, "right", "wrong"))
levels(glesmelt$variable) <- itemnames
glesmelt <- glesmelt[glesmelt$treatment=="verbal",]
glesmelt$variable <- setFactorOrder(glesmelt$variable, levels(glesmelt$variable)[c(2,1,4,3,5,7,6,8)]) # resort items
gg <- ggplot(glesmelt, aes(x=answer, y = value)) + geom_boxplot(outlier.size=2, outlier.shape=19, outlier.colour= alpha("black", 1/2), fill=c(rgb(.6,.6,.6),rgb(.8,.8,.8))) + facet_grid(. ~ variable) + ylab("Response Latency (seconds)") + xlab(NULL) + scale_y_continuous(breaks = seq(0, 300, 30))
gg  
dev.off()  

pdf(file = "figures/latencyVisual.pdf", width = 10, height = 6, family="URWTimes")
glesmelt <- melt(gles, measure.vars = rlnames)
glesmelt$answer <- NA
glesmelt$answer <- ifelse(glesmelt$variable=="rl1", glesmelt$k1, glesmelt$answer)
glesmelt$answer <- ifelse(glesmelt$variable=="rl2", glesmelt$k2, glesmelt$answer)
glesmelt$answer <- ifelse(glesmelt$variable=="rl3", glesmelt$k3, glesmelt$answer)
glesmelt$answer <- ifelse(glesmelt$variable=="rl4", glesmelt$k4, glesmelt$answer)
glesmelt$answer <- ifelse(glesmelt$variable=="rl5", glesmelt$k5, glesmelt$answer)
glesmelt$answer <- ifelse(glesmelt$variable=="rl6", glesmelt$k6, glesmelt$answer)
glesmelt$answer <- ifelse(glesmelt$variable=="rl7", glesmelt$k7, glesmelt$answer)
glesmelt$answer <- ifelse(glesmelt$variable=="rl8", glesmelt$k8, glesmelt$answer)
glesmelt$answer <- as.factor(ifelse(glesmelt$answer==1, "right", "wrong"))
levels(glesmelt$variable) <- itemnames
glesmelt <- glesmelt[glesmelt$treatment=="visual",]
glesmelt$variable <- setFactorOrder(glesmelt$variable, levels(glesmelt$variable)[c(2,1,4,3,5,7,6,8)]) # resort items
gg <- ggplot(glesmelt, aes(x=answer, y = value)) + geom_boxplot(outlier.size=2, outlier.shape=19, outlier.colour= alpha("black", 1/2), fill=c(rgb(.6,.6,.6),rgb(.8,.8,.8))) + facet_grid(. ~ variable) + ylab("Response Latency (seconds)") + xlab(NULL) + scale_y_continuous(breaks = seq(0, 300, 30))
gg  
dev.off()  


# boxplots of latencies, by treatment
pdf(file = "figures/itemWrongLatencyBoxplots.pdf", width = 10, height = 6, family="URWTimes")
glesmelt <- melt(gles, measure.vars = rlnames)
glesmelt$answer <- NA
glesmelt$answer <- ifelse(glesmelt$variable=="rl1", glesmelt$k1, glesmelt$answer)
glesmelt$answer <- ifelse(glesmelt$variable=="rl2", glesmelt$k2, glesmelt$answer)
glesmelt$answer <- ifelse(glesmelt$variable=="rl3", glesmelt$k3, glesmelt$answer)
glesmelt$answer <- ifelse(glesmelt$variable=="rl4", glesmelt$k4, glesmelt$answer)
glesmelt$answer <- ifelse(glesmelt$variable=="rl5", glesmelt$k5, glesmelt$answer)
glesmelt$answer <- ifelse(glesmelt$variable=="rl6", glesmelt$k6, glesmelt$answer)
glesmelt$answer <- ifelse(glesmelt$variable=="rl7", glesmelt$k7, glesmelt$answer)
glesmelt$answer <- ifelse(glesmelt$variable=="rl8", glesmelt$k8, glesmelt$answer)
glesmelt$answer <- as.factor(ifelse(glesmelt$answer==1, "right", "wrong"))
levels(glesmelt$variable) <- itemnames
glesmelt <- glesmelt[glesmelt$answer=="wrong",]
glesmelt$variable <- setFactorOrder(glesmelt$variable, levels(glesmelt$variable)[c(2,1,4,3,5,7,6,8)]) # resort items
gg <- ggplot(glesmelt, aes(x=treatment, y = value)) + geom_boxplot(outlier.size=2, outlier.shape=19, outlier.colour= alpha("black", 1/2), fill=c(rgb(.6,.6,.6),rgb(.8,.8,.8))) + facet_grid(. ~ variable) + ylab("Response Latency (seconds)") + xlab(NULL) + scale_y_continuous(breaks = seq(0, 300, 30))
gg  
dev.off()  

pdf(file = "figures/itemRightLatencyBoxplots.pdf", width = 10, height = 6, family="URWTimes")
glesmelt <- melt(gles, measure.vars = rlnames)
glesmelt$answer <- NA
glesmelt$answer <- ifelse(glesmelt$variable=="rl1", glesmelt$k1, glesmelt$answer)
glesmelt$answer <- ifelse(glesmelt$variable=="rl2", glesmelt$k2, glesmelt$answer)
glesmelt$answer <- ifelse(glesmelt$variable=="rl3", glesmelt$k3, glesmelt$answer)
glesmelt$answer <- ifelse(glesmelt$variable=="rl4", glesmelt$k4, glesmelt$answer)
glesmelt$answer <- ifelse(glesmelt$variable=="rl5", glesmelt$k5, glesmelt$answer)
glesmelt$answer <- ifelse(glesmelt$variable=="rl6", glesmelt$k6, glesmelt$answer)
glesmelt$answer <- ifelse(glesmelt$variable=="rl7", glesmelt$k7, glesmelt$answer)
glesmelt$answer <- ifelse(glesmelt$variable=="rl8", glesmelt$k8, glesmelt$answer)
glesmelt$answer <- as.factor(ifelse(glesmelt$answer==1, "right", "wrong"))
levels(glesmelt$variable) <- itemnames
glesmelt <- glesmelt[glesmelt$answer=="right",]
glesmelt$variable <- setFactorOrder(glesmelt$variable, levels(glesmelt$variable)[c(2,1,4,3,5,7,6,8)]) # resort items
gg <- ggplot(glesmelt, aes(x=treatment, y = value)) + geom_boxplot(outlier.size=2, outlier.shape=19, outlier.colour= alpha("black", 1/2), fill=c(rgb(.6,.6,.6),rgb(.8,.8,.8))) + facet_grid(. ~ variable) + ylab("Response Latency (seconds)") + xlab(NULL) + scale_y_continuous(breaks = seq(0, 300, 30))
gg  
dev.off()  

pdf(file = "figures/latencyVisual.pdf", width = 10, height = 6, family="URWTimes")
glesmelt <- melt(gles, measure.vars = rlnames)
glesmelt$answer <- NA
glesmelt$answer <- ifelse(glesmelt$variable=="rl1", glesmelt$k1, glesmelt$answer)
glesmelt$answer <- ifelse(glesmelt$variable=="rl2", glesmelt$k2, glesmelt$answer)
glesmelt$answer <- ifelse(glesmelt$variable=="rl3", glesmelt$k3, glesmelt$answer)
glesmelt$answer <- ifelse(glesmelt$variable=="rl4", glesmelt$k4, glesmelt$answer)
glesmelt$answer <- ifelse(glesmelt$variable=="rl5", glesmelt$k5, glesmelt$answer)
glesmelt$answer <- ifelse(glesmelt$variable=="rl6", glesmelt$k6, glesmelt$answer)
glesmelt$answer <- ifelse(glesmelt$variable=="rl7", glesmelt$k7, glesmelt$answer)
glesmelt$answer <- ifelse(glesmelt$variable=="rl8", glesmelt$k8, glesmelt$answer)
glesmelt$answer <- as.factor(ifelse(glesmelt$answer==1, "right", "wrong"))
levels(glesmelt$variable) <- itemnames
glesmelt <- glesmelt[glesmelt$treatment=="visual",]
glesmelt$variable <- setFactorOrder(glesmelt$variable, levels(glesmelt$variable)[c(2,1,4,3,5,7,6,8)]) # resort items
gg <- ggplot(glesmelt, aes(x=answer, y = value)) + geom_boxplot(outlier.size=2, outlier.shape=19, outlier.colour= alpha("black", 1/2), fill=c(rgb(.6,.6,.6),rgb(.8,.8,.8))) + facet_grid(. ~ variable) + ylab("Response Latency (seconds)") + xlab(NULL) + scale_y_continuous(breaks = seq(0, 300, 30))
gg  
dev.off()  


# bar plot of total scores
pdf(file = "figures/correctTotals.pdf", width = 8, height = 6, family="URWTimes")
gg <- ggplot(gles, aes(factor(ktotal)))
#gg + geom_bar(aes(y = (..count..)*100/sum(..count..))) + facet_wrap(~ treatment) + ylab("Percent") + xlab("Number of correct answers")
gg + geom_bar(aes(y = (..count..)*100/sum(..count..), fill=treatment), position = "dodge", color="black") + ylab("Percent") + xlab("Number of correct answers") + 
theme(legend.position = c(0, 1.025), legend.justification = c(0, 1), 
       legend.background = element_rect(colour = NA, fill = NA)) + labs(fill = "") +scale_fill_manual(values = c(rgb(.6,.6,.6),rgb(.8,.8,.8)))
dev.off()


# line plot of item characteristic curves
d=data.frame(lt=c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "1F", "F1", "4C88C488", "12345678"))
pdf(file = "figures/itemCurves.pdf", width = 12, height = 6, family="URWTimes")
ksums <- ddply(gles, .(ktotal, treatment), summarise, k1mean = mean(k1), k2mean = mean(k2), k3mean = mean(k3), k4mean = mean(k4), k5mean = mean(k5), k6mean = mean(k6), k7mean = mean(k7), k8mean = mean(k8))
ksumsmelt <- melt(ksums, c("ktotal", "treatment"))
levels(ksumsmelt$variable) <- itemnames
ksumsmelt$variable <- setFactorOrder(ksumsmelt$variable, levels(ksumsmelt$variable)[c(2,1,4,3,5,7,6,8)]) # resort items
gg <- ggplot(ksumsmelt, aes(x = ktotal, y = value, group = variable, colour = variable, linetype = variable))
gg + geom_line() + facet_grid(. ~treatment) + ylab("Share of correct answers") + xlab("Overall score") + scale_x_continuous(breaks=seq(0,8,1)) + theme(legend.position = "bottom")+ scale_linetype_manual(values=seq(1,8,1), name = "Items:  ") + scale_color_grey(end=.6, name = "Items:  ")
dev.off()


# line plot of item characteristic curves, by item
pdf(file = "figures/itemCurvesByItem.pdf", width = 10, height = 6, family="URWTimes")
ksums <- ddply(gles, .(ktotal, treatment), summarise, k1mean = mean(k1), k2mean = mean(k2), k3mean = mean(k3), k4mean = mean(k4), k5mean = mean(k5), k6mean = mean(k6), k7mean = mean(k7), k8mean = mean(k8))
ksumsmelt <- melt(ksums, c("ktotal", "treatment"))
levels(ksumsmelt$variable) <- itemnames
ksumsmelt$variable <- setFactorOrder(ksumsmelt$variable, levels(ksumsmelt$variable)[c(2,1,4,3,5,7,6,8)]) # resort items
gg <- ggplot(ksumsmelt, aes(x = ktotal, y = value, group = treatment, linetype = treatment))
gg + geom_line() + facet_wrap( ~variable, ncol=4) + ylab("Share of correct answers") + xlab("Overall score") + scale_x_continuous(breaks=seq(0,8,1)) + theme(legend.position = "bottom")+ scale_linetype_manual(values=c(1,2), name = "Items:  ")
dev.off()


# line plot of response latencies
latencyPlot <- function(dataset, knowledgeLevel) {
glesmelt <- melt(dataset, measure.vars = rlnames)
glesmelt2 <- melt(dataset, measure.vars = knames, value.name = "kright")
glesmelt$kright <-  glesmelt2$kright
levels(glesmelt$variable) <- itemnames
glesmelt$variable <- setFactorOrder(glesmelt$variable, levels(glesmelt$variable)[c(2,1,4,3,5,7,6,8)])
glesmelt <- glesmelt[glesmelt$ktotal==knowledgeLevel,]
gg <- ggplot(glesmelt, aes(x = variable, y = value, group = lfdn))
gg + geom_line(color = alpha("black", 1/2)) + geom_point(aes(color=factor(kright))) + scale_y_continuous(limits=c(0,180), breaks=seq(0,180,30)) + xlab("Item") + ylab("Response Latency (seconds)") + facet_grid(~treatment) + ggtitle(str_c("Overall knowledge score: ", knowledgeLevel, "\n"))
}

pdf(file = "figures/itemLatencyCurves.pdf", width = 15, height = 6, family="URWTimes")
latencyPlot(gles, 0)
latencyPlot(gles, 1)
latencyPlot(gles, 2)
latencyPlot(gles, 3)
latencyPlot(gles, 4)
latencyPlot(gles, 5)
latencyPlot(gles, 6)
latencyPlot(gles, 7)
latencyPlot(gles, 8)
dev.off()