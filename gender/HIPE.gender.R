## Highly Important Papers in Ecology (HIPE)
## "Ecologists are gender-biased in their perceptions of important papers"
## Corey J. A. Bradshaw & Franck Courchamp
## September 2017

## Remove everything
rm(list = ls())

## libraries (install first)
library(igraph)
library(boot)
library(Hmisc)

## set working directory (adjust accordingly to where files 'HIPE.refs.txt', 'citation.csv', 'VoteArticles.final.csv',
## 'type.csv', 'field.csv', 'approach.csv', & 'nproposed.csv' live)
setwd("~/Documents/.../.../.../.../")

###############################################
## import PDF descriptions
pdf.dat <- read.table("HIPE.refs.txt", header=T, sep="\t")
pdf.name <- na.omit(pdf.dat[,1:2])

###############################################
## import citation data
cit.dat <- read.table("citation.csv", header=T, sep=",")

###############################################
## import gender data
gender.dat <- read.table("gender.csv", header=T, sep=",")

###############################################
## import proposer gender details
proposer.gender <- read.table("proposer.gender.csv", header=T, sep=",", na.strings = "NULL")

###############################################
## import survey data
dat.surv <- read.table("VoteArticles.final.csv", header=T, sep=",", na.strings = "NULL")
ldat <- dim(dat.surv)[1]

# country fix (fix bad nomenclature from survey entries)
dat.surv$Country <- as.character(dat.surv$Country)
country <- ifelse(dat.surv$Country == "U__S__A_" | dat.surv$Country == "U_S_A_" | dat.surv$Country == "U_S_" | dat.surv$Country == "US" | dat.surv$Country == "usa" | dat.surv$Country == "Usa" | dat.surv$Country == "USA" | dat.surv$Country == "USA_" | dat.surv$Country == "USA_UK" | dat.surv$Country == "United_States" |  dat.surv$Country == "United_Stated" | dat.surv$Country == "United_States_of_America", "USA", dat.surv$Country)
country <- ifelse(dat.surv$Country == "uk" | dat.surv$Country == "Uk" | dat.surv$Country == "UK" | dat.surv$Country == "united_kingdom" | dat.surv$Country == "United_Kingdom" | dat.surv$Country == "gb" | dat.surv$Country == "united_kindgdom" | dat.surv$Country == "England" | dat.surv$Country == "Englandb" | dat.surv$Country == "ENgland", "GBR", country)
country <- ifelse(dat.surv$Country == "Argentina", "ARG", country)
country <- ifelse(dat.surv$Country == "Australia" | dat.surv$Country == "Australia_and_UK" | dat.surv$Country == "Australia__UK" | dat.surv$Country == "Australia_and_USA", "AUS", country)
country <- ifelse(dat.surv$Country == "Austria", "AUT", country)
country <- ifelse(dat.surv$Country == "Belgium", "BEL", country)
country <- ifelse(dat.surv$Country == "Brazil", "BRA", country)
country <- ifelse(dat.surv$Country == "canada" | dat.surv$Country == "Canada" | dat.surv$Country == "Canada__USA" | dat.surv$Country == "Canada_USA", "CAN", country)
country <- ifelse(dat.surv$Country == "Spain" | dat.surv$Country == "Catalonia" | dat.surv$Country == "Spain_and_USA", "ESP", country)
country <- ifelse(dat.surv$Country == "Chile", "CHL", country)
country <- ifelse(dat.surv$Country == "China", "CHN", country)
country <- ifelse(dat.surv$Country == "Czech_Republic" | dat.surv$Country == "Czechoslovakia", "CZE", country)
country <- ifelse(dat.surv$Country == "Denmark", "DNK", country)
country <- ifelse(dat.surv$Country == "Finland", "FIN", country)
country <- ifelse(dat.surv$Country == "france" | dat.surv$Country == "France", "FRA", country)
country <- ifelse(dat.surv$Country == "germany" | dat.surv$Country == "Germany" | dat.surv$Country == "Germany__UK" | dat.surv$Country == "Germany_and_UK", "DEU", country)
country <- ifelse(dat.surv$Country == "israel" | dat.surv$Country == "Israel", "ISR", country)
country <- ifelse(dat.surv$Country == "Italy", "ITA", country)
country <- ifelse(dat.surv$Country == "Italy_and_USA", "ITA", country)
country <- ifelse(dat.surv$Country == "Japan", "JPN", country)
country <- ifelse(dat.surv$Country == "Mexico__B_A___M__Sc____England__Ph_D__", "MEX", country)
country <- ifelse(dat.surv$Country == "New_Zealand", "NZL", country)
country <- ifelse(dat.surv$Country == "Netherlands" | dat.surv$Country == "the_netherlands" | dat.surv$Country == "netherlands", "NLD", country)
country <- ifelse(dat.surv$Country == "Norway", "NOR", country)
country <- ifelse(dat.surv$Country == "Russia", "RUS", country)
country <- ifelse(dat.surv$Country == "South_Africa", "RSA", country)
country <- ifelse(dat.surv$Country == "Japan", "JPN", country)
country <- ifelse(dat.surv$Country == "Switzerland" | dat.surv$Country == "Switzerland_and_UK", "CHE", country)
country <- ifelse(dat.surv$Country == "Sweden", "SWE", country)
country <- ifelse(dat.surv$Country == "Physucs", "unknown", country)
table(country)

# loop through to copy experience (1 = <10yr; 2 = 10-25yr; 3 = >25yr), & country
# to identical ID numbers
experience <- dat.surv$Years
nation <- country
ip <- dat.surv$IP
ip.unique <- na.omit(unique(ip))
length(ip.unique)

# add continent
continent <- ifelse(country == "ARG" | country == "BRA" | country == "CHL", "SAM", NA)
continent <- ifelse(country == "CAN" | country == "USA" | country == "MEX", "NAM", continent)
continent <- ifelse(country == "AUT" | country == "BEL" | country == "CHE" | country == "CZE" | country == "DEU" | country == "DNK" | country == "ESP" | country == "FIN" | country == "FRA" | country == "GBR" | country == "ITA" | country == "NLD" | country == "NOR" | country == "SWE", "EUR", continent)
continent <- ifelse(country == "AUS" | country == "CHN" | country == "JPN", "ASI", continent)
continent <- ifelse(country == "ISR" | country == "RSA" | country == "RUS" | country == "unknown", "OTH", continent)

table(continent)
sum(table(continent))
cont <- continent

for (i in 1:(ldat-1)) {
  if (dat.surv$id[i+1] == dat.surv$id[i]) {
    experience[i+1] <- experience[i]
    nation[i+1] <- nation[i]
    cont[i+1] <- cont[i]
    ip[i+1] <- ip[i]
  }
  print(i)
}

dat.surv <- data.frame(dat.surv, nation, cont, experience, ip)

## choose subset (comment out where appropriate)
dat <- dat.surv # all voters
#dat <- subset(dat.surv, gender == "M") # males voters only
#dat <- subset(dat.surv, gender == "F") # females voters only


##########################
## Compile voting results
##########################
## List (score)
# 0 = no answer; 1 = top 10; 2 = 11-25; 3 = 26-100; 4 = not in top 100
# knowledge
# 0 = no answer; 1 = read it; 2 = know it; 3 = don't know it

pdf.seq <- seq(6,(20*3)+5,3)
score.seq <- seq(7,(20*3)+6,3)
know.seq <- seq(8,(20*3)+7,3)

dat.n0sc.mat <- matrix(data=NA, nrow=1, ncol=3)
dat.n0sc <- as.data.frame(dat.n0sc.mat)
colnames(dat.n0sc) <- c("pdf","score","know")

# proportions not known, scored, scored/not known storage matrix
dat.props <- matrix(data=NA, nrow=ldat, ncol=4)

## start loop
for (i in 1:ldat) {
  ident <- dat$id[i]
  pdf <- as.numeric(dat[i,pdf.seq])
  score <- as.numeric(dat[i,score.seq])
  know <- as.numeric(dat[i,know.seq])
  dat.i <- data.frame(pdf,score,know)
  p20.notknown <- length(which(dat.i$know == 0))/20 # proportion of 20 randomly selected papers 'not known'
  p20.scored <- length(which(dat.i$score > 0))/20 # proportion of 20 randomly selected papers 'not scored'
  p.scored.notknown <- (length(which(dat.i[which(dat.i$score > 0),]$know == 0)))/(length(which(dat.i$score > 0))) # proportion of scored papers 'not known'
  dat.props[i, ] <- c(ident, p20.notknown, p20.scored, p.scored.notknown)
  dat.i.n0sc <- subset(dat.i, score > 0)
  row.names(dat.i.n0sc) <- NULL
  dat.n0sc <- rbind(dat.n0sc, dat.i.n0sc)
  print(i)
}

# remove 1st row
dat.n0sc <- dat.n0sc[-1,]
dim(dat.n0sc)

# proportions summaries (scored, not known, scored/not known)
dat.props.out <- as.data.frame(dat.props)
colnames(dat.props.out) <- c("id","p20.notknown", "p20.scored", "p.scored.notknown")
dat.props.clean <- subset(dat.props.out, id > 0) # remove '0' id
p20.notknown.mn <- xtabs(p20.notknown ~ id, data=dat.props.clean)/table(dat.props.clean$id)
p20.scored.mn <- xtabs(p20.scored ~ id, data=dat.props.clean)/table(dat.props.clean$id)

psnk.ids <- as.numeric(names(xtabs(p.scored.notknown ~ id, data=dat.props.clean)))
ldpc <- length(psnk.ids)
denom.psnk <- rep(0,ldpc)
for (a in 1:ldpc) {
  denom.psnk[a] <- length(which(is.na(sub.dat <- subset(dat.props.clean, id == psnk.ids[a])$p.scored.notknown) == FALSE))
}
p.scored.notknown.mn <- xtabs(p.scored.notknown ~ id, data=dat.props.clean, exclude=c("NaN"))/denom.psnk
props.mn.out <- data.frame(p20.notknown.mn, p20.scored.mn)


## average rank
pdf.votes <- table(dat.n0sc$pdf) ## no votes for paper 67 or 157; hence, only 544 papers with voting data
sum.score <- xtabs(dat.n0sc$score ~ dat.n0sc$pdf)
score.avg <- sum.score/pdf.votes

score.avg.d <- as.data.frame(score.avg)
score.avg.d$pdf <- as.numeric(as.character(score.avg.d$dat.n0sc.pdf))
score.avg.d <- score.avg.d[,-1]
score.avg.dat <- data.frame(score.avg.d,as.integer(pdf.votes))
score.avg.dat <- score.avg.dat[-1,]
score.rnk <- rank(score.avg.dat[,1], ties.method="average")
score.avg.dat <- data.frame(score.avg.dat,score.rnk)
colnames(score.avg.dat) <- c("score.avg","pdf","nvotes","rank")
  
score.avg.mrg <- merge(score.avg.dat, pdf.name, by="pdf")

score.avg.mrg.sort <- score.avg.mrg[order(score.avg.mrg[,2],decreasing=F),]
rownames(score.avg.mrg.sort) <- seq(1:(dim(score.avg.mrg.sort)[1]))

## merge with citation data
score.cit <- merge(score.avg.mrg.sort, cit.dat, by="pdf")
age <- 2016 - score.cit$year
citWoKyr <- score.cit$citWoK/age
citGoogyr <- score.cit$citGoog/age
score.cit <- data.frame(score.cit, age, citWoKyr, citGoogyr)
score.cit.sort <- score.cit[order(score.cit[,2],decreasing=F),]

## merge with gender data
score.cit.gend <- merge(score.cit.sort, gender.dat, by="pdf")
plot(score.cit.gend$score.avg, score.cit.gend$prop.female, pch=19, xlab="score", ylab="proportion female co-authors")
fit.pF.score <- lm(score.cit.gend$prop.female ~ score.cit.gend$score.avg)
summary(fit.pF.score)
abline(fit.pF.score, lty=2, lwd=2, col="red")


## proposer gender analysis
# create table
prop.gend.tab <- table(proposer.gender)

# merge with author sex ratios
prop.gend.sex.ratio <- merge(prop.gend.tab, score.cit.gend, by="pdf")
prop.gend.sex.ratio.sort <- prop.gend.sex.ratio[order(prop.gend.sex.ratio[,1],decreasing=F),]

prop.sum.x.pdf <- xtabs(Freq ~ pdf, data=prop.gend.sex.ratio.sort)

prop.gend.pr <- 0
for (p in 1:dim(prop.sum.x.pdf)[1]) {
  pdf.int <- as.numeric(names(prop.sum.x.pdf)[p])
  prop.gend.pr <- c(prop.gend.pr, subset(prop.gend.sex.ratio.sort, pdf == pdf.int)$Freq / as.numeric(prop.sum.x.pdf[p]))
  print(p)
}
prop.gend.pr <- prop.gend.pr[-1]
prop.gend.sex.ratio.sort.prop.pr <- data.frame(prop.gend.sex.ratio.sort[c(1:4,22)], prop.gend.pr)
M.prop.gend.sex.ratio.sort.prop.pr <- subset(prop.gend.sex.ratio.sort.prop.pr, prop.gend=="M")
F.prop.gend.sex.ratio.sort.prop.pr <- subset(prop.gend.sex.ratio.sort.prop.pr, prop.gend=="F")

par(mfrow=c(1,2))
plot(F.prop.gend.sex.ratio.sort.prop.pr[,6],F.prop.gend.sex.ratio.sort.prop.pr[,5], pch=19, xlab="proportion female proposed", ylab="proportion female co-authors")
plot(M.prop.gend.sex.ratio.sort.prop.pr[,6],M.prop.gend.sex.ratio.sort.prop.pr[,5], pch=19, xlab="proportion male proposed", ylab="proportion female co-authors")
par(mfrow=c(1,1))

M.only.prop.gend.sex.ratio.sort.prop.pr <- subset(prop.gend.sex.ratio.sort.prop.pr, prop.gend=="M" & prop.gend.pr==1)
mean(M.only.prop.gend.sex.ratio.sort.prop.pr$prop.female, na.rm=T)
sd(M.only.prop.gend.sex.ratio.sort.prop.pr$prop.female, na.rm=T)
length(M.only.prop.gend.sex.ratio.sort.prop.pr$prop.female)

F.only.prop.gend.sex.ratio.sort.prop.pr <- subset(prop.gend.sex.ratio.sort.prop.pr, prop.gend=="F" & prop.gend.pr==1)
mean(F.only.prop.gend.sex.ratio.sort.prop.pr$prop.female, na.rm=T)
sd(F.only.prop.gend.sex.ratio.sort.prop.pr$prop.female, na.rm=T)
length(F.only.prop.gend.sex.ratio.sort.prop.pr$prop.female)


## read-only articles (in vote)
read.dat <- subset(dat.n0sc, know == 1)
pdf.votes <- table(read.dat$pdf)
sum.score <- xtabs(read.dat$score ~ read.dat$pdf)
score.avg <- sum.score/pdf.votes

score.avg.d <- as.data.frame(score.avg)
score.avg.d <- data.frame(as.integer(row.names(score.avg.d)), score.avg.d)
score.avg.d <- score.avg.d[,-2]
score.avg.dat <- data.frame(score.avg.d,as.integer(pdf.votes))
score.avg.dat <- score.avg.dat[-1,]
score.rnk <- rank(score.avg.dat[,2], ties.method="average")
score.avg.dat <- data.frame(score.avg.dat,score.rnk)
colnames(score.avg.dat) <- c("pdf","score.avg","nvotes","rank")

score.avg.mrg <- merge(score.avg.dat, pdf.name, by="pdf")

score.avg.mrg.sort <- score.avg.mrg[order(score.avg.mrg[,2],decreasing=F),]
rownames(score.avg.mrg.sort) <- seq(1:(dim(score.avg.mrg.sort)[1]))
score.avg.mrg.sort[1:100,]

## merge with citation data
score.cit <- merge(score.avg.mrg.sort, cit.dat, by="pdf")
age <- 2016 - score.cit$year
citWoKyr <- score.cit$citWoK/age
citGoogyr <- score.cit$citGoog/age
score.cit <- data.frame(score.cit, age, citWoKyr, citGoogyr)
score.cit.sort <- score.cit[order(score.cit[,2],decreasing=F),]
range(score.cit.sort$nvotes)
median(score.cit.sort$nvotes)

## read-only articles gender analysis
## merge with gender data
score.cit.gend <- merge(score.cit.sort, gender.dat, by="pdf")
plot(score.cit.gend$score.avg, score.cit.gend$prop.female, pch=19, xlab="score", ylab="proportion female co-authors")
fit.pF.score <- lm(score.cit.gend$prop.female ~ score.cit.gend$score.avg)
summary(fit.pF.score)
abline(fit.pF.score, lty=2, lwd=2, col="red")
