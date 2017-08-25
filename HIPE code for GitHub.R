## Highly Important Papers in Ecology (HIPE)
## "100 articles every ecologist should read" (Nature Ecology & Evolution)
## Corey J. A. Bradshaw & Franck Courchamp
## August 2017

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

# summarise by IP address
dim(table(dat.surv$ip)) - 1 # unique IPs
ip.vec <- names(table(dat.surv$ip))
ip.vec <- ip.vec[-1]
lip <- length(ip.vec)
nation.des <- NA


for(p in 1:lip) {
 ip.tmp <- subset(dat.surv, ip == ip.vec[p]) 
 nation.des[p] <- names(which((table(ip.tmp$nation)) > 0))
}
table(nation.des)

# Import category data
paper.type <- read.table("type.csv", header=T, sep=",", na.strings = "NULL")
paper.field <- read.table("field.csv", header=T, sep=",", na.strings = "NULL")
paper.approach <- read.table("approach.csv", header=T, sep=",", na.strings = "NULL")

## choose subset
dat <- dat.surv # all


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


mean(as.numeric(p20.notknown.mn))
sd(as.numeric(p20.notknown.mn))
median(as.numeric(p20.notknown.mn))

mean(as.numeric(p20.scored.mn))
sd(as.numeric(p20.scored.mn))
median(as.numeric(p20.scored.mn))

mean(as.numeric(p.scored.notknown.mn))
sd(as.numeric(p.scored.notknown.mn))
median(as.numeric(p.scored.notknown.mn))

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
  
score.avg.mrg1 <- merge(score.avg.dat, pdf.name, by="pdf")
score.avg.mrg2 <- merge(score.avg.mrg1, paper.type, by="pdf")
score.avg.mrg3 <- merge(score.avg.mrg2, paper.field, by="pdf")
score.avg.mrg <- merge(score.avg.mrg3, paper.approach, by="pdf")

score.avg.mrg.sort <- score.avg.mrg[order(score.avg.mrg[,2],decreasing=F),]
rownames(score.avg.mrg.sort) <- seq(1:(dim(score.avg.mrg.sort)[1]))

## merge with citation data
score.cit <- merge(score.avg.mrg.sort, cit.dat, by="pdf")
age <- 2016 - score.cit$year
citWoKyr <- score.cit$citWoK/age
citGoogyr <- score.cit$citGoog/age
score.cit <- data.frame(score.cit, age, citWoKyr, citGoogyr)
score.cit.sort <- score.cit[order(score.cit[,2],decreasing=F),]



############################
## proposer trends analysis
############################
proposed.dat <- read.table("nproposed.csv", header=T, sep=",")
proposed.dat.sort <- proposed.dat[order(proposed.dat[,2], decreasing=T),]
par(mfrow=c(2,2))
plot(proposed.dat.sort$nproposed, type="l", ylab="times proposed", xlab="paper index")

# merge with citation data
## merge with citation data
proposed.cit <- merge(proposed.dat.sort, score.cit.sort, by="pdf")
plot(proposed.cit$nproposed,proposed.cit$score.avg, pch=19, ylab="mean score", xlab="times proposed")
plot(proposed.cit$nproposed,proposed.cit$age, pch=19, ylab="age", xlab="times proposed")
plot(proposed.cit$nproposed,proposed.cit$citWoKyr, pch=19, ylab="WoK cites/year", xlab="times proposed")
par(mfrow=c(1,1))

## randomised correlation
iter <- 10000; itdiv <- iter/100

x <- proposed.cit$nproposed
y <- proposed.cit$citWoKyr

dat.xy <- na.omit(data.frame(x,y))

## turn on if only top 100
dat.xy.sort <- dat.xy[order(dat.xy[,1],decreasing=F),]
#dat.xy <- dat.xy[1:100,]

plot(dat.xy$x,(dat.xy$y),pch=19,cex=0.7,xlab="x",ylab="y")
abline(lm((dat.xy$y)~dat.xy$x),col="red",lty=2,lwd=3)
lpdf <- dim(dat.xy)[1]

SSxx.obs <- sum((dat.xy$x - mean(dat.xy$x))^2)
SSyy.obs <- sum((dat.xy$y - mean(dat.xy$y))^2)
SSxy.obs <- sum((dat.xy$x - mean(dat.xy$x))*(dat.xy$y - mean(dat.xy$y)))
COVxy.obs <- SSxy.obs/(lpdf-1)
VARx.obs <- SSxx.obs/(lpdf-1)
slope.obs <- SSxy.obs/SSxx.obs
int.obs <- ((sum(dat.xy$y))/lpdf) - slope.obs*(sum(dat.xy$x)/lpdf)
yhat.obs <- int.obs + (slope.obs*dat.xy$x)
e.obs <- dat.xy$y - yhat.obs  
MSE.obs <- (SSyy.obs - (SSxy.obs^2 / SSxx.obs))/(lpdf-2)
RSE.obs <- sqrt(MSE.obs)

RSE.ran <- slope.RAN <- rep(0,iter)

for (i in 1:iter) {
  ran <- runif(lpdf,min=1,max=lpdf)
  ran.dat <- data.frame(dat.xy$y,ran)
  ran.ord <- ran.dat[order(ran.dat[,2],decreasing=F),]
  colnames(ran.ord) <- c("y","ran")
  dat.iter <- data.frame(dat.xy$x,ran.ord$y)
  colnames(dat.iter) <- c("x","y")
  
  SSxx.ran <- sum((dat.iter$x - mean(dat.iter$x))^2)
  SSyy.ran <- sum((dat.iter$y - mean(dat.iter$y))^2)
  SSxy.ran <- sum((dat.iter$x - mean(dat.iter$x))*(dat.iter$y - mean(dat.iter$y)))
  slope.RAN[i] <- SSxy.ran/SSxx.ran
  RSE.ran[i] <- sqrt((SSyy.ran - (SSxy.ran^2 / SSxx.ran))/(lpdf-2))
  
  if (i %% itdiv==0) print(i)
}
ran.RSE.vec <- ifelse(RSE.ran <= RSE.obs, 1, 0)
ran.slope.vec <- ifelse(slope.RAN >= slope.obs, 1, 0)
p.slope.ran <- sum(ran.slope.vec)/iter
p.slope.ran <- ifelse(p.slope.ran == 0, paste("p.slope <", 1/iter), paste("p.slope =", p.slope.ran))
p.slope.ran
p.ran <- sum(ran.RSE.vec)/iter
p.cor <- ifelse(p.ran == 0, paste("p <", 1/iter), paste("p =", p.ran))
p.cor
slope.obs



## bootstrap median scores by category
# type
cat.lab <- colnames(paper.type[2:dim(paper.type)[2]])
lcat <- length(cat.lab)

# field
cat.lab <- colnames(paper.field[2:dim(paper.field)[2]])
lcat <- length(cat.lab)

# approach
cat.lab <- colnames(paper.approach[2:dim(paper.approach)[2]])
lcat <- length(cat.lab)

lo.vec <- hi.vec <- med.vec <- rep(0,lcat)
## choose category
for (c in 1:lcat) {
  col.sub <- which(colnames(score.cit.sort) == cat.lab[c])
  row.subs <- which(score.cit.sort[,col.sub] == 1)
  dat.cat <- score.cit.sort[row.subs,]    

  ldat.cat <- dim(dat.cat)[1]
  iter.cat <- 10000
  boot.med.vec <- rep(0,iter.cat)
  
  for (s in 1:iter.cat) {
    score.cat.smp <- sample(dat.cat$score.avg, ldat.cat, replace=T)
    boot.med.vec[s] <- median(score.cat.smp)
  }
  med.vec[c] <- median(boot.med.vec)
  lo.vec[c] <- quantile(boot.med.vec, probs=0.025)
  hi.vec[c] <- quantile(boot.med.vec, probs=0.975)
  
  print(c)
}

boot.med.out <- data.frame(med.vec,hi.vec,lo.vec)
row.names(boot.med.out) <- cat.lab
boot.med.out


errbar(row.names(boot.med.out), boot.med.out[,1], yplus = boot.med.out[,2], yminus = boot.med.out[,3], xlab="score")


## randomisation correlations
iter <- 10000; itdiv <- iter/100

x <- score.cit.sort$score.avg

## choose desired 'y' (ordinate) data (i.e., blank out others)
#y <- score.cit.sort$age
#y <- score.cit.sort$Ifjrnl
#y <- score.cit.sort$citWoK
#y <- score.cit.sort$citWoKyr
#y <- score.cit.sort$citGoog
y <- score.cit.sort$citGoogyr

dat.xy <- na.omit(data.frame(x,y))

## turn on if only top 100
dat.xy.sort <- dat.xy[order(dat.xy[,1],decreasing=F),]
#dat.xy <- dat.xy[1:100,]

plot(dat.xy$x,log10(dat.xy$y),pch=19,cex=0.7,xlab="x",ylab="y")
abline(lm(log10(dat.xy$y)~dat.xy$x),col="red",lty=2,lwd=3)
lpdf <- dim(dat.xy)[1]


SSxx.obs <- sum((dat.xy$x - mean(dat.xy$x))^2)
SSyy.obs <- sum((dat.xy$y - mean(dat.xy$y))^2)
SSxy.obs <- sum((dat.xy$x - mean(dat.xy$x))*(dat.xy$y - mean(dat.xy$y)))
COVxy.obs <- SSxy.obs/(lpdf-1)
VARx.obs <- SSxx.obs/(lpdf-1)
slope.obs <- SSxy.obs/SSxx.obs
int.obs <- ((sum(dat.xy$y))/lpdf) - slope.obs*(sum(dat.xy$x)/lpdf)
yhat.obs <- int.obs + (slope.obs*dat.xy$x)
e.obs <- dat.xy$y - yhat.obs  
MSE.obs <- (SSyy.obs - (SSxy.obs^2 / SSxx.obs))/(lpdf-2)
RSE.obs <- sqrt(MSE.obs)

RSE.ran <- rep(0,iter)

for (i in 1:iter) {
  ran <- runif(lpdf,min=1,max=lpdf)
  ran.dat <- data.frame(dat.xy$y,ran)
  ran.ord <- ran.dat[order(ran.dat[,2],decreasing=F),]
  colnames(ran.ord) <- c("y","ran")
  dat.iter <- data.frame(dat.xy$x,ran.ord$y)
  colnames(dat.iter) <- c("x","y")

  SSxx.ran <- sum((dat.iter$x - mean(dat.iter$x))^2)
  SSyy.ran <- sum((dat.iter$y - mean(dat.iter$y))^2)
  SSxy.ran <- sum((dat.iter$x - mean(dat.iter$x))*(dat.iter$y - mean(dat.iter$y)))
  RSE.ran[i] <- sqrt((SSyy.ran - (SSxy.ran^2 / SSxx.ran))/(lpdf-2))

  if (i %% itdiv==0) print(i)
}
ran.RSE.vec <- ifelse(RSE.ran <= RSE.obs, 1, 0)
p.ran <- sum(ran.RSE.vec)/iter
p.cor <- ifelse(p.ran == 0, paste("p <", 1/iter), paste("p =", p.ran))
p.cor


## read or known only
readknown.dat <- subset(dat.n0sc, know != 1)
pdf.votes <- table(readknown.dat$pdf)
sum.score <- xtabs(readknown.dat$score ~ readknown.dat$pdf)
score.avg <- sum.score/pdf.votes

score.avg.d <- as.data.frame(score.avg)
score.avg.d <- data.frame(as.integer(row.names(score.avg.d)), score.avg.d)
score.avg.d <- score.avg.d[,-2]
score.avg.dat <- data.frame(score.avg.d,as.integer(pdf.votes))
score.avg.dat <- score.avg.dat[-1,]
score.rnk <- rank(score.avg.dat[,2], ties.method="average")
score.avg.dat <- data.frame(score.avg.dat,score.rnk)
colnames(score.avg.dat) <- c("pdf","score.avg","nvotes","rank")

score.avg.mrg1 <- merge(score.avg.dat, pdf.name, by="pdf")
score.avg.mrg2 <- merge(score.avg.mrg1, paper.type, by="pdf")
score.avg.mrg3 <- merge(score.avg.mrg2, paper.field, by="pdf")
score.avg.mrg <- merge(score.avg.mrg3, paper.approach, by="pdf")

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
round(quantile(score.cit.sort$age[1:100], prob=0.025),0)
round(median(score.cit.sort$age[1:100]),0)
round(quantile(score.cit.sort$age[1:100], prob=0.975),0)

par(mfrow=c(2,3))
plot(score.cit.sort$rank, log10(score.cit.sort$age), pch=4, xlab="rank", ylab="log article age (yrs)")
abline(lm(log10(score.cit.sort$age) ~ score.cit.sort$rank),col="red",lwd=3,lty=2)
plot(score.cit.sort$rank, score.cit.sort$Ifjrnl, pch=4, xlab="rank", ylab="journal IF")
abline(lm((score.cit.sort$Ifjrnl) ~ score.cit.sort$rank),col="red",lwd=3,lty=2)
plot(score.cit.sort$rank, log10(score.cit.sort$citWoK), pch=4, xlab="rank", ylab="log WoK citations")
abline(lm(log10(citWoK) ~ rank, data=score.cit.sort, na.action=na.omit), col="red",lwd=3,lty=2)
plot(score.cit.sort$rank, log10(score.cit.sort$citWoKyr), pch=4, xlab="rank", ylab="log mean annual WoK citations")
abline(lm(log10(score.cit.sort$citWoKyr) ~ score.cit.sort$rank),col="red",lwd=3,lty=2)
plot(score.cit.sort$rank, log10(score.cit.sort$citGoog), pch=4, xlab="rank", ylab="log Google citations")
abline(lm(log10(score.cit.sort$citGoog) ~ score.cit.sort$rank),col="red",lwd=3,lty=2)
plot(score.cit.sort$rank, log10(score.cit.sort$citGoogyr), pch=4, xlab="rank", ylab="log mean annual Google citations")
abline(lm(log10(score.cit.sort$citGoogyr) ~ score.cit.sort$rank),col="red",lwd=3,lty=2)
par(mfrow=c(1,1))


## bootstrap median scores by category
# type
cat.lab <- colnames(paper.type[2:dim(paper.type)[2]])
lcat <- length(cat.lab)

# field
cat.lab <- colnames(paper.field[2:dim(paper.field)[2]])
lcat <- length(cat.lab)

# approach
cat.lab <- colnames(paper.approach[2:dim(paper.approach)[2]])
lcat <- length(cat.lab)

lo.vec <- hi.vec <- med.vec <- rep(0,lcat)
## choose category
for (c in 1:lcat) {
  col.sub <- which(colnames(score.cit.sort) == cat.lab[c])
  row.subs <- which(score.cit.sort[,col.sub] == 1)
  dat.cat <- score.cit.sort[row.subs,]    
  
  ldat.cat <- dim(dat.cat)[1]
  iter.cat <- 10000
  boot.med.vec <- rep(0,iter.cat)
  
  for (s in 1:iter.cat) {
    score.cat.smp <- sample(dat.cat$score.avg, ldat.cat, replace=T)
    boot.med.vec[s] <- median(score.cat.smp)
  }
  med.vec[c] <- median(boot.med.vec)
  lo.vec[c] <- quantile(boot.med.vec, probs=0.025)
  hi.vec[c] <- quantile(boot.med.vec, probs=0.975)
  
  print(c)
}

boot.med.out <- data.frame(med.vec,hi.vec,lo.vec)
row.names(boot.med.out) <- cat.lab
boot.med.out

library(Hmisc)
errbar(row.names(boot.med.out), boot.med.out[,1], yplus = boot.med.out[,2], yminus = boot.med.out[,3], xlab="score")



## randomisation correlations
iter <- 10000; itdiv <- iter/100

x <- score.cit.sort$score.avg

## choose desired 'y' (ordinate) data (i.e., blank out others)
#y <- score.cit.sort$age
#y <- score.cit.sort$Ifjrnl
#y <- score.cit.sort$citWoK
#y <- score.cit.sort$citWoKyr
#y <- score.cit.sort$citGoog
y <- score.cit.sort$citGoogyr

dat.xy <- na.omit(data.frame(x,y))

## turn on if only top 100
dat.xy.sort <- dat.xy[order(dat.xy[,1],decreasing=F),]
#dat.xy <- dat.xy[1:100,]

plot(dat.xy$x,log10(dat.xy$y),pch=19,cex=0.7,xlab="x",ylab="y")
abline(lm(log10(dat.xy$y)~dat.xy$x),col="red",lty=2,lwd=3)
lpdf <- dim(dat.xy)[1]

SSxx.obs <- sum((dat.xy$x - mean(dat.xy$x))^2)
SSyy.obs <- sum((dat.xy$y - mean(dat.xy$y))^2)
SSxy.obs <- sum((dat.xy$x - mean(dat.xy$x))*(dat.xy$y - mean(dat.xy$y)))
COVxy.obs <- SSxy.obs/(lpdf-1)
VARx.obs <- SSxx.obs/(lpdf-1)
slope.obs <- SSxy.obs/SSxx.obs
int.obs <- ((sum(dat.xy$y))/lpdf) - slope.obs*(sum(dat.xy$x)/lpdf)
yhat.obs <- int.obs + (slope.obs*dat.xy$x)
e.obs <- dat.xy$y - yhat.obs  
MSE.obs <- (SSyy.obs - (SSxy.obs^2 / SSxx.obs))/(lpdf-2)
RSE.obs <- sqrt(MSE.obs)

RSE.ran <- rep(0,iter)

for (i in 1:iter) {
  ran <- runif(lpdf,min=1,max=lpdf)
  ran.dat <- data.frame(dat.xy$y,ran)
  ran.ord <- ran.dat[order(ran.dat[,2],decreasing=F),]
  colnames(ran.ord) <- c("y","ran")
  dat.iter <- data.frame(dat.xy$x,ran.ord$y)
  colnames(dat.iter) <- c("x","y")
  
  SSxx.ran <- sum((dat.iter$x - mean(dat.iter$x))^2)
  SSyy.ran <- sum((dat.iter$y - mean(dat.iter$y))^2)
  SSxy.ran <- sum((dat.iter$x - mean(dat.iter$x))*(dat.iter$y - mean(dat.iter$y)))
  RSE.ran[i] <- sqrt((SSyy.ran - (SSxy.ran^2 / SSxx.ran))/(lpdf-2))
  
  if (i %% itdiv==0) print(i)
}
ran.RSE.vec <- ifelse(RSE.ran <= RSE.obs, 1, 0)
p.ran <- sum(ran.RSE.vec)/iter
p.cor <- ifelse(p.ran == 0, paste("p <", 1/iter), paste("p =", p.ran))
p.cor


## read only
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

score.avg.mrg1 <- merge(score.avg.dat, pdf.name, by="pdf")
score.avg.mrg2 <- merge(score.avg.mrg1, paper.type, by="pdf")
score.avg.mrg3 <- merge(score.avg.mrg2, paper.field, by="pdf")
score.avg.mrg <- merge(score.avg.mrg3, paper.approach, by="pdf")

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
round(quantile(score.cit.sort$age[1:100], prob=0.025),0)
round(median(score.cit.sort$age[1:100]),0)
round(quantile(score.cit.sort$age[1:100], prob=0.975),0)
write.table(score.cit.sort,file="read.score.cit.csv",sep=",",dec = ".", row.names = T,col.names = TRUE)

par(mfrow=c(2,3))
plot(score.cit.sort$rank, log10(score.cit.sort$age), pch=4, xlab="rank", ylab="log article age (yrs)")
abline(lm(log10(score.cit.sort$age) ~ score.cit.sort$rank),col="red",lwd=3,lty=2)
plot(score.cit.sort$rank, score.cit.sort$Ifjrnl, pch=4, xlab="rank", ylab="journal IF")
abline(lm((score.cit.sort$Ifjrnl) ~ score.cit.sort$rank),col="red",lwd=3,lty=2)
plot(score.cit.sort$rank, log10(score.cit.sort$citWoK), pch=4, xlab="rank", ylab="log WoK citations")
abline(lm(log10(citWoK) ~ rank, data=score.cit.sort, na.action=na.omit), col="red",lwd=3,lty=2)
plot(score.cit.sort$rank, log10(score.cit.sort$citWoKyr), pch=4, xlab="rank", ylab="log mean annual WoK citations")
abline(lm(log10(score.cit.sort$citWoKyr) ~ score.cit.sort$rank),col="red",lwd=3,lty=2)
plot(score.cit.sort$rank, log10(score.cit.sort$citGoog), pch=4, xlab="rank", ylab="log Google citations")
abline(lm(log10(score.cit.sort$citGoog) ~ score.cit.sort$rank),col="red",lwd=3,lty=2)
plot(score.cit.sort$rank, log10(score.cit.sort$citGoogyr), pch=4, xlab="rank", ylab="log mean annual Google citations")
abline(lm(log10(score.cit.sort$citGoogyr) ~ score.cit.sort$rank),col="red",lwd=3,lty=2)
par(mfrow=c(1,1))


## bootstrap median scores by category
# type
cat.lab <- colnames(paper.type[2:dim(paper.type)[2]])
lcat <- length(cat.lab)

# field
cat.lab <- colnames(paper.field[2:dim(paper.field)[2]])
lcat <- length(cat.lab)

# approach
cat.lab <- colnames(paper.approach[2:dim(paper.approach)[2]])
lcat <- length(cat.lab)

lo.vec <- hi.vec <- med.vec <- rep(0,lcat)
## choose category
for (c in 1:lcat) {
  col.sub <- which(colnames(score.cit.sort) == cat.lab[c])
  row.subs <- which(score.cit.sort[,col.sub] == 1)
  dat.cat <- score.cit.sort[row.subs,]    
  
  ldat.cat <- dim(dat.cat)[1]
  iter.cat <- 10000
  boot.med.vec <- rep(0,iter.cat)
  
  for (s in 1:iter.cat) {
    score.cat.smp <- sample(dat.cat$score.avg, ldat.cat, replace=T)
    boot.med.vec[s] <- median(score.cat.smp)
  }
  med.vec[c] <- median(boot.med.vec)
  lo.vec[c] <- quantile(boot.med.vec, probs=0.025)
  hi.vec[c] <- quantile(boot.med.vec, probs=0.975)
  
  print(c)
}

boot.med.out <- data.frame(med.vec,hi.vec,lo.vec)
row.names(boot.med.out) <- cat.lab
boot.med.out

library(Hmisc)
errbar(row.names(boot.med.out), boot.med.out[,1], yplus = boot.med.out[,2], yminus = boot.med.out[,3], xlab="score")



## randomisation correlations
iter <- 10000; itdiv <- iter/100

x <- score.cit.sort$score.avg

## choose desired 'y' (ordinate) data (i.e., blank out others)
#y <- score.cit.sort$age
#y <- score.cit.sort$Ifjrnl
#y <- score.cit.sort$citWoK
#y <- score.cit.sort$citWoKyr
#y <- score.cit.sort$citGoog
y <- score.cit.sort$citGoogyr

dat.xy <- na.omit(data.frame(x,y))

## turn on if only top 100
dat.xy.sort <- dat.xy[order(dat.xy[,1],decreasing=F),]
#dat.xy <- dat.xy[1:100,]

plot(dat.xy$x,log10(dat.xy$y),pch=19,cex=0.7,xlab="x",ylab="y")
abline(lm(log10(dat.xy$y)~dat.xy$x),col="red",lty=2,lwd=3)
lpdf <- dim(dat.xy)[1]

SSxx.obs <- sum((dat.xy$x - mean(dat.xy$x))^2)
SSyy.obs <- sum((dat.xy$y - mean(dat.xy$y))^2)
SSxy.obs <- sum((dat.xy$x - mean(dat.xy$x))*(dat.xy$y - mean(dat.xy$y)))
COVxy.obs <- SSxy.obs/(lpdf-1)
VARx.obs <- SSxx.obs/(lpdf-1)
slope.obs <- SSxy.obs/SSxx.obs
int.obs <- ((sum(dat.xy$y))/lpdf) - slope.obs*(sum(dat.xy$x)/lpdf)
yhat.obs <- int.obs + (slope.obs*dat.xy$x)
e.obs <- dat.xy$y - yhat.obs  
MSE.obs <- (SSyy.obs - (SSxy.obs^2 / SSxx.obs))/(lpdf-2)
RSE.obs <- sqrt(MSE.obs)

RSE.ran <- rep(0,iter)

for (i in 1:iter) {
  ran <- runif(lpdf,min=1,max=lpdf)
  ran.dat <- data.frame(dat.xy$y,ran)
  ran.ord <- ran.dat[order(ran.dat[,2],decreasing=F),]
  colnames(ran.ord) <- c("y","ran")
  dat.iter <- data.frame(dat.xy$x,ran.ord$y)
  colnames(dat.iter) <- c("x","y")
  
  SSxx.ran <- sum((dat.iter$x - mean(dat.iter$x))^2)
  SSyy.ran <- sum((dat.iter$y - mean(dat.iter$y))^2)
  SSxy.ran <- sum((dat.iter$x - mean(dat.iter$x))*(dat.iter$y - mean(dat.iter$y)))
  RSE.ran[i] <- sqrt((SSyy.ran - (SSxy.ran^2 / SSxx.ran))/(lpdf-2))
  
  if (i %% itdiv==0) print(i)
}
ran.RSE.vec <- ifelse(RSE.ran <= RSE.obs, 1, 0)
p.ran <- sum(ran.RSE.vec)/iter
p.cor <- ifelse(p.ran == 0, paste("p <", 1/iter), paste("p =", p.ran))
p.cor




