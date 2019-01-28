#install.packages("caper")
#install.packages("ape")
#install.packages("geiger")
#install.packages("ggplot2")
#install.packages("sqldf")
library(caper)
library(ape)
library(geiger)
library(ggplot2)
library(sqldf)
library(dplyr)


#Import cleaned data
#Note: I imported data that I generated using your Data corrections.R file with the missing2 -> missing3 modification
#Once you have finalized your corrections script, you can modify this to import that final dataset
indata <-read.csv("Short_data_for_PCA_SK.csv", header=TRUE, sep=",")

#convert relevant columns back to numeric data
#colnames(indata)
indata[10:33] <- sapply(indata[10:33], as.numeric )

#remove any entries not included in the phylogeny
indata <- subset(indata, PhyloCode != "")

#summarize data by species and behavior so we can use for phylogenetic analyses
avgData <- indata %>% 
  group_by(PhyloCode) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

behData <- indata %>% 
  group_by(PhyloCode) %>% 
  summarise_at("BehPheno", funs(paste(na.omit(.), collapse = ",")))

#merge behavioral and averaged data into single data frame
cleanData <- data.frame(inner_join(avgData,behData, by="PhyloCode"))

#use a for loop to include only unique behavioral entries
for (i in 1:nrow(cleanData)) {
  cleanData$BehPheno[i] <- toString(sort(unique(unlist(strsplit(cleanData$BehPheno[i], ","))))) #this is ridiculous but it works...
}

#summarize different types of behaviors
cleanData$Social_Status == "NA"
cleanData$Social_Status[cleanData$BehPheno == "1"] <- "Solitary"
cleanData$Social_Status[cleanData$BehPheno == "2"] <- "Communal"
cleanData$Social_Status[cleanData$BehPheno == "1, 2"] <- "Communal"
cleanData$Social_Status[cleanData$BehPheno == "3"] <- "Semisocial"
cleanData$Social_Status[cleanData$BehPheno == "2, 3"] <- "Semisocial"
cleanData$Social_Status[cleanData$BehPheno == "4"] <- "Eusocial"
cleanData$Social_Status[cleanData$BehPheno == "5"] <- "Eusocial"
cleanData$Social_Status[cleanData$BehPheno == "1, 4"] <- "Sol/Eus"
cleanData$Social_Status[cleanData$BehPheno == "1, 3, 4"] <- "Sol/Eus"
cleanData$Social_Status[cleanData$BehPheno == "1, 2, 4"] <- "Sol/Eus"


row.names(cleanData) <- cleanData$PhyloCode

tree <-makeLabel(read.tree("gibbs_outgroups.tre"))
plot(tree, cex=0.25)

both <- intersect(tree$tip.label, rownames(cleanData))
tree.matched <- root(drop.tip(tree, setdiff(tree$tip.label, both)), outgroup="Dufnovaeangl")
data.matched <- cleanData[both,]

plot(tree.matched, no.margin=TRUE, cex =.5)
data.matched <- data.frame(rownames(data.matched), data.matched)
colnames(data.matched)[1]="sp_name"


datacomp <-comparative.data(phy=tree.matched, data=data.matched, names.col=sp_name, vcv = TRUE, vcv.dim=3, na.omit=FALSE, warn.dropped=TRUE, force.root = TRUE)
dev.off()


#graph it
socialityLabel <- character(length(tree.matched$tip.label))
names(socialityLabel) <- tree.matched$tip.label

data.matched$color <- "gray"
data.matched$color[data.matched$Social_Status == "Eusocial"] <- "dodgerblue3"
data.matched$color[data.matched$Social_Status=="Communal"] <- "gold3"
data.matched$color[data.matched$Social_Status=="Solitary"] <- "firebrick3"
data.matched$color[data.matched$Social_Status=="Sol/Eus"] <- "hotpink4"

colors <- c(data.matched$color[match(names(socialityLabel), data.matched$sp_name)])
names(colors) <- tree.matched$tip.label

# make circles on phylogeny proportional to the average antennal sensilla density

#sizes <- sqrt(super$avg)/pi

sizes <- sqrt(data.matched$NnestsMin)
#sizes <- sqrt(c(super$seg9[match(names(socialityLabel), super$sp_name)]) + c(super$seg10[match(names(socialityLabel), super$sp_name)])/2)/pi
names(sizes) <- data.matched$sp_name
#izes["Laszephyrum"] = sqrt(c(super$seg9[super$sp_name=="Laszephyrum"])/pi)
#sizes["Halpoeyi"] = sqrt(c(super$seg10[super$sp_name=="Halpoeyi"])/pi)
#sizes["Goeperuensis"] = sqrt(c(super$seg10[super$sp_name=="Goeperuensis"])/pi)

pdf("miriam_phylogeny.pdf")
plot(tree.matched, type="phylogram", adj=0, cex=0.5, label.offset = 2, use.edge.length=FALSE, edge.width=1.5)
points(rep(53, length(tree.matched$tip.label)), 1:length(tree.matched$tip.label), pch=21, bg=colors[tree.matched$tip.label], cex=(sizes[tree.matched$tip.label])*.25, lwd=1.5)
legend.sizes<-c(10,5,1)
points(rep(3, 3), 1:3, pch=21, cex=legend.sizes*.25, lwd=1.5)
dev.off()




##there must be a way to code this as a loop that actually works :(
#vars <- colnames(data.matched)

#for (i in 5:50) {
#  i<-4

#  datacomp <-comparative.data(scope=Laterals.present~Social_Status, phy=tree.matched, data=data.matched, names.col=sp_name, vcv = TRUE, vcv.dim=3, na.omit=TRUE, warn.dropped=FALSE)
# Problem is that we are often using categorical response variables


#Miriam correlations to test

attach(cleanData)
pdf("miriam_correlations.pdf")

  #QWDimorph vs PropB1.males
  test <- pgls(QWDimorphHead~PropB1.males, data=datacomp, lambda='ML', param.CI=.95)
  summary(test) 
  plot(QWDimorphHead~PropB1.males)
  abline(a=coef(test)[1], b = coef(test)[2], col='red')
  legend("topright", bty="n", legend=paste("Adj R2=", 
    format(summary(test)$adj.r.squared, digits=3), "\n", "p=", 
    format(anova(test)$'Pr(>F)'[1], digits=3)))
  title("QWDimorph vs PropB1.males")
  
  #QWDimorph vs WorkersMated Negative
  test <- pgls(QWDimorphHead~WorkersMated, data=datacomp, lambda='ML', param.CI=.95)
  summary(test) 
  plot(cleanData$QWDimorphHead~cleanData$WorkersMated)
  abline(a=coef(test)[1], b = coef(test)[2], col='red')
  legend("topright", bty="n", legend=paste("Adj R2=", 
    format(summary(test)$adj.r.squared, digits=3), "\n", "p=", 
    format(anova(test)$'Pr(>F)'[1], digits=3)))
  title("QWDimorph vs WorkersMated Negative")
  
  #QWDimorph vs NumWbroods Positive:
  test <- pgls(QWDimorphHead~NumWorkerBroods, data=datacomp, lambda='ML', param.CI=.95)
  summary(test) 
  plot(cleanData$QWDimorphHead~cleanData$NumWorkerBroods)
  abline(a=coef(test)[1], b = coef(test)[2], col='red')
  legend("topright", bty="n", legend=paste("Adj R2=", 
    format(summary(test)$adj.r.squared, digits=3), "\n", "p=", 
    format(anova(test)$'Pr(>F)'[1], digits=3)))
  title("QWDimorph vs NumWbroods Positive")
  
  #WorkerOvDev vs Wmated Positive
  test <- pgls(WorkerOvDev~WorkersMated, data=datacomp, lambda='ML', param.CI=.95)
  summary(test) 
  plot(cleanData$WorkerOvDev~cleanData$WorkersMated)
  abline(a=coef(test)[1], b = coef(test)[2], col='red')
  legend("topright", bty="n", legend=paste("Adj R2=", 
    format(summary(test)$adj.r.squared, digits=3), "\n", "p=", 
    format(anova(test)$'Pr(>F)'[1], digits=3)))
  title("WorkerOvDev vs Wmated Positive")
  
  # Weaker relationships to investigate
  
  # QWDimorph vs NumWorkerBroods
  test <- pgls(QWDimorphHead~NumWorkerBroods, data=datacomp, lambda='ML', param.CI=.95)
  summary(test) 
  plot(cleanData$QWDimorphHead~cleanData$NumWorkerBroods)
  abline(a=coef(test)[1], b = coef(test)[2], col='red')
  legend("topright", bty="n", legend=paste("Adj R2=", 
    format(summary(test)$adj.r.squared, digits=3), "\n", "p=", 
    format(anova(test)$'Pr(>F)'[1], digits=3)))
  title("QWDimorph vs NumWorkerBroods")
  
 # QWDimorph  vs WorkerOvDev
  test <- pgls(QWDimorphHead~WorkerOvDev, data=datacomp, lambda='ML', param.CI=.95)
  summary(test) 
  plot(cleanData$QWDimorphHead~cleanData$WorkerOvDev)
  abline(a=coef(test)[1], b = coef(test)[2], col='red')
  legend("topright", bty="n", legend=paste("Adj R2=", 
    format(summary(test)$adj.r.squared, digits=3), "\n", "p=", 
    format(anova(test)$'Pr(>F)'[1], digits=3)))  
  title("QWDimorph  vs WorkerOvDev")
  
 # NumWorkerBroods vs PropB1.males 
  test <- pgls(NumWorkerBroods~PropB1.males, data=datacomp, lambda='ML', param.CI=.95)
  summary(test) 
  plot(cleanData$NumWorkerBroods~cleanData$PropB1.males)
  abline(a=coef(test)[1], b = coef(test)[2], col='red')
  legend("topright", bty="n", legend=paste("Adj R2=", 
    format(summary(test)$adj.r.squared, digits=3), "\n", "p=", 
    format(anova(test)$'Pr(>F)'[1], digits=3)))
  title("NumWorkerBroods vs PropB1.males ")
  
  
 # NumWorkerBroods  vs WorkerOvDev
  test <- pgls(NumWorkerBroods~WorkerOvDev, data=datacomp, lambda='ML', param.CI=.95)
  summary(test) 
  plot(cleanData$NumWorkerBroods~cleanData$WorkerOvDev)
  abline(a=coef(test)[1], b = coef(test)[2], col='red')
  legend("topright", bty="n", legend=paste("Adj R2=", 
    format(summary(test)$adj.r.squared, digits=4), "\n", "p=", 
    format(anova(test)$'Pr(>F)'[1], digits=4)))
  title(" NumWorkerBroods  vs WorkerOvDev")
  
  #NumWorkerBroods vs WorkersMated
  test <- pgls(NumWorkerBroods~WorkersMated, data=datacomp, lambda='ML', param.CI=.95)
  summary(test) 
  plot(cleanData$NumWorkerBroods~cleanData$WorkersMated)
  abline(a=coef(test)[1], b = coef(test)[2], col='red')
  legend("topright", bty="n", legend=paste("Adj R2=", 
    format(summary(test)$adj.r.squared, digits=3), "\n", "p=", 
    format(anova(test)$'Pr(>F)'[1], digits=3)))
  title("NumWorkerBroods vs WorkersMated")
  
  #NumB1.offspring vs NumQ 
  test<- pgls(NumB1.offspring~NumQ, data=datacomp, lambda='ML', param.CI=.95)
  summary(test) 
  plot(cleanData$NumB1.offspring~cleanData$NumQ)
  abline(a=coef(test)[1], b = coef(test)[2], col='red')
  legend("topright", bty="n", legend=paste("Adj R2=", 
    format(summary(test)$adj.r.squared, digits=3), "\n", "p=", 
    format(anova(test)$'Pr(>F)'[1], digits=3)))
  title("NumB1.offspring vs NumQ ")
 
  dev.off()
  detach(cleanData)

  
  
  HibSite<-pgls(HibSite~Social_Status, data=datacomp, lambda='ML', param.CI=.95)
  summary(HibSite)
  anova(HibSite)
  plot(HibSite)
  
  NumQ<-pgls(NumQ~Social_Status, data=datacomp, lambda='ML', param.CI=.95)
  summary(NumQ)
  anova(NumQ)
  plot(NumQ)
  boxplot(cleanData$NumQ~cleanData$Social_Status)
  
  NumB1.offspring<-pgls(NumB1.offspring~Social_Status, data=datacomp, lambda='ML', param.CI=.95)
  summary(NumB1.offspring)
  anova(NumB1.offspring)
  plot(NumB1.offspring)
  boxplot(cleanData$NumB1.offspring~cleanData$Social_Status)
  
  RBOff<-pgls(RBOff~Social_Status, data=datacomp, lambda='ML', param.CI=.95)
  summary(RBOff)
  anova(RBOff)
  plot(RBOff)
  boxplot(cleanData$RBOff~cleanData$Social_Status)
  
  PropB1.males<-pgls(PropB1.males~Social_Status, data=datacomp, lambda='ML', param.CI=.95)
  summary(PropB1.males)
  anova(PropB1.males)
  plot(PropB1.males)
  boxplot(cleanData$PropB1.males~cleanData$Social_Status)
  
  PropRB.males<-pgls(PropRB.males~Social_Status, data=datacomp, lambda='ML', param.CI=.95)
  summary(PropRB.males)
  anova(PropRB.males)
  plot(PropRB.males)
  
  Lecty<-pgls(Lecty~Social_Status, data=datacomp, lambda='ML', param.CI=.95)
  summary(Lecty)
  anova(Lecty)
  plot(Lecty)
  boxplot(cleanData$Lecty~cleanData$Social_Status)
  
  Voltinism<-pgls(Voltinism~Social_Status, data=datacomp, lambda='ML', param.CI=.95)
  summary(Voltinism)
  anova(Voltinism)
  plot(Voltinism)
  
  #=====OLD ANALYSES======#
  
  #brunch is for binary categorical variables
  cat<- brunch(NestReused ~ Social_Status, data=datacomp)
  summary(cat)
  plot(cat)
  
  Num.Wbroods<-pgls(Num.Wbroods~Social_Status, data=datacomp, lambda='ML', param.CI=.99)
  summary(Num.Wbroods)
  
  PropB1.males<-pgls(PropB1.males~Social_Status, data=datacomp, lambda='ML')
  summary(PropB1.males)
  
  Diapause.stage<-pgls(Diapause.stage~Social_Status, data=datacomp, lambda='ML')
  summary(Diapause.stage)
  
  Wmated<-pgls(Wmated~Social_Status, data=datacomp, lambda='ML')
  summary(Wmated)
  
  Lecty<-pgls(Lecty~Social_Status, data=datacomp, lambda='ML')
  summary(Lecty)

  PropRB.males<-pgls(PropRB.males~Social_Status, data=datacomp, lambda='ML')
  summary(PropRB.males)
  
  hib_site<-pgls(hib_site~Social_Status, data=datacomp, lambda='ML')
  summary(hib_site)
  
  Cavity.formed<-pgls(Cavity.formed~Social_Status, data=datacomp, lambda='ML')
  summary(Cavity.formed)
  
  Flight.season<-pgls(Flight.season~Social_Status, data=datacomp, lambda='ML')
  summary(Flight.season)
  
#  NumB1.offspring<-pgls(NumB1.offspring~Social_Status, data=datacomp, lambda='ML')
#  summary(NumB1.offspring)
  
#  r.Wks<-pgls(r.Wks~Social_Status, data=datacomp, lambda='ML')
#  summary(r.Wks)
  
  
  #  QWSizediff.HW<-pgls(QWSizediff.HW~Social_Status, data=datacomp, lambda='ML')
  #  summary(QWSizediff.HW)
  
  #  cofounding<-pgls(cofounding~Social_Status, data=datacomp, lambda='ML')
#  summary(cofounding)
  
  
#  num_Q<-pgls(num_Q~Social_Status, data=datacomp, lambda='ML')
#  summary(num_Q)
  
#  Wk.size.W<-pgls(Wk.size.W~Social_Status, data=datacomp, lambda='ML')
#  summary(Wk.size.W)
  
#  r.Gyn<-pgls(r.Gyn~Social_Status, data=datacomp, lambda='ML')
#  summary(r.Gyn)
  
#  Gyne.size.W<-pgls(Gyne.size.W~Social_Status, data=datacomp, lambda='ML')
#  summary(Gyne.size.W)
  
  
#  NumRB.broodcells<-pgls(NumRB.broodcells~Social_Status, data=datacomp, lambda='ML')
#  summary(NumRB.broodcells)
  
  
#}

