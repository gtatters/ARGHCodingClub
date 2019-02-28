library(ape)
library(ade4) # source of example data and tree
library(ape) # tree handling
library(nlme) # regression modelling

# Set working directory
setwd("~/Desktop/LizardPhylogeny")

# lepidosauria_family.nwk was derived from timetime.org by searching for Lepidosauria
# and selecting the tree option on the timetree.org website

tree<-read.tree("lepidosauria_family.nwk")
tree
plot(tree)
tree$tip.label # tip labels

# Mapping Tb trait onto tree ####
# Load in trait data, and assign the family names (column 1) as row.names 
lizards <- read.csv("Clusella Trullas Review Table 1.csv", row.names=1)
lizards

sort(tree$tip.label)
length(tree$tip.label)
rownames(lizards)
nrow(lizards)
# We have 56 taxa in the phylogeny
# But only 34 data points in file for temperature data
# Create a variable called both that describes which taxa are found in both:

both <- intersect(tree$tip.label, rownames(lizards))
both

# Create a tree that is matched to the data by only selecting those found in both:
tree.matched <- root(drop.tip(tree, setdiff(tree$tip.label, both)), outgroup="Sphenodontidae")
tree.matched
plot(tree.matched)

# data in the lizards data frame is not in the same order as the tips from 
# tree, so re-arrange:
dat <- lizards[both, ]
rownames(dat)
# tree$edge.length<-tree$edge.length/max(tree$edge.length) # normalise branch lengths to 1

library(phytools)
library(Thermimage)

## plot continuous trait onto phylogey:

# Create a vector corresponding to the trait Tb and assign the names to this vector:
Tb<-dat$Tb
names(Tb)<-tree.matched$tip.label

# Use the contMap function to create an object that is your plot, but we need to make
# further adjustments below, so set plot=F.
# Invert refers to the colour palette direction 
# The res value above sets the resolution of the colour mapping onto the 
# continuous trait.  A higher res will create a smoother gradient, but colour 
# palettes are sometimes simply a limited number of values and we need to upsample
# any palette we use

phylopalette<-ironbowpal

objTb<-contMap(tree.matched, Tb, plot=F, type="fan", invert=T, res=300,
               lims=c(22,38))

n<-length(objTb$cols)
paln<-length(phylopalette)

ind<-floor(seq(1, paln, paln/(n+floor(n/paln))))
objTb$cols[1:n]<-phylopalette[ind]

# Phylogram of Tb in Lizard Families.pdf ####

plot(objTb, type="fan",  fsize=0.7, outline=F, leg.txt="Tb", offset=1,
     Vars=T)
plot(objTb, type="phylogram", direction="rightwards", fsize=0.7, outline=F, leg.txt="Tb", offset=1,
     Vars=T)
