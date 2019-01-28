library(caper)
library(phytools)
library(phangorn)
library(ape)
library(geiger)
library(ggplot2)
library(sqldf)
library(dplyr)

traits <- read.csv("c:traits.csv", row.names = 1, stringsAsFactors = TRUE)
traits
x <- data.matrix(traits)
x

tree <- read.tree("c:Apidae2.tre")
tree
plot(tree)

# plot(tree, type="phylogram", adj=0, cex=, label.offset = 1, use.edge.length=FALSE, edge.width=1.5)

dotTree(tree, traits, legend=FALSE, method = "phylogram", standardize = FALSE)

# dot.legend(traits,x,2,3,length=15,Ntip = 14, prompt=FALSE)

