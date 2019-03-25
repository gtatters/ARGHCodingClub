# HOW TO MAKE NETWORKS FROM INTERACTION DATA
# We need an edgelist or matrix, which we can then use to make a network object.
# The network object can be plotted in many different ways. 
# I will show how to employ my current favorite plotting method.

# For additional information please follow this link:
# http://kateto.net/network-visualization

#######################################################################################
##   1.0.0        CLEARING MEMORY AND SETTING WORKING DIRECTORY                      ##
#######################################################################################
rm(list=ls())

# Ctrl + Shift + H -> (set working directory)
# Ctrl + L to clear console
#######################################################################################
##   2.0.0        LOADING PACKAGES & LOADING DATA                                    ##
#######################################################################################
# Packages to install if you do not already have them:
# install.packages("visNetwork")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("RColorBrewer")

# Load packages
# library(package name) # Example

# Network packages
library(visNetwork) # Visualization of networks
# library(igraph) # 
# library(sna)
# library(network)
# library(threejs)
# library(networkD3)
# library(ndtv)

# Data curation
library(dplyr)
library(tidyr)

# Colours
library(RColorBrewer)

# Reading in raw interaction data file
raw.data <-
  read.csv("raw.data.csv",
           stringsAsFactors = FALSE,
           header = TRUE,
           sep =",")

# Reading in node attributes data file.
node_attributes <- 
  read.csv("attributes.csv",
           stringsAsFactors = FALSE,
           header = TRUE,
           sep =",")

# Preparing and edgelist ----------------------------------------------------------

# Creating an edgelist from raw numbers of chase-flee interactions between individual males
edge_list <- raw.data %>%
  group_by(from = ID_1, to = ID_2, type = Behaviour) %>%
  summarise(edge.weight = n())
edge_list

# Need a list of all the unique node names from the edgelist
ID <- unique(c(as.character(edge_list$from), as.character(edge_list$to)))
node_names <- as.data.frame(ID, row.names = NULL, stringsAsFactors = FALSE)

# Merging node_names with node_attributes.
node_attributes <- merge(node_names, node_attributes, by = "ID")

# Adding new node and edge attributes to the dataframes. 
nodes <- node_attributes %>%
  select(id = "ID", "Age_group", "Sex") # For some reason id needs to be lowercase to work

links <- edge_list %>%
  select("from", "to", "type", weight = "edge.weight") # We will be tinkering with this new links file

# Woohoo! Now we are ready for networks!

# Plotting your network -----------------------------------------------------------
# Renaming probably doesn't need to happen. This is pieced together code from a tutorial.
vis.nodes <- nodes
vis.links <- links

# Making some character strings into factors to apply labels, colours, and shapes.
vis.nodes$Sex <- as.factor(vis.nodes$Sex)
vis.nodes$Age_group <- as.factor(vis.nodes$Age_group)
vis.links$type <- as.factor(vis.links$type)

# Creating some objects for sets of colours and shapes.
colrs <- c("slategrey", "tomato")
age_colrs <- c("slategrey", "limegreen", "tomato")
shapes <- c("dot", "triangle")

# Changing the visual properties of the nodes
vis.nodes$shape  <- shapes[vis.nodes$Sex] 
vis.nodes$shadow <- FALSE # Nodes will drop shadow
vis.nodes$title  <- vis.nodes$Sex # Text on click
vis.nodes$label  <- vis.nodes$ID # Node label
vis.nodes$size   <- 20 # Node size
vis.nodes$borderWidth <- 2 # Node border width
vis.nodes$color.background <- age_colrs[vis.nodes$Age_group]
vis.nodes$color.border <- "black"
vis.nodes$color.highlight.background <- "orange"
vis.nodes$color.highlight.border <- "darkred"

# Changing the visual properties of the edges
vis.links$width <- vis.links$weight*3 # line width
vis.links$color <- colrs[vis.links$type] # Change colors to highlight chases (first colour), or flees (second colour)  
vis.links$arrows <- "to" # arrows: can use 'from', 'to', or 'both'
vis.links$smooth <- FALSE    # should the edges be curved?
vis.links$shadow <- FALSE    # edge shadow
# vis.links$label <- vis.links$type # optional for labelling edges
  
# Creating the first network
visnet <- visNetwork(vis.nodes, vis.links, main = "Call-text network") 

# THE NETWORK IN ALL ITS GLORY!!!!
# Edges are the sender to the receiver.
visOptions(visnet, 
           selectedBy = "Age_group",
           highlightNearest = TRUE, 
           nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 30) %>%
  visIgraphLayout() 

# Another way to view the same network with slightly higher ram requirements
visOptions(visnet, selectedBy = "Age_group",
           highlightNearest = TRUE, 
           nodesIdSelection = TRUE
) %>%
  visPhysics(stabilization = TRUE) %>%
  visLayout(randomSeed = 30) %>%
  visInteraction(dragNodes = TRUE,
                 dragView = TRUE, 
                 zoomView = TRUE)
