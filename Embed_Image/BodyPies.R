#wholebody_cutLH_wide<-dcast(wholebody_cutLH[,1:3],tax_name~tax_rank,value.var="count_norm")
dat<-as.data.frame(replicate(50, rnorm(6)) )
### ### Figure 4 v2 with legend - relative abundance of taxa across body sites - pies
library(png)
library(grid)
library(scatterpie)
#
dir<-"/Users/alisonwaller/Documents/Professional/Brock/ADickson_Microbiome/"
img <- readPNG( paste(dir,"aac_human_body.png",sep=""))
g<-rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc"), interpolate = FALSE)
#
colnames(dat)<-replicate(50, paste(sample(LETTERS, 3, replace=TRUE), collapse=""))
rownames(dat)<-c("ear","genital","gut","LH","mouth","nose")
#Ear, Genital,Gut, LH,Mouth,Nose
dat$imX<-c(2,1.5,1.75,0.3,1.5,1.5)
dat$imY<-c(5.5,1,2,1,4.8,5.2)
dat$radius<-0.2
#dat<-apply(dat,2,function(x) {as.numeric(x)})
#dat<-as.data.frame(dat)
# trying to get top genera for each site for the legend
top5_ear<-names(rev(sort(dat[1,1:(ncol(dat)-3)]))[1:5])
top5_genital<-names(rev(sort(dat[2,1:(ncol(dat)-3)]))[1:5])
top5_gut<-names(rev(sort(dat[3,1:(ncol(dat)-3)]))[1:5])
top5_LH<-names(rev(sort(dat[4,1:(ncol(dat)-3)]))[1:5])
top5_mouth<-names(rev(sort(dat[5,1:(ncol(dat)-3)]))[1:5])
top5_nose<-names(rev(sort(dat[6,1:(ncol(dat)-3)]))[1:5])
#
allcolors = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
colors35<-sample(allcolors,35)
#label_df<-data.frame(label=c("Staphylococcaceae","Propionibacteriaceae",
#                             "Lachnospiraceae","Ruminococcaceae","Bacteroidaceae",
#                             "Corynebacteriaceae",
#                             "Propionibacteriaceae","Staphylococcaceae",
#                             "Bifidobacteriaceae","Lachnospiraceae","Ruminococcaceae","Prevotellaceae",
#                             "Streptococcaceae","Neisseriaceae","Veillonellaceae"),
#                     Xloc=c(2.0,2.2,2.2,1.2,2.25,0.5,2.0,0.9,2.55,2.4,1.5,1.5,0.9,1.1,1.95),
#                     Yloc=c(0.3,0.6,1.4,1.7,1.8,0.35,4.15,4.45,5.3,4.85,5.25,5.1,4.85,5.0,4.65))
ggplot(dat) +
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  geom_scatterpie(aes(x=imX, y=imY,r=radius),
                  data=dat, cols=colnames(dat)[1:50],color=NA) +
  scale_fill_manual(values=sample(allcolors,50)) +
  scale_x_continuous(expand=c(0,0), lim=c(0,3)) +
  scale_y_continuous(expand=c(0,0), lim=c(0,6)) +
#  annotate("text", x=label_df$Xloc, y=label_df$Yloc+0.4,
#           label= label_df$label)+
  theme(legend.position="none",
        panel.background = element_rect(fill = "transparent") # bg of the panel
        , plot.background = element_rect(fill = "transparent") # bg of the plot
        , panel.grid.major = element_blank() # get rid of major grid
        , panel.grid.minor = element_blank(), # get rid of minor grid
        line = element_blank(),
        text = element_blank(),
        title = element_blank()
  )  
ggsave('Fig4v2_wholebody_cutLH_cutbody_pies_col4.png', height=10, width = 5, units = 'in',dpi=300)


