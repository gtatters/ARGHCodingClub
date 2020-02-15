rm(list=ls())

# http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html

# Libraries used
library(lme4)
# library(lmerTest) # we use this below but it conflicts with lme4's lmer function
library(effects)
library(ggplot2)
library(tidyverse)

# Load in data
setwd("~/Dropbox/ARGHCodingClub/ggplot")
d<-read.csv("RivulusMirrorET.csv")

# File containing the Emersion Threshold Data from fish heated in the
# presence of a mirror under-water
# Hypothesis: Reflection and social behaviours interfere with decision to leave water 
# under thermal stress
str(d)

# Factorise
d$Order<-factor(d$Order)
d$Treatment<-relevel(d$Treatment, ref="Opaque")

# Calculate rates of behaviours per min
d$SENum<-d$SENum/d$ExptDuration
d$BSNum<-d$BSNum/d$ExptDuration
d$SurfaceScore<-d$SENum+d$BSNum
d$MirrorScore<-d$MCNum+d$LDNum
# SE = Surface Excursion
# BS = Break Surface
# Normalise the counts to the duration of the experiment


# Create two functions to allow 95% CI approximation to be calculated
lower<-function(x){
  xbar<-mean(x)
  se<-sd(x)/sqrt(length(x))
  lwr<-xbar-1.96*se
  return(lwr)
}

upper<-function(x){
  xbar<-mean(x)
  se<-sd(x)/sqrt(length(x))
  lwr<-xbar+1.96*se
  return(lwr)
}

# summarise data using piping (%>%)
ds<-d[c("Treatment", "ET1")] %>%
  group_by(Treatment) %>%
  summarize(mean=mean(ET1), lower=lower(ET1), upper=upper(ET1))
ds



# Model Fits ####
lmET1<-lmer(ET1 ~ Treatment + Order + (1|FishID), data=d)
car::Anova(lmET1)
# Explicitly call lmerTest's lmer function:
lmET1<-lmerTest::lmer(ET1 ~ Treatment + (1|FishID), data=d)
summary(lmET1)

anova(lmET1, ddf="Satterthwaite")

# Model effects (i.e. the predicted values + 95% CI)
effET1<-Effect("Treatment", lmET1)

# put model effects into a data.frame for ggplotting
effET1<-data.frame(Effect("Treatment", lmET1), P=c("P = 0.0041", NA))
str(effET1)
effET1


# Emersion Threshold Figure ####


ET1.plot<-ggplot()+
  geom_line(data=d, aes(x=Treatment, y=ET1,  group=FishID), 
            col="grey")
ET1.plot

dodge<-position_dodge(width=.1)
ET1.plot<-ggplot()+
  geom_line(data=d, aes(x=Treatment, y=ET1, group=FishID), col="grey", 
            position=dodge)+
  geom_point(data=d, aes(x=Treatment, y=ET1, fill=Treatment, group=FishID), shape=21, position=dodge, size=0.5)
ET1.plot

ET1.plot<-ggplot()+
  geom_line(data=d, aes(x=Treatment, y=ET1, group=FishID), col="grey", position=dodge)+
  geom_point(data=d, aes(x=Treatment, y=ET1, fill=Treatment, group=FishID), shape=21, position=dodge, size=0.5)+
  geom_errorbar(data=effET1, aes(x=Treatment, ymin=lower, ymax=upper), width=0.05, size=0.5)+
  geom_point(data=effET1, aes(x=Treatment, y=fit, fill=Treatment), col="black", shape=21, size=3)
ET1.plot

# Using the model effects:
ET1.plot<-ggplot()+
  geom_line(data=d, aes(x=Treatment, y=ET1, group=FishID), col="grey", position=dodge)+
  geom_point(data=d, aes(x=Treatment, y=ET1, fill=Treatment, group=FishID), shape=21, position=dodge, size=0.5)+
  geom_errorbar(data=effET1, aes(x=Treatment, ymin=lower, ymax=upper), width=0.05, size=0.5)+
  geom_point(data=effET1, aes(x=Treatment, y=fit, fill=Treatment), col="black", shape=21, size=3)+
  annotate("label", x=1.5, y=43.5, label = effET1$P[1], size=3, label.size=NA)+
  scale_fill_manual(values=c("black", "white"), name="", guide=F)+
  ylab("Emersion\nTemperature (°C)")+
  xlab("Treatment")+
  ylim(39,43.5)+
  theme_classic()+
  #  ggtheme(10,0.3)+
  theme(legend.position=c(0.15,0.85))+
  theme(panel.border = element_rect(fill=NA, colour=NA))
ET1.plot

# Using the group means +- se values: 
ET1.plot<-ggplot()+
  geom_line(data=d, aes(x=Treatment, y=ET1, group=FishID), 
            col="grey", position=dodge)+
  geom_point(data=d, aes(x=Treatment, y=ET1, fill=Treatment, group=FishID),
             shape=21, position=dodge, size=0.5)+
  geom_errorbar(data=ds, aes(x=Treatment, ymin=lower, ymax=upper), 
                width=0.05, size=0.5)+
  geom_point(data=ds, aes(x=Treatment, y=mean, fill=Treatment), 
             col="black", shape=21, size=3)+
  #annotate("label", x=1.5, y=43.5, label = effET1$P[1], size=3, label.size=NA)+
  scale_fill_manual(values=c("black", "white"), name="", guide=F)+
  #ylab("Emersion\nTemperature (°C)")+
  ylab(expression('Emersion Temperature ('*degree*'C)'))+
  #ylab(expression('Emersion Temperature ('*~degree*'C)'))+
  xlab("Treatment")+
  ylim(39,43.5)+
  theme_classic()+
  #  ggtheme(10,0.3)+
  theme(legend.position=c(0.15,0.85))+
  theme(panel.border = element_rect(fill=NA, colour=NA))
ET1.plot

save(ET1.plot, file="Mygraph.Rda")
ET1.plot<-NULL
ET1.plot
load("Mygraph.Rda")
ET1.plot

ggsave("Figure 1 - Mirror vs Opaque Emersion Thresholds_4x4in.pdf", 
       ET1.plot,  width=4, height=4)
ggsave("Figure 1 - Mirror vs Opaque Emersion Thresholds_4x4cm.pdf", 
       ET1.plot,  width=4, height=4, units="cm")

ggsave("Figure 1 - Mirror vs Opaque Emersion Thresholds_4x4in.tiff",
       ET1.plot, width=4, height=4)
ggsave("Figure 1 - Mirror vs Opaque Emersion Thresholds_4x4in_600dpi.tiff",
       dpi=600,
       ET1.plot, width=4, height=4)
ggsave("Figure 1 - Mirror vs Opaque Emersion Thresholds_4x4cm.tiff",
       dpi=600, 
       ET1.plot, width=4, height=4, units="cm")

# Using Vector based graphics (pdf, eps) - the output is mostly scaleable, but 
# exported sizes of fonts will need tweaking
# Using Raster Graphics output (tiff, png, jpg, bmp) will require a lot of
# trial and error to get sizes of lines, points, and fonts correct





ET1.plot+ 
  xlab("Fish Treatment")

lmSE<-lmer(SurfaceScore ~ Treatment + (1|FishID), d)
lmSE<-lmerTest::lmer(SurfaceScore ~ Treatment + (1|FishID), d)
summary(lmSE)
anova(lmSE, ddf="Satterthwaite")

effSE<-data.frame(Effect("Treatment", lmSE), P=c("P = 0.00061", NA))

SE_emersion.plot<-ggplot()+
  geom_line(data=d, aes(x=Treatment, y=SurfaceScore, group=FishID), 
            col="grey", position=dodge)+
  geom_point(data=d, aes(x=Treatment, y=SurfaceScore, fill=Treatment, 
                         group=FishID), shape=21, position=dodge, size=0.5)+
  geom_errorbar(data=effSE, aes(x=Treatment, ymin=lower, ymax=upper),
                width=0.05, size=0.5)+
  geom_point(data=effSE, aes(x=Treatment, y=fit, fill=Treatment),
             col="black", shape=21, size=3)+
  annotate("label", x=1.5, y=10, label = effSE$P[1], size=3, label.size=NA)+
  scale_fill_manual(values=c("black", "white"), name="")+
  scale_y_continuous(breaks=c(0,2,4,6,8,10))+
  ylab(expression("Surface Behaviours (min"^-1*")", adj=0.5))+ 
  xlab("Treatment")+
  #  ggtheme(10,0.3)+
  theme_classic()+
  theme(legend.position=c(0.15,0.85))+
  theme(panel.border = element_rect(fill=NA, colour=NA))
SE_emersion.plot


library(cowplot)

biplot<-plot_grid(ET1.plot, SE_emersion.plot, labels=c("a)", "b)"))
biplot

ggsave("Figure 2.pdf", biplot, width=8, height=4)

SE_emersion.plot+theme_grey()
biplot+theme_grey()
biplot+theme_bw()

library(ggthemes)
ET1.plot+theme_economist()
ET1.plot+theme_gdocs()

# You can create your own theme function
# Involves a lot of theme calls
theme_grey
# ggtheme settings ####
ggtheme <- function(base_size=12, base_line=0.3) {
  #theme_bw() %+replace%
  theme(
    
    text =        element_text(size=base_size),
    line =        element_line(size=base_line, linetype="solid"),
    
    axis.text.x = element_text(size=base_size*0.8, colour='black',  hjust=0.5, vjust=1, angle=0),
    axis.text.y = element_text(size=base_size*0.8, colour='black', hjust=1, vjust=0.5, angle=0),
    axis.line.x = element_line(size=base_line),
    axis.line.y = element_line(size=base_line),
    
    axis.title.x =  element_text(size = base_size, vjust = 1, margin=unit(c(3,0,0,0),"mm")),
    axis.title.y =  element_text(size = base_size, angle = 90, vjust = 0.5, margin=unit(c(0,3,0,0),"mm")),
    axis.ticks =    element_line(size=base_line),
    axis.ticks.length = unit(0.3, "lines"),
    
    panel.background = element_blank(),
    #panel.grid.minor = element_line(colour='#EEEEEE', linetype="solid", size=base_line),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.border     = element_rect(fill=NA, colour=NA, size=base_line),
    panel.spacing    = unit(1, "lines"),
    
    legend.background = element_rect(fill="transparent", colour="transparent"),
    legend.key =       element_rect(fill="transparent", colour="transparent"),
    legend.text=       element_text(size=base_size),
    
    strip.background =  element_blank(),
    strip.text.x =      element_text(size = base_size * 0.8),
    strip.text.y =      element_text(size = base_size * 0.8, angle = -90),
    strip.switch.pad.grid = unit(0, "mm"),
    strip.switch.pad.wrap = unit(0, "mm"),
    
    plot.margin =       unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
    plot.title =        element_text(size = base_size * 1.2),
    plot.background =   element_rect(colour = "transparent", fill="transparent", size=base_line)
  )}

ET1.plot+ggtheme(12, 0.1)

