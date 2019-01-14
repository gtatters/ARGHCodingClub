
# Libraries used
library(lme4)

# library(lmerTest) # we use this below but it conflicts with lme4's lmer function
library(effects)
library(ggplot2)
library(dplyr)

# Load in data
setwd("~/Desktop")
setwd("~/Dropbox/ARGHCodingClub/Mean_Error_Dot_Plot")
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


# Create two functions to allow 95% CI to be calculated
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

# summarise data using dplyr piping (%>%)
ds<-d[c("Treatment", "ET1")] %>%
  group_by(Treatment) %>%
  summarise_all(funs(mean, lower, upper))
ds



# Model Fits ####
lmET1<-lmer(ET1 ~ Treatment + Order + (1|FishID), data=d)
# Explicitly call lmerTest's lmer function:
lmET1<-lmerTest::lmer(ET1 ~ Treatment + Order + (1|FishID), data=d)
summary(lmET1)

anova(lmET1, ddf="Satterthwaite")

# Model effects (i.e. the predicted values + 95% CI)
effET1<-Effect("Treatment", lmET1)
plot(effET1)

# put model effects into a data.frame for ggplotting
effET1<-data.frame(Effect("Treatment", lmET1), P=c("P = 0.0041", NA))
str(effET1)
effET1


# Emersion Threshold Figure ####

dodge<-position_dodge(width=.1)
ET1.plot<-ggplot()+
  geom_line(data=d, aes(x=Treatment, y=ET1, group=FishID), 
            col="grey", position=dodge)
ET1.plot

ET1.plot<-ggplot()+
  geom_line(data=d, aes(x=Treatment, y=ET1, group=FishID), col="grey", position=dodge)+
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
  ylab("Emersion\nTemperature (°C)")+
  xlab("Treatment")+
  ylim(39,43.5)+
  theme_classic()+
  #  ggtheme(10,0.3)+
  theme(legend.position=c(0.15,0.85))+
  theme(panel.border = element_rect(fill=NA, colour=NA))
ET1.plot

# ggsave("Figure 1 - Mirror vs Opaque Emersion Thresholds.pdf", ET1.plot,  width=4, height=4)


lmSE<-lmer(SurfaceScore ~ Treatment + (1|FishID), d)
lmSE<-lmerTest::lmer(SurfaceScore ~ Treatment + (1|FishID), d)
summary(lmSE)
anova(lmSE, ddf="Satterthwaite")

effSE<-data.frame(Effect("Treatment", lmSE), P=c("P = 0.00061", NA))

SE_emersion.plot<-ggplot()+
  geom_line(data=d, aes(x=Treatment, y=SurfaceScore, group=FishID), col="grey", position=dodge)+
  geom_point(data=d, aes(x=Treatment, y=SurfaceScore, fill=Treatment, group=FishID), shape=21, position=dodge, size=0.5)+
  geom_errorbar(data=effSE, aes(x=Treatment, ymin=lower, ymax=upper), width=0.05, size=0.5)+
  geom_point(data=effSE, aes(x=Treatment, y=fit, fill=Treatment), col="black", shape=21, size=3)+
  annotate("label", x=1.5, y=10, label = effSE$P[1], size=3, label.size=NA)+
  scale_fill_manual(values=c("black", "white"), name="", guide=F)+
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

# ggsave("Figure 2.pdf", biplot, width=8, height=4)



